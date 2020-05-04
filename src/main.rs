// use crow::{Context, DrawConfig, Texture, DrawTarget};
// use crow::color::{IDENTITY, RED};
// use crow::glutin::event::{Event, StartCause, WindowEvent};
// use crow::glutin::event_loop::{ControlFlow, EventLoop};
// use crow::glutin::window::WindowBuilder;
// use crow::image::{ImageBuffer, Rgba, RgbaImage};
use rand::{Rng, thread_rng};

#[allow(unused)]
use crate::dungeon::{GridDungeon, GridDungeonGenerator, RandomRoomGridDungeonGenerator, RoomId, RoomSize};
use crate::geom::{Corners, Rect};
use crate::tile::{CompassDirection, TileAddress, WallType};
use crate::dungeon::{GridDungeonGraph, BiasGraph, GeneratorStrategy, GeneratorStep, Sliding, SlidingIter, BasicGridDungeon};
use crate::graph::{DungeonFloorGraph, AabbData};
use ncollide2d::math::Point;
use ncollide2d::query::visitors::PointInterferencesCollector;
use ncollide2d::partitioning::BVH;
use ncollide2d::bounding_volume::AABB;
use std::slice::Iter;
use std::rc::Rc;

//
use glutin_window::GlutinWindow;
use opengl_graphics::{GlGraphics, OpenGL};
use piston::event_loop::{EventSettings, Events};
use piston::input::*;
use piston::window::{Window, WindowSettings, AdvancedWindow};
use graphics::math::{identity, Matrix2d, Vec2d};
use graphics::{triangulation, line_from_to, Context};
use graphics::character::CharacterCache;
use graphics::types::Color;
use graphics::color::{BLACK, WHITE};
use nalgebra::Point2;
//

mod dungeon;
mod geom;
mod graph;
mod tile;

const CURSOR_COLOR: Color = BLACK;

const BACKGROUND_COLOR: Color = [0.4, 0.4, 0.4, 1.0];
const WALKABLE_ROOM_COLOR: Color = [0.1, 0.6, 1.0, 0.5];
const WALKABLE_DOOR_COLOR: Color = [0.1, 0.9, 0.2, 0.5];

const POINTED_ROOM_COLOR: Color = [1.0, 0.0, 0.0, 0.9];

const DEBUG_ROOM_LOW: Color = [0.1, 0.6, 1.0, 1.0];
const DEBUG_ROOM_HIGH: Color = WHITE;
const DEBUG_WALL_COLOR: Color = BLACK;

const WEIGHT_ROOM_LOW: Color = [0.1, 0.9, 0.2, 1.0];
const WEIGHT_ROOM_HIGH: Color = [1.0, 0.1, 0.1, 1.0];

fn main() {

    let opengl = OpenGL::V3_2;

    let mut window: GlutinWindow = WindowSettings::new("Procedural Dungeon", [1024, 768])
        .graphics_api(opengl)
        .exit_on_esc(false) // example code used true but I don't want that
        .build()
        .unwrap();

    let mut events = Events::new(EventSettings::new());

    let mut app = App::new(opengl);

    while let Some(e) = events.next(&mut window) {
        e.render(|args| {
            app.render(args);
        });

        e.update(|args| {
            if app.generate_requested {
                let wsize = window.size();
                let w = wsize.width as i32;
                let h = wsize.height as i32;
                app.world.regenerate(Rect::from_xywh(0, 0, w, h));
                app.generate_requested = false;
            }
        });

        // handle keyboard/button presses
        e.press(|button| {
            if let Button::Keyboard(key) = button {
                if key == Key::Space {
                    app.generate_requested = true;
                }
                println!("Typed key: {:?}", key);
            }
        });

        e.mouse_cursor(|pos| {
            app.update_pointer(Some(&pos), None);
        });
        e.mouse_relative(|change| {
            // TODO: only do this if the cursor is "captured"
            // app.update_pointer(None, Some(&change));
        });
    }
}

struct App {
    gl: GlGraphics,
    world: World,
    cursor: [f64; 2],
    generate_requested: bool,
    pointed_room: Option<AabbData>,
    pointed_query_buf: Vec<AabbData>,
}
impl App {
    fn new(opengl: OpenGL) -> Self {
        App {
            gl: GlGraphics::new(opengl),
            world: World::new(),
            cursor: [0.0; 2],
            generate_requested: true,
            pointed_room: None,
            pointed_query_buf: Vec::with_capacity(8),
        }
    }

    fn render(&mut self, args: &RenderArgs) {
        use graphics::*;

        let self_gl = &mut self.gl;
        let world = &self.world;
        let pointed_room = &self.pointed_room;
        let cursor = &self.cursor;

        &self.gl.draw(args.viewport(), |c, gl| {

            clear(BACKGROUND_COLOR, gl);

            // PRETTY room tiles + walls + doors
            if let Some(dungeon) = world.dungeon() {
                let tiles = dungeon.tiles();
                let tile_size = world.tile_pixel_size() as f64;

                // fill in a square for each room tile in the grid
                for addr in tiles.tile_addresses() {
                    if let Some((_room_id, room_weight)) = tiles[addr] {
                        let color = {
                            if room_weight >= 1.0 && room_weight <= 2.0 {
                                lerp_color(&DEBUG_ROOM_LOW, &DEBUG_ROOM_HIGH, room_weight - 1.0)
                            } else if room_weight >= 1.0 {
                                WHITE
                            } else {
                                lerp_color(&WEIGHT_ROOM_LOW, &WEIGHT_ROOM_HIGH, room_weight)
                            }
                        };
                        let x = addr.x as f64 * tile_size;
                        let y = addr.y as f64 * tile_size;
                        let rect = [x, y, tile_size, tile_size];
                        rectangle(color, rect, c.transform, gl);
                    }
                }

                // draw an appropriate line(s) for each wall in the dungeon
                for (wall_addr, wall_type) in dungeon.walls().iter() {
                    match *wall_type {
                        WallType::Clear => (),
                        WallType::Wall => {
                            let TileAddress { x, y } = wall_addr.tile();
                            let (base_to, base_from) = match wall_addr.direction() {
                                CompassDirection::North => ((0, 1), (1, 1)),
                                CompassDirection::East => ((1, 1), (1, 0)),
                                CompassDirection::South => ((0, 0), (1, 0)),
                                CompassDirection::West => ((0, 0), (0, 1)),
                            };
                            let to_px = |(dx, dy)| {
                                [(dx + x) as f64 * tile_size, (dy + y) as f64 * tile_size]
                            };
                            line_from_to(DEBUG_WALL_COLOR, 0.5, to_px(base_from), to_px(base_to), c.transform, gl);
                        }
                        WallType::Door => {
                            let TileAddress { x, y } = wall_addr.tile();
                            match wall_addr.direction() {
                                CompassDirection::North => draw_horizontal_door(&c, gl, tile_size, x as f64, y as f64 + 1.0),
                                CompassDirection::East => draw_vertical_door(&c, gl, tile_size, (x + 1) as f64, y as f64),
                                CompassDirection::South => draw_horizontal_door(&c, gl, tile_size, x as f64, y as f64),
                                CompassDirection::West => draw_vertical_door(&c, gl, tile_size, x as f64, y as f64),
                            }
                        }
                    }
                }
            }

            // DEBUG: walkable areas
            if let Some(floor_graph) = &world.floor_graph {
                for id in floor_graph.nodes.iter() {
                    let leaf = &floor_graph.bvt[*id];
                    let bounds = &leaf.bounding_volume;
                    let data = &leaf.data;

                    let color = match data {
                        AabbData::Room { .. } => WALKABLE_ROOM_COLOR,
                        AabbData::Door { .. } => WALKABLE_DOOR_COLOR,
                    };
                    let rect = rectangle::rectangle_by_corners(bounds.mins().x, bounds.mins().y, bounds.maxs().x, bounds.maxs().y);
                    rectangle(color, rect, c.transform, gl);
                }
            }

            // DEBUG: cursor target walkable area
            if let Some(pointed_room) = pointed_room {
                let bounds = match pointed_room {
                    AabbData::Room { world_bounds, .. } => world_bounds,
                    AabbData::Door { world_bounds, .. } => world_bounds,
                };
                let rect = rectangle::rectangle_by_corners(bounds.mins().x, bounds.mins().y, bounds.maxs().x, bounds.maxs().y);
                rectangle(POINTED_ROOM_COLOR, rect, c.transform, gl);
            }

            // DEBUG: cursor
            /*{
                let [cx, cy] = *cursor;
                let vertical = rectangle::centered([cx, cy, 1.0, 4.0]);
                let horizontal = rectangle::centered([cx, cy, 4.0, 1.0]);
                rectangle(CURSOR_COLOR, vertical, c.transform, gl);
                rectangle(CURSOR_COLOR, horizontal, c.transform, gl);
            }*/
        });
    }

    fn update_pointer(&mut self, abs: Option<&[f64; 2]>, rel: Option<&[f64; 2]>) {
        let mut changed = false;
        let [cx, cy] = &mut self.cursor;
        if let Some([x, y]) = abs {
            if x != cx || y != cy {
                changed = true;
                *cx = *x;
                *cy = *y;
            }
        }

        if let Some([dx, dy]) = rel {
            if *dx != 0.0 || *dy != 0.0 {
                println!("relative movement :o");
                changed = true;
                *cx += *dx;
                *cy += *dy;
            }
        }

        if changed {
            if let Some(graph) = &self.world.floor_graph {
                let buf = &mut self.pointed_query_buf;
                buf.clear();
                {
                    let point = Point::from(self.cursor);
                    let mut visitor = PointInterferencesCollector::new(&point, buf);
                    graph.bvt.visit(&mut visitor);
                }
                self.pointed_room = buf.iter().next().cloned();
            }
        }
    }
}

const DOOR_COLOR: (f32, f32, f32, f32) = (0.25, 0.25, 0.25, 1.0);

fn draw_horizontal_door(ctx: &Context, gl: &mut GlGraphics, tile_size: f64, x: f64, y: f64) {
    let pixel_pos = |xt: f64, yt: f64| { [xt * tile_size, yt * tile_size] };
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x, y), pixel_pos(x + 0.25, y), ctx.transform, gl);
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x + 0.75, y), pixel_pos(x + 1.0, y), ctx.transform, gl);
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x + 0.25, y + 0.1), pixel_pos(x + 0.25, y - 0.1), ctx.transform, gl);
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x + 0.75, y + 0.1), pixel_pos(x + 0.75, y - 0.1), ctx.transform, gl);
}
fn draw_vertical_door(ctx: &Context, gl: &mut GlGraphics, tile_size: f64, x: f64, y: f64) {
    let pixel_pos = |xt: f64, yt: f64| { [xt * tile_size, yt * tile_size] };
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x, y), pixel_pos(x, y + 0.25), ctx.transform, gl);
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x, y + 0.75), pixel_pos(x, y + 1.0), ctx.transform, gl);
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x - 0.1, y + 0.25), pixel_pos(x + 0.1, y + 0.25), ctx.transform, gl);
    graphics::line_from_to(DEBUG_WALL_COLOR, 0.5, pixel_pos(x - 0.1, y + 0.75), pixel_pos(x + 0.1, y + 0.75), ctx.transform, gl);
}

fn lerp_color(from: &Color, to: &Color, ratio: f32) -> Color {
    let [r1, g1, b1, a1] = from;
    let [r2, g2, b2, a2] = to;
    [
        r1 + (r2 - r1) * ratio,
        g1 + (g2 - g1) * ratio,
        b1 + (b2 - b1) * ratio,
        a1 + (a2 - a1) * ratio,
    ]
}

struct World {
    tile_pixel_size: usize,
    dungeon: Option<BasicGridDungeon>,
    floor_graph: Option<DungeonFloorGraph>,
    generator: GeneratorStrategy<&'static str>,
}
impl World {
    fn new() -> Self {
        let generator = GeneratorStrategy {
            room_chances: vec![
                (RoomSize::new(5,4), 1),
                (RoomSize::new(4,4), 2),
                (RoomSize::new(4,3), 3),
                (RoomSize::new(4,2), 3),
                (RoomSize::new(4,1), 1),
                (RoomSize::new(3,2), 5),
                (RoomSize::new(3,1), 2),
                (RoomSize::new(2,2), 5),
                (RoomSize::new(2,1), 1),
                (RoomSize::new(1,1), 1)
            ].into_iter().flat_map(|(size_opt, chance)| {
                size_opt.map(|s| (s, chance))
            }).collect(),
            bias_graph: BiasGraph::new(
                &vec![
                    ("start", (0.0, 0.0)),
                    ("upper-left", (0.25, 0.75)),
                    ("lower-right", (0.75, 0.25)),
                    ("end", (1.0, 1.0))
                ],
                &vec![
                    ("start", "upper-left"),
                    ("upper-left", "lower-right"),
                    ("lower-right", "end"),
                    ("start", "lower-right"),
                    ("upper-left", "end")
                ]
            ),
            steps: vec![
                GeneratorStep::Branches { count: 15 },
                GeneratorStep::Clusters { count: 10, iterations: 50 },
                GeneratorStep::Widen { iterations: 150 },
            ]
        };
        World {
            tile_pixel_size: 16,
            dungeon: None,
            floor_graph: None,
            generator
        }
    }

    fn regenerate(&mut self, pixel_bounds: Rect) -> () {
        let grid_width = pixel_bounds.width() as usize / self.tile_pixel_size;
        let grid_height = pixel_bounds.height() as usize / self.tile_pixel_size;
        let dungeon = self.generator.generate(grid_width, grid_height);
        self.floor_graph = Some(graph::decompose(&dungeon, self.tile_pixel_size as f64, 2.0, 6.0));
        self.dungeon = Some(dungeon);
    }

    fn tile_pixel_size(&self) -> usize { self.tile_pixel_size }
    fn dungeon(&self) -> &Option<BasicGridDungeon> { &self.dungeon }
}

