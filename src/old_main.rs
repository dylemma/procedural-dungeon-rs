use std::rc::Rc;
use std::slice::Iter;

use glutin_window::GlutinWindow;
use graphics::{Context, line_from_to, triangulation};
use graphics::character::CharacterCache;
use graphics::color::{BLACK, WHITE};
use graphics::Transformed;
use graphics::math::{identity, Matrix2d, Vec2d, scale, translate};
use graphics::types::Color;
use ncollide2d::bounding_volume::AABB;
use ncollide2d::partitioning::BVH;
use ncollide2d::query::visitors::PointInterferencesCollector;
use opengl_graphics::{GlGraphics, OpenGL};
use piston::event_loop::{Events, EventSettings};
use piston::input::*;
use piston::window::{AdvancedWindow, Size, Window, WindowSettings};
use rand::{Rng, thread_rng};
use rand::seq::SliceRandom;
use vecmath::*;

#[allow(unused)]
use crate::dungeon::{GridDungeon, GridDungeonGenerator, RandomRoomGridDungeonGenerator, RoomId, RoomSize};
use crate::dungeon::{BasicGridDungeon, BiasGraph, GeneratorStep, GeneratorStrategy, GridDungeonGraph, Sliding, SlidingIter};
use crate::geom::{Corners, Point, point_eq, Rect};
use crate::graph::{DungeonFloorGraph, FloorNode, FloorNodeId};
use crate::tile::{CompassDirection, TileAddress, WallType};
use graphics::ellipse::circle;


mod dungeon;
mod geom;
mod graph;
mod tile;

const CURSOR_COLOR: Color = BLACK;
const PATH_COLOR: Color = [0.0, 0.0, 0.0, 0.75];

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

    let initial_screen_size = [1024, 768];
    let mut window: GlutinWindow = WindowSettings::new("Procedural Dungeon", initial_screen_size)
        .graphics_api(opengl)
        .exit_on_esc(false) // example code used true but I don't want that
        .build()
        .unwrap();

    let mut events = Events::new(EventSettings::new());

    let mut app = App::new(opengl, window);

    while let Some(e) = events.next(&mut app.window) {

        e.render(|args| {
            app.render(args);
        });

        e.update(|args| {
            app.update(args.dt);
        });

        // handle keyboard/button presses
        e.press(|button| {
            if let Button::Keyboard(key) = button {
                if key == Key::Space {
                    app.generate_requested = true;
                }
                println!("Typed key: {:?}", key);
            }

            if let Button::Mouse(MouseButton::Left) = button {
                app.mouse_pressed = true;
                app.nav_requested = true;
            }
        });

        e.release(|button| {
            if let Button::Mouse(MouseButton::Left) = button {
                app.mouse_pressed = false;
            }
        });

        e.mouse_cursor(|pos| {
            if app.set_cursor(pos) {
                if app.mouse_pressed {
                    app.nav_requested = true;
                }
            }
        });
        e.mouse_relative(|change| {
            // TODO: only do this if the cursor is "captured"
            // if app.update_pointer(None, Some(&change)) { app.route_requested = true; }
        });
    }
}

struct NavController {
    current: Option<Nav>,
    last_goal: Option<Point>
}
impl NavController {
    fn new() -> Self {
        NavController {
            current: None,
            last_goal: None,
        }
    }
    fn forget(&mut self) {
        self.current = None;
        self.last_goal = None;
    }
    fn update_nav(&mut self, goal: Point, player_pos: &Point, graph: &DungeonFloorGraph) {
        let should_update = match self.last_goal {
            Some(g) => !point_eq(&goal, &g),
            None => true,
        };
        if should_update {
            self.current = graph.find_route(player_pos, &goal).map(|route| Nav::new(route));
            self.last_goal = Some(goal);
        }
    }
}

struct Nav {
    waypoints: Vec<Point>,
    progress: usize,
}
impl Nav {
    fn new(waypoints: Vec<Point>) -> Self {
        Nav { waypoints, progress: 0,  }
    }
    fn waypoints(&self) -> &Vec<Point> {
        &self.waypoints
    }
    fn progress(&self) -> usize {
        self.progress
    }
    fn current_target(&self) -> Option<&Point> {
        self.waypoints.get(self.progress)
    }

    fn is_complete(&self) -> bool {
        self.progress >= self.waypoints.len()
    }

    /// Modify `pos` by moving it `step` units towards the next waypoint, or no-op if navigation is complete.
    /// Returns `true` to indicate navigation is complete, or `false` to indicate there is further movement to do.
    fn advance_by(&mut self, step: f64, pos: &mut Point) -> bool {
        if let Some(&target) = self.current_target() {
            let to_target = vec2_sub(target, *pos);
            let dist = vec2_len(to_target);


            if dist < step {
                // `pos` has reached the current target, so we can update the `progress`,
                // then recurse to spend the remaining `step` to progress to the next waypoint
                *pos = target;
                self.progress += 1;
                self.advance_by(step - dist, pos)
            } else {
                // move as far as the player can in the direction of the target; this should end the loop
                let movement = vec2_scale(to_target, step / dist);
                pos[0] += movement[0];
                pos[1] += movement[1];

                // Navigation is not yet complete
                false
            }
        } else {
            // Navigation is complete
            true
        }
    }
}

struct App {
    gl: GlGraphics,
    window: GlutinWindow,
    world: MyGameWorld,
    pcc: PlayerCameraCursor,
    mouse_pressed: bool,
    generate_requested: bool,
    pointed_room: PointedRoom,
    nav_requested: bool,
    nav: NavController,
}
impl App {
    fn new(opengl: OpenGL, window: GlutinWindow) -> Self {
        let screen_size = window.size().into();
        App {
            gl: GlGraphics::new(opengl),
            window,
            world: MyGameWorld::new(),
            pcc: PlayerCameraCursor::new(screen_size),
            mouse_pressed: false,
            generate_requested: true,
            pointed_room: PointedRoom::new(),
            nav_requested: false,
            nav: NavController::new(),
        }
    }

    fn update(&mut self, dt: f64) {
        // if the world needs to regenerate, do it now
        if self.generate_requested {
            let Size { width, height } = self.window.size();
            self.regenerate(width as i32, height as i32);
        }

        // update the navigation target as long as the mouse is down
        if self.mouse_pressed {
            if let Some(graph) = &self.world.floor_graph {
                self.nav.update_nav(self.pcc.cursor_pos, &self.pcc.player_pos, graph);
            }
        }

        // move the player along the current navigation path
        if let Some(nav) = &mut self.nav.current {
            self.pcc.modify(|PccState { player_pos, .. }| {
                nav.advance_by(200.0 * dt, player_pos);
            });
        }

        // update the player camera/cursor if it was modified since the last update
        self.pcc.update();

        // re-check the 'pointed room' if the mouse cursor's world position has changed
        if let Some(graph) = &self.world.floor_graph {
            self.pointed_room.update(self.pcc.cursor_pos, graph);
        }
    }

    fn render(&mut self, args: &RenderArgs) {
        use graphics::*;

        let world = &self.world;
        let pcc = &self.pcc;
        let player_pos = &pcc.player_pos;
        let cursor = pcc.cursor_pos;
        let pointed_room = &self.pointed_room;
        let nav_opt = &self.nav.current;

        &self.gl.draw(args.viewport(), |_c, gl| {
            let c = _c.append_transform(pcc.camera);

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

            // NAVIGATION-related debug
            if let Some(floor_graph) = &world.floor_graph {
                // DEBUG: walkable areas
                for node in floor_graph.nodes().iter() {
                    let bounds = &floor_graph.get_bounds(*node.id());

                    let color = match node {
                        FloorNode::Room { .. } => WALKABLE_ROOM_COLOR,
                        FloorNode::Door { .. } => WALKABLE_DOOR_COLOR,
                    };
                    let rect = rectangle::rectangle_by_corners(bounds.mins().x, bounds.mins().y, bounds.maxs().x, bounds.maxs().y);
                    rectangle(color, rect, c.transform, gl);
                }

                // DEBUG: cursor target walkable area
                if let Some(pointed_room) = pointed_room.current {
                    let bounds = floor_graph.get_bounds(pointed_room);
                    let rect = rectangle::rectangle_by_corners(bounds.mins().x, bounds.mins().y, bounds.maxs().x, bounds.maxs().y);
                    rectangle(POINTED_ROOM_COLOR, rect, c.transform, gl);
                }
            }

            if let Some(nav) = nav_opt {
                let start = Some(player_pos.clone());
                let lines = start.iter().chain(nav.waypoints().iter().skip(nav.progress)).sliding();
                for (from, to) in lines {
                    line_from_to(PATH_COLOR, 1.0, *from, *to, c.transform, gl);
                }
            }

            // DEBUG: cursor
            {
                let [cx, cy] = cursor;
                let vertical = rectangle::centered([cx, cy, 1.0, 4.0]);
                let horizontal = rectangle::centered([cx, cy, 4.0, 1.0]);
                rectangle(CURSOR_COLOR, vertical, c.transform, gl);
                rectangle(CURSOR_COLOR, horizontal, c.transform, gl);
            }

            {
                let [x, y] = player_pos;
                let player = circle(*x, *y, 3.0);
                ellipse(CURSOR_COLOR, player, c.transform, gl);
            }
        });
    }

    // updates the app's knowledge of the mouse cursor, returning `true` if the cursor position has changed since last time
    fn set_cursor(&mut self, cursor_screen: [f64; 2]) -> bool {
        self.pcc.modify(|PccState { cursor_px, .. }| {
            *cursor_px = cursor_screen;
        });
        self.pcc.dirty
    }

    fn regenerate(&mut self, width: i32, height: i32) {
        // regenerate the "world"
        self.world.regenerate(Rect::from_xywh(0, 0, width, height));

        // reset any app state that depends on the previous "world"
        self.nav.forget();
        self.pointed_room.forget();
        self.generate_requested = false;

        // pick a random position for the player
        let new_player_pos = self.world.floor_graph.as_ref().and_then(|graph| {
            let mut rng = thread_rng();
            graph.nodes().choose(&mut rng).map(|n| {
                let point = graph.get_bounds(*n.id()).center();
                [point.x, point.y]
            }).clone()
        });
        if let Some(pos) = new_player_pos {
            self.pcc.modify(|PccState { player_pos, .. }| {
                *player_pos = pos;
            });
        }
    }
}

struct PointedRoom {
    current: Option<FloorNodeId>,
    last_pointer: Option<Point>
}
impl PointedRoom {
    fn new() -> Self {
        PointedRoom {
            current: None,
            last_pointer: None,
        }
    }
    fn forget(&mut self) {
        self.current = None;
        self.last_pointer = None;
    }
    fn update(&mut self, pointer: Point, graph: &DungeonFloorGraph) {
        let should_update = match self.last_pointer {
            Some(last) => last[0] != pointer[0] || last[1] != pointer[1],
            None => true,
        };
        if should_update {
            self.current = graph.node_at_point(&pointer).map(|n| n.id()).cloned();
            self.last_pointer = Some(pointer);
        }
    }
}

struct PccState<'a> {
    pub cursor_px: &'a mut [f64; 2],
    pub screen_px: &'a mut [f64; 2],
    pub player_pos: &'a mut [f64; 2],
}

struct PlayerCameraCursor {
    cursor_px: [f64; 2],
    cursor_pos: [f64; 2],
    screen_px: [f64; 2],
    player_pos: [f64; 2],
    camera: Matrix2d,
    camera_inv: Matrix2d,
    dirty: bool,
}
impl PlayerCameraCursor {
    fn new(screen_size: [u32; 2]) -> Self {
        PlayerCameraCursor {
            cursor_px: [0.0, 0.0],
            cursor_pos: [0.0, 0.0],
            screen_px: [screen_size[0] as f64, screen_size[1] as f64],
            player_pos: [screen_size[0] as f64 / 2.0, screen_size[1] as f64 / 2.0],
            camera: identity(),
            camera_inv: identity(),
            dirty: true,
        }
    }

    fn update(&mut self) {
        if self.dirty {
            let zoom_factor = 4.0;

            // this is some kind of voodoo...
            // for one, the order of operations seems wrong to me
            // for two, after translating by `-player_pos` without a scale factor,
            //          you have to apply the scale factor to the half_screen translation??
            self.camera = identity()
                .zoom(zoom_factor)
                .trans_pos(vec2_neg(self.player_pos))
                .trans_pos(vec2_scale(self.screen_px, 0.5 / zoom_factor));

            self.camera_inv = mat2x3_inv(self.camera);

            self.cursor_pos = row_mat2x3_transform_pos2(self.camera_inv, self.cursor_px);

            self.dirty = false;
        }
    }

    fn modify<F>(&mut self, f: F)
        where F: FnOnce(PccState) -> ()
    {
        let [cx1, cy1] = self.cursor_px;
        let [sx1, sy1] = self.screen_px;
        let [px1, py1] = self.player_pos;

        f(PccState {
            cursor_px: &mut self.cursor_px,
            screen_px: &mut self.screen_px,
            player_pos: &mut self.player_pos,
        });

        let [cx2, cy2] = self.cursor_px;
        let [sx2, sy2] = self.screen_px;
        let [px2, py2] = self.player_pos;

        if (cx1 != cx2) || (cy1 != cy2) || (sx1 != sx2) || (sy1 != sy2) || (px1 != px2) || (py1 != py2) {
            self.dirty = true;
        }
    }
}

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

struct MyGameWorld {
    tile_pixel_size: usize,
    dungeon: Option<BasicGridDungeon>,
    floor_graph: Option<DungeonFloorGraph>,
    generator: GeneratorStrategy<&'static str>,
}
impl MyGameWorld {
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
                // GeneratorStep::Branches { count: 5 },
                // GeneratorStep::Clusters { count: 3, iterations: 20 },
                // GeneratorStep::Widen { iterations: 20 },
            ]
        };
        MyGameWorld {
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

