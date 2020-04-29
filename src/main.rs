use crow::{Context, DrawConfig, Texture, DrawTarget};
use crow::color::IDENTITY;
use crow::glutin::event::{Event, StartCause, WindowEvent};
use crow::glutin::event_loop::{ControlFlow, EventLoop};
use crow::glutin::window::WindowBuilder;
use crow::image::{ImageBuffer, Rgba, RgbaImage};
use rand::{Rng, thread_rng};

#[allow(unused)]
use crate::dungeon::{GridDungeon, GridDungeonGenerator, RandomRoomGridDungeonGenerator, RoomId, RoomSize};
use crate::geom::{Corners, Rect};
use crate::tile::{CompassDirection, TileAddress, WallType};
use crate::dungeon::{GridDungeonGraph, BiasGraph, GeneratorStrategy, GeneratorStep};

mod dungeon;
mod geom;
mod tile;

fn main() -> Result<(), crow::Error> {
    let event_loop = EventLoop::new();
    let mut ctx = Context::new(WindowBuilder::new(), &event_loop)?;

    let img: RgbaImage = ImageBuffer::from_fn(1, 1, |_x, _y| Rgba([255, 255, 255, 255]));
    let tex = Texture::from_image(&mut ctx, img)?;

    let color_weight = ColorLerp {
        from: [0.1, 0.9, 0.2, 1.0],
        to  : [1.0, 0.1, 0.1, 1.0]
    };
    let debug_colors = ColorLerp {
        from: [0.1, 0.6, 1.0, 1.0],
        to  : [1.0, 1.0, 1.0, 1.0],
    };

    let mut world = World::new();

    event_loop.run(
        move |event: Event<()>, _window_target: _, control_flow: &mut ControlFlow| match event {
            Event::WindowEvent { event: WindowEvent::CloseRequested,  .. } => *control_flow = ControlFlow::Exit,
            Event::MainEventsCleared => ctx.window().request_redraw(),
            Event::RedrawRequested(_) => {
                let mut surface = ctx.surface();
                ctx.clear_color(&mut surface, (0.4, 0.4, 0.4, 1.0));

                let mut draw_config = DrawConfig {
                    scale: (world.tile_pixel_size() as u32, world.tile_pixel_size() as u32),
                    ..Default::default()
                };

                if let Some(dungeon) = world.dungeon() {
                    let tiles = dungeon.tiles();
                    let tile_size = world.tile_pixel_size() as i32;
                    for addr in tiles.tile_addresses() {
                        if let Some((_room_id, room_weight)) = tiles[addr] {
                            if room_weight >= 1.0 {
                                // for debugging
                                if room_weight <= 2.0 {
                                    draw_config.color_modulation = debug_colors.lerp_as_matrix(room_weight - 1.0);
                                } else {
                                    draw_config.color_modulation = IDENTITY;
                                }
                            } else {
                                draw_config.color_modulation = color_weight.lerp_as_matrix(room_weight);
                            }
                            let x = addr.x as i32 * tile_size;
                            let y = addr.y as i32 * tile_size;
                            ctx.draw(&mut surface, &tex, (x, y), &draw_config);

                        }
                    }

                    for (wall_addr, wall_type) in dungeon.walls().iter() {
                        match *wall_type {
                            WallType::Clear => (),
                            WallType::Wall => {
                                let TileAddress { x, y } = wall_addr.tile();
                                let (base_to, base_from) = match wall_addr.direction() {
                                    CompassDirection::North => ((0,1), (1,1)),
                                    CompassDirection::East => ((1,1), (1,0)),
                                    CompassDirection::South => ((0,0), (1,0)),
                                    CompassDirection::West => ((0,0), (0,1)),
                                };
                                let to_px = |(dx, dy)| {
                                    ((dx + x as i32) * tile_size, (dy + y as i32) * tile_size)
                                };
                                ctx.debug_line(&mut surface, to_px(base_from), to_px(base_to), (0.0, 0.0, 0.0, 1.0));
                            }
                            WallType::Door => {
                                let TileAddress { x, y } = wall_addr.tile();
                                match wall_addr.direction() {
                                    CompassDirection::North => draw_horizontal_door(&mut ctx, &mut surface, tile_size, x as f32, y as f32 + 1.0),
                                    CompassDirection::East => draw_vertical_door(&mut ctx, &mut surface, tile_size, (x + 1) as f32, y as f32),
                                    CompassDirection::South => draw_horizontal_door(&mut ctx, &mut surface, tile_size, x as f32, y as f32),
                                    CompassDirection::West => draw_vertical_door(&mut ctx, &mut surface, tile_size, x as f32, y as f32),
                                }
                            }
                        }
                    }
                }

                ctx.present(surface).unwrap();
            },
            Event::NewEvents(StartCause::Init) => {
                println!("Init! {:?}", ctx.window_dimensions());
                world.regenerate(window_bounds(&ctx));
                ctx.window().request_redraw();
            },
            Event::WindowEvent { event: WindowEvent::ReceivedCharacter(' '), .. } => {
                // regenerate the world when space is pressed
                world.regenerate(window_bounds(&ctx));
            }
            Event::WindowEvent { event: WindowEvent::Resized(size), .. } => {
                println!("Resize: {:?} or {:?}", ctx.window_dimensions(), size);
            },
            _ => (),
        }
    )
}

const DOOR_COLOR: (f32, f32, f32, f32) = (0.25, 0.25, 0.25, 1.0);

fn draw_horizontal_door<T: DrawTarget>(ctx: &mut Context, target: &mut T, tile_size: i32, x: f32, y: f32) {
    // let TileAddress { x: xu, y: yu } = upper_left;
    // let x = xu as f32;
    // let y = yu as f32;
    let pixel_pos = |xt: f32, yt: f32| { ((xt * tile_size as f32) as i32, (yt * tile_size as f32) as i32) };
    ctx.debug_line(target, pixel_pos(x, y), pixel_pos(x + 0.25, y), DOOR_COLOR);
    ctx.debug_line(target, pixel_pos(x + 0.75, y), pixel_pos(x + 1.0, y), DOOR_COLOR);
    ctx.debug_line(target, pixel_pos(x + 0.25, y + 0.1), pixel_pos(x + 0.25, y - 0.1), DOOR_COLOR);
    ctx.debug_line(target, pixel_pos(x + 0.75, y + 0.1), pixel_pos(x + 0.75, y - 0.1), DOOR_COLOR);
}
fn draw_vertical_door<T: DrawTarget>(ctx: &mut Context, target: &mut T, tile_size: i32, x: f32, y: f32) {
    // let TileAddress { x: xu, y: yu } = upper_left;
    // let x = xu as f32;
    // let y = yu as f32;
    let pixel_pos = |xt: f32, yt: f32| { ((xt * tile_size as f32) as i32, (yt * tile_size as f32) as i32) };
    ctx.debug_line(target, pixel_pos(x, y), pixel_pos(x, y + 0.25), DOOR_COLOR);
    ctx.debug_line(target, pixel_pos(x, y + 0.75), pixel_pos(x, y + 1.0), DOOR_COLOR);
    ctx.debug_line(target, pixel_pos(x - 0.1, y + 0.25), pixel_pos(x + 0.1, y + 0.25), DOOR_COLOR);
    ctx.debug_line(target, pixel_pos(x - 0.1, y + 0.75), pixel_pos(x + 0.1, y + 0.75), DOOR_COLOR);
}

struct ColorLerp {
    pub from: [f32; 4],
    pub to: [f32; 4]
}
impl ColorLerp {
    fn lerp(&self, dist: f32) -> [f32; 4] {
        let from = self.from;
        let to = self.to;
        [
            from[0] + (to[0] - from[0]) * dist,
            from[1] + (to[1] - from[1]) * dist,
            from[2] + (to[2] - from[2]) * dist,
            from[3] + (to[3] - from[3]) * dist,
        ]
    }
    fn lerp_as_matrix(&self, dist: f32) -> [[f32; 4]; 4] {
        let [r,g,b,a] = self.lerp(dist);
        [
            [r, 0.0, 0.0, 0.0],
            [0.0, g, 0.0, 0.0],
            [0.0, 0.0, b, 0.0],
            [0.0, 0.0, 0.0, a]
        ]
    }
}

fn window_bounds(ctx: &Context) -> Rect {
    Corners((0, 0), (ctx.window_width() as i32, ctx.window_height() as i32)).into()
}


type RoomData = Option<(RoomId, f32)>;

struct World {
    tile_pixel_size: usize,
    dungeon: Option<GridDungeon<RoomData>>,
    // generator: Box<dyn GridDungeonGenerator<RoomData>>
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
            generator
        }
    }

    fn regenerate(&mut self, pixel_bounds: Rect) -> () {
        let grid_width = pixel_bounds.width() as usize / self.tile_pixel_size;
        let grid_height = pixel_bounds.height() as usize / self.tile_pixel_size;
        let dungeon = self.generator.generate(grid_width, grid_height);
        self.dungeon = Some(dungeon);
    }

    fn tile_pixel_size(&self) -> usize { self.tile_pixel_size }
    fn dungeon(&self) -> &Option<GridDungeon<RoomData>> { &self.dungeon }
}

