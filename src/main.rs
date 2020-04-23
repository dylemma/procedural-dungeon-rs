use crow::{Context, DrawConfig, glutin::{
    event::{Event, StartCause, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
}, image::{ImageBuffer, Rgba, RgbaImage}, Texture};
use rand::{Rng, thread_rng};

#[allow(unused)]
use crate::dungeon::{Corners, Rect};
use crate::dungeon::{GridDungeon, GridDungeonGenerator, RandomRoomGridDungeonGenerator, RoomId, RoomSize};

mod dungeon;
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
                            draw_config.color_modulation = color_weight.lerp_as_matrix(room_weight);
                            let x = addr.x as i32 * tile_size;
                            let y = addr.y as i32 * tile_size;
                            ctx.draw(&mut surface, &tex, (x, y), &draw_config);

                            // keeping this as an example:
                            // ctx.debug_rectangle(&mut surface, rect.lower_left(), rect.upper_right(), (0.0, 0.0, 0.0, 1.0));
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

struct ColorLerp {
    pub from: [f32; 4],
    pub to: [f32; 4]
}
impl ColorLerp {
    fn lerp(&self, dist: f32) -> [f32; 4] {
        let from = self.from;
        let to = self.to;
        [
            to[0] + (from[0] - to[0]) * dist,
            to[1] + (from[1] - to[1]) * dist,
            to[2] + (from[2] - to[2]) * dist,
            to[3] + (from[3] - to[3]) * dist,
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
    generator: Box<dyn GridDungeonGenerator<RoomData>>
}
impl World {
    fn new() -> Self {
        let room_chances = vec![
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
            size_opt.into_iter().flat_map(move |size| {
                size.permutations().into_iter().map(move |s| (s, chance))
            })
        }).collect();
        World {
            tile_pixel_size: 24,
            dungeon: None,
            generator: Box::new(RandomRoomGridDungeonGenerator::new(room_chances))
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

