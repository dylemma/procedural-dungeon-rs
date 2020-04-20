mod dungeon;

use crow::{
    glutin::{
        event::{Event, WindowEvent, StartCause},
        event_loop::{ControlFlow, EventLoop},
        window::WindowBuilder,
    },
    Context,
    // DrawConfig, Texture,
};

#[allow(unused)]
use crate::dungeon::{Rect, DungeonGenerator, NaiveRandomDungeon, Corners, SliceAwayGenerator};
use crate::dungeon::RandomRoomGridGenerator;


fn main() -> Result<(), crow::Error> {
    let event_loop = EventLoop::new();
    let mut ctx = Context::new(WindowBuilder::new(), &event_loop)?;

    // let generator = Box::new(NaiveRandomDungeon{ count: 20 });
    // let generator = Box::new(SliceAwayGenerator);
    let generator = Box::new(RandomRoomGridGenerator::new(24));

    let mut world = World::new(generator);

    event_loop.run(
        move |event: Event<()>, _window_target: _, control_flow: &mut ControlFlow| match event {
            Event::WindowEvent { event: WindowEvent::CloseRequested,  .. } => *control_flow = ControlFlow::Exit,
            Event::MainEventsCleared => ctx.window().request_redraw(),
            Event::RedrawRequested(_) => {
                let mut surface = ctx.surface();
                ctx.clear_color(&mut surface, (0.4, 0.4, 0.4, 1.0));
                for rect in world.rects() {
                    ctx.debug_rectangle(&mut surface, rect.lower_left(), rect.upper_right(), (1.0, 0.0, 0.2, 1.0));
                }
                ctx.present(surface).unwrap();
            },
            Event::NewEvents(StartCause::Init) => {
                println!("Init! {:?}", ctx.window_dimensions());
                world.regenerate(Some(window_bounds(&ctx)));
                ctx.window().request_redraw();
            },
            Event::WindowEvent { event: WindowEvent::ReceivedCharacter(' '), .. } => {
                // regenerate the world when space is pressed
                world.regenerate(Some(window_bounds(&ctx)));
            }
            Event::WindowEvent { event: WindowEvent::Resized(size), .. } => {
                println!("Resize: {:?} or {:?}", ctx.window_dimensions(), size);
            },
            _ => (),
        }
    )
}

fn window_bounds(ctx: &Context) -> Rect {
    Corners((0, 0), (ctx.window_width() as i32, ctx.window_height() as i32)).into()
}

struct World {
    generator: Box<dyn DungeonGenerator>,
    bounds: Rect,
    rects: Vec<Rect>,
}
impl World {
    pub fn new(generator: Box<dyn DungeonGenerator>) -> World {
        World {
            generator,
            bounds: Rect::from_xywh(0, 0, 10, 10),
            rects: Vec::new(),
        }
    }
    pub fn regenerate(&mut self, new_bounds: Option<Rect>) {
        if let Some(bounds) = new_bounds {
            self.bounds = bounds
        }
        self.rects = self.generator.generate(&self.bounds)
    }
    pub fn rects(&self) -> &Vec<Rect> {
        &self.rects
    }
}
