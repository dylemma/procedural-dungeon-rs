use std::{
    ops::Deref,
    sync::mpsc::{channel, Receiver, TryRecvError},
    thread,
};

use amethyst::{
    assets::{
        AssetLoaderSystemData,
        Handle,
    },
    core::{
        ArcThreadPool,
        bundle::SystemBundle,
        geometry::Plane,
        math::{Point2, Vector2, Vector3},
        SystemDesc,
        timing::Time,
        transform::{Parent, Transform, TransformBundle},
    },
    derive::SystemDesc,
    ecs::{
        Component,
        DenseVecStorage,
        Entities,
        Entity,
        Join,
        join::JoinIter,
        HashMapStorage,
        NullStorage,
        Read,
        ReadExpect,
        storage::{ReadStorage, WriteStorage},
        System,
        Write,
        world::EntitiesRes,
    },
    Error,
    input::{
        Axis,
        Button,
        Bindings,
        BindingError,
        BindingTypes,
        InputBundle,
        InputEvent,
        InputHandler,
        StringBindings
    },
    prelude::*,
    renderer:: {
        camera::Camera,
        light::{Light, PointLight, SpotLight},
        Mesh,
        mtl::{Material, MaterialDefaults},
        palette::{
            rgb::Rgb,
            Srgba,
        },
        plugins::{RenderPbr3D, RenderToWindow},
        RenderingBundle,
        rendy::mesh::{Normal, Position, Tangent, TexCoord},
        resources::Tint,
        shape::Shape,
        transparent::Transparent,
        types::DefaultBackend,
    },
    shred::{Dispatcher, DispatcherBuilder, Resource, SystemData},
    shrev::{EventChannel, ReaderId},
    utils::application_root_dir,
    window::{DisplayConfig, ScreenDimensions},
    winit::MouseButton,
};

use crate::dungeon::*;
use crate::graph::*;
use rand::{Rng, seq::SliceRandom, thread_rng};
use std::fmt::{Display, Formatter};
use std::thread::current;
use ncollide2d::query::PointQuery;
use ncollide2d::shape::{Cuboid, Ball};
use nalgebra::Isometry2;

mod dungeon;
mod geom;
mod graph;
mod tile;

type MyCustomVertices = (Vec<Position>, Vec<Normal>, Vec<Tangent>, Vec<TexCoord>);

fn main() -> amethyst::Result<()> {
    amethyst::start_logger(Default::default());

    let app_root = application_root_dir()?;
    let assets_dir = app_root.join("assets");

    let display_config = DisplayConfig {
        title: "Amethyst".to_string(),
        dimensions: Some((1024, 768)),
        ..Default::default()
    };

    let game_data = GameDataBuilder::default()
        .with_bundle(DefaultLayoutBundle)?
        .with_bundle(TransformBundle::new())?
        .with_bundle(InputBundle::<CustomBindings>::new()
            .with_bindings(hardcoded_input_bindings()?)
        )?
        .with_bundle(RenderingBundle::<DefaultBackend>::new()
            .with_plugin(
                RenderToWindow::from_config(display_config).with_clear([0.529, 0.808, 0.98, 1.0])
            )
            .with_plugin(RenderPbr3D::default())
        )?
        // .with_bundle(NavigationBundle)?
        .with_bundle(PlayerBundle)?
        .with(NavigationSystem.pausable(GameplayState::Playable), "navigation_system", &[])
        // .with(PlayerSystem.pausable(GameplayState::Playable), "PlayerSystem", &["input_system"])
    ;

    let initial_state = Init { generator_grid_size: [100, 100] };
        //Generating::new(100, 100);

    let mut game = Application::new(assets_dir, initial_state, game_data)?;
    game.run();

    Ok(())
}

// -------------------------------------------------------------------------

type CustomInputHandler = InputHandler<CustomBindings>;
type CustomInputEvent = InputEvent<CustomBindings>;

#[derive(Debug)]
struct CustomBindings;
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum InputAction {
    Move
}
impl Display for InputAction {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self)
    }
}
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
enum InputAxis {
    Zoom
}
impl Display for InputAxis {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl BindingTypes for CustomBindings {
    type Axis = InputAxis;
    type Action = InputAction;
}
fn hardcoded_input_bindings() -> Result<Bindings<CustomBindings>, BindingError<CustomBindings>> {
    let mut bindings = Bindings::<CustomBindings>::new();
    bindings.insert_axis(InputAxis::Zoom, Axis::MouseWheel { horizontal: false })?;
    bindings.insert_action_binding(InputAction::Move, vec![Button::Mouse(MouseButton::Left)])?;
    Ok(bindings)
}

#[derive(Component, Default)]
#[storage(NullStorage)]
struct Player;

#[derive(Component, Default)]
#[storage(NullStorage)]
struct PlayerCursor;

#[derive(Component)]
#[storage(DenseVecStorage)]
struct NavigationCursor { entity: Entity }

#[derive(Eq, PartialEq)]
enum GameplayState {
    Playable,
    Paused
}
impl Default for GameplayState {
    fn default() -> Self {
        GameplayState::Paused
    }
}

#[derive(SystemDesc)]
#[system_desc(name(PlayerSystemDesc))]
struct PlayerSystem;
//{
   // #[system_desc(event_channel_reader)]
   // reader_id: ReaderId<CustomInputEvent>
//}
impl PlayerSystem {
    // fn new(reader_id: ReaderId<CustomInputEvent>) -> Self {
    //     PlayerSystem { reader_id }
    // }
}
impl <'s> System<'s> for PlayerSystem {
    type SystemData = (
        ReadStorage<'s, Player>,
        ReadStorage<'s, PlayerCursor>,
        ReadStorage<'s, Camera>,
        WriteStorage<'s, Transform>,
        WriteStorage<'s, Navigation>,
        ReadExpect<'s, ScreenDimensions>,
        Read<'s, Option<Layout>>,
        Read<'s, CustomInputHandler>,
    );

    fn run(&mut self, (players, player_cursors, cameras, mut transforms, mut navigations, screen_dims, layout_opt, input): Self::SystemData) {
        if let Some(mouse_pos) = input.mouse_position() {
            let mouse_pos_world = (&cameras, &transforms).join().next().map(
                |(camera, camera_transform): (&Camera, &Transform)| {
                    let camera: &Camera = camera; // workaround to make IntelliJ work
                    let ray = camera.projection().screen_ray(
                        Point2::new(mouse_pos.0, mouse_pos.1),
                        Vector2::new(screen_dims.width(), screen_dims.height()),
                        camera_transform,
                    );
                    let dist = ray.intersect_plane(&Plane::with_z(0.0)).unwrap();
                    let mouse_pos_world = ray.at_distance(dist);
                    mouse_pos_world.coords
                }
            );

            if let Some(cursor_pos) = mouse_pos_world {
                for (cursor, cursor_transform) in (&player_cursors, &mut transforms).join() {
                    let cursor_transform: &mut Transform = cursor_transform;
                    cursor_transform.set_translation(cursor_pos.clone());
                }

                if let Some(true) = input.action_is_down(&InputAction::Move) {
                    if let Some(layout) = layout_opt.deref() {
                        for (_, nav) in (&players, &mut navigations).join() {
                            let nav: &mut Navigation = nav; // IntelliJ workaround
                            nav.set_goal(&layout.floor_graph, [cursor_pos.x, cursor_pos.y]);
                        }
                    }
                }
            }
        }
    }
}

#[derive(Component)]
#[storage(DenseVecStorage)]
struct NavigationSpeed(f32);

struct NavigationSystem;
impl <'s> System<'s> for NavigationSystem {
    type SystemData = (
        ReadStorage<'s, NavigationSpeed>,
        WriteStorage<'s, Navigation>,
        WriteStorage<'s, Transform>,
        ReadStorage<'s, NavigationCursor>,
        ReadExpect<'s, Time>,
    );

    fn run(&mut self, (speeds, mut navigations, mut transforms, cursors, time): Self::SystemData) {
        let time_step = time.delta_seconds();
        for (speed, nav, transform) in (&speeds, &mut navigations, &mut transforms).join() {
            let nav: &mut Navigation = nav; // IntelliJ workaround
            let transform: &mut Transform = transform; // IntelliJ workaround
            let step = time_step * speed.0;
            nav.advance(step);
            if let Some([x, y]) = nav.pos() {
                transform.set_translation_xyz(*x, *y, 0.0);
            }
        }

        for(cursor, transform) in (&cursors, &mut transforms).join() {
            if let Some(nav) = navigations.get(cursor.entity) {
                if let Some(path) = &nav.path {
                    let [x, y] = path.end_point();
                    transform.set_translation_xyz(*x, *y, 0.0);
                }
            }
        }
    }
}

struct PlayerBundle;
impl <'a, 'b> SystemBundle<'a, 'b> for PlayerBundle {
    fn build(self, world: &mut World, dispatcher: &mut DispatcherBuilder<'a, 'b>) -> Result<(), Error> {
        let player_sys = PlayerSystemDesc::default().build(world)
            .pausable(GameplayState::Playable);
        dispatcher.add(player_sys, "player_system", &["input_system"]);
        Ok(())
    }
}

type NavPoint = (FloorNodeId, [f32; 2]);

#[derive(Debug)]
enum NavPath {
    WithinNode(NavPoint),
    InterNode(FloorNodeId, Vec<NavPoint>, usize)
}
impl NavPath {
    fn current_target(&self) -> Option<&NavPoint> {
        match self {
            NavPath::WithinNode(target) => Some(target),
            NavPath::InterNode(_, waypoints, index) => waypoints.get(*index),
        }
    }
    fn into_next(self) -> Option<NavPath> {
        match self {
            NavPath::WithinNode(_) => None,
            NavPath::InterNode(end_id, waypoints, index) => {
                if index + 1 >= waypoints.len() {
                    None
                } else {
                    Some(NavPath::InterNode(end_id, waypoints, index + 1))
                }
            }
        }
    }
    fn end_node(&self) -> &FloorNodeId {
        match self {
            NavPath::WithinNode((id, _)) => id,
            NavPath::InterNode(id, _, _) => id,
        }
    }
    fn end_point(&self) -> &[f32; 2] {
        match self {
            NavPath::WithinNode((_, p)) => p,
            NavPath::InterNode(_, v, _) => &v[v.len() - 1].1,
        }
    }
    fn replace_final_point(&mut self, point: [f32; 2]) {
        match self {
            NavPath::WithinNode((_id, p)) => *p = point,
            NavPath::InterNode(_, v, _) => v.last_mut().expect("Last step of InterNode").1 = point,
        };
    }
}
#[derive(Component)]
#[storage(DenseVecStorage)]
struct Navigation {
    current: Option<NavPoint>,
    // current_node: FloorNodeId,
    // current_pos: [f32; 2],
    path: Option<NavPath>,
}
impl Default for Navigation {
    fn default() -> Self {
        Navigation {
            current: None,
            path: None,
        }
    }
}
impl Navigation {
    fn new(current_node: FloorNodeId, current_pos: [f32; 2]) -> Self {
        Navigation {
            current: Some((current_node, current_pos)),
            path: None,
        }
    }

    fn pos(&self) -> Option<&[f32; 2]> {
        self.current.as_ref().map(|(_id, p)| p)
    }

    fn clear_and_reset(&mut self, current_node: FloorNodeId, current_pos: [f32; 2]) {
        self.current = Some((current_node, current_pos));
        self.path = None;
    }

    fn set_goal(&mut self, graph: &DungeonFloorGraph, goal_pos: [f32; 2]) {
        if let Some((current_node, current_pos)) = self.current {

            // compatibility between f32 from Amethyst and f64 from my old code
            let current_pos_64 = [current_pos[0] as f64, current_pos[1] as f64];
            let goal_pos_64 = [goal_pos[0] as f64, goal_pos[1] as f64];

            let goal_node_opt = graph.node_at_point(&goal_pos_64).map(|n| n.id().clone());

            match goal_node_opt {
                None => {
                    // TODO: if the goal_pos is not in any room, find the edge of the current room in the direction of that pos and navigate there instead
                    let current_room_aabb = graph.get_bounds(current_node);
                    let projection = current_room_aabb.project_point(&Isometry2::identity(), &goal_pos_64.into(), true);
                    let projected_pos = [projection.point.x as f32, projection.point.y as f32];
                    self.path = Some(NavPath::WithinNode((current_node, projected_pos)));
                },
                Some(goal_node) => {

                    // if the current path's final node is the same node as the new goal,
                    // all we have to do is put the new goal point into that path
                    if let Some(path) = &mut self.path {
                        if goal_node == *path.end_node() {
                            path.replace_final_point(goal_pos);
                            return;
                        }
                    }

                    // if the current node is the same as the goal node, we can use
                    // the "same node" path and not have to do a pathfinding operation
                    if goal_node == current_node {
                        self.path = Some(NavPath::WithinNode((goal_node, goal_pos)));
                        return
                    }

                    // if we got this far, the easy routes failed,
                    // so we have to use pathfinding to create an `InterNode` path
                    let start_proj = graph.get_bounds(current_node).project_point(&Isometry2::identity(), &current_pos_64.into(), true);
                    if !start_proj.is_inside {
                        println!("edge case hit!");
                    }
                    let start = [start_proj.point.x, start_proj.point.y];

                    self.path = graph.find_route(&start, &goal_pos_64, 256).map(|route_64| {
                        let route_32: Vec<_> = route_64.into_iter().map(|(id, [px, py])| (id, [px as f32, py as f32])).collect();
                        NavPath::InterNode(goal_node, route_32, 0)
                    });

                    if self.path.is_none() {
                        // let end_proj = graph.get_bounds(current_node).project_point(&Isometry2::identity(), &goal_pos_64.into(), true);
                        println!("Still no path?");
                    }
                }
            };
        }
    }

    fn advance(&mut self, step: f32) -> bool {

        if let Some((current_node, current_pos)) = self.current {
            if let Some(path) = self.path.take() {
                if let Some((target_node, target_pos)) = path.current_target() {
                    use vecmath::*;
                    let to_target = vec2_sub(*target_pos, current_pos);
                    let dist = vec2_len(to_target);

                    if dist < step {
                        // we are able to reach the next point within the current step,
                        // so we will need to advance the navigation state,
                        // then recurse to spend the rest of the step
                        self.current = Some((*target_node, *target_pos));
                        self.path = path.into_next();
                        return self.advance(step - dist)
                    } else {
                        // move as far as the subject can towards the target pos,
                        // with no need to change the node or the path
                        let movement = vec2_scale(to_target, step / dist);
                        self.current = Some((
                            current_node,
                            vec2_add(current_pos, movement)
                        ));
                        self.path = Some(path); // put the path back because we 'took' it earlier

                        // navigation is not yet complete
                        return false
                    }
                }
            }
        }

        // no current pos, no path, or no next target;
        // all of these can be interpreted as having completed navigation
        true
    }
}

// struct NavigationBundle;
// impl <'a, 'b> SystemBundle<'a, 'b> for NavigationBundle {
//     fn build(self, world: &mut World, dispatcher: &mut DispatcherBuilder<'a, 'b>) -> Result<(), Error> {
//         let nav_sys = NavigationSystemDesc::default().build(world);
//         dispatcher.add(nav_sys, "navigation", &["input_system"]);
//         Ok(())
//     }
// }
// #[derive(SystemDesc)]
// #[system_desc(name(NavigationSystemDesc))]
// struct NavigationSystem {
//     #[system_desc(event_channel_reader)]
//     reader_id: ReaderId<CustomInputEvent>
// }
// impl NavigationSystem {
//     fn new(reader_id: ReaderId<CustomInputEvent>) -> Self {
//         NavigationSystem { reader_id }
//     }
// }
// impl NavigationSystem {
//     fn new(world: &mut World) -> Self {
//         <Self as System<'_>>::setup(world);
//         let reader_id = world.fetch_mut::<EventChannel<CustomInputEvent>>().register_reader();
//         Self { reader_id }
//     }
// }
// impl <'s> System<'s> for NavigationSystem {
//     type SystemData = (
//         Read<'s, EventChannel<CustomInputEvent>>,
//         // ReadStorage<'s, Player>,
//         ReadStorage<'s, PlayerCursor>,
//         // ReadStorage<'s, Camera>,
//         WriteStorage<'s, Transform>,
//         Option<Read<'s, Option<Layout>>>,
//         // ReadExpect<'s, ScreenDimensions>,
//         // Read<'s, CustomInputHandler>,
//     );
//
//     fn run(&mut self, (input_events, player_cursors, transforms, current_layout): Self::SystemData) {
//         for event in input_events.read(&mut self.reader_id) {
//             match event {
//                 InputEvent::ActionPressed(InputAction::Move) => {
//                     println!("MOVE pressed");
//
//                     let destination_point = (&player_cursors, &transforms).join().next().map(|(_, cursor_transform)| {
//                         let pos = cursor_transform.translation();
//                         let x: f32 = pos.x;
//                         let y: f32 = pos.y;
//                         [x, y]
//                     });
//                     println!("Destination: {:?}", destination_point);
//                 },
//                 InputEvent::AxisMoved { axis: InputAxis::Zoom, value } => println!("ZOOM {}", value),
//                 _ => ()
//             };
//         }
//     }
//
// }

// -------------------------------------------------------------------------

struct Init {
    generator_grid_size: [usize; 2]
}
impl SimpleState for Init {
    fn on_start(&mut self, state_data: StateData<'_, GameData<'_, '_>>) {
        let world = state_data.world;

        // only necessary if we don't have a system that uses these components
        // world.register::<Player>();
        // world.register::<PlayerCursor>();

        let player_entity = Init::init_player(world);
        Init::init_camera(world, player_entity);
        Init::init_ground(world);
        Init::init_light(world);
        Init::init_player_cursor(world);
        Init::init_player_nav_cursor(world, player_entity);
    }

    fn update(&mut self, _data: &mut StateData<'_, GameData<'_, '_>>) -> SimpleTrans {
        let [w, h] = self.generator_grid_size;
        Trans::Replace(Box::new(
            Generating::new(w, h)
        ))
    }
}
impl Init {
    fn init_player(world: &mut World) -> Entity {

        let player_entity = world.create_entity()
            .with(Player)
            .with(Navigation::default())
            .with(NavigationSpeed(5.0))
            .with(Transform::default())
            .build();

        // render a cylinder where the player is, plus a slight transform
        let mesh = Init::load_simple_mesh(world, Shape::Cylinder(16, None), Some((0.2, 0.2, 0.2)));
        let texture = Init::load_default_material(world);
        let transform = Transform::from(Vector3::new(0.0, 0.0, 0.2));
        let _player_render = world.create_entity()
            .with(mesh)
            .with(texture)
            .with(transform)
            .with(Tint(Srgba::new(0.2, 0.2, 0.0, 1.0)))
            .with(Parent{ entity: player_entity })
            .build();

        player_entity
    }
    fn init_player_cursor(world: &mut World) {
        let light: Light = PointLight {
            intensity: 5.0,
            color: Rgb::new(1.0, 0.0, 0.5),
            ..PointLight::default()
        }.into();

        let mesh = Init::load_simple_mesh(world, Shape::Cube, Some((0.1, 0.1, 0.1)));
        let mat = Init::load_default_material(world);

        world.create_entity()
            .with(PlayerCursor)
            // .with(light)
            .with(mesh)
            .with(Transparent)
            .with(mat)
            .with(Transform::default())
            .with(Tint(Srgba::new(1.0, 0.0, 0.5, 0.1)))
            .build();
    }
    fn init_player_nav_cursor(world: &mut World, player_entity: Entity) {
        let mesh = Init::load_simple_mesh(world, Shape::IcoSphere(None), Some((0.1, 0.1, 0.1)));
        let mat = Init::load_default_material(world);

        world.create_entity()
            .with(NavigationCursor { entity: player_entity })
            .with(mesh)
            .with(mat)
            .with(Transform::default())
            .with(Tint(Srgba::new(0.0, 1.0, 0.2, 1.0)))
            .build();
    }

    fn init_camera(world: &mut World, player_entity: Entity) {
        let camera = world.exec(|screen: ReadExpect<'_, ScreenDimensions>| {
            Camera::standard_3d(screen.width(), screen.height())
        });

        let mut transform = Transform::default();
        // up close and personal:
        // transform.set_translation_xyz(-1.0, -1.0, 2.0);

        // a little far away
        // transform.set_translation_xyz(-2.0, -2.0, 5.0);

        // a lot far away
        transform.set_translation_xyz(-2.0, -2.0, 10.0);
        transform.face_towards(Vector3::new(0.0, 0.0, 0.0), Vector3::z());

        world.create_entity()
            .with(camera)
            .with(transform)
            .with(Parent::new(player_entity))
            .build();
    }

    fn init_ground(world: &mut World) {
        let mesh = Init::load_simple_mesh(world, Shape::Plane(None), Some((10.0, 10.0, 1.0)));
        let ground_mat = Init::load_default_material(world);

        let mut transform = Transform::default();
        transform.set_translation_z(-0.1);

        world.create_entity()
            .with(mesh)
            .with(ground_mat)
            .with(transform)
            .with(Tint(Srgba::new(0.3, 0.2, 0.0, 1.0)))
            .build();
    }

    fn init_light(world: &mut World) {
        let light: Light = PointLight {
            intensity: 10.0,
            color: Rgb::new(1.0, 1.0, 1.0),
            ..PointLight::default()
        }.into();

        let mut transform = Transform::default();
        transform.set_translation_xyz(0.0, 0.0, 20.0);

        world.create_entity()
            .with(light)
            .with(transform)
            .build();
    }

    fn load_default_material(world: &mut World) -> Handle<Material> {
        let mat_defaults = world.read_resource::<MaterialDefaults>().0.clone();
        world.exec(|loader: AssetLoaderSystemData<'_, Material>| {
            loader.load_from_data(Material { ..mat_defaults }, ())
        })
    }

    fn load_simple_mesh(world: &mut World, shape: Shape, scale: Option<(f32, f32, f32)>) -> Handle<Mesh> {
        world.exec(|loader: AssetLoaderSystemData<'_, Mesh>| {
            loader.load_from_data(
                shape
                    .generate::<MyCustomVertices>(scale)
                    .into(),
                ()
            )
        })
    }
}

// -------------------------------------------------------------------------

struct Generating {
    grid_width: usize,
    grid_height: usize,
    result_rcv: Option<Receiver<Layout>>,
    wait_counter: u32,
}
impl Generating {
    fn new(grid_width: usize, grid_height: usize) -> Self {
        Generating {
            grid_width,
            grid_height,
            result_rcv: None,
            wait_counter: 0,
        }
    }
}
impl SimpleState for Generating {
    fn on_start(&mut self, state_data: StateData<'_, GameData<'_, '_>>) {
        assert!(self.result_rcv.is_none(), "Generating state should not be initialized with a receiver");

        let layout_strategy = state_data.world.fetch::<LayoutStrategy>().deref().clone();

        println!("Starting layout generator...");

        let grid_width = self.grid_width;
        let grid_height = self.grid_height;

        let (send, recv) = channel();
        thread::spawn(move || {
            let layout = layout_strategy.generate(grid_width, grid_height);
            send.send(layout).unwrap();
        });

        self.result_rcv = Some(recv);
    }

    fn fixed_update(&mut self, state_data: StateData<'_, GameData<'_, '_>>) -> SimpleTrans {
        if let Some(recv) = &self.result_rcv {
            match recv.try_recv() {
                Ok(layout) => {
                    println!("Generation complete after {} ticks", self.wait_counter);

                    // put the "layout" in the world as a resource, then switch state to "running"
                    *state_data.world.entry::<Option<Layout>>().or_insert(None) = Some(layout);
                    // *state_data.world.write_resource::<Option<Layout>>() = Some(layout);
                    Trans::Switch(Box::new(Running::new()))
                },
                Err(TryRecvError::Disconnected) => {
                    println!("Generator went weird. Quitting instead");
                    Trans::Quit
                },
                Err(TryRecvError::Empty) => {
                    self.wait_counter += 1;
                    Trans::None
                }
            }
        } else {
            println!("Generator has no receive channel? That's weird so I'm quitting");
            Trans::Quit
        }
    }
}

// -------------------------------------------------------------------------

struct Running {
    added_entities: Vec<Entity>,
}
impl Running {
    fn new() -> Self {
        Running {
            added_entities: Vec::new(),
        }
    }

    fn init_player_navigation(&self, world: &mut World) {
        let (players, mut navigations, mut transforms, layout_opt): (
            ReadStorage<Player>,
            WriteStorage<Navigation>,
            WriteStorage<Transform>,
            Read<Option<Layout>>,
        ) = world.system_data();
        let mut rng = thread_rng();

        if let Some(layout) = &*layout_opt {
            println!("init player pos to...");
            let graph = &layout.floor_graph;
            for (_player, navigation, transform) in (&players, &mut navigations, &mut transforms).join() {
                println!("Found a player");
                if let Some(node) = graph.nodes().choose(&mut rng) {
                    let bounds = graph.get_bounds(*node.id()).center();
                    let pos = [bounds.x as f32, bounds.y as f32];
                    println!("Move player to {:?}", pos);
                    transform.set_translation_xyz(pos[0], pos[1], 0.0);
                    navigation.clear_and_reset(*node.id(), pos);
                }

            }
        } else {
            println!("No layout yet?");
        }
    }

    fn add_floor_entities(&mut self, world: &mut World) {

        let floor_aabbs: Vec<_> = world.read_resource::<Option<Layout>>().deref().iter()
            .flat_map(|layout| {
                layout.floor_graph.nodes().iter().map(move |node| layout.floor_graph.get_bounds(*node.id()))
            })
            .map(|aabb| {
                let center = aabb.center();
                let half_extents = aabb.half_extents();
                let scale = Some((half_extents.x as f32, half_extents.y as f32, 1.0));
                let translation = Vector3::new(center.x, center.y, 0.0);
                (scale, translation)
            })
            .collect();

        let mat_defaults = world.read_resource::<MaterialDefaults>().0.clone();
        let floor_mat = world.exec(|loader: AssetLoaderSystemData<'_, Material>| {
            loader.load_from_data(Material { ..mat_defaults }, ())
        });

        for (scale, translation) in floor_aabbs {
            // let center = aabb.center();
            // let half_extents = aabb.half_extents();
            // let scale = Some((half_extents.x as f32, half_extents.y as f32, 1.0));
            // let translation = Vector3::new(center.x, center.y, 0.0);

            let mesh = world.exec(|loader: AssetLoaderSystemData<'_, Mesh>| {
                loader.load_from_data(
                    Shape::Plane(None)
                        .generate::<MyCustomVertices>(scale)
                        .into(),
                    ()
                )
            });
            let mut transform = Transform::from(translation);

            let entity = world.create_entity()
                .with(mesh.clone())
                .with(floor_mat.clone())
                .with(transform)
                .with(Tint(Srgba::new(1.0, 1.0, 1.0, 1.0)))
                .build();
            self.added_entities.push(entity);
        }
    }
}
impl SimpleState for Running {
    fn on_start(&mut self, state_data: StateData<'_, GameData<'_, '_>>) {
        let world = state_data.world;

        self.add_floor_entities(world);
        self.init_player_navigation(world);

        println!("Playable!");
        *world.write_resource::<GameplayState>() = GameplayState::Playable;
    }

    fn on_stop(&mut self, state_data: StateData<'_, GameData<'_, '_>>) {
        state_data.world.exec(|entities: Write<'_, EntitiesRes>| {
            for entity in self.added_entities.iter() {
                entities.delete(*entity).expect("Failed to delete entity");
            }
        });
    }


    fn update(&mut self, data: &mut StateData<'_, GameData<'_, '_>>) -> SimpleTrans {
        Trans::None
    }
}

// -------------------------------------------------------------------------

struct Layout {
    dungeon: BasicGridDungeon,
    floor_graph: DungeonFloorGraph,
}

struct DefaultLayoutBundle;
impl <'a, 'b> SystemBundle<'a, 'b> for DefaultLayoutBundle {
    fn build(self, world: &mut World, _dispatcher: &mut DispatcherBuilder<'a, 'b>) -> Result<(), Error> {
        world.entry::<LayoutStrategy>().or_insert_with(|| LayoutStrategy::default());
        Ok(())
    }
}

#[derive(Clone)]
struct LayoutStrategy {
    tile_size_units: f64,
    wall_margin_units: f64,
    door_width_units: f64,
    generator: GeneratorStrategy<&'static str>
}
impl Default for LayoutStrategy {
    fn default() -> Self {
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
        LayoutStrategy {
            tile_size_units: 1.0,
            wall_margin_units: 0.1,
            door_width_units: 0.5,
            generator,
        }
    }
}
impl LayoutStrategy {
    fn generate(&self, grid_width: usize, grid_height: usize) -> Layout {
        let dungeon = self.generator.generate(grid_width, grid_height);
        let floor_graph = graph::decompose(
            &dungeon,
            self.tile_size_units,
            self.wall_margin_units,
            self.door_width_units
        );
        Layout { dungeon, floor_graph }
    }
}