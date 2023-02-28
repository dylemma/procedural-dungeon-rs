use std::ops::DerefMut;
use bevy::prelude::*;
use crate::dungeon::*;
use crate::graph::{decompose, DungeonFloorGraph, FloorNode};
use crate::nav::Navigation;
use crate::WorldParams;
use rand::{seq::SliceRandom, thread_rng};
use crate::convert::point_to_vec2;

pub struct LevelGeneratorPlugin;

impl Plugin for LevelGeneratorPlugin {
    fn build(&self, app: &mut App) {

        let empty_layout = Layout {
            dungeon: GridDungeon::new(0, 0),
            floor_graph: DungeonFloorGraph::new()
        };

        app
            .insert_resource(empty_layout)
            .insert_resource(LayoutRegenerateRequest(true))
            .insert_resource(LayoutStrategy::default())
            .add_system(generate_layout)
            .add_system(seed_regen_requests)
        ;
    }
}

fn seed_regen_requests(
    keys: Res<Input<KeyCode>>,
    mut regen_request: ResMut<LayoutRegenerateRequest>,
) {
    if keys.just_pressed(KeyCode::Space) {
        regen_request.set();
    }
}

fn generate_layout(
    mut regen_request: ResMut<LayoutRegenerateRequest>,
    mut layout: ResMut<Layout>,
    layout_strat: Res<LayoutStrategy>,
    wp: Res<WorldParams>,
    mut navigators_to_reset: Query<(&mut Navigation, &mut Transform), With<ResetNavigationOnLayout>>,
) {
    if regen_request.check_and_reset() {
        /*let Layout { dungeon, floor_graph } = */
        let new_layout = layout_strat.generate(wp.grid_width, wp.grid_height);
        // let x = layout.deref_mut();
        *(layout.deref_mut()) = new_layout;
        // layout.dungeon = dungeon;
        // layout.floor_graph = floor_graph;

        let nodes: Vec<&FloorNode> = layout.floor_graph.nodes().iter().filter(|&node| {
            match node {
                FloorNode::Room { room_state, .. } => room_state.weight().is_none(),
                _ => true,
            }
        }).collect();
        let mut rng = thread_rng();
        for (mut navigator, mut transform) in navigators_to_reset.iter_mut() {
            // pick a random room
            if let Some(node) = nodes.choose(&mut rng) {
                let node_id = *node.id();
                let bounds = layout.floor_graph.get_bounds(node_id);
                let pos = point_to_vec2(bounds.center());
                transform.translation.x = pos.x;
                transform.translation.y = pos.y;
                navigator.clear_and_reset(node_id, pos);
                println!("reset a nav agent to {:?} in node {:?}", pos, node_id);
            }
        }
    }
}

#[derive(Component)]
pub struct ResetNavigationOnLayout;

// TODO: sync room/door sprites (add/remove as needed)
// TODO: handle keyboard input to set the LayoutRegenerateRequest

#[derive(Resource)]
pub struct Layout {
    pub dungeon: BasicGridDungeon,
    pub floor_graph: DungeonFloorGraph,
}

#[derive(Resource)]
struct LayoutRegenerateRequest(bool);
impl LayoutRegenerateRequest {
    fn check_and_reset(&mut self) -> bool {
        let result = self.0;
        self.0 = false;
        result
    }
    fn set(&mut self) {
        self.0 = true;
    }
}

#[derive(Clone, Resource)]
pub struct LayoutStrategy {
    tile_size_units: f32,
    wall_margin_units: f32,
    door_width_units: f32,
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
                GeneratorStep::Clusters { count: 10, iterations: 500 },
                GeneratorStep::Widen { iterations: 500 },
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
    pub fn generate(&self, grid_width: usize, grid_height: usize) -> Layout {
        let dungeon = self.generator.generate(grid_width, grid_height);
        let floor_graph = decompose(
            &dungeon,
            self.tile_size_units,
            self.wall_margin_units,
            self.door_width_units
        );
        Layout { dungeon, floor_graph }
    }
}