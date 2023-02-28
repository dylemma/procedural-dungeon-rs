use bevy::prelude::*;
use bevy::render::camera::{ScalingMode, WindowOrigin};
use bevy::sprite::Anchor;

use crate::convert::*;
use crate::dungeon::{GeneratorStep, RoomState};
use crate::graph::FloorNode;
use crate::systems::levelgen::Layout;
use crate::WorldParams;

pub struct Simple2dRenderPlugin;

impl Plugin for Simple2dRenderPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_startup_system(setup_camera)
            .add_system(room_sprites)
        ;
    }
}

pub fn setup_camera(
    mut commands: Commands,
    wp: Res<WorldParams>,
) {
    commands.spawn(Camera2dBundle {
        projection: OrthographicProjection {
            window_origin: WindowOrigin::Center,
            scaling_mode: ScalingMode::Auto {
                min_width: wp.width(),
                min_height: wp.height(),
            },
            ..default()
        },
        transform: Transform::from_translation((wp.center(), 0.).into()),
        ..default()
    });
}

#[derive(Component)]
pub struct RoomSprite;

pub fn room_sprites(
    mut commands: Commands,
    layout: Res<Layout>,
    existing_sprites: Query<Entity, With<RoomSprite>>,
) {
    if layout.is_added() || layout.is_changed() {
        println!("Update room sprites!");

        // Despawn sprites from the previous layout
        let mut despawn_count = 0u32;
        for old_sprite in existing_sprites.iter() {
            commands.entity(old_sprite).despawn();
            despawn_count += 1;
        }
        println!("Despawned {} old sprites", despawn_count);

        // Spawn new sprites for the new layout
        let mut spawn_count = 0u32;
        for node in layout.floor_graph.nodes().iter() {
            let color = match node {
                FloorNode::Room { room_state, .. } => match room_state {
                    //Color::CYAN,
                    RoomState::Seed => Color::CYAN,
                    RoomState::BiasPath => Color::WHITE,
                    RoomState::ConnectedBy { step: GeneratorStep::Branches { .. }} => Color::ANTIQUE_WHITE,
                    RoomState::ConnectedBy { step: GeneratorStep::Clusters { .. }} => Color::BEIGE,
                    RoomState::ConnectedBy { step: GeneratorStep::Widen { .. }} => Color::BISQUE,
                    RoomState::Pending { weight } => Color::YELLOW_GREEN * [*weight; 3],
                }
                FloorNode::Door { .. } => Color::ALICE_BLUE,
            };
            let size: Vec2 = vector_to_vec2(node.world_bounds().extents());
            let bottom_left = point_to_vec2(node.world_bounds().mins);
            // println!("add node {:?}, @ {:?} with size {:?}", node.id(), size, bottom_left);
            commands
                .spawn((
                    SpriteBundle {
                        sprite: Sprite {
                            custom_size: Some(size),
                            anchor: Anchor::BottomLeft,
                            color,
                            ..default()
                        },
                        transform: Transform::from_translation((bottom_left, -10.).into()),
                        ..default()
                    },
                    RoomSprite,
                ));
            spawn_count += 1;
        }
        println!("Spawned {} new sprites", spawn_count);
    }
}