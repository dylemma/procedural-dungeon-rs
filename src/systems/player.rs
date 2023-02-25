use bevy::prelude::*;

use crate::convert::set_2d_translate;
use crate::nav::{Navigation, NavigationSpeed};
use crate::systems::levelgen::ResetNavigationOnLayout;
use crate::systems::nav::{NavigateTo, PlayerNavigationWaypoint};

pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_startup_system(setup_player)
            .add_system(update_player_nav_cursor)
        ;
    }
}

#[derive(Component)]
pub struct Player;

pub fn setup_player(
    mut commands: Commands,
) {
    commands.spawn((
        Player,
        SpriteBundle {
            sprite: Sprite {
                custom_size: Some(Vec2::new(0.5, 0.5)),
                color: Color::CRIMSON,
                ..default()
            },
            transform: Transform::from_xyz(1.0, 1.0, 0.0),
            ..default()
        },
        NavigationSpeed(3.0),
        Navigation::default(),
        NavigateTo::<PlayerNavigationWaypoint>::new(),
        ResetNavigationOnLayout,
    ));
}

#[derive(Component)]
pub struct PlayerNavigationCursor;

pub fn update_player_nav_cursor(
    waypoint: Res<PlayerNavigationWaypoint>,
    mut cursor: Query<(Entity, &mut Transform), With<PlayerNavigationCursor>>,
    mut sprite_exists: Local<bool>,
    mut commands: Commands,
) {
    if waypoint.is_changed() || waypoint.is_added() {
        let has_sprite = *sprite_exists;
        let PlayerNavigationWaypoint { pos } = *waypoint;
        match pos {
            Some(waypoint) => {
                if has_sprite {
                    // just update the transforms
                    for (_, mut transform) in cursor.iter_mut() {
                        set_2d_translate(&mut transform, waypoint);
                    }
                } else {
                    // spawn a new sprite to act as the cursor
                    commands.spawn((
                        SpriteBundle {
                            sprite: Sprite {
                                custom_size: Some(Vec2::new(0.5, 0.5)),
                                color: Color::DARK_GREEN,
                                ..default()
                            },
                            transform: Transform::from_xyz(waypoint.x, waypoint.y, 0.),
                            ..default()
                        },
                        PlayerNavigationCursor,
                    ));
                    *sprite_exists = true;
                }
            },
            None => {
                if has_sprite {
                    // despawn the cursor entity(ies)
                    for (entity, _) in cursor.iter() {
                        commands.entity(entity).despawn();
                    }
                    *sprite_exists = false;
                }
            }
        }
    }
}