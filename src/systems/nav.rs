use std::marker::PhantomData;

use bevy::prelude::*;

use crate::nav::*;
use crate::systems::input::CursorPos;
use crate::systems::levelgen::Layout;

#[derive(Component)]
pub struct NavigateTo<Target> {
    phantom: PhantomData<Target>,
}

impl<T> NavigateTo<T> {
    pub fn new() -> Self {
        NavigateTo {
            phantom: PhantomData,
        }
    }
}

pub trait NavigationTarget: Resource {
    fn target_pos(&self) -> Option<Vec2>;
    fn clear(&mut self) -> Option<Vec2>;
}

#[derive(Resource, Default)]
pub struct PlayerNavigationWaypoint {
    pub pos: Option<Vec2>,
}

impl NavigationTarget for PlayerNavigationWaypoint {
    fn target_pos(&self) -> Option<Vec2> {
        self.pos
    }

    fn clear(&mut self) -> Option<Vec2> {
        self.pos.take()
    }
}

pub struct NavigationPlugin;

impl Plugin for NavigationPlugin {
    fn build(&self, app: &mut App) {
        app
            .insert_resource(PlayerNavigationWaypoint::default())
            .add_system(update_player_nav_waypoint)
            .add_system(handle_navigation::<PlayerNavigationWaypoint>)
        ;
    }
}

pub fn update_player_nav_waypoint(
    mut waypoint: ResMut<PlayerNavigationWaypoint>,
    cursor_pos: Res<CursorPos>,
    mouse: Res<Input<MouseButton>>,
) {
    if mouse.pressed(MouseButton::Left) {
        waypoint.pos = Some(cursor_pos.world_pos());
    }
}

pub fn handle_navigation<Target: NavigationTarget>(
    mut target: ResMut<Target>,
    layout: Res<Layout>,
    mut agents: Query<(&mut Navigation, &mut Transform, &NavigationSpeed), With<NavigateTo<Target>>>,
    time: Res<Time>,
) {
    // update goal position if the target changed
    if target.is_added() || target.is_changed() {
        if let Some(goal_pos) = target.target_pos() {
            for (mut agent, _, _) in agents.iter_mut() {
                agent.set_goal(&layout.floor_graph, goal_pos);
            }
        }
    }

    // keep track of whether the agents have finished
    let mut agents_all_finished = true;

    // advance each agent toward their goal
    let time_step = time.delta_seconds();
    for (mut navigation, mut transform, speed) in &mut agents {
        let step = time_step * speed.0;
        agents_all_finished = agents_all_finished && navigation.advance(step);
        if let Some(Vec2 { x, y }) = navigation.pos() {
            transform.translation.x = *x;
            transform.translation.y = *y;
        }
    }

    // when all agents reach the common goal, clear the goal
    if agents_all_finished {
        target.clear();
    }
}