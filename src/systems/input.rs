use bevy::prelude::*;

pub struct GameInputPlugin;

impl Plugin for GameInputPlugin {
    fn build(&self, app: &mut App) {
        app
            .insert_resource(CursorPos::new())
            .add_system(cursor_update)
            .add_system(debug_clicks)
        ;
    }
}

#[derive(Resource)]
pub struct CursorPos {
    window_pos: Vec2,
    world_pos: Vec2,
    needs_recalc: bool,
}

impl CursorPos {
    fn new() -> Self {
        CursorPos {
            window_pos: Vec2::ZERO,
            world_pos: Vec2::ZERO,
            needs_recalc: true,
        }
    }
    fn set_window_pos(&mut self, pos: Vec2) {
        self.window_pos = pos;
        self.needs_recalc = true;
    }
    pub fn window_pos(&self) -> Vec2 {
        self.window_pos
    }
    pub fn world_pos(&self) -> Vec2 {
        self.world_pos
    }
}

fn cursor_update(
    mut cursor: ResMut<CursorPos>,
    mut move_events: EventReader<CursorMoved>,
    q_camera: Query<(&Camera, &GlobalTransform)>,
) {
    for event in move_events.iter() {
        cursor.set_window_pos(event.position);
    }

    if cursor.needs_recalc {
        let (camera, camera_transform) = q_camera.single();
        if let Some(world_pos) = mouse_to_world(camera, camera_transform, cursor.window_pos) {
            cursor.world_pos = world_pos;
            cursor.needs_recalc = false;
        }
    }
}

// https://bevy-cheatbook.github.io/cookbook/cursor2world.html
fn mouse_to_world(camera: &Camera, camera_transform: &GlobalTransform, mouse_pixel_pos: Vec2) -> Option<Vec2> {
    let window_size = camera.logical_viewport_size()?;
    let ndc = (mouse_pixel_pos / window_size) * 2.0 - Vec2::ONE;
    let ndc_to_world = camera_transform.compute_matrix() * camera.projection_matrix().inverse();
    let world_pos = ndc_to_world.project_point3(ndc.extend(-1.0));
    Some(world_pos.truncate())
}

fn debug_clicks(
    cursor: Res<CursorPos>,
    button: Res<Input<MouseButton>>,
) {
    if button.just_pressed(MouseButton::Left) {
        println!("left-clicked @ window:{:?}, world:{:?}", cursor.window_pos(), cursor.world_pos());
    }
}