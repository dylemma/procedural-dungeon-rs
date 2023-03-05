#![allow(dead_code)]

use bevy::prelude::*;

mod convert;
mod dungeon;
mod geom;
mod graph;
mod util;
mod nav;
mod tile;
mod systems;

fn main() {
    let world_params = WorldParams {
        grid_height: 50,
        grid_width: 50,
        tile_size: Vec2::ONE,
    };


    App::new()
        .add_plugins(DefaultPlugins)
        .insert_resource(world_params)
        .add_plugin(systems::render2d::Simple2dRenderPlugin)
        .add_plugin(systems::input::GameInputPlugin)
        .add_plugin(systems::levelgen::LevelGeneratorPlugin)
        .add_plugin(systems::nav::NavigationPlugin)
        .add_plugin(systems::player::PlayerPlugin)
        .run();
}

// -------------------------------------------------------------------------

#[derive(Resource)]
pub struct WorldParams {
    grid_width: usize,
    grid_height: usize,
    tile_size: Vec2,
}

impl WorldParams {
    fn extents(&self) -> Vec2 {
        Vec2::new(self.width(), self.height())
    }
    fn center(&self) -> Vec2 {
        Vec2::new(self.width() * 0.5, self.height() * 0.5)
    }
    fn width(&self) -> f32 {
        self.tile_size.x * self.grid_width as f32
    }
    fn height(&self) -> f32 {
        self.tile_size.y * self.grid_height as f32
    }
}
