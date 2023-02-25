use bevy::prelude::{Transform, Vec2};
use parry2d::na::{Point2, Vector2};

pub fn point_to_vec2(point: Point2<f32>) -> Vec2 {
    Vec2::new(point.x, point.y)
}

pub fn vector_to_vec2(vector: Vector2<f32>) -> Vec2 {
    Vec2::new(vector.x, vector.y)
}

pub fn vec2_to_point(vec: Vec2) -> Point2<f32> {
    Point2::new(vec.x, vec.y)
}

pub fn vec2_to_vector(vec: Vec2) -> Vector2<f32> {
    Vector2::new(vec.x, vec.y)
}

pub fn set_2d_translate(transform: &mut Transform, pos: Vec2) {
    transform.translation.x = pos.x;
    transform.translation.y = pos.y;
}
