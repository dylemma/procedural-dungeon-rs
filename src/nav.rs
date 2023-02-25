use bevy::prelude::*;
use parry2d::na::Isometry2;
use parry2d::query::PointQuery;

use crate::convert::*;
use crate::graph::*;

#[derive(Component)]
pub struct NavigationSpeed(pub f32);

#[derive(Component)]
pub struct NavigationCursor {
    pub entity: Entity,
}

pub type NavPoint = (FloorNodeId, Vec2);

#[derive(Debug)]
pub enum NavPath {
    WithinNode(NavPoint),
    InterNode(FloorNodeId, Vec<NavPoint>, usize),
}

impl NavPath {
    pub fn current_target(&self) -> Option<&NavPoint> {
        match self {
            NavPath::WithinNode(target) => Some(target),
            NavPath::InterNode(_, waypoints, index) => waypoints.get(*index),
        }
    }
    pub fn into_next(self) -> Option<NavPath> {
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
    pub fn end_node(&self) -> &FloorNodeId {
        match self {
            NavPath::WithinNode((id, _)) => id,
            NavPath::InterNode(id, _, _) => id,
        }
    }
    pub fn end_point(&self) -> &Vec2 {
        match self {
            NavPath::WithinNode((_, p)) => p,
            NavPath::InterNode(_, v, _) => &v[v.len() - 1].1,
        }
    }
    pub fn replace_final_point(&mut self, point: Vec2) {
        match self {
            NavPath::WithinNode((_id, p)) => *p = point,
            NavPath::InterNode(_, v, _) => v.last_mut().expect("Last step of InterNode").1 = point,
        };
    }
}

#[derive(Component)]
// #[storage(DenseVecStorage)]
pub struct Navigation {
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
    pub fn new(current_node: FloorNodeId, current_pos: Vec2) -> Self {
        Navigation {
            current: Some((current_node, current_pos)),
            path: None,
        }
    }

    pub fn pos(&self) -> Option<&Vec2> {
        self.current.as_ref().map(|(_id, p)| p)
    }

    pub fn clear_and_reset(&mut self, current_node: FloorNodeId, current_pos: Vec2) {
        self.current = Some((current_node, current_pos));
        self.path = None;
    }

    pub fn set_goal(&mut self, graph: &DungeonFloorGraph, goal_pos: Vec2) {
        if let Some((current_node, current_pos)) = self.current {
            let goal_node_opt = graph.node_at_point(&vec2_to_point(goal_pos)).map(|n| n.id().clone());

            match goal_node_opt {
                None => {
                    // if the goal_pos is not in any room, find the edge of the current room in the direction of that pos and navigate there instead
                    let current_room_aabb = graph.get_bounds(current_node);
                    let projection = current_room_aabb.project_point(&Isometry2::identity(), &vec2_to_point(goal_pos), true);
                    let projected_pos = Vec2::new(projection.point.x, projection.point.y);
                    println!("Goal {:?} is outside room, use {:?} instead", goal_pos, projected_pos);
                    self.path = Some(NavPath::WithinNode((current_node, projected_pos)));
                }
                Some(goal_node) => {
                    println!("Goal {:?} is in room {:?}", goal_pos, goal_node);
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
                        return;
                    }

                    // if we got this far, the easy routes failed,
                    // so we have to use pathfinding to create an `InterNode` path
                    let start_proj = graph.get_bounds(current_node).project_point(&Isometry2::identity(), &vec2_to_point(current_pos), true);
                    if !start_proj.is_inside {
                        println!("edge case hit!");
                    }

                    self.path = graph.find_route(&start_proj.point, &vec2_to_point(goal_pos), 256).map(|route_na| {
                        let route: Vec<_> = route_na.into_iter().map(|(id, p)| (id, Vec2::new(p.x, p.y))).collect();
                        NavPath::InterNode(goal_node, route, 0)
                    });

                    if self.path.is_none() {
                        // let end_proj = graph.get_bounds(current_node).project_point(&Isometry2::identity(), &goal_pos_64.into(), true);
                        println!("Still no path?");
                    }
                }
            };
        }
    }

    /// Returns `true` if navigation completed
    pub fn advance(&mut self, step: f32) -> bool {
        if let Some((current_node, current_pos)) = self.current {
            if let Some(path) = self.path.take() {
                if let Some((target_node, target_pos)) = path.current_target() {
                    let to_target = *target_pos - current_pos;
                    let dist = to_target.length(); //target_pos.distance(current_pos);
                    // let to_target = vec2_sub(*target_pos, current_pos);
                    // let dist = vec2_len(to_target);

                    if dist < step {
                        // we are able to reach the next point within the current step,
                        // so we will need to advance the navigation state,
                        // then recurse to spend the rest of the step
                        self.current = Some((*target_node, *target_pos));
                        self.path = path.into_next();
                        return self.advance(step - dist);
                    } else {
                        // move as far as the subject can towards the target pos,
                        // with no need to change the node or the path
                        // let movement = vec2_scale(to_target, step / dist);
                        let movement = to_target * (step / dist);
                        self.current = Some((
                            current_node,
                            // vec2_add(current_pos, movement)
                            current_pos + movement,
                        ));
                        self.path = Some(path); // put the path back because we 'took' it earlier

                        // navigation is not yet complete
                        return false;
                    }
                }
            }
        }

        // no current pos, no path, or no next target;
        // all of these can be interpreted as having completed navigation
        true
    }
}