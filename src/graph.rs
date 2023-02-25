use std::collections::{HashMap, HashSet};

use parry2d::{
    bounding_volume::{Aabb, BoundingVolume},
    math::Real,
    na,
    partitioning::Qbvh,
    query::visitors::PointIntersectionsVisitor,
};
use parry2d::na::distance;
use parry2d::partitioning::{GenericQbvh, IndexedData};
use pathfinding::directed::astar::astar;

use crate::dungeon::*;
use crate::geom::Point;
use crate::tile::{CompassDirection, TileAddress, WallAddress, WallType};

pub fn decompose(dungeon: &BasicGridDungeon, tile_size: Real, wall_margin: Real, door_width: Real) -> DungeonFloorGraph {
    assert!(door_width + wall_margin < tile_size, "doors and walls must be able to fit in a tile");

    let mut seen_tiles: HashSet<TileAddress> = HashSet::new();
    let mut seen_doors: HashMap<WallAddress, FloorNodeId> = HashMap::new();

    let mut graph = DungeonFloorGraph::new();

    let room_tiles_itr = dungeon.tiles().iter().filter(|(_, data)| data.is_some()).map(|(tile, _)| tile);
    for tile in room_tiles_itr {
        if !seen_tiles.contains(&tile) {
            if let Some((room_id, southwest, northeast)) = find_corners(dungeon, &tile) {
                // a room is defined here, by its corners at southwest and northeast
                tiles_within_corners(southwest, northeast, |t| { seen_tiles.insert(t); });

                // create an AABB of the room and insert it into our DBVT
                let southwest_point = Point::new(southwest.x as Real * tile_size, southwest.y as Real * tile_size);
                let northeast_point = Point::new((northeast.x + 1) as Real * tile_size, (northeast.y + 1) as Real * tile_size);
                let aabb = Aabb::new(southwest_point, northeast_point).tightened(wall_margin);

                let room_handle = graph.insert_with(|id/*, bvid*/| {
                    FloorNode::Room {
                        id,
                        room_id,
                        grid_corners: [southwest, northeast],
                        world_bounds: aabb,
                    }
                });

                doors_from_room(&dungeon, &(southwest, northeast), |door, dir| {
                    let door_handle = seen_doors.entry(*door).or_insert_with(|| {
                        let aabb = door_aabb(door, tile_size, wall_margin, door_width);
                        graph.insert_with(|id/*, bvid*/| FloorNode::Door {
                            id,
                            address: *door,
                            world_bounds: aabb,
                        })
                    });
                    let door_bounds = graph.get_bounds(*door_handle);

                    // find the edge of the door that borders with the current room.
                    // This will be the edge that the path planning "funnel" needs to pass through.
                    // Since the given `dir` is from the room to the door, we need to use the opposite
                    // direction when picking which side of the room to treat as the edge.
                    // We're trying to represent the edge as a (L, R) pair, where the first point
                    // is on the left when viewed from the center of the room/door AABB
                    let (left_from_door, right_from_door) = aabb_edge(door_bounds, &dir.reflect());
                    graph.set_adjacent(
                        *door_handle,
                        PathGate::new(left_from_door, right_from_door),
                        room_handle,
                    );
                });
            }
        }
    }

    graph.refit_bv();

    graph
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub struct FloorNodeId(usize);

// allow FloorNodeId to be used as the `LeafData` type parameter on parry's `Qbvh` quadtree type
impl IndexedData for FloorNodeId {
    fn default() -> Self {
        // mimic the `IndexedData for usize` impl
        FloorNodeId(u32::MAX as usize)
    }

    fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug)]
pub enum FloorNode {
    Room {
        id: FloorNodeId,
        room_id: RoomId,
        grid_corners: [TileAddress; 2],
        // bvid: DBVTLeafId,
        world_bounds: Aabb,
    },
    Door {
        id: FloorNodeId,
        address: WallAddress,
        world_bounds: Aabb,
        // bvid: DBVTLeafId,
    },
}

impl FloorNode {
    pub fn id(&self) -> &FloorNodeId {
        match self {
            FloorNode::Room { id, .. } => id,
            FloorNode::Door { id, .. } => id,
        }
    }
    pub fn world_bounds(&self) -> &Aabb {
        match self {
            FloorNode::Room { world_bounds, .. } => world_bounds,
            FloorNode::Door { world_bounds, .. } => world_bounds,
        }
    }
}

pub struct DungeonFloorGraph {
    bvt: Qbvh<FloorNodeId>,
    nodes: Vec<FloorNode>,
    adj: Vec<HashMap<FloorNodeId, PathGate>>,
}

impl DungeonFloorGraph {
    pub fn new() -> Self {
        DungeonFloorGraph::with_capacity(128)
    }
    pub fn with_capacity(capacity: usize) -> Self {
        DungeonFloorGraph {
            // bvt: DBVT::new(),
            bvt: GenericQbvh::new(),
            nodes: Vec::with_capacity(capacity),
            adj: Vec::with_capacity(capacity),
        }
    }

    pub fn insert_with<F>(&mut self, f: F) -> FloorNodeId
        where F: FnOnce(FloorNodeId/*, DBVTLeafId*/) -> FloorNode
    {
        let index = self.nodes.len();
        let id = FloorNodeId(index);
        let node = f(id/*, bvid*/);
        self.nodes.push(node);
        self.adj.push(HashMap::new());
        self.bvt.pre_update_or_insert(id);
        id
    }

    pub fn refit_bv(&mut self) {
        let DungeonFloorGraph { bvt, nodes, .. } = self;
        let mut workspace = Default::default();
        bvt.refit(0f32, &mut workspace, |&id| {
            nodes[id.0].world_bounds().clone()
        });
    }

    pub fn set_adjacent(&mut self, from: FloorNodeId, gate: PathGate, to: FloorNodeId) {
        {
            // add an adjacency `to => from` that that goes through the gate backwards (left/right swapped)
            let adj_to = &mut self.adj[to.0];
            adj_to.insert(from, gate.swapped());
        }
        {
            // add an adjacency `from => to` that goes through the gate as given
            let adj_from = &mut self.adj[from.0];
            adj_from.insert(to, gate);
        }
    }

    pub fn get(&self, id: FloorNodeId) -> Option<&FloorNode> {
        if id.0 >= self.nodes.len() {
            None
        } else {
            Some(&self.nodes[id.0])
        }
    }

    pub fn adjacencies_of(&self, id: FloorNodeId) -> &HashMap<FloorNodeId, PathGate> {
        &self.adj[id.0]
    }

    pub fn get_root_bound(&self) -> &Aabb {
        // TODO: this is useless; it just returns [-Inf, Inf]
        self.bvt.root_aabb()
    }

    pub fn get_bounds(&self, id: FloorNodeId) -> &Aabb {
        self.nodes[id.0].world_bounds()
    }

    pub fn node_at_point(&self, point: &Point) -> Option<&FloorNode> {
        let mut result = None;
        let mut callback = |id: &FloorNodeId| {
            result = self.get(*id);
            result.is_none() // visitor callback returns true to continue
        };
        let mut visitor = PointIntersectionsVisitor::new(point, &mut callback);
        self.bvt.traverse_depth_first(&mut visitor);
        result
    }


    pub fn nodes(&self) -> &Vec<FloorNode> {
        &self.nodes
    }

    pub fn find_route(&self, from: &Point, to: &Point, max_steps: u16) -> Option<Vec<(FloorNodeId, Point)>> {
        let start_node = self.node_at_point(from)?;
        let goal_node = self.node_at_point(to)?;

        let u = |dist: Real| (dist * 10000f32) as u32;

        if start_node.id() == goal_node.id() {
            return Some(vec![(*goal_node.id(), *to)]);
        }

        let point_in = |node_id: &FloorNodeId| {
            if node_id == start_node.id() { *from } else if node_id == goal_node.id() { *to } else { self.get_bounds(*node_id).center() }
        };
        let goal_point = point_in(goal_node.id());

        #[derive(Eq, PartialEq, Hash, Clone)]
        struct SNode(FloorNodeId, u16);

        let (path, _total_cost) = astar(
            &SNode(*start_node.id(), 0u16),
            |SNode(node_id, depth)| {
                let current_center = point_in(node_id);
                let current_depth = depth.clone();
                self.adjacencies_of(*node_id).iter()
                    .filter(move |_| current_depth < max_steps)
                    .map(move |(neighbor, _gate)| {
                        let dist = distance(&point_in(neighbor), &current_center);
                        (SNode(*neighbor, current_depth + 1), u(dist))
                    })
            },
            |SNode(node_id, _)| {
                let current_point = point_in(node_id);
                let dist = distance(&goal_point, &current_point);
                u(dist)
            },
            |SNode(node_id, _)| node_id == goal_node.id(),
        )?;

        // the path is a Vec whose items are NodeIds...
        // now compute a path of Points by aiming for the middle of each PathGate on the borders between Nodes,
        // and finish off the path with the goal point which is found inside the final Node
        let mut points: Vec<(FloorNodeId, Point)> = Vec::with_capacity(path.len() - 1);
        for (current, next) in path.iter().map(|SNode(id, _)| id).sliding() {
            let gate: &PathGate = self.adj[current.0].get(next)?;
            points.push((*next, gate.center()));
        }
        points.push((path[path.len() - 1].0, goal_point));

        // let points: Vec<_> = path.iter().map(point_in).collect();
        Some(points)
    }
}

#[derive(PartialEq, Clone)]
pub struct PathGate {
    left: Point,
    right: Point,
}

impl PathGate {
    pub fn new(left: Point, right: Point) -> Self {
        PathGate { left, right }
    }
    pub fn swapped(&self) -> Self {
        PathGate {
            left: self.right.clone(),
            right: self.left.clone(),
        }
    }
    pub fn center(&self) -> Point {
        na::center(&self.left, &self.right)
    }
}

fn find_corners(dungeon: &BasicGridDungeon, start: &TileAddress) -> Option<(RoomId, TileAddress, TileAddress)> {
    // assume rectangular rooms
    let room_at = |addr: TileAddress| {
        match dungeon.tiles().get(addr) {
            Some(&Some((room_id, _))) => Some(room_id),
            _ => None,
        }
    };
    if let Some(room_id) = room_at(*start) {
        let matching_neighbor = |addr: Option<TileAddress>| {
            addr
                .and_then(|tile| room_at(tile).map(|id| (id, tile)))
                .filter(|(id, _)| *id == room_id)
                .map(|(_, tile)| tile)
        };

        let mut southwest: TileAddress = *start;
        let mut northeast: TileAddress = *start;

        while let Some(tile) = matching_neighbor(southwest + CompassDirection::West) { southwest = tile }
        while let Some(tile) = matching_neighbor(southwest + CompassDirection::South) { southwest = tile }
        while let Some(tile) = matching_neighbor(northeast + CompassDirection::North) { northeast = tile }
        while let Some(tile) = matching_neighbor(northeast + CompassDirection::East) { northeast = tile }

        Some((room_id, southwest, northeast))
    } else {
        None
    }
}

fn tiles_within_corners<F: FnMut(TileAddress) -> ()>(c1: TileAddress, c2: TileAddress, mut f: F) {
    let (min_x, max_x) = if c1.x < c2.x { (c1.x, c2.x) } else { (c2.x, c1.x) };
    let (min_y, max_y) = if c1.y < c2.y { (c1.y, c2.y) } else { (c2.y, c2.y) };

    for y in min_y..=max_y {
        for x in min_x..=max_x {
            f(TileAddress { x, y });
        }
    }
}

fn doors_from_room<F>(dungeon: &BasicGridDungeon, room_corners: &(TileAddress, TileAddress), mut f: F)
    where F: FnMut(&WallAddress, &CompassDirection) -> ()
{
    let (c1, c2) = room_corners;
    let (min_x, max_x) = if c1.x < c2.x { (c1.x, c2.x) } else { (c2.x, c1.x) };
    let (min_y, max_y) = if c1.y < c2.y { (c1.y, c2.y) } else { (c2.y, c1.y) };
    let walls = dungeon.walls();

    let mut try_door = |from: TileAddress, dir: CompassDirection| {
        let wall = WallAddress::new(from, dir);
        if walls[wall] == WallType::Door { f(&wall, &dir); }
    };

    // west+east walls
    for y in min_y..=max_y {
        try_door(TileAddress { x: min_x, y }, CompassDirection::West);
        try_door(TileAddress { x: max_x, y }, CompassDirection::East);
    }
    // south+north walls
    for x in min_x..=max_x {
        try_door(TileAddress { x, y: min_y }, CompassDirection::South);
        try_door(TileAddress { x, y: max_y }, CompassDirection::North);
    }
}

/// Creates an AABB representing the position of the door at the given WallAddress.
/// The width of the door is `door_width`.
/// The depth of the door is `2 * wall_margin`, as the wall intrudes into the current room
/// as well as the neighboring room by `wall_margin`.
fn door_aabb(door: &WallAddress, tile_size: Real, wall_margin: Real, door_width: Real) -> Aabb {
    let tile = door.tile();
    let tile_half_width = tile_size / 2.0;
    let door_half_width = door_width / 2.0;

    let x = tile.x as Real * tile_size;
    let y = tile.y as Real * tile_size;

    match door.direction() {
        CompassDirection::West => Aabb::new(
            Point::new(x - wall_margin, y + tile_half_width - door_half_width),
            Point::new(x + wall_margin, y + tile_half_width + door_half_width),
        ),
        CompassDirection::East => Aabb::new(
            Point::new(x + tile_size - wall_margin, y + tile_half_width - door_half_width),
            Point::new(x + tile_size + wall_margin, y + tile_half_width + door_half_width),
        ),
        CompassDirection::South => Aabb::new(
            Point::new(x + tile_half_width - door_half_width, y - wall_margin),
            Point::new(x + tile_half_width + door_half_width, y + wall_margin),
        ),
        CompassDirection::North => Aabb::new(
            Point::new(x + tile_half_width - door_half_width, y + tile_size - wall_margin),
            Point::new(x + tile_half_width + door_half_width, y + tile_size + wall_margin),
        )
    }
}

fn aabb_edge(bb: &Aabb, dir: &CompassDirection) -> (Point, Point) {
    let min_x = bb.mins.x;
    let min_y = bb.mins.y;
    let max_x = bb.maxs.x;
    let max_y = bb.maxs.y;
    match *dir {
        CompassDirection::North => (Point::new(min_x, max_y), Point::new(max_x, max_y)),
        CompassDirection::East => (Point::new(max_x, max_y), Point::new(max_x, min_y)),
        CompassDirection::South => (Point::new(max_x, min_y), Point::new(min_x, min_y)),
        CompassDirection::West => (Point::new(min_x, min_y), Point::new(min_x, max_y)),
    }
}