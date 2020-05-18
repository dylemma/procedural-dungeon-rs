use std::collections::{HashMap, HashSet};

use ncollide2d::bounding_volume::{AABB, BoundingVolume};
use ncollide2d::math::{Isometry, Point as NaPoint};
use ncollide2d::partitioning::*;
use ncollide2d::partitioning::BVH;
use ncollide2d::query::PointQuery;
use pathfinding::directed::astar::astar;
use vecmath::{vec2_len, vec2_sub};

use crate::dungeon::*;
use crate::geom::Point;
use crate::tile::{CompassDirection, TileAddress, WallAddress, WallType};
use crate::tile::WallType::Wall;

pub fn decompose(dungeon: &BasicGridDungeon, tile_size: f64, wall_margin: f64, door_width: f64) -> DungeonFloorGraph {
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
                let southwest_point = NaPoint::new(southwest.x as f64 * tile_size, southwest.y as f64 * tile_size);
                let northeast_point = NaPoint::new((northeast.x + 1) as f64 * tile_size, (northeast.y + 1) as f64 * tile_size);
                let aabb = AABB::new(southwest_point, northeast_point).tightened(wall_margin);

                let room_handle = graph.insert_with(aabb, |id, bvid| {
                    FloorNode::Room {
                        id,
                        bvid,
                        room_id,
                        grid_corners: [southwest, northeast],
                    }
                });

                doors_from_room(&dungeon, &(southwest, northeast), |door, dir| {
                    let door_handle = seen_doors.entry(*door).or_insert_with(|| {
                        let aabb = door_aabb(door, tile_size, wall_margin, door_width);
                        graph.insert_with(aabb, |id, bvid| FloorNode::Door {
                            id,
                            bvid,
                            address: *door,
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
                        room_handle
                    );
                });
            }
        }
    }

    graph
}

fn from_na_point(p: &NaPoint<f64>) -> Point {
    [p.x, p.y]
}
fn to_na_point(p: &Point) -> NaPoint<f64> {
    NaPoint::new(p[0], p[1])
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub struct FloorNodeId(usize);

pub enum FloorNode {
    Room {
        id: FloorNodeId,
        room_id: RoomId,
        grid_corners: [TileAddress; 2],
        bvid: DBVTLeafId,
        // world_bounds: AABB<f64>,
    },
    Door {
        id: FloorNodeId,
        address: WallAddress,
        // world_bounds: AABB<f64>,
        bvid: DBVTLeafId,
    }
}
impl FloorNode {
    pub fn id(&self) -> &FloorNodeId {
        match self {
            FloorNode::Room { id, .. } => id,
            FloorNode::Door { id, .. } => id,
        }
    }
    pub fn bvid(&self) -> &DBVTLeafId {
        match self {
            FloorNode::Room { bvid, .. } => bvid,
            FloorNode::Door { bvid, .. } => bvid,
        }
    }
}

pub struct DungeonFloorGraph {
    bvt: DBVT<f64, FloorNodeId, AABB<f64>>,
    nodes: Vec<FloorNode>,
    adj: Vec<HashMap<FloorNodeId, PathGate>>
}
impl DungeonFloorGraph {
    pub fn new() -> Self {
        DungeonFloorGraph::with_capacity(128)
    }
    pub fn with_capacity(capacity: usize) -> Self {
        DungeonFloorGraph {
            bvt: DBVT::new(),
            nodes: Vec::with_capacity(capacity),
            adj: Vec::with_capacity(capacity),
        }
    }

    pub fn insert_with<F>(&mut self, bounds: AABB<f64>, f: F) -> FloorNodeId
        where F: FnOnce(FloorNodeId, DBVTLeafId) -> FloorNode
    {
        let index = self.nodes.len();
        let id = FloorNodeId(index);
        let bvid = self.bvt.insert(DBVTLeaf::new(bounds, id));
        let node = f(id, bvid);
        self.nodes.push(node);
        self.adj.push(HashMap::new());
        id
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

    pub fn get_bounds(&self, id: FloorNodeId) -> &AABB<f64> {
        let bvid = self.nodes[id.0].bvid();
        &self.bvt.get(*bvid).unwrap().bounding_volume
    }

    pub fn node_at_point(&self, point: &Point) -> Option<&FloorNode> {
        let mut result = None;
        {
            let na_point: NaPoint<f64> = to_na_point(point);
            let mut visitor = FirstInterferenceAtPointFinder { point: &na_point, result: &mut result };
            self.bvt.visit(&mut visitor);
        }
        result.and_then(|id| { self.get(id) })
    }

    pub fn nodes(&self) -> &Vec<FloorNode> {
        &self.nodes
    }

    pub fn find_route(&self, from: &Point, to: &Point, max_steps: u16) -> Option<Vec<(FloorNodeId, Point)>> {
        let start_node = self.node_at_point(from)?;
        let goal_node = self.node_at_point(to)?;

        let u = |dist: f64| (dist * 10000f64) as u32;

        if start_node.id() == goal_node.id() {
            return Some(vec![(*goal_node.id(), *to)]);
        }

        let point_in = |node_id: &FloorNodeId| {
            if node_id == start_node.id() { *from }
            else if node_id == goal_node.id() { *to }
            else { from_na_point(&self.get_bounds(*node_id).center()) }
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
                        let dist = vec2_len(vec2_sub(point_in(neighbor), current_center));
                        (SNode(*neighbor, current_depth + 1), u(dist))
                    })
            },
            |SNode(node_id, _)| {
                let current_point = point_in(node_id);
                let dist = vec2_len(vec2_sub(goal_point, current_point));
                u(dist)
            },
            |SNode(node_id, _)| node_id == goal_node.id()
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

struct FirstInterferenceAtPointFinder<'a, T: 'a> {
    point: &'a NaPoint<f64>,
    result: &'a mut Option<T>
}
impl<'a, T, BV> Visitor<T, BV> for FirstInterferenceAtPointFinder<'a, T>
where
    T: Clone,
    BV: PointQuery<f64>
{
    fn visit(&mut self, bv: &BV, data: Option<&T>) -> VisitStatus {
        if bv.contains_point(&Isometry::identity(), self.point) {
            if let Some(t) = data {
                self.result.replace(t.clone());
                VisitStatus::ExitEarly
            } else {
                VisitStatus::Continue
            }
        } else {
            VisitStatus::Stop
        }
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
        let [x1, y1] = self.left;
        let [x2, y2] = self.right;
        [x1 + (x2 - x1) / 2.0, y1 + (y2 - y1) / 2.0]
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
    let (min_x, max_x) = if c1.x < c2.x { (c1.x, c2.x) } else { (c2.x, c1.x ) };
    let (min_y, max_y) = if c1.y < c2.y { (c1.y, c2.y) } else { (c2.y, c2.y ) };

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
    let (min_x, max_x) = if c1.x < c2.x { (c1.x, c2.x) } else { (c2.x, c1.x ) };
    let (min_y, max_y) = if c1.y < c2.y { (c1.y, c2.y) } else { (c2.y, c1.y ) };
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
fn door_aabb(door: &WallAddress, tile_size: f64, wall_margin: f64, door_width: f64) -> AABB<f64> {
    let tile = door.tile();
    let tile_half_width = tile_size / 2.0;
    let door_half_width = door_width / 2.0;

    let x = tile.x as f64 * tile_size;
    let y = tile.y as f64 * tile_size;

    match door.direction() {
        CompassDirection::West => AABB::new(
            NaPoint::new(x - wall_margin, y + tile_half_width - door_half_width),
            NaPoint::new(x + wall_margin, y + tile_half_width + door_half_width)
        ),
        CompassDirection::East => AABB::new(
            NaPoint::new(x + tile_size - wall_margin, y + tile_half_width - door_half_width),
            NaPoint::new(x + tile_size + wall_margin, y + tile_half_width + door_half_width),
        ),
        CompassDirection::South => AABB::new(
            NaPoint::new(x + tile_half_width - door_half_width, y - wall_margin),
            NaPoint::new(x + tile_half_width + door_half_width, y + wall_margin)
        ),
        CompassDirection::North => AABB::new(
            NaPoint::new(x + tile_half_width - door_half_width, y + tile_size - wall_margin),
            NaPoint::new(x + tile_half_width + door_half_width, y + tile_size + wall_margin)
        )
    }
}

fn aabb_edge(bb: &AABB<f64>, dir: &CompassDirection) -> (Point, Point) {
    let min_x = bb.mins().x;
    let min_y = bb.mins().y;
    let max_x = bb.maxs().x;
    let max_y = bb.maxs().y;
    match *dir {
        CompassDirection::North => ([min_x, max_y], [max_x, max_y]),
        CompassDirection::East => ([max_x, max_y], [max_x, min_y]),
        CompassDirection::South => ([max_x, min_y], [min_x, min_y]),
        CompassDirection::West => ([min_x, min_y], [min_x, max_y]),
    }
}