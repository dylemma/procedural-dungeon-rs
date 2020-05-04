use crate::dungeon::*;
use crate::tile::{TileAddress, CompassDirection, WallAddress, WallType};

use ncollide2d::partitioning::*;

use std::cmp::{max, min, Ordering};
use std::collections::{HashSet, HashMap};
use ncollide2d::bounding_volume::{AABB, BoundingVolume};
use ncollide2d::math::Point;
use std::ops::Index;
use crate::tile::WallType::Wall;
use unordered_pair::UnorderedPair;

pub fn decompose(dungeon: &BasicGridDungeon, tile_size: f64, wall_margin: f64, door_width: f64) -> DungeonFloorGraph {
    assert!(door_width + wall_margin < tile_size, "doors and walls must be able to fit in a tile");

    let mut seen_tiles: HashSet<TileAddress> = HashSet::new();
    let mut seen_doors: HashMap<WallAddress, DBVTLeafId> = HashMap::new();
    let mut dbvt = DBVT::new();

    let mut graph_nodes: HashSet<DBVTLeafId> = HashSet::new();
    let mut graph_adjacency: HashMap<DBVTLeafId, HashSet<DBVTLeafId>> = HashMap::new();

    let room_tiles_itr = dungeon.tiles().iter().filter(|(_, data)| data.is_some()).map(|(tile, _)| tile);
    for tile in room_tiles_itr {
        if !seen_tiles.contains(&tile) {
            if let Some((room_id, southwest, northeast)) = find_corners(dungeon, &tile) {
                // a room is defined here, by its corners at southwest and northeast
                tiles_within_corners(southwest, northeast, |t| { seen_tiles.insert(t); });

                // create an AABB of the room and insert it into our DBVT
                let southwest_point = Point::new(southwest.x as f64 * tile_size, southwest.y as f64 * tile_size);
                let northeast_point = Point::new((northeast.x + 1) as f64 * tile_size, (northeast.y + 1) as f64 * tile_size);
                let aabb = AABB::new(southwest_point, northeast_point).tightened(wall_margin);
                let data = AabbData::Room {
                    room_id,
                    grid_corners: [southwest, northeast],
                    world_bounds: aabb.clone(),
                };
                let room_handle = dbvt.insert(DBVTLeaf::new(aabb, data));

                // create a "node" for our graph based on the room
                graph_nodes.insert(room_handle);

                doors_from_room(&dungeon, &(southwest, northeast), |door| {
                    let door_handle = seen_doors.entry(*door).or_insert_with(|| {
                        let aabb = door_aabb(door, tile_size, wall_margin, door_width);
                        let data = AabbData::Door {
                            world_bounds: aabb.clone(),
                        };
                        let id = dbvt.insert(DBVTLeaf::new(aabb, data));
                        graph_nodes.insert(id);
                        id
                    });

                    graph_adjacency.entry(room_handle).or_default().insert(*door_handle);
                    graph_adjacency.entry(*door_handle).or_default().insert(room_handle);
                });
            }
        }
    }

    DungeonFloorGraph {
        bvt: dbvt,
        nodes: graph_nodes,
        adj: graph_adjacency,
    }
}

pub struct DungeonFloorGraph {
    pub bvt: DBVT<f64, AabbData, AABB<f64>>,
    pub nodes: HashSet<DBVTLeafId>,
    pub adj: HashMap<DBVTLeafId, HashSet<DBVTLeafId>>
}

#[derive(Debug, Clone)]
pub enum AabbData {
    Room {
        room_id: RoomId,
        grid_corners: [TileAddress; 2],
        world_bounds: AABB<f64>,
    },
    Door {
        world_bounds: AABB<f64>
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
    where F: FnMut(&WallAddress) -> ()
{
    let (c1, c2) = room_corners;
    let (min_x, max_x) = if c1.x < c2.x { (c1.x, c2.x) } else { (c2.x, c1.x ) };
    let (min_y, max_y) = if c1.y < c2.y { (c1.y, c2.y) } else { (c2.y, c1.y ) };
    let walls = dungeon.walls();

    let mut try_door = |wall: WallAddress| {
        if walls[wall] == WallType::Door { f(&wall); }
    };

    // west+east walls
    for y in min_y..=max_y {
        try_door(WallAddress::new(TileAddress { x: min_x, y }, CompassDirection::West));
        try_door(WallAddress::new(TileAddress { x: max_x, y }, CompassDirection::East));
    }
    // south+north walls
    for x in min_x..=max_x {
        try_door(WallAddress::new(TileAddress { x, y: min_y }, CompassDirection::South));
        try_door(WallAddress::new(TileAddress { x, y: max_y }, CompassDirection::North));
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
            Point::new(x - wall_margin, y + tile_half_width - door_half_width),
            Point::new(x + wall_margin, y + tile_half_width + door_half_width)
        ),
        CompassDirection::East => AABB::new(
            Point::new(x + tile_size - wall_margin, y + tile_half_width - door_half_width),
            Point::new(x + tile_size + wall_margin, y + tile_half_width + door_half_width),
        ),
        CompassDirection::South => AABB::new(
            Point::new(x + tile_half_width - door_half_width, y - wall_margin),
            Point::new(x + tile_half_width + door_half_width, y + wall_margin)
        ),
        CompassDirection::North => AABB::new(
            Point::new(x + tile_half_width - door_half_width, y + tile_size - wall_margin),
            Point::new(x + tile_half_width + door_half_width, y + tile_size + wall_margin)
        )
    }
}