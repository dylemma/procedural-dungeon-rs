use std::collections::VecDeque;

use bevy::utils::HashSet;

use crate::dungeon::*;
use crate::tile::{CompassDirection, TileAddress, WallAddress, WallType};

/// Represents a contiguous area of tiles in a grid,
/// including a semi-arbitrary "leftmost" tile that serves as the seed for a perimeter builder.
struct ConnectedTiles {
    addresses: HashSet<TileAddress>,
    leftmost: TileAddress,
}

/// Performs a BFS traversal of the `grid`, starting at the given `start` address,
/// only following edges between tiles that are considered connected by the given `connection_check`
fn get_connected_tiles<T, W>(grid: &GridDungeon<T, W>, start: TileAddress, connection_check: impl ConnectionCheck<T, W>) -> ConnectedTiles {
    // create the "seen" output set and insert the start address into it
    let mut seen = HashSet::new();
    let mut leftmost = start;
    seen.insert(start);

    let connected_neighbor = |addr: TileAddress, dir: CompassDirection| {
        println!("..\nfrom {:?} go {:?}", addr, dir);
        let t1_value = grid.tiles().get(addr)?;
        println!("  got t1_value");
        let neighbor = (addr + dir)?;
        println!("  got neighbor at {:?}", neighbor);
        let t2_value = grid.tiles().get(neighbor)?;
        println!("  got t2_value");
        let wall = &grid.walls()[WallAddress::new(addr, dir)];
        if connection_check.is_connected(t1_value, t2_value, wall) {
            println!("  they connect!");
            Some(PendingVisit { address: neighbor, ingress: dir })
        } else {
            println!("  not connected :(");
            None
        }
    };

    // initialize a queue of PendingVisit, seeded with the `start` address's neighbors
    let mut q = VecDeque::new();
    CompassDirection::ALL.iter()
        .filter_map(|dir| connected_neighbor(start, *dir))
        .for_each(|pv| q.push_back(pv));

    while let Some(visit) = q.pop_front() {
        let current = visit.address;
        println!("visit {:?}", current);

        // now that we've reached the visit, count it as "seen"
        seen.insert(current);

        // update the `leftmost`
        if current.x < leftmost.x {
            leftmost = current;
        }

        // add the new tile's unseen neighbors to the queue
        let backward = visit.ingress.reflect();
        CompassDirection::ALL.iter()
            .filter(|&dir| *dir != backward) // don't backtrack
            .filter_map(|dir| connected_neighbor(visit.address, *dir)) // obey the connection check
            .filter(|v| !seen.contains(&v.address)) // don't revisit seen tiles
            .for_each(|v| q.push_back(v));
    }

    ConnectedTiles {
        addresses: seen,
        leftmost,
    }
}

/// Helper struct representing the intent to visit a tile during a grid traversal.
/// Includes an `ingress` direction to indicate how the tile is being reached, in
/// order to avoid wasting cycles due to backtracking.
struct PendingVisit {
    address: TileAddress,
    // the direction that the cursor moved in order to reach this address,
    // i.e. `address + ingress.reflect()` would bring you back to the previous address
    ingress: CompassDirection,
}

/// Function-ish trait that checks whether two tiles can be considered connected to each other.
/// This probably could have been replaced by an anonymous function, but I like it this way.
pub trait ConnectionCheck<T, W> {
    fn is_connected(&self, tile_1: &T, tile_2: &T, edge_state: &W) -> bool;
}

/// Default connection check implementation for `BasicGridDungeon` tile and wall types
struct BasicGridDungeonConnectionCheck;

impl ConnectionCheck<Option<(RoomId, RoomState)>, WallType> for BasicGridDungeonConnectionCheck {
    fn is_connected(&self, tile_1: &Option<(RoomId, RoomState)>, tile_2: &Option<(RoomId, RoomState)>, edge_state: &WallType) -> bool {
        let is_wall = match edge_state {
            WallType::Wall => true,
            _ => false,
        };
        match (tile_1, tile_2) {
            (None, None) => {
                // neither tile has data, so technically they are in the same state and therefore can be considered
                // connected to each other as long as there isn't a wall between them
                !is_wall
            }
            (Some((_, state_1)), Some((_, state_2))) => {
                // both tiles have data, so we must inspect their RoomState; if both are in a "connected" state
                // (in the sense that the generator algorithm has added the corresponding room to the generation),
                // then they can be considered connected to each other as long as there isn't a wall between them.
                // The same applies if both tiles are *outside* the generation
                state_1.is_connected() == state_2.is_connected() && !is_wall
            }
            _ => {
                // the tiles are in different states entirely, so they cannot be considered connected to each other
                false
            }
        }
    }
}

/// Represents the bottom-left corner of a tile
#[derive(Copy, Clone, Debug)]
struct Corner {
    tile: TileAddress,
}

/// Represents the current focus of a perimeter traversal in grid space
#[derive(Copy, Clone, Debug)]
struct Cursor {
    current_corner: Corner,
    heading: CompassDirection,
}

impl Cursor {
    fn next(&self) -> Option<Cursor> {
        let next_tile = (self.current_corner.tile + self.heading)?;
        Some(Cursor {
            current_corner: Corner { tile: next_tile },
            heading: self.heading,
        })
    }

    fn as_wall_address(&self) -> Option<WallAddress> {
        match self.heading {
            CompassDirection::North => {
                Some(WallAddress::new(self.current_corner.tile, CompassDirection::West))
            },
            CompassDirection::East => {
                Some(WallAddress::new(self.current_corner.tile, CompassDirection::South))
            },
            CompassDirection::South => {
                let tile = (self.current_corner.tile + CompassDirection::South)?;
                Some(WallAddress::new(tile, CompassDirection::West))
            },
            CompassDirection::West => {
                let tile = (self.current_corner.tile + CompassDirection::West)?;
                Some(WallAddress::new(tile, CompassDirection::South))
            },
        }
    }

    /// If you think of the heading as a ray originating from the `current_corner`,
    /// going in some cardinal direction, that ray separates two tiles. Those two
    /// tiles can be collected in the form of a WallAddress, where the tile to the "left"
    /// of the ray will be the WallAddress's focus tile, and the tile to the "right"
    /// of the ray will be the WallAddress's neighbor tile.
    fn tiles_ahead(&self) -> (Option<TileAddress>, Option<TileAddress>) {
        match self.heading {
            CompassDirection::North => (
                // to the left is the tile to the west.
                self.current_corner.tile + CompassDirection::West,
                // to the right is the corner's own tile
                Some(self.current_corner.tile),
            ),
            CompassDirection::East => (
                // east traverses the bottom edge of the cursor's tile, so the "left" tile is the current tile
                Some(self.current_corner.tile),
                // to the "right" while facing east is south
                self.current_corner.tile + CompassDirection::South,
            ),
            CompassDirection::South => (
                // south leaves the current tile, where "left" is the tile south of the cursor's tile
                self.current_corner.tile + CompassDirection::South,
                // ..and "right" is the tile west of that one
                southwest_of(self.current_corner.tile),
            ),
            CompassDirection::West => (
                // west leaves the current tile, where "left" is the tile southwest of the cursor's tile
                southwest_of(self.current_corner.tile),
                // ..and "right" is the tile west of the cursor's tile
                self.current_corner.tile + CompassDirection::West,
            ),
        }
    }
}

fn southwest_of(tile: TileAddress) -> Option<TileAddress> {
    (tile + CompassDirection::South).and_then(|s| s + CompassDirection::West)
}

#[derive(Copy, Clone, Debug)]
enum Turn {
    Left,
    Right,
    None,
}

#[derive(Copy, Clone, Debug)]
struct PerimeterStep {
    turn: Turn,
    continuation: Cursor,
}

fn traverse_perimeter(connected_tiles: &ConnectedTiles) -> Vec<PerimeterStep> {
    // Given a tile at the left edge of the connected area, we can assume its left edge is part of the perimeter.
    // Then, given our Cursor construction where a corner is identified as the bottom-left of its corresponding tile,
    // we can create a cursor for that left edge by starting at the corner of the `leftmost` tile, facing north.
    // However, since we also want to know what "turn" the perimeter made to reach that edge, we can't immediately
    // insert the starting cursor into the perimeter Vec. Instead, we'll let it "advance" according to the normal
    // perimeter-following loop rules, ending the loop after we re-encounter the start tile.
    let start_tile = connected_tiles.leftmost;
    let mut perimeter = Vec::new();

    let mut cursor = Cursor {
        current_corner: Corner { tile: start_tile },
        heading: CompassDirection::North,
    };

    loop {
        // Advance the cursor
        cursor = cursor.next().unwrap();

        // Inspect the "connected" state of the two tiles that the cursor's "heading" edge separates.
        let (ahead_left, ahead_right) = cursor.tiles_ahead();
        let left_is_open = ahead_left.map_or(false, |addr| connected_tiles.addresses.contains(&addr));
        let right_is_open = ahead_right.map_or(false, |addr| connected_tiles.addresses.contains(&addr));

        // Adjust the cursor's heading based on the state of the two tiles ahead of it.
        // If both tiles are open, then we've encountered a corner, turning left.
        // If only the right tile is open, then the edge continues in the same direction.
        // If the right tile is closed, then we've encountered a corner, turning right.
        let turn =
            if left_is_open && right_is_open { Turn::Left }
            else if !right_is_open { Turn::Right }
            else { Turn::None };
        match turn {
            Turn::Left => cursor.heading = cursor.heading.rotate_counterclockwise(),
            Turn::Right => cursor.heading = cursor.heading.rotate_clockwise(),
            Turn::None => (),
        }

        // With the turn calculated and the heading adjusted, we can append to the Perimeter
        perimeter.push(PerimeterStep {
            turn,
            continuation: cursor,
        });

        // If we got back to the starting tile, the Perimeter is complete
        if cursor.current_corner.tile == start_tile {
            break;
        }
    }

    perimeter
}

pub fn run_test() {
    let mut dungeon = GridDungeon::<bool>::new(10, 10);

    // mark a handful of tiles as `true` in a custom shape
    {
        let tiles = dungeon.tiles_mut();
        let mut mark = |x: usize, y: usize, value: bool| {
            println!("mark({}, {})", x, y);
            tiles[TileAddress { x, y }] = value;
        };

        // a square from 3,3 to 6,6
        for x in 3 ..= 6 {
            for y in 3 ..= 6 {
                mark(x, y, true);
            }
        }
        // a little nub poking off the bottom-left of the square
        mark(2, 3, true);

        // a peninsula jutting out of the upper-right of the square
        for x in 6 ..= 7 {
            for y in 5 ..= 8 {
                mark(x, y, true);
            }
        }

        mark(4, 4, false);
        mark(5, 4, false);
        mark(5, 5, false);
    }

    // update the wall states so there's a Wall between tiles of different values
    {
        let (tiles, walls_mut) = dungeon.tiles_and_walls_mut();

        for (wall_addr, wall_type) in walls_mut.iter_mut() {
            let t1 = tiles.get(wall_addr.tile()).map(|b| *b).unwrap_or_default();
            let t2 = wall_addr.neighbor().and_then(|n| tiles.get(n)).map(|b| *b).unwrap_or_default();
            *wall_type = if t1 == t2 { WallType::Clear } else {  WallType::Wall };
        }
    }

    struct SimpleConnectivityCheck;
    impl ConnectionCheck<bool, WallType> for SimpleConnectivityCheck {
        fn is_connected(&self, tile_1: &bool, tile_2: &bool, edge_state: &WallType) -> bool {
            *tile_1 == *tile_2 && *edge_state != WallType::Wall
        }
    }

    let connected_tiles = get_connected_tiles(&dungeon, TileAddress { x: 7, y: 8 }, SimpleConnectivityCheck);
    for tile in &connected_tiles.addresses {
        println!("connected to {:?}", tile);
    }
    println!(".....\n\n");

    let perimeter = traverse_perimeter(&connected_tiles);
    for cursor in &perimeter {
        println!("Perimiter: {:?}", cursor);
    }

    let perimiter_walls: HashSet<WallAddress> = perimeter.iter().filter_map(|step| step.continuation.as_wall_address()).collect();
    // {
    //     let tiles = dungeon.tiles();
    //     for (wall_addr, &wall_type) in dungeon.walls() {
    //         let t1 = tiles.get(wall_addr.tile()).map(|b| *b).unwrap_or_default();
    //         let t2 = wall_addr.neighbor().and_then(|n| tiles.get(n)).map(|b| *b).unwrap_or_default();
    //         if t1 != t2 && !perimiter_walls.contains(&wall_addr) {
    //             println!("hole tile? {:?}", );
    //         }
    //     }
    // }

    for tile in connected_tiles.addresses {
        if let Some(t1) = dungeon.tiles().get(tile) {
            for dir in CompassDirection::ALL {
                if let Some(neighbor) = tile + dir {
                    if let Some(t2) = dungeon.tiles().get(neighbor) {
                        let wall_addr = WallAddress::new(tile, dir);
                        if *t1 != *t2 && !perimiter_walls.contains(&wall_addr) {
                            println!("Hole! {:?}", neighbor);
                        }
                    }
                }
            }
        }
    }
}

enum AreaId {
    Connected(usize),
    Hole(usize),
}

struct AreaTraversal {
    areas: Vec<(AreaId, ConnectedTiles)>
}