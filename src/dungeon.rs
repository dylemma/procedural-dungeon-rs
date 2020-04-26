use std::cmp::{max, Ordering};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::Iterator;
use std::num::NonZeroUsize;

use num::clamp;
use pathfinding::directed::dijkstra::dijkstra;
use pathfinding::undirected::connected_components::connected_components;
use rand::{Rng, thread_rng};
use rand::distributions::{Distribution, WeightedIndex};
use rand::seq::SliceRandom;
use unordered_pair::UnorderedPair;

use crate::tile::{CompassDirection, GridTiles, GridWalls, TileAddress, WallAddress, WallType};

pub struct GridDungeon<R, W = WallType> {
    grid_width: usize,
    grid_height: usize,
    tiles: GridTiles<R>,
    walls: GridWalls<W>,
}

impl<R, W> GridDungeon<R, W>
    where
        R: Default + Clone,
        W: Default + Clone,
{
    fn new(grid_width: usize, grid_height: usize) -> Self {
        GridDungeon {
            grid_width,
            grid_height,
            tiles: GridTiles::new(grid_width, grid_height),
            walls: GridWalls::new(grid_width, grid_height),
        }
    }
}

impl<R, W> GridDungeon<R, W> {
    pub fn grid_width(&self) -> usize { self.grid_width }
    pub fn grid_height(&self) -> usize { self.grid_height }
    pub fn tiles(&self) -> &GridTiles<R> { &self.tiles }
    pub fn walls(&self) -> &GridWalls<W> { &self.walls }
    pub fn tiles_mut(&mut self) -> &mut GridTiles<R> { &mut self.tiles }
    pub fn walls_mut(&mut self) -> &mut GridWalls<W> { &mut self.walls }
    pub fn tiles_and_walls_mut(&mut self) -> (&mut GridTiles<R>, &mut GridWalls<W>) { (&mut self.tiles, &mut self.walls) }
}


// =============================================

#[derive(Copy, Clone, Debug)]
pub struct RoomSize(NonZeroUsize, NonZeroUsize);

impl RoomSize {
    pub fn new(width: usize, height: usize) -> Option<RoomSize> {
        NonZeroUsize::new(width).and_then(move |w| {
            NonZeroUsize::new(height).map(|h| {
                RoomSize(w, h)
            })
        })
    }

    pub fn width(&self) -> usize { self.0.get() }
    pub fn height(&self) -> usize { self.1.get() }
    pub fn permutations(&self) -> Vec<RoomSize> {
        if self.0 == self.1 {
            vec!(*self)
        } else {
            vec!(*self, RoomSize(self.1, self.0))
        }
    }

    pub fn can_fit_at<F>(&self, anchor: &TileAddress, can_set: F) -> bool
        where
            F: Fn(&TileAddress) -> bool
    {
        for x in anchor.x..(anchor.x + self.width()) {
            for y in anchor.y..(anchor.y + self.height()) {
                if !can_set(&TileAddress { x, y }) {
                    return false;
                }
            }
        }
        true
    }
}

pub trait GridDungeonGenerator<R, W = WallType> {
    fn generate(&mut self, grid_width: usize, grid_height: usize) -> GridDungeon<R, W>;
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct RoomId(usize);

pub struct RandomRoomGridDungeonGenerator {
    room_chances: Vec<(RoomSize, usize)>
}

impl RandomRoomGridDungeonGenerator {
    pub fn new(room_chances: Vec<(RoomSize, usize)>) -> Self {
        RandomRoomGridDungeonGenerator {
            room_chances
        }
    }
}

fn random_room_placement<R, F>(
    target_tile: &TileAddress,
    room_chances: &Vec<(RoomSize, usize)>,
    rng: &mut R,
    is_unoccupied: F,
) -> Option<(RoomSize, TileAddress)>
    where
        R: Rng,
        F: Fn(&TileAddress) -> bool + Copy
{
    let (possible_placements, placement_weights): (Vec<(RoomSize, TileAddress)>, Vec<usize>) = room_chances.iter()
        .flat_map(|(room, chance)| {
            let TileAddress { x, y } = *target_tile;
            (0..room.width()).flat_map(move |dx| {
                (0..room.height()).flat_map(move |dy| {
                    if x >= dx && y >= dy {
                        let addr = TileAddress {
                            x: x - dx,
                            y: y - dy,
                        };
                        Some(((*room, addr), *chance))
                    } else {
                        None
                    }
                })
            })
        })
        .filter(|((room, target_tile), _)| {
            room.can_fit_at(target_tile, is_unoccupied)
        })
        .unzip();

    if possible_placements.is_empty() {
        None
    } else {
        // select one of the possible room placements
        let dist = WeightedIndex::new(&placement_weights).expect("assumed valid weights");
        Some(possible_placements[dist.sample(rng)])
    }
}

impl GridDungeonGenerator<Option<(RoomId, f32)>> for RandomRoomGridDungeonGenerator {
    fn generate(&mut self, grid_width: usize, grid_height: usize) -> GridDungeon<Option<(RoomId, f32)>, WallType> {
        let mut dungeon: GridDungeon<Option<(RoomId, f32)>, WallType> = GridDungeon::new(grid_width, grid_height);
        let mut rng = thread_rng();
        let mut next_room_id: usize = 0;
        let mut unassigned_tiles: Vec<TileAddress> = dungeon.tiles().tile_addresses().collect();

        // fill the grid with rooms
        while !unassigned_tiles.is_empty() {
            let target_tile = unassigned_tiles[rng.gen_range(0, unassigned_tiles.len())];
            let maybe_placed = random_room_placement(
                &target_tile,
                &self.room_chances,
                &mut rng,
                |addr| {
                    let tiles = dungeon.tiles();
                    tiles.has_tile(addr) && tiles[*addr].is_none()
                },
            );

            if let Some((room_size, room_origin)) = maybe_placed {
                let room_w = room_size.width();
                let room_h = room_size.height();
                let TileAddress { x: room_x, y: room_y } = room_origin;

                // assign tiles on the grid
                let room_id = RoomId(next_room_id);
                next_room_id += 1;
                let room_weight: f32 = rng.gen();
                for x in room_x..(room_x + room_w) {
                    for y in room_y..(room_y + room_h) {
                        let addr = TileAddress { x, y };
                        dungeon.tiles[addr] = Some((room_id, room_weight));
                    }
                }

                // remove all of the newly-assigned tiles from the 'unassigned' list
                unassigned_tiles.retain(|&TileAddress { x, y }| {
                    x < room_x ||
                        y < room_y ||
                        x >= room_x + room_w ||
                        y >= room_y + room_h
                })
            } else {
                break;
            }
        }

        // assign wall values at borders between different rooms
        for (wall_addr, wall_type) in dungeon.walls.iter_mut() {
            let tiles = &dungeon.tiles;
            let this_room = tiles[wall_addr.tile()].map(|(id, _)| id);
            let neighbor_room = wall_addr.neighbor().and_then(|n| {
                if tiles.has_tile(&n) {
                    tiles[n].map(|(id, _)| id)
                } else {
                    None
                }
            });
            if this_room != neighbor_room {
                *wall_type = WallType::Wall;
            }
        }

        dungeon
    }
}


trait Sliding<I, T> {
    fn sliding(self) -> SlidingIter<I, T>;
}

impl<S, T, I> Sliding<I, T> for S
    where
        S: IntoIterator<Item=T, IntoIter=I>,
        I: Iterator<Item=T>
{
    fn sliding(self) -> SlidingIter<I, T> {
        SlidingIter {
            current: None,
            iter: self.into_iter(),
        }
    }
}

struct SlidingIter<I, T> {
    current: Option<T>,
    iter: I,
}

impl<I, T> Iterator for SlidingIter<I, T>
    where
        I: Iterator<Item=T>,
        T: Copy,
{
    type Item = (T, T);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().and_then(|n| {
            match self.current.replace(n) {
                None => self.next(),
                Some(prev) => Some((prev, n)),
            }
        })
    }
}

type BasicGridDungeon = GridDungeon<Option<(RoomId, f32)>>;
type RoomIdPair = UnorderedPair<RoomId>;

#[derive(Eq, PartialEq)]
enum EdgeState {
    Connected,
    Unconnected,
}

// TODO: eventually make this a non-public implementation detail of the generator
pub struct GridDungeonGraph {
    dungeon: BasicGridDungeon,
    rooms: HashMap<RoomId, (TileAddress, f32)>,
    adjacency: HashMap<RoomId, HashSet<RoomId>>,
    edge_data: HashMap<RoomIdPair, EdgeState>,
}

impl From<BasicGridDungeon> for GridDungeonGraph {
    fn from(dungeon: BasicGridDungeon) -> Self {
        // create an index lookup for room weights and southwest corners
        let mut rooms: HashMap<RoomId, (TileAddress, f32)> = HashMap::new();
        dungeon.tiles().iter().for_each(|(addr, tile_data)| {
            if let Some((id, weight)) = tile_data {
                rooms.entry(*id)
                    .and_modify(|(prev_addr, prev_weight)| {
                        if addr.x < prev_addr.x || addr.y < prev_addr.y {
                            *prev_addr = addr;
                        }
                        if weight > prev_weight {
                            *prev_weight = *weight;
                        }
                    })
                    .or_insert((addr, *weight));
            }
        });

        // collect information about how the rooms relate to each other
        let mut adjacency: HashMap<RoomId, HashSet<RoomId>> = HashMap::new();
        let mut edge_data: HashMap<RoomIdPair, EdgeState> = HashMap::new();
        for (wall_addr, wall_type) in dungeon.walls().iter() {
            if *wall_type == WallType::Wall {
                if let Some(r1) = room_id_at(&dungeon, wall_addr.tile()) {
                    if let Some(r2) = wall_addr.neighbor().and_then(|n| room_id_at(&dungeon, n)) {
                        if r1 != r2 {
                            adjacency.entry(r1).or_default().insert(r2);
                            adjacency.entry(r2).or_default().insert(r1);
                            edge_data.entry(UnorderedPair(r1, r2)).or_insert(EdgeState::Unconnected);
                        }
                    }
                }
            }
        }

        GridDungeonGraph {
            dungeon,
            rooms,
            adjacency,
            edge_data,
        }
    }
}

fn room_id_at(dungeon: &BasicGridDungeon, tile: TileAddress) -> Option<RoomId> {
    dungeon.tiles().get(tile)?.map(|(id, _)| id)
}

/// Graph representing a series of interconnected points in the [0..1) space.
/// Meant to be interpreted as relative positions in a GridDungeon to describe
/// a general layout of main paths.
pub struct BiasGraph<N> {
    nodes: HashMap<N, (f32, f32)>,
    edges: Vec<(N, N)>,
}

impl<'a, N: 'a + Copy + Eq + Hash> BiasGraph<N> {
    pub fn new<NL, EL>(nodes: NL, edges: EL) -> Self
        where
            NL: IntoIterator<Item=&'a (N, (f32, f32))>,
            EL: IntoIterator<Item=&'a (N, N)>
    {
        let nodes_map: HashMap<N, (f32, f32)> = nodes.into_iter().cloned().collect();
        let edges_vec = edges.into_iter().cloned().filter(|(l, r)| {
            nodes_map.contains_key(l) && nodes_map.contains_key(r)
        }).collect();

        BiasGraph {
            nodes: nodes_map,
            edges: edges_vec,
        }
    }

    pub fn real_edges<P, FP>(&self, get_real_pos: FP) -> Vec<(P, P)>
        where
            P: Copy,
            FP: Fn(f32, f32) -> P
    {
        let addresses: HashMap<N, P> = self.nodes.iter().map(|(id, &(x, y))| (*id, get_real_pos(x, y))).collect();
        self.edges.iter().map(|(a, b)| {
            (addresses[a], addresses[b])
        }).collect()
    }
}

type GraphError = (&'static str, GridDungeonGraph);

impl GridDungeonGraph {
    pub fn take(self) -> BasicGridDungeon {
        self.dungeon
    }

    pub fn find_nearest_room(&self, target: &TileAddress) -> &(RoomId, f32) {
        match self.dungeon.tiles.get(*target) {
            // happy path: the dungeon has a room at the target address
            Some(Some(data)) => data,

            // no room at the address; do a search to find a valid address with room data
            _ => {
                let x = clamp(target.x, 0, self.dungeon.grid_width() - 1);
                let y = clamp(target.y, 0, self.dungeon.grid_height() - 1);
                let path_opt = dijkstra(
                    &TileAddress {
                        x: clamp(target.x, 0, self.dungeon.grid_width() - 1),
                        y: clamp(target.y, 0, self.dungeon.grid_height() - 1),
                    },
                    |&TileAddress { x, y }| {
                        let x_1: Option<usize> = x.checked_sub(1);
                        let y_1: Option<usize> = y.checked_sub(1);
                        vec![
                            Some((TileAddress { x, y: y + 1 }, 1000)), // North
                            Some((TileAddress { x: x + 1, y: y + 1 }, 1414)), // Northeast
                            Some((TileAddress { x: x + 1, y }, 1000)), // East
                            y_1.map(|yy| (TileAddress { x: x + 1, y: yy }, 1414)), // Southeast
                            y_1.map(|yy| (TileAddress { x, y: yy }, 1000)), // South
                            x_1.and_then(|xx| y_1.map(|yy| (TileAddress { x: xx, y: yy }, 1414))), // Southwest
                            x_1.map(|xx| (TileAddress { x: xx, y }, 1000)), // West
                            x_1.map(|xx| (TileAddress { x: xx, y: y + 1 }, 1414)), // Northwest
                        ].into_iter().flatten()
                    },
                    |addr| {
                        match self.dungeon.tiles.get(*addr) {
                            Some(Some(_data)) => true,
                            _ => false,
                        }
                    },
                );
                let path = path_opt.expect("No nearest tile. Grid must be completely empty.").0;
                let addr = path[path.len() - 1];
                match self.dungeon.tiles.get(addr) {
                    Some(Some(data)) => data,
                    _ => panic!("impossible case")
                }
            }
        }
    }

    pub fn connect_bias_paths<N: Copy + Eq + Hash>(&mut self, bias: &BiasGraph<N>) {
        let edges = bias.real_edges(|rx, ry| {
            self.find_nearest_room(&TileAddress {
                x: (rx * self.dungeon.grid_width() as f32).floor() as usize,
                y: (ry * self.dungeon.grid_height() as f32).floor() as usize,
            }).0
        });

        for (start_room, goal_room) in edges {
            let path_opt = dijkstra(
                &start_room,
                |current_room| {
                    self.adjacency
                        .get(current_room).into_iter()
                        .flat_map(|neighbors| {
                            neighbors.iter().map(|neighbor_id| {
                                // f32 doesn't have an Ord, so we'll map the [0.0, 1.0) range to [0, 256) for cost calculations
                                let cost = (self.rooms[neighbor_id].1 * 256.0) as i32;
                                (*neighbor_id, cost)
                            })
                        })
                },
                |current_room| { *current_room == goal_room },
            );

            if let Some((path, _path_cost)) = path_opt {
                for room in path.iter() {
                    self.set_room_weight(room, 2.0);
                }
                for (a, b) in path.iter().sliding() {
                    self.edge_data.insert(UnorderedPair(*a, *b), EdgeState::Connected);
                }
            }
        }
    }

    pub fn remove_unconnected_rooms(&mut self) {
        let connected_rooms: HashSet<RoomId> = self.edge_data.iter()
            .filter_map(|(edge, state)| {
                if *state == EdgeState::Connected { Some(edge) } else { None }
            })
            .flat_map(|UnorderedPair(r1, r2)| { vec![*r1, *r2] })
            .collect();

        // remove any walls
        {
            // local scope here to limit how long we mutably borrow `self.dungeon`
            let (tiles, walls) = self.dungeon.tiles_and_walls_mut();
            for (wall_addr, wall_type) in walls.iter_mut() {
                if let Some((r1, _)) = tiles[wall_addr.tile()] {
                    if let Some((r2, _)) = wall_addr.neighbor().and_then(|tile| tiles.get(tile)).and_then(|x| *x) {
                        if !connected_rooms.contains(&r1) && !connected_rooms.contains(&r2) {
                            *wall_type = WallType::Clear;
                        }
                    }
                }
            }
        }

        for (_addr, tile_data) in self.dungeon.tiles_mut().iter_mut() {
            match tile_data {
                Some((room_id, _)) => {
                    if !connected_rooms.contains(room_id) { *tile_data = None }
                }
                _ => ()
            }
        }
        self.adjacency.retain(|room_id, _neighbors| connected_rooms.contains(room_id));
        for (_, neighbors) in self.adjacency.iter_mut() {
            neighbors.retain(|id| connected_rooms.contains(id));
        }
    }

    pub fn random_branching_grow(&mut self, num_endpoints: usize) {
        // identify the "main path" of pre-connected rooms
        let preconnected: HashSet<RoomId> = self.edge_data.iter()
            .flat_map(|(rooms, edge_state)| {
                if *edge_state == EdgeState::Connected {
                    vec![rooms.0, rooms.1].into_iter()
                } else {
                    vec![].into_iter()
                }
            })
            .collect();

        // copy the pre-connected set of rooms in a set
        // that will grow to include any new rooms we visit below
        let mut connected = preconnected.clone();

        // pick an arbitrary point to use as the start in our path searching later
        let search_start = match connected.iter().next() {
            Some(x) => *x,
            None => return // bail if there are no pre-connected rooms
        };

        let mut rng = thread_rng();

        // try to get num_endpoints `target_room`s that aren't already in the 'connected' set,
        // but make sure to give up trying after `max_attempts` to avoid infinite looping if
        // every room happens to be connected
        let mut remaining_attempts = num_endpoints * 10;
        let mut remaining_outputs = num_endpoints;
        while remaining_attempts > 0 && remaining_outputs > 0 {
            remaining_attempts -= 1;

            // pick a random position, and whatever room is there is our target
            let x = rng.gen_range(0, self.dungeon.grid_width());
            let y = rng.gen_range(0, self.dungeon.grid_height());
            if let Some(target_room) = room_id_at(&self.dungeon, TileAddress { x, y }) {
                // avoid target rooms that are already connected
                if !connected.contains(&target_room) {
                    // use dijkstra's algorithm to get the shortest weighted path from our `search_start` to tht target room.
                    // treat any pre-connected room as a zero-cost node.
                    let path_opt = dijkstra(
                        &search_start,
                        |room| {
                            self.adjacency[room].iter().map(|neighbor| {
                                let cost = if preconnected.contains(neighbor) { 0 } else { (self.rooms[neighbor].1 * 256.0) as i32 };
                                (*neighbor, cost)
                            })
                        },
                        |room| *room == target_room,
                    );
                    if let Some((path, _cost)) = path_opt {
                        // success!
                        remaining_outputs -= 1;

                        // mark all rooms along the path as connected
                        path.iter().for_each(|room| {
                            if connected.insert(*room) {
                                self.set_room_weight(&room, 1.2);
                            }
                        });

                        // mark all edges along the path as connected
                        path.iter().sliding().for_each(|(a, b)| {
                            self.edge_data.insert(UnorderedPair(*a, *b), EdgeState::Connected);
                        });
                    }
                }
            }
        }
    }

    pub fn dfs_grow_path(&mut self, num_seeds: usize, mut iterations: u32) {
        // create a vector of the rooms that are "connected" via an EdgeState::Connected edge touching them.
        // avoid adding duplicate entries to the `frontier` by also tracking `seen` rooms
        let mut connected = HashSet::new();
        let mut frontier: Vec<RoomId> = self.edge_data.iter()
            .flat_map(|(rooms, edge_state)| {
                if *edge_state == EdgeState::Connected {
                    vec![rooms.0, rooms.1].into_iter()
                } else {
                    vec![].into_iter()
                }
            })
            .filter(|room_id| { connected.insert(*room_id) })
            .collect();

        // Remove any rooms from the `frontier` whose only adjacent neighbors are also in the `frontier`.
        // The whole point is to add connections to rooms that aren't in the frontier,
        // so if would defeat the purpose if we picked a room that was surrounded by already-connected rooms.
        frontier.retain(|room_id| {
            self.adjacency[room_id].iter().any(|neighbor_id| { !connected.contains(neighbor_id) })
        });

        // replace the "frontier" with a subset of the frontier rooms
        let mut rng = thread_rng();
        frontier = frontier.choose_multiple(&mut rng, num_seeds).cloned().collect();

        while iterations > 0 && !frontier.is_empty() {
            iterations -= 1;

            let advance_index = rng.gen_range(0, frontier.len());
            let advance_from = frontier[advance_index];
            let (neighbors, weights): (Vec<RoomId>, Vec<f32>) = self.adjacency[&advance_from].iter().cloned()
                .filter(|n_id| {
                    let edge = UnorderedPair(advance_from, *n_id);
                    self.edge_data[&edge] != EdgeState::Connected && !connected.contains(n_id)
                })
                .map(|n_id| {
                    // we want to favor lower-cost rooms in our choice of neighbor,
                    // which means inverting the weight for WeightedIndex distribution purposes
                    let weight = f32::max(0.0, 1.0 - self.rooms[&n_id].1);
                    (n_id, weight)
                })
                .unzip();
            if let Some(dist) = WeightedIndex::new(weights).ok() {
                let advance_to = neighbors[dist.sample(&mut rng)];

                self.edge_data.insert(UnorderedPair(advance_from, advance_to), EdgeState::Connected);
                if connected.insert(advance_to) {
                    frontier.push(advance_to);
                }
                // mark the added room with a special "weight" to make it render white
                self.set_room_weight(&advance_to, 1.4);
            }

            // update the frontier (don't try to be fancy by only removing the `room_id` and the `neighbor`,
            // since connecting the neighbor may have removed the last 'frontier room' from another room on the frontier)
            frontier.retain(|room_id| {
                self.adjacency[room_id].iter().any(|neighbor_id| { !connected.contains(neighbor_id) })
            });
        }
    }

    pub fn bfs_grow_path(&mut self, mut iterations: u32) {
        // create a vector of the rooms that are "connected" via an EdgeState::Connected edge touching them.
        // avoid adding duplicate entries to the `frontier` by also tracking `seen` rooms
        let mut seen = HashSet::new();
        let mut frontier: Vec<RoomId> = self.edge_data.iter()
            .flat_map(|(rooms, edge_state)| {
                if *edge_state == EdgeState::Connected {
                    vec![rooms.0, rooms.1].into_iter()
                } else {
                    vec![].into_iter()
                }
            })
            .filter(|room_id| { seen.insert(*room_id) })
            .collect();

        // Remove any rooms from the `frontier` whose only adjacent neighbors are also in the `frontier`.
        // The whole point is to add connections to rooms that aren't in the frontier,
        // so if would defeat the purpose if we picked a room that was surrounded by already-connected rooms.
        frontier.retain(|room_id| {
            self.adjacency[room_id].iter().any(|neighbor_id| { !seen.contains(neighbor_id) })
        });

        // with the setup done, we can start randomly picking rooms from the frontier and "connecting" them
        let mut rng = thread_rng();
        while iterations > 0 && !frontier.is_empty() {
            iterations -= 1;

            // pick a random unconnected neighbor that is adjacent to the 'frontier', and connect it
            let f_index = rng.gen_range(0, frontier.len());
            let room_id = frontier[f_index];
            let unconnected_neighbors: Vec<RoomId> = self.adjacency[&room_id].iter().cloned().filter(|neighbor_id| {
                let edge = UnorderedPair(room_id, *neighbor_id);
                self.edge_data[&edge] != EdgeState::Connected && !seen.contains(neighbor_id)
            }).collect();
            let neighbor = unconnected_neighbors.choose(&mut rng)
                .expect("Any room on the 'frontier' should have at least one unconnected neighbor");
            self.edge_data.insert(UnorderedPair(room_id, *neighbor), EdgeState::Connected);

            // mark the added room with a special "weight" to make it render white
            self.set_room_weight(neighbor, 1.6);

            // add the neighbor to the frontier (it'll just be removed next if it is surrounded)
            if seen.insert(*neighbor) {
                frontier.push(*neighbor);
            }

            // update the frontier (don't try to be fancy by only removing the `room_id` and the `neighbor`,
            // since connecting the neighbor may have removed the last 'frontier room' from another room on the frontier)
            frontier.retain(|room_id| {
                self.adjacency[room_id].iter().any(|neighbor_id| { !seen.contains(neighbor_id) })
            });
        }
    }

    /// Sets the weight of all tiles in the given room.
    /// Really this is used for debugging+rendering purposes,
    /// since the main display loop will pick a color based on the weight of each room.
    pub fn set_room_weight(&mut self, room: &RoomId, weight: f32) {
        self.rooms.entry(*room).and_modify(|(_, w)| *w = 2.0);

        if let Some(room_addr) = self.rooms.get(room).map(|(addr, _)| *addr) {
            // the `rooms` index is initialized with the lower-left corners, so we need to identify the upper-right corner
            let mut corner_x = room_addr.x;
            let mut corner_y = room_addr.y;
            let check = |x: usize, y: usize| {
                self.dungeon.tiles().get(TileAddress { x, y }).map_or_else(|| false, |tile_data| {
                    match tile_data {
                        Some((id, _)) => id == room,
                        _ => false
                    }
                })
            };
            while check(corner_x, corner_y) { corner_x += 1 }
            corner_x -= 1;
            while check(corner_x, corner_y) { corner_y += 1 }
            corner_y -= 1;

            for x in room_addr.x..(corner_x + 1) {
                for y in room_addr.y..(corner_y + 1) {
                    self.dungeon.tiles_mut()[TileAddress { x, y }] = Some((*room, weight));
                }
            }
        }
    }

    pub fn insert_doors(&mut self) -> () {

        // collect walls that need to be converted to doors
        let mut doorable_walls: HashMap<RoomIdPair, Vec<WallAddress>> = HashMap::new();
        for (wall_addr, wall_data) in self.dungeon.walls().iter() {
            if let Some(r1) = room_id_at(&self.dungeon, wall_addr.tile()) {
                if let Some(r2) = wall_addr.neighbor().and_then(|n| room_id_at(&self.dungeon, n)) {
                    let edge = UnorderedPair(r1, r2);
                    match self.edge_data.get(&edge) {
                        Some(&EdgeState::Connected) => {
                            doorable_walls.entry(edge).or_default().push(wall_addr);
                        }
                        _ => (),
                    }
                }
            }
        }
        // knock down a random wall for each inter-room edge
        let mut rng = thread_rng();
        for (_, walls) in doorable_walls {
            let wall = walls[rng.gen_range(0, walls.len())];
            self.dungeon.walls[wall] = WallType::Door;
        }
    }
}