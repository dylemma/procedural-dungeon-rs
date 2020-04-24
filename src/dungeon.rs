use std::cmp::{max, Ordering};
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Iterator;
use std::num::NonZeroUsize;

use rand::{Rng, thread_rng};
use rand::distributions::{Distribution, WeightedIndex};

use crate::tile::{GridTiles, GridWalls, TileAddress, WallType};

pub struct GridDungeon<R, W = WallType> {
    grid_width: usize,
    grid_height: usize,
    tiles: GridTiles<R>,
    walls: GridWalls<W>,
}
impl <R, W> GridDungeon<R, W>
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
impl <R, W> GridDungeon<R, W> {
    pub fn grid_width(&self) -> usize { self.grid_width }
    pub fn grid_height(&self) -> usize { self.grid_height }
    pub fn tiles(&self) -> &GridTiles<R> { &self.tiles }
    pub fn walls(&self) -> &GridWalls<W> { &self.walls }
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
                    return false
                }
            }
        }
        true
    }
}


pub trait GridDungeonGenerator<R, W = WallType> {
    fn generate(&mut self, grid_width: usize, grid_height: usize) -> GridDungeon<R, W>;
}



#[derive(Copy, Clone, Hash, Eq, PartialEq)]
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
                }
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
                println!("Wall at {:?}", wall_addr);
                *wall_type = WallType::Wall;
            }
        }

        dungeon
    }
}