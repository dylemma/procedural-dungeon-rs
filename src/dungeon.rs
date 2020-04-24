use std::cmp::{max, Ordering};

use std::num::NonZeroUsize;
use rand::{Rng, thread_rng};
use rand::distributions::{Distribution, WeightedIndex};
use std::collections::HashMap;
use std::iter::Iterator;
use std::fmt::Debug;
use crate::tile::{WallType, GridTiles, GridWalls, TileAddress};

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

pub struct Corners<T>(pub (T, T), pub (T, T));

#[derive(Debug, Clone, Copy)]
pub struct Rect {
    x_min: i32,
    x_max: i32,
    y_min: i32,
    y_max: i32,
}
impl Rect {
    pub fn from_xywh(x: i32, y: i32, w: i32, h: i32) -> Rect {
        Rect {
            x_min: x,
            x_max: x + max(0, w),
            y_min: y,
            y_max: y + max(0, h),
        }
    }
    pub fn x_min(&self) -> i32 { self.x_min }
    pub fn x_max(&self) -> i32 { self.x_max }
    pub fn y_min(&self) -> i32 { self.y_min }
    pub fn y_max(&self) -> i32 { self.y_max }
    pub fn width(&self) -> i32 { self.x_max - self.x_min }
    pub fn height(&self) -> i32 { self.y_max - self.y_min }
    pub fn lower_left(&self) -> (i32, i32) { (self.x_min, self.y_min) }
    pub fn upper_right(&self) -> (i32, i32) { (self.x_max, self.y_max) }
    pub fn area(&self) -> i32 { self.width() * self.height() }
    // pub fn intersects(&self, that: &RectU32)

    pub fn slice_away(&self, hole: &Rect) -> RectSliceAway {
        let mut tiles: Vec<Rect> = Vec::new();

        // follow a path clockwise around the hole, starting at the upper-left corner,
        // adding "tiles"
        let stop_indexes = vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1)];
        let x_stops = vec![self.x_min, hole.x_min, hole.x_max, self.x_max];
        let y_stops = vec![self.y_max, hole.y_max, hole.y_min, self.y_min];

        for (i,j) in stop_indexes {
            let x_min = x_stops[i];
            let x_max = x_stops[i+1];
            let y_max = y_stops[j];
            let y_min = y_stops[j+1];

            if x_min < x_max && y_min < y_max {
                tiles.push(Corners((x_min, y_min), (x_max, y_max)).into());
            }
        }

        RectSliceAway(tiles)
    }
}
impl <T> From<Corners<T>> for Rect
    where T: Into<i32>
{
    fn from(corners: Corners<T>) -> Self {
        let Corners((x1, y1), (x2, y2)) = corners;
        let (x_min, x_max) = arrange(x1.into(), x2.into());
        let (y_min, y_max) = arrange(y1.into(), y2.into());
        Rect { x_min, x_max, y_min, y_max }
    }
}

pub struct RectSliceAway(Vec<Rect>);
impl RectSliceAway {
    pub fn random_stitch<R: Rng>(self, rng: &mut R) -> Vec<Rect> {
        let mut pending = self.0;
        let mut out: Vec<Rect> = Vec::new();
        while !pending.is_empty() {
            // pick a random index in the vector and check if the tiles at neighboring indexes are adjacent
            let i = rng.gen_range(0, pending.len());

            let can_weld_forward = check_weldability(i, &pending, true);
            let can_weld_backward = check_weldability(i, &pending, false);

            // if it could go either way, randomly choose which direction to go
            let weld_forward = if can_weld_forward && can_weld_backward {
                rng.gen_bool(0.5)
            } else if can_weld_backward {
                false
            } else {
                true
            };
            let (pending2, focus) = do_welds(i, pending, weld_forward);
            pending = pending2;
            out.push(focus);
        }

        out
    }
}

fn check_weldability(current: usize, vec: &Vec<Rect>, forward: bool) -> bool {
    if let Some((next_index, current_rect)) = check_vec_and_advance(current, vec, forward) {
        if let Some((_, next_rect)) = check_vec_and_advance(next_index, vec, forward) {
            return weld(current_rect, next_rect).is_some();
        }
    }
    return false;
}
fn do_welds(current: usize, vec: Vec<Rect>, forward: bool) -> (Vec<Rect>, Rect) {
    let mut drain = VecDrain { vec, current, forward };
    let mut out = drain.take_current().unwrap();
    loop {
        match drain.check_current() {
            None => break,
            Some(incoming) => {
                if let Some(welded) = weld(&out, incoming) {
                    out = welded;
                    drain.take_current();
                } else {
                    break;
                }
            },
        }
    }
    (drain.restore(), out)
}

fn check_vec_and_advance<T>(index: usize, vec: &Vec<T>, forward: bool) -> Option<(usize, &T)> {
    let len = vec.len();
    if index >= len {
        None
    } else {
        let item = &vec[index];
        let next_index = if forward {
            (index + 1) % len
        } else {
            (index + len - 1) % len
        };
        Some((next_index, item))
    }
}

struct VecDrain<T> {
    vec: Vec<T>,
    current: usize,
    forward: bool,
}

impl <T> VecDrain<T>
{
    fn restore(self) -> Vec<T> {
        self.vec
    }

    fn check_current(&self) -> Option<&T> {
        if self.vec.is_empty() {
            None
        } else {
            Some(&self.vec[self.current])
        }
    }

    fn take_current(&mut self) -> Option<T> {
        let len = self.vec.len();
        if len == 0 {
            None
        } else {
            let out = self.vec.remove(self.current);
            let new_len = self.vec.len();
            if new_len > 0 {
                // wrap back to the beginning if we removed the last element while moving forward
                if self.forward {
                    if self.current >= new_len {
                        self.current = 0;
                    }
                } else {
                    // move the index backwards, possibly wrapping to the end of the array, if moving backward
                    self.current = (self.current + new_len - 1) % new_len;
                }
            }
            Some(out)
        }
    }
}


fn weld(a: &Rect, b: &Rect) -> Option<Rect> {
    weld_h(a, b).or_else(|| {
        weld_h(b, a).or_else(|| {
            weld_v(a, b).or_else(|| {
                weld_v(b, a)
            })
        })
    })
}
fn weld_h(left: &Rect, right: &Rect) -> Option<Rect> {
    // same height, same y pos, and the X min/max are touching
    if (left.height() == right.height()) && (left.y_max() == right.y_max()) && (left.x_max() == right.x_min()) {
        Some(Corners(left.lower_left(), right.upper_right()).into())
    } else {
        None
    }
}
fn weld_v(top: &Rect, bottom: &Rect) -> Option<Rect> {
    if (top.width() == bottom.width()) && (top.x_min() == bottom.x_min()) && (top.y_min() == bottom.y_max()) {
        Some(Corners(top.upper_right(), bottom.lower_left()).into())
    } else {
        None
    }
}

fn arrange<T: Ord>(a: T, b: T) -> (T, T) {
    match a.cmp(&b) {
        Ordering::Less => (a, b),
        _ => (b, a)
    }
}

// =============================================

#[derive(Copy, Clone, Debug)]
pub struct RoomSize(NonZeroUsize, NonZeroUsize);
impl RoomSize {
    pub fn new(x: usize, y: usize) -> Option<RoomSize> {
        NonZeroUsize::new(x).and_then(move |xx| {
            NonZeroUsize::new(y).map(|yy| {
                RoomSize(xx, yy)
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
}

fn can_fit_room <F>(
    room: &RoomSize,
    anchor: &TileAddress,
    is_unoccupied: F
) -> bool
where
    F: Fn(&TileAddress) -> bool
{
    for x in anchor.x..(anchor.x + room.width()) {
        for y in anchor.y..(anchor.y + room.height()) {
            if !is_unoccupied(&TileAddress { x, y }) {
                return false
            }
        }
    }
    true
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
            can_fit_room(room, target_tile, is_unoccupied)
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