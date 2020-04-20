use std::cmp::{max, Ordering};

use rand::{Rng, thread_rng};
use rand::distributions::{Distribution, Uniform, WeightedIndex};
use std::collections::HashMap;
use std::iter::Iterator;
use std::fmt::Debug;

pub trait DungeonGenerator {
    fn generate(&self, bounds: &Rect) -> Vec<Rect>;
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

// ---------------------------------------------------------------------------

pub struct NaiveRandomDungeon {
    pub count: u32,
}
impl DungeonGenerator for NaiveRandomDungeon {
    fn generate(&self, bounds: &Rect) -> Vec<Rect> {
        let width_range = Uniform::new(10, max(10, bounds.width() / 3));
        let height_range = Uniform::new(10, max(10, bounds.height() / 3));
        let mut rng = thread_rng();

        let mut v = Vec::new();

        for _ in 1..self.count {
            let width = width_range.sample(&mut rng);
            let height = height_range.sample(&mut rng);
            let x = rng.gen_range(bounds.x_min(), bounds.x_max() - width);
            let y = rng.gen_range(bounds.y_min(), bounds.y_max() - height);
            v.push(Rect::from_xywh(x, y, width, height));
        }

        v
    }
}

trait TileScale {
    fn pixel_to_tile(&self, grid_size: i32) -> Self;
    fn tile_to_pixel(&self, grid_size: i32) -> Self;
}
impl TileScale for (i32, i32) {
    fn pixel_to_tile(&self, grid_size: i32) -> Self {
        (self.0 / grid_size, self.1 / grid_size)
    }
    fn tile_to_pixel(&self, grid_size: i32) -> Self {
        (self.0 * grid_size, self.1 * grid_size)
    }
}
impl TileScale for Rect {
    fn pixel_to_tile(&self, grid_size: i32) -> Self {
        Rect::from(Corners(
            self.lower_left().pixel_to_tile(grid_size),
            self.upper_right().pixel_to_tile(grid_size),
        ))
    }
    fn tile_to_pixel(&self, grid_size: i32) -> Self {
        Rect::from(Corners(
            self.lower_left().tile_to_pixel(grid_size),
            self.upper_right().tile_to_pixel(grid_size),
        ))
    }
}

pub struct SliceAwayGenerator;
impl DungeonGenerator for SliceAwayGenerator {
    fn generate(&self, bounds: &Rect) -> Vec<Rect> {
        let mut rng = thread_rng();
        let tile_size = 25;

        let tile_bounds = bounds.pixel_to_tile(tile_size);

        let mut out = Vec::new();
        slice_away_split(&mut rng, &tile_bounds, 20, &mut out);

        out.iter().map(|r| r.tile_to_pixel(tile_size)).collect()
    }
}

type RoomSize = (i32, i32);

/// ((width, height), probability_weight)
fn room_tile_sizes() -> Vec<(RoomSize, u32)> {
    vec![
        ((5, 4), 1),
        ((4, 4), 2),
        ((4, 2), 4),
        ((3, 4), 6),
        ((3, 3), 8),
        ((3, 2), 10),
        ((3, 1), 2),
        ((2, 2), 10),
        ((2, 1), 2),
        ((1, 1), 1)
    ]
}
fn slice_away_split<R: Rng>(rng: &mut R, bounds: &Rect, remaining_depth: u32, out: &mut Vec<Rect>) {
    if remaining_depth == 0 || bounds.area() <= 1 {
        out.push(*bounds);
    } else {
        let (possible_rooms, weights): (Vec<(i32, i32)>, Vec<u32>) = room_tile_sizes().iter()
            .flat_map(|((w, h), i)| { vec![((*w, *h), *i), ((*h, *w), *i)] })
            .filter(|((width, height), _)| {  *width <= bounds.width() && *height <= bounds.height() })
            .unzip();

        match WeightedIndex::new(weights) {
            Err(_) => {
                out.push(*bounds);
            },
            Ok(dist) => {
                // pick a random room size
                let (room_w, room_h) = possible_rooms[dist.sample(rng)];
                // pick a random position for the room within the bounds (the +1 is to make the range inclusive)
                let room_x = rng.gen_range(bounds.x_min(), bounds.x_max() - room_w + 1);
                let room_y = rng.gen_range(bounds.y_min(), bounds.y_max() - room_h + 1);

                let room = Rect::from_xywh(room_x, room_y, room_w, room_h);

                // cut the room out of the `bounds`, and parition out the remaining Rects
                bounds.slice_away(&room).random_stitch(rng).iter().for_each(|r| {
                    slice_away_split(rng, r, remaining_depth - 1, out);
                });

                // don't split the room; it's just the way we want it
                out.push(room);
            },
        };
    }
}


pub struct RandomRoomGridGenerator {
    tile_size: i32,
}
impl RandomRoomGridGenerator {
    pub fn new(tile_size: i32) -> Self {
        RandomRoomGridGenerator { tile_size }
    }
}
impl DungeonGenerator for RandomRoomGridGenerator {
    fn generate(&self, bounds: &Rect) -> Vec<Rect> {
        let grid_width = bounds.width() / self.tile_size;
        let grid_height = bounds.height() / self.tile_size;

        let room_weights = room_tile_sizes().iter()
            .flat_map(|((w, h), i)| { vec![((*w, *h), *i), ((*h, *w), *i)] })
            .collect();
            // .filter(|((width, height), _)| {  *width <= bounds.width() && *height <= bounds.height() })
            // .unzip();

        // let rooms: Vec<Rect> = room_tile_sizes().iter()
        //     .map(|(room_size, _)| *room_size)
        //     .flat_map(|(w,h)| vec![(w,h), (h,w)])
        //     .map(|(w,h)| Rect::from_xywh(0, 0, w, h))
        //     .collect();

        let mut grid = RoomGrid::new(grid_width, grid_height);
        let mut out = Vec::new();
        // for _ in 0..100 {
            while let Some(room) = grid.try_place_room(&room_weights) {
                out.push(room.tile_to_pixel(self.tile_size));
            }
        // }
        out
    }
}

type Tile = (i32, i32);

struct RoomGrid {
    unassigned_tiles: Vec<Tile>,
    tile_states: HashMap<Tile, bool>,
}
impl RoomGrid {
    pub fn new(grid_width: i32, grid_height: i32) -> Self {
        let unassigned_tiles: Vec<Tile> = (0..grid_width).flat_map(|x| (0..grid_height).map(move |y| (x, y))).collect();
        let mut tile_states: HashMap<Tile, bool> = HashMap::new();
        for tile in unassigned_tiles.iter() {
            tile_states.insert(*tile, false);
        }
        RoomGrid {
            unassigned_tiles,
            tile_states
        }
    }

    pub fn try_place_room(&mut self, room_weights: &Vec<(RoomSize, u32)>) -> Option<Rect> {
        let mut rng = thread_rng();
        if self.unassigned_tiles.is_empty() || room_weights.is_empty() {
            return None
        }
        // randomly pick a starting point from the unassigned tiles
        let start_index = rng.gen_range(0, self.unassigned_tiles.len());
        let start_tile = self.unassigned_tiles[start_index];

        let can_place_room = |(w, h): RoomSize, (x, y): Tile| {
            for i in x..(x + w) {
                for j in y..(y + h) {
                    if self.is_assigned(&(i, j)) || !self.has_tile(&(i, j)) {
                        return false
                    }
                }
            }
            return true
        };

        let valid_weighted_room_placements: Vec<(RoomSize, u32, Tile)> = room_weights.iter()
            .flat_map(|(room, chance)| {
                (0..room.0).flat_map(move |dx| {
                    (0..room.1).map(move |dy| {
                        (*room, *chance, (start_tile.0 - dx, start_tile.1 - dy))
                    })
                })
            })
            .filter(|(room, _chance, tile)| {
                can_place_room(*room, *tile)
            })
            .collect();

        if valid_weighted_room_placements.is_empty() {
            return None
        }

        let (valid_room_placements, valid_room_weights): (Vec<(RoomSize, Tile)>, Vec<u32>) = valid_weighted_room_placements.iter()
            .map(|(size, chance, pos)| ((*size, *pos), *chance))
            .unzip();
        let dist = WeightedIndex::new(&valid_room_weights).expect("assumed valid weights");
        let ((room_w, room_h), (room_x, room_y)) = valid_room_placements[dist.sample(&mut rng)];
        // let (room, room_x, room_y) = valid_weighted_room_placements[rng.gen_range(0, valid_weighted_room_placements.len())];

        // remove all tiles associated with the room and its placement from the `unassigned_tiles` vec
        self.unassigned_tiles.retain(|&(x, y)| {
            x < room_x ||
                y < room_y ||
                x >= room_x + room_w ||
                y >= room_y + room_h
        });
        // mark all tiles associated with the room and its placement as 'assigned'
        for x in room_x..(room_x + room_w) {
            for y in room_y..(room_y + room_h) {
                self.tile_states.insert((x, y), true);
            }
        }

        let placed_room = Rect::from_xywh(room_x, room_y, room_w, room_h);
        Some(placed_room)
    }

    fn is_assigned(&self, tile: &(i32, i32)) -> bool {
        match self.tile_states.get(tile) {
            None => false,
            Some(assigned) => *assigned,
        }
    }
    fn has_tile(&self, tile: &(i32, i32)) -> bool {
        self.tile_states.get(tile).is_some()
    }
}
