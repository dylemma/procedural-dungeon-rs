use std::cmp::{max, Ordering};

use rand::Rng;
use vecmath;

pub type Point = vecmath::Vector2<f64>;
pub type Pixel = vecmath::Vector2<u32>;

pub fn point_eq<T: PartialEq>(l: &vecmath::Vector2<T>, r: &vecmath::Vector2<T>) -> bool {
    l[0] == r[0] && l[1] == r[1]
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