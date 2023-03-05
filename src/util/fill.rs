use std::fmt::Debug;
use std::iter::Iterator;
use std::ops::{Not, RangeInclusive};

pub trait Tiles {
    type Tile: Sized + PartialEq;

    fn get_tile(&self, x: usize, y: usize) -> Option<&Self::Tile>;
    fn set_tile(&mut self, x: usize, y: usize, tile: Self::Tile);
}

pub fn flood_fill<G, P, T>(
    tiles: &mut G,
    start: (usize, usize),
    color_equivalence: P,
    color: T,
)
    where G: Tiles<Tile=T>,
          T: Clone + Debug,
          P: Fn(&T, &T) -> bool,
{
    /* This is an implementation of the "combined-scan-and-fill" algorithm described on Wikipedia:
     *  - Get start color from the starting tile
     *  - Expand left and right from the starting point to get the initial row
     *  - Add scan seeds for the Spans above and below the initial row
     *  - For each scan seed:
     *    - Find consecutive Spans in the next row up/down that are adjacent to the parent Span
     *    - If any of those Spans overhangs the parent span, recurse back to the parent row where it overhung
     *    - Also recurse (by pushing a Seed to the stack) into the next row in the current direction
     */

    let start_color: T = match tiles.get_tile(start.0, start.1) {
        Some(t) => t.clone(),
        None => return,
    };

    let mut seed_stack: Vec<(Span, usize, Dir)> = Vec::new();

    if let Some(start_range) = expand_range(start.0, &GridRowFloodTest {
        grid: tiles,
        tile_test: |c: &T| color_equivalence(c, &start_color),
        y: start.1,
    }) {
        // fill the initial row
        for x in start_range.into_iter() {
            tiles.set_tile(x, start.1, color.clone());
        }

        // seed the next row up
        if let Some(up_y) = Dir::Up.step(start.1) {
            seed_stack.push((start_range, up_y, Dir::Up));
        }

        // seed the next row down
        if let Some(down_y) = Dir::Down.step(start.1) {
            seed_stack.push((start_range, down_y, Dir::Down));
        }
    }

    // Stack-based recursion by pushing and popping to the seed_stack.
    // `parent_range` is a x=min..=max span representing a series of consecutive filled pixels from the previous row.
    // `y` is the current row coordinate
    // `dir` is the up/down direction that was taken to get from the parent row to the current `y`
    while let Some((parent_range, y, dir)) = seed_stack.pop() {
        let Span(parent_start, parent_end) = parent_range;

        // precalculate the "overhang" thresholds for when a parent span reaches past the parent range
        let parent_start_minus_2 = parent_start.checked_sub(2);
        let parent_end_plus_2 = parent_end.checked_add(2);

        // scan over the "child" row for Spans of consecutive "inside" tiles
        let mut child_scan = ChildScan::start(parent_range);
        while let Some(child_range) = child_scan.next(&GridRowFloodTest {
            grid: tiles,
            y,
            tile_test: |c: &T| color_equivalence(c, &start_color),
        }) {

            // fill the tiles in the child span
            for x in child_range.into_iter() {
                tiles.set_tile(x, y, color.clone());
            }

            // add a new seed using the child range as a parent, in the same y direction
            if let Some(next_y) = dir.step(y) {
                seed_stack.push((child_range, next_y, dir));
            }

            // if the child range overhung the parent range, we've passed some obstacle
            // on the previous row, and need to jump back to that row to continue
            if let Some(prev_y) = (!dir).step(y) {
                let Span(child_start, child_end) = child_range;

                if let Some(ps2) = parent_start_minus_2 {
                    if child_start <= ps2 {
                        seed_stack.push((Span(child_start, ps2), prev_y, !dir));
                    }
                }
                if let Some(pe2) = parent_end_plus_2 {
                    if child_end >= pe2 {
                        seed_stack.push((Span(pe2, child_end), prev_y, !dir));
                    }
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Dir {
    Up,
    Down,
}

impl Dir {
    fn step(&self, i: usize) -> Option<usize> {
        match *self {
            Dir::Up => i.checked_add(1),
            Dir::Down => i.checked_sub(1),
        }
    }
}

impl Not for Dir {
    type Output = Dir;
    fn not(self) -> Self::Output {
        match self {
            Dir::Up => Dir::Down,
            Dir::Down => Dir::Up,
        }
    }
}

#[derive(Copy, Clone)]
struct Span(usize, usize);

impl IntoIterator for Span {
    type Item = usize;
    type IntoIter = RangeInclusive<usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.0..=self.1
    }
}

trait FloodTest {
    fn inside(&self, x: usize) -> bool;
}

struct GridRowFloodTest<'g, G, P> {
    grid: &'g G,
    tile_test: P,
    y: usize,
}

impl<'g, G, P, T> FloodTest for GridRowFloodTest<'g, G, P>
    where G: Tiles<Tile=T>,
          P: Fn(&T) -> bool,
{
    fn inside(&self, x: usize) -> bool {
        match self.grid.get_tile(x, self.y) {
            None => false,
            Some(t) => (self.tile_test)(t),
        }
    }
}

fn expand_range<F>(x: usize, test: &F) -> Option<Span>
    where F: FloodTest,
{
    if test.inside(x) {
        let x_min = DescendFrom(Some(x))
            .take_while(|x| test.inside(*x))
            .last()
            .unwrap_or_else(|| x);
        let x_max = AscendFrom(Some(x))
            .take_while(|x| test.inside(*x))
            .last()
            .unwrap_or_else(|| x);
        Some(Span(x_min, x_max))
    } else {
        None
    }
}

struct ChildScan {
    parent_range: Span,
    current_x: Option<usize>,
}

impl ChildScan {
    fn start(parent_range: Span) -> Self {
        ChildScan {
            current_x: Some(parent_range.0),
            parent_range,
        }
    }
    fn next<F: FloodTest>(&mut self, test: &F) -> Option<Span> {
        let current_x = self.current_x?;
        match next_child_range_right(self.parent_range.1, current_x, test) {
            Some(next_range) => {
                let next_x = next_range.1.checked_add(2);
                self.current_x = next_x;
                Some(next_range)
            }
            None => {
                self.current_x = None;
                None
            }
        }
    }
}

fn next_child_range_right<F>(parent_max_x: usize, current_x: usize, test: &F) -> Option<Span>
    where F: FloodTest,
{
    // Find the leftmost X in [start_x, parent_max_x] that passes the test.
    // This represents a tile adjacent to an already-filled tile in the parent row.
    let x_min = (current_x..=parent_max_x).find(|x| test.inside(*x))?;
    expand_range(x_min, test)
}

struct DescendFrom(Option<usize>);

impl Iterator for DescendFrom {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let prev = self.0?.checked_sub(1);
        self.0 = prev.clone();
        prev
    }
}

struct AscendFrom(Option<usize>);

impl Iterator for AscendFrom {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.0?.checked_add(1);
        self.0 = next.clone();
        next
    }
}