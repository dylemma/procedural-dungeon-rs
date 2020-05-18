use std::borrow::{Borrow, BorrowMut};
use std::cmp::Eq;
use std::convert::TryFrom;
use std::iter::Enumerate;
use std::ops::{Add, Index, IndexMut};
use std::slice::{Iter, IterMut};

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct TileAddress {
    pub x: usize,
    pub y: usize,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum CompassDirection {
    North,
    East,
    South,
    West,
}
impl CompassDirection {
    pub fn reflect(&self) -> CompassDirection {
        match *self {
            CompassDirection::North => CompassDirection::South,
            CompassDirection::South => CompassDirection::North,
            CompassDirection::East => CompassDirection::West,
            CompassDirection::West => CompassDirection::East,
        }
    }
}

impl Add<CompassDirection> for TileAddress {
    type Output = Option<TileAddress>;

    fn add(self, rhs: CompassDirection) -> Option<TileAddress> {
        match rhs {
            CompassDirection::North => Some(TileAddress { x: self.x, y: self.y + 1 }),
            CompassDirection::East => Some(TileAddress { x: self.x + 1, y: self.y }),
            CompassDirection::South => {
                if self.y > 0 {
                    Some(TileAddress { x: self.x, y: self.y - 1 })
                } else {
                    None
                }
            },
            CompassDirection::West => {
                if self.x > 0 {
                    Some(TileAddress { x: self.x - 1, y: self.y })
                } else {
                    None
                }
            },
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum WallType {
    Clear,
    Wall,
    Door
}
impl Default for WallType {
    fn default() -> Self {
        WallType::Clear
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct WallAddress {
    tile: TileAddress,
    direction: CompassDirection,
}

impl WallAddress {
    pub fn new(tile: TileAddress, direction: CompassDirection) -> Self {
        match direction {
            CompassDirection::North | CompassDirection::East => {
                WallAddress { tile, direction }
            }
            CompassDirection::South => {
                let TileAddress { x, y } = tile;
                if y > 0 {
                    // south walls of non-edge tiles are treated as the north wall of the southern neighbor
                    WallAddress {
                        tile: TileAddress { x, y: y - 1 },
                        direction: CompassDirection::North,
                    }
                } else {
                    // south walls of edge tiles at y=0 are not changed
                    WallAddress { tile, direction }
                }
            }
            CompassDirection::West => {
                let TileAddress { x, y } = tile;
                if x > 0 {
                    // west walls of non-edge tiles are treated as the east wall of the western neighbor
                    WallAddress {
                        tile: TileAddress { x: x - 1, y },
                        direction: CompassDirection::East
                    }
                } else {
                    // west walls of edge tiles at x=y are not changed
                    WallAddress { tile, direction }
                }
            }
        }
    }
    pub fn tile(&self) -> TileAddress {
        self.tile
    }
    pub fn neighbor(&self) -> Option<TileAddress> {
        self.tile + self.direction
    }
    pub fn direction(&self) -> CompassDirection {
        self.direction
    }
}
impl From<(TileAddress, CompassDirection)> for WallAddress {
    fn from((tile, direction): (TileAddress, CompassDirection)) -> Self {
        WallAddress::new(tile, direction)
    }
}

pub struct GridTiles<T> {
    grid_width: usize,
    grid_height: usize,
    tiles_data: Vec<T>
}
impl <T: Default + Clone> GridTiles<T> {
    pub fn new(grid_width: usize, grid_height: usize) -> Self {
        let tiles_data = vec![Default::default(); grid_width * grid_height];
        GridTiles { grid_width, grid_height, tiles_data }
    }

}
impl <T> GridTiles<T> {
    pub fn has_tile(&self, address: &TileAddress) -> bool {
        address.x < self.grid_width && address.y < self.grid_height
    }
    pub fn tile_addresses(&self) -> GridTilesAddresses<T> {
        GridTilesAddresses {
            parent: self,
            i: 0,
        }
    }
    pub fn iter(&self) -> GridTilesIter<T> {
        GridTilesIter {
            grid_width: self.grid_width,
            inner: self.tiles_data.iter().enumerate(),
        }
    }
    pub fn iter_mut(&mut self) -> GridTilesIterMut<T> {
        GridTilesIterMut {
            grid_width: self.grid_width,
            inner: self.tiles_data.iter_mut().enumerate()
        }
    }
    pub fn get(&self, index: TileAddress) -> Option<&T> {
        if self.has_tile(&index) {
            let i = index.x + (index.y * self.grid_width);
            self.tiles_data.get(i)
        } else {
            None
        }
    }
    pub fn get_mut(&mut self, index: TileAddress) -> Option<&mut T> {
        if self.has_tile(&index) {
            let i = index.x + (index.y * self.grid_width);
            self.tiles_data.get_mut(i)
        } else {
            None
        }
    }
}
impl <T> Index<TileAddress> for GridTiles<T> {
    type Output = T;

    fn index(&self, index: TileAddress) -> &Self::Output {
        let i = index.x + (index.y * self.grid_width);
        &self.tiles_data[i]
    }
}
impl <T> IndexMut<TileAddress> for GridTiles<T> {
    fn index_mut(&mut self, index: TileAddress) -> &mut Self::Output {
        let i = index.x + (index.y * self.grid_width);
        &mut self.tiles_data[i]
    }
}

pub struct GridTilesAddresses<'a, T> {
    parent: &'a GridTiles<T>,
    i: usize,
}
impl <'a, T> Iterator for GridTilesAddresses<'a, T> {
    type Item = TileAddress;

    fn next(&mut self) -> Option<Self::Item> {
        let w = self.parent.grid_width;
        let y = self.i / w;
        if y >= self.parent.grid_height {
            None
        } else {
            let x = self.i % w;
            self.i += 1;
            Some(TileAddress{ x, y })
        }
    }
}

pub struct GridTilesIter<'a, T> {
    grid_width: usize,
    inner: Enumerate<Iter<'a, T>>
}
impl <'a, T> Iterator for GridTilesIter<'a, T> {
    type Item = (TileAddress, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(index, t)| {
            let x = index % self.grid_width;
            let y = index / self.grid_width;
            (TileAddress { x, y }, t)
        })
    }
}
pub struct GridTilesIterMut<'a, T> {
    grid_width: usize,
    inner: Enumerate<IterMut<'a, T>>
}
impl <'a, T> Iterator for GridTilesIterMut<'a, T> {
    type Item = (TileAddress, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(index, t)| {
            let x = index % self.grid_width;
            let y = index / self.grid_width;
            (TileAddress { x, y }, t)
        })
    }
}

pub struct GridWalls<T> {
    grid_width: usize,
    grid_height: usize,
    north_walls: Vec<T>,
    east_walls: Vec<T>,
    south_walls: Vec<T>,
    west_walls: Vec<T>,
}

impl<T: Default + Clone> GridWalls<T> {
    pub fn new(grid_width: usize, grid_height: usize) -> Self {
        GridWalls {
            grid_width,
            grid_height,
            north_walls: vec![Default::default(); grid_width * grid_height],
            east_walls: vec![Default::default(); grid_width * grid_height],
            south_walls: vec![Default::default(); grid_width],
            west_walls: vec![Default::default(); grid_height],
        }
    }
}
impl<T> GridWalls<T> {
    pub fn iter(&self) -> GridWallsIterator<T> {
        GridWallsIterator {
            direction: Some(CompassDirection::North),
            grid_width: self.grid_width,
            wall_iters: [
                self.north_walls.iter().enumerate(),
                self.east_walls.iter().enumerate(),
                self.south_walls.iter().enumerate(),
                self.west_walls.iter().enumerate()
            ]
        }
    }
    pub fn iter_mut(&mut self) -> GridWallsIteratorMut<T> {
        GridWallsIteratorMut {
            direction: Some(CompassDirection::North),
            grid_width: self.grid_width,
            wall_iters: [
                self.north_walls.iter_mut().enumerate(),
                self.east_walls.iter_mut().enumerate(),
                self.south_walls.iter_mut().enumerate(),
                self.west_walls.iter_mut().enumerate()
            ]
        }
    }
}

impl<T> Index<WallAddress> for GridWalls<T> {
    type Output = T;

    fn index(&self, index: WallAddress) -> &Self::Output {
        let TileAddress { x, y } = index.tile;
        if x >= self.grid_width || y >= self.grid_height { panic!("WallAddress out of bounds for grid ({},{}) vs ({}, {})", x,y, self.grid_width, self.grid_height) }
        match index.direction {
            CompassDirection::North => &self.north_walls[x + y * self.grid_width],
            CompassDirection::East => &self.east_walls[x + y * self.grid_width],
            CompassDirection::South => &self.south_walls[x],
            CompassDirection::West => &self.west_walls[y]
        }
    }
}

impl<T> IndexMut<WallAddress> for GridWalls<T> {
    fn index_mut(&mut self, index: WallAddress) -> &mut Self::Output {
        let TileAddress { x, y } = index.tile;
        match index.direction {
            CompassDirection::North => {
                let i = x + y * self.grid_width;
                &mut self.north_walls[i]
            },
            CompassDirection::East => {
                let i = x + y * self.grid_width;
                &mut self.east_walls[i]
            },
            CompassDirection::South => {
                &mut self.south_walls[x]
            },
            CompassDirection::West => {
                &mut self.west_walls[y]
            }
        }
    }
}

impl <'a, T> IntoIterator for &'a GridWalls<T> {
    type Item = (WallAddress, &'a T);
    type IntoIter = GridWallsIterator<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

fn wall_addr_from_grid_index(index: usize, direction: CompassDirection, grid_width: usize) -> WallAddress {
    match direction {
        CompassDirection::North => {
            WallAddress::new(
                TileAddress {
                    x: index % grid_width,
                    y: index / grid_width,
                },
                CompassDirection::North
            )
        },
        CompassDirection::East => {
            WallAddress::new(
                TileAddress {
                    x: index % grid_width,
                    y: index / grid_width,
                },
                CompassDirection::East
            )
        },
        CompassDirection::South => {
            WallAddress::new(
                TileAddress {
                    x: index,
                    y: 0
                },
                CompassDirection::South
            )
        },
        CompassDirection::West => {
            WallAddress::new(
                TileAddress {
                    x: 0,
                    y: index,
                },
                CompassDirection::West
            )
        }
    }
}
fn next_direction(current: CompassDirection) -> Option<CompassDirection> {
    match current {
        CompassDirection::North => Some(CompassDirection::East),
        CompassDirection::East => Some(CompassDirection::South),
        CompassDirection::South => Some(CompassDirection::West),
        CompassDirection::West => None,
    }
}

pub struct GridWallsIterator<'a, T> {
    grid_width: usize,
    wall_iters: [Enumerate<Iter<'a, T>>; 4],
    direction: Option<CompassDirection>,
}
impl <'a, T> Iterator for GridWallsIterator<'a, T> {
    type Item = (WallAddress, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        self.direction.and_then(|d| {
            let inner_iter = match d {
                CompassDirection::North => &mut self.wall_iters[0],
                CompassDirection::East => &mut self.wall_iters[1],
                CompassDirection::South => &mut self.wall_iters[2],
                CompassDirection::West => &mut self.wall_iters[3]
            };
            inner_iter.next()
                .map(|(index, t)| {
                    (wall_addr_from_grid_index(index, d, self.grid_width), t)
                })
                .or_else(|| {
                    self.direction = next_direction(d);
                    self.next()
                })
        })
    }
}

pub struct GridWallsIteratorMut<'a, T> {
    grid_width: usize,
    wall_iters: [Enumerate<IterMut<'a, T>>; 4],
    direction: Option<CompassDirection>,
}

impl <'a, T> Iterator for GridWallsIteratorMut<'a, T> {
    type Item = (WallAddress, &'a mut T);

    fn next(&mut self) -> Option<Self::Item> {
        self.direction.and_then(|d| {
            let inner_iter = match d {
                CompassDirection::North => &mut self.wall_iters[0],
                CompassDirection::East => &mut self.wall_iters[1],
                CompassDirection::South => &mut self.wall_iters[2],
                CompassDirection::West => &mut self.wall_iters[3]
            };
            inner_iter.next()
                .map(|(index, t)| {
                    (wall_addr_from_grid_index(index, d, self.grid_width), t)
                })
                .or_else(|| {
                    self.direction = next_direction(d);
                    self.next()
                })
        })
    }
}