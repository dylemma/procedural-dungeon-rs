use std::convert::TryFrom;
use std::ops::{Index, IndexMut, Add};
use std::cmp::Eq;
use std::slice::Iter;
use std::iter::Enumerate;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct TileAddress {
    pub x: usize,
    pub y: usize,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub enum CompassDirection {
    North,
    East,
    South,
    West,
}

impl Add<CompassDirection> for TileAddress {
    type Output = TileAddress;

    fn add(self, rhs: CompassDirection) -> TileAddress {
        match rhs {
            CompassDirection::North => TileAddress { x: self.x, y: self.y + 1 },
            CompassDirection::East => TileAddress { x: self.x + 1, y: self.y },
            CompassDirection::South => TileAddress { x: self.x, y: self.y - 1 },
            CompassDirection::West => TileAddress { x: self.x - 1, y: self.y },
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

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
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

    pub fn neighbor(&self) -> TileAddress {
        self.tile + self.direction
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
    pub fn has_tile(&self, address: &TileAddress) -> bool {
        address.x < self.grid_width && address.y < self.grid_height
    }
}
impl <T> GridTiles<T> {
    pub fn tile_addresses(&self) -> GridTilesAddresses<T> {
        GridTilesAddresses {
            parent: self,
            i: 0,
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
    fn iter(&self) -> GridWallsIterator<T> {
        GridWallsIterator {
            parent: self,
            direction: Some(CompassDirection::North),
            inner: self.north_walls.iter().enumerate()
        }
    }
}

impl<T> Index<WallAddress> for GridWalls<T> {
    type Output = T;

    fn index(&self, index: WallAddress) -> &Self::Output {
        let TileAddress { x, y } = index.tile;
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
pub struct GridWallsIterator<'a, T> {
    parent: &'a GridWalls<T>,
    direction: Option<CompassDirection>,
    inner: Enumerate<Iter<'a, T>>
}
impl <'a, T> Iterator for GridWallsIterator<'a, T> {
    type Item = (WallAddress, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        match self.direction {
            None => None,
            Some(CompassDirection::North) => {
                self.inner.next()
                    .map(|(index, t)| {
                        let addr = WallAddress::new(
                            TileAddress {
                                x: index % self.parent.grid_width,
                                y: index / self.parent.grid_width,
                            },
                            CompassDirection::North
                        );
                        (addr, t)
                    })
                    .or_else(|| {
                        // if we ran out of north walls, move on to the east walls
                        self.direction = Some(CompassDirection::East);
                        self.inner = self.parent.east_walls.iter().enumerate();
                        self.next()
                    })
            },
            Some(CompassDirection::East) => {
                self.inner.next()
                    .map(|(index, t)| {
                        let addr = WallAddress::new(
                            TileAddress {
                                x: index % self.parent.grid_width,
                                y: index / self.parent.grid_width,
                            },
                            CompassDirection::East
                        );
                        (addr, t)
                    })
                    .or_else(|| {
                    // if we ran out of east walls, move on to the south walls
                        self.direction = Some(CompassDirection::South);
                        self.inner = self.parent.south_walls.iter().enumerate();
                        self.next()
                    })
            }
            Some(CompassDirection::South) => {
                self.inner.next()
                    .map(|(index, t)| {
                        let addr = WallAddress::new(
                            TileAddress {
                                x: index,
                                y: 0
                            },
                            CompassDirection::South
                        );
                        (addr, t)
                    })
                    .or_else(|| {
                        // if we ran out of south walls, move on to the west walls
                        self.direction = Some(CompassDirection::West);
                        self.inner = self.parent.west_walls.iter().enumerate();
                        self.next()
                    })
            },
            Some(CompassDirection::West) => {
                self.inner.next()
                    .map(|(index, t)| {
                        let addr = WallAddress::new(
                            TileAddress {
                                x: 0,
                                y: index,
                            },
                            CompassDirection::West
                        );
                        (addr, t)
                    })
                    .or_else(|| {
                        // if we ran out of west walls, we're done!
                        self.direction = None;
                        None
                    })
            }
        }
    }
}