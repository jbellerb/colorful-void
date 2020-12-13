/*
 * Copyright (C) 2020  Jared Beller
 * This file is part of void-client
 *
 * void-client is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

use std::fmt;

use log::trace;

#[derive(Clone, Default, Debug)]
pub struct RGB {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl From<[u8; 4]> for RGB {
    fn from(array: [u8; 4]) -> Self {
        RGB {
            r: array[0],
            g: array[1],
            b: array[2],
        }
    }
}

impl fmt::Display for RGB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02X}{:02X}{:02X}", self.r, self.g, self.b)
    }
}

#[derive(Debug)]
pub struct Light {
    array: usize,
    active: bool,
    brightness: u8,
    color: RGB,
}

impl Light {
    pub fn new() -> Light {
        let array = 0; // Placeholder for ws8212b driver

        Light {
            array,
            active: false,
            brightness: 255,
            color: RGB::default(),
        }
    }

    pub fn set_active(&mut self, active: bool) {
        trace!(
            "Setting light state to {}",
            if active { "on" } else { "off" }
        );

        self.active = active;
    }

    pub fn get_active(&self) -> bool {
        self.active
    }

    pub fn set_brightness(&mut self, brightness: u8) {
        trace!("Setting light to {} brightness", brightness);

        self.brightness = brightness;
    }

    pub fn get_brightness(&self) -> u8 {
        self.brightness
    }

    pub fn set_color(&mut self, color: RGB) {
        trace!("Setting light to color {}", color);

        self.color = color;
    }

    pub fn get_color(&self) -> RGB {
        self.color.clone()
    }
}
