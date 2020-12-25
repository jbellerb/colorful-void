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

use serde::{de::Deserializer, ser::Serializer, Deserialize, Serialize};

#[derive(Clone, Debug)]
pub struct RGB {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl From<RGB> for [u8; 4] {
    fn from(val: RGB) -> Self {
        [val.b, val.g, val.r, 0]
    }
}

impl Default for RGB {
    fn default() -> Self {
        RGB {
            r: 255,
            g: 255,
            b: 255,
        }
    }
}

impl fmt::Display for RGB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{:02X}{:02X}{:02X}", self.r, self.g, self.b)
    }
}

impl<'de> Deserialize<'de> for RGB {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let val: u32 = Deserialize::deserialize(deserializer)?;
        let val = val.to_le_bytes();

        Ok(RGB {
            r: val[2],
            g: val[1],
            b: val[0],
        })
    }
}

impl Serialize for RGB {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let num = ((self.r as u32) << 16) + ((self.g as u32) << 8) + (self.b as u32);

        serializer.serialize_u32(num)
    }
}
