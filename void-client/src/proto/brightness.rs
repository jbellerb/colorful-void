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
pub struct Brightness(u8);

impl From<u8> for Brightness {
    fn from(val: u8) -> Self {
        Brightness(val)
    }
}

impl Default for Brightness {
    fn default() -> Self {
        Brightness(255)
    }
}

impl fmt::Display for Brightness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}%", ((self.0 as u32) * 100) / 255)
    }
}

impl<'de> Deserialize<'de> for Brightness {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let val: u8 = Deserialize::deserialize(deserializer)?;

        Ok(Brightness((((val as u32) * 255) / 100) as u8))
    }
}

impl Serialize for Brightness {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_u8((((self.0 as u32) * 100) / 255) as u8)
    }
}
