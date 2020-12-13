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

use crate::light::RGB;

use serde::{de::Deserializer, ser::Serializer, Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct ActiveRequest {
    pub value: bool,
}

fn deserialize_brightness<'de, D>(deserializer: D) -> Result<u8, D::Error>
where
    D: Deserializer<'de>,
{
    let val: u8 = Deserialize::deserialize(deserializer)?;

    Ok((((val as u32) * 255) / 100) as u8)
}

fn serialize_brightness<S>(val: &u8, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_u8((((*val as u32) * 100) / 255) as u8)
}

#[derive(Deserialize, Serialize)]
pub struct BrightnessRequest {
    #[serde(deserialize_with = "deserialize_brightness")]
    #[serde(serialize_with = "serialize_brightness")]
    pub value: u8,
}

fn deserialize_rgb<'de, D>(deserializer: D) -> Result<RGB, D::Error>
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

fn serialize_rgb<S>(val: &RGB, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let num = ((val.r as u32) << 16) + ((val.g as u32) << 8) + (val.b as u32);

    serializer.serialize_u32(num)
}

#[derive(Deserialize, Serialize)]
pub struct ColorRequest {
    #[serde(deserialize_with = "deserialize_rgb")]
    #[serde(serialize_with = "serialize_rgb")]
    pub value: RGB,
}
