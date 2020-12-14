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

mod brightness;
mod rgb;

pub use brightness::Brightness;
pub use rgb::RGB;

use serde::{Deserialize, Serialize};

#[derive(Deserialize, Serialize)]
pub struct StatusRequest {
    pub active: bool,
    pub brightness: Brightness,
    pub color: RGB,
}

#[derive(Deserialize, Serialize)]
pub struct ActiveRequest {
    pub value: bool,
}

#[derive(Deserialize, Serialize)]
pub struct BrightnessRequest {
    pub value: Brightness,
}

#[derive(Deserialize, Serialize)]
pub struct ColorRequest {
    pub value: RGB,
}
