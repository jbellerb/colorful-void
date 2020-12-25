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

use crate::proto::{Brightness, RGB};

use log::trace;
use rs_ws281x::{ChannelBuilder, Controller, ControllerBuilder, StripType};

#[derive(Debug)]
pub struct Light {
    controller: Controller,
    active: bool,
    brightness: Brightness,
    color: RGB,
}

impl Light {
    pub fn new() -> Light {
        let mut controller = ControllerBuilder::new()
            .freq(800000)
            .dma(10)
            .channel(
                0,
                ChannelBuilder::new()
                    .pin(18)
                    .count(75)
                    .strip_type(StripType::Ws2812)
                    .brightness(0)
                    .build(),
            )
            .build()
            .expect("[Fatal] Failed to initialize light strip");

        let color = RGB::default();
        for led in controller.leds_mut(0) {
            *led = color.clone().into();
        }
        controller.render();

        Light {
            controller,
            active: false,
            brightness: Brightness::default(),
            color,
        }
    }

    pub fn set_active(&mut self, active: bool) {
        trace!(
            "Setting light power to {}",
            if active { "on" } else { "off" }
        );

        self.active = active;
        let true_brightness = if active {
            self.brightness.clone().into()
        } else {
            0
        };
        self.controller.set_brightness(0, true_brightness);
        self.controller.render();
    }

    pub fn get_active(&self) -> bool {
        self.active
    }

    pub fn set_brightness(&mut self, brightness: Brightness) {
        trace!("Setting light to {} brightness", brightness);

        self.brightness = brightness.clone();
        self.controller.set_brightness(0, brightness.into());
        self.controller.render();
    }

    pub fn get_brightness(&self) -> Brightness {
        self.brightness.clone()
    }

    pub fn set_color(&mut self, color: RGB) {
        trace!("Setting light to color {}", color);

        self.color = color.clone();
        for led in self.controller.leds_mut(0) {
            *led = color.clone().into();
        }
        self.controller.render();
    }

    pub fn get_color(&self) -> RGB {
        self.color.clone()
    }
}
