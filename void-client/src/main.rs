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

mod light;
mod proto;

use light::Light;
use proto::{ActiveRequest, BrightnessRequest, ColorRequest};

use anyhow::{anyhow, Result};
use log::debug;
use tiny_http::{Method, Response, Server};

fn main() -> Result<()> {
    pretty_env_logger::init();

    let server = Server::http("0.0.0.0:8000").map_err(|e| anyhow!(e))?;

    let mut light = Light::new();

    for mut request in server.incoming_requests() {
        let response = match request.method() {
            Method::Get => match request.url() {
                "/active" => {
                    debug!("Current status requested");

                    let response = ActiveRequest {
                        value: light.get_active(),
                    };

                    Some(Response::from_string(serde_json::to_string(&response)?).boxed())
                }
                "/brightness" => {
                    debug!("Current brightness requested");

                    let response = BrightnessRequest {
                        value: light.get_brightness(),
                    };

                    Some(Response::from_string(serde_json::to_string(&response)?).boxed())
                }
                "/color" => {
                    debug!("Current color requested");

                    let response = ColorRequest {
                        value: light.get_color(),
                    };

                    Some(Response::from_string(serde_json::to_string(&response)?).boxed())
                }
                _ => None,
            },
            Method::Post => {
                let mut body = String::new();
                request.as_reader().read_to_string(&mut body)?;

                match request.url() {
                    "/active" => {
                        let body: ActiveRequest = serde_json::from_str(&body)?;

                        debug!(
                            "Status change requested: {}",
                            if body.value { "on" } else { "off" }
                        );

                        light.set_active(body.value);
                        Some(Response::empty(200).boxed())
                    }
                    "/brightness" => {
                        let body: BrightnessRequest = serde_json::from_str(&body)?;

                        debug!("Brightness change requested: {}", body.value);

                        light.set_brightness(body.value);
                        Some(Response::empty(200).boxed())
                    }
                    "/color" => {
                        let body: ColorRequest = serde_json::from_str(&body)?;

                        debug!("Color change requested: {}", body.value);

                        light.set_color(body.value);
                        Some(Response::empty(200).boxed())
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        let response = response.unwrap_or_else(|| {
            Response::from_string("Not found")
                .with_status_code(404)
                .boxed()
        });
        request.respond(response)?;
    }

    Ok(())
}
