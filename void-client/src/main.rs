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

use std::io::Cursor;

use light::Light;
use proto::{ActiveRequest, BrightnessRequest, ColorRequest, StatusRequest};

use anyhow::{anyhow, Result};
use log::debug;
use serde::Serialize;
use tiny_http::{Header, Method, Response, Server, StatusCode};

fn main() -> Result<()> {
    pretty_env_logger::init();

    let server = Server::http("0.0.0.0:8000").map_err(|e| anyhow!(e))?;

    let mut light = Light::new();

    for mut request in server.incoming_requests() {
        let response = match request.method() {
            Method::Get => match request.url() {
                "/" => {
                    debug!("Current status requested");

                    let response = StatusRequest {
                        active: light.get_active(),
                        brightness: light.get_brightness(),
                        color: light.get_color(),
                    };

                    Some(response_json(response)?)
                }
                "/active" => {
                    debug!("Current power requested");

                    let response = ActiveRequest {
                        value: light.get_active(),
                    };

                    Some(response_json(response)?)
                }
                "/brightness" => {
                    debug!("Current brightness requested");

                    let response = BrightnessRequest {
                        value: light.get_brightness(),
                    };

                    Some(response_json(response)?)
                }
                "/color" => {
                    debug!("Current color requested");

                    let response = ColorRequest {
                        value: light.get_color(),
                    };

                    Some(response_json(response)?)
                }
                _ => None,
            },
            Method::Put => {
                let mut body = String::new();
                request.as_reader().read_to_string(&mut body)?;

                match request.url() {
                    "/active" => {
                        let body: ActiveRequest = serde_json::from_str(&body)?;

                        debug!(
                            "Power change requested: {}",
                            if body.value { "on" } else { "off" }
                        );

                        light.set_active(body.value);

                        let response = ActiveRequest {
                            value: light.get_active(),
                        };

                        Some(response_json(response)?)
                    }
                    "/brightness" => {
                        let body: BrightnessRequest = serde_json::from_str(&body)?;

                        debug!("Brightness change requested: {}", body.value);

                        light.set_brightness(body.value);

                        let response = BrightnessRequest {
                            value: light.get_brightness(),
                        };

                        Some(response_json(response)?)
                    }
                    "/color" => {
                        let body: ColorRequest = serde_json::from_str(&body)?;

                        debug!("Color change requested: {}", body.value);

                        light.set_color(body.value);

                        let response = ColorRequest {
                            value: light.get_color(),
                        };

                        Some(response_json(response)?)
                    }
                    _ => None,
                }
            }
            _ => None,
        };

        let response =
            response.unwrap_or_else(|| Response::from_string("Not found").with_status_code(404));
        request.respond(response)?;
    }

    Ok(())
}

fn response_json<J>(json: J) -> Result<Response<Cursor<Vec<u8>>>>
where
    J: Serialize,
{
    let json = serde_json::to_vec(&json)?;
    let json_len = json.len();

    Ok(Response::new(
        StatusCode(200),
        vec![Header::from_bytes(
            &b"Content-Type"[..],
            &b"application/json; charset=UTF-8"[..],
        )
        .unwrap()],
        Cursor::new(json),
        Some(json_len),
        None,
    ))
}
