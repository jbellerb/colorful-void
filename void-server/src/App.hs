-- Copyright (C) 2020  Jared Beller
-- This file is part of void-server
--
-- void-server is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module App where

import Control.Monad.Trans.Reader (ReaderT)
import Network.HTTP.Client (Manager)
import Servant
import Servant.Auth.Server

data Env = Env
  { jwts :: JWTSettings
  , manager :: Manager
  }

type App = ReaderT Env Handler