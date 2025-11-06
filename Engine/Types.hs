module Engine.Types where

import qualified Data.Map as Map


type RoomName = String
type ItemName = String



data Direction = Norte | Sur | Este | Oeste
  deriving (Eq, Ord, Show, Read)


-- objeto que se puede tomar en el juego
data Item = Item
  { itemName :: ItemName,
    itemDesc :: String
  }
  deriving (Show, Eq)


-- sala del mundo del juego
data Room = Room
  { roomName :: RoomName,
    roomDesc :: String,
    roomExits :: Map.Map Direction RoomName,  -- direccion -> nombre de sala destino
    roomItems :: [ItemName]                   -- items presentes en la sala
  }
  deriving (Show, Eq)



data GameState = GameState
  { currentRoom :: RoomName,                -- nombre de la sala actual
    inventory   :: Map.Map ItemName Item,   -- items que el jugador ha recogido
    worldRooms  :: Map.Map RoomName Room,   -- todas las salas del mundo
    worldItems  :: Map.Map ItemName Item    -- todos los items definidos en el mundo
  }
  deriving (Show)


-- comandos que el jugador puede ingresar
data Command
  = Ir Direction        -- moverse en una direccion
  | Mirar               -- ver la descripcion de la sala actual
  | Tomar ItemName      -- recoger un objeto de la sala
  | Inventario          -- mostrar los objetos en el inventario
  | Salir               -- terminar el juego
  deriving (Show, Eq)