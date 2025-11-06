module Engine.Core where

import Engine.Types
import qualified Data.Map as Map



processCommand :: Command -> GameState -> (String, GameState)
processCommand (Ir dir) state =
  case Map.lookup dir exits of
    -- si la direccion es valida, mueve al jugador a la nueva habitacion
    Just nextRoomName ->
      let newState = state { currentRoom = nextRoomName }
          nextRoom = worldRooms state Map.! nextRoomName
      in (roomDesc nextRoom, newState)
    -- si no hay salida en esa direccion, no cambia el estado
    Nothing -> ("No puedes ir en esa dirección.", state)
  where
    -- obtiene la habitacion actual y sus salidas
    current = worldRooms state Map.! currentRoom state
    exits = roomExits current



-- muestra la descripcion de la habitacion actual
processCommand Mirar state =
  let room = worldRooms state Map.! currentRoom state
  in (roomDesc room, state)


-- intenta tomar un objeto de la habitacion actual
processCommand (Tomar itemName) state =
  let currentRoomName = currentRoom state
      room = worldRooms state Map.! currentRoomName
  in if itemName `elem` roomItems room
       then
         case Map.lookup itemName (worldItems state) of
           -- si el item existe en la base de datos, lo agrega al inventario y lo quita de la habitacion
           Just item ->
             let newInventory = Map.insert itemName item (inventory state)
                 newRoom = room { roomItems = filter (/= itemName) (roomItems room) }
                 newWorld = Map.insert currentRoomName newRoom (worldRooms state)
                 newState = state { inventory = newInventory, worldRooms = newWorld }
             in ("Has tomado: " ++ itemName, newState)
         
           Nothing -> ("Error interno: ítem no encontrado en base de datos.", state)
       else ("No hay ningún '" ++ itemName ++ "' aquí.", state)


-- muestra el contenido del inventario del jugador
processCommand Inventario state =
  let inv = inventory state
  in if Map.null inv
       then ("Tu inventario está vacío.", state)
       else ("Inventario:\n" ++ unlines (map ((" - " ++) . itemName) (Map.elems inv)), state)


processCommand Salir state = ("¡Gracias por jugar!", state)