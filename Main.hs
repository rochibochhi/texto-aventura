module Main where

import Engine.Types
import Engine.Parser
import Engine.Core
import Engine.Persistence
import qualified Data.Map as Map
import System.IO (stdout, hFlush)
import System.Exit (exitSuccess)


main :: IO ()
main = do

  result <- loadWorldData "mundo.txt"
  case result of
    -- si hay un error al cargar, muestra mensaje y termina
    Left err -> do
      putStrLn $ "Error al cargar el mundo: " ++ err
      exitSuccess
    -- si se carga bien, crea el estado inicial y empieza el loop
    Right (rooms, items) -> do
        let initialState = GameState
              { currentRoom = "Celda de Piedra",
                inventory   = Map.empty,
                worldRooms  = rooms,
                worldItems  = items
              }
        gameLoop initialState



-- bucle principal del juego: muestra prompt, lee comando, lo procesa y repite
gameLoop :: GameState -> IO ()
gameLoop state = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parseCommand input of
    -- comando invalido
    Nothing -> do
      putStrLn "Comando no reconocido. Usa: ir, mirar, tomar, inventario, salir."
      gameLoop state
    -- comando 'salir'
    Just Salir -> do
      putStrLn "¡Gracias por jugar!"
      exitSuccess
    -- cualquier otro comando valido
    Just cmd -> do
      let (msg, newState) = processCommand cmd state
      putStrLn msg
      gameLoop newState