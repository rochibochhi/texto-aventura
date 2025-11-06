module Engine.Parser where

import Engine.Types
import Data.Char (toLower, isSpace)


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
  

-- convierte una cadena de entrada en un comando del juego
parseCommand :: String -> Maybe Command
parseCommand input = case words (map toLower (trim input)) of
  ("ir":dir:_)       -> parseDirection dir >>= Just . Ir
  ["mirar"]          -> Just Mirar
  ("tomar":rest)     -> if null rest then Nothing else Just $ Tomar (unwords rest)
  ("coger":rest)     -> if null rest then Nothing else Just $ Tomar (unwords rest)
  ["inventario"]     -> Just Inventario
  ["inv"]            -> Just Inventario
  ["salir"]          -> Just Salir
  _                  -> Nothing


-- convierte una cadena en una direccion valida
parseDirection :: String -> Maybe Direction
parseDirection "norte" = Just Norte
parseDirection "sur"   = Just Sur
parseDirection "este"  = Just Este
parseDirection "oeste" = Just Oeste
parseDirection _       = Nothing