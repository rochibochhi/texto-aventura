module Engine.Persistence where

import Engine.Types
import qualified Data.Map as Map
import Data.List (isPrefixOf, stripPrefix)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import System.IO (openFile, hSetEncoding, utf8, hGetContents, IOMode(ReadMode))


type ParseResult = (Map.Map RoomName Room, Map.Map ItemName Item)


-- carga el archivo del mundo desde la ruta dada
loadWorldData :: FilePath -> IO (Either String ParseResult)
loadWorldData path = do
  handle <- openFile path ReadMode
  hSetEncoding handle utf8
  content <- hGetContents handle
  return $ parseWorldFile content


-- divide el contenido en bloques y los parsea
parseWorldFile :: String -> Either String ParseResult
parseWorldFile content = do
  blocks <- splitIntoBlocks content
  (itemsMap, roomsMap) <- parseBlocks blocks Map.empty Map.empty
  return (roomsMap, itemsMap)


-- separa el contenido del archivo en bloques usando '---' como delimitador
splitIntoBlocks :: String -> Either String [String]
splitIntoBlocks content = Right $ map unlines $ filter (not . null) $ splitWhen (== "---") (lines content)
  where
    splitWhen :: Eq a => (a -> Bool) -> [a] -> [[a]]
    splitWhen _ [] = []
    splitWhen p xs = case break p xs of
      (ys, [])     -> [ys]
      (ys, _:zs)   -> ys : splitWhen p zs


-- procesa cada bloque como item o sala y construye los mapas correspondientes
parseBlocks :: [String] -> Map.Map ItemName Item -> Map.Map RoomName Room -> Either String (Map.Map ItemName Item, Map.Map RoomName Room)
parseBlocks [] items rooms = Right (items, rooms)
parseBlocks (block:rest) items rooms =
  case lines block of
    (header:_) | "ITEM:" `isPrefixOf` header -> do
      item <- parseItemBlock (lines block)
      let newItems = Map.insert (itemName item) item items
      parseBlocks rest newItems rooms
    (header:_) | "SALA:" `isPrefixOf` header -> do
      room <- parseRoomBlock (lines block) items
      let newRooms = Map.insert (roomName room) room rooms
      parseBlocks rest items newRooms
    _ -> Left "Bloque inválido: debe comenzar con ITEM: o SALA:"


-- parsea un bloque de item y crea un valor Item
parseItemBlock :: [String] -> Either String Item
parseItemBlock ls = do
  nameLine <- findLineWithPrefix "ITEM:" ls
  descLine <- findLineWithPrefix "DESC:" ls
  let name = trim $ dropPrefix "ITEM:" nameLine
      desc = trim $ dropPrefix "DESC:" descLine
  if null name
    then Left "Nombre de ítem vacío"
    else Right $ Item name desc


-- parsea un bloque de sala y crea un valor Room
parseRoomBlock :: [String] -> Map.Map ItemName Item -> Either String Room
parseRoomBlock ls allItems = do
  nameLine <- findLineWithPrefix "SALA:" ls
  descLine <- findLineWithPrefix "DESC:" ls
  let name = trim $ dropPrefix "SALA:" nameLine
      desc = trim $ dropPrefix "DESC:" descLine
  exits <- mapM parseExitLine [l | l <- ls, "SALIDA:" `isPrefixOf` l]
  objNames <- mapM (parseObjLine allItems) [l | l <- ls, "OBJETO:" `isPrefixOf` l]
  if null name
    then Left "Nombre de sala vacío"
    else Right $ Room name desc (Map.fromList exits) objNames


-- divide una cadena en dos partes usando '->' como separador
splitOnArrow :: String -> Maybe (String, String)
splitOnArrow s =
  case break (== '-') s of
    (before, '-' : '>' : after) -> Just (before, after)
    _ -> Nothing


-- parsea una línea de salida y devuelve una direccion y el nombre de la sala destino
parseExitLine :: String -> Either String (Direction, RoomName)
parseExitLine line = do
  let rest = dropPrefix "SALIDA:" line
  case splitOnArrow rest of
    Just (dirStr, target) -> do
      case parseDirectionSafe (trim dirStr) of
        Just dir -> Right (dir, trim target)
        Nothing  -> Left $ "Dirección inválida en SALIDA: " ++ trim dirStr
    Nothing -> Left "Formato inválido en SALIDA (esperado: SALIDA: <dir> -> <sala>)"


-- verifica que un objeto mencionado en una sala exista en la lista de items
parseObjLine :: Map.Map ItemName Item -> String -> Either String ItemName
parseObjLine allItems line = do
  let objName = trim $ dropPrefix "OBJETO:" line
  if Map.member objName allItems
    then Right objName
    else Left $ "Ítem '" ++ objName ++ "' no definido"


-- busca una línea que empiece con un prefijo dado; falla si hay cero o más de una
findLineWithPrefix :: String -> [String] -> Either String String
findLineWithPrefix prefix lines' =
  case filter (isPrefixOf prefix) lines' of
    [l] -> Right l
    []  -> Left $ "Falta línea obligatoria: " ++ prefix
    _   -> Left $ "Múltiples líneas con prefijo: " ++ prefix



dropPrefix :: String -> String -> String
dropPrefix prefix str = fromMaybe str (stripPrefix prefix str)


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
  

-- convierte una cadena en una dirección 
parseDirectionSafe :: String -> Maybe Direction
parseDirectionSafe "Norte" = Just Norte
parseDirectionSafe "Sur"   = Just Sur
parseDirectionSafe "Este"  = Just Este
parseDirectionSafe "Oeste" = Just Oeste
parseDirectionSafe "norte" = Just Norte
parseDirectionSafe "sur"   = Just Sur
parseDirectionSafe "este"  = Just Este
parseDirectionSafe "oeste" = Just Oeste
parseDirectionSafe _       = Nothing