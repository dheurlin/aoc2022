{-# LANGUAGE TupleSections, LambdaCase #-}

import Prelude hiding ( FilePath )
import Data.Map ( Map, (!))
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe, mapMaybe )

data Entry = Dir String | File Integer String
  deriving ( Eq, Show )

parseEntry :: String -> Entry
parseEntry s = case words s of
  ["dir", name] -> Dir name
  [num, name]   -> File (read num) name
  _             -> error $ "Invalid file: " <> s

type FilePath = [String]

type FileLocation = (FilePath, Entry)

type St = (FilePath, [FileLocation])

parseFiles' :: St -> [String] -> St
parseFiles' st [] = st
parseFiles' st@(currPath, entries) ss = case head ss of
   "$ ls" -> parseLs st (tail ss)
   "$ cd .." -> parseFiles' (tail currPath, entries) (tail ss)
   hd -> case words hd of
      ["$", "cd", name] -> parseFiles' (name : currPath, entries) (tail ss)
      _                 -> error $ "Invalid file structure at: " <> hd

parseLs :: St -> [String] -> St
parseLs (currPath, entries) ss = (currPath, snd $ parseFiles' newSt rest)
  where
    lines = takeWhile ((/= '$') . head) ss
    rest = drop (length lines) ss
    parsed = map ((currPath ,) . parseEntry) lines
    newSt = (currPath, parsed <> entries)

-- Returns a list of files together with their filepath
parseFiles :: String -> [FileLocation]
parseFiles input = snd $ parseFiles' ([], []) (lines input)

-- A datatype for a file system in a tree structure
data FSNode = FSDir String (Map String FSNode) | FSFile Integer String
  deriving ( Eq )

toFsNode :: Entry -> FSNode
toFsNode (File size name) = FSFile size name
toFsNode (Dir name) = FSDir name Map.empty

instance Show FSNode where
  show = showFsNode 1

showFsNode n (FSFile sz name) = "- " <> name <> " (file, size=" <> show sz <> ")\n"
showFsNode n (FSDir name children) = "- " <> name <> " (dir)\n" <> showChildren
    where
      indent = concat $ replicate n "  "
      showChildren = concatMap ((indent <>) . showFsNode (n + 1) . snd)
        $ Map.toList children

fsName :: FSNode -> String
fsName (FSDir name _) = name
fsName (FSFile _ name) = name

-- Given an existing file structure, inserts a FileLocation at an appropriate location
insertFsNode :: FSNode -> FileLocation -> FSNode
insertFsNode fs (reversePath, file) = go fs (tail $ reverse reversePath) (toFsNode file)
  where
    go :: FSNode -> [String] -> FSNode -> FSNode
    go (FSDir name children) [] node = FSDir name (Map.insert (fsName node) node children)
    go (FSDir name children) (dir:dirs) node = FSDir name (Map.insert dir newChild children)
      where
        subDir = fromMaybe (FSDir dir Map.empty) (Map.lookup dir children)
        newChild = go subDir dirs node
    go _ _ _ = error "kebab"

-- Creates a hierarchical file structure from a list of files + filepaths
makeFS :: [FileLocation] -> FSNode
makeFS = foldr (flip insertFsNode) (FSDir "/" Map.empty)

parseFS :: String -> FSNode
parseFS = makeFS . parseFiles

-- A hierarchical file system with each node being tagged with a size
data SizedFS =
  SizedDir String Integer (Map String SizedFS) |
  SizedFile Integer String

sizedSize :: SizedFS -> Integer
sizedSize (SizedFile s _) = s
sizedSize (SizedDir _ size _) = size

toSizedFS :: FSNode -> SizedFS
toSizedFS (FSFile size name) = SizedFile size name
toSizedFS (FSDir name children) = SizedDir name s children'
  where
    children' = Map.map toSizedFS children
    s         = Map.foldl (\a b -> a + sizedSize b) 0 children'

flattenSized :: SizedFS -> [(String, String, Integer)]
flattenSized (SizedFile size name) = [("f", name, size)]
flattenSized (SizedDir name size children) = ("d", name, size) : childrenSizes
  where
    childrenSizes = concatMap snd . Map.toList $ Map.map flattenSized children

getDirs :: [(String, String, Integer)] -> [Integer]
getDirs = mapMaybe (\case ("f", _, _) -> Nothing; (d, s, n) -> Just n)

star1 :: String -> Integer
star1 = sum . filter (<= 100000) . getDirs . flattenSized . toSizedFS . parseFS

star2 :: String -> Integer
star2 input = minimum . filter (>= spaceNeeded) . getDirs $ parsed
  where
    parsed = flattenSized . toSizedFS . parseFS $ input
    spaceUsed = sum $ mapMaybe (\case (_, "/", size) -> Just size; _ -> Nothing) parsed
    spaceNeeded = 30000000 - (70000000 - spaceUsed)

main = do
  input <- readFile "input.txt"
  putStrLn $ "Star1: " <> show (star1 input)
  putStrLn $ "Star2: " <> show (star2 input)
