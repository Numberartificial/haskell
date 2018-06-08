-- file: ch09/BetterPredicate.hs
module BetterPredicate
where

import Control.Monad (liftM, filterM, forM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Exception (IOException, bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Time.Clock.System (SystemTime, utcToSystemTime)
import Data.Ord
import Data.List

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory
        then getRecursiveContents path
      else return [path]
    return (concat paths)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return $ filter p names
-- the function we wrote earlier

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> SystemTime     -- last modified
               -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size (utcToSystemTime modified))

exceptionThen :: IOException -> IO (Maybe Integer)
exceptionThen e = return Nothing

getFileSize path = handle exceptionThen $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> SystemTime       -- last modified
             -> a

pathP :: InfoP FilePath
pathP p _ _ _ = p

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f k = \x y z s -> (f x y z s) `q` (k x y z s)

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)
greaterP = liftP (>)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f p _ _ _ = f p

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

(==?) = equalP
(&&?) = andP
(>?) = greaterP
infixl 3 >?

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)


data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe SystemTime
    } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo = undefined

traverseF :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseF order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && path /= infoPath info
      then traverseF order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [FilePath]
getUsefulContents path = do
        names <-  getDirectoryContents path
        return $ filter (not . (`elem` [".", ".."])) names

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

ioExceptionThen :: IOException -> IO (Maybe a)
ioExceptionThen e = return Nothing

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ioExceptionThen (Just `liftM` act)

-- 逆序遍历
traverseOrderByDesc = traverseF (sortBy $ flip compare)
-- 后序遍历, 先子，后根
traversePostOrder = traverseF (\(x:xs) -> xs ++ [x])

infoP :: InfoP Info
infoP x y z k = Info x (Just y) z (Just k)

traverseVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : usefulNames)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                             Nothing -> False
                             Just perms -> searchable perms
        recurse info = do
            if isDirectory info && infoPath info /= path
                then traverseVerbose order (infoPath info)
                else return [info]

data Iterate seed = Done {unwrap :: seed}
                   | Skip {unwrap :: seed}
                   | Continue {unwrap :: seed}
                   deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed
