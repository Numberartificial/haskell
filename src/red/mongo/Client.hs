{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Red.Mongo.Client (main) where

import Database.MongoDB   (Host(..), PortID(..), Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find,
                            host, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)
import System.Cron.Schedule
import Database.Redis hiding (connect, select, sort)
import Control.Applicative
import Data.Functor

main :: IO ()
main = do
    pipe <- connect (Host "127.0.0.1" $ PortNumber 8888)
    e <- access pipe master "baseball" run
    close pipe
    print e

run :: Action IO ()
run = do
    clearTeams
    insertTeams
    allTeams >>= printDocs "All Teams"
    nationalLeagueTeams >>= printDocs "National League Teams"
    newYorkTeams >>= printDocs "New York Teams"

clearTeams :: Action IO ()
clearTeams = delete (select [] "team")

insertTeams :: Action IO [Value]
insertTeams = insertMany "team" [
    ["name" =: "Yankees", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "American", "age" =: 1],
    ["name" =: "Mets", "home" =: ["city" =: "New York", "state" =: "NY"], "league" =: "National"],
    ["name" =: "Phillies", "home" =: ["city" =: "Philadelphia", "state" =: "PA"], "league" =: "National"],
    ["name" =: "Red Sox", "home" =: ["city" =: "Boston", "state" =: "MA"], "league" =: "American"] ]

allTeams :: Action IO [Document]
allTeams = rest =<< find (select [] "team") {sort = ["home.city" =: 1]}

nationalLeagueTeams :: Action IO [Document]
nationalLeagueTeams = rest =<< find (select ["league" =: "National"] "team")

newYorkTeams :: Action IO [Document]
newYorkTeams = rest =<< find (select ["home.state" =: "NY"] "team") {project = ["name" =: 1, "league" =: 1]}

printDocs :: String -> [Document] -> Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude ["_id"]) docs

test :: IO ()
test = do
       tids <- execSchedule $ do
           addJob job1 "* * * * *"
           addJob job2 "0 * * * *"
       print tids

job1 :: IO ()
job1 = putStrLn "job1"

job2 :: IO ()
job2 = putStrLn "job2"

redisJob :: IO ()
redisJob = do
  conn <- checkedConnect defaultConnectInfo
  runRedis conn $ do
     set "hello" "hello"
     set "world" "world"
     hello <- get "hello"
     world <- get "world"
     liftIO $ print (hello,world)

