{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Database.PostgreSQL.LTree.Talk where

import Prelude
import Data.Functor
import Data.Traversable

import Control.Monad.State (State)
import Data.List (partition)
import Data.Text (Text)
import Data.Tree (Tree)
import qualified Control.Monad.State as State
import qualified Data.Text as Text
import qualified Data.Tree as Tree
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.SqlQQ as PG

import Database.PostgreSQL.LTree (LTree)
import qualified Database.PostgreSQL.LTree as LTree

main :: IO ()
main = do
  c <- PG.connectPostgreSQL "dbname=ltree_talk host=localhost"
  paths <- fetchPaths c
  let tree = pathsToTree paths
  putStrLn $ Tree.drawTree $ renderName <$> tree

renderName :: LTree -> String
renderName path =
  case LTree.toList path of
    [] -> error "Unexpected empty ltree"
    xs -> Text.unpack $ LTree.unLabel $ last xs

fetchPaths :: PG.Connection -> IO [LTree]
fetchPaths c =
  fmap PG.fromOnly <$>
    PG.query_ c [PG.sql|
      values ('CEO'::ltree)
           , ('CEO.Admin_Assistant')
           , ('CEO.COO')
           , ('CEO.COO.VP_Customer_Service')
           , ('CEO.COO.Operations_Manager')
           , ('CEO.COO.VP_of_Engineering')
           , ('CEO.COO.VP_of_Engineering.Engineering_Team')
           , ('CEO.COO.VP_of_Marketing')
           , ('CEO.COO.VP_of_Marketing.Research_Assistant')
           , ('CEO.COO.VP_of_Marketing.Research_Assistant.Marketing_Coordinator')
           , ('CEO.COO.VP_of_Sales')
           , ('CEO.COO.VP_of_Sales.Sales_Team')
           , ('CEO.COO.VP_of_Production')
           , ('CEO.COO.VP_of_Production.Production_Team')
           , ('CEO.COO.HR_Manager')
    |]

pathsToTree :: [LTree] -> Tree LTree
pathsToTree = State.evalState go
  where
  go :: State [LTree] (Tree LTree)
  go = takeSubtree =<< takeRoot

  takeRoot :: State [LTree] LTree
  takeRoot = do
    State.state (partition ((== 1) . LTree.numLabels)) <&> \case
      [r] -> r
      []  -> error "SQL error: root not found"
      _   -> error "SQL error: multiple roots found"

  takeSubtree :: LTree -> State [LTree] (Tree LTree)
  takeSubtree parent = do
    Tree.unfoldTreeM (\path -> (path,) <$> takeChildren path) parent

  takeChildren :: LTree -> State [LTree] [LTree]
  takeChildren parent = do
    children <- State.state $ partition (parent `LTree.isImmediateParentOf`)
    pure children
