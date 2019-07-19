{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
module Database.PostgreSQL.LTree.Talk.Notes where

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

-- Do this last
renderName :: LTree -> String
renderName path =
  case LTree.toList path of
    [] -> error "Unexpected empty ltree"
    xs -> Text.unpack $ LTree.unLabel $ last xs

fetchPaths :: PG.Connection -> IO [LTree]
fetchPaths c =
  fmap PG.fromOnly <$>
    PG.query_ c [PG.sql|
      values ('CEO' :: ltree)
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


-- We need to create a Tree from our List of LTrees.
--
-- Each LTree in the List will be considered a node, so
-- we need to build the tree accordingly.
--
-- Once we add a node to our tree, we're done with it and
-- should ignore it going forward.

pathsToTree :: [LTree] -> Tree LTree
pathsToTree = State.evalState go
  where

  -- How can we consume our list, omitting paths we've already
  -- inserted into our tree?
  --
  -- Let's use State since it's easy.

  go :: State [LTree] (Tree LTree)
  go = do

    -- Let's look at the definition of Data.Tree.Tree
    --
    -- data Tree a = Node { rootLabel :: a, subForest :: [Tree a] }
    --
    -- So the first thing we need to do is obtain the root node.

    root <- takeRoot

    -- GOTO takeRoot

    -- RETURN from takeRoot

    -- Now let's get the children for the root

    -- State.state

    -- Now we need to build the Tree starting at the root
    -- Let's call our new function 'takeSubtree'

    takeSubtree root

    -- GOTO takeSubtree


  -- Let's find the root node

  takeRoot :: State [LTree] LTree
  takeRoot = do

    -- (Note: First show it with State.get, then State.gets,
    -- then finish with State.state)

    -- paths <- State.get
    -- let (roots, rest) = partition ((== 1) . LTree.numLabels) paths

    -- There should be only one!

    -- case roots of
    --   [r] -> r
    --   []  -> error "SQL error: root not found"
    --   _   -> error "SQL error: multiple roots found"

    -- Solution with State.state

    State.state (partition ((== 1) . LTree.numLabels)) <&> \case
      [r] -> r
      []  -> error "SQL error: root not found"
      _   -> error "SQL error: multiple roots found"

    -- GOTO go

  takeSubtree :: LTree -> State [LTree] (Tree LTree)
  takeSubtree parent = do
    Tree.unfoldTreeM (\path -> (path,) <$> takeChildren path) parent

  takeChildren :: LTree -> State [LTree] [LTree]
  takeChildren parent = do
    children <- State.state $ partition (parent `LTree.isImmediateParentOf`)
    pure children
