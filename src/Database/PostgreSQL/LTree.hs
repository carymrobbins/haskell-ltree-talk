{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.LTree
  ( LTree(..)
  , Label(unLabel)
  , fromList
  , toList
  , root
  , parent
  , numLabels
  , mkLabel
  , unsafeMkLabel
  , uuidToLabel
  , empty
  , singleton
  , snoc
  , isImmediateParentOf
  , parseUUIDFromLabel
  , allLabelsUnique
  ) where

import Prelude
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Sequence (Seq((:<|), (:|>)), (|>))
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.FromField (
  FromField(fromField),
  ResultError(Incompatible, UnexpectedNull),
  typename, returnError)
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID

-- | Wrapper for Postgres' "ltree" (label tree) type.
newtype LTree = LTree { unLTree :: Seq Label }
  deriving newtype (Show, Eq)

-- | Wrapper for a single label in an "ltree".
-- The constructor IS NOT exported to ensure we only construct valid
-- labels. See 'mkLabel' for constructing a 'Label'.
newtype Label = Label { unLabel :: Text }
  deriving newtype (Show, Eq, Ord)

fromList :: [Label] -> LTree
fromList = LTree . Seq.fromList

toList :: LTree -> [Label]
toList = Foldable.toList . unLTree

root :: LTree -> Maybe Label
root (LTree (x :<| _)) = Just x
root _ = Nothing

-- | Get the second-to-last 'Label' in an 'LTree'.
parent :: LTree -> Maybe Label
parent (LTree x) = Seq.lookup (Seq.length x - 2) x

numLabels :: LTree -> Int
numLabels (LTree x) = Seq.length x

-- | Safely construct a 'Label' from 'Text'. If the supplied 'Text'
-- contains characters unsupported by "ltree". On failure, returns 'Left'
-- with an error message.
mkLabel :: Text -> Either String Label
mkLabel t =
  if Text.null t then
    Left "ltree label must be non-empty"
  else if null invalidChars then
    Right $ Label t
  else
    Left $ "Invalid ltree label chars found: " <> show invalidChars
  where
  invalidChars = List.nub $ Text.unpack $ Text.filter (not . isValidLabelChar) t

-- | Same as 'mkLabel' except throws an error for an invalid 'Text' input.
unsafeMkLabel :: Text -> Label
unsafeMkLabel = either error id . mkLabel

-- | A 'UUID' can always be converted into a 'Label' without error by
-- dropping the hyphens. The resulting 'Label' will only contain
-- numbers and lower-case alpha characters.
uuidToLabel :: UUID -> Label
uuidToLabel = Label . Text.filter (/= '-') . UUID.toText

-- | Predicate for which characters are supported by "ltree".
isValidLabelChar :: Char -> Bool
isValidLabelChar = flip Set.member valid
  where
  valid = mconcat
    [ Set.singleton '_'
    , Set.fromList ['0'..'9']
    , Set.fromList ['A'..'Z']
    , Set.fromList ['a'..'z']
    ]

-- | An empty 'LTree'.
empty :: LTree
empty = LTree mempty

singleton :: Label -> LTree
singleton = LTree . Seq.singleton

-- | Append a single 'Label' to the end of an 'LTree'; should be O(1)
-- since it's delegating to 'Data.Sequence.|>'
snoc :: LTree -> Label -> LTree
snoc (LTree xs) x = LTree (xs |> x)

-- | Render an "ltree" as it would appear in the database.
render :: LTree -> Text
render = Text.intercalate "." . coerce . toList

isImmediateParentOf :: LTree -> LTree -> Bool
isImmediateParentOf (LTree xs) (LTree (ys :|> _)) | xs == ys = True
isImmediateParentOf _ _ = False

-- | Attempt to parse a 'UUID' from a 'Label'.
parseUUIDFromLabel :: Label -> Either String UUID
parseUUIDFromLabel (Label t) =
  Atto.parseOnly p t
  where
  p = do
    a <- Atto.take 8
    b <- Atto.take 4
    c <- Atto.take 4
    d <- Atto.take 4
    e <- Atto.take 12
    Atto.endOfInput
    maybe
      (fail "Label is not a valid UUID")
      pure
      (UUID.fromText $ Text.intercalate "-" [a, b, c, d, e])

allLabelsUnique :: LTree -> Bool
allLabelsUnique (LTree xs) = length xs == (Set.size . Set.fromList . Foldable.toList $ xs)

instance ToField LTree where
  toField = toField . render

instance FromField LTree where
  fromField fld mbs = do
    -- There might be a more efficient way to check this, need to see
    -- if the "ltree" type has a stable typoid or not.
    typ <- typename fld
    -- Ensure we don't accidentally deserialize a "text" field which
    -- would produce corrupted labels.
    when (typ /= "ltree") $
      returnError Incompatible fld $ "Expected type ltree, got: " <> show typ
    case mbs of
      Nothing -> returnError UnexpectedNull fld ""
      Just bs ->
        pure $ fromList $ coerce $ Text.splitOn "." $ Text.decodeUtf8 bs
