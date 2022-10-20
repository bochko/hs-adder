module PrefixTree where

import qualified Data.Map as Map

data PrefixTree = PrefixTree
  { nodes :: Map.Map Char PrefixTree,
    eow :: Bool
  }
  deriving (Show)

data PrefixMatch = PrefixMatch
  { offset :: Int,
    ptree :: PrefixTree
  }
  deriving (Show)

empty :: PrefixTree
empty = PrefixTree Map.empty False

fromList :: [String] -> PrefixTree
fromList [] = empty
fromList xs = foldr insert empty xs

insert :: [Char] -> PrefixTree -> PrefixTree
insert [] (PrefixTree nodes _) = PrefixTree nodes True
insert (ch : chrest) (PrefixTree nodes eow) = case Map.lookup ch nodes of
  Just ptree -> PrefixTree (Map.insert ch (insert chrest ptree) nodes) eow
  Nothing -> PrefixTree (Map.insert ch (insert chrest (empty :: PrefixTree)) nodes) eow

combine :: PrefixMatch -> Maybe PrefixMatch -> PrefixMatch
combine pml@(PrefixMatch _ _) Nothing = pml
combine (PrefixMatch loffset lptree) (Just (PrefixMatch roffset rptree)) =
  PrefixMatch {offset = loffset + roffset, ptree = rptree}

(//>) = combine

match :: [Char] -> PrefixTree -> Maybe PrefixMatch
match [] _ = Nothing
match chars@(ch : chrest) ptree@(PrefixTree nodes eow) = match' chars ptree 0
  where
    match' [] _ _ = Nothing
    match' (ch : chrest) (PrefixTree nodes eow) offset = case Map.lookup ch nodes of
      Just ptree@(PrefixTree _ eow)
        | eow -> Just (PrefixMatch (offset + 1) ptree)
        | otherwise -> match' chrest ptree (offset + 1)
      Nothing -> Nothing

matchgreedy :: [Char] -> PrefixTree -> Maybe PrefixMatch
matchgreedy [] _ = Nothing
matchgreedy chars ptree = matchgreedy' chars ptree Nothing
  where
    matchgreedy' [] _ _ = Nothing
    matchgreedy' chars ptree pm = case match chars ptree of
      Just pm@(PrefixMatch offset ptree) -> Just $ pm //> 
        matchgreedy' (drop offset chars) ptree (Just pm)
      Nothing -> Nothing