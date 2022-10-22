module Tokenizer where

import Data.HashSet as HashSet (fromList, HashSet)
import Data.Hashable as Hashable (Hashable)
import PrefixTree (PrefixMatch (PrefixMatch), PrefixTree (PrefixTree), matchgreedy)

data TokenKind = Operator | Number | Symbol | Error deriving(Show)

allowedSymbolStartChars :: HashSet Char
allowedSymbolStartChars = fromList $ '_' : ['a' .. 'z'] ++ ['A' .. 'Z']

allowedNumberStartChars :: HashSet Char
allowedNumberStartChars = fromList $ '.' : ['0' .. '9']

allowedSymbolChars :: HashSet Char
allowedSymbolChars = fromList ['0' .. '9'] <> allowedSymbolStartChars

allowedNumberChars :: HashSet Char
allowedNumberChars = allowedNumberStartChars

countContiguousIn :: Hashable a => HashSet a -> [a] -> Int
countContiguousIn set list = countIn set list 0
  where
    countIn _ [] offset = offset
    countIn set (x:xs) offset 
      | x `elem` set = 1 + countIn set xs offset
      | otherwise = offset

tokenize :: String -> PrefixTree -> [(TokenKind, String)]
tokenize [] _ = []
tokenize (' ' : xs) ptree = tokenize xs ptree
tokenize input@(x : xs) ptree = case PrefixTree.matchgreedy input ptree of
  Just (PrefixMatch offset _) -> 
    (Operator, take offset input) : tokenize (drop offset input) ptree
  Nothing
    | x `elem` allowedNumberStartChars -> do
      let offset = countContiguousIn allowedNumberChars input
      (Number, take offset input) : tokenize (drop offset input) ptree
    | x `elem` allowedSymbolStartChars -> do
      let offset = countContiguousIn allowedSymbolChars input
      (Symbol, take offset input) : tokenize (drop offset input) ptree
    | otherwise -> [(Error, [x])]