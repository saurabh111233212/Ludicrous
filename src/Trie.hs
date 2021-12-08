{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Trie where



import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.Foldable as Foldable
import qualified Data.List
import Test.QuickCheck
import Data.Map.Lazy as M
import Data.Maybe

{-
Custom implementation of a Trie, drawing ideas from this tutorial:
https://bigonotetaking.wordpress.com/2015/11/06/a-trie-in-haskell/
-}


-- | The trie stores sequences of a's. In the typical use case of strings, a = Char
data Trie a
 = Trie {
     end :: Bool, -- is there a sequence which ends here? 
     getMap :: M.Map a (Trie a) -- the mapping to the rest of the children
 } deriving (Show, Eq)

-- | empty Trie
empty :: Trie a
empty = Trie {end = False, getMap = M.empty}

-- | is this sequence stored in the Trie?
member :: Ord a => [a] -> Trie a -> Bool
member [] t = end t
member (a : as) (Trie end map) = case M.lookup a map of
    Nothing -> False
    Just t -> Trie.member as t

-- | inserts a sequence into the Trie
insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie _ map) = Trie True map
insert (a : as) (Trie end map) = Trie end (M.insert a (Trie.insert as Trie.empty) map)

-- | deletes a sequence from the Trie
delete :: Ord a => [a] -> Trie a -> Trie a
delete [] (Trie _ map) = Trie False map
delete (a : as) t@(Trie end map) = case M.lookup a map of
    Nothing -> t
    Just t' -> Trie end (M.insert a (Trie.delete as t') map)

-- | is this the empty Trie?
null :: Trie a -> Bool
null (Trie end map) = not end && M.null map

-- | inserts all elements into a Trie
fromList :: Ord a => [[a]] -> Trie a
fromList [] = Trie.empty
fromList (a : as) = Trie.insert a (Trie.fromList as)


-- | converts the trie to a list 
toList :: Ord a => Trie a -> [[a]]
toList t = aux t [] [] 
    where 
        aux :: Trie a -> [a] -> [[a]] -> [[a]]
        aux t curr rest = let newRest = if end t then curr : rest else rest in
            aux Trie.empty curr newRest


-- q: How to iterate over children? Something like an fmap over elements in the map...then glueing together results...