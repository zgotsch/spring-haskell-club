-- trie.hs
module Trie where

import Prelude hiding (words)
import Control.Applicative

-- a trie is a list of edges
type Trie a = [TrieEdge a]
-- a trie edge is a combination of three values
--   the value on the edge, of type a (Char for our trie)
--   whether this edge ends a word (needed to distinguish "a" from "apple")
--   the child trie
--
-- a trie which contains "a" and "apple" looks like:
-- [Edge 'a' True [Edge 'p' False [Edge 'p' False [Edge 'l' False [Edge 'e' True []]]]]]
data TrieEdge a = Edge a Bool (Trie a)
  deriving Show

-- Print the words contained in a character trie
-- This, along with `show`, should help you debug!
words :: Trie Char -> [String]
words branches = concat $ fmap words' branches

words' :: TrieEdge Char -> [String]
words' (Edge a True endings) = liftA2 (:) [a] ("":(words endings))
words' (Edge a False endings) = liftA2 (:) [a] (words endings)

dictionary :: [String]
dictionary = [
  "apple",
  "apricot",
  "avocado",
  "banana",
  "blackberry",
  "blueberry",
  "cherry",
  "cranberry"]
