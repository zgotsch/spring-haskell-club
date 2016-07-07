Homomorphic encryption (https://en.wikipedia.org/wiki/Homomorphic_encryption) is a subset of encryption that allows computations to be done on the cyphertext, which when decrypted yields the result of some useful computation on the plaintext. More mathematically, if we have some encryption function `e` and some decryption function `d`, and some function `f` which we wish to apply to our input `x`, there is a function `g` such that `f(x) = d(g(e(x)))`. (This is equivalent to `e(f(x)) = g(e(x))`, since `e` and `d` are inverses.) Homomorphic encryption allows for secure cloud computing, since it allows operation on encrypted data without service providers having access to the unencrypted data.

For this assignment, we will be using ROT13 (https://en.wikipedia.org/wiki/ROT13) as our encryption. We will write a spellchecker which checks the spelling of ROT13'ed text without decrypting it, using a Trie (https://en.wikipedia.org/wiki/Trie). We'll work through this in a few steps.

There is a module included in this repo with the Trie data structure and some debugging utilties.
```haskell
-- trie.hs

-- ...

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

-- ...
```

0. Write a function `rot13` which applies the ROT13 cipher to a character.

  ```haskell
  rot13 :: Char -> Char
  ```

1. Import the `trie.hs` module. Write functions `insert` which takes a string and a Trie and returns a new Trie which includes that string.

  ```haskell
  insert :: String -> Trie Char -> Trie Char
  ```

1. Write a function `fromList` which takes a list of strings and returns a `Trie` which contains those strings. Hint: recall how we created `fromList` for binary search trees.

  ```haskell
  fromList :: [String] -> Trie Char
  ```

4. Write a function which checks whether a given string is in the trie.

  ```haskell
  contains :: Trie Char -> String -> Bool
  ```

2. Write an instance of `Functor` for `Trie`. Ensure that your implementation satisfies the `Functor` laws.

  ```haskell
  fmap :: (a -> b) -> Trie a -> Trie b
  ```

5. Write a function which takes a dictionary as a list of words, constructs a trie, and uses that trie to check whether a given ROT13'ed string is spelled correctly. This function should never ROT13 the input string. (You can use the dictionary included in `trie.hs` to debug.)

  ```haskell
  isRot13SpelledCorrect :: [String] -> String -> Bool
  ```

6. Extra credit: Wrap this in a full program, which takes a dictionary file and a word on the command line, and checks the spelling of the given word.
7. Extra credit: Instead of simply returning whether the word is correctly spelled, return a list of suggestions. (Hint: you'll probably want to define a function `getSuggestions :: [String] -> String -> Maybe [String]` which serves the same purpose as `isROT13SpelledCorrect` does above).
