{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Helper where

import           Control.Monad              (mzero)
import qualified Data.Aeson                 as A
import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Char                  as C
import qualified Data.HashMap.Strict        as H
import qualified Data.List                  as List
import qualified Data.Maybe                 as M
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax (Exp(..), Lift, lift)
import qualified Numeric                    as N
import qualified System.Environment         as Env
import           System.IO.Unsafe           (unsafePerformIO)

type TheoremID = String

newtype Name = N T.Text deriving (Eq, Ord, Show)

instance Lift Name where
  lift (N n) = do arg <- lift (T.unpack n)
                  pure (AppE (ConE 'N) (AppE (VarE 'T.pack) arg))

instance A.FromJSON Name where
  parseJSON s = N <$> A.parseJSON s

instance A.ToJSON Name where
  toJSON (N n) = A.toJSON n

instance L.FromLisp Name where
  parseLisp l = case l of
                  L.Symbol n -> pure (N (unescapeSym n))
                  _          -> mzero
    -- Our cached data uses symbols, which may be escaped if they
    -- contain e.g. "'". Escaped symbols are wrapped in pipes "|".
    where unescapeSym s = case T.stripPrefix "|" s of
                            Nothing -> s
                            Just s2 -> case T.stripSuffix "|" s2 of
                                         Nothing  -> s
                                         Just s3 -> s3

newtype TheoremDeps = TDs [(TheoremID, [Name])]

instance L.FromLisp TheoremDeps where
  parseLisp l = do tds <- L.parseLisp l
                   pure (TDs (map toPair tds))
    where toPair [Left id, Right deps] = (id, deps)

-- Lists which we assume are sorted
newtype AscendingList a = AscendingList { unAsc :: [a] } deriving (Eq, Show)

mkAscendingList :: Ord a => [a] -> AscendingList a
mkAscendingList = AscendingList . List.sort

-- | Check whether the first AscendingList is a subset of the second. This is
--   faster than comparing with (one or two) unordered lists, since we can
--   discard small elements as they're encountered, knowing they won't appear
--   further down the lists.
subsetAsc :: Ord a => AscendingList a -> AscendingList a -> Bool
subsetAsc (AscendingList l1) (AscendingList l2) = go l1 l2
  where go []     _          = True
        go _      []         = False
        go a@(x:xs) b@(y:ys) = case x `compare` y of
                                 LT -> False
                                 EQ -> go xs b
                                 GT -> go a  ys

decodeName (N n) = case M.catMaybes [T.stripPrefix "global" n,
                                     T.stripPrefix "Global" n] of
                     []   -> error ("Bad name " ++ show n)
                     n2:_ -> N (T.pack (decodeASCII "" (T.unpack n2)))
  where decodeASCII acc s = case s of
                              []     -> reverse acc
                              a:b:cs -> decodeASCII (unHex a b:acc) cs
        unHex a b = case N.readHex [a, b] of
                      [(n, "")] -> C.chr n
                      _         -> error (show ("Invalid hex", a, b))

nub :: (Eq a, Ord a) => [a] -> [a]
nub = go [] . List.sort
  where go acc l  = case l of
                      []       ->   acc
                      [x]      -> x:acc
                      (x:y:zs) -> if x == y
                                     then go    acc  (y:zs)
                                     else go (x:acc) (y:zs)

data Result = R {
    names    :: Either [Name] [[Name]]
  , theorems :: [TheoremID]
  }

instance A.ToJSON Result where
  toJSON r = A.object [
      "names"    A..= either A.toJSON A.toJSON (names r)
    , "theorems" A..= A.toJSON (theorems r)
    ]

sampleResultToJSON r = A.object [
    "sampleNames"    A..= either A.toJSON A.toJSON (names r)
  , "sampleTheorems" A..= A.toJSON (theorems r)
  ]

-- We arrange the theorem dependencies into a trie to speed up subset finding.
-- The dependencies are sorted, then the first level of the trie contains a list
-- for each smallest-element, e.g. [(t1, [A, B]), (t2, [B, D]), (t3, [E]),
-- (t4, [E, F])] gives
--
--   +-- A ----- B --- t1
--   |
-- --+-- B ----- D --- t2
--   |
--   |       +-- t1
--   |       |
--   +-- E --+
--           |
--           +-- F --- t4

data Trie a b = TrieNode [a] (AscendingList (b, Trie a b)) deriving (Eq, Show)

-- | The empty Trie; useful for initialising a fold
zTrie :: Ord b => Trie a b
zTrie = TrieNode [] (AscendingList [])

-- | Checks the invariants of the given Trie. Returns a list of errors found (an
--   empty list means no errors). We require Show to generate specific messages.
checkTrie :: (Ord b, Show a, Show b) => Trie a b -> [String]
checkTrie (TrieNode _ asc) = go (unAsc asc)
  where go []                   = []
        go [(a, ta)]            = checkTrie ta
        go ((a, ta):(b, tb):ys) =
          if a < b then [] else [show a ++ " > " ++ show b] ++
          checkTrie ta                                      ++
          go ((b, tb):ys)

addTrie :: forall a b. Ord b => a -> AscendingList b -> Trie a b -> Trie a b
addTrie tid (AscendingList deps) (TrieNode tids (AscendingList branches)) =
    case deps of
      []   -> TrieNode (tid:tids) (AscendingList branches)
      d:ds -> TrieNode      tids  (   go d ds [] branches)

  where go :: Ord b => b -> [b] -> [(b, Trie a b)] -> [(b, Trie a b)]
                    -> AscendingList (b, Trie a b)
        go d ds acc bss = case bss of
          []        -> let b' = addTrie tid (AscendingList ds) zTrie
                        in AscendingList $ reverse ((d, b'):acc)

          (x, b):bs -> case d `compare` x of
                         LT -> let b' = addTrie tid (AscendingList ds) zTrie
                                in AscendingList $
                                     reverse ((x, b):(d, b'):acc) ++ bs

                         EQ -> let b' = addTrie tid (AscendingList ds) b
                                in AscendingList $ reverse ((x, b'):acc) ++ bs

                         GT -> go d ds ((x, b):acc) bs

listToTrie :: forall a b. Ord b => [(a, [b])] -> Trie a b
listToTrie = go zTrie . map sortDeps
  where sortDeps :: Ord b => (a, [b]) -> (a, AscendingList b)
        sortDeps (t, deps) = (t, sortUniq deps)

        go :: Trie a b -> [(a, AscendingList b)] -> Trie a b
        go !acc []              = acc
        go !acc ((t, deps):tds) = go (addTrie t deps acc) tds

sortUniq :: forall a. Ord a => [a] -> AscendingList a
sortUniq = AscendingList . List.foldl' insertIfMissing []
  where insertIfMissing :: Ord a => [a] -> a -> [a]
        insertIfMissing []     x = [x]
        insertIfMissing (y:ys) x = case x `compare` y of
                                     LT -> x:y:ys
                                     EQ ->   y:ys
                                     GT ->   y:insertIfMissing ys x

theoremsToTrie :: [(TheoremID, [Name])] -> Trie TheoremID Name
theoremsToTrie = listToTrie

trieSubsets :: Ord b => AscendingList b -> Trie a b -> [a]
trieSubsets (AscendingList deps) t = go [] t deps
  where go !acc (TrieNode ids branches) ds = go' (ids ++ acc) (unAsc branches) ds

        go' !acc []            _      = acc
        go' !acc _             []     = acc
        go' !acc l@((x, b):bs) (d:ds) = case d `compare` x of
                                          -- Ignore d and try again
                                          LT -> go' acc           l  ds

                                          -- Look in b; also look for ds in bs
                                          EQ -> go' (go acc b ds) bs ds

                                          -- Skip b and look in bs
                                          GT -> go' acc bs (d:ds)

theoremsFromTrie :: AscendingList Name -> Trie TheoremID Name -> [TheoremID]
theoremsFromTrie = trieSubsets
