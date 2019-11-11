{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified BucketUtil                 as BU
import           Control.Monad.State.Strict (get, put, replicateM_, runState, State)
import           Criterion                  (benchmark', env, nf)
import           Criterion.Types            (anMean, reportAnalysis)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Char                  (isAscii, isPrint, ord)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (foldl', nub, nubBy, sort)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Debug.Trace                (traceShow)
import qualified GetGroundTruths            as GGT
import           GHC.IO.Encoding            (setLocaleEncoding, utf8)
import qualified Helper
import           Helper                     ( addTrie
                                            , AscendingList(..)
                                            , checkTrie
                                            , listToTrie
                                            , mkAscendingList
                                            , Name(..)
                                            , sortUniq
                                            , Trie(..)
                                            , trieSubsets
                                            , zTrie
                                            )
import           Numeric                    (showHex)
import           Numeric.Natural
import           Statistics.Types           (estPoint)
import           Test.QuickCheck
import           Test.Tasty                 (defaultMain, localOption, testGroup)
import           Test.Tasty.QuickCheck      (QuickCheckTests(..), testProperty)

main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "All tests" [
      augmentArray
    , augmentNull
    , benchmarks
    , canSortUniq
    , findColon
    , subset
    , tries
    ]

augmentArray = localOption (QuickCheckTests 3) $ testGroup "Rep handling" [
      testProperty "Read whole array"    (go checkPost   )
    , testProperty "Output is parseable" (go checkOutput )
    , testProperty "Have right methods"  (go checkMethods)
    , testProperty "Check buckets"       (go checkBuckets)
    ]

  where go f (AA args) = let result = BU.runOn GGT.augmentRep (render args)
                             -- Show final state if there's a failure
                          in counterexample (show ("result", result))
                                            (f args result)

        checkPost args@(_, _, _, post) (_, x) = BU.next x === post

        checkOutput _ (_, x) =
          let inner = parseOut (reverse (BU.out x))
           in isJust inner

        checkMethods args@(methods, _, _, _) (_, x) =
          let Just obj        = parseOut (reverse (BU.out x))
              (names, values) = unzip (Map.toList obj)
           in sort methods === sort names

        checkBuckets args (_, x) =
          let Just obj    = parseOut (reverse (BU.out x))
              (_, values) = unzip (Map.toList obj)
           in conjoin (map (checkMethod args) values)

        checkMethod args@(_, sizes, _, _) obj =
          let (names, values) = unzip (Map.toList obj)
              namesMatch = sort names === sort (map fst sizes)
           in counterexample "Bucket sizes as requested" namesMatch .&&.
              conjoin (map (checkBucket args) values)

        checkBucket (_, _, names, _) lst =
          let ns   = map nameToString names
              ok n = counterexample (show (("test", "Names come from sample"),
                                           ("name" , n                      ),
                                           ("names", ns                     )))
                                    (property (n `elem` ns))
           in conjoin (map ok (concat lst))

        render args@(methods, sizes, names, post) =
          let sample = Map.fromList [("sampleNames", map nameToString names)]

              obj :: Map.Map String (Map.Map String [[String]])
              obj = Map.fromList $ map (\m -> (m, renderMethod args))
                                       methods
           in LB.unpack (A.encode (sample, obj)) ++ post

        renderMethod args@(_, sizes, _, _) =
          Map.fromList (map (\(s, content) -> (s, renderBucket args content))
                            sizes)

        renderBucket (_, _, names, _) content =
          let ns       = map nameToString names
              select n = if n < length ns
                            then ns !! n  -- Safe thanks to our Arbitrary instance
                            else error (show ("length ns", length ns, "n", n))
           in map (map select) content

augmentNull = testProperty "Can handle null reps" go
  where go post = check post (BU.runOn GGT.augmentRep ("null" ++ post))

        check post (_, x) = BU.previous x === "llun" .&&.
                            BU.next     x === post

benchmarks = testGroup "Benchmarks" [
      ("trie lookup", map GGT.theoremFilesAdmittedBy  ) `beats`
      ("list lookup", map GGT.theoremFilesAdmittedTrie) $
      vectorOf 1000 (genAscNames 50)
    ]
  where beats (fastName, fastF) (slowName, slowF) gen =
          testProperty (fastName ++ " beats " ++ slowName)
                       (once . ioProperty $ do
                         -- Generate an input out here, so it's shared
                         input    <- generate gen

                         -- Use 'env' to force input before benchmarking
                         fastTime <- meanTime (nf fastF input)
                         slowTime <- meanTime (nf slowF input)

                         return $
                           counterexample (show (("fastTime", fastTime)
                                                 ,("slowTime", slowTime)))
                                          (property (fastTime < slowTime)))

        meanTime b = estPoint . anMean . reportAnalysis <$> benchmark' b

tries = testGroup "Trie tests" [addCommutes, buildTrie, checkAddTrie]

addCommutes = testProperty "Adding to a Trie doesn't care about order" go
  where go :: Int -> Int -> String -> String -> [String] -> [String] -> Property
        go t1 t2 d1 d2 deps1 deps2 = let
            trie12 = addTrie t2 l2 (addTrie t1 l1 zTrie)
            trie21 = addTrie t1 l1 (addTrie t2 l2 zTrie)

            l1 = sortUniq (d1:deps1)
            l2 = sortUniq (d2:deps2)

          in l1 /= l2 ==> trie12 === trie21

buildTrie = testProperty "Can build trie of theorem dependencies" go
  where go :: [[Dep]] -> Dep -> [Dep] -> Natural -> [Dep] -> Property
        go deps d ds n extra =
          let -- Prepend d to ensure the list is non-empty. We will use this as
              -- a set of dependencies for a theorem, then try looking it up.
              want = d:ds

              -- Every theorem should have at least one dep
              deps' = filter (not . null) deps

              -- Pick a theorem ID to associate with want. We will assign IDs
              -- sequentially, so this can't be larger than the total number of
              -- deps sets (including want).
              wantID = fromIntegral n `mod` (length deps' + 1)

              -- Insert want into deps' such that it gets assigned to wantID
              deps'' :: [[Dep]]
              deps'' = map fixDeps $
                         take wantID deps' ++ [want] ++ drop wantID deps'

              -- Assign IDs using zip and build a trie
              trie :: Trie Int Dep
              trie = listToTrie (zip [0..] deps'')

              -- Our query should work for any superset of want, so we allow it
              -- to be extended with extra deps to check this.
              query :: [Dep]
              query = fixDeps (want ++ extra)

              -- Look up theorem IDs whose deps are subsets of our query
              got :: [Int]
              got = Helper.trieSubsets (mkAscendingList query) trie

              -- Querying for a superset of want should always find wantID
              foundWant = counterexample (show (("error" , "Didn't get wantID")
                                               ,("want"  , want               )
                                               ,("wantID", wantID             )
                                               ,("query" , query              )
                                               ,("got"   , got                )
                                               ,("deps''", deps''             )
                                               ,("trie"  , trie               )
                                               ,("extra" , extra              )
                                               ))
                                         (property (wantID `elem` got))

              -- Only subsets of our query should be returned
              gotSubset t p = p .&&. counterexample
                (show (("error" , "Result not in query")
                      ,("query" , query                )
                      ,("result", t                    )
                      ,("got"   , got                  )
                      ,("all"   , map (deps'' !!) got  )
                      ))
                (property (all (`elem` query)
                               (deps'' !! t)))

           in foldr gotSubset foundWant got

-- Removes dupes and sorts, turning a list of names into valid deps
fixDeps :: Ord a => [a] -> [a]
fixDeps = nub . sort

checkAddTrie = testProperty "addTrie passes checks" go
  where go :: [[String]] -> Int -> [String] -> Property
        go pre t deps = let
            pre' = map fixDeps (filter (not . null) pre)

            trie = listToTrie (zip [0..] pre')

            deps'  = mkAscendingList (fixDeps deps)

            result = addTrie t deps' trie
          in checkTrie trie == [] ==> checkTrie result === []

canSortUniq = testProperty "sortUniq sorts and removes dupes" go
  where go :: [(Natural, Int)] -> Property
        go given = check (mkWant given) (Helper.sortUniq (fromFreqs given))

        fromFreqs []          = []
        fromFreqs ((0, _):xs) =     fromFreqs           xs
        fromFreqs ((n, x):xs) = x : fromFreqs ((n-1, x):xs)

        mkWant :: [(Natural, Int)] -> [Int]
        mkWant = nub . sort . foldl' add []

        add xs (0, _) =   xs
        add xs (_, x) = x:xs

        check want (AscendingList got) = want === got

findColon = testProperty "Can find colons" go
  where go :: String -> Int -> Property
        go post n = forAll (genSpace n) (canFind post)

        canFind post pre = found pre post (BU.runOn GGT.findColon
                                                    (mkStart pre post))

        mkStart pre post = pre ++ ":" ++ post

        found pre post (_, x) = BU.previous x === (':':reverse pre) .&&.
                                BU.next     x === post              .&&.
                                BU.out      x === ""                .&&.
                                BU.err      x === []

subset = testGroup "Can find subsets" [
      testProperty     "Subsets are spotted" spotSubsets,
      testProperty "Non-subsets are spotted" spotNonSubsets
    ]
  where spotSubsets :: Int -> [Int] -> [Int] -> Property
        spotSubsets x xs indices =
          let super  = nub (x:xs)
              super' = mkAscendingList super
              sub    = mkAscendingList (nub (map get indices))
              get i  = super !! (abs i `mod` length super)
           in counterexample (show (("sub", sub), ("super", super)))
                (property (sub `Helper.subsetAsc` super'))

        spotNonSubsets :: [Int] -> [Int] -> Property
        spotNonSubsets xs ys = not (null (filter (`notElem` ys) xs)) ==>
          property (not (mkAscendingList xs `Helper.subsetAsc`
                         mkAscendingList ys))

-- Helpers

type Dep         = String
type Method      = String
type BucketCount = String

newtype ArrayArgs = AA
  ([Method], [(String, [[Int]])], [Name], String)
  deriving (Show, Eq)

checkArgs :: ArrayArgs -> ArrayArgs
checkArgs args@(AA (methods, sizes, names, post)) =
    if all indexInRange indices
       then args
       else error "Index overflow"
  where indices :: [Int]
        indices = concat (concatMap snd sizes)

        indexInRange n = (n >= 0 && n < length names) ||
                         error (show ("Index not in range", n, length names))

instance Arbitrary ArrayArgs where
  arbitrary = do
      -- Each method will have all of the sizes, so don't generate too many. In
      -- reality we'll only have a few methods (e.g. "hash" and "recurrent"),
      -- and bucket sizes of a few dozen
      methods <- choose (0, 3)  >>= vector
      sizes   <- choose (0, 20) >>= vector

      -- We need a non-empty list of names
      names <- nub <$> (choose (1, 100) >>= vector)

      post  <- printable <$> arbitrary

      pure (checkArgs (AA (
        -- These are object keys, so make them unique to avoid edge
        -- cases. Also stick to printable ASCII characters.
        nub (map printable methods),

        -- 'sizes' tells us which buckets to use (fst) and which names to
        -- include in each bucket (snd). We use random Ints, treating each 'n'
        -- as the index '|n| % length names'.
        -- We also remove duplicate names from buckets, to reduce lookup time.
        nubBy sameSize (map (fixSize (length names)) sizes),

        names,
        post)))

    where fixSize :: Int -> (String, [[Int]]) -> (String, [[Int]])
          fixSize r (s, ids) = (
            -- Bucket sizes should be printable (in reality they're numeric
            -- strings, but we might as well check more generically)
            printable s,

            -- We traverse 'ids' to (a) limit values to the range of 'names' and
            -- (b) to remove duplicate entries in each list of buckets.
            fst (foldl' (dedupe r) ([], []) ids))

          -- Each entry gets restricted to the range [0, r). We then make sure
          -- that each 'Int' occurs at most once in the resulting '[[Int]]'.
          dedupe :: Int -> ([[Int]], [Int]) -> [Int] -> ([[Int]], [Int])
          dedupe r (prev, seen) ns = let ranged = map (idInRange r) ns
                                         unseen = filter (`notElem` seen) ranged
                                         unique = nub unseen
                                      in (unique:prev, unique ++ seen)

          idInRange r n = abs n `mod` r

          sameSize (x, _) (y, _) = x == y

          printable = map replaceUnprintable

          replaceUnprintable c = if isAscii c && isPrint c
                                    then c
                                    else 'X'

  shrink (AA (methods, sizes, names, post)) =
      map checkArgs (shrinkMethods ++ shrinkSizes ++ shrinkNames ++ shrinkPost)
    where shrinkMethods = case methods of
                            []   -> []
                            [""] -> [AA ([]      , sizes, names, post)]
                            [m]  -> [AA ([m']    , sizes, names, post) |
                                     m' <- shrink m]
                            _    -> [AA (methods', sizes, names, post) |
                                     methods' <- halves methods]

          halves []  = []
          halves [_] = [[]]
          halves l   = [take (length l `div` 2) l,
                        drop (length l `div` 2) l]

          shrinkSizes = if null sizes
                           then []
                           else [AA (methods, sizes', names, post) |
                                 sizes' <- halves sizes ++ shrinkSizesInner]

          shrinkSizesInner = [map (\(s, l) -> (shrinkLst s, shrinkLst l)) sizes]

          shrinkLst [] = []
          shrinkLst l  = head (halves l)

          shrinkNames = [AA (methods, newSizes names', names', post) |
                         names' <- filter (not . null) (halves names)]

          newSizes names'        = map (newSize names') sizes
          newSize  names' (s, l) = (s, map (nub . map (newIndex names')) l)
          newIndex names' n      = n `mod` length names'

          shrinkPost = if null post
                          then []
                          else [AA (methods, sizes, names, post') |
                                post' <- halves post]


parseOut :: String -> Maybe (Map.Map Method (Map.Map BucketCount [[String]]))
parseOut s = do
    pair <- parseOut' s
    let obj = snd pair

        getNames :: Map.Map String [A.Value] -> [[String]]
        getNames m = let Just ns = Map.lookup "names" m
                      in map extract ns

        extract :: A.Value -> [String]
        extract (A.Array ns) = map jsonToString (V.toList ns)

    pure (Map.map (Map.map getNames) obj)

  where parseOut' :: String -> Maybe (A.Value,
                                      Map.Map Method
                                              (Map.Map BucketCount
                                                       (Map.Map String
                                                                [A.Value])))
        parseOut' = A.decode . LB.pack

        jsonToString (A.String s) = T.unpack s

genSpace :: Int -> Gen String
genSpace 0          = pure ""
genSpace n | n < 0  = genSpace (abs n)
genSpace n          = do c  <- elements " \n\t"
                         cs <- genSpace (min (n - 1) 20)
                         return (c : cs)

genName :: Gen String
genName = nameToString <$> arbitrary

nameToString (Helper.N t) = T.unpack t

encodeName (Helper.N t) = Helper.N (T.append (T.pack "global")
                                             (T.foldl' encode (T.pack "") t))
  where encode s c = T.append s (T.pack (showHex (ord c) ""))

instance Arbitrary Name where
  arbitrary = encodeName <$> elements depNames

depNames = nub (concatMap get GGT.theoremDeps)
  where get (_, AscendingList deps) = deps

genAscNames :: Int -> Gen (AscendingList Name)
genAscNames n = mkAscendingList . nub <$> vectorOf n arbitrary
