{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Main where

import qualified BucketUtil                 as BU
import           Control.Monad.State.Strict (State, get, put, replicateM_,
                                             runState)
import           Criterion                  (benchmark', env, nf)
import           Criterion.Types            (anMean, reportAnalysis)
import qualified Data.Aeson                 as A
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Char                  (isAscii, isPrint, ord)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (foldl', intercalate, nub, nubBy,
                                             sort)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Debug.Trace                (traceShow)
import qualified GetGroundTruths            as GGT
import           GHC.IO.Encoding            (setLocaleEncoding, utf8)
import           Helper                     (AscendingList (..), Name (..),
                                             mkAscendingList, sortUniq)
import qualified Helper
import           Numeric                    (showHex)
import           Numeric.Natural
import           Statistics.Types           (estPoint)
import           Test.QuickCheck
import           Test.Tasty                 (defaultMain, localOption,
                                             testGroup)
import           Test.Tasty.QuickCheck      (QuickCheckTests (..), testProperty)

main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "All tests" [
      augmentRepArray
    , augmentRepNull
    , augmentSize
    , benchmarks
    , canSortUniq
    , findColon
    , getGroundTruths
    , parseEquivalent
    , parseWorks
    , subset
    ]

parseWorks  = testGroup "Can parse examples" [
      testProperty "Can stream objects" checkObject
    ]
  where checkObject =
          let gen = (,) <$> sized genObj <*> arbitrary

              shrink' :: (String, String) -> [(String, String)]
              shrink' (o, s) = let Just o' = A.decode (LB.pack o)
                                   oToS    = LB.unpack . A.encode . A.Object
                                   os      = map oToS (shrinkO o')
                                   ss      = shrink s
                                in map (o,) ss ++ map (,s) os

              f key = do mv <- BU.parseOneChunked' BU.testImp
                         BU.info BU.testImp (show (("key", key), ("val", mv)))

              check (o, s) = case BU.runOn (`BU.streamKeyVals` f) (o ++ s) of
                ((), i) -> counterexample (show ("state", i)) $
                  ("next"    , BU.next     i) === ("next"    , s        ) .&&.
                  ("previous", BU.previous i) === ("previous", reverse o)

           in forAllShrink gen shrink' check

parseEquivalent = testProperty "parseOne' equivalent to parseOneChunked'" go
  where go  = forAllShrink ((,) <$> sized genJSON  <*> arbitrary) shrink' check

        check :: (String, String) -> Property
        check (json, extra) =
          let s          = json ++ extra
              (omb, oi ) = old s
              (nmb, ni ) = new s
              nms        = LB.unpack <$> nmb

           in ("state", ni        ) === ("state", oi        ) .&&.
              ("got"  , nmb       ) === ("got"  , omb       ) .&&.
              ("json" , nms       ) === ("json" , Just json ) .&&.
              ("next" , BU.next ni) === ("next" , extra     )

        old = BU.runOn BU.parseOne'
        new = BU.runOn BU.parseOneChunked'

        shrink' :: (String, String) -> [(String, String)]
        shrink' (json, extra) =
          let Just j   = A.decode (LB.pack json) :: Maybe A.Value
              js       = map ((,extra) . LB.unpack . A.encode) (shrinkJSON j)
              (s1, s2) = splitAt (length extra `div` 2) extra
           in case extra of
                ""  -> js
                [c] -> (json, "") : js
                _   -> (json, s1) : (json, s2) : js

augmentRepArray = testFewer $ testGroup "Rep handling" [
                  testProperty "Read whole array"    (go checkPost   )
    ,             testProperty "Output is parseable" (go checkOutput )
    ,             testProperty "Have right methods"  (go checkMethods)
    , testFewer $ testProperty "Check buckets"       (go checkBuckets)
    ]

  where go f aa@(AA args) = let result = BU.runOn GGT.augmentRep (renderAA aa)
                                -- Show final state if there's a failure
                             in counterexample (show ("result", result))
                                               (f args result)

        checkPost args@(_, _, _, post) (_, x) = BU.next x === post

        checkOutput _ (_, x) =
          let inner = parseOut (BU.out x)
           in isJust inner

        checkMethods args@(methods, _, _, _) (_, x) =
          let Just obj        = parseOut (BU.out x)
              (names, values) = unzip (Map.toList obj)
           in sort methods === sort names

        checkBuckets args (_, x) =
          let Just obj    = parseOut (BU.out x)
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

augmentRepNull = testProperty "Can handle null reps" go
  where go post = check post (BU.runOn GGT.augmentRep ("null" ++ post))

        check post (_, x) = BU.previous x === "llun" .&&.
                            BU.next     x === post

augmentSize = testFewer $
    testProperty "Can augment sizes" (forAllShrink (sized gen) shrink' check)
  where gen :: Int -> Gen (String, String)
        gen n = (,) <$> genSize n <*> arbitrary

        shrink' (s, post) = let Just (A.Object o) = A.decode (LB.pack s)
                             in map ((, post)  .
                                     LB.unpack .
                                     A.encode  .
                                     HM.fromList)   .
                                shrinkKvs (pure []) shrinkRep .
                                HM.toList $ o

        shrinkRep r = []

        check (size, post) = case BU.runOn GGT.augmentSize (size ++ post) of
          ((), i) -> counterexample (show ("state", i)) $
            ("next", BU.next i) === ("next", post)

getGroundTruths = testGroup "getGroundTruths.main'" [
      testProperty "getGroundTruths from empty" empty
    , testFewer $ testProperty "Can run getGroundTruths.main'"
                    (forAllShrink (sized gen) shrink' check)
    ]
  where empty = case BU.runOn GGT.main' "{}" of
          ((), s) -> BU.out  s === "{}" .&&.
                     BU.next s === ""

        gen n = do post  <- arbitrary
                   sizes <- genList genSize n
                   obj   <- genObjWithValues sizes
                   pure (obj, post)

        shrink' (json, post) =
          let posts             = shrink post
              Just (A.Object o) = A.decode (LB.pack json)
              os                = map (LB.unpack . A.encode) (shrinkO o)
           in map (, post) os ++ map (json,) posts

        check (json, post) = case BU.runOn GGT.main' (json ++ post) of
          ((), s) -> counterexample (show ("state", s)) $
            ("previous", BU.previous s) === ("previous", reverse json) .&&.
            ("next"    , BU.next     s) === ("next"    , post        )

benchmarks = testGroup "Benchmarks" [
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

-- Removes dupes and sorts, turning a list of names into valid deps
fixDeps :: Ord a => [a] -> [a]
fixDeps = nub . sort

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

testFewer = localOption (QuickCheckTests 10)

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

renderAA :: ArrayArgs -> String
renderAA (AA args@(methods, sizes, names, post)) =
    let sample = Map.fromList [("sampleNames", map nameToString names)]

        obj :: Map.Map String (Map.Map String [[String]])
        obj = Map.fromList $ map (\m -> (m, renderMethod args))
                                       methods
     in LB.unpack (A.encode (sample, obj)) ++ post

  where renderMethod args@(_, sizes, _, _) =
          Map.fromList (map (\(s, content) -> (s, renderBucket args content))
                            sizes)

        renderBucket (_, _, names, _) content =
          let ns       = map nameToString names
              select n = if n < length ns
                            then ns !! n  -- Safe thanks to our Arbitrary instance
                            else error (show ("length ns", length ns, "n", n))
           in map (map select) content

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

-- | Generate a String of JSON, with a few restrictions. In particular, we don't
--   (yet) bother randomising the whitespace, we don't generate floats or
--   negative numbers and the top-level value will not be a number. These
--   restrictions on numbers are to appease our parser's quirks.
genJSON :: Int -> Gen String
genJSON n = oneof [genArr n, genObj n, genStr, genKeyword]
  where go fuel = case fuel of
                    0 -> oneof [genStr, genNum, genKeyword]
                    _ -> do c <- choose (0, 2) :: Gen Int
                            case c of
                              0 -> genArr fuel
                              1 -> genObj fuel
                              _ -> go     0

-- | Generate a String containing a valid, non-empty JSON string (including the
--   quotes). We don't include non-ASCII characters, but we do include (escaped)
--   backslashes and double-quotes (except the at the start and end, obv.).
genStr = do c  <- genChar
            cs <- listOf genChar
            pure . wrap "\"" "\"" . concatMap esc $ c:cs
  where genChar = oneof (map pure chars)

        chars = concat $ [
            "abcdefghijklmnopqrstuvwxyz"
          , "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
          , "0123456789!\"£$%^&*()-_=+{[}]:;@'~#<,>.?/|\\"
          ]

        esc '"'  = "\\\""
        esc '\\' = "\\\\"
        esc c    = [c]

-- | Generate a String containing a positive, whole number.
genNum = show . abs <$> (arbitrary :: Gen Int)

-- | Pick a random JSON keyword (null, true or false)
genKeyword :: Gen String
genKeyword = oneof (map return ["null", "true", "false"])

-- | Generate a String containing a JSON array. The parameter is "fuel", used to
--   control the size.
genArr :: Int -> Gen String
genArr fuel = wrap "[" "]" <$> (genList genJSON fuel >>= commaSep)

-- | Generate a String containing a JSON object. The parameter is "fuel", used
--   to control the size.
genObj :: Int -> Gen String
genObj fuel = genList genJSON fuel >>= genObjWithValues

-- | Replace duplicate keys with freshly generated ones
uniqueKeys :: [String] -> [(String, a)] -> Gen [(String, a)]
uniqueKeys seen kvs = case kvs of
  []          -> pure []
  (k, v):kvs' -> if k `elem` seen
                    then do k' <- genStr
                            uniqueKeys seen ((k', v):kvs')
                    else ((k, v):) <$> uniqueKeys (k:seen) kvs'

-- | Generate a JSON object with randomly generated keys, where the values are
--   taken from the given list.
genObjWithValues :: [String] -> Gen String
genObjWithValues vals = do keys <- vectorOf (length vals) genStr
                           uniqueKeys [] (zip keys vals) >>= mkObj

-- | Turn (key, value) pairs of JSON into a JSON object.
mkObj :: [(String, String)] -> Gen String
mkObj kvs = mapM pairUp kvs >>= commaSep >>= (pure . wrap "{" "}")
  where pairUp (k, v) = do [a, b, c, d] <- vectorOf 4 (genSpace 3)
                           pure (concat [a, k, b, ":", c, v, d])

-- | Join the given strings together, interspersed with commas. Random
--   whitespace will be added at the start, end and between the commas.
commaSep :: [String] -> Gen String
commaSep []     = genSpace 3
commaSep [x]    = (x ++) <$> genSpace 3
commaSep (x:xs) = do pre <- genSpace 3
                     suf <- genSpace 3
                     ((pre ++ x ++ suf ++ ",") ++) <$> commaSep xs

-- | Add a prefix and suffix the a third String.
wrap :: String -> String -> String -> String
wrap pre suf = (++ suf) . (pre ++)

-- | Generate a list of values, using the given generator for each element. A
--   "fuel" parameter will be divided up amongst the calls, preventing unbounded
--   recursion.
genList :: (Int -> Gen a) -> Int -> Gen [a]
genList _   0 = return []
genList gen 1 = return <$> gen 0
genList gen n = do fuel <- choose (1, n)
                   x    <- gen fuel
                   xs   <- genList gen (n - fuel)
                   return (x:xs)

-- | Generate a String containing JSON for one "rep".
genRep n = if n == 0
              then pure "null"
              else do AA (ms, ss, ns, _) <- arbitrary
                      pure (renderAA (AA (ms, ss, ns, "")))

-- | Generate a String containing JSON for one "size".
genSize n = genList genRep n >>= genObjWithValues

-- | Shrink an Aeson Value; useful with 'forAllShrink'.
shrinkJSON :: A.Value -> [A.Value]
shrinkJSON j = case j of
  A.Bool True -> [A.Null, A.Bool False]
  A.Bool _    -> [A.Null]
  A.Null      -> []
  A.Object o  -> A.Null : map A.Object (shrinkO o)
  A.Array  a  -> A.Null : map A.Array  (shrinkA a)
  A.String t  -> A.Null : map A.String (shrinkT t)
  A.Number n  -> A.Null : map A.Number (shrinkN n)

-- | Shrink a HashMap; map Aeson's 'Object' over this make JSON Values.
shrinkO o = let lst = HM.toList o
                len = length lst
                (pre, suf) = splitAt (len `div` 2) lst
             in case len of
                  0 -> []
                  1 -> let [(k, v)] = lst
                           ks = shrinkT k
                           vs = shrinkJSON v
                           os = map (,v) ks ++ map (k,) vs
                        in map (HM.fromList . pure) os
                  _ -> [HM.fromList pre, HM.fromList suf]

-- | Shrink a Vector; map Aeson's 'Array' over this to make JSON values.
shrinkA a = let lst = V.toList a
                len = length lst
                (pre, suf) = splitAt (len `div` 2) lst
             in case len of
                  0 -> []
                  1 -> let [x] = lst
                           xs  = shrinkJSON x
                        in map (V.fromList . pure) xs
                  _ -> [V.fromList pre, V.fromList suf]

-- | Shrink a Text; map Aeson's 'String' over this to make JSON values.
shrinkT t = map T.pack (shrink (T.unpack t))

-- | Shrink a Scientific; map Aeson's 'Number' over this to make JSON values.
shrinkN n = if n == 0 then [] else [0]

shrinkKvs :: (key -> [key]) -> (val -> [val]) -> [(key, val)] -> [[(key, val)]]
shrinkKvs fk fv kvs = case kvs of
  []       -> []
  [(k, v)] -> map pure $ map (k,) (fv v) ++ map (,v) (fk k)
  _        -> let (pre, suf) = splitAt (length kvs `div` 2) kvs
               in [pre, suf]
