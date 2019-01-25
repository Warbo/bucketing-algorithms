{-# LANGUAGE BangPatterns, FlexibleContexts, OverloadedStrings, PartialTypeSignatures #-}
module BucketBounds where

import qualified Control.Lens               as Lens
import           Control.Lens.Operators
import           Control.Lens.Type          (Lens', Prism', Traversal')
import           Data.Aeson                 (toJSON)
import           Data.Aeson.Lens
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Char                  as C
import qualified Data.HashMap               as HM
import           Data.List                  ((\\), intercalate, isInfixOf, nub, sort)
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe                 as M
import           Data.Ratio                 ((%))
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Vector.Lens           as V
import qualified GetGroundTruths            as GGT
import qualified Helper                     as GGTH
import           System.Environment         (lookupEnv)
import           System.Exit                (ExitCode(ExitSuccess, ExitFailure))
import           System.IO                  (hPutStrLn, openFile, IOMode(ReadMode), stderr)
import qualified System.Process             as P
import           Test.QuickCheck            hiding (sample, Result)
import           Test.Tasty                 (defaultMain, localOption, testGroup)
import           Test.Tasty.QuickCheck      (testProperty)
import           Text.Read                  (readMaybe)

-- Entry points

boundsMain :: IO ()
boundsMain = do
  jsonFileM <- lookupEnv "SAMPLE_JSON"
  let jsonFile = M.fromMaybe (error "No SAMPLE_JSON env var given, aborting")
                             jsonFileM
  file <- openFile jsonFile ReadMode
  BS.hGetContents file >>= Lens.traverseOf sizeSamples' processSample'
                       >>= BS.putStr

boundsTest = defaultMain $ testGroup "All tests" [
    prop_bucketSize,
    prop_calcSizesExact,
    prop_calcSizesPlusOne,
    prop_calcSizes,
    prop_makeSizesExact,
    prop_makeSizesRem,
    prop_getSample,
    prop_getReps,
    prop_getSizes,
    prop_collate
  ]

-- Inputs
type Size      = Int
type Name      = T.Text
type Sample    = V.Vector Name
type RawSample = V.Vector A.Value

-- Intermediate
type BucketSize = Int

-- Outputs
type Count       = Int
type Proportion  = Float
type Bucketing   = [Sample]

bucketSizesForBucketCount :: Size -> Count -> [BucketSize]
bucketSizesForBucketCount size count | count > size || count == 0 = error
  ("Can't divide " ++ show size ++ " into " ++ show count ++ "buckets")

bucketSizesForBucketCount size count =
  makeSizes count 0 (calcSizes [] size count)

prop_bucketSize = testProperty "Can calculate bucket sizes to split up samples"
                               go
  where go (Positive size) (Positive count) = size >= count ==> go' size count

        go' size count = let bucketSizes = bucketSizesForBucketCount size count
                             capacity    = sum bucketSizes
                             atCapacity  = capacity === size

                             correctCount = length bucketSizes === count

                             diffMaxOne = let min:_ = sort bucketSizes
                                              diffs = map (`subtract` min) bucketSizes
                                           in property (all (< 2) diffs)

                             info = (("size"       , size       ),
                                     ("count"      , count      ),
                                     ("bucketSizes", bucketSizes),
                                     ("capacity"   , capacity   ))

                          in counterexample (show info)
                                            (atCapacity   .&&.
                                             correctCount .&&.
                                             diffMaxOne)

calcSizes :: [Int] -> Int -> Int -> [Int]
calcSizes acc size count = if remainder == 0
                              then reverse (multiple:acc)
                              else calcSizes (multiple:acc) remainder (count-1)
  where (multiple, remainder) = size `divMod` count

prop_calcSizesExact = testProperty "Can calculate exactly divisible buckets" go
  where go (Positive n) (Positive count) =
          let size = n * count
              got  = calcSizes [] size count
              want = [n]
              info = show (("n", n), ("count", count), ("size", size),
                           ("got", got), ("want", want))
           in counterexample info (want === got)

prop_calcSizesPlusOne = testProperty "Can calculate buckets with remainder 1" go
  where go (Positive n) (Positive count) =
          let size = (n * count) + 1
              got  = bucketSizesForBucketCount size count
              want = replicate (count - 1) n ++ [n+1]
              info = show (("n", n), ("count", count), ("size", size),
                           ("got", got), ("want", want))
           in counterexample info (want === got)

prop_calcSizes = testProperty "Calculated sizes add up" go
  where go (Positive size) (Positive count) =
          size >= count ==> let sizes   = calcSizes [] size count
                                padding = replicate (count - length sizes) 0
                                total   = countUp 0 (sizes ++ padding)
                                info    = show (("count"  , count  ),
                                                ("size"   , size   ),
                                                ("sizes"  , sizes  ),
                                                ("padding", padding),
                                                ("total"  , total  ))
                            in counterexample info (total === size)

        countUp !acc []       = acc
        countUp !acc l@(x:xs) = countUp (acc + (x * length l)) xs

makeSizes len acc []     = replicate len acc
makeSizes len acc (n:ns) = n' : makeSizes (len - 1) n' ns
  where n' = acc + n

prop_makeSizesExact = testProperty "Can make exactly divided buckets" go
  where go (Positive n) (Positive count) =
          let size = n * count
              got  = makeSizes count 0 [n]
              want = replicate count n
              info = show (("n", n), ("count", count), ("size", size),
                           ("got", got), ("want", want))
           in counterexample info (want === got)

prop_makeSizesRem = testProperty "Can make buckets with simple remainder" go
  where go (Positive n) (Positive count) =
          let size = (count * n) + (count - 1)

              gotCounts  = calcSizes [] size count
              wantCounts = [n, 1]

              gotSizes  = makeSizes count 0 wantCounts
              wantSizes = n : replicate (count - 1) (n+1)

              info = show (("n", n), ("count", count), ("size", size),
                           ("gotCounts", gotCounts), ("wantCounts", wantCounts),
                           ("gotSizes" , gotSizes ), ("wantSizes" , wantSizes ))
           in count > 1 ==>
              counterexample info ((gotCounts === wantCounts) .&&.
                                   (gotSizes  === wantSizes ))

-- Like 'map f xs' but also gives an index, starting from 1
imap1 :: (Enum a, Num a) => (a -> b -> c) -> [b] -> [c]
imap1 f = zipWith f [1..]

mapLines f = unlines . map f

data Bound = Min | Max

instance Show Bound where
  show Min = "Min"
  show Max = "Max"

renderModel :: Bound -> [Int] -> Sample -> [[Name]] -> String
renderModel bound bSizes sample theorems = unlines [
    "% " ++ show (("bound"      , bound          ),
                  ("bCount"     , bCount         ),
                  ("bSizes"     , bSizes          ),
                  ("sample"     , sample         ),
                  ("theorems"   , theorems       ),
                  ("theoremDeps", map (\(x, y) -> (x, noAsc y)) GGT.theoremDeps)),

    "include \"globals.mzn\";",

    "% Declare bCount buckets, as sets of elements from 1 to nameCount",
    let declareBucket n = "var " ++ typeS ++ ": " ++ bucket n ++ ";"
     in mapLines declareBucket [1..bCount],

    "% Constrain bucket sizes (cardinalities). These should be the same except",
    "% that bucket1 may contain a remainder",
    let go (n, s) = "constraint card(" ++ bucket n ++ ") = " ++ show s ++ ";"
     in mapLines go (zip [1..bCount] bSizes),

    "% Stop buckets overlapping (one constraint per distinct x/y intersection)",
    let disjoint x y | x >= y = ""  -- Avoid x == y and redundant constraints
        disjoint x y          = let bX        = "bucket" ++ show x
                                    bY        = "bucket" ++ show y
                                    intersect = bX ++ " intersect " ++ bY
                                 in "constraint card(" ++ intersect ++ ") = 0;"
     in mapLines (\x -> mapLines (disjoint x) [1..bCount])
                 [1..bCount],

    "% Define each theorem as a function, taking a bucket and returning 1 if",
    "% its dependencies are all in that bucket and 0 otherwise",
    let mkFunc theorem = let arg    = "(var " ++ typeS ++ ": bucket)"
                             sig    = "function var int: " ++ fst theorem ++ arg
                             isIn d = "(" ++ show d ++ " in bucket)"
                             cond   = sepStrings " /\\ " isIn (snd theorem)
                             body   = "if (" ++ cond ++ ") then 1 else 0 endif"
                          in sig ++ " = " ++ body ++ ";"
     in mapLines mkFunc deps,

    "% Define the score of a bucket as the sum of all theorem functions",
    let sig          = "function var int: score(var " ++ typeS ++ ": bucket)"
        call theorem = "(" ++ fst theorem ++ "(bucket))"
        body         = sepStrings " + " call deps
     in sig ++ " = " ++ body ++ ";",

    "% Tell MiniZinc to min/maximise the sum of all bucket scores",
    let direction = case bound of
                      Max -> "maximize"
                      Min -> "minimize"
        scoreOf n = "score( " ++ bucket n ++ " )"
        sum       = sepStrings " + " scoreOf [1..bCount]
     in "solve " ++ direction ++ " " ++ sum ++ ";",

    "% Turn our integer names back into strings by looking up in an array",
    "array[1..", nameCountS, "] of string: names;",
    let quoted n = "\"" ++ n ++ "\""
     in "names = [ " ++ sepStrings ", " quoted names ++ " ];",

    "% Render chosen buckets to output",
    let showBucket n = "show([ names[i] | i in " ++ bucket n ++ " ])"
     in "output [" ++ sepStrings ", \"\\n\", " showBucket [1..bCount] ++ "];"]
  where nameCount       = V.length sample

        bCount          = length bSizes

        nameCountS      = show nameCount

        typeS    = "set of 1.." ++ nameCountS
        bucket n = "bucket" ++ show n

        names = map T.unpack (V.toList sample)

        -- Since we models the names as numbers 1..nameCount, we convert the
        -- deps of each theorem into their corresponding numbers too.
        deps = let indices :: Map.Map T.Text Int
                   indices      = Map.fromList (imap1 (flip (,))
                                                      (V.toList sample))
                   collect i ds = ("theorem" ++ show i,
                                   map (\n -> M.fromJust (Map.lookup n indices))
                                       ds)
                in imap1 collect theorems

runModel :: String -> IO String
runModel model = P.readProcessWithExitCode "mzn-fzn"
                                           ["-f", "fzn-gecode", "-"]
                                           model >>= go
  where go (code, out, err) = case code of
                                ExitSuccess   -> pure out
                                ExitFailure x -> putStrLn (concat [
                                                   "EXIT CODE\n\n",
                                                   show x,
                                                   "\n\nSTDOUT\n\n",
                                                   out,
                                                   "\n\nSTDERR\n\n",
                                                   err,
                                                   "\n\nMODEL\n\n",
                                                   model]) >> error "DIE"

bucketCounts = [1..20]

data Result = Result {
    minimise :: (Proportion, Bucketing),
    maximise :: (Proportion, Bucketing)
  }

runForAllCounts :: Sample -> IO (Map.Map Count (Either String Result))
runForAllCounts sample = Map.fromList <$> mapM go possibleCounts
  where go bucketCount = let sizes  = bucketSizes bucketCount
                             run  b = runModel (renderModel b
                                                            sizes
                                                            unhexed
                                                            theorems)
                          in do min <- run Min
                                max <- run Max
                                let min' = parseResult unhexed theorems min
                                    max' = parseResult unhexed theorems max
                                showErr min'
                                showErr max'
                                pure (bucketCount, Result <$> min' <*> max')

        possibleCounts = filter (<= sampleSize) bucketCounts

        showErr (Left err) = writeErr err
        showErr _          = pure ()

        unhexed = V.map (unN . GGTH.decodeName . GGTH.N) sample

        sampleSize = length sample

        bucketSizes = bucketSizesForBucketCount sampleSize

        theorems :: [[Name]]
        theorems = theoremsOf unhexed

writeErr :: String -> IO ()
writeErr = hPutStrLn stderr

parseResult :: Sample -> [[Name]] -> String -> Either String
                                                      (Proportion, Bucketing)
parseResult sample theoremDeps out = go                          .
                                     map readMaybe               .
                                     filter (not . null)         .
                                     takeWhile (/= "----------") .
                                     lines                       $ out
  where go :: [Maybe Sample] -> Either String (Proportion, Bucketing)
        go parses = case sequence parses of
          Nothing      -> Left ("Couldn't parse " ++ out)
          Just buckets -> let got  = sum                                    .
                                     map (length . theoremsOf' theoremDeps) .
                                     check                                  $
                                     buckets
                              want = length theoremDeps
                           in Right (fromIntegral got / fromIntegral want,
                                     buckets)

        check :: [Sample] -> [Sample]
        check buckets = case filter (`notElem` V.toList sample)
                                    (concatMap V.toList buckets) of
                          [] -> buckets
                          xs -> error (show (("error",
                                              "Parsed names not in sample"),
                                             ("parsed", xs),
                                             ("sample", sample)))

theoremsOf' :: [[Name]] -> Sample -> [[Name]]
theoremsOf' theoremDeps sample = filter (`GGTH.subset` ascSample) theoremDeps
  where ascSample = GGTH.mkAscendingList (V.toList sample)

theoremsOf :: Sample -> [[Name]]
theoremsOf sample = check (theoremsOf' unwrappedTheoremDeps sample)
  where check l = if null l
                     then error (show (
                            ("error", "Sample should admit at least 1 theorem"),
                            ("sample", sample),
                            ("deps",   unwrappedTheoremDeps)))
                     else l

unwrappedTheoremDeps = map depsOf GGT.theoremDeps
  where depsOf = map unN . unasc . snd

unN (GGTH.N x) = x

unasc (GGTH.AscendingList x) = x

noAsc = map unN . unasc

sepStrings :: String -> (a -> String) -> [a] -> String
sepStrings sep f = intercalate sep . map f

processSample :: Sample -> IO A.Value
processSample s = do r <- rendered
                     pure (A.object ["sample"      A..= s,
                                     "proportions" A..= r])
  where rendered :: IO A.Value
        rendered = A.toJSON . Map.map render <$> runForAllCounts s

        render (Left  s) = A.object ["ok"  A..= False     , "error" A..= s         ]
        render (Right r) = A.object ["ok"  A..= True      ,
                                     "min" A..= let (p, b) = minimise r
                                                 in A.object [
                                                      "proportion" A..= p,
                                                      "buckets"    A..= b],
                                     "max" A..= let (p, b) = maximise r
                                                 in A.object [
                                                      "proportion" A..= p,
                                                      "buckets"    A..= b]]

decodeSample :: Prism' RawSample Sample
decodeSample = Lens.prism encodeSample decodeSample''

decodeSample' :: A.Value -> Either A.Value Sample
decodeSample' (A.Array v) = case decodeSample'' v of
                              Left  x -> Left  (A.Array x)
                              Right x -> Right x

decodeSample'' :: RawSample -> Either RawSample Sample
decodeSample'' = go  {-. _Array-}
  where go raw = let l  = V.toList raw
                     l' = M.catMaybes (map (^? _String) l)
                  in if length l == length l'
                        then Right (V.fromList l')
                        else Left  raw

encodeSample = V.map A.String

sample :: AsValue a => Traversal' a Sample
sample = key "sample" . _Array . decodeSample

repSamples :: AsValue a => Traversal' a Sample
repSamples = members . sample

sizeSamples :: AsValue a => Traversal' a Sample
sizeSamples = members . repSamples

sizeSamples' :: AsValue a => Traversal' a A.Value
sizeSamples' = members . members . key "sample"

processSample' :: A.Value -> IO A.Value
processSample' (A.Array v) = case decodeSample'' v of
                               Left  x -> error ("Couldn't decode " ++ show x)
                               Right s -> processSample s
processSample' x = error ("processSample' needed an Array, given " ++ show x)

prop_getSample = testProperty "Can look up 'sample' from object" go
  where go names = canGetSample names (mkObj names)

        mkObj :: Sample -> String
        mkObj names = let list = renderSample names
                       in "{\"sample\":" ++ list ++ "}"

        sampledNames :: String -> Maybe Sample
        sampledNames = (^? sample)

        canGetSample :: Sample -> String -> Property
        canGetSample names str = let got  = sampledNames str
                                     want = Just names
                                  in got === want

prop_getReps = testProperty "Can look up samples from object of reps"
                            (forAllSamples go)
  where go :: [Sample] -> Property
        go namess = canGetSamples namess (mkReps 1 [] namess)

        mkReps :: Int -> [String] -> [Sample] -> String
        mkReps n acc []       = "{" ++ intercalate "," acc ++ "}"
        mkReps n acc (ns:nss) = let sample = renderSampleObject ns
                                    key    = show (show n)  -- Needs quotes
                                 in mkReps (n + 1)
                                           ((key ++ ":" ++ sample) : acc)
                                           nss

        canGetSamples :: [Sample] -> String -> Property
        canGetSamples nss json = let got :: [Sample]
                                     got = json ^.. repSamples

                                     result = sort got === sort nss
                                  in counterexample (show ("json", json)) result

prop_getSizes = testProperty "Can look up samples from objects of sizes"
                             (forAllSamples go)
  where go :: [Sample] -> Property
        go samples = canGetSamples samples (mkSizes [] (collate samples))

        mkSizes :: [(String, String)] -> [[Sample]] -> String
        mkSizes acc []                 = renderObject acc
        mkSizes acc ([]:sizes)         = error "Empty size; shouldn't happen!"
        mkSizes acc ((rep:reps):sizes) = let key = show (show (length rep))
                                             val = mkReps 0 [] (rep:reps)
                                          in mkSizes ((key, val):acc) sizes

        mkReps :: Int -> [(String, String)] -> [Sample] -> String
        mkReps n acc []         = renderObject acc
        mkReps n acc (rep:reps) = let key = show (show n)
                                      val = renderSampleObject rep
                                   in mkReps (n + 1) ((key,val):acc) reps

        canGetSamples :: [Sample] -> String -> Property
        canGetSamples samples str = let got = str ^.. sizeSamples
                                     in sort samples === sort got

-- Takes a list of (key, value) string pairs and combines them into an object,
-- e.g. 'renderObject [("\"x\"\", "42"), ("\"y\"\", "[]"]' gives {"x":42,"y":[]}
renderObject xs = "{" ++ intercalate "," (map join xs) ++ "}"
  where join (x, y) = if check x
                         then x ++ ":" ++ y
                         else error (show (("error", "Key isn't JSON string"),
                                           ("key"  , x),
                                           ("val"  , y)))
        check "" = False
        check x  = head x == '"' && last x == '"'

-- Test helpers

instance Arbitrary T.Text where
  arbitrary = T.pack . filter ok <$> arbitrary
    where ok c = C.isAlphaNum c && C.isAscii c

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> listOf arbitrary
  shrink    = map V.fromList . shrink . V.toList

renderArray xs = "[" ++ intercalate ", " xs ++ "]"

renderSample :: Sample -> String
renderSample s = renderArray (map show (V.toList s))

renderSampleObject s = "{\"sample\": " ++ renderSample s ++ "}"

collate :: [Sample] -> [[Sample]]
collate = go []
  where go acc []               = acc
        go acc (sample:samples) = go (insert sample acc) samples

        insert sample []            = [[sample]]
        insert sample ((x:xs):rest) = if length sample == length x
                                         then (sample:x:xs) : rest
                                         else (       x:xs) : insert sample rest

prop_collate = testProperty "Can collate samples by size" (forAllSamples go)
  where go :: [Sample] -> Property
        go samples = let collated = collate samples
                      in sameSizes        collated .&&.
                         lengthsDiffer    collated .&&.
                         allFound samples collated

        sameSizes []       = property True
        sameSizes (xs:xss) = counterexample (show ("Lengths should match", xs))
                                            (property (allSameLength xs)) .&&.
                             sameSizes xss

        allSameLength = (== 1) . length . nub . map length

        lengthsDiffer :: [[Sample]] -> Property
        lengthsDiffer = allDistinct . map (head . map length)

        allFound samples collated = sort samples === sort (concat collated)

allDistinct :: (Show a, Eq a) => [a] -> Property
allDistinct xs = let distinct = nub xs
                     dupes    = xs \\ distinct
                  in counterexample (show (("error", "Found dupes"),
                                           ("input", xs           ),
                                           ("dupes", dupes        )))
                                    (length dupes === 0)

-- Using 'arbitrary :: [Sample]' can take ages; this makes smaller values
genSampleList :: Gen [Sample]
genSampleList = do n <- choose (0, 20)
                   vectorOf n genSample
  where genSample :: Gen Sample
        genSample = do n <- choose (0, 20)
                       V.fromList <$> vectorOf n arbitrary

forAllSamples = forAllShrink genSampleList shrink
