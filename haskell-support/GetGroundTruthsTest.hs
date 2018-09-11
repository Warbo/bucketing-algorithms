module Main where

import qualified BucketUtil                 as BU
import           Control.Monad.State.Lazy   (get, put, replicateM_, runState, State)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified GetGroundTruths            as GGT
import           Test.QuickCheck
import           Test.Tasty                 (defaultMain, testGroup)
import           Test.Tasty.QuickCheck      (testProperty)

main = defaultMain $ testGroup "All tests" [
    findColon
  ]

findColon = testProperty "Can find :" go
  where go :: String -> Int -> Property
        go post n = forAll (genSpace n) (canFind post)

        canFind post pre = found pre post (runOn GGT.findColon
                                                 (mkStart pre post))

        mkStart pre post = pre ++ ":" ++ post

        found pre post (_, x) = previous x === (':':reverse pre) .&&.
                                next     x === post              .&&.
                                out      x === ""                .&&.
                                err      x === []

subset = testGroup "Can find subsets" [
      testProperty     "Subsets are spotted" spotSubsets,
      testProperty "Non-subsets are spotted" spotNonSubsets
    ]
  where spotSubsets :: Int -> [Int] -> [Int] -> Property
        spotSubsets x xs indices =
          let super  = nub (x:xs)
              super' = Helper.mkAscendingList super
              sub    = nub (map get indices)
              get i  = super !! (abs i `mod` length super)
           in counterexample (show (("sub", sub), ("super", super)))
                (property (sub `Helper.subset` super'))

        spotNonSubsets :: [Int] -> [Int] -> Property
        spotNonSubsets xs ys = not (null (filter (`notElem` ys) xs)) ==>
          property (not (xs `Helper.subset` (Helper.mkAscendingList ys)))

runOn f s = runState (f testImp) (startState s)

genSpace :: Int -> Gen String
genSpace 0          = pure ""
genSpace n | n < 0  = genSpace (abs n)
genSpace n          = do c  <- elements " \n\t"
                         cs <- genSpace (n - 1)
                         return (c : cs)

data TestImp = TestImp {
    previous :: String
  , next     :: String
  , out      :: String
  , err      :: [String]
  }

startState s = TestImp {
    previous = ""
  , next     = s
  , out      = ""
  , err      = []
  }

testImp :: BU.StreamImp (State TestImp)
testImp = BU.StreamImp {
      BU.getchar     = getchar
    , BU.getcontents = getcontents
    , BU.info        = info
    , BU.putchar     = putchar
    , BU.putstr      = putstr
    }
  where getchar = do x <- get
                     let pre      = previous x
                         (c:rest) = next     x
                     put (x { previous = c:pre, next = rest })
                     pure c

        getcontents = do x <- get
                         let pre  = previous x
                             rest = next     x
                         put (x { previous = reverse rest ++ pre, next = "" })
                         pure (LB.pack rest)

        info :: String -> State TestImp ()
        info s = do x <- get
                    let msgs = err x
                    put (x { err = msgs ++ [s] })

        putchar :: Char -> State TestImp ()
        putchar c = do x <- get
                       put (x { out = out x ++ [c] })

        putstr = mapM_ putchar . LB.unpack
