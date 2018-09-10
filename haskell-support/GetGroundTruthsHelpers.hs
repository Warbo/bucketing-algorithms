{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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

newtype Name = N T.Text deriving (Eq, Show)

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

subset []     ys = True
subset (x:xs) ys = (x `elem` ys) && (xs `subset` ys)

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
