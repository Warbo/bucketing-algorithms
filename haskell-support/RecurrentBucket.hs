{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module RecurrentBucket where
import           BucketUtil
import           Control.Applicative        ((<|>))
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char                  as C
import qualified Data.Functor.Identity      as I
import qualified Data.HashMap.Strict        as H
import           Data.Maybe                 (fromJust)
import qualified Data.Vector                as V
import qualified HS2AST.Types               as HT
import           ML4HSFE.Loop
import qualified ML4HSFE.Outer              as O
import qualified ML4HSFE.Types              as Ty
import           System.Environment
import           System.IO
import           System.IO.Unsafe           (unsafePerformIO)

bucket :: Int -> [AST] -> [[HT.Identifier]]
bucket !c !asts = processAsts clustered
  where (entries, sccs) = handle width height (A.encode asts)
        clustered       = O.clusterLoop (O.pureKmeans (Just c)) entries sccs

bucketer = (Method "recurrent", bucket)

processAsts = H.elems . V.foldl' ins H.empty
  where ins h o = case (Ty.entryCluster       o,
                        Ty.entryQuickspecable o,
                        Ty.entryTyped         o) of
          (Nothing, _   , _    ) -> error "Unclustered AST"
          (_      , _   , False) -> h
          (Just n , True, True ) -> H.insertWith (++) n [Ty.entryId o] h

width, height :: Int
(width, height) = unsafePerformIO $ do
  w <- lookupEnv "WIDTH"
  h <- lookupEnv "HEIGHT"
  pure (maybe 30 read w, maybe 30 read h)
