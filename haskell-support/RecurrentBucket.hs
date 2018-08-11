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
import           ML4HSFE.Loop
import           ML4HSFE.Outer
import           System.Environment
import           System.IO
import           System.IO.Unsafe           (unsafePerformIO)

bucket :: Int -> [AST] -> [[AST]]
bucket c asts = map (map convert) processed
  where clustered = I.runIdentity
                      (clusterLoop (pureKmeans (Just c))
                                   (handleString width height (A.encode asts)))
        processed = processAsts clustered
        convert v = case A.fromJSON v of
                      A.Error err -> error err
                      A.Success x -> x

bucketer = (Method "recurrent", bucket)

processAsts = H.elems . V.foldl' acc H.empty
  where acc h v = case v of
                    A.Object o -> ins h o
                    _          -> error ("Expected obj got " ++ show v)

        ins h o = let A.Number n = o H.! "cluster"
                   in case (o H.! "quickspecable", o H.! "type") of
                        (_,           A.Null) -> h
                        (A.Bool True, _     ) ->
                          H.insertWith (++)
                                       n
                                       [A.Object
                                         (H.delete "tocluster"
                                           (H.delete "features" o))]
                                       h

width, height :: Int
(width, height) = unsafePerformIO $ do
  w <- lookupEnv "WIDTH"
  h <- lookupEnv "HEIGHT"
  pure (maybe 30 read w, maybe 30 read h)
