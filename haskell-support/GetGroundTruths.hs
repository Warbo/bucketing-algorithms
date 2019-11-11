{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
module GetGroundTruths where

import           Helper
import           Control.Monad              (mzero)
import qualified BucketUtil                 as BU
import           BucketUtil                 (getchar, info, putchar, putstr)
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
import           Language.Haskell.TH.Syntax (lift, runIO)
import qualified Numeric                    as N
import qualified System.Environment         as Env
import           System.IO.Unsafe           (unsafePerformIO)

theoremDeps :: [(TheoremID, AscendingList Name)]
theoremDeps = map wrapSecond unwrapped
  where -- At run time we convert each (already sorted) list of deps into an
        -- 'AscendingList'. This is a newtype so there's minimal overhead.
        wrapSecond (t, deps) = (t, AscendingList deps)

        -- Build the datastructure at compile time, including sorting the deps
        -- into ascending order. There's no 'Lift' instance for 'AscendingList'
        -- so we leave them as regular lists.
        unwrapped =
          $(let name = "BENCHMARKS_THEOREM_DEPS"

                lispToData l = case L.parseEither L.parseLisp l of
                                 Left  e -> error e
                                 Right d -> d

                strToData :: B.ByteString -> [(TheoremID, [Name])]
                strToData  s = case AP.parseOnly (L.lisp <* AP.endOfInput) s of
                                 Left  e -> error e
                                 Right l -> lispToData l

                sortSecond (t, deps) = (t, List.sort deps)

             in do mp <- runIO (Env.lookupEnv name)
                   s  <- case mp of
                           Nothing -> error ("Env doesn't contain " ++ name)
                           Just p  -> runIO (B.readFile p)
                   lift (map sortSecond (strToData s)))

-- | A (rose) tree structure where nodes contain a list of TheoremIDs and each
--   branch/subtree is labelled with a Name. The path from the root to the node
--   containing a TheoremID is that theorem's dependency set. For example:
--
--       Trie [] [
--         (n1, Trie [t1, t2] [
--           (n2, Trie [t3] [])
--         ]),
--         (n2, Trie [] [
--           (n3, Trie [t4, t5] [
--             (n4, Trie [t6] [])
--           ]),
--           (n4, Trie [t7] [])
--         ])
--       ]
--
--   In this case each theorem (t1, t2, etc.) has at least one dependency, since
--   the outermost list is empty. The paths to each theorem are the
--   dependencies, hence:
--
--     t1 depends only on n1
--     t2 also depends only on n1
--     t3 depends on n1 and n2
--     t4 and t5 depend on n2 and n3
--     t6 depends on n2, n3 and n4
--     t7 depends on n2 and n4
--
--   The dependencies are always in ascending order, which makes querying for
--   supersets faster, since we can short-circuit and avoid some subtrees
--   entirely.
theoremDepsTrie :: Trie TheoremID Name
theoremDepsTrie = listToTrie unwrapped  -- Convert into a Trie at runtime
  -- We read  the data at compile time using TemplateHaskell. This was we get a
  -- pure value to work with, and we avoid repeating some work over and over.
  -- Note that TemplateHaskell can only build certain types of values, so we do
  -- the final conversion step at runtime.
  where unwrapped :: [(TheoremID, [Name])]
        unwrapped =
          $(let name = "BENCHMARKS_THEOREM_DEPS"

                lispToData l = case L.parseEither L.parseLisp l of
                                 Left  e -> error e
                                 Right d -> d

                strToData :: B.ByteString -> [(TheoremID, [Name])]
                strToData  s = case AP.parseOnly (L.lisp <* AP.endOfInput) s of
                                 Left  e -> error e
                                 Right l -> lispToData l

                sortSecond (t, deps) = (t, List.sort deps)

             in do mp <- runIO (Env.lookupEnv name)
                   s  <- case mp of
                           Nothing -> error ("Env doesn't contain " ++ name)
                           Just p  -> runIO (B.readFile p)
                   lift (map sortSecond (strToData s)))

-- | Look up the ground truth theorems for a given sample
theoremFilesAdmittedTrie :: AscendingList Name -> [TheoremID]
theoremFilesAdmittedTrie sample = trieSubsets sample theoremDepsTrie

theoremFilesAdmittedBy :: AscendingList Name -> [TheoremID]
theoremFilesAdmittedBy sample = map fst (filter match theoremDeps)
  where match td = snd td `subsetAsc` sample

mkResult :: LB.ByteString -> Result
mkResult = mkResult' . Right . M.fromJust . A.decode

sampleResult :: [Name] -> A.Value
sampleResult = sampleResultToJSON . mkResult' . Left

mkResult' :: Either [Name] [[Name]] -> Result
mkResult' ns = R { names = ns, theorems = either process nested ns }
  where process :: [Name] -> [TheoremID]
        process [] = []
        process ns = theoremFilesAdmittedBy (mkAscendingList (map decodeName
                                                                  ns))

        nested :: [[Name]] -> [TheoremID]
        nested []  = []
        nested nns = nub (concatMap process nns)

augmentSize :: Monad m => BU.StreamImp m -> m ()
augmentSize imp = BU.streamKeyVals' imp go
  where go key = do info imp ("Found rep " ++ show key)
                    putstr imp key
                    putstr imp " : "
                    augmentRep imp

-- We can't use streamKeyVals here, since reps may be null
augmentRep :: Monad m => BU.StreamImp m -> m ()
augmentRep imp = do c <- BU.skipSpace' imp
                    case c of
                      '[' -> do info imp "Rep is an array"
                                putchar imp '['
                                goArray imp
                      'n' -> do 'u' <- getchar imp
                                'l' <- getchar imp
                                'l' <- getchar imp
                                info imp "Rep is null"
                                putstr imp "null"
                      _   -> error (show (("Wanted", "'[' or 'null'"),
                                          ("Got",    c)))

goArray :: Monad m => BU.StreamImp m -> m ()
goArray imp = do -- First entry is {"sampleNames":[...]}
                 Just sampleStr <- BU.parseOne' imp
                 info imp ("First array elem is " ++ show sampleStr)
                 let Just obj    = A.decode sampleStr
                     Just !names = H.lookup "sampleNames"
                                     (obj :: H.HashMap String [Name])
                 info imp ("Array elem contains names " ++ show names)
                 putstr imp (A.encode (sampleResult names))
                 putchar imp ','

                 -- Second entry is {"method1":{...}, ...}
                 info imp "Looking for second elem (an object)"
                 '{' <- BU.skipSpace' imp
                 putchar imp '{'
                 go True
                 info imp "Closing off array"
                 ']' <- BU.skipSpace' imp
                 putchar imp ']'

  where go first = do info imp ("Looking for " ++ (if first
                                                      then "first"
                                                      else "next") ++ " key")
                      mk <- BU.parseOne' imp
                      case mk of
                        Nothing -> do info imp "No key found, closing off object"
                                      putchar imp '}' -- We've hit, and consumed, the '}'
                        Just !k -> do let key = BU.trimKey k
                                      info imp ("Found key " ++ show k)
                                      if first
                                         then pure ()
                                         else putchar imp ','
                                      f key
                                      go False

        f key = do putstr imp key
                   putstr imp " : "
                   info imp ("Processing " ++ show key ++ " as an object")
                   BU.streamKeyVals' imp (augmentRun imp)

findColon :: Monad m => BU.StreamImp m -> m ()
findColon imp = do c <- getchar imp
                   case c of
                     ':' -> pure ()
                     _ | C.isSpace c -> findColon imp
                     _ -> error ("Looking for ':' found " ++ show c)

augmentRun :: Monad m => BU.StreamImp m -> LB.ByteString -> m ()
augmentRun imp key = do info imp ("Found run " ++ show key)
                        putstr imp key
                        putstr imp " : "
                        findColon imp
                        Just str <- BU.parseOne' imp
                        info imp ("Value is " ++ show str)
                        putstr imp (A.encode (mkResult str))

main' imp = BU.streamKeyVals' imp go
  where go !key = do info imp ("Found size " ++ show key)
                     putstr imp key
                     putstr imp " : "
                     augmentSize imp

mainIO = main' BU.io
