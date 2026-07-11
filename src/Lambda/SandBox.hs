{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE BangPatterns #-}



module Lambda.SandBox where

import Control.Applicative (Alternative (..), Const (..))
import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), Kleisli (..), (>>>), (>>^), (^>>))
import Control.Category (Category, (.), id)
import Control.Comonad (Comonad (..))
import Control.Monad (MonadPlus (..), (>=>))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Writer (MonadWriter, runWriter, tell)
import Control.Natural (type (:~>) (..), type (~>))
import Data.Coerce (coerce)
import Data.Default (Default (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Adjunction (Adjunction (..))
import Data.Functor.Alt (Alt (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Contravariant (Op (..))
import Data.Functor.Extend (Extend (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Plus (Plus (..))
import Data.Functor.Rep (Representable (..))
import Data.Functor.Yoneda (liftYoneda, runYoneda)
import Data.Key (Key, Keyed (..), Lookup (..))
import Data.List (isPrefixOf, nub, sortOn, tails)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (..), Sum (..))
import Data.Profunctor (Profunctor (..), Strong (..))
import Data.Tuple (swap)
import Data.Void (Void, absurd)
import qualified Data.Set as Set

import Data.Foldable (Foldable(..), fold)
import Data.List.NonEmpty (NonEmpty(..))
import Lambda (safeHead)
import Prelude hiding (id, (.), filter, reverse, iterate)
import qualified Data.List.NonEmpty as NE
import Safe (tailMay)

-- | splits an even length list such as [1,2,3,4,5,6] -> ([1,2,3], [4,5,6])
halve :: [a] -> Maybe ([a], [a])
halve list
  | even (length list) = Just (splitAt (length list `div` 2) list)
  | otherwise = Nothing

-- | get the third element of a list
third' :: [a] -> Maybe a
third' (_ : _ : x : _) = Just x
third' _ = Nothing

third :: [a] -> Maybe a
third = safeHead . drop 2

-- | behaves in the same way as tail except that
-- it maps the empty list to itself rather than producing an error
sTail :: [a] -> [a]
sTail [] = []
sTail (_ : xs) = xs

sTail' :: [a] -> [a]
sTail' = fromMaybe [] . Safe.tailMay

sTail'' :: [a] -> [a]
sTail'' = drop 1

scalar :: (Num a) => [a] -> [a] -> a
-- scalar xs = zip xs >>> map (uncurry (*)) >>> sum
scalar = zipWith (*) >>> (>>> sum)

-- scalar v1 v2 = sum $ zipWith (*) v1 v2

-- -- Counts the number of occurence of True in a list
-- countTrue :: [Bool] -> Int
-- countTrue = filter id >>> length

-- Counts the number of occurence of a word w in a string s
count :: (Eq a) => [a] -> [a] -> Int
count w = tails >>> filter (isPrefixOf w) >>> length

count2 :: String -> String -> Int
count2 w = words >>> filter (== w) >>> length

printOrmolu :: IO ()
printOrmolu = do
  content <- readFile "ormolu.yaml"
  putStr content

countFile :: String -> FilePath -> IO ()
-- countFile w = readFile >>> fmap (count2 w) >>> (>>= print)
-- countFile w = readFile >>> fmap (count2 w) >=> print
-- countFile w = readFile >=> (count2 w >>> return) >=> print
-- countFile w = runKleisli (countArrow w)
-- countFile w = readFile >=> arr words >=> arr (filter (==w)) >=> arr length >=> print
-- countFile w = readFile >=> arr (count2 w) >=> print
countFile w = runKleisli $ Kleisli readFile >>> arr (count2 w) >>> Kleisli print

countArrow :: String -> Kleisli IO FilePath ()
countArrow w = Kleisli readFile >>> arr words >>> arr (filter (== w)) >>> arr length >>> Kleisli print

-- do
-- content <- readFile filePath
-- return $ count2 w content
-- ========================================================================= --
--                               STREAM FUNCTIONS                            --
-- ========================================================================= --

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Category SF where
  id = SF id
  (.) (SF lf) (SF lg) = SF (lf . lg) -- coerce
  --
  -- (>>>) (SF lf) (SF lg) = SF lg . SF lf
  -- (<<<) (SF lf) (SF lg) = SF (lg . lf)

data DiscreteArrows a b where
  Refl :: DiscreteArrows a a

instance Category DiscreteArrows where
  id = Refl
  Refl . Refl = Refl

instance Arrow SF where
  -- arr :: (b -> c) -> SF b c
  arr f = SF (map f)

  -- first :: SF b c -> SF (b, d) (c, d)
  first (SF lf) = SF $ unzip >>> first lf >>> uncurry zip

  -- second :: SF b c -> SF (d, b) (d, c)
  second sf = arr swap >>> first sf >>> arr swap

  -- (&&&) :: SF b c -> SF b d -> SF b (c, d)
  -- (&&&) sf1 sf2 = arr (id &&& id) >>> first sf1 >>> second sf2
  -- (&&&) (SF lf) (SF lg) = SF $ zip <$> lf <*> lg
  -- (&&&) (SF lf) (SF lg) = SF $ liftA2 zip lf lg
  (&&&) (SF lf) (SF lg) = SF $ (lf &&& lg) >>> uncurry zip

  -- (***) :: SF b c -> SF b' c' -> SF (b, b') (c, c')
  (***) sf1 sf2 = first sf1 >>> second sf2

-- (***) (SF lf) (SF lg) = SF $ unzip >>> lf *** lg >>> uncurry zip -- more efficient.

composeMaybe :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
composeMaybe f g = f >>> (>>= g)

composeMonad :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
composeMonad f g = f >=> g

composeSF :: ([a] -> [b]) -> ([b] -> [c]) -> [a] -> [c]
composeSF f g = f >>> g

composeMonadSF :: (Maybe a -> [b]) -> (Maybe b -> [c]) -> (Maybe a -> [c])
composeMonadSF f g = f >>> map pure >>> (>>= g)

composeApplicativeList :: (Applicative f) => (f a -> [b]) -> (f b -> [c]) -> f a -> [c]
composeApplicativeList f g = f >>> map pure >>> (>>= g)

composeAppMonad :: (Applicative f, Monad m) => (f a -> m b) -> (f b -> m c) -> f a -> m c
composeAppMonad f g = f >=> g . pure

lift :: Maybe a -> [a]
lift Nothing = []
lift (Just x) = [x]

newtype MyArrow f m a b = MyArrow {runMyArrow :: f a -> m b}

-- instance (Applicative f, Monad m) => C.Category (MyArrow f m) where
-- id :: MyArrow f m a a
-- id = MyArrow f m lift

-- instance (Applicative f, Monad m) => Arrow MyArrow where
-- id :: MyArrow f m a a
-- id = MyArrow lift
-- (.) f g = f >= g . pure

-- newtype CoArrow w a b = CoArrow {runCoArrow :: w a -> w b}
-- instance Comonad w => Category (CoArrow w) where
--   id = CoArrow id
--   CoArrow f . CoArrow g = CoArrow (f . g)
-- instance Comonad w => Arrow (CoArrow w) where
-- arr = CoArrow . fmap
-- first (CoArrow f) = CoArrow (split >>> reconcile)
--   where
--     split = (fmap fst >>> f) &&& (extract >>> snd)
--     reconcile (wc, d) = fmap (, d) wc

choiceSF :: SF a c -> SF b c -> SF (Either a b) c
choiceSF (SF lf) (SF lg) = SF (\xs -> combine xs (lf [x | Left x <- xs]) (lg [y | Right y <- xs]))
  where
    combine (Left _ : xs) (z : zs) ws = z : combine xs zs ws
    combine (Right _ : xs) zs (w : ws) = w : combine xs zs ws
    combine (Left _ : _) [] _ = error "SF choiceSF: length mismatch for Left branch"
    combine (Right _ : _) _ [] = error "SF choiceSF: length mismatch for Right branch"
    combine [] _ _ = []

choiceSF' :: SF a c -> SF b c -> SF (Either a b) c
choiceSF' (SF lf) (SF lg) = SF (zip [0 ..]) >>> SF process
  where
    process ind_e = mergeAndSort left_res right_res
      where
        (left_in, right_in) = foldr split ([], []) ind_e
        split (i, Left y) (ls, rs) = ((i, y) : ls, rs)
        split (j, Right z) (ls, rs) = (ls, (j, z) : rs)
        left_res = uncurry zip (fmap lf (unzip left_in))
        right_res = uncurry zip (fmap lg (unzip right_in))
        mergeAndSort :: [(Int, c)] -> [(Int, c)] -> [c]
        mergeAndSort t1 t2 = map snd (sortOn fst (t1 ++ t2))

mapA' :: SF a b -> SF [a] [b]
mapA' f = arr listcase >>> (arr id `choiceSF'` (f *** mapA' f >>> arr (uncurry (:))))
  where
    listcase [] = Left []
    listcase (x : xs) = Right (x, xs)


-- ========================================================================= --
--                                Writer Arrow                             --
-- ========================================================================= --

-- newtype Writer w a b = Writer {runWriter :: (w, a) -> (w, b)}

-- instance Category (Writer w) where
--   id = Writer id
--   (Writer f) . (Writer g) = Writer (f . g)

-- instance Arrow (Writer w) where
--   arr = fmap >>> Writer
--   first (Writer f) = Writer $ split >>> reconcile
--     where
--       split = (fmap fst >>> f) &&& (snd >>> snd)
--       reconcile ((e, b), c) = (e, (b, c))

-- Note, Writer cannot be a Monoid: ArrowZero and ArrowPlus
-- For this we need to specialize such as with WriterKleisli

type ArrowWriter w = WriterKleisli w Identity

-- instance ArrowChoice (Writer w) where
--   left (Writer f) = Writer fn
--     where
--       fn (e, Left x) = Left <$> f (e, x)
--       fn (e, Right y) = (e, Right y)

instance ArrowChoice SF where
  left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
    where
      combine (Left _ : xs) (z : zs) = Left z : combine xs zs
      combine (Right y : xs) zs = Right y : combine xs zs
      combine (Left _ : _) [] = error "SF left: length mismatch"
      combine [] _ = []

mapA :: (ArrowChoice arr) => arr a b -> arr [a] [b]
mapA fn = arr listCase >>> (fBase ||| fRec)
  where
    listCase [] = Left []
    listCase (x : xs) = Right (x, xs)
    fBase = arr id
    fRec = (fn *** mapA fn) >>> arr (uncurry (:))

-- (|||) :: ArrowChoice arr => arr a c -> arr b c -> arr (Either a b) c
-- (|||) a1 a2 = arr \cond -> case cond of
--             Left x  -> a1 x
--             Right y -> a2 y

-- ========================================================================= --
--                                StateKleisli Arrow                      --
-- ========================================================================= --

type FailingWriter w = WriterKleisli w Maybe

newtype WriterKleisli w m a b = WriterKleisli {runWriterKleisli :: (w, a) -> m (w, b)}
  deriving Functor

-- 1. The Standalone Primitive Constructor
arrWK :: Applicative m => (a -> b) -> WriterKleisli w m a b
arrWK f = WriterKleisli $ fmap f >>> pure

instance (Monad m) => Applicative (WriterKleisli w m a) where
  pure = const >>> arrWK
  WriterKleisli ff <*> WriterKleisli fx = WriterKleisli $ \(w, a) -> do 
    (e, f) <- ff (w, a)
    (e', x) <- fx (e, a)
    pure (e', f x)

instance Functor m => Profunctor (WriterKleisli w m) where
  lmap f (WriterKleisli g) = WriterKleisli $ fmap f >>> g
  rmap f (WriterKleisli g) = WriterKleisli $ g >>> fmap (fmap f)
  -- dimap f g (WriterKleisli h) = WriterKleisli $ fmap f >>> h >>> fmap (fmap g)

instance Monad m => Category (WriterKleisli w m) where
  id = WriterKleisli pure
  (WriterKleisli f) . (WriterKleisli g) = WriterKleisli (g >=> f)



-- 2. Strong depends ONLY on arrWK (No Arrow required!)
instance Monad m => Strong (WriterKleisli w m) where
  first'  wk = liftA2 (,) (lmap fst wk) (arrWK snd)
  second' wk = liftA2 (,) (arrWK fst) (lmap snd wk)

-- 3. Arrow inherits everything cleanly from Strong and arrWK
instance Monad m => Arrow (WriterKleisli w m) where
  arr = arrWK
  first = first'
  second = second'

-- morphFirst is a perfectly lawful natural transformation from WriterKleisli w m a to WriterKleisli w m (a, c)
morphFirst :: WriterKleisli w m a ~> WriterKleisli w m (a, c)
morphFirst (WriterKleisli f) = WriterKleisli (f . fmap fst)

-- morphSecond is a perfectly lawful natural transformation from WriterKleisli w m b to WriterKleisli w m (d, b)
morphSecond :: WriterKleisli w m b ~> WriterKleisli w m (d, b)
morphSecond (WriterKleisli f) = WriterKleisli (f . fmap snd)

instance MonadPlus m => ArrowZero (WriterKleisli w m) where
  zeroArrow = WriterKleisli (const mzero)
instance MonadPlus m => ArrowPlus (WriterKleisli w m) where
  WriterKleisli f <+> WriterKleisli g = WriterKleisli fn 
    where 
      fn pair = f pair <|> g pair
instance Monad m => ArrowChoice (WriterKleisli w m) where
  left (WriterKleisli f) = WriterKleisli fn 
    where 
      fn (w, Left a)  = fmap Left <$> f (w, a) 
      fn (w, Right b) = pure (w, Right b)

type MyMonad w m a = WriterKleisli w m a 



-- instance Monad m => Monad (WriterKleisli w m a) where
--   WriterKleisli fx >>= h = WriterKleisli (\(w, a) -> do
--     (w', x) <- fx (w, a)
--     runWriterKleisli (h x) (w', a))


-- fn :: Functor m => c -> WriterKleisli w m a b -> WriterKleisli w m a (b, c)
-- fn y  = fmap (, y)

-- morph :: WriterKleisli w m a (b, c) -> WriterKleisli w m (a,c) (b, c)
-- morph (WriterKleisli f) = WriterKleisli (\(w, (a, _)) -> f (w, a))



-- morphSecond :: c -> WriterKleisli w m a b -> WriterKleisli w m (a, c) b
-- morphSecond c (WriterKleisli f) = WriterKleisli $ fmap fst >>> f 

-- functorSecond :: WriterKleisli w m (a, c) b -> WriterKleisli w m (a, c) (a,b)
-- functorSecond = fmap











-- test1 = runSF (mapA' (SF (map (+1)))) [[1,2], [3,4,5]]

delay :: b -> SF b b
delay x = SF (x :)

test2 :: [[Int]]
test2 = runSF (mapA' (delay 0)) [[1, 2], [3, 4, 5], [6]]

-- main :: IO ()
-- main = print test2

nor :: SF (Bool, Bool) Bool
nor = arr (not . uncurry (||))

-- test3 = runSF nor (zip [True, False, False, True] [False, False, True, True])
s1 :: [Bool]
s1 = replicate 5 False ++ replicate 10 True ++ replicate 5 False

s2 :: [Bool]
s2 = cycle [True, False]

-- main :: IO ()
-- main = print (runSF nor (zip s1 s2))

s3 :: [Bool]
s3 = cycle $ replicate 5 False ++ replicate 5 True

edge :: SF Bool Bool
edge = arr id &&& (delay False >>> arr not) >>> arr (uncurry (&&))

-- >>> arr detect
-- where detect (a,b) = a && not b

main :: IO ()
main = print (take 40 (runSF edge s3))

filterA :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterA f as = fmap fn pair
  where
    pair = (sequenceA . (id &&& (sequenceA . map f))) as
    fn = uncurry zip >>> filter snd >>> map fst

-- filterA f = fmap (map fst . filter snd) . sequenceA . map (sequenceA . (id &&& f))
filterA''' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterA''' p = foldr fn (pure [])
  where
    fn x = liftA2 (\keep -> if keep then (x :) else id) (p x)

filterA' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterA' _ [] = pure []
filterA' p (x : xs) = liftA2 fn (p x) (filterA' p xs)
  where
    fn True bs = x : bs
    fn False bs = bs

filterA'' :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterA'' p = foldr fn (pure [])
  where
    fn x acc = case p x of
      Just False -> acc
      Just True -> (x :) <$> acc
      Nothing -> Nothing

filterA5 :: (a -> Maybe Bool) -> [a] -> Maybe [a]
filterA5 p = foldr fn (pure [])
  where
    fn x acc = case p x of
      Just False -> acc
      Just True -> (x :) <$> acc
      Nothing -> Nothing

mySecond' :: (Arrow a) => a b c -> a (d, b) (d, c)
mySecond' f = swap ^>> first f >>^ swap

fanout' :: Arrow a => a b c -> a b d -> a b (c, d)
fanout' f g = dupe ^>> first f >>> mySecond' g
  where dupe x = (x, x)

-- split' :: Arrow a => a b c -> a d e -> a (b, d) (c, e)
-- split' f g = first f >>> second' g

-- instance MonadPlus m => ArrowZero (Kleisli m) where
--     zeroArrow = Kleisli (const mzero)

-- instance MonadPlus m => ArrowPlus (Kleisli m) where
-- Kleisli f <+>' Kleisli g = Kleisli fn 
--     where fn y = mplus (f y) (g y)

instance ArrowZero SF where
    zeroArrow = SF (const [])
instance ArrowPlus SF where 
      SF f <+> SF g = SF fn
        where fn xs = f xs ++ g xs


-- ========================================================================= --
--                          Natural Transformation                         --
-- ========================================================================= --

nt :: Maybe ~> []
nt Nothing = []
nt (Just x) = [x]

nt2 :: Maybe ~> []
nt2 Nothing = []
nt2 (Just _) = []

nt3 :: Maybe ~> []
nt3 Nothing = []
nt3 (Just x) = [x, x]

lengthC :: [] ~> Const Int
lengthC [] = Const 0
lengthC (_:xs) = lengthC xs + Const 1

length' :: [a] -> Int 
length' = lengthC >>> getConst

scam :: Const b ~> Maybe
scam = const Nothing

scam2 :: Monoid a => Const b a -> Maybe a
scam2 = const $ Just mempty

scam3 :: Default a => Const b a -> Maybe a
scam3 = const $ Just def

-- There are two possible natural transformations from Reader () to Maybe
-- There are two possible value of Maybe ()
-- This is Yoneda lemma.
-- nat :: Reader () ~> Maybe 
-- -- nat (Reader f) = Just (f ()) -- will not compile ReaderT stuff
-- nat = runReader >>> ($ ()) >>> Just

readerToMaybe :: Reader () :~> Maybe
readerToMaybe = NT (runReader >>> ($ ()) >>> Just)

maybeToList :: Maybe :~> []
maybeToList = NT (maybe [] pure)

readerToList :: Reader () :~> []
readerToList = readerToMaybe >>> maybeToList


-- Yoneda. This is equivalent ot liftYoneda (Just True)
-- added some quickchek to verify.

maybeBoolToNat :: Maybe Bool -> (((->) Bool) :~> Maybe)
maybeBoolToNat m = NT (
  case m of 
    Nothing -> const Nothing
    Just True -> ($ True) >>> Just
    Just False -> ($ False) >>> Just
  )

maybeBoolToNat' :: Maybe Bool -> (((->) Bool) :~> Maybe)
maybeBoolToNat' m = NT $ runYoneda (liftYoneda m)

-- testResult1 = runYoneda yo (\b -> if b then 10 else 20)
-- testResult2 = maybeBoolToNat (Just True) # (\b -> if b then 10 else 20)

-- There are the 3 Yoneda prediction. 3 possible value of Either ()
-- Normal becausue Either () is iso to Maybe
eitherBoolToNat :: Either () Bool -> (((->) Bool) :~> Either ())
eitherBoolToNat m = NT (
  case m of 
    Left () -> const (Left ())
    Right True -> ($ True) >>> Right
    Right False -> ($ False) >>> Right
  )

eitherBoolToNat' :: Either () Bool -> (((->) Bool) :~> Either ())
eitherBoolToNat' m = NT $ runYoneda (liftYoneda m)

-- natBoolEither1 :: Reader Bool ~> Either ()
-- natBoolEither1 = const $ Left () 

-- natBoolEither2 :: Reader Bool ~> Either ()
-- natBoolEither2 = runReader >>> ($ True) >>> Right 

-- natBoolEither3 :: Reader Bool ~> Either ()
-- natBoolEither3 = runReader >>> ($ False) >>> Right 


natOp :: Op Bool :~> Op String 
-- natOp = getOp >>> (>>> show) >>> Op 
natOp = NT (Op show >>>)


-- newtype Yoneda f a = Yoneda { runYoneda :: Reader a :~> f}

-- -- helperFn ::(a -> b) -> (Reader a :~> f) -> Reader b c -> f c
-- -- helperFn ab ff = runReader >>> lmap ab >>> reader >>> (ff #)

-- -- helper2Fn ::(a -> b) -> Yoneda f a -> Reader b :~> f
-- -- helper2Fn ab (Yoneda ff) = NT $ runReader >>> lmap ab >>> reader >>> (ff #)

-- instance Functor (Yoneda f) where
--     fmap g y = Yoneda $ NT $ runReader >>> lmap g >>> reader >>> (runYoneda y #)


-- ========================================================================= --
--                             Writer for logging                          --
-- ========================================================================= --

class Monoid w => LogSink w where
  emitLog :: String -> w

instance LogSink (Endo [String]) where
  emitLog s = Endo (s :)

instance LogSink () where
  emitLog _ = () -- Zero allocation log discarding

instance LogSink (Sum Int) where
  emitLog _ = Sum 1 -- Ignores string, counts exactly 1 call per event

instance (LogSink a, LogSink b) => LogSink (a, b) where
  emitLog s = (emitLog s, emitLog s) -- Broadcasts to both sinks simultaneously

factorial :: (MonadWriter w m, LogSink w) => Int -> m Int
factorial 0 = 1 <$ tell (emitLog "0! is 1. ")
factorial n = do 
  res <- fmap (* n) (factorial (n - 1))
  tell $ emitLog (show n ++ "! is " ++ show res ++ ". ")
  return res

runFactorialEndo :: Int -> ([String], Int)
runFactorialEndo n = let (result, builder) = runWriter (factorial n)
                      in (appEndo builder [], result)

runFactorialNull :: Int -> Int
runFactorialNull n = let (result, ()) = runWriter (factorial n)
                      in result

runFactorialCount :: Int -> (Int, Int)
runFactorialCount n = let (result, Sum cnt) = runWriter (factorial n)
                       in (cnt, result)

runFactorialHybrid :: Int -> (Int, [String], Int)
runFactorialHybrid n = let (result, (Sum cnt, builder)) = runWriter (factorial n)
                        in (cnt, appEndo builder [], result)


-- ========================================================================= --
-- 1. Modern Architecture of Data.Functor.Adjunction (adjunctions package)
-- ========================================================================= --

-- In Edward Kmett's `adjunctions` library, Functor Adjunctions (f -| u) are defined
-- via leftAdjunct and rightAdjunct.
-- 
-- class (Functor f, Functor u) => Adjunction f u | f -> u, u -> f where
--   unit         :: a -> u (f a)
--   counit       :: f (u a) -> a
--   leftAdjunct  :: (f a -> b) -> a -> u b
--   rightAdjunct :: (a -> u b) -> f a -> b

-- The classic Tuple / Function Adjunction (Currying as Adjunction)
curryAdjunction :: (f, a) -> (f -> a) -> a
curryAdjunction (env, _) g = g env


-- ========================================================================= --
-- 1. ! Functor
-- ========================================================================= --

data Zero a = Zero Void deriving (Show, Eq, Functor)
type instance Key Zero = Void
instance Keyed Zero where
  mapWithKey :: (Key Zero -> a -> b) -> Zero a -> Zero b
  mapWithKey _ (Zero v) = absurd v
instance Lookup Zero where 
  lookup :: Key Zero -> Zero a -> Maybe a
  lookup _ _ = Nothing
instance Alt Zero where -- Cannot be Plus 
  Zero x <!> _ = absurd x
instance Comonad Zero where
  extract :: Zero a -> a
  extract (Zero v) = absurd v
  duplicate :: Zero a -> Zero (Zero a)
  duplicate (Zero v) = absurd v 
instance Extend Zero where 
  duplicated :: Zero a -> Zero (Zero a)
  duplicated (Zero v) = absurd v
instance Semigroup (Zero a) where 
  Zero x <> _  = absurd x
-- instance Monoid (Zero a) where 
--   mempty = Zero undefined

data MyV1 a deriving (Show, Eq, Ord, Read, Functor)
instance Semigroup (MyV1 a) where 
  z <> _ = case z of {}

-- instance Alt (MyV1 a) where 
--   x <!> y = 


-- The Terminal Bang Functor (!) - equivalalent to Const ().
data MyProxy a = MyProxy deriving (Show, Eq, Ord, Read, Functor) -- cannot be a semigroup
instance Alt MyProxy where
  _ <!> _ = MyProxy
instance Plus MyProxy where 
  zero = MyProxy 
instance Extend MyProxy where 
  duplicated :: MyProxy a -> MyProxy (MyProxy a)
  duplicated _ = MyProxy
instance Applicative MyProxy where
  pure = const MyProxy
  _ <*> _ = MyProxy
instance Monad MyProxy where 
  _ >>= _ = MyProxy 
type instance Key MyProxy = Void
instance Keyed MyProxy where
  mapWithKey :: (Key MyProxy -> a -> b) -> MyProxy a -> MyProxy b
  mapWithKey _ _ = MyProxy
instance Lookup MyProxy where 
  lookup :: Key MyProxy -> MyProxy a -> Maybe a
  lookup _ _ = Nothing
instance Distributive MyProxy where
    distribute :: Functor f => f (MyProxy a) -> MyProxy (f a)
    distribute _ = MyProxy
instance Representable MyProxy where
  type Rep MyProxy = Void
  tabulate _ = MyProxy
  index MyProxy = absurd

newtype MyIdentity a = MyIdentity a deriving (Show, Eq, Semigroup, Monoid, Functor)
instance Alt MyIdentity where -- cannot be Plus 
  idX <!> _ = idX
instance Extend MyIdentity where 
  duplicated :: MyIdentity a -> MyIdentity (MyIdentity a)
  duplicated = MyIdentity
instance Comonad MyIdentity where
  extract :: MyIdentity a -> a
  extract (MyIdentity a) = a
  duplicate :: MyIdentity a -> MyIdentity (MyIdentity a)
  duplicate = MyIdentity
instance Applicative MyIdentity where
  pure = MyIdentity
  MyIdentity ff <*> MyIdentity a = MyIdentity (ff a)
type instance Key MyIdentity = ()  
instance Keyed MyIdentity where
  mapWithKey :: (Key MyIdentity -> a -> b) -> MyIdentity a -> MyIdentity b
  mapWithKey f idA = MyIdentity (f ()) <*> idA
instance Lookup MyIdentity where 
  lookup :: Key MyIdentity -> MyIdentity a -> Maybe a
  lookup () (MyIdentity a) = Just a 
instance Distributive MyIdentity where
  distribute :: Functor f => f (MyIdentity a) -> MyIdentity (f a)
  distribute ff = MyIdentity $ extract <$> ff 
  -- collect :: Functor f => (a -> MyIdentity b) -> f a -> MyIdentity (f b)
  -- collect ff fa = distribute  $ ff <$> fa
instance Representable MyIdentity where
  type Rep MyIdentity = ()  
  tabulate :: (() -> a) -> MyIdentity a
  tabulate f = MyIdentity (f ()) 
  index :: MyIdentity a -> () -> a
  index (MyIdentity a) () = a
instance Monad MyIdentity where 
  (>>=) :: MyIdentity a -> (a -> MyIdentity b) -> MyIdentity b
  -- MyIdentity a >>= f = f a
  (>>=) = flip ($) . extract

data MyReader r a = MyReader { runMyReader :: r -> a } deriving (Functor)
instance Alt (MyReader r) where
  ra <!> _ = ra
-- instance Plus (MyReader r) where 
--   zero =  
instance Applicative (MyReader r) where
  pure :: a -> MyReader r a
  pure = const >>> MyReader
  (<*>) :: MyReader r (a -> b) -> MyReader r a -> MyReader r b
  MyReader ff <*> MyReader g = MyReader (ff <*> g) 
type instance Key (MyReader r) = r
instance Keyed (MyReader r) where
  mapWithKey :: (Key (MyReader r) -> a -> b) -> MyReader r a -> MyReader r b
  mapWithKey ff rg = MyReader ff <*> rg 
instance Extend (MyReader r) where 
  duplicated :: MyReader r a -> MyReader r (MyReader r a)
  duplicated = const >>> MyReader 
instance Distributive (MyReader r) where 
  distribute :: Functor f => f (MyReader r a) -> MyReader r (f a)
  distribute = fmap runMyReader >>> distribute >>> MyReader
instance Representable (MyReader r) where
  type Rep (MyReader r) = r
  tabulate :: (r -> a) -> MyReader r a
  tabulate = MyReader
  index :: MyReader r a -> r -> a
  index = runMyReader



readerToProxy :: MyReader Void ~> MyProxy
readerToProxy _ = MyProxy

proxyToReader :: MyProxy ~> MyReader Void
proxyToReader _ = MyReader absurd

-- Easiest Adjunction in Haskell Zero -| MyProxy
-- it's really V1 -| U1
instance Adjunction Zero MyProxy where 
  unit :: a -> MyProxy (Zero a)
  unit _ = MyProxy 
  counit :: Zero (MyProxy a) -> a 
  counit (Zero v) = absurd v
  leftAdjunct :: (Zero a -> b) -> a -> MyProxy b
  leftAdjunct _ _ = MyProxy 
  rightAdjunct :: (a -> MyProxy b) -> Zero a -> b
  rightAdjunct _ (Zero v) = absurd v

-- Instance already defined in Data.Functor.Adjunction
-- instance Adjunction V1 U1 where 
--   unit :: a -> U1 (V1 a)
--   unit _ = U1
--   counit :: V1 (U1 a) -> a 
--   counit v = case v of {}
--   leftAdjunct :: (V1 a -> b) -> a -> U1 b
--   leftAdjunct _ _ = U1
--   rightAdjunct :: (a -> U1 b) -> V1 a -> b
--   rightAdjunct _ v = case v of {}

-- Using tuple section ((,) ()) directly

-- Adjunction between ((,) ()) and MyIdentity
-- It's really MyIdentity -| MyIdentity 
-- instance Adjunction ((,) ()) MyIdentity where 
--   unit :: a -> MyIdentity ((), a)
--   unit x = MyIdentity ((), x)
--   counit :: ((), MyIdentity a) -> a
--   counit ((), MyIdentity x) = x
--   leftAdjunct :: (((), a) -> b) -> a -> MyIdentity b
--   leftAdjunct f x = MyIdentity $ f ((), x)
--   rightAdjunct :: (a -> MyIdentity b) -> ((), a) -> b
--   rightAdjunct f = fmap f >>> snd >>> extract

instance Adjunction MyIdentity MyIdentity where 
  unit :: a -> MyIdentity (MyIdentity a)
  unit = MyIdentity >>> MyIdentity 
  counit :: MyIdentity (MyIdentity a) -> a
  counit = extract >>> extract 
  leftAdjunct :: (MyIdentity a -> b) -> a -> MyIdentity b
  leftAdjunct f = MyIdentity >>> f >>> MyIdentity 
  rightAdjunct :: (a -> MyIdentity b) -> MyIdentity a -> b
  rightAdjunct f = extract >>> f >>> extract 

newtype MyWriter r a = MyWriter (r, a) deriving (Show, Eq, Functor)

instance Adjunction (MyWriter r) (MyReader r) where 
  unit :: a -> MyReader r (MyWriter r a)
  unit a = MyReader (MyWriter . (, a))
  counit :: MyWriter r (MyReader r a) -> a
  counit (MyWriter (r, MyReader f)) = f r 
  leftAdjunct :: (MyWriter r a -> b) -> a -> MyReader r b
  leftAdjunct f a = MyReader (\r -> f (MyWriter (r, a)))
  rightAdjunct :: (a -> MyReader r b) -> MyWriter r a -> b
  rightAdjunct f (MyWriter (r, a)) = runMyReader (f a) r

readerToId :: ((->) ()) ~> MyIdentity 
readerToId ff = MyIdentity (ff ())

idToReader :: MyIdentity ~> ((->) ())
idToReader (MyIdentity a) = const a  

-- The Universal Initial/Terminal Adjunction
-- instance Adjunction Zero MyProxy where
--   unit _ = MyProxy
--   counit (Zero v) = absurd v
--   leftAdjunct _ _ = MyProxy
--   rightAdjunct _ (Zero v) = absurd v

sampleMyProxy :: MyProxy Int 
sampleMyProxy = MyProxy
-- 
newtype DeltaF a = DeltaF (a, a) deriving (Show, Eq, Functor)
instance Extend DeltaF where
  duplicated :: DeltaF a -> DeltaF (DeltaF a)
  duplicated (DeltaF (x, y)) = DeltaF (DeltaF (x, y), DeltaF (y, y))
instance Comonad DeltaF where
  extract :: DeltaF a -> a
  extract (DeltaF (x, _)) = x
  duplicate :: DeltaF a -> DeltaF (DeltaF a)
  duplicate = duplicated
type instance Key DeltaF = Bool
instance Keyed DeltaF where
  mapWithKey f (DeltaF (x, y)) = DeltaF (f True x, f False y)
instance Lookup DeltaF where
  lookup True (DeltaF (x, _)) = Just x
  lookup False (DeltaF (_, y)) = Just y
instance Distributive DeltaF where
  distribute :: Functor f => f (DeltaF a) -> DeltaF (f a)
  distribute = (fmap (\(DeltaF (x, _)) -> x) &&& fmap (\(DeltaF (_, y)) -> y)) >>> DeltaF

instance Representable DeltaF where
  type Rep DeltaF = Bool
  tabulate f = DeltaF (f True, f False)
  index (DeltaF (x, _)) True = x
  index (DeltaF (_, y)) False = y

instance Applicative DeltaF where
  pure = (id &&& id) >>> DeltaF
  DeltaF (f, g) <*> DeltaF (x, y) = DeltaF (f x, g y)

sampleDeltaF :: DeltaF Int 
sampleDeltaF = DeltaF (1, 2)


-- The True Adjunction for DeltaF!
-- curryDelta :: (DeltaF a -> b) -> (a -> (Bool -> b))

-- data V1 a -- 0 Constructors!
-- instance Functor V1 where
--   fmap _ z = case z of {}
-- 2. UnitF a in GHC (U1 a)
-- In GHC.Generics, UnitF a is literally called U1 a (The Unit type constructor):

-- haskell
-- data U1 a = U1
-- instance Functor U1 where
--   fmap _ U1 = U1

data UnitF a = UnitF () deriving (Show, Eq, Functor)

newtype DoubleIdentity a = DoubleIdentity (Compose MyIdentity MyIdentity a)
  deriving stock (Show, Eq)
  deriving newtype Functor

-- Prove that DoubleIdentity and MyIdentity are equivalent Functors. 
doubleToSingle :: DoubleIdentity ~> MyIdentity
doubleToSingle = coerce 

foldr' :: Foldable f => (a -> b -> b) -> b -> f a -> b 
-- foldr' g b as =  appEndo (foldMap (g >>> Endo) as) b
foldr' g =  flip (foldMap (g >>> Endo) >>> appEndo) 


data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Functor)

instance Foldable Tree where 
  fold Empty = mempty 
  fold (Leaf a) = a 
  fold (Node l a r) = fold l <> a <> fold r 
  foldMap f = fmap f >>> fold
  

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)} deriving (Functor)
instance Applicative Parser where 
  pure :: a -> Parser a  
  pure x = Parser (\s -> Just (s, x))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
  (<*>) (Parser f) (Parser g) = Parser (\s -> do 
    (s', f')  <- f s
    (s'', x)  <- g s'
    return (s'', f' x))

instance Alternative Parser where 
  empty = Parser (\_ -> Nothing)
  (<|>) :: Parser a -> Parser a -> Parser a 
  (<|>) (Parser f) (Parser g) = Parser (\s -> f s <|> g s) 

char :: Char -> Parser Char 
char c = Parser (\s -> case s of 
  (y:ys) | y == c -> Just (ys, c)
  _ -> Nothing)

string :: String -> Parser String 
string [] = pure ""
string (x:xs) = liftA2 (:) (char x) (string xs)

string' :: String -> Parser String 
string' = foldr fn (pure "") where 
  fn c p = liftA2 (:) (char c) p 

string'' :: String -> Parser String 
string'' = traverse char 

seq :: Applicative f => Maybe (f a) -> f (Maybe a)
seq Nothing = pure Nothing
seq (Just fa) = Just <$> fa

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m 
-- foldMap f container = foldr (\a b -> f a <> b) mempty container
-- foldMap' f  = foldr (\a b -> f a <> b) mempty 
-- foldMap'' f = foldr (\a -> (f a <>)) mempty 
-- foldMap''' f = foldr ((<>) . f) mempty

-- foldr f acc container = appEndo (foldMap (\a -> Endo (f a)) container) acc
-- foldr' f = flip $ appEndo . foldMap (Endo . f)

sumsq :: Int -> Int 
-- sumsq n = sum $ map (^2)[1..n]
-- sumsq n = foldr (\x acc -> acc + x * x) 0 [1..n]
sumsq n = foldr (\x acc -> x * x + acc) 0 [1..n]

-- Define length, which returns the number of elements in a list, 
-- using foldr . Redefine it using foldl.

lengthFoldr :: [a] -> Int 
lengthFoldr list = foldr (\_ acc -> acc + 1) 0 list 

lengthFoldl :: [a] -> Int 
lengthFoldl list = foldl' (\acc _ -> acc + 1) 0 list 

-- Define minlist, which returns the smallest integer in a non-empty list of integers,
-- using foldr1 . Redefine it using foldl1 .

minList :: NonEmpty Int -> Int 
minList = foldr1 min

minList' :: [Int] -> Int
minList' = foldl1 min

-- (4) Define reverse, which reverses a list, using foldr.
reverse :: [a] -> [a]
reverse = foldr (\x acc -> acc ++ [x]) [] 
reverse' :: [a] -> [a]
reverse' list = foldr (\x acc -> acc . (x:)) id list []

reverse'' :: [a] -> [a]
-- reverse'' list = foldr (\x acc -> (x:`) >>> acc) id list []
-- reverse'' list = foldr (\x -> ((x:) >>>)) id list []
reverse'' list = foldr ((>>>) . (:)) id list []

-- (5) Using foldr , define a function remove which takes two strings as its arguments
-- and removes every letter from the second list that occurs in the first list. For
-- example, remove "first" "second" = "econd".

remove :: String -> String -> String
remove xs ys = let s = Set.fromList xs in 
    foldr (\y -> if Set.notMember y s then (y:) else id) [] ys

remove' :: String -> String -> String
remove' xs ys = let s = Set.fromList xs in [y | y <- ys, Set.notMember y s]
    -- foldr (\y -> if Set.notMember y s then (y:) else id) [] ys


    -- (6) Define filter using foldr . Define filter again using foldl.

filter :: Foldable f => (a -> Bool) -> f a -> [a]
filter p = foldr (\x -> if p x then (x:) else id) [] 

filter' :: Foldable f => (a -> Bool) -> f a -> [a]
filter' p = reverse . foldl' (\acc x -> if p x then (x:acc) else acc) []

-- The function remdups removes adjacent duplicates from a list. For example,
-- remdups [1, 2, 2, 3, 3, 3, 1, 1] = [1, 2, 3, 1].
-- Define remdups using foldr . Give another definition using foldl.

remdups :: (Eq a, Foldable f) => f a -> [a] 
remdups = foldr step [] where 
  step x acc@(a:_) | x == a = acc 
  step x acc = x : acc

remdups' :: (Eq a, Foldable f) => f a -> [a]
remdups' = reverse . foldl' step [] where 
  step acc@(a:_) x | x == a = acc 
  step acc x = x : acc 

remdups'' :: (Eq a, Foldable f) => f a -> [a]
remdups'' = toList >>> NE.group >>> fmap NE.head

-- The function inits returns the list of all initial segments of a list. Thus, inits
-- "ate" = [[], "a", "at", "ate"]. Define inits using foldr .

inits :: [a] -> [[a]]
inits = foldr step [[]]  where 
  step x = map (x:) >>> ([]:)

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (x:xs) = [] : map (x:) (inits' xs)

-- sing foldl define approxe n such that
-- approxe n =
-- X
-- i=n
-- i=0
-- 1
-- i!

approxe :: Fractional a => Int -> a 
approxe n = foldr step 1 [1..n] where 
  step x acc = 1 + (acc / fromIntegral x)

approximationsOfE :: [Double]
approximationsOfE = scanl1 (+) ratios where
  facts :: [Integer]
  facts  = 1 : zipWith (*) [1..] facts
  ratios = map ((1 /) . fromIntegral) facts

approxeFoldl :: Int -> Double
approxeFoldl n = fst $ foldl' step (1.0, 1.0) [1..n] 
  where 
    -- acc is (currentSum, currentFactorial)
    -- x is the current number from 1 to n
    step (s, f) x = 
      let nextFact = f * fromIntegral x
      in (s + (1 / nextFact), nextFact)

sae :: Int -> [Double]
sae n = fmap fst $ scanl step (1.0, 1.0) [1..n] where 
    step (s, f) x = let nextFact = f * fromIntegral x 
      in (s + (1 / nextFact), nextFact)

iterate :: (a -> a) -> a -> [a]
iterate f x = scanl step x (repeat ()) where 
  step = f >>> const     

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

rotate :: [a] -> [[a]]
rotate list = take (length list) (scanl step list (repeat ()))  where
  step = shift >>> const 

rotate' :: [a] -> [[a]]
rotate' list = take (length list) $ iterate shift list 

-- https://www.cantab.net/users/antoni.diller/haskell/

succe :: Num a => a -> a
succe i = i + 1

prede :: Num a => a -> a
prede i = i - 1

add :: (Num a, Num b, Eq b) => a -> b -> a
add i 0 = i
add i j = succe (add i (prede j))

mult :: (Num a, Num b, Eq a, Eq b) => a -> b -> a
mult _ 0 = 0
mult i j = add i $ mult i (prede j)

expe :: (Num a, Num b, Eq a, Eq b) => a -> b -> a
expe _ 0 = 1
expe i j = mult i $ expe i (prede j)

foldi :: (a -> a) -> a -> Int -> a
foldi _ q 0 = q
foldi f q i = f (foldi f q (pred i))

add' :: Num a => a -> Int -> a
add' a b = foldi succe a b 

mult' :: (Eq a, Num a) => a -> Int -> a
mult' a b = foldi (add a) 0 b

expe' :: (Num a, Eq a) => a -> Int -> a
expe' i j = foldi (mult i) 1 j 

fact :: Int -> Int
fact n = snd (foldi step (1, 1) n)
  where
    step (idx, prod) = (idx + 1, prod * idx)

domain :: Eq a => [(a, b)] -> [a]
domain  = map fst >>> nub

range :: Eq b => [(a, b)] -> [b]
range = map snd >>> nub
  
compose :: Eq b => [(a, b)] -> [(b, c)] -> [(a, c)]
compose pairs1 pairs2 = [(a, c) | (a, b) <- pairs1, (b', c) <- pairs2, b == b']

inverse :: [(a, b)] -> [(b, a)]
inverse = map swap

reflexive :: Eq a => [(a, a)] -> Bool
reflexive pairs = length cand == length ref where 
  cand = nub $ range pairs ++ domain pairs 
  ref = nub $ map fst $ filter (\(x,y) -> x == y) pairs


