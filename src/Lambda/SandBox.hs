{-# LANGUAGE DeriveFunctor #-}
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


module Lambda.SandBox where

import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..), Kleisli (..), (>>>), (^>>), (>>^))
import Control.Category (Category, (.), id)
import Prelude hiding (id, (.))
import Control.Applicative (Alternative (..), Const (..))
import Control.Natural (type (:~>) (..), type (~>))
import Control.Monad (MonadPlus (..), (>=>))
import Data.Functor.Yoneda (liftYoneda, runYoneda)
import Data.Profunctor (Profunctor (..), Strong (..))
import Data.Functor.Identity (Identity(..))
import Data.List (isPrefixOf, sortOn, tails)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Lambda (safeHead)
import Safe (tailMay)
import Data.Default (Default (..))
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Writer (tell, MonadWriter, runWriter)
import Data.Monoid (Endo (..), Sum (..))
import Data.Void (Void, absurd)
import Data.Functor.Adjunction (Adjunction (..))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..))
import Data.Functor.Contravariant (Op (..))
import Data.Key (type Key, Keyed(..), Lookup(..))
import Data.Functor.Alt (Alt(..))
import Data.Functor.Plus (Plus(..))
import Control.Comonad (Comonad(..))
import Data.Functor.Extend (Extend(..))

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

data Zero a = Zero Void deriving (Show, Eq)
instance Functor Zero where -- Cannot be Applicative.
  fmap _ (Zero v) = absurd v
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

-- The Terminal Bang Functor (!) - equivalalent to Const ().
data MyProxy a = MyProxy deriving (Show, Eq)
instance Functor MyProxy where -- cannot be a comonad
  fmap _ _ = MyProxy
instance Alt MyProxy where
  _ <!> _ = MyProxy
instance Plus MyProxy where 
  zero = MyProxy 
instance Extend MyProxy where 
  duplicated :: MyProxy a -> MyProxy (MyProxy a)
  duplicated _ = MyProxy
instance Applicative MyProxy where
  pure _ = MyProxy
  _ <*> _ = MyProxy
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

newtype MyIdentity a = MyIdentity a deriving (Show, Eq)
instance Functor MyIdentity where
  fmap f (MyIdentity a) = MyIdentity (f a)
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

data MyReader r a = MyReader { runMyReader :: r -> a }
instance Functor (MyReader r) where
  fmap f (MyReader g) = MyReader $ g >>> f
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

newtype RAProxy a =  RAProxy (Void, a) deriving (Show, Eq)
instance Functor RAProxy where
  fmap f (RAProxy pair) = RAProxy $ f <$> pair

instance Adjunction RAProxy MyProxy where 
  unit :: a -> MyProxy (RAProxy a)
  unit _ = MyProxy
  counit :: RAProxy (MyProxy a) -> a
  counit (RAProxy (v, _)) = absurd v
  leftAdjunct :: (RAProxy a -> b) -> a -> MyProxy b
  leftAdjunct _ _ = MyProxy
  rightAdjunct :: (a -> MyProxy b) -> RAProxy a -> b
  rightAdjunct _ (RAProxy (v, _)) = absurd v

readerToId :: ((->) ()) ~> Identity 
readerToId ff = Identity (ff ())

idToReader :: Identity ~> ((->) ())
idToReader (Identity a) = const a  

-- The Universal Initial/Terminal Adjunction
-- instance Adjunction Zero MyProxy where
--   unit _ = MyProxy
--   counit (Zero v) = absurd v
--   leftAdjunct _ _ = MyProxy
--   rightAdjunct _ (Zero v) = absurd v

sampleMyProxy :: MyProxy Int 
sampleMyProxy = MyProxy
-- 
newtype DeltaF a = DeltaF (a, a) deriving (Show, Eq)
instance Functor DeltaF where 
  fmap f (DeltaF (x, y)) = DeltaF (f x, f y)

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

data UnitF a = UnitF () deriving (Show, Eq)
instance Functor UnitF where
  fmap _ (UnitF ()) = UnitF ()

-- pure :: a -> fa 
-- fmap1 :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c
-- fmap2 f fa fb = f <$> fa <*> fb 
-- fmap2 f fa fb = pure f <*> fa <*> fb 

-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- fmap3 f fa fb fc = f <$> fa <*> fb <*> fc
-- fmap3 f fa fb fc = pure f <*> fa <*> fb <*> fc 
