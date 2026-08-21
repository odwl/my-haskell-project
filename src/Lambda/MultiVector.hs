module Lambda.MultiVector where

-- | Multivector representation and Geometric Algebra operations
-- Define the Multivector type
data Multivector = MV 
  { scalar :: Double
  , e1     :: Double
  , e2     :: Double
  , e12    :: Double
  } deriving (Show, Eq)

-- Helper constructors for basis elements
vec :: Double -> Double -> Multivector
vec x y = MV 0 x y 0

toScalar :: Double -> Multivector
toScalar s = MV s 0 0 0

biv :: Double -> Multivector
biv b = MV 0 0 0 b

e1v, e2v, e12v :: Multivector
e1v  = vec 1 0
e2v  = vec 0 1
e12v = biv 1

makeRotor :: Double -> Multivector
makeRotor teta = MV (cos half) 0 0 (- sin half) where 
  half = teta / 2

reverseMV :: Multivector -> Multivector
reverseMV (MV s x y b) = MV s x y (-b)

instance Num Multivector where 
  (MV sA xA yA bA) + (MV sB xB yB bB) = 
    MV (sA + sB) (xA + xB) (yA + yB) (bA + bB)

  (MV sA xA yA bA) * (MV sB xB yB bB) =
    MV (sA * sB + xA * xB + yA * yB - bA * bB)
       (sA * xB + xA * sB - yA * bB + bA * yB)
       (sA * yB + yA * sB + xA * bB - bA * xB)
       (sA * bB + bA * sB + xA * yB - yA * xB)

  fromInteger n = toScalar (fromInteger n)
  abs (MV s x y b) = toScalar (sqrt (s*s + x*x + y*y + b*b))
  signum (MV 0 0 0 0) = MV 0 0 0 0
  signum (MV s x y b) = MV (s/len) (x/len) (y/len) (b/len) 
    where len = scalar (abs (MV s x y b)) 
  negate (MV s x y b) = MV (-s) (-x) (-y) (-b)

-- | Scalar product: <A B>_0
scalarProd :: Multivector -> Multivector -> Multivector
scalarProd a b = grade 0 (a * b)

-- | Grade projection: extracts homogeneous k-grade part (k = 0, 1, 2)
grade :: Int -> Multivector -> Multivector
grade 0 (MV s _ _ _) = MV s 0 0 0
grade 1 (MV _ x y _) = MV 0 x y 0
grade 2 (MV _ _ _ b) = MV 0 0 0 b
grade _ _            = MV 0 0 0 0

-- | Decompose multivector into its (Grade 0, Grade 1, Grade 2) components
grades :: Multivector -> (Multivector, Multivector, Multivector)
grades (MV s x y b) = (MV s 0 0 0, MV 0 x y 0, MV 0 0 0 b)

-- | Outer / Wedge product: A ^ B
--   Defined via Grade Projection and Geometric Product (*)
--   <A ^ B>_k = < sum_(r+s=k) A_r * B_s >_k
wedge :: Multivector -> Multivector -> Multivector
wedge a b = 
  grade 0 (sa * sb) +
  grade 1 (sa * vb + va * sb) +
  grade 2 (sa * bb + va * vb + ba * sb)
  where
    (sa, va, ba) = grades a
    (sb, vb, bb) = grades b

-- | Clifford scalar product (Positive-definite): <A ~B>_0
--   Guarantees <A ~A>_0 >= 0 for all multivectors A (including bivectors).
cliffordScalar :: Multivector -> Multivector -> Multivector
cliffordScalar a b = grade 0 (a * reverseMV b)

-- | Hestenes Inner Product: A • B 
--   Defined via Grade Difference Projection:
--   <A_r • B_s> = < A_r * B_s >_|r-s|   (for r > 0 and s > 0)
--   <A_r • B_s> = 0                     (if r = 0 or s = 0)
dotHestenes :: Multivector -> Multivector -> Multivector
dotHestenes a b = 
  grade 0 (va * vb + ba * bb) +
  grade 1 (va * bb + ba * vb)
  where 
    (_, va, ba) = grades a
    (_, vb, bb) = grades b

fatDot :: Multivector -> Multivector -> Multivector
fatDot a b = 
  grade 0 (sa * sb + va * vb + ba * bb) +
  grade 1 (sa * vb + va * sb + va * bb + ba * vb) +
  grade 2 (sa * bb + ba * sb)
  where
    (sa, va, ba) = grades a
    (sb, vb, bb) = grades b

-- | Infix Outer / Wedge Product: u ∧ v
infixl 7 ∧
(∧) :: Multivector -> Multivector -> Multivector
(∧) = wedge

-- | Infix Scalar / Dot Product: u · v
infixl 7 ·
(·) :: Multivector -> Multivector -> Multivector
(·) = scalarProd

-- | Infix Clifford Scalar Product: u ∗ v (U+2217)
infixl 7 ∗
(∗) :: Multivector -> Multivector -> Multivector
(∗) = cliffordScalar

-- | Infix Hestenes Inner Product: u • v (U+2022)
infixl 7 •
(•) :: Multivector -> Multivector -> Multivector
(•) = dotHestenes

-- | Infix Fat Dot Product: u ● v (U+25CF)
infixl 7 ●
(●) :: Multivector -> Multivector -> Multivector
(●) = fatDot

-- | Left Contraction: A ⌋ B
--   Defined via Grade Difference (s - r):
--   <A_r ⌋ B_s> = < A_r * B_s >_(s - r)  (for r <= s)
--   <A_r ⌋ B_s> = 0                      (for r > s)
leftContract :: Multivector -> Multivector -> Multivector
leftContract a b = 
  grade 0 (sa * sb + va * vb + ba * bb) +
  grade 1 (sa * vb + va * bb) +
  grade 2 (sa * bb)
  where
    (sa, va, ba) = grades a
    (sb, vb, bb) = grades b

-- | Infix Left Contraction: u ⨼ v (U+2A3C Interior Product / Left Contraction)
infixl 7 ⨼
(⨼) :: Multivector -> Multivector -> Multivector
(⨼) = leftContract

rotateVector :: Multivector -> Double -> Multivector
rotateVector v theta = r * v * reverseMV r
  where 
    r = makeRotor theta
