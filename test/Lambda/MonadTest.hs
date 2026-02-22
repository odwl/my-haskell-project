module Lambda.MonadTest (monadTests) where

import Test.Tasty
import Test.Tasty.QuickCheck

monadTests :: TestTree
monadTests =
  testGroup
    "Monad & Generators"
    [ testProperty "genEven" prop_genEven,
      testProperty "genEvenBetter" prop_gen_2_100,
      testProperty "genUser" prop_genUser,
      testProperty "fullName" prop_fullName
    ]

randomNum :: Gen Int
randomNum = choose (1, 50)

genEvenNum :: Gen Int
genEvenNum = fmap (\x -> 2 * x) randomNum

genEvenNumBetter :: Gen Int
-- genEvenNumBetter = choose (2, 100)
-- genEvenNumBetter = liftA2 (+) randomNum randomNum
genEvenNumBetter = do
  x <- randomNum -- "Draw" the first number
  y <- randomNum -- "Draw" the second number
  return (x + y) -- Add them and put the result back in the Gen box

prop_genEven :: Property
prop_genEven = forAll genEvenNum even

prop_gen_2_100 :: Property
prop_gen_2_100 = forAll genEvenNumBetter (\x -> x <= 100)

data User = User String Int
  deriving (Show)

validNames :: [String]
validNames = ["Alice", "Bob", "Charlie"]

genUser :: Gen User
genUser = do
  name <- elements validNames
  age <- choose (18, 99)
  pure (User name age)

prop_genUser :: Property
prop_genUser = forAll genUser $ \(User name age) ->
  elem name validNames && age >= 18 && age <= 99

firstName :: Maybe String
firstName = Just "Alice"

lastName :: Maybe String
lastName = Just "Smith"

fullName :: Maybe String
fullName = do
  fName <- firstName
  lName <- lastName
  pure $ unwords [fName, lName]

prop_fullName :: Property
prop_fullName = property $ fullName == Nothing || fullName == Just "Alice Smith"