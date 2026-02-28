module ExercismTest (exercismSuite) where

import Exercism.AnagramTest (anagramTests)
import Exercism.BobTest (bobTests)
import Exercism.PangramTest (pangramTests)
import Exercism.ReverseStringTest (reverseStringTests)
import Test.Tasty (TestTree, testGroup)

exercismSuite :: TestTree
exercismSuite =
  testGroup
    "Exercism"
    [ anagramTests,
      reverseStringTests,
      pangramTests,
      bobTests
    ]
