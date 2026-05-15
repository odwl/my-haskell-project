module ExercismTest (exercismSuite) where

import Exercism.AnagramTest (anagramTests)
import Exercism.BobTest (bobTests)
import Exercism.ClockTest (clockTests)
import Exercism.PangramTest (pangramTests)
import Exercism.ReverseStringTest (reverseStringTests)
import Exercism.LuhnTest (luhnTests)
import Exercism.ZipperTest (zipperTests)
import Exercism.ParallelTest (parallelTests)
import Test.Tasty (TestTree, testGroup)

exercismSuite :: TestTree
exercismSuite =
  testGroup
    "Exercism"
    [ anagramTests,
      reverseStringTests,
      pangramTests,
      bobTests,
      clockTests,
      zipperTests,
      luhnTests,
      parallelTests
    ]
