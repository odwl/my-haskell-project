module ExercismTest (exercismSuite) where

import Exercism.AnagramTest (anagramTests)
import Exercism.BobTest (bobTests)
import Exercism.ClockTest (clockTests)
import Exercism.LuhnTest (luhnTests)
import Exercism.PangramTest (pangramTests)
import Exercism.ParallelTest (parallelTests)
import Exercism.ReverseStringTest (reverseStringTests)
import Exercism.ZipperTest (zipperTests)
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
