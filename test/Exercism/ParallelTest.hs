{-# LANGUAGE OverloadedStrings #-}

module Exercism.ParallelTest (parallelTests) where

import Data.Map (empty, fromList, lookup, singleton)
import Data.Text (concat)
import Exercism.Parallel (frequency)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude hiding (concat, lookup)

parallelTests :: TestTree
parallelTests =
  testGroup
    "Parallel Letter Frequency"
    [ testCase "no texts mean no letters" $
        frequency 1 [] @?= empty,
      testCase "one letter" $
        frequency 1 ["a"] @?= singleton 'a' 1,
      testCase "case insensitivity" $
        frequency 1 ["aA"] @?= singleton 'a' 2,
      testCase "many empty texts still mean no letters" $
        frequency 1 (replicate 10000 "  ") @?= empty,
      testCase "many times the same text gives a predictable result" $
        frequency 1 (replicate 1000 "abc")
          @?= fromList [('a', 1000), ('b', 1000), ('c', 1000)],
      testCase "punctuation doesn't count" $
        lookup ',' (frequency 1 [odeAnDieFreude]) @?= Nothing,
      testCase "numbers don't count" $
        lookup '1' (frequency 1 ["Testing, 1, 2, 3"]) @?= Nothing,
      testCase "all three anthems, together, 1 worker" $ testAllAnthems 1,
      testCase "all three anthems, together, 4 workers" $ testAllAnthems 4
    ]
  where
    odeAnDieFreude =
      concat -- Poem by Friedrich Schiller.
        [ "Freude schöner Götterfunken", -- The corresponding music is
          "Tochter aus Elysium,", -- the European Anthem.
          "Wir betreten feuertrunken,",
          "Himmlische, dein Heiligtum!",
          "Deine Zauber binden wieder",
          "Was die Mode streng geteilt;",
          "Alle Menschen werden Brüder,",
          "Wo dein sanfter Flügel weilt."
        ]
    starSpangledBanner =
      concat -- American national anthem
        [ "O say can you see by the dawn's early light,",
          "What so proudly we hailed at the twilight's last gleaming,",
          "Whose broad stripes and bright stars through the perilous fight,",
          "O'er the ramparts we watched, were so gallantly streaming?",
          "And the rockets' red glare, the bombs bursting in air,",
          "Gave proof through the night that our flag was still there;",
          "O say does that star-spangled banner yet wave,",
          "O'er the land of the free and the home of the brave?"
        ]
    wilhelmus =
      concat -- Dutch national anthem
        [ "Wilhelmus van Nassouwe",
          "ben ik, van Duitsen bloed,",
          "den vaderland getrouwe",
          "blijf ik tot in den dood.",
          "Een Prinse van Oranje",
          "ben ik, vrij, onverveerd,",
          "den Koning van Hispanje",
          "heb ik altijd geëerd."
        ]
    anthems = [odeAnDieFreude, starSpangledBanner, wilhelmus]

    testAllAnthems n = do
      let frequencies = frequency n anthems
      lookup 'a' frequencies @?= Just 49
      lookup 't' frequencies @?= Just 56
      lookup 'ü' frequencies @?= Just 2
