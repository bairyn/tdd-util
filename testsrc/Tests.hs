module Tests
    ( tests
    ) where

import Test.Util.Framework

import qualified Test.Test.Util
import qualified Test.Test.Util.Framework

tests :: [TTest]
tests =
    [ testGroup "Test.Test.Util.tests" $ Test.Test.Util.tests
    , testGroup "Test.Test.Util.Framework.tests" $ Test.Test.Util.Framework.tests
    ]
