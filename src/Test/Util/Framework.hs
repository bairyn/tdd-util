{-# LANGUAGE CPP #-}

-- | "Test.Util.Framework" is a super-module that re-exports other modules
-- pertaining to TDD, so that they can be imported under a single module.
--
-- HUnit's @Test@ type is renamed to 'HTest', and test-framework's to 'TTest'.
-- The same renaming scheme, with the addition that @Q@ is prepended for
-- QuickCheck, has been applied to the following names:
--
--  * 'Test'
--
--  * 'assert'
--
--  * 'State'
--
--  * 'test'
--
-- 'Test.QuickCheck.Property.Result' is renamed to 'SingleResult'; 'reason'
-- in this module cannot be renamed, so it is unfortunately not exported.
--
-- Unfortunately, Haskell's design makes it inconvenient to rename classes.
-- In this module, 'Testable' is not re-exported from any module.
--
-- QuickCheck-2.6's new "Test.QuickCheck.Test.interrupted" is not exported
-- under a name that does not conflict with
-- "Test.QuickCheck.Property.interrupted", from either modules
-- ("Test.QuickCheck.Test" and "Test.QuickCheck"), since its existence would depend on
-- the version of QuickCheck.
module Test.Util.Framework
    ( module Test.HUnit
    , module Test.QuickCheck
    , module Test.QuickCheck.All
    , module Test.QuickCheck.Arbitrary
    , module Test.QuickCheck.Function
    , module Test.QuickCheck.Gen
    , module Test.QuickCheck.Modifiers
    , module Test.QuickCheck.Monadic
    , module Test.QuickCheck.Poly
    , module Test.QuickCheck.Property
    , module Test.QuickCheck.State
    , module Test.QuickCheck.Test
    , module Test.QuickCheck.Text
    , module Test.Framework
    , module Test.Framework.Providers.HUnit
    , module Test.Framework.Providers.QuickCheck2

    , HTest
    , TTest
    , qAssert
    , QState
    , qTest
    , SingleResult
    ) where

import           System.Random (StdGen)
import           Test.HUnit hiding (Test, Testable, assert, State, test)
import qualified Test.HUnit
#ifdef QUICKCHECK26
import           Test.QuickCheck hiding (Testable, interrupted)
#else
import           Test.QuickCheck hiding (Testable)
#endif /* #ifdef QUICKCHECK26 */
import           Test.QuickCheck.All
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Function
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Monadic hiding (assert)
import qualified Test.QuickCheck.Monadic
import           Test.QuickCheck.Poly
import           Test.QuickCheck.Property hiding (Result(reason))
import qualified Test.QuickCheck.Property
import           Test.QuickCheck.State hiding (State)
import qualified Test.QuickCheck.State
#ifdef QUICKCHECK26
import           Test.QuickCheck.Test hiding (test, interrupted)
#else
import           Test.QuickCheck.Test hiding (test)
#endif /* #ifdef QUICKCHECK26 */
import qualified Test.QuickCheck.Test
import           Test.QuickCheck.Text
import           Test.Framework hiding (Test)
import qualified Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2

-- | Alias for 'Test.HUnit.Test'.
type HTest = Test.HUnit.Test

-- | Alias for 'Test.Framework.Test'.
type TTest = Test.Framework.Test

-- | Alias for 'Test.QuickCheck.Monadic.assert'.
qAssert :: (Monad m) => Bool -> PropertyM m ()
qAssert = Test.QuickCheck.Monadic.assert

-- | Alias for 'Test.QuickCheck.State.State'.
type QState = Test.QuickCheck.State.State

-- | Alias for 'Test.QuickCheck.Test.test'.
qTest :: QState -> (StdGen -> Int -> Prop) -> IO Result
qTest = Test.QuickCheck.Test.test

-- | Alias for 'Test.QuickCheck.Property.Result'.
type SingleResult = Test.QuickCheck.Property.Result
