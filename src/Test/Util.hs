{-# LANGUAGE GADTs, TemplateHaskell, DeriveDataTypeable #-}

-- | A module for various useful functions for TDD missing in other libraries.
module Test.Util
    (
    -- * Throwing and catching exceptions
      isExceptionThrown
    , assertThrown
    , assertNotThrown

    -- * Concurrent TDD
    -- TODO: provide a means of testing a monadic property with many threads

    -- * Process timing
    , timeMicroseconds
    , timeoutMicroseconds
    , assertMicroseconds
    , timeoutProcessMicroseconds
    , assertProcessMicroseconds

    -- * Exceptions
    , TestUtilException(..)
    , testUtilExceptionToException
    , testUtilExceptionFromException
    , TimeoutOverflow(..), timeoutOverflow_message, timeoutOverflow_microseconds, timeoutOverflow_inputBound
    , TimeLimitExceeded(..), timelimitExceeded_message, timelimitExceeded_callerName, timelimitExceeded_microseconds
    ) where

import Control.Applicative
--import Control.Concurrent.ParallelIO.Local
import Control.Exception hiding (catch)
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Lens.TH
import Data.Dynamic
import Data.Maybe
import Data.Proxy
import Data.Time.Clock
import System.Exit
import System.Process
import System.Timeout (timeout)
import Text.Printf

import Test.Util.Framework

--- Throwing and catching exceptions ---

-- | Determine whether an exception was caught, and return it if so.
isExceptionThrown :: (Functor m, MonadCatchIO m, Exception e) => m a -> m (Either e a)
isExceptionThrown m = do
    (Right <$> m) `catch` (return . Left)

-- | Assert that an exception is thrown.
--
-- When an exception is not thrown, the input 'String', or otherwise a
-- default string, is output.
--
-- For more control, see the more fundamental 'isExceptionThrown'.
assertThrown :: (Functor m, MonadCatchIO m, Exception e, Show e) => Maybe String -> Proxy e -> m () -> m ()
assertThrown ms ep m = do
    either (\e -> flip const (e `asProxyTypeOf` ep) $ return ()) (const . liftIO $ assertString s) =<< isExceptionThrown m
    where s = fromMaybe "exception NOT thrown" ms

-- | Assert that an exception is not thrown.
--
-- When an exception is thrown, the input function, or a default one, is
-- given the exception and the resulting string is output.
--
-- For more control, see the more fundamental 'isExceptionThrown'.
assertNotThrown :: (Functor m, MonadCatchIO m, Exception e, Show e) => Maybe (e -> String) -> m () -> m ()
assertNotThrown msf m = do
    either (liftIO . assertString . sf) (const $ return ()) =<< isExceptionThrown m
    where sf = fromMaybe (\e -> printf "exception thrown: %s" (show e)) msf


--- Concurrent TDD ---


--- Process timing ---

-- | Time a computation.
timeMicroseconds :: (Monad m, MonadIO m) => m a -> m (a, Integer)
timeMicroseconds m = do
    begin <- liftIO $ getCurrentTime
    a <- m
    end   <- liftIO $ getCurrentTime
    let nomDiffTime :: NominalDiffTime
        nomDiffTime = diffUTCTime end begin
        microsecondsDiff :: Integer
        microsecondsDiff = round $ nomDiffTime * 1000000
    return (a, microsecondsDiff)

-- | Run a computation within an approximate time limit.
--
-- This is currently a wrapper for 'System.Timeout.timeout' that checks for
-- overflows.
timeoutMicroseconds :: Integer -> IO a -> IO (Maybe a)
timeoutMicroseconds us m
    | us <= (fromIntegral (maxBound :: Int)) =
        timeout (fromIntegral us) m
    | otherwise                              =
        throwIO $ TimeoutOverflow Nothing us (fromIntegral (maxBound :: Int))

-- | Assert that a computation runs within an approximate time limit.
--
-- If the computation does not finish within the given time limit, a
-- 'TimeLimitExceeded' exception is thrown.
--
-- For more control, see the more fundamental 'timeoutMicroseconds' function.
assertMicroseconds :: Integer -> IO a -> IO a
assertMicroseconds us m = do
    maybe (throwIO $ TimeLimitExceeded Nothing "assertMicroseconds" us) return =<< timeoutMicroseconds us m

-- | Apply an approximate time limit, from the current time, to a process by
-- its handle.
--
-- If the process finishes approximately within the given time limit, 'Just'
-- its exit code is returned.  Otherwise, it is killed and 'Nothing' is
-- returned.
--
-- This function requires a threaded runtime system to work properly.
timeoutProcessMicroseconds :: Integer -> ProcessHandle -> IO (Maybe ExitCode)
timeoutProcessMicroseconds us ph = do
    (maybe (terminateProcess ph >> return Nothing) (return . Just) =<<) . timeoutMicroseconds us $ do
        waitForProcess ph

-- | Assert that a process finishes within an approximate time limit.
--
-- If the computation does not finish within the given time limit, a
-- 'TimeLimitExceeded' exception is thrown.
--
-- For more control, see the more fundamental 'timeoutProcessMicroseconds' function.
assertProcessMicroseconds :: Integer -> ProcessHandle -> IO ()
assertProcessMicroseconds us ph = do
    maybe (throwIO $ TimeLimitExceeded Nothing "assertProcessMicroseconds" us) (const $ return ()) =<< timeoutProcessMicroseconds us ph

--- Exceptions ---

-- | A class of exceptions for "Tests.Util".
data TestUtilException where                                                                                                                                                               
    TestUtilException :: (Exception e) => e -> TestUtilException
    deriving (Typeable)

instance Show TestUtilException where
    show (TestUtilException e) = show e

instance Exception TestUtilException where

testUtilExceptionToException :: Exception e => e -> SomeException
testUtilExceptionToException = toException . TestUtilException

testUtilExceptionFromException :: Exception e => SomeException -> Maybe e
testUtilExceptionFromException x = do
    (TestUtilException a) <- fromException x
    cast a

-- | 'timeoutMicrosoconds' was invoked with an integer that would cause the
-- input given to 'timeout' to overflow.
data TimeoutOverflow =
    TimeoutOverflow
        { _timeoutOverflow_message      :: Maybe String  -- ^ Optional error message.
        , _timeoutOverflow_microseconds :: Integer       -- ^ Input given to 'timeoutMicroseconds'.
        , _timeoutOverflow_inputBound   :: Integer       -- ^ Maximum bound of 'Int' as an 'Integer'.
        }
    deriving (Typeable, Show, Eq)

instance Exception TimeoutOverflow where
    toException   = testUtilExceptionToException
    fromException = testUtilExceptionFromException

data TimeLimitExceeded =
    TimeLimitExceeded
        { _timelimitExceeded_message      :: Maybe String  -- ^ Optional error message.
        , _timelimitExceeded_callerName   :: String         -- ^ Name of the function that directly threw the exception ('assertMicroseconds', etc.)
        , _timelimitExceeded_microseconds :: Integer       -- ^ The timelimit.
        }
    deriving (Typeable, Show, Eq)

instance Exception TimeLimitExceeded where
    toException   = testUtilExceptionToException
    fromException = testUtilExceptionFromException

makeLenses ''TimeoutOverflow
makeLenses ''TimeLimitExceeded
