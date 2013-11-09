module Test.Test.Util
    ( tests
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Proxy
import Data.String
import System.IO
import System.Process
import Text.Printf

import Test.Util
import Test.Util.Framework hiding (output)

newtype ShowableIO a = ShowableIO (IO a)

instance Show (ShowableIO a) where
    show = const "<IO *>"

tests :: [TTest]
tests =
    [ testGroup "Throwing and catching exceptions - isExceptionThrown" $
        [ testCase "throwing an exception" $ do
           thrown <- isExceptionThrown $ do
               throwIO $ AssertionFailed "assertion failed"
           when (either (\e -> flip const (e :: AssertionFailed) $ False) (const True) $ thrown) $ do
               assertString "exception NOT thrown"
        , testCase "not throwing an exception" $ do
           thrown <- isExceptionThrown $ do
               return ()
           when (either (\e -> flip const (e :: AssertionFailed) $ True) (const False) $ thrown) $ do
               assertString "exception thrown"
        ]
    , testGroup "Throwing and catching exceptions - assert*Thrown" $
        [ testCase "throwing an exception" $ do
           assertThrown Nothing (Proxy :: Proxy AssertionFailed) $ do
               throwIO $ AssertionFailed "assertion failed"
        , testCase "not throwing an exception" $ do
           assertNotThrown (Nothing :: Maybe (AssertionFailed -> String)) $ do
               return ()
        ]
    , testGroup "isExceptionThrown -> assert*Thrown" $
        [ testProperty "Applying appropriate assert*Thrown given result of isExceptionThrown" . monadicIO $
            forAllM (elements . map ShowableIO $ [throwIO $ AssertionFailed "assertion failed", return ()]) $ \ ~(ShowableIO m) -> do
                run $ do
                    either (\e -> flip const (e :: AssertionFailed) $
                                      assertThrown Nothing (let p = Proxy in flip const (e `asProxyTypeOf` p) $ (p :: Proxy AssertionFailed)) m)
                           (\ ~() -> assertNotThrown (Nothing :: Maybe (AssertionFailed -> String)) m)
                       =<< (isExceptionThrown m :: IO (Either AssertionFailed ()))
        ]
    -- TODO: Once a means of testing a monadic property with many threads is implemented, increase maxDelayTime from 30ms to 600ms; tests may not be as reliable until then.
    , testGroup "Timed tests" $
        [ testGroup "timeMicroseconds" $
            let time process m = do
                    forAllM (choose (0, maxDelayTime)) $ \actualUs -> do
                        us <- run . (snd <$>) . timeMicroseconds $ m actualUs
                        qAssert $ (abs $ us - actualUs) <= if process then processCushion else cushion
            in  [ testProperty "timeMicroseconds is accurate for random sleep times within 10ms" . monadicIO $
                    time True $ \actualUs -> do
                        ph <- createSleepProcess (printf "%f" ((fromIntegral actualUs  :: Double) / 1000000))
                        void $ waitForProcess ph
                , testProperty "timeMicroseconds is accurate for random delay times by timeout within 10ms" . monadicIO $
                    time False $ \actualUs -> do
                        threadDelay (fromIntegral actualUs)
                ]
        , testGroup "timeoutMicroseconds behaves like timeout and throws exceptions appropriately" $
            [ testCase "timeoutMicroseconds overflow" $
                assertThrown Nothing (Proxy :: Proxy TimeoutOverflow) $ do
                    let us :: Integer
                        us = (fromIntegral (maxBound :: Int)) + 1
                    (fromMaybe () <$>) . timeoutMicroseconds us $ return ()
            , testCase "timeoutMicroseconds non-overflow" $
                assertNotThrown (Nothing :: Maybe (TimeoutOverflow -> String)) $ do
                    let us :: Integer
                        us = 2
                    (fromMaybe () <$>) . timeoutMicroseconds us $ return ()
            , testProperty "waiting for a random amount of time from 0ms - 600ms; measured time difference is less than 10ms" . monadicIO $
                forAllM (choose (0, maxDelayTime)) $ \us -> do
                    actualUs <- run . (snd <$>) . timeMicroseconds $ do
                        void . timeoutMicroseconds us $ do
                            forever $ threadDelay 1
                    qAssert $ (abs $ us - actualUs) <= cushion
            ]
        ]
    -- TODO: Once a means of testing a monadic property with many threads is implemented, increase maxDelayTime from 30ms to 600ms; tests may not be as reliable until then.
    , testGroup "assertMicroseconds" $
        [ testProperty "timeoutMicroseconds -> assertMicroseconds (assert*Thrown)" . monadicIO $
            -- (us, cap); cap from us is greater than cushion
            forAllM (choose (0, maxDelayTime)) $ \us -> forAllM (choose (0, maxDelayTime) `suchThat` \cap -> abs (cap - us) > cushion) $ \cap -> do
                run $ do
                    let sleepM = threadDelay (fromIntegral us)
                    killed <- maybe True (const False) <$> timeoutMicroseconds cap sleepM
                    if killed
                        then do
                            assertThrown Nothing (Proxy :: Proxy TimeLimitExceeded) $ do
                                assertMicroseconds cap sleepM
                        else do
                            assertNotThrown (Nothing :: Maybe (TimeLimitExceeded -> String)) $ do
                                assertMicroseconds cap sleepM
        ]
    , testGroup "timeoutProcessMicroseconds behaves like timeoutMicroseconds and throws exceptions appropriately" $
        [ testCase "timeoutProcessMicroseconds overflow" $
            assertThrown Nothing (Proxy :: Proxy TimeoutOverflow) $ do
                let us :: Integer
                    us = (fromIntegral (maxBound :: Int)) + 1
                (fromMaybe () <$>) . timeoutMicroseconds us $ return ()
        , testCase "timeoutProcessMicroseconds non-overflow" $
            assertNotThrown (Nothing :: Maybe (TimeoutOverflow -> String)) $ do
                let us :: Integer
                    us = 2
                (fromMaybe () <$>) . timeoutMicroseconds us $ return ()
        , testProperty "random sleep times and timeouts; return value is appropriate (NB: requires -threaded to work properly)" . monadicIO $
            forAllM (choose (0, maxDelayTime)) $ \us -> forAllM (choose (0, maxDelayTime) `suchThat` \cap -> abs (cap - us) > cushion) $ \cap -> do
                killed <- run $ do
                    ph <- createSleepProcess (printf "%f" ((fromIntegral us  :: Double) / 1000000))
                    maybe True (const False) <$> timeoutProcessMicroseconds cap ph
                qAssert $ killed == (cap < us)
        ]
    -- TODO: Once a means of testing a monadic property with many threads is implemented, increase maxDelayTime from 30ms to 600ms; tests may not be as reliable until then.
    , testGroup "assertProcessMicroseconds" $
        [ testProperty "timeoutProcessMicroseconds -> assertProcessMicroseconds (assert*Thrown)" . monadicIO $
            -- (us, cap); cap from us is greater than cushion
            forAllM (choose (0, maxDelayTime)) $ \us -> forAllM (choose (0, maxDelayTime) `suchThat` \cap -> abs (cap - us) > cushion) $ \cap -> do
                run $ do
                    let sleepM = createSleepProcess (printf "%f" ((fromIntegral us  :: Double) / 1000000))
                    killed <- maybe True (const False) <$> (timeoutProcessMicroseconds cap =<< sleepM)
                    if killed
                        then do
                            assertThrown Nothing (Proxy :: Proxy TimeLimitExceeded) $ do
                                assertProcessMicroseconds cap =<< sleepM
                        else do
                            assertNotThrown (Nothing :: Maybe (TimeLimitExceeded -> String)) $ do
                                assertProcessMicroseconds cap =<< sleepM
        ]
    , mutuallyExclusive . testGroup "catching output" $ redirectTests
    ]
    where cushion :: Integer
          cushion = 20000
          -- Cushion with process creation; creating process can take time.
          processCushion :: Integer
          processCushion = 200000
          maxDelayTime :: Integer
          --maxDelayTime = 600000
          maxDelayTime = 40000
          createSleepProcess :: String -> IO ProcessHandle
          createSleepProcess arg1 = do
            ((Just _), (Just _), (Nothing), ph) <- createProcess $ CreateProcess
                { cmdspec      = RawCommand "sleep" [arg1]
                , cwd          = Nothing
                , env          = Nothing
                , std_in       = CreatePipe
                , std_out      = CreatePipe
                , std_err      = Inherit
                , close_fds    = True
                , create_group = False
                }
            return ph

-- | Group with 'mutuallyExclusive'.
redirectTests :: [TTest]
redirectTests =
    [ testCase "catchStdout catches the output of Hello World" $ do
        output <- snd <$> catchStdout helloWorld
        fromString "Hello, World!\n" @=? output
    , testCase "catchStderr catches the output of a program that prints to stderr" $ do
        output <- snd <$> catchStderr helloWorldErr
        fromString "Hello, World!\n" @=? output
    , testCase "no stderr is received from hellowWorld" $ do
        output <- snd <$> catchStderr helloWorld
        fromString "" @=? output
    , testCase "catchStdout behaves correctly with exceptions, a test in the middle of other redirectHandle tests" $ do
        assertThrown Nothing (Proxy :: Proxy IOError) $ do
            output <- snd <$> ((throwIO . userError $ "User error!") >> catchStdout helloWorld)
            fromString "This is not the output." @=? output
    , testCase "no stdout is received from hellowWorldErr" $ do
        output <- snd <$> catchStdout helloWorldErr
        fromString "" @=? output
    , testCase "redirectHandle stdout works the same as catchStdout" $ do
        output <- snd <$> redirectHandle stdout "<stdout>" helloWorld
        fromString "Hello, World!\n" @=? output
    , testCase "redirectHandle stderr works the same as catchStderr" $ do
        output <- snd <$> redirectHandle stderr "<stderr>" helloWorldErr
        fromString "Hello, World!\n" @=? output
    ]
    where helloWorld :: IO ()
          helloWorld = putStrLn "Hello, World!"
          helloWorldErr :: IO ()
          helloWorldErr = hPutStrLn stderr "Hello, World!"
