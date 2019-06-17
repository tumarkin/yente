-----------------------------------------------------------------------------
-- |
-- Module      : Concurrent
-- Updated     : 2018-Jan-22
--
-- Multi-threaded queue processing with progressbar. The full featured
-- implementation `runConcurrent` allows for custom IO configuration functions
-- for each thread, which can be used to allow separate connections to
-- databases, websites, etc). The result processing function can be used to
-- centralize the output, which may be necessary when the threads need to
-- access a shared resource. If that is not necessary, have each thread process
-- results and return ().
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module App.Yente.Core.Concurrent
  ( runConcurrentSimple
  , runConcurrent
  ) where

import           ClassyPrelude
import           Control.Concurrent           (forkIO, ThreadId, threadDelay)
import           System.Console.AsciiProgress


runConcurrent ∷ Int                    -- ^ Number of processed to run
              → IO config              -- ^ Thread config function
              → (config → a → IO b)    -- ^ Queue processing function
              → (b → IO ())            -- ^ Result processing function
              → Vector a               -- ^ Queue
              → IO ()
runConcurrent nConcurrent configF queueProcessor resultProcessor items = do
    when (nConcurrent < 1) (error "No cores specified")
    queue    <- newMVar items
    results  <- replicateM nConcurrent newEmptyMVar
    threadId <- spawnThreads configF queueProcessor queue results

    renderProgressBarUntilComplete resultProcessor (length items) results

runConcurrentSimple ∷ Int         -- ^ Number of processes
                    → (a → IO ()) -- ^ Queue processing function
                    → Vector a         -- ^ Queue
                    → IO ()
runConcurrentSimple nConcurrent queueProcessor =
    runConcurrent nConcurrent (return ()) (const queueProcessor) (const (return ()))

spawnThreads ∷ IO config
             → (config → a → IO b)
             → InputQueue a
             → [ResultMVar b]
             → IO [ThreadId]
spawnThreads configF queueProcessor queue results =
    forM results $ \r → do
      c <- configF
      forkIO (singleThread c queueProcessor queue r)

singleThread ∷ config
             → (config → a → IO b) -- ^ Queue processing function
             → InputQueue a
             → ResultMVar b
             → IO ()
singleThread config queueProcessor q result =
  nextInQueue q >>= \case
    Nothing -> return ()
    Just next -> do
      b <- queueProcessor config next
      putMVar result b
      singleThread config queueProcessor q result

-- Asynchronous queue and counter
type InputQueue a    = MVar (Vector a)
type ResultMVar b    = MVar b

nextInQueue ∷ InputQueue a → IO (Maybe a)
nextInQueue q = do
  queue <- takeMVar q
  case fromNullable queue of
    Nothing -> putMVar q queue >> return Nothing
    Just x  -> putMVar q (tail x) >> return (Just $ head x)


renderProgressBarUntilComplete ∷ (b → IO ()) → Int → [ResultMVar b] → IO ()
renderProgressBarUntilComplete resultProcessor lenQueue rbs = displayConsoleRegions $ do
  pg <- newProgressBar def { pgWidth       = 100
                          , pgOnCompletion = Just "Done: :percent"
                          , pgTotal        = fromIntegral lenQueue
                          }
  loopProgressBar resultProcessor rbs pg

loopProgressBar ∷ (b → IO ()) → [ResultMVar b] → ProgressBar → IO ()
loopProgressBar resultProcessor rbs pg =
  unlessM (isComplete pg) $ do
    finishedResults <- catMaybes <$> mapM tryTakeMVar rbs
    tickNI pg . fromIntegral . length $ finishedResults
    mapM_ resultProcessor finishedResults
    threadDelay $ 10 * 1000
    loopProgressBar resultProcessor rbs pg



