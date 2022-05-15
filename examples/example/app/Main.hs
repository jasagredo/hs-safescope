{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Main where

import           Control.Concurrent ( newMVar, MVar, modifyMVar, forkIO )
import           Debug.Trace ( traceEventIO )
import qualified Foreign.C as C (CInt(..))
import           GHC.Stack ( ccsToStrings, getCurrentCCS )
import qualified Language.C.Inline as C ( block, include, verbatim )
import           System.IO.Unsafe ( unsafePerformIO )

C.verbatim "#define _GNU_SOURCE"
C.include "<stdlib.h>"
C.include "<unistd.h>"
C.include "Rts.h"
C.include "Trace.h"
C.include "Capability.h"
C.include "bar.h"

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{-# NOINLINE bar_cnt #-}
bar_cnt :: MVar C.CInt
bar_cnt = unsafePerformIO $ newMVar 0

bar :: C.CInt -> IO ()
bar a1 = do
  c <- modifyMVar bar_cnt (\c -> return (c + 1, c))
  traceEventIO $ "START " <> show c <> " bar"
  traceEventIO . (<>) ("ANN_SCC " <> show c <> " bar ") . show =<< ccsToStrings =<< getCurrentCCS undefined
  [C.block| void {

          pid_t tid = gettid();
          int counter = $(int c);
          ssize_t bufsz = snprintf(NULL, 0, "ANN_TH %d %s %d", counter, "bar", tid);
          char* buf = malloc(bufsz + 1);
          snprintf(buf, bufsz + 1, "ANN_TH %d %s %d", counter, "bar", tid);

          // there is no Haskell capability running this, so just assign it to the MainCapability
          traceUserMsg(&MainCapability, buf);

          free(buf);

          bar_c($(int a1));
          } |]
  traceEventIO $ "STOP " <> show c <> " bar"
  
main :: IO ()
main = do
  forkIO $ print $ fib 35
  bar 10
  print $ fib 34
