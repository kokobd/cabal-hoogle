module Hoogle.Cabal.Logger
  ( Logger,
    cmapLogger,
    stdoutLogger,
    logWith,
    module Colog.Core,
  )
where

import Colog.Core
import Data.Time (getCurrentTime)
import Control.Monad.IO.Class

type Logger msg = LogAction IO (WithSeverity msg)

cmapLogger :: (a -> b) -> Logger b -> Logger a
cmapLogger f = cmap (fmap f)

stdoutLogger :: Show msg => Logger msg
stdoutLogger = LogAction $ \msg -> do
  -- cmap showMsg logStringStdout
  now <- getCurrentTime
  putStrLn $ showMsg now msg
  where
    showMsg time (WithSeverity msg severity) =
      "["
        ++ show severity
        ++ "]["
        ++ show time
        ++ "] "
        ++ show msg

logWith :: MonadIO m => Logger msg -> Severity -> msg -> m ()
logWith logger severity msg = liftIO $ unLogAction logger (WithSeverity msg severity)
