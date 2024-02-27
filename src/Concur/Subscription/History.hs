-----------------------------------------------------------------------------
-- |
-- Module      :  Concur.Subscription.History
-- Copyright   :  (C) 2016-2017 David M. Johnson. Adapted by Anupam Jain.
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  Anupam Jain
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Concur.Subscription.History
  ( getCurrentURI
  , pushURI
  , replaceURI
  , backURI
  , forwardURI
  , goURI
  , uri
  , URI (..)
  ) where

import           Data.JSString

import           Control.Concurrent.STM     (atomically)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.MonadSTM           (MonadSTM (liftSTM))

import           GHC.JS.Foreign.Callback    (asyncCallback, Callback)
import           Network.URI            hiding (path)
import           System.IO.Unsafe

import           Concur.Core

-- | Retrieves current URI of page
getCurrentURI :: IO URI
{-# INLINE getCurrentURI #-}
getCurrentURI = getURI

-- | Retrieves current URI of page
getURI :: IO URI
{-# INLINE getURI #-}
getURI = do
  URI <$> pure mempty
      <*> pure Nothing
      <*> do Prelude.drop 1 . unpack <$> getPathName
      <*> do unpack <$> getSearch
      <*> pure mempty

-- | Pushes a new URI onto the History stack
pushURI :: URI -> IO ()
{-# INLINE pushURI #-}
pushURI uri = pushStateNoModel uri { uriPath = path } >> atomically (notify chan uri)
  where
    path | uriPath uri == mempty = "/"
         | otherwise = uriPath uri

-- | Replaces current URI on stack
replaceURI :: URI -> IO ()
{-# INLINE replaceURI #-}
replaceURI uri = replaceTo' uri { uriPath = path } >> atomically (notify chan uri)
  where
    path | uriPath uri == mempty = "/"
         | otherwise = uriPath uri

-- | Navigates backwards
backURI :: IO ()
{-# INLINE backURI #-}
backURI = backURI'

-- | Navigates forwards
forwardURI :: IO ()
{-# INLINE forwardURI #-}
forwardURI = forwardURI'

-- | Jumps to a specific position in history
goURI :: Int -> IO ()
{-# INLINE goURI #-}
goURI n = goURI' n

chan :: Notify URI
{-# NOINLINE chan #-}
chan = unsafePerformIO newNotifyIO

-- | Captures `popState` events, from the History API
uri :: Monoid v => IO (Widget v URI)
uri = do
  liftIO $ onPopState =<< do
    asyncCallback $ do
      getURI >>= atomically . notify chan
  return $ liftSTM $ await chan

foreign import javascript unsafe "(($1) => { return window.history.go($1); })"
  goURI' :: Int -> IO ()

foreign import javascript unsafe "window.history.back"
  backURI' :: IO ()

foreign import javascript unsafe "window.history.forward"
  forwardURI' :: IO ()

foreign import javascript unsafe "(() => { return window.location.pathname; })"
  getPathName :: IO JSString

foreign import javascript unsafe "(() => { return window.location.search; })"
  getSearch :: IO JSString

foreign import javascript unsafe "(($1) => { return window.addEventListener('popstate', $1); })"
  onPopState :: Callback (IO ()) -> IO ()

foreign import javascript unsafe "(($1) => { return window.history.pushState(null, null, $1); })"
  pushStateNoModel' :: JSString -> IO ()

foreign import javascript unsafe "(($1) => { return window.history.replaceState(null, null, $1); })"
  replaceState' :: JSString -> IO ()

pushStateNoModel :: URI -> IO ()
{-# INLINE pushStateNoModel #-}
pushStateNoModel u = do
  pushStateNoModel' . pack . show $ u
  atomically $ notify chan u

replaceTo' :: URI -> IO ()
{-# INLINE replaceTo' #-}
replaceTo' u = do
  replaceState' . pack . show $ u
  atomically $ notify chan u
