module Concur.DOM.FFI
   ( windowAddEventListener
   ) where

import           Control.Monad.IO.Class (liftIO)
import           GHC.JS.Foreign.Callback (Callback)
import           GHC.JS.Prim (JSVal)
import GHCJS.Types (jsval)

import           GHCJS.DOM (currentWindowUnchecked)
import           GHCJS.DOM.EventTarget (addEventListener)
import           GHCJS.DOM.Types (EventListener(..), toAddEventListenerOptionsOrBool)


windowAddEventListener :: String -> Callback (JSVal -> IO ()) -> IO ()
windowAddEventListener eventName callback = liftIO $ do
    window <- currentWindowUnchecked
    flip (addEventListener window eventName) (toAddEventListenerOptionsOrBool False) . Just . EventListener $ jsval callback
