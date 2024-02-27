{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Concur.DOM.Widgets where

import           Concur.Core
import           Concur.DOM.Run                (HTML, DOMEventHandler, HTMLNodeName)

import           Control.Applicative           ((<|>))
import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM        (STM, atomically)
import           Control.Monad                 (forever, void, when)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Control.Monad.State           (execStateT, get, lift, put)
import           Control.MonadSTM              (MonadSTM (liftSTM))
import           Control.ShiftMap              (ShiftMap, shiftMap)

import           Data.List                     (intercalate)
import           Data.Maybe                    (mapMaybe)
import           Data.String                   (fromString, IsString(..))
import           Data.Time.Clock               (UTCTime, getCurrentTime)

import           GHC.JS.Prim                   (JSVal, fromJSString)
import           GHCJS.DOM                     (currentDocumentUnchecked)
import           GHCJS.DOM.EventM              (mouseClientXY, on, uiKeyCode)
import           GHCJS.DOM.GlobalEventHandlers (click, keyDown)
import qualified Concur.DOM.Helper.Attribute   as A
import qualified Concur.DOM.Helper.Element     as E
import qualified Concur.DOM.Helper.Event       as Ev


-- Global mouse click notifications
-- Sets up the click handler once and then return a Widget that listens to it
documentClickNotifications :: Monoid v => IO (Widget v (Int,Int))
documentClickNotifications = do
  n <- atomically newNotify
  doc <- currentDocumentUnchecked
  _ <- on doc click $ do
    (x, y) <- mouseClientXY
    liftIO $ atomically $ notify n (x,y)
  return $ listenNotify n

-- Global Keyboard notifications
keyboardNotifications :: Monoid v => IO (Widget v Word)
keyboardNotifications = do
  n <- atomically newNotify
  doc <- currentDocumentUnchecked
  _ <- on doc keyDown $ do
    k <- uiKeyCode
    liftIO $ atomically $ notify n k
  return $ listenNotify n

-- Returns a widget which waits for a Notification to happen
listenNotify :: Monoid v => Notify a -> Widget v a
listenNotify = liftSTM . await

-- An example of a completely IO widget
-- Waits for the specified number of milliseconds
delay :: Monoid v => Int -> Widget v ()
delay i = liftIO $ threadDelay (i*1000)

interval :: Monoid v => Int -> IO (Widget v UTCTime)
interval i = do
  n <- atomically newNotify
  -- TODO: Kill Thread at some point. Use Weak TVars for it
  tid <- forkIO $ forever $ threadDelay (i*1000) >> getCurrentTime >>= atomically . notify n
  return $ listenNotify n

-- Text display widget
text :: String -> Widget HTML a
text s = display [E.text s]

-- A clickable button widget
button :: String -> Widget HTML ()
button = button' []

button' :: [A.Attribute] -> String -> Widget HTML ()
button' a s = void $ awaitViewAction $ \n -> [E.button (Ev.click (atomically . notify n) : a) [E.text s]]

-- An Element which can be clicked
clickEl :: HTMLNodeName [A.Attribute] -> [A.Attribute] -> (Ev.MouseEvent -> a) -> [Widget HTML a] -> Widget HTML a
clickEl e attrs onClick children = either id id <$> elEvent Ev.click onClick e attrs (orr children)

-- Handle arbitrary events on an element.
-- Returns Right on child events, and Left on event
elEvent :: (DOMEventHandler -> A.Attribute)
        -> (JSVal -> a)
        -> HTMLNodeName [A.Attribute]
        -> [A.Attribute]
        -> Widget HTML b
        -> Widget HTML (Either a b)
elEvent evt onEvt e attrs w = do
  n <- liftSTM newNotify
  let wEvt = onEvt <$> listenNotify n
  let child = el_ e (evt (atomically . notify n): attrs) w
  fmap Left wEvt <|> fmap Right child

targetValue :: Notify String -> JSVal -> IO ()
targetValue n = atomically . notify n . fromJSString . Ev.inputValue

-- Text input. Returns the contents on keypress enter.
inputEnter :: [A.Attribute] -> Widget HTML String
inputEnter attrs = do
  n <- liftSTM newNotify
  let handleKeypress e = when (Ev.key e == "Enter") $ targetValue n e
  let txt = E.input (Ev.keydown handleKeypress : attrs) []
  effect [txt] $ await n

-- Text input. Returns the contents on every change.
-- This allows setting the value of the textbox, however
--  it suffers from the usual virtual-dom lost focus problem :(
input :: String -> Widget HTML String
input def = do
  n <- liftSTM newNotify
  let txt = E.input [A.value def, Ev.input (targetValue n)] []
  effect [txt] $ await n

-- Text input. Returns the contents on keypress enter.
-- This one does not allow setting the value of the textbox, however
--  this does not suffer from the virtual-dom lost focus problem, as
--  the vdom representation of the textbox never changes
mkInput :: STM (Widget HTML String)
mkInput = do
  n <- newNotify
  let txt = E.input [Ev.input (targetValue n)] []
  return $ effect [txt] $ await n

-- A custom widget. An input field with a button.
-- When the button is pressed, the value of the input field is returned.
-- Note the use of local state to store the input value
inputWithButton :: String -> String -> Widget HTML String
inputWithButton label def = do
  inp <- liftSTM mkInput
  flip execStateT def $ go inp
  where
    -- On text change, we simply update the state, but on button press, we return the current state
    go inp= w inp >>= either (\s -> put s >> go inp) (const get)
    -- Note we put a space between the text and the button. `text` widget is polymorphic and can be inserted anywhere
    w inp = fmap Left (lift inp) <|> lift (text " ") <|> fmap Right (lift $ button label)

-- A Checkbox
checkbox :: Bool -> Widget HTML Bool
checkbox checked = awaitViewAction $ \n -> [E.input [Ev.click (const $ atomically $ notify n (not checked))] []]

-- Generic Element wrapper (single child widget)
el_ :: ShiftMap (Widget HTML) m => HTMLNodeName [A.Attribute] -> [A.Attribute] -> m a -> m a
el_ e attrs = shiftMap (wrapView (e attrs))

-- Generic Element wrapper
el :: (ShiftMap (Widget HTML) m, MultiAlternative m) => HTMLNodeName [A.Attribute] -> [A.Attribute] -> [m a] -> m a
el e attrs = el_ e attrs . orr

-- Utility to easily create class attributes
classList :: [(String, Bool)] -> A.Attribute
classList xs = A.class_ $ fromString classes
  where classes = intercalate " " $ flip mapMaybe xs $ \(s,c) -> if c then Just s else Nothing
