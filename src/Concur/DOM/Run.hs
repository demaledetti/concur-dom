{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concur.DOM.Run where

import           Control.Concurrent.STM (atomically)
import           Control.Monad          (void)
import           Control.Monad.Free     (Free (..))

import           Data.Maybe             (fromMaybe)
import           Data.Tree              (Tree(..))

import           GHCJS.DOM (currentDocumentUnchecked)
import           GHCJS.DOM.Types (JSM)
import qualified GHCJS.DOM.Types as DOM
import           GHCJS.DOM.Document (createElement, createTextNode, getBodyUnchecked)
import           GHCJS.DOM.Element (setAttribute)
import           GHCJS.DOM.Node (appendChild)
import           GHC.JS.Foreign.Callback (Callback, syncCallback1, OnBlocked(..), releaseCallback)
import           GHC.JS.Prim (JSVal, toJSString)
import           Concur.Core


foreign import javascript "((o, n) => { nanomorph(o, n, { childrenOnly: true }); })"
  js_morph :: JSVal -> JSVal -> IO ()

foreign import javascript "((o, p, v) => { o[p] = v; })"
  js_setEventProperty :: JSVal -> JSVal -> Callback DOMEventHandler -> IO ()

data DOMNode = DOMElement !String ![DOMAttr]
             | DOMText !String

data DOMAttr = DOMAttr !String !DOMAttrValue

data DOMAttrValue = DOMAttrText !String
                  | DOMAttrEvent !DOMEventHandler

type DOMEventHandler = JSVal -> IO ()
type CleanupActions = [IO ()]

renderDOM :: HTML -> IO (JSVal, CleanupActions)
renderDOM nodes = do
    d <- currentDocumentUnchecked
    e <- createElement d "div"
    (,) <$> DOM.toJSVal e <*> foldMapM (renderNode d (void . appendChild e)) nodes
  where

    renderNode :: DOM.Document -> (forall n . DOM.IsNode n => n -> IO ()) -> Tree DOMNode -> IO CleanupActions
    renderNode d addToParent (Node (DOMElement v as) cs) = do
        e <- createElement d v
        addToParent e
        (<>) <$> foldMapM (renderAttr e) as <*> foldMapM (renderNode d (void . appendChild e)) cs
    renderNode d addToParent (Node (DOMText s) _) =
        createTextNode d s >>= addToParent >> pure mempty

    renderAttr :: DOM.Element -> DOMAttr -> IO CleanupActions
    renderAttr e (DOMAttr n (DOMAttrText v)) = setAttribute e n v >> pure mempty
    renderAttr e (DOMAttr n (DOMAttrEvent cb)) = do
        cb' <- syncCallback1 ThrowWouldBlock cb
        js_setEventProperty (DOM.pToJSVal e) (toJSString n) cb'
        pure [releaseCallback cb']

-- see also: https://github.com/serokell/universum/issues/171
foldMapM :: forall f a m b. (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step pure xs mempty
  where
    -- writing this signature requires ScopedTypeVariables and the forall in foldMapM's signature
    step :: a -> (b -> m b) -> (b -> m b)
    step x r z = f x >>= \y -> r $! z <> y

-- HTML structure
type VNode = Tree DOMNode
type HTML = [VNode]
type HTMLNode = VNode
type HTMLWrapper = HTML -> HTMLNode
type HTMLNodeName attrs = attrs -> HTMLWrapper

runWidgetInBody :: Widget HTML a -> JSM a
runWidgetInBody w = do
  root <- getBodyUnchecked =<< currentDocumentUnchecked
  runWidget w root

runWidget :: (DOM.IsElement e, DOM.PToJSVal e) => Widget HTML a -> e -> JSM a
runWidget (Widget w) root = go [] w
  where
    go :: CleanupActions -> Free (Suspend HTML) a -> IO a
    go cleanups w' = do
        foldMap id cleanups
        case w' of
            Pure a -> putStrLn "WARNING: Application exited: This may have been unintentional!" >> return a
            Free (Suspend io) -> io >>= \ws -> do
                (rendered, cleanups') <- renderDOM $ view ws
                js_morph (DOM.pToJSVal root) rendered
                atomically (fromMaybe (Free $ Suspend $ return ws) <$> cont ws) >>= go cleanups'
