{-# LANGUAGE MultiWayIf        #-}
module Main where

import           Control.Applicative    ((<|>))
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)

import qualified Concur.DOM.Helper.Attribute as A
import qualified Concur.DOM.Helper.Element   as E

import           System.Random          as R

import           Concur.Core            (Widget)
import           Concur.DOM             (HTML, button, el, el_,
                                         inputEnter,
                                         runWidgetInBody, text)

-- Hi/Lo Game. Demonstrates simple architecture of a Concur app.
-- Also a good demonstration of how Concur makes IO effects safe at widget transitions (the random number generation).
hilo :: Widget HTML ()
hilo = do
  el_ E.h1 [] (text "I'm thinking of a number between 1 and 100")
  <|> (liftIO (R.randomRIO (1,100)) >>= go)
  where
    go :: Int -> Widget HTML ()
    go n = do
      guess <- read <$> el E.div []
        [ text "Try to guess: "
        , inputEnter [A.autofocus "autofocus"]
        ]
      if | guess <  n -> el_ E.div [] (text $ show guess ++ " - Go High!") <|> go n
         | guess >  n -> el_ E.div [] (text $ show guess ++ " - Go Low!") <|> go n
         | otherwise  -> el_ E.div [] (text $ "You guessed it! The answer was " ++ show n)
             <|> (button "Play again" >> hilo)

main :: IO ()
main = void $ runWidgetInBody hilo
