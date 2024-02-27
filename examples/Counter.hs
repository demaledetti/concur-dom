module Main where

import           Control.Applicative    ((<|>))
import           Control.Monad          (forever, void)
import           Control.Monad.State    (execStateT, get, lift, modify)
import           Data.String            (fromString)

import           Concur.Core            (Widget)
import           Concur.DOM             (HTML, delay,
                                         runWidgetInBody, text)

-- Counter widget
-- This widget is stateful, it maintains the current number
-- We can simply use the StateT monad transformer
counter :: Widget HTML ()
counter = void $ flip execStateT (0::Int) $ do
    -- Run forever - incrementing count AND displaying count
    forever $ autoIncrement <|> displayCount
  where
    -- Automatically increment clicks every second. Note the simple synchronous control flow.
    autoIncrement = lift (delay 1000) >> modify (+1)
    -- Get seconds count, and display it using `text` widget
    displayCount = get >>= \count -> lift $ text $ fromString $ show count ++ " seconds"

main :: IO ()
main = void $ runWidgetInBody counter
