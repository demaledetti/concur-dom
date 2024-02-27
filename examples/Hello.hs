module Main where

import Concur.Core (Widget)
import Concur.DOM (HTML, runWidgetInBody, text)

hello :: Widget HTML ()
hello = text "hello world"

main :: IO ()
main = runWidgetInBody hello
