module Concur.DOM.Helper.Element (
    a
  , button
  , div
  , footer
  , h1
  , header
  , hr
  , img
  , input
  , label
  , li
  , p
  , section
  , span
  , strong
  , table
  , text
  , td
  , tr
  , ul
  ) where

import Concur.DOM.Run (DOMAttr(..), DOMNode(..))
import Data.Tree (Tree(..))
import Prelude hiding (div, span)

text :: String -> Tree DOMNode
text s = Node (DOMText s) []

a, button, div, footer, h1, header, hr, img, input,
    label, li, p, section, span, strong, table, td,
    tr, ul :: [DOMAttr] -> [Tree DOMNode] -> Tree DOMNode
a = _e "a"
button = _e "button"
div = _e "div"
footer = _e "footer"
h1 = _e "h1"
header = _e "header"
img = _e "img"
input = _e "input"
hr = _e "hr"
label = _e "label"
li = _e "li"
p = _e "p"
section = _e "section"
span = _e "span"
strong = _e "strong"
table = _e "table"
td = _e "td"
tr = _e "tr"
ul = _e "ul"

_e :: String -> [DOMAttr] -> [Tree DOMNode] -> Tree DOMNode
_e name attrs = Node (DOMElement name attrs)
