module Concur.DOM.Helper.Attribute (
    Attribute
  , alt
  , autofocus
  , checked
  , class_
  , height
  , href
  , maxlength
  , name
  , placeholder
  , src
  , style
  , type_
  , value
  , width
  ) where

import Concur.DOM.Run (DOMAttr(..), DOMAttrValue(..))


type Attribute = DOMAttr

alt, autofocus, checked, class_, href, name, placeholder, src, style, type_, value :: String -> DOMAttr
alt = _a "alt"
autofocus = _a "autofocus"
checked = _a "checked"
class_ = _a "class"
href = _a "href"
name = _a "name"
placeholder = _a "placeholder"
src = _a "src"
style = _a "style"
type_ = _a "type"
value = _a "value"

height, maxlength, width :: Int -> DOMAttr
height = _na "height"
maxlength = _na "maxlength"
width = _na "width"

_a :: String -> String -> DOMAttr
_a name' = DOMAttr name' . DOMAttrText

_na :: String -> Int -> DOMAttr
_na name' = _a name' . show
