-- | This is a module which contains some ad-hoc HTML combinators for use when
-- benchmarking
--
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module BenchmarkUtils
    ( Html
    , toHtml

    , tr
    , td
    , html
    , head
    , title
    , body
    , div
    , h1
    , h2
    , p
    , ol
    , li
    , table
    , img
    , id
    ) where

import Prelude hiding (div, head, id)
import Text.Blaze
import Text.Blaze.Internal

type Html = Markup

toHtml :: ToMarkup a => a -> Html
toHtml = toMarkup

tr :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
tr = Parent "tr" "<tr" "</tr>"
{-# INLINE tr #-}

td :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
td = Parent "td" "<td" "</td>"
{-# INLINE td #-}

html :: Html  -- ^ Inner HTML.
     -> Html  -- ^ Resulting HTML.
html = Parent "html" "<html" "</html>"
{-# INLINE html #-}

head :: Html  -- ^ Inner HTML.
     -> Html  -- ^ Resulting HTML.
head = Parent "head" "<head" "</head>"
{-# INLINE head #-}

title :: Html  -- ^ Inner HTML.
      -> Html  -- ^ Resulting HTML.
title = Parent "title" "<title" "</title>"
{-# INLINE title #-}

body :: Html  -- ^ Inner HTML.
     -> Html  -- ^ Resulting HTML.
body = Parent "body" "<body" "</body>"
{-# INLINE body #-}

div :: Html  -- ^ Inner HTML.
    -> Html  -- ^ Resulting HTML.
div = Parent "div" "<div" "</div>"
{-# INLINE div #-}

h1 :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
h1 = Parent "h1" "<h1" "</h1>"
{-# INLINE h1 #-}

h2 :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
h2 = Parent "h2" "<h2" "</h2>"
{-# INLINE h2 #-}

p :: Html  -- ^ Inner HTML.
  -> Html  -- ^ Resulting HTML.
p = Parent "p" "<p" "</p>"
{-# INLINE p #-}

ol :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
ol = Parent "ol" "<ol" "</ol>"
{-# INLINE ol #-}

li :: Html  -- ^ Inner HTML.
   -> Html  -- ^ Resulting HTML.
li = Parent "li" "<li" "</li>"
{-# INLINE li #-}

table :: Html  -- ^ Inner HTML.
      -> Html  -- ^ Resulting HTML.
table = Parent "table" "<table" "</table>"
{-# INLINE table #-}

img :: Html  -- ^ Resulting HTML.
img = Leaf "img" "<img" ">"
{-# INLINE img #-}

id :: AttributeValue  -- ^ Attribute value.
   -> Attribute       -- ^ Resulting attribute.
id = attribute "id" " id=\""
{-# INLINE id #-}
