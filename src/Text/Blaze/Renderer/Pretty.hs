-- | A renderer that produces pretty HTML, mostly meant for debugging purposes.
--
module Text.Blaze.Renderer.Pretty
    ( renderMarkup
    , renderHtml
    ) where

import Text.Blaze.Internal
import Text.Blaze.Renderer.String (fromChoiceString)

-- | Render some 'Markup' to an appending 'String'.
--
renderString :: Markup  -- ^ Markup to render
             -> String  -- ^ String to append
             -> String  -- ^ Resulting String
renderString = go 0
  where
    go :: Int -> MarkupM b -> String -> String
    go i (Parent _ open close content) =
        ind i . getString open . (">\n" ++) . go (inc i) content
              . ind i . getString close .  ('\n' :)
    go i (CustomParent tag content) =
        ind i . ('<' :) . fromChoiceString tag . (">\n" ++) .
        go (inc i) content . ind i . ("</" ++) . fromChoiceString tag .
        (">\n" ++)
    go i (Leaf _ begin end) =
        ind i . getString begin . getString end . ('\n' :)
    go i (CustomLeaf tag close) =
        ind i . ('<' :) . fromChoiceString tag .
        ((if close then " />\n" else ">\n") ++)
    go i (AddAttribute _ key value h) = flip (go_attrs i) h $
        getString key . fromChoiceString value . ('"' :)
    go i (AddCustomAttribute key value h) = flip (go_attrs i) h $
        (' ' : ) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
        ('"' :)
    go i (Content content) = ind i . fromChoiceString content . ('\n' :)
    go i (Comment comment) = ind i .
        ("<!-- " ++) . fromChoiceString comment . (" -->\n" ++)
    go i (Append h1 h2) = go i h1 . go i h2
    go _ Empty = id
    {-# NOINLINE go #-}

    go_attrs :: Int -> (String -> String) -> MarkupM b -> String -> String
    go_attrs i attrs (Parent _ open close content) =
        ind i . getString open . attrs . (">\n" ++) . go (inc i) content
              . ind i . getString close .  ('\n' :)
    go_attrs i attrs (CustomParent tag content) =
        ind i . ('<' :) . fromChoiceString tag . attrs . (">\n" ++) .
        go (inc i) content . ind i . ("</" ++) . fromChoiceString tag .
        (">\n" ++)
    go_attrs i attrs (Leaf _ begin end) =
        ind i . getString begin . attrs . getString end . ('\n' :)
    go_attrs i attrs (CustomLeaf tag close) =
        ind i . ('<' :) . fromChoiceString tag . attrs .
        ((if close then " />\n" else ">\n") ++)
    go_attrs i attrs (AddAttribute _ key value h) = flip (go_attrs i) h $
        getString key . fromChoiceString value . ('"' :) . attrs
    go_attrs i attrs (AddCustomAttribute key value h) = flip (go_attrs i) h $
        (' ' : ) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
        ('"' :) .  attrs
    go_attrs i _ (Content content) = ind i . fromChoiceString content . ('\n' :)
    go_attrs i _ (Comment comment) = ind i .
        ("<!-- " ++) . fromChoiceString comment . (" -->\n" ++)
    go_attrs i attrs (Append h1 h2) = go_attrs i attrs h1 . go_attrs i attrs h2
    go_attrs _ _ Empty = id
    {-# NOINLINE go_attrs #-}

    -- Increase the indentation
    inc = (+) 4

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)
{-# INLINE renderString #-}

-- | Render markup to a lazy 'String'. The result is prettified.
--
renderMarkup :: Markup -> String
renderMarkup html = renderString html ""
{-# INLINE renderMarkup #-}

renderHtml :: Markup -> String
renderHtml = renderMarkup
{-# INLINE renderHtml #-}
{-# DEPRECATED renderHtml
    "Use renderHtml from Text.Blaze.Html.Renderer.Pretty instead" #-}
