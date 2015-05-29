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
    go i (AddStyle _ key value h) = go_css i id styles Nothing h
        where styles = Just $ getString key . fromChoiceString value . (";\"" ++)
    go i (AddCustomStyle key value h) = go_css i id styles Nothing h
        where styles = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . (";\"" ++)
    go i (AddClass key h) = go_css i id Nothing classes h 
        where classes = Just $ fromChoiceString key . ('"' :)
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
    go_attrs i attrs (AddStyle _ key value h) = go_css i attrs styles Nothing h
        where styles = Just $ getString key . fromChoiceString value . (";\"" ++)
    go_attrs i attrs (AddCustomStyle key value h) = go_css i attrs styles Nothing h
        where styles = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . (";\"" ++)
    go_attrs i attrs (AddClass key h) = go_css i attrs Nothing classes h
        where classes = Just $ fromChoiceString key . ('"' :)
    go_attrs i _ (Content content) = ind i . fromChoiceString content . ('\n' :)
    go_attrs i _ (Comment comment) = ind i .
        ("<!-- " ++) . fromChoiceString comment . (" -->\n" ++)
    go_attrs i attrs (Append h1 h2) = go_attrs i attrs h1 . go_attrs i attrs h2
    go_attrs _ _ Empty = id
    {-# NOINLINE go_attrs #-}

    go_css :: Int -> (String -> String) -> Maybe (String -> String) -> Maybe (String -> String) -> MarkupM b -> String -> String
    go_css i attrs styles classes (Parent _ open close content) =
        ind i . getString open . attrs . mk_style styles . mk_class classes . (">\n" ++) .
        go (inc i) content . ind i . getString close .  ('\n' :)
    go_css i attrs styles classes (CustomParent tag content) =
        ind i . ('<' :) . fromChoiceString tag . attrs . mk_style styles . mk_class classes . (">\n" ++) .
        go (inc i) content .
        ind i . ("</" ++) . fromChoiceString tag .  (">\n" ++)
    go_css i attrs styles classes (Leaf _ begin end) =
        ind i . getString begin . attrs . mk_style styles . mk_class classes . getString end . ('\n' :)
    go_css i attrs styles classes (CustomLeaf tag close) =
        ind i . ('<' :) . fromChoiceString tag . attrs . mk_style styles . mk_class classes .
        ((if close then " />\n" else ">\n") ++)
    go_css i attrs styles classes (AddAttribute _ key value h) = go_css i attrs' styles classes h
        where attrs' = getString key . fromChoiceString value . ('"' :) . attrs
    go_css i attrs styles classes (AddCustomAttribute key value h) = go_css i attrs' styles classes h
        where attrs' = (' ' : ) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
                       ('"' :) .  attrs
    go_css i attrs Nothing classes (AddStyle _ key value h) = go_css i attrs styles classes h
        where styles = Just $ getString key . fromChoiceString value . (";\"" ++)
    go_css i attrs (Just styles) classes (AddStyle _ key value h) = go_css i attrs styles' classes h
        where styles' = Just $ getString key . fromChoiceString value . ("; " ++) . styles
    go_css i attrs Nothing classes (AddCustomStyle key value h) = go_css i attrs styles classes h
        where styles = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . (";\"" ++)
    go_css i attrs (Just styles) classes (AddCustomStyle key value h) = go_css i attrs styles' classes h
        where styles' = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . ("; " ++) . styles
    go_css i attrs styles Nothing (AddClass key h) = go_css i attrs styles classes h
        where classes = Just $ fromChoiceString key . ('"' :)
    go_css i attrs styles (Just classes ) (AddClass key h) = go_css i attrs styles classes' h
        where classes' = Just $ fromChoiceString key . (' ' :) . classes
    go_css i _ _ _ (Content content) = ind i . fromChoiceString content . ('\n' :)
    go_css i _ _ _ (Comment comment) = ind i .  ("<!-- " ++) . fromChoiceString comment . (" -->\n" ++)
    go_css i attrs styles classes (Append h1 h2) = go_css i attrs styles classes h1 . go_css i attrs styles classes h2
    go_css _ _ _ _ Empty = id
    {-# NOINLINE go_css #-}

    mk_style Nothing = id
    mk_style (Just styles) = (" style=\"" ++) . styles
    mk_class Nothing = id
    mk_class (Just classes) = (" class=\"" ++) . classes

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
