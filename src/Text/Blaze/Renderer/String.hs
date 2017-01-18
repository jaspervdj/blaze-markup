-- | A renderer that produces a native Haskell 'String', mostly meant for
-- debugging purposes.
--
{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.String
    ( fromChoiceString
    , renderMarkup
    , renderHtml
    ) where

import Data.List (isInfixOf)

import qualified Data.ByteString.Char8 as SBC
import qualified Data.Text as T
import qualified Data.ByteString as S

import Text.Blaze.Internal

-- | Escape predefined XML entities in a string
--
escapeMarkupEntities :: String  -- ^ String to escape
                   -> String  -- ^ String to append
                   -> String  -- ^ Resulting string
escapeMarkupEntities []     k = k
escapeMarkupEntities (c:cs) k = case c of
    '<'  -> '&' : 'l' : 't' : ';'             : escapeMarkupEntities cs k
    '>'  -> '&' : 'g' : 't' : ';'             : escapeMarkupEntities cs k
    '&'  -> '&' : 'a' : 'm' : 'p' : ';'       : escapeMarkupEntities cs k
    '"'  -> '&' : 'q' : 'u' : 'o' : 't' : ';' : escapeMarkupEntities cs k
    '\'' -> '&' : '#' : '3' : '9' : ';'       : escapeMarkupEntities cs k
    x    -> x                                 : escapeMarkupEntities cs k

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> String        -- ^ String to append
                 -> String        -- ^ Resulting string
fromChoiceString (Static s)     = getString s
fromChoiceString (String s)     = escapeMarkupEntities s
fromChoiceString (Text s)       = escapeMarkupEntities $ T.unpack s
fromChoiceString (ByteString s) = (SBC.unpack s ++)
fromChoiceString (PreEscaped x) = case x of
    String s -> (s ++)
    Text   s -> (\k -> T.foldr (:) k s)
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then id else (s ++)
    Text   s     -> if "</" `T.isInfixOf` s then id else (\k -> T.foldr (:) k s)
    ByteString s -> if "</" `S.isInfixOf` s then id else (SBC.unpack s ++)
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x . fromChoiceString y
fromChoiceString EmptyChoiceString = id
{-# INLINE fromChoiceString #-}

-- | Render some 'Markup' to an appending 'String'.
--
renderString :: Markup    -- ^ Markup to render
             -> String  -- ^ String to append
             -> String  -- ^ Resulting String
renderString = go
  where
    go :: MarkupM b -> String -> String
    go (Parent _ open close content) =
        getString open . ('>' :) . go content . getString close
    go (CustomParent tag content) =
        ('<' :) . fromChoiceString tag . ('>' :) .  go content .
        ("</" ++) . fromChoiceString tag . ('>' :)
    go (Leaf _ begin end) = getString begin . getString end
    go (CustomLeaf tag close) =
        ('<' :) . fromChoiceString tag .
        (if close then (" />" ++) else ('>' :))
    go (AddAttribute _ key value h) = flip go_attrs h $
        getString key . fromChoiceString value . ('"' :)
    go (AddCustomAttribute key value h) = flip go_attrs h $
        (' ' :) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .  ('"' :)
    go (AddStyle _ key value h) = go_css id styles Nothing h
        where styles = Just $ getString key . fromChoiceString value . (";\"" ++)
    go (AddCustomStyle key value h) = go_css id styles Nothing h
        where styles = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . (";\"" ++)
    go (AddClass key h) = go_css id Nothing classes h
        where classes = Just $ fromChoiceString key . ('"' :)
    go (Content content) = fromChoiceString content
    go (Comment comment) = ("<!-- " ++) . fromChoiceString comment . (" -->" ++)
    go (Append h1 h2) = go h1 . go h2
    go Empty = id
    {-# NOINLINE go #-}

    go_attrs :: (String -> String) -> MarkupM b -> String -> String
    go_attrs attrs (Parent _ open close content) =
        getString open . attrs . ('>' :) . go content . getString close
    go_attrs attrs (CustomParent tag content) =
        ('<' :) . fromChoiceString tag . attrs . ('>' :) .  go content .
        ("</" ++) . fromChoiceString tag . ('>' :)
    go_attrs attrs (Leaf _ begin end) = getString begin . attrs . getString end
    go_attrs attrs (CustomLeaf tag close) =
        ('<' :) . fromChoiceString tag . attrs .
        (if close then (" />" ++) else ('>' :))
    go_attrs attrs (AddAttribute _ key value h) = flip go_attrs h $
        getString key . fromChoiceString value . ('"' :) . attrs
    go_attrs attrs (AddCustomAttribute key value h) = flip go_attrs h $
        (' ' :) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
        ('"' :) .  attrs
    go_attrs attrs (AddStyle _ key value h) = go_css attrs styles Nothing h
        where styles = Just $ getString key . fromChoiceString value . (";\"" ++)
    go_attrs attrs (AddCustomStyle key value h) = go_css attrs styles Nothing h
        where styles = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . (";\"" ++)
    go_attrs attrs (AddClass key h) = go_css attrs Nothing classes h
        where classes = Just $ fromChoiceString key . ('"' :)
    go_attrs _ (Content content) = fromChoiceString content
    go_attrs _ (Comment comment) = ("<!-- " ++) . fromChoiceString comment . (" -->" ++)
    go_attrs attrs (Append h1 h2) = go_attrs attrs h1 . go_attrs attrs h2
    go_attrs _ Empty = id
    {-# NOINLINE go_attrs #-}

    go_css :: (String -> String) -> Maybe (String -> String) -> Maybe (String -> String) -> MarkupM b -> String -> String
    go_css attrs styles classes (Parent _ open close content) =
        getString open . attrs . mk_style styles . mk_class classes . ('>' :) .
        go content . getString close
    go_css attrs styles classes (CustomParent tag content) =
        ('<' :) . fromChoiceString tag . attrs . mk_style styles . mk_class classes . ('>' :) .
        go content .
        ("</" ++) . fromChoiceString tag . ('>' :)
    go_css attrs styles classes (Leaf _ begin end) =
        getString begin . attrs . mk_style styles . mk_class classes . getString end
    go_css attrs styles classes (CustomLeaf tag close) =
        ('<' :) . fromChoiceString tag . attrs . mk_style styles . mk_class classes .
        (if close then (" />" ++) else ('>' :))
    go_css attrs styles classes (AddAttribute _ key value h) = go_css attrs' styles classes h
        where attrs' = getString key . fromChoiceString value . ('"' :) . attrs
    go_css attrs styles classes (AddCustomAttribute key value h) = go_css attrs' styles classes h
        where attrs' = (' ' :) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
                       ('"' :) .  attrs
    go_css attrs Nothing classes (AddStyle _ key value h) = go_css attrs styles classes h
        where styles = Just $ getString key . fromChoiceString value . (";\"" ++)
    go_css attrs (Just styles) classes (AddStyle _ key value h) = go_css attrs styles' classes h
        where styles' = Just $ getString key . fromChoiceString value . ("; " ++) . styles
    go_css attrs Nothing classes (AddCustomStyle key value h) = go_css attrs styles classes h
        where styles = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . (";\"" ++)
    go_css attrs (Just styles) classes (AddCustomStyle key value h) = go_css attrs styles' classes h
        where styles' = Just $ fromChoiceString key . (": " ++) . fromChoiceString value . ("; " ++) . styles
    go_css attrs styles Nothing (AddClass key h) = go_css attrs styles classes h
        where classes = Just $ fromChoiceString key . ('"' :)
    go_css attrs styles (Just classes) (AddClass key h) = go_css attrs styles classes' h
        where classes' = Just $ fromChoiceString key . (' ' :) . classes
    go_css _ _ _ (Content content) = fromChoiceString content
    go_css _ _ _ (Comment comment) =
        ("<!-- " ++) . fromChoiceString comment . (" -->" ++)
    go_css attrs styles classes (Append h1 h2) = go_css attrs styles classes h1 . go_css attrs styles classes h2
    go_css _ _ _ Empty = id
    {-# NOINLINE go_css #-}

    mk_style Nothing = id
    mk_style (Just styles) = (" style=\"" ++) . styles
    mk_class Nothing = id
    mk_class (Just classes) = (" class=\"" ++) . classes

{-# INLINE renderString #-}

-- | Render markup to a lazy 'String'.
--
renderMarkup :: Markup -> String
renderMarkup html = renderString html ""
{-# INLINE renderMarkup #-}

renderHtml :: Markup -> String
renderHtml = renderMarkup
{-# INLINE renderHtml #-}
{-# DEPRECATED renderHtml
    "Use renderHtml from Text.Blaze.Html.Renderer.String instead" #-}
