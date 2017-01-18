{-# LANGUAGE OverloadedStrings #-}
-- | A renderer that produces a lazy 'L.Text' value, using the Text Builder.
--
module Text.Blaze.Renderer.Text
    ( renderMarkupBuilder
    , renderMarkupBuilderWith
    , renderMarkup
    , renderMarkupWith
    , renderHtmlBuilder
    , renderHtmlBuilderWith
    , renderHtml
    , renderHtmlWith
    ) where

import Data.Monoid (mappend, mempty)
import Data.List (isInfixOf)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L
import Data.ByteString (ByteString)
import qualified Data.ByteString as S (isInfixOf)

import Text.Blaze.Internal
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

-- | Escape predefined XML entities in a text value
--
escapeMarkupEntities :: Text     -- ^ Text to escape
                   -> Builder  -- ^ Resulting text builder
escapeMarkupEntities = T.foldr escape mempty
  where
    escape :: Char -> Builder -> Builder
    escape '<'  b = B.fromText "&lt;"   `mappend` b
    escape '>'  b = B.fromText "&gt;"   `mappend` b
    escape '&'  b = B.fromText "&amp;"  `mappend` b
    escape '"'  b = B.fromText "&quot;" `mappend` b
    escape '\'' b = B.fromText "&#39;"  `mappend` b
    escape x    b = B.singleton x       `mappend` b

-- | Render a 'ChoiceString'. TODO: Optimization possibility, apply static
-- argument transformation.
--
fromChoiceString :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                 -> ChoiceString          -- ^ String to render
                 -> Builder               -- ^ Resulting builder
fromChoiceString _ (Static s)     = B.fromText $ getText s
fromChoiceString _ (String s)     = escapeMarkupEntities $ T.pack s
fromChoiceString _ (Text s)       = escapeMarkupEntities s
fromChoiceString d (ByteString s) = B.fromText $ d s
fromChoiceString d (PreEscaped x) = case x of
    String s -> B.fromText $ T.pack s
    Text   s -> B.fromText s
    s        -> fromChoiceString d s
fromChoiceString d (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.fromText (T.pack s)
    Text   s     -> if "</" `T.isInfixOf` s then mempty else B.fromText s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.fromText (d s)
    s            -> fromChoiceString d s
fromChoiceString d (AppendChoiceString x y) =
    fromChoiceString d x `mappend` fromChoiceString d y
fromChoiceString _ EmptyChoiceString = mempty
{-# INLINE fromChoiceString #-}

-- | Render markup to a text builder
renderMarkupBuilder :: Markup -> Builder
renderMarkupBuilder = renderMarkupBuilderWith decodeUtf8
{-# INLINE renderMarkupBuilder #-}

renderHtmlBuilder :: Markup -> Builder
renderHtmlBuilder = renderMarkupBuilder
{-# INLINE renderHtmlBuilder #-}
{-# DEPRECATED renderHtmlBuilder
    "Use renderHtmlBuilder from Text.Blaze.Html.Renderer.Text instead" #-}

-- | Render some 'Markup' to a Text 'Builder'.
--
renderMarkupBuilderWith :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                        -> Markup                -- ^ Markup to render
                        -> Builder               -- ^ Resulting builder
renderMarkupBuilderWith d = go
  where
    go :: MarkupM b -> Builder
    go (Parent _ open close content) =
        B.fromText (getText open)
            `mappend` B.singleton '>'
            `mappend` go content
            `mappend` B.fromText (getText close)
    go (CustomParent tag content) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` B.singleton '>'
            `mappend` go content
            `mappend` B.fromText "</"
            `mappend` fromChoiceString d tag
            `mappend` B.singleton '>'
    go (Leaf _ begin end) =
        B.fromText (getText begin)
            `mappend` B.fromText (getText end)
    go (CustomLeaf tag close) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` (if close then B.fromText " />" else B.singleton '>')
    go (AddAttribute _ key value h) =
        go_attrs (B.fromText (getText key)
                 `mappend` fromChoiceString d value
                 `mappend` B.singleton '"') h
    go (AddCustomAttribute key value h) =
        go_attrs (B.singleton ' '
                 `mappend` fromChoiceString d key
                 `mappend` B.fromText "=\""
                 `mappend` fromChoiceString d value
                 `mappend` B.singleton '"') h
    go (AddStyle _ key value h) =
        go_css mempty
            (Just $ B.fromText (getText key)
                `mappend` fromChoiceString d value
                `mappend` B.fromText (getText ";\""))
            Nothing h
    go (AddCustomStyle key value h) =
        go_css mempty
            (Just $ fromChoiceString d key
               `mappend` B.fromText (getText ": ")
               `mappend` fromChoiceString d value
               `mappend` B.fromText (getText ";\""))
            Nothing h
    go (AddClass key h) =
        go_css mempty Nothing
            (Just $ fromChoiceString d key
                `mappend` B.singleton '"')
            h
    go (Content content)  = fromChoiceString d content
    go (Comment comment)  =
        B.fromText "<!-- "
            `mappend` fromChoiceString d comment
            `mappend` " -->"
    go (Append h1 h2) = go h1 `mappend` go h2
    go Empty              = mempty
    {-# NOINLINE go #-}

    go_attrs :: Builder -> MarkupM b -> Builder
    go_attrs attrs (Parent _ open close content) =
        B.fromText (getText open)
            `mappend` attrs
            `mappend` B.singleton '>'
            `mappend` go content
            `mappend` B.fromText (getText close)
    go_attrs attrs (CustomParent tag content) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` attrs
            `mappend` B.singleton '>'
            `mappend` go content
            `mappend` B.fromText "</"
            `mappend` fromChoiceString d tag
            `mappend` B.singleton '>'
    go_attrs attrs (Leaf _ begin end) =
        B.fromText (getText begin)
            `mappend` attrs
            `mappend` B.fromText (getText end)
    go_attrs attrs (CustomLeaf tag close) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` attrs
            `mappend` (if close then B.fromText " />" else B.singleton '>')
    go_attrs attrs (AddAttribute _ key value h) =
        go_attrs
            (B.fromText (getText key)
                `mappend` fromChoiceString d value
                `mappend` B.singleton '"'
                `mappend` attrs) h
    go_attrs attrs (AddCustomAttribute key value h) =
        go_attrs
            (B.singleton ' '
                `mappend` fromChoiceString d key
                `mappend` B.fromText "=\""
                `mappend` fromChoiceString d value
                `mappend` B.singleton '"'
                `mappend` attrs) h
    go_attrs attrs (AddStyle _ key value h) =
        go_css
            attrs
            (Just $ B.fromText (getText key)
                `mappend` fromChoiceString d value
                `mappend` B.fromText (getText ";\""))
            Nothing h
    go_attrs attrs (AddCustomStyle key value h) =
        go_css attrs
            (Just $ fromChoiceString d key
               `mappend` B.fromText (getText ": ")
               `mappend` fromChoiceString d value
               `mappend` B.fromText (getText ";\""))
            Nothing h
    go_attrs attrs (AddClass key h) =
        go_css attrs Nothing
            (Just $ fromChoiceString d key
                `mappend` B.singleton '"')
            h
    go_attrs _ (Content content)  = fromChoiceString d content
    go_attrs _ (Comment comment)  =
        B.fromText "<!-- "
            `mappend` fromChoiceString d comment
            `mappend` " -->"
    go_attrs attrs (Append h1 h2) = go_attrs attrs h1 `mappend` go_attrs attrs h2
    go_attrs _ Empty              = mempty
    {-# NOINLINE go_attrs #-}

    go_css :: Builder -> Maybe Builder -> Maybe Builder -> MarkupM b -> Builder
    go_css attrs styles classes (Parent _ open close content) =
        B.fromText (getText open)
            `mappend` attrs
            `mappend` mk_style styles
            `mappend` mk_class classes
            `mappend` B.singleton '>'
            `mappend` go content
            `mappend` B.fromText (getText close)
    go_css attrs styles classes (CustomParent tag content) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` attrs
            `mappend` mk_style styles
            `mappend` mk_class classes
            `mappend` B.singleton '>'
            `mappend` go content
            `mappend` B.fromText "</"
            `mappend` fromChoiceString d tag
            `mappend` B.singleton '>'
    go_css attrs styles classes (Leaf _ begin end) =
        B.fromText (getText begin)
            `mappend` attrs
            `mappend` mk_style styles
            `mappend` mk_class classes
            `mappend` B.fromText (getText end)
    go_css attrs styles classes (CustomLeaf tag close) =
        B.singleton '<'
            `mappend` fromChoiceString d tag
            `mappend` attrs
            `mappend` mk_style styles
            `mappend` mk_class classes
            `mappend` (if close then B.fromText " />" else B.singleton '>')
    go_css attrs styles classes (AddAttribute _ key value h) =
        go_css (B.fromText (getText key)
            `mappend` fromChoiceString d value
            `mappend` B.singleton '"'
            `mappend` attrs) styles classes h
    go_css attrs styles classes (AddCustomAttribute key value h) =
        go_css (B.singleton ' '
            `mappend` fromChoiceString d key
            `mappend` B.fromText "=\""
            `mappend` fromChoiceString d value
            `mappend` B.singleton '"'
            `mappend` attrs) styles classes h
    go_css attrs Nothing classes (AddStyle _ key value h) =
        go_css attrs
           (Just $ B.fromText (getText key)
                   `mappend` fromChoiceString d value
                   `mappend` B.fromText (getText ";\""))
           classes
           h
    go_css attrs (Just styles) classes (AddStyle _ key value h) =
        go_css attrs
           (Just $ B.fromText (getText key)
                   `mappend` fromChoiceString d value
                   `mappend` B.fromText (getText "; ")
                   `mappend` styles)
           classes
           h
    go_css attrs Nothing classes (AddCustomStyle key value h) =
        go_css attrs
           (Just $ fromChoiceString d key
                   `mappend` B.fromText (getText ": ")
                   `mappend` fromChoiceString d value
                   `mappend` B.fromText (getText ";\""))
           classes
           h
    go_css attrs (Just styles) classes (AddCustomStyle key value h) =
        go_css attrs
           (Just $ fromChoiceString d key
                   `mappend` B.fromText (getText ": ")
                   `mappend` fromChoiceString d value
                   `mappend` B.fromText (getText "; ")
                   `mappend` styles)
           classes
           h
    go_css attrs styles Nothing (AddClass key h) =
        go_css attrs
           styles
           (Just $ fromChoiceString d key
                   `mappend` B.singleton '"')
           h
    go_css attrs styles (Just classes) (AddClass key h) =
        go_css attrs
           styles
           (Just $ fromChoiceString d key
                   `mappend` B.singleton ' '
                   `mappend` classes)
           h
    go_css _ _ _ (Content content)  = fromChoiceString d content
    go_css _ _ _ (Comment comment)  =
        B.fromText "<!-- "
            `mappend` fromChoiceString d comment
            `mappend` " -->"
    go_css attrs styles classes (Append h1 h2) = go_css attrs styles classes h1 `mappend` go_css attrs styles classes h2
    go_css _ _ _ Empty              = mempty
    {-# NOINLINE go_css #-}

    mk_style Nothing = mempty
    mk_style (Just styles) = B.fromText (getText " style=\"") `mappend` styles 
    mk_class Nothing = mempty
    mk_class (Just classes) = B.fromText (getText " class=\"") `mappend` classes 
{-# INLINE renderMarkupBuilderWith #-}

renderHtmlBuilderWith :: (ByteString -> Text)  -- ^ Decoder for bytestrings
                      -> Markup                -- ^ Markup to render
                      -> Builder               -- ^ Resulting builder
renderHtmlBuilderWith = renderMarkupBuilderWith
{-# INLINE renderHtmlBuilderWith #-}
{-# DEPRECATED renderHtmlBuilderWith
    "Use renderHtmlBuilderWith from Text.Blaze.Html.Renderer.Text instead" #-}

-- | Render markup to a lazy Text value. If there are any ByteString's in the
-- input markup, this function will consider them as UTF-8 encoded values and
-- decode them that way.
--
renderMarkup :: Markup -> L.Text
renderMarkup = renderMarkupWith decodeUtf8
{-# INLINE renderMarkup #-}

renderHtml :: Markup -> L.Text
renderHtml = renderMarkup
{-# INLINE renderHtml #-}
{-# DEPRECATED renderHtml
    "Use renderHtml from Text.Blaze.Html.Renderer.Text instead" #-}

-- | Render markup to a lazy Text value. This function allows you to specify what
-- should happen with ByteString's in the input HTML. You can decode them or
-- drop them, this depends on the application...
--
renderMarkupWith :: (ByteString -> Text)  -- ^ Decoder for ByteString's.
                 -> Markup                -- ^ Markup to render
                 -> L.Text                -- Resulting lazy text
renderMarkupWith d = B.toLazyText . renderMarkupBuilderWith d

renderHtmlWith :: (ByteString -> Text)  -- ^ Decoder for ByteString's.
               -> Markup                -- ^ Markup to render
               -> L.Text                -- ^ Resulting lazy text
renderHtmlWith = renderMarkupWith
{-# DEPRECATED renderHtmlWith
    "Use renderHtmlWith from Text.Blaze.Html.Renderer.Text instead" #-}
