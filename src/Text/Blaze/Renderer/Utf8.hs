{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.Utf8
    ( renderMarkupBuilder
    , renderMarkup
    , renderMarkupToByteStringIO
    , renderHtmlBuilder
    , renderHtml
    , renderHtmlToByteStringIO
    ) where

import Data.Monoid (mappend, mempty)
import Data.List (isInfixOf)

import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T (isInfixOf)
import qualified Data.ByteString as S (ByteString, isInfixOf)

import Text.Blaze.Internal
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Html.Utf8 as B

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> Builder       -- ^ Resulting builder
fromChoiceString (Static s)     = B.copyByteString $ getUtf8ByteString s
fromChoiceString (String s)     = B.fromHtmlEscapedString s
fromChoiceString (Text s)       = B.fromHtmlEscapedText s
fromChoiceString (ByteString s) = B.fromByteString s
fromChoiceString (PreEscaped x) = case x of
    String s -> B.fromString s
    Text   s -> B.fromText s
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.fromString s
    Text   s     -> if "</" `T.isInfixOf` s then mempty else B.fromText s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.fromByteString s
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x `mappend` fromChoiceString y
fromChoiceString EmptyChoiceString = mempty
{-# INLINE fromChoiceString #-}

-- | Render some 'Markup' to a 'Builder'.
--
renderMarkupBuilder, renderHtmlBuilder :: Markup     -- ^ Markup to render
                  -> Builder  -- ^ Resulting builder
renderMarkupBuilder = go
  where
    go :: MarkupM b -> Builder
    go (Parent _ open close content) =
        B.copyByteString (getUtf8ByteString open)
            `mappend` B.fromChar '>'
            `mappend` go content
            `mappend` B.copyByteString (getUtf8ByteString close)
    go (CustomParent tag content) =
        B.fromChar '<'
            `mappend` fromChoiceString tag
            `mappend` B.fromChar '>'
            `mappend` go content
            `mappend` B.fromByteString "</"
            `mappend` fromChoiceString tag
            `mappend` B.fromChar '>'
    go (Leaf _ begin end) =
        B.copyByteString (getUtf8ByteString begin)
            `mappend` B.copyByteString (getUtf8ByteString end)
    go (CustomLeaf tag close) =
        B.fromChar '<'
            `mappend` fromChoiceString tag
            `mappend` (if close then B.fromByteString " />" else B.fromChar '>')
    go (AddAttribute _ key value h) =
        go_attrs (B.copyByteString (getUtf8ByteString key)
                 `mappend` fromChoiceString value
                 `mappend` B.fromChar '"') h
    go (AddCustomAttribute key value h) =
        go_attrs (B.fromChar ' '
                 `mappend` fromChoiceString key
                 `mappend` B.fromByteString "=\""
                 `mappend` fromChoiceString value
                 `mappend` B.fromChar '"') h
    go (Content content)  = fromChoiceString content
    go (Comment comment)  =
        B.fromByteString "<!-- "
            `mappend` fromChoiceString comment
            `mappend` B.fromByteString " -->"
    go (Append h1 h2) = go h1 `mappend` go h2
    go Empty              = mempty
    {-# NOINLINE go #-}

    go_attrs :: Builder -> MarkupM b -> Builder
    go_attrs attrs (Parent _ open close content) =
        B.copyByteString (getUtf8ByteString open)
            `mappend` attrs
            `mappend` B.fromChar '>'
            `mappend` go content
            `mappend` B.copyByteString (getUtf8ByteString close)
    go_attrs attrs (CustomParent tag content) =
        B.fromChar '<'
            `mappend` fromChoiceString tag
            `mappend` attrs
            `mappend` B.fromChar '>'
            `mappend` go content
            `mappend` B.fromByteString "</"
            `mappend` fromChoiceString tag
            `mappend` B.fromChar '>'
    go_attrs attrs (Leaf _ begin end) =
        B.copyByteString (getUtf8ByteString begin)
            `mappend` attrs
            `mappend` B.copyByteString (getUtf8ByteString end)
    go_attrs attrs (CustomLeaf tag close) =
        B.fromChar '<'
            `mappend` fromChoiceString tag
            `mappend` attrs
            `mappend` (if close then B.fromByteString " />" else B.fromChar '>')
    go_attrs attrs (AddAttribute _ key value h) =
        go_attrs (B.copyByteString (getUtf8ByteString key)
            `mappend` fromChoiceString value
            `mappend` B.fromChar '"'
            `mappend` attrs) h
    go_attrs attrs (AddCustomAttribute key value h) =
        go_attrs (B.fromChar ' '
            `mappend` fromChoiceString key
            `mappend` B.fromByteString "=\""
            `mappend` fromChoiceString value
            `mappend` B.fromChar '"'
            `mappend` attrs) h
    go_attrs _ (Content content)  = fromChoiceString content
    go_attrs _ (Comment comment)  =
        B.fromByteString "<!-- "
            `mappend` fromChoiceString comment
            `mappend` B.fromByteString " -->"
    go_attrs attrs (Append h1 h2) = go_attrs attrs h1 `mappend` go_attrs attrs h2
    go_attrs _ Empty              = mempty
    {-# NOINLINE go_attrs #-}
{-# INLINE renderMarkupBuilder #-}

renderHtmlBuilder = renderMarkupBuilder
{-# INLINE renderHtmlBuilder #-}
{-# DEPRECATED renderHtmlBuilder
    "Use renderHtmlBuilder from Text.Blaze.Html.Renderer.Utf8 instead" #-}

-- | Render HTML to a lazy UTF-8 encoded 'L.ByteString.'
--
renderMarkup, renderHtml :: Markup          -- ^ Markup to render
                         -> L.ByteString  -- ^ Resulting 'L.ByteString'
renderMarkup = B.toLazyByteString . renderMarkupBuilder
{-# INLINE renderMarkup #-}

renderHtml = renderMarkup
{-# INLINE renderHtml #-}
{-# DEPRECATED renderHtml
    "Use renderHtml from Text.Blaze.Html.Renderer.Utf8 instead" #-}


-- | Repeatedly render HTML to a buffer and process this buffer using the given
-- IO action.
--
renderMarkupToByteStringIO, renderHtmlToByteStringIO :: (S.ByteString -> IO ())
                                                        -- ^ IO action to execute per rendered buffer
                                                     -> Markup          -- ^ Markup to render
                                                     -> IO ()         -- ^ Resulting IO action
renderMarkupToByteStringIO io = B.toByteStringIO io . renderMarkupBuilder
{-# INLINE renderMarkupToByteStringIO #-}

renderHtmlToByteStringIO = renderMarkupToByteStringIO
{-# INLINE renderHtmlToByteStringIO #-}
{-# DEPRECATED renderHtmlToByteStringIO
    "Use renderMarkupToByteStringIO from Text.Blaze.Html.Renderer.Utf8 instead" #-}
