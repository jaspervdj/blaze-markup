{-# LANGUAGE OverloadedStrings #-}
module Text.Blaze.Renderer.Utf8
    ( renderMarkupBuilder
    , renderMarkup
    -- , renderMarkupToByteStringIO
    , renderHtmlBuilder
    , renderHtml
    -- , renderHtmlToByteStringIO
    ) where

import           Data.List                     (isInfixOf)
import           Data.Monoid                   (mappend, mempty)

import           Data.Text                     (Text)
import qualified Data.Text                     as T (isInfixOf, unpack)

import qualified Data.ByteString               as S (isInfixOf)
import           Data.ByteString.Builder       (Builder)
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Builder.Prim  as BP
import           Data.ByteString.Builder.Prim  (condB, (>$<), (>*<))
import qualified Data.ByteString.Lazy          as L

import           Text.Blaze.Internal

-- Support functions
--------------------

{-# INLINE charUtf8HtmlEscaped #-}
charUtf8HtmlEscaped :: BP.BoundedPrim Char
charUtf8HtmlEscaped =
    condB (>  '>' ) BP.charUtf8 $
    condB (== '<' ) (fixed4 ('&',('l',('t',';')))) $        -- &lt;
    condB (== '>' ) (fixed4 ('&',('g',('t',';')))) $        -- &gt;
    condB (== '&' ) (fixed5 ('&',('a',('m',('p',';'))))) $  -- &amp;
    condB (== '"' ) (fixed5 ('&',('#',('3',('4',';'))))) $  -- &#34;
    condB (== '\'') (fixed5 ('&',('#',('3',('9',';'))))) $  -- &#39;
    (BP.liftFixedToBounded BP.char7)    -- fallback for Chars smaller than '>'
  where
    {-# INLINE fixed4 #-}
    fixed4 x = BP.liftFixedToBounded $ const x >$<
      BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7

    {-# INLINE fixed5 #-}
    fixed5 x = BP.liftFixedToBounded $ const x >$<
      BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7

stringUtf8HtmlEscaped :: String -> Builder
stringUtf8HtmlEscaped = BP.primMapListBounded charUtf8HtmlEscaped

textUtf8HtmlEscaped :: Text -> Builder
textUtf8HtmlEscaped = stringUtf8HtmlEscaped . T.unpack

textUtf8 :: Text -> Builder
textUtf8 = B.stringUtf8 . T.unpack

-- Rendering
------------

-- | Render a 'ChoiceString'.
--
fromChoiceString :: ChoiceString  -- ^ String to render
                 -> Builder       -- ^ Resulting builder
fromChoiceString (Static s)     = B.byteStringCopy $ getUtf8ByteString s
fromChoiceString (String s)     = stringUtf8HtmlEscaped s
fromChoiceString (Text s)       = textUtf8HtmlEscaped s
    -- FIXME: Once we have the proper 'Builder' intergration in 'text' use
    -- the corresponding escaping mechanism.
fromChoiceString (ByteString s) = B.byteString s
fromChoiceString (PreEscaped x) = case x of
    String s -> B.stringUtf8 s
    Text   s -> textUtf8 s
    s        -> fromChoiceString s
fromChoiceString (External x) = case x of
    -- Check that the sequence "</" is *not* in the external data.
    String s     -> if "</" `isInfixOf` s then mempty else B.stringUtf8 s
    Text   s     -> if "</" `T.isInfixOf` s then mempty else textUtf8 s
    ByteString s -> if "</" `S.isInfixOf` s then mempty else B.byteString s
    s            -> fromChoiceString s
fromChoiceString (AppendChoiceString x y) =
    fromChoiceString x `mappend` fromChoiceString y
fromChoiceString EmptyChoiceString = mempty
{-# INLINE fromChoiceString #-}

-- | Render some 'Markup' to a 'Builder'.
--
renderMarkupBuilder, renderHtmlBuilder :: Markup     -- ^ Markup to render
                  -> Builder  -- ^ Resulting builder
renderMarkupBuilder = go mempty
  where
    go :: Builder -> MarkupM b -> Builder
    go attrs (Parent _ open close content) =
        B.byteStringCopy (getUtf8ByteString open)
            `mappend` attrs
            `mappend` B.char7 '>'
            `mappend` go mempty content
            `mappend` B.byteStringCopy (getUtf8ByteString close)
    go attrs (CustomParent tag content) =
        B.char7 '<'
            `mappend` fromChoiceString tag
            `mappend` attrs
            `mappend` B.char7 '>'
            `mappend` go mempty content
            `mappend` B.byteStringCopy "</"
            `mappend` fromChoiceString tag
            `mappend` B.char7 '>'
    go attrs (Leaf _ begin end) =
        B.byteStringCopy (getUtf8ByteString begin)
            `mappend` attrs
            `mappend` B.byteStringCopy (getUtf8ByteString end)
    go attrs (CustomLeaf tag close) =
        B.char7 '<'
            `mappend` fromChoiceString tag
            `mappend` attrs
            `mappend` (if close then B.byteStringCopy " />" else B.char7 '>')
    go attrs (AddAttribute _ key value h) =
        go (B.byteStringCopy (getUtf8ByteString key)
            `mappend` fromChoiceString value
            `mappend` B.char7 '"'
            `mappend` attrs) h
    go attrs (AddCustomAttribute key value h) =
        go (B.char7 ' '
            `mappend` fromChoiceString key
            `mappend` B.byteStringCopy "=\""
            `mappend` fromChoiceString value
            `mappend` B.char7 '"'
            `mappend` attrs) h
    go _ (Content content)  = fromChoiceString content
    go attrs (Append h1 h2) = go attrs h1 `mappend` go attrs h2
    go _ Empty              = mempty
    {-# NOINLINE go #-}
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


{- No longer supported with the new builder interface.
 -
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
-}
