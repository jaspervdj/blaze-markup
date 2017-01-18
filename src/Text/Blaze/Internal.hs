{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances, ExistentialQuantification,
             DeriveDataTypeable, GADTs, CPP #-}
-- | The BlazeMarkup core, consisting of functions that offer the power to
-- generate custom markup elements. It also offers user-centric functions,
-- which are exposed through 'Text.Blaze'.
--
-- While this module is exported, usage of it is not recommended, unless you
-- know what you are doing. This module might undergo changes at any time.
--
module Text.Blaze.Internal
    (
      -- * Important types.
      ChoiceString (..)
    , StaticString (..)
    , MarkupM (..)
    , Markup
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating custom tags and attributes.
    , customParent
    , customLeaf
    , attribute
    , dataAttribute
    , customAttribute

      -- * Converting values to Markup.
    , text
    , preEscapedText
    , lazyText
    , preEscapedLazyText
    , textBuilder
    , preEscapedTextBuilder
    , string
    , preEscapedString
    , unsafeByteString
    , unsafeLazyByteString

      -- * Comments
    , textComment
    , lazyTextComment
    , stringComment
    , unsafeByteStringComment
    , unsafeLazyByteStringComment

      -- * Converting values to tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , textValue
    , preEscapedTextValue
    , lazyTextValue
    , preEscapedLazyTextValue
    , textBuilderValue
    , preEscapedTextBuilderValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue
    , unsafeLazyByteStringValue

      -- * Setting attributes
    , Attributable
    , (!)
    , (!?)

      -- * Modifying Markup elements
    , contents
    , external

      -- * Querying Markup elements
    , null
    ) where

import Prelude hiding (null)
import Control.Applicative (Applicative (..))
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import qualified Data.List as List

import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Exts (IsString (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup)
#endif

-- | A static string that supports efficient output to all possible backends.
--
data StaticString = StaticString
    { getString         :: String -> String  -- ^ Appending haskell string
    , getUtf8ByteString :: B.ByteString      -- ^ UTF-8 encoded bytestring
    , getText           :: Text              -- ^ Text value
    }

-- 'StaticString's should only be converted from string literals, as far as I
-- can see.
--
instance IsString StaticString where
    fromString s = let t = T.pack s
                   in StaticString (s ++) (T.encodeUtf8 t) t

-- | A string denoting input from different string representations.
--
data ChoiceString
    -- | Static data
    = Static {-# UNPACK #-} !StaticString
    -- | A Haskell String
    | String String
    -- | A Text value
    | Text Text
    -- | An encoded bytestring
    | ByteString B.ByteString
    -- | A pre-escaped string
    | PreEscaped ChoiceString
    -- | External data in style/script tags, should be checked for validity
    | External ChoiceString
    -- | Concatenation
    | AppendChoiceString ChoiceString ChoiceString
    -- | Empty string
    | EmptyChoiceString

instance Monoid ChoiceString where
    mempty = EmptyChoiceString
    {-# INLINE mempty #-}
    mappend = AppendChoiceString
    {-# INLINE mappend #-}

instance IsString ChoiceString where
    fromString = String
    {-# INLINE fromString #-}

-- | The core Markup datatype.
--
data MarkupM a where
    -- | Tag, open tag, end tag, content
    Parent :: StaticString -> StaticString -> StaticString -> MarkupM a -> MarkupM a
    -- | Custom parent
    CustomParent :: ChoiceString -> MarkupM a -> MarkupM a
    -- | Tag, open tag, end tag
    Leaf :: StaticString -> StaticString -> StaticString -> MarkupM ()
    -- | Custom leaf
    CustomLeaf :: ChoiceString -> Bool -> MarkupM ()
    -- | HTML content.  Has universal type to allow generic IsString instance to work.
    Content :: ChoiceString -> MarkupM a
    -- | HTML comment. Note: you should wrap the 'ChoiceString' in a
    -- 'PreEscaped'.
    Comment :: ChoiceString -> MarkupM ()
    -- | Concatenation of two HTML pieces
    Append :: MarkupM b -> MarkupM a -> MarkupM a
    -- | Add an attribute to the inner HTML. Raw key, key, value, HTML to
    -- receive the attribute.
    AddAttribute :: StaticString -> StaticString -> ChoiceString -> MarkupM a -> MarkupM a
    -- | Add a custom attribute to the inner HTML.
    AddCustomAttribute :: ChoiceString -> ChoiceString -> MarkupM a -> MarkupM a
    -- | Empty HTML.
    Empty :: !a -> MarkupM a
    deriving (Typeable)

-- | Simplification of the 'MarkupM' datatype.
--
type Markup = MarkupM ()

markupValue :: MarkupM a -> a
markupValue (Parent _ _ _ x) = markupValue x
markupValue (CustomParent _ x) = markupValue x
markupValue Leaf{} = ()
markupValue CustomLeaf{} = ()
markupValue Content{} = error "Text.Blaze.Internal.MarkupM: invalid use of monadic bind from string literal"
markupValue Comment{} = ()
markupValue (Append _ x) = markupValue x
markupValue (AddAttribute _ _ _ x) = markupValue x
markupValue (AddCustomAttribute _ _ x) = markupValue x
markupValue (Empty a) = a

instance Monoid a => Monoid (MarkupM a) where
    mempty = Empty mempty
    {-# INLINE mempty #-}
    mappend x y = Append x y
    {-# INLINE mappend #-}
    mconcat = foldr Append (Empty mempty)
    {-# INLINE mconcat #-}

#if MIN_VERSION_base(4,9,0)
instance Monoid a => Semigroup (MarkupM a) where
#endif

instance Functor MarkupM where
    fmap f (Empty x) = Empty $ f x
    fmap f x = Append x $ Empty $ f $ markupValue x

instance Applicative MarkupM where
    pure = Empty
    {-# INLINE pure #-}
    f <*> Empty v = Append f $ Empty $ markupValue f v
    f <*> v = Append (Append f v) $ Empty $ markupValue f $ markupValue v
    {-# INLINE (<*>) #-}
    (*>) = Append
    {-# INLINE (*>) #-}
    f <* Empty _ = f
    f <* v = Append (Append f v) $ Empty (markupValue f)
    {-# INLINE (<*) #-}

instance Monad MarkupM where
    return = Empty
    {-# INLINE return #-}
    (>>) = Append
    {-# INLINE (>>) #-}
    h1 >>= f = h1 >> f (markupValue h1)
    {-# INLINE (>>=) #-}

instance IsString (MarkupM a) where
    fromString = Content . fromString
    {-# INLINE fromString #-}

-- | Type for an HTML tag. This can be seen as an internal string type used by
-- BlazeMarkup.
--
newtype Tag = Tag { unTag :: StaticString }
    deriving (IsString)

-- | Type for an attribute.
--
newtype Attribute = Attribute (forall a. MarkupM a -> MarkupM a)

instance Monoid Attribute where
    mempty                            = Attribute id
    Attribute f `mappend` Attribute g = Attribute (g . f)

-- | The type for the value part of an attribute.
--
newtype AttributeValue = AttributeValue { unAttributeValue :: ChoiceString }
    deriving (IsString, Monoid)

-- | Create a custom parent element
customParent :: Tag     -- ^ Element tag
             -> Markup  -- ^ Content
             -> Markup  -- ^ Resulting markup
customParent tag = CustomParent (Static $ unTag tag)

-- | Create a custom leaf element
customLeaf :: Tag     -- ^ Element tag
           -> Bool    -- ^ Close the leaf?
           -> Markup  -- ^ Resulting markup
customLeaf tag = CustomLeaf (Static $ unTag tag)

-- | Create an HTML attribute that can be applied to an HTML element later using
-- the '!' operator.
--
attribute :: Tag             -- ^ Raw key
          -> Tag             -- ^ Shared key string for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute rawKey key value = Attribute $
    AddAttribute (unTag rawKey) (unTag key) (unAttributeValue value)
{-# INLINE attribute #-}

-- | From HTML 5 onwards, the user is able to specify custom data attributes.
--
-- An example:
--
-- > <p data-foo="bar">Hello.</p>
--
-- We support this in BlazeMarkup using this function. The above fragment could
-- be described using BlazeMarkup with:
--
-- > p ! dataAttribute "foo" "bar" $ "Hello."
--
dataAttribute :: Tag             -- ^ Name of the attribute.
              -> AttributeValue  -- ^ Value for the attribute.
              -> Attribute       -- ^ Resulting HTML attribute.
dataAttribute tag value = Attribute $ AddCustomAttribute
    (Static "data-" `mappend` Static (unTag tag))
    (unAttributeValue value)
{-# INLINE dataAttribute #-}

-- | Create a custom attribute. This is not specified in the HTML spec, but some
-- JavaScript libraries rely on it.
--
-- An example:
--
-- > <select dojoType="select">foo</select>
--
-- Can be produced using:
--
-- > select ! customAttribute "dojoType" "select" $ "foo"
--
customAttribute :: Tag             -- ^ Name of the attribute
                -> AttributeValue  -- ^ Value for the attribute
                -> Attribute       -- ^ Resulting HTML attribtue
customAttribute tag value = Attribute $ AddCustomAttribute
    (Static $ unTag tag)
    (unAttributeValue value)
{-# INLINE customAttribute #-}

-- | Render text. Functions like these can be used to supply content in HTML.
--
text :: Text    -- ^ Text to render.
     -> Markup  -- ^ Resulting HTML fragment.
text = Content . Text
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: Text    -- ^ Text to insert
               -> Markup  -- ^ Resulting HTML fragment
preEscapedText = Content . PreEscaped . Text
{-# INLINE preEscapedText #-}

-- | A variant of 'text' for lazy 'LT.Text'.
--
lazyText :: LT.Text  -- ^ Text to insert
         -> Markup   -- ^ Resulting HTML fragment
lazyText = mconcat . map text . LT.toChunks
{-# INLINE lazyText #-}

-- | A variant of 'preEscapedText' for lazy 'LT.Text'
--
preEscapedLazyText :: LT.Text  -- ^ Text to insert
                   -> Markup   -- ^ Resulting HTML fragment
preEscapedLazyText = mconcat . map preEscapedText . LT.toChunks
{-# INLINE preEscapedLazyText #-}

-- | A variant of 'text' for text 'LTB.Builder'.
--
textBuilder :: LTB.Builder -- ^ Text to insert
            -> Markup      -- ^ Resulting HTML fragment
textBuilder = lazyText . LTB.toLazyText
{-# INLINE textBuilder #-}

-- | A variant of 'preEscapedText' for lazy 'LT.Text'
--
preEscapedTextBuilder :: LTB.Builder -- ^ Text to insert
                      -> Markup      -- ^ Resulting HTML fragment
preEscapedTextBuilder = preEscapedLazyText . LTB.toLazyText
{-# INLINE preEscapedTextBuilder #-}

-- | Create an HTML snippet from a 'String'.
--
string :: String  -- ^ String to insert.
       -> Markup  -- ^ Resulting HTML fragment.
string = Content . String
{-# INLINE string #-}

-- | Create an HTML snippet from a 'String' without escaping
--
preEscapedString :: String  -- ^ String to insert.
                 -> Markup  -- ^ Resulting HTML fragment.
preEscapedString = Content . PreEscaped . String
{-# INLINE preEscapedString #-}

-- | Insert a 'ByteString'. This is an unsafe operation:
--
-- * The 'ByteString' could have the wrong encoding.
--
-- * The 'ByteString' might contain illegal HTML characters (no escaping is
--   done).
--
unsafeByteString :: ByteString  -- ^ Value to insert.
                 -> Markup      -- ^ Resulting HTML fragment.
unsafeByteString = Content . ByteString
{-# INLINE unsafeByteString #-}

-- | Insert a lazy 'BL.ByteString'. See 'unsafeByteString' for reasons why this
-- is an unsafe operation.
--
unsafeLazyByteString :: BL.ByteString  -- ^ Value to insert
                     -> Markup         -- ^ Resulting HTML fragment
unsafeLazyByteString = mconcat . map unsafeByteString . BL.toChunks
{-# INLINE unsafeLazyByteString #-}

-- | Create a comment from a 'Text' value.
-- The text should not contain @"--"@.
-- This is not checked by the library.
textComment :: Text -> Markup
textComment = Comment . PreEscaped . Text

-- | Create a comment from a 'LT.Text' value.
-- The text should not contain @"--"@.
-- This is not checked by the library.
lazyTextComment :: LT.Text -> Markup
lazyTextComment = Comment . mconcat . map (PreEscaped . Text) . LT.toChunks

-- | Create a comment from a 'String' value.
-- The text should not contain @"--"@.
-- This is not checked by the library.
stringComment :: String -> Markup
stringComment = Comment . PreEscaped . String

-- | Create a comment from a 'ByteString' value.
-- The text should not contain @"--"@.
-- This is not checked by the library.
unsafeByteStringComment :: ByteString -> Markup
unsafeByteStringComment = Comment . PreEscaped . ByteString

-- | Create a comment from a 'BL.ByteString' value.
-- The text should not contain @"--"@.
-- This is not checked by the library.
unsafeLazyByteStringComment :: BL.ByteString -> Markup
unsafeLazyByteStringComment =
    Comment . mconcat . map (PreEscaped . ByteString) . BL.toChunks

-- | Create a 'Tag' from some 'Text'.
--
textTag :: Text  -- ^ Text to create a tag from
        -> Tag   -- ^ Resulting tag
textTag t = Tag $ StaticString (T.unpack t ++) (T.encodeUtf8 t) t

-- | Create a 'Tag' from a 'String'.
--
stringTag :: String  -- ^ String to create a tag from
          -> Tag     -- ^ Resulting tag
stringTag = Tag . fromString

-- | Render an attribute value from 'Text'.
--
textValue :: Text            -- ^ The actual value.
          -> AttributeValue  -- ^ Resulting attribute value.
textValue = AttributeValue . Text
{-# INLINE textValue #-}

-- | Render an attribute value from 'Text' without escaping.
--
preEscapedTextValue :: Text            -- ^ The actual value
                    -> AttributeValue  -- ^ Resulting attribute value
preEscapedTextValue = AttributeValue . PreEscaped . Text
{-# INLINE preEscapedTextValue #-}

-- | A variant of 'textValue' for lazy 'LT.Text'
--
lazyTextValue :: LT.Text         -- ^ The actual value
              -> AttributeValue  -- ^ Resulting attribute value
lazyTextValue = mconcat . map textValue . LT.toChunks
{-# INLINE lazyTextValue #-}

-- | A variant of 'preEscapedTextValue' for lazy 'LT.Text'
--
preEscapedLazyTextValue :: LT.Text         -- ^ The actual value
                        -> AttributeValue  -- ^ Resulting attribute value
preEscapedLazyTextValue = mconcat . map preEscapedTextValue . LT.toChunks
{-# INLINE preEscapedLazyTextValue #-}

-- | A variant of 'textValue' for text 'LTB.Builder'
--
textBuilderValue :: LTB.Builder    -- ^ The actual value
                 -> AttributeValue -- ^ Resulting attribute value
textBuilderValue = lazyTextValue . LTB.toLazyText
{-# INLINE textBuilderValue #-}

-- | A variant of 'preEscapedTextValue' for text 'LTB.Builder'
--
preEscapedTextBuilderValue :: LTB.Builder    -- ^ The actual value
                           -> AttributeValue -- ^ Resulting attribute value
preEscapedTextBuilderValue = preEscapedLazyTextValue . LTB.toLazyText
{-# INLINE preEscapedTextBuilderValue #-}

-- | Create an attribute value from a 'String'.
--
stringValue :: String -> AttributeValue
stringValue = AttributeValue . String
{-# INLINE stringValue #-}

-- | Create an attribute value from a 'String' without escaping.
--
preEscapedStringValue :: String -> AttributeValue
preEscapedStringValue = AttributeValue . PreEscaped . String
{-# INLINE preEscapedStringValue #-}

-- | Create an attribute value from a 'ByteString'. See 'unsafeByteString'
-- for reasons why this might not be a good idea.
--
unsafeByteStringValue :: ByteString      -- ^ ByteString value
                      -> AttributeValue  -- ^ Resulting attribute value
unsafeByteStringValue = AttributeValue . ByteString
{-# INLINE unsafeByteStringValue #-}

-- | Create an attribute value from a lazy 'BL.ByteString'. See
-- 'unsafeByteString' for reasons why this might not be a good idea.
--
unsafeLazyByteStringValue :: BL.ByteString   -- ^ ByteString value
                          -> AttributeValue  -- ^ Resulting attribute value
unsafeLazyByteStringValue = mconcat . map unsafeByteStringValue . BL.toChunks
{-# INLINE unsafeLazyByteStringValue #-}

-- | Used for applying attributes. You should not define your own instances of
-- this class.
class Attributable h where
    -- | Apply an attribute to an element.
    --
    -- Example:
    --
    -- > img ! src "foo.png"
    --
    -- Result:
    --
    -- > <img src="foo.png" />
    --
    -- This can be used on nested elements as well.
    --
    -- Example:
    --
    -- > p ! style "float: right" $ "Hello!"
    --
    -- Result:
    --
    -- > <p style="float: right">Hello!</p>
    --
    (!) :: h -> Attribute -> h

instance Attributable (MarkupM a) where
    h ! (Attribute f) = f h
    {-# INLINE (!) #-}

instance Attributable (MarkupM a -> MarkupM b) where
    h ! f = (! f) . h
    {-# INLINE (!) #-}

-- | Shorthand for setting an attribute depending on a conditional.
--
-- Example:
--
-- > p !? (isBig, A.class "big") $ "Hello"
--
-- Gives the same result as:
--
-- > (if isBig then p ! A.class "big" else p) "Hello"
--
(!?) :: Attributable h => h -> (Bool, Attribute) -> h
(!?) h (c, a) = if c then h ! a else h

-- | Mark HTML as external data. External data can be:
--
-- * CSS data in a @<style>@ tag;
--
-- * Script data in a @<script>@ tag.
--
-- This function is applied automatically when using the @style@ or @script@
-- combinators.
--
external :: MarkupM a -> MarkupM a
external (Content x) = Content $ External x
external (Append x y) = Append (external x) (external y)
external (Parent x y z i) = Parent x y z $ external i
external (CustomParent x i) = CustomParent x $ external i
external (AddAttribute x y z i) = AddAttribute x y z $ external i
external (AddCustomAttribute x y i) = AddCustomAttribute x y $ external i
external x = x
{-# INLINE external #-}

-- | Take only the text content of an HTML tree.
--
-- > contents $ do
-- >     p ! $ "Hello "
-- >     p ! $ "Word!"
--
-- Result:
--
-- > Hello World!
--
contents :: MarkupM a -> MarkupM a
contents (Parent _ _ _ c)           = contents c
contents (CustomParent _ c)         = contents c
contents (Content c)                = Content c
contents (Append c1 c2)             = Append (contents c1) (contents c2)
contents (AddAttribute _ _ _ c)     = contents c
contents (AddCustomAttribute _ _ c) = contents c
contents Leaf{}                     = Empty ()
contents CustomLeaf{}               = Empty ()
contents Comment{}                  = Empty ()
contents (Empty a)                  = Empty a

-- | Check if a 'Markup' value is completely empty (renders to the empty
-- string).
null :: MarkupM a -> Bool
null markup = case markup of
    Parent _ _ _ _           -> False
    CustomParent _ _         -> False
    Leaf _ _ _               -> False
    CustomLeaf _ _           -> False
    Content c                -> emptyChoiceString c
    Comment c                -> emptyChoiceString c
    Append c1 c2             -> null c1 && null c2
    AddAttribute _ _ _ c     -> null c
    AddCustomAttribute _ _ c -> null c
    Empty _                  -> True
  where
    emptyChoiceString cs = case cs of
        Static ss                -> emptyStaticString ss
        String s                 -> List.null s
        Text t                   -> T.null t
        ByteString bs            -> B.null bs
        PreEscaped c             -> emptyChoiceString c
        External c               -> emptyChoiceString c
        AppendChoiceString c1 c2 -> emptyChoiceString c1 && emptyChoiceString c2
        EmptyChoiceString        -> True

    emptyStaticString = B.null . getUtf8ByteString
