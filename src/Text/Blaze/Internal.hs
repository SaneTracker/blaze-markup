{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances, ExistentialQuantification,
             DeriveDataTypeable, FlexibleContexts #-}
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
    , Markup (..)
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
    , string
    , preEscapedString
    , unsafeByteString
    , unsafeLazyByteString

      -- * Converting values to tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , textValue
    , preEscapedTextValue
    , lazyTextValue
    , preEscapedLazyTextValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue
    , unsafeLazyByteStringValue

      -- * Setting attributes
    , Attributable
    , (!)

      -- * Modifying Markup elements
    , contents
    , contents'
    , external
    , external'
    ) where

import Control.Monad
import Control.Monad.Trans
import Data.Monoid (Monoid, mappend, mempty, mconcat)
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Identity
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Exts (IsString (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)

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
newtype MarkupM m a = MarkupM { runMarkupM :: m (a, Markup) }

instance MonadTrans MarkupM where
    lift = MarkupM . liftM (\x -> (x, mempty))

instance (MonadIO m) => MonadIO (MarkupM m) where
    liftIO = MarkupM . liftM (\x -> (x, mempty)) . liftIO

data Markup
    -- | Tag, open tag, end tag, content
    = Parent StaticString StaticString StaticString Markup
    -- | Custom parent
    | CustomParent ChoiceString Markup
    -- | Tag, open tag, end tag
    | Leaf StaticString StaticString StaticString
    -- | Custom leaf
    | CustomLeaf ChoiceString Bool
    -- | HTML content
    | Content ChoiceString
    -- | Concatenation of two HTML pieces
    | Append Markup Markup
    -- | Add an attribute to the inner HTML. Raw key, key, value, HTML to
    -- receive the attribute.
    | AddAttribute StaticString StaticString ChoiceString Markup
    -- | Add a custom attribute to the inner HTML.
    | AddCustomAttribute ChoiceString ChoiceString Markup
    -- | Empty HTML.
    | Empty
    deriving (Typeable)

-- | Simplification of the 'MarkupM' datatype.
--
type PureMarkup = forall m. Monad m => MarkupM m ()

instance Monoid Markup where
    mempty = Empty
    {-# INLINE mempty #-}
    mappend = Append
    {-# INLINE mappend #-}
    mconcat = foldr Append Empty
    {-# INLINE mconcat #-}

instance (Monad m, Monoid a) => Monoid (MarkupM m a) where
    mempty = MarkupM $ return (mempty, mempty)
    {-# INLINE mempty #-}
    mappend (MarkupM l) (MarkupM r) = MarkupM $ do
        (x, ml) <- l
        (y, mr) <- r
        return (mappend x y, mappend ml mr)
    {-# INLINE mappend #-}
    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

instance (Functor f) => Functor (MarkupM f) where
    fmap f (MarkupM m) = MarkupM $ fmap (\(x, r) -> (f x, r)) m

instance (Monad m) => Monad (MarkupM m) where
    return x = MarkupM $ return (x, Empty)
    {-# INLINE return #-}
    (>>) (MarkupM l) (MarkupM r) = MarkupM $ do
        (_, ml) <- l
        (y, mr) <- r
        return (y, mappend ml mr)
    {-# INLINE (>>) #-}
    (MarkupM m) >>= f = MarkupM $ do
        (x, ml) <- m
        let (MarkupM m') = f x
        (y, mr) <- m'
        return (y, mappend ml mr)
    {-# INLINE (>>=) #-}

instance (Monad m) => IsString (MarkupM m a) where
    fromString s = MarkupM $ return (undefined, Content $ fromString s)
    {-# INLINE fromString #-}

-- | Type for an HTML tag. This can be seen as an internal string type used by
-- BlazeMarkup.
--
newtype Tag = Tag { unTag :: StaticString }
    deriving (IsString)

-- | Type for an attribute.
--
newtype Attribute = Attribute (forall a m. Functor m => MarkupM m a -> MarkupM m a)

instance Monoid Attribute where
    mempty                            = Attribute id
    Attribute f `mappend` Attribute g = Attribute (g . f)

-- | The type for the value part of an attribute.
--
newtype AttributeValue = AttributeValue { unAttributeValue :: ChoiceString }
    deriving (IsString, Monoid)

-- | Create a custom parent element
customParent :: Functor m => Tag     -- ^ Element tag
             -> MarkupM m () -- ^ Content
             -> MarkupM m () -- ^ Resulting markup
customParent tag = applyToContent $ CustomParent (Static $ unTag tag)

-- | Create a custom leaf element
customLeaf :: Monad m => Tag -- ^ Element tag
           -> Bool           -- ^ Close the leaf?
           -> MarkupM m ()   -- ^ Resulting markup
customLeaf tag close = MarkupM $ return (mempty, CustomLeaf (Static $ unTag tag) close)

-- | Create an HTML attribute that can be applied to an HTML element later using
-- the '!' operator.
--
attribute :: Tag             -- ^ Raw key
          -> Tag             -- ^ Shared key string for the HTML attribute.
          -> AttributeValue  -- ^ Value for the HTML attribute.
          -> Attribute       -- ^ Resulting HTML attribute.
attribute rawKey key value = Attribute $ applyToContent $
    AddAttribute (unTag rawKey) (unTag key) (unAttributeValue value)
{-# INLINE attribute #-}

-- | From HTML 5 onwards, the user is able to specify custom data attributes.
--
-- An example:
--
-- > <p data-foo="bar">Hello.</p>
--
-- We support this in BlazeMarkup using this funcion. The above fragment could
-- be described using BlazeMarkup with:
--
-- > p ! dataAttribute "foo" "bar" $ "Hello."
--
dataAttribute :: Tag             -- ^ Name of the attribute.
              -> AttributeValue  -- ^ Value for the attribute.
              -> Attribute       -- ^ Resulting HTML attribute.
dataAttribute tag value = Attribute $ applyToContent $ AddCustomAttribute
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
customAttribute tag value = Attribute $ applyToContent $ AddCustomAttribute
    (Static $ unTag tag)
    (unAttributeValue value)
{-# INLINE customAttribute #-}

-- | Render text. Functions like these can be used to supply content in HTML.
--
text :: Monad m => Text  -- ^ Text to render.
     -> MarkupM m ()     -- ^ Resulting HTML fragment.
text t = MarkupM $ return (mempty, Content $ Text t)
{-# INLINE text #-}

-- | Render text without escaping.
--
preEscapedText :: Monad m => Text  -- ^ Text to insert
               -> MarkupM m ()     -- ^ Resulting HTML fragment
preEscapedText t = MarkupM $ return (mempty, Content $ PreEscaped $ Text t)
{-# INLINE preEscapedText #-}

-- | A variant of 'text' for lazy 'LT.Text'.
--
lazyText :: Monad m => LT.Text  -- ^ Text to insert
         -> MarkupM m ()        -- ^ Resulting HTML fragment
lazyText = mconcat . map text . LT.toChunks
{-# INLINE lazyText #-}

-- | A variant of 'preEscapedText' for lazy 'LT.Text'
--
preEscapedLazyText :: Monad m => LT.Text  -- ^ Text to insert
                   -> MarkupM m ()        -- ^ Resulting HTML fragment
preEscapedLazyText = mconcat . map preEscapedText . LT.toChunks

-- | Create an HTML snippet from a 'String'.
--
string :: Monad m => String  -- ^ String to insert.
       -> MarkupM m ()       -- ^ Resulting HTML fragment.
string s = MarkupM $ return (mempty, Content $ String s)
{-# INLINE string #-}

-- | Create an HTML snippet from a 'String' without escaping
--
preEscapedString :: Monad m => String  -- ^ String to insert.
                 -> MarkupM m ()       -- ^ Resulting HTML fragment.
preEscapedString s = MarkupM $ return (mempty, Content $ PreEscaped $ String s)
{-# INLINE preEscapedString #-}

-- | Insert a 'ByteString'. This is an unsafe operation:
--
-- * The 'ByteString' could have the wrong encoding.
--
-- * The 'ByteString' might contain illegal HTML characters (no escaping is
--   done).
--
unsafeByteString :: Monad m => ByteString  -- ^ Value to insert.
                 -> MarkupM m ()           -- ^ Resulting HTML fragment.
unsafeByteString b = MarkupM $ return (mempty, Content $ ByteString b)
{-# INLINE unsafeByteString #-}

-- | Insert a lazy 'BL.ByteString'. See 'unsafeByteString' for reasons why this
-- is an unsafe operation.
--
unsafeLazyByteString :: Monad m => BL.ByteString  -- ^ Value to insert
                     -> MarkupM m ()              -- ^ Resulting HTML fragment
unsafeLazyByteString = mconcat . map unsafeByteString . BL.toChunks
{-# INLINE unsafeLazyByteString #-}

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

instance Functor m => Attributable (MarkupM m a) where
    h ! (Attribute f) = f h
    {-# INLINE (!) #-}

instance Functor m => Attributable (MarkupM m a -> MarkupM m a) where
    h ! f = (! f) . h
    {-# INLINE (!) #-}

applyToContent :: Functor m => (Markup -> Markup) -> MarkupM m a -> MarkupM m a
applyToContent f (MarkupM m) = MarkupM $ fmap (\(x, mr) -> (x, f mr)) m

-- | Mark HTML as external data. External data can be:
--
-- * CSS data in a @<style>@ tag;
--
-- * Script data in a @<script>@ tag.
--
-- This function is applied automatically when using the @style@ or @script@
-- combinators.
--
external :: Functor m => MarkupM m a -> MarkupM m a
external = applyToContent external'

external' :: Markup -> Markup
external' (Content x) = Content $ External x
external' (Append x y) = Append (external' x) (external' y)
external' (Parent x y z i) = Parent x y z $ external' i
external' (CustomParent x i) = CustomParent x $ external' i
external' (AddAttribute x y z i) = AddAttribute x y z $ external' i
external' (AddCustomAttribute x y i) = AddCustomAttribute x y $ external' i
external' x = x
{-# INLINE external' #-}

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
contents :: Functor m => MarkupM m a -> MarkupM m a
contents = applyToContent contents'

contents' :: Markup -> Markup
contents' (Parent _ _ _ c)           = contents' c
contents' (CustomParent _ c)         = contents' c
contents' (Content c)                = Content c
contents' (Append c1 c2)             = Append (contents' c1) (contents' c2)
contents' (AddAttribute _ _ _ c)     = contents' c
contents' (AddCustomAttribute _ _ c) = contents' c
contents' _                          = Empty
