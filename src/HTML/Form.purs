module HTML.Form
  ( class FromForm
  , class FromFormAtom
  , class ToForm
  , fromForm
  , toForm
  , fromFormAtom
  , IntError(..)
  ) where

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Validation.Semigroup (V, invalid)
import Prelude

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Type class for things that form data can be transformed into.
class FromForm e a | a -> e where
  fromForm :: String -> Map String (NonEmptyList String) -> V (NonEmptyList e) a

-- | Type class for things that use at most one value from the non-empty list
-- | of values.
class FromForm e a <= FromFormAtom e a | a -> e

fromFormAtom :: ∀ e a. FromFormAtom e a => String -> Maybe String -> V (NonEmptyList e) a
fromFormAtom k v = fromForm k (maybe Map.empty (Map.singleton k <<< pure) v)

-- | Type class for things that can be transformed into form data.
class ToForm a where
  toForm :: String -> a -> Map String (NonEmptyList String)

--------------------------------------------------------------------------------

instance fromFormVoid :: FromForm Unit Void where
  fromForm _ _ = invalid (pure unit)

instance fromFormAtomVoid :: FromFormAtom Unit Void

instance toFormVoid :: ToForm Void where
  toForm _ = absurd

--------------------------------------------------------------------------------

instance fromFormUnit :: FromForm e Unit where
  fromForm _ _ = pure unit

instance fromFormAtomUnit :: FromFormAtom e Unit

instance toFormUnit :: ToForm Unit where
  toForm _ _ = Map.empty

--------------------------------------------------------------------------------

instance fromFormBoolean :: FromForm e Boolean where
  fromForm k vs = pure $ Map.member k vs

instance fromFormAtomBoolean :: FromFormAtom e Boolean

instance toFormBoolean :: ToForm Boolean where
  toForm _ false = Map.empty
  toForm k true = Map.singleton k (pure "on")

--------------------------------------------------------------------------------

data IntError
  = IntMissing
  | IntBadParse

instance fromFormInt :: FromForm IntError Int where
  fromForm k vs = case Int.fromString <<< NEL.head <$> Map.lookup k vs of
    Nothing -> invalid $ pure IntMissing
    Just Nothing -> invalid $ pure IntBadParse
    Just (Just x) -> pure x

instance fromFormAtomInt :: FromFormAtom IntError Int

instance toFormInt :: ToForm Int where
  toForm k x = Map.singleton k (pure $ show x)

--------------------------------------------------------------------------------

instance fromFormString :: FromForm Unit String where
  fromForm k vs = maybe (invalid $ pure unit) pure $ NEL.head <$> Map.lookup k vs

instance fromFormAtomString :: FromFormAtom Unit String

instance toFormString :: ToForm String where
  toForm k x = Map.singleton k (pure x)

--------------------------------------------------------------------------------

fstK :: String -> String
fstK = (_ <> ".fst")

sndK :: String -> String
sndK = (_ <> ".snd")

instance fromFormTuple :: (FromForm e a, FromForm e b) => FromForm e (a /\ b) where
  fromForm k vs = (/\) <$> fromForm (fstK k) vs <*> fromForm (sndK k) vs

instance toFormTuple :: (ToForm a, ToForm b) => ToForm (a /\ b) where
  toForm k (a /\ b) = toForm k a <> toForm k b

--------------------------------------------------------------------------------

instance fromFormArray :: (FromFormAtom e a) => FromForm e (Array a) where
  fromForm k vs =
    traverse (fromFormAtom k <<< pure) $
      foldMap Array.fromFoldable $ Map.lookup k vs

instance toFormArray :: (ToForm a) => ToForm (Array a) where
  toForm k xs = foldMap (toForm k) xs
