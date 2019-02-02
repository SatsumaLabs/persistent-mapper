{-# LANGUAGE ScopedTypeVariables, RankNTypes, GADTs, TypeFamilies, TypeApplications, LambdaCase, OverloadedStrings #-}

module Database.Persist.Mapper.Util where

{-|
Module: Database.Persist.Mapper.Util
Description: functions useful outside of 'PersistEntity' instances
Copyright: © 2018-2019 Satsuma labs

This module defines additional functions (mainly lenses) for use outside of 'PersistEntity' instances.
-}

import Database.Persist.Mapper.InstanceHelpers
import Database.Persist
import Control.Lens

-- | Collect the difference between two Wntity values as an SQL-compatible 'Update'
-- Compares values only, not the keys.
sqlDiff :: (SimplePersistEntity e) => Entity e -> Entity e -> [Update e]
sqlDiff old new = allFields >>= diffField where
    diffField (SEF f) = let
        l = fieldLens f
        oldf = old ^. l
        newf = new ^. l
        in if oldf /= newf then [f =. newf] else []

-- | Filter to check if a field is between two values (invlusive)
inRange :: (PersistField a) => EntityField record a -> (a,a) -> Filter record
inRange field (lo,hi) = FilterAnd [field >=. lo, field <=. hi]

-- | Lens for key of an 'Entity'
entKey :: (PersistEntity a) => Lens' (Entity a) (Key a)
entKey f (Entity k v) = fmap (\k' -> Entity k' v) (f k)

-- | Lens for the value of an 'Entity'
entVal :: (PersistEntity a) => Lens' (Entity a) a
entVal f (Entity k v) = fmap (Entity k) (f v)

-- | Lift a lens through a maybe to become a psuedo-lens.
-- This functions corectly as a getter and will function as a lens on 'Just' values.
-- Updates will fail on 'Nothing' values.
unsafeLiftLensMaybe :: Lens' a b -> Lens' (Maybe a) (Maybe b)
unsafeLiftLensMaybe l f (Just x) = fmap outerfunc . f . Just $ view l x
    where outerfunc (Just y) = Just (set l y x)
          outerfunc Nothing = Nothing
unsafeLiftLensMaybe _ f Nothing = fmap (const Nothing) (f Nothing)


-- | Class for entities with natural keys allowing for lenses which correctly handle the duplicate keys in 'Entity'.
class (PersistEntity a) => HasNaturalKey a where
    -- | Derives the key from the value
    natKey :: Lens' a (Key a)


-- | Turns a record with a natural key into its Entity
natEntity :: (HasNaturalKey a) => a -> Entity a
natEntity x = Entity (x ^. natKey) x

-- | Lens for value of an entity which recalculates a natural key on update
entNatVal :: (HasNaturalKey a) => Iso' (Entity a) a
entNatVal = iso entityVal natEntity

-- | Lens for key of 'Entity' of a natural key record which updates both copies
entNatKey :: (HasNaturalKey a) => Lens' (Entity a) (Key a)
entNatKey = entNatVal . natKey
