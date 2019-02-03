{-# LANGUAGE ScopedTypeVariables, RankNTypes, GADTs, TypeFamilies, TypeApplications, LambdaCase, OverloadedStrings #-}

{-|
Module: Database.Persist.Mapper.ClassHelpers
Description: Building blocks for creating 'PersistEntity' instances
Copyright: © 2018-2019 Satsuma labs

This module defines a set of building blocks for manually writing PersistEntity instances,
allowing for mapping existing data types into a database.
-}

module Database.Persist.Mapper.ClassHelpers where

import Database.Persist
import Data.Text (Text, pack)
import Control.Lens (Lens')
import Data.Proxy
import Type.Reflection
import Database.Persist.Sql


-- * 'FieldDef' constructors

-- | Create a 'FieldDef' for a not null column. Takes in the field name and a proxy for its type.
simpleFieldDef :: forall typ . (PersistFieldSql typ, Typeable typ) => Text -> Proxy typ -> FieldDef
simpleFieldDef name p = FieldDef
    { fieldHaskell = HaskellName name
    , fieldDB = DBName name
    , fieldType = FTTypeCon Nothing . pack . show $ (typeRep :: TypeRep typ)
    , fieldSqlType = sqlType p
    , fieldAttrs = []
    , fieldStrict = True
    , fieldReference = NoReference
    }

-- | Create a FieldDef for a nullable column. The type proxy should be for the base (non 'Maybe') type.
-- @maybeFieldDef "name" (Proxy@typ)@ should correnpond to a field of type @Maybe typ@
-- unless @typ@ includes a null value in its 'PersistField' instance (such as 'Checkmark').
maybeFieldDef :: forall typ . (PersistFieldSql typ, Typeable typ) => Text -> Proxy typ -> FieldDef
maybeFieldDef name p = FieldDef
    { fieldHaskell = HaskellName name
    , fieldDB = DBName name
    , fieldType = FTTypeCon Nothing . pack . show $ (typeRep :: TypeRep typ)
    , fieldSqlType = sqlType p
    , fieldAttrs = ["Maybe"]
    , fieldStrict = True
    , fieldReference = NoReference
    }

-- | Adds a 'Maybe' attribute to a 'FieldDef', making the field nullable.
maybeFieldAttr :: FieldDef -> FieldDef
maybeFieldAttr fd = fd {fieldAttrs = ["Maybe"]}

-- | Create a 'FieldDef' for a foreign key column. Takes a foeld name and a type proxy for the target table.
foreignKeyFieldDef :: forall target . (PersistEntity target) => Text -> Proxy target -> FieldDef
foreignKeyFieldDef name _ = FieldDef
    { fieldHaskell = HaskellName name
    , fieldDB = DBName name
    , fieldType = fieldType targetDef
    , fieldSqlType = fieldSqlType targetDef
    , fieldAttrs = []
    , fieldStrict = True
    , fieldReference = ForeignRef (entityHaskell (entityDef (Proxy :: Proxy target))) (fieldType targetDef) }
    where targetDef = persistFieldDef (persistIdField :: EntityField target (Key target))

-- | Clones the definition of an existing field with a new name.
cloneFieldDef :: forall target typ . (PersistEntity target) => Text -> EntityField target typ -> FieldDef
cloneFieldDef name f = (persistFieldDef f)
    { fieldHaskell = HaskellName name
    , fieldDB = DBName name }

-- | Create a FieldDef for a composite primary key given a name and a list of the fields it reperesents. Does not define a column.
compositePKDef :: Text -> [SomeEntityField a] -> FieldDef
compositePKDef name cols = FieldDef {
        fieldHaskell = HaskellName name,
        fieldDB = DBName name,
        fieldType = FTTypeCon Nothing "Key",
        fieldSqlType = SqlOther "Composite Reference",
        fieldAttrs = [],
        fieldStrict = True,
        fieldReference = CompositeRef $ CompositeDef (fmap fieldRef cols) [] }
        where fieldRef (SEF field) = (persistFieldDef field) {fieldSqlType = SqlOther "SqlType unset", fieldReference = NoReference}

-- | Adds a @default=unique_rowid()@ attribute to a field for surrogate keys on CockroachDB.
defaultUniqueAttr :: FieldDef -> FieldDef
defaultUniqueAttr fd = fd {fieldAttrs = ["default=unique_rowid()"]}


-- * Parsing/Emitting

-- | Applicative parser over lists of 'PersistValue's. Use this to easily create 'fromPersistValues' methods.
newtype FromFieldsA a = FromFieldsA ([PersistValue] -> Either Text ([PersistValue], Maybe a))
instance Functor FromFieldsA where
    fmap f (FromFieldsA x) = FromFieldsA $ (fmap . fmap . fmap . fmap) f x

instance Applicative FromFieldsA where
    pure x = FromFieldsA $ \cs -> Right (cs, Just x)
    FromFieldsA af <*> FromFieldsA ax = FromFieldsA $ \cs -> case af cs of
        Left err -> Left err
        Right (cs', mf) -> case ax cs' of
            Left err -> Left err
            Right (cs'', mx) -> Right (cs'', mf <*> mx)

-- | Turn the applicative form into a reader with the signature of 'fromPersistValues'
runFromFieldsA :: FromFieldsA a -> [PersistValue] -> Either Text a
runFromFieldsA (FromFieldsA f) cs = case f cs of
    Left err -> Left err
    Right ([], Just x) -> Right x
    Right ([], Nothing) -> Left "Unexpected Null"
    Right (_:_, _) -> Left "Too Many Columns"

-- | Read a single field from the row
parseField :: (PersistField a) => FromFieldsA a
parseField = FromFieldsA $ \case
    [] -> Left "Not Enough Columns"
    c:cs -> case (fromPersistValue c, c) of
        (Right x, _) -> Right (cs, Just x)
        (Left _, PersistNull) -> Right (cs, Nothing)
        (Left err, _) -> Left err

-- | Collects any caught NULL values. Useful for nullable composite fields
collectNull :: FromFieldsA a -> FromFieldsA (Maybe a)
collectNull (FromFieldsA x) = FromFieldsA $ (fmap . fmap . fmap) Just x

-- | Parses a RersistValue from a singleton list. Used for writing 'keyFromValues' methods for single column keys
-- inverse to @return . toPersistValue@
fromSingleField :: (PersistField a) => [PersistValue] -> Either Text a
fromSingleField = runFromFieldsA parseField

-- | Wraps 'keyFromValues' to take a single 'PersistList' containing the relevant fields.
-- Used for writing 'PersistField' instances for composite keys.
keyFromCompositeValue :: (PersistEntity a) => PersistValue -> Either Text (Key a)
keyFromCompositeValue (PersistList l) = keyFromValues l
keyFromCompositeValue _ = Left "Expected composite PersistList"

-- | Short alias for 'SomePersistField'.
-- Useful for writing 'toPersistFields' methods
spf :: (PersistField a) => a -> SomePersistField
spf = SomePersistField




-- * Composite Fields

-- | Defined a mapping from a haskell type to a set of columns, defining a composite field.
-- The methods defined hare are sufficient to add these fields to a 'PersistEntity' definition without knlwledge of the underlying structure of the composite field.
class PersistCompositeField a where
    -- | Field pointers acting the same way as 'EntityField' values.
    -- To refer to the composite field (and its subfields) an 'EntityField' constructor can be defined of type
    -- @EntitySubfield subfield a -> EntityField entity a@
    data EntitySubfield a :: * -> *
    -- | Enumeration of all 'EntitySubfield' values
    allSubfields :: [SomeEntitySubfield a]
    -- | Parser from database columns, in the order defined in 'allSubfields'.
    parseMultiField :: FromFieldsA a
    -- | Emitter to database columns, in the order defined in 'allSubfields'.
    toMultiField :: a -> [SomePersistField]
    -- | FieldDef for each subfield given a base field name.
    subfieldDef :: EntitySubfield a b -> Text -> FieldDef
    -- | Lens in the haskell type for each FieldDef to the value of that field.
    subfieldLens :: EntitySubfield a b -> Lens' a b

-- | Additional methods for composite fields that can be included wrapped in 'Maybe'.
class (PersistCompositeField a) => PersistNullableCompositeField a where
    -- | Database columns for 'Nothing' value, defined by default.
    nullMultiField :: Proxy a -> [SomePersistField]
    nullMultiField _ = fmap (\(_::SomeEntitySubfield a) -> spf PersistNull) allSubfields
    -- | lenses from maybe type . These must at least function as getters although it is not always possible to update NULL values.
    subfieldLensMaybe :: EntitySubfield a b -> forall f. Functor f => (Maybe b -> f (Maybe b)) -> Maybe a -> f (Maybe a)

-- | Existential type repeesenting an 'EntitySubfield' pointer of unknown field type
data SomeEntitySubfield a where
    SSF :: (PersistCompositeField a, PersistField f, Eq f) => EntitySubfield a f -> SomeEntitySubfield a

-- | Wrapper for toMultiField handling a null case.
toMultiFieldMaybe :: forall a . (PersistNullableCompositeField a) => Maybe a -> [SomePersistField]
toMultiFieldMaybe (Just x) = toMultiField x
toMultiFieldMaybe Nothing = nullMultiField (Proxy @a)


-- * Entities

-- | Extension of 'PersistEntity' enumerating all field pointers and handling unique indices in much nicer values.
-- Allows for generic generation of 'entityDef' and unoque methods.
class (PersistEntity e) => SimplePersistEntity e where
    -- | Enumeration of all 'EntityField' values except that of the key
    allFields :: [SomeEntityField e]
    -- | Names and column sets of the unique indices.
    allUniqueKeys :: [(Text, [SomeEntityField e])]
    -- | Typesafe replacement of 'persistUniqueToValues' and 'persistUniqueToFieldNames' mapping a Unique value to a list of field/value pairs.
    uniqueToFields :: Unique e -> [SomeFieldValue e]

-- | Existential type repeesenting an 'EntitySubfield' pointer of unknown field type
data SomeEntityField :: * -> * where
    SEF :: (PersistEntity e, PersistField f, Eq f) => EntityField e f -> SomeEntityField e

-- | Reperesentation of a field pointer plus a value belonging to that field. Used for seperesenting subsets of a row.
data SomeFieldValue a where
    SFV :: (PersistEntity a, PersistField f) => EntityField a f -> f -> SomeFieldValue a

-- | Get all 'EntityField's from a subfield
allFieldsFrom :: (PersistEntity a, PersistCompositeField b) => (forall c. EntitySubfield b c -> EntityField a c) -> [SomeEntityField a]
allFieldsFrom fcon = fmap hoistfield allSubfields  where
    hoistfield (SSF sf) = SEF (fcon sf)

-- | Get all 'EntityField's from a nullable subfield
allFieldsFromMaybe :: (PersistEntity a, PersistNullableCompositeField b) => (forall c. EntitySubfield b c -> EntityField a (Maybe c)) -> [SomeEntityField a]
allFieldsFromMaybe fcon = fmap hoistfield allSubfields  where
    hoistfield (SSF sf) = SEF (fcon sf)



-- | Generate a 'UniqueDef' from a name and list of fields.
persistUniqueDefSEF :: Text -> [SomeEntityField e] -> UniqueDef
persistUniqueDefSEF n fields = UniqueDef {
    uniqueHaskell = HaskellName n,
    uniqueDBName = DBName n,
    uniqueFields = fmap ufield fields,
    uniqueAttrs = [] }
    where ufield (SEF field) = let fd = persistFieldDef field in (fieldHaskell fd, fieldDB fd)

-- | Create an EntityDef given an entity name (in both Haskell and database forms) and a type proxy for the entity implementing 'SimplePersistEntity'.
simpleEntityDef :: forall e m. (SimplePersistEntity e) => Text -> Text -> m e -> EntityDef
simpleEntityDef hn dn = const EntityDef{
    entityHaskell = HaskellName hn,
    entityDB = DBName dn,
    entityId = persistFieldDef (persistIdField @e),
    entityAttrs = [],
    entityFields = fmap (\(SEF field) -> persistFieldDef field) (allFields @e),
    entityUniques = fmap (uncurry persistUniqueDefSEF) (allUniqueKeys @e),
    entityForeigns = [],
    entityDerives = [],
    entityExtra = mempty,
    entitySum = False }

-- | implementation using 'SimplePersistEntity'
simpleUniqueToFieldNames :: (SimplePersistEntity e) => Unique e -> [(HaskellName, DBName)]
simpleUniqueToFieldNames = fmap (\(SFV field _) -> let fd = persistFieldDef field in (fieldHaskell fd, fieldDB fd)) . uniqueToFields

-- | implementation using 'SimplePersistEntity'
simpleUniqueToValues :: (SimplePersistEntity e) => Unique e -> [PersistValue]
simpleUniqueToValues = fmap (\(SFV _ val) -> toPersistValue val) . uniqueToFields
