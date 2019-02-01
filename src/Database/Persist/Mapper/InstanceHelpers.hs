{-# LANGUAGE ScopedTypeVariables, RankNTypes, GADTs, TypeFamilies, TypeApplications, LambdaCase, OverloadedStrings #-}

module Database.Persist.Mapper.InstanceHelpers where

import Database.Persist
import Data.Text (Text, pack)
import Control.Lens (Lens')
import Data.Proxy
import Type.Reflection
import Database.Persist.Sql


-- * 'FieldDef' constructors

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

maybeFieldAttr :: FieldDef -> FieldDef
maybeFieldAttr fd = fd {fieldAttrs = ["Maybe"]}


cloneFieldDef :: forall target typ . (PersistEntity target) => Text -> EntityField target typ -> FieldDef
cloneFieldDef name f = (persistFieldDef f)
    { fieldHaskell = HaskellName name
    , fieldDB = DBName name }

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

defaultUniqueAttr :: FieldDef -> FieldDef
defaultUniqueAttr fd = fd {fieldAttrs = ["default=unique_rowid()"]}

keyFromCompositeValue :: (PersistEntity a) => PersistValue -> Either Text (Key a)
keyFromCompositeValue (PersistList l) = keyFromValues l
keyFromCompositeValue _ = Left "Expected composite PersistList"




-- * Parsing/Emitting

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

parseField :: (PersistField a) => FromFieldsA a
parseField = FromFieldsA $ \case
    [] -> Left "Not Enough Columns"
    c:cs -> case (fromPersistValue c, c) of
        (Right x, _) -> Right (cs, Just x)
        (Left _, PersistNull) -> Right (cs, Nothing)
        (Left err, _) -> Left err

collectNull :: FromFieldsA a -> FromFieldsA (Maybe a)
collectNull (FromFieldsA x) = FromFieldsA $ (fmap . fmap . fmap) Just x

runFromFieldsA :: FromFieldsA a -> [PersistValue] -> Either Text a
runFromFieldsA (FromFieldsA f) cs = case f cs of
    Left err -> Left err
    Right ([], Just x) -> Right x
    Right ([], Nothing) -> Left "Unexpected Null"
    Right (_:_, _) -> Left "Too Many Columns"

fromSingleField :: (PersistField a) => [PersistValue] -> Either Text a
fromSingleField = runFromFieldsA parseField



spf :: (PersistField a) => a -> SomePersistField
spf = SomePersistField




-- * Composite Fields

data SomeEntitySubfield a where
    SSF :: (PersistCompositeField a, PersistField f, Eq f) => EntitySubfield a f -> SomeEntitySubfield a

class PersistCompositeField a where
    data EntitySubfield a :: * -> *
    parseMultiField :: FromFieldsA a
    toMultiField :: a -> [SomePersistField]
    subfieldDef :: EntitySubfield a b -> Text -> FieldDef
    allSubfields :: [SomeEntitySubfield a]
    subfieldLens :: EntitySubfield a b -> Lens' a b

data SomeEntityField :: * -> * where
    SEF :: (PersistEntity e, PersistField f, Eq f) => EntityField e f -> SomeEntityField e

persistFieldDefSEF :: SomeEntityField e -> FieldDef
persistFieldDefSEF (SEF field) = persistFieldDef field

persistUniqueDefSEF :: Text -> [SomeEntityField e] -> UniqueDef
persistUniqueDefSEF n fields = UniqueDef {
    uniqueHaskell = HaskellName n,
    uniqueDBName = DBName n,
    uniqueFields = fmap ufield fields,
    uniqueAttrs = [] }
    where ufield (SEF field) = let fd = persistFieldDef field in (fieldHaskell fd, fieldDB fd)

allFieldsFrom :: (PersistEntity a, PersistCompositeField b) => (forall c. EntitySubfield b c -> EntityField a c) -> [SomeEntityField a]
allFieldsFrom fcon = fmap hoistfield allSubfields  where
    hoistfield (SSF sf) = SEF (fcon sf)

class (PersistCompositeField a) => PersistNullableCompositeField a where
    nullMultiField :: Proxy a -> [SomePersistField]
    nullMultiField _ = fmap (\(_::SomeEntitySubfield a) -> spf PersistNull) allSubfields
    subfieldLensMaybe :: EntitySubfield a b -> forall f. Functor f => (Maybe b -> f (Maybe b)) -> Maybe a -> f (Maybe a)




toMultiFieldMaybe :: forall a . (PersistNullableCompositeField a) => Maybe a -> [SomePersistField]
toMultiFieldMaybe (Just x) = toMultiField x
toMultiFieldMaybe Nothing = nullMultiField (Proxy @a)

allFieldsFromMaybe :: (PersistEntity a, PersistNullableCompositeField b) => (forall c. EntitySubfield b c -> EntityField a (Maybe c)) -> [SomeEntityField a]
allFieldsFromMaybe fcon = fmap hoistfield allSubfields  where
    hoistfield (SSF sf) = SEF (fcon sf)



-- 'EntityDef' Helpers

data SomeFieldValue a where
    SFV :: (PersistEntity a, PersistField f) => EntityField a f -> f -> SomeFieldValue a

class (PersistEntity e) => SimplePersistEntity e where
    allFields :: [SomeEntityField e]
    allUniqueKeys :: [(Text, [SomeEntityField e])]
    uniqueToFields :: Unique e -> [SomeFieldValue e]


simpleEntityDef :: forall e m. (SimplePersistEntity e) => Text -> Text -> m e -> EntityDef
simpleEntityDef hn dn = const EntityDef{
    entityHaskell = HaskellName hn,
    entityDB = DBName dn,
    entityId = persistFieldDef (persistIdField @e),
    entityAttrs = [],
    entityFields = fmap persistFieldDefSEF (allFields @e),
    entityUniques = fmap (uncurry persistUniqueDefSEF) (allUniqueKeys @e),
    entityForeigns = [],
    entityDerives = [],
    entityExtra = mempty,
    entitySum = False }

simpleUniqueToFieldNames :: (SimplePersistEntity e) => Unique e -> [(HaskellName, DBName)]
simpleUniqueToFieldNames = fmap (\(SFV field _) -> let fd = persistFieldDef field in (fieldHaskell fd, fieldDB fd)) . uniqueToFields

simpleUniqueToValues :: (SimplePersistEntity e) => Unique e -> [PersistValue]
simpleUniqueToValues = fmap (\(SFV _ val) -> toPersistValue val) . uniqueToFields
