# persistent-mapper

Yesod's persistent library is used to map database records to Haskell data structures, with those mappings being specified as instances of the `PersistEntity` typeclass.
While this class is capable of representing any isomorphism between the data structure and the underlying database row,
instances of it are somewhat messy to write by hand, and are usually only generated via Template Haskell through `Database.Persist.TH`,
which is limited to generating data structures which are a flat record with a one-to-one correspondence between Haskell fields and SQL fields.

The Aim of this library is to provide a set of building blocks that can be used to write `PersistEntity` instances for non-trivial isomorphisms.
This allows a number of useful features, such as multi-column fields and computed columns.
