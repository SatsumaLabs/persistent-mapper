# persistent-mapper

Yesod's persistent library is used to map database records to Haskell data structures, with those mappings being specified as instances of the `PersistEntity` typeclass.
While this class is capable of representing any isomorphism between the data structure and the underlying database row,
instances of it are somewhat messy to write by hand, and are usually only generated via Template Haskell through `Database.Persist.TH`,
which is limited to generating data structures which are a flat record with a one-to-one correspondence between Haskell fields and SQL fields.

The aim of this library is to provide a set of building blocks that can be used to manually write `PersistEntity` for existing data types.
This allows for more complex structures than a flat record of columns, with non-trivial isomorphisms to the database row,
allowing for structures such as multi-column composite fields and computed columns. 
