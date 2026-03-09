# Serth 

`serth` is a serialisation framework inspired by Rust's `serde`: it uses meta-programming and code generation to generate specialised serialisation functions at compile time. 
It is designed so that users can define their own serialisation layout. The library comes with support for JSON and XML.

Compared to Haskell's `aeson`, serialisation to JSON using `serth` is around 4 times faster.

`serth-base` provides internal utilities, especially to retrieve information about data types. `serth-serialiser` is the main serialisation library. 

## Example

```haskell
import Data.Serth.Serialiser
import Data.Serth.Serialiser.Format.JSON
import Data.Serth.Serialiser.Format.XML

newtype MySimpleRecord = MSR {a :: Int} deriving (Show)

genSerialisable @JSON ''MySimpleRecord
genSerialisable @XML ''MySimpleRecord

main = do 
    let r = MSR 1
    print $ serialise @JSON r
    print $ serialise @XML r
```

## `serth-json-parser`

`serth-json-parser` is a secondary library that allows generating specialised JSON parsers at compile time. Same as for `serth-serialiser`, it is 3-4x faster than `aeson`.

```haskell
import Data.JSON.FromJSON 
import Data.JSON.TH 
import FlatParse.Basic hiding (Parser)

genFromJSON ''MySimpleRecord

main = case runParser parse "{\"a\": 1}" of
    OK r@(MSR a' ) _ -> print r
    e -> fail $ "Expected OK, got " ++ show e
```

