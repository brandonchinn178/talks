theme: Plain Jane
footer: Haskell Love 2020 | `aeson-schemas`
slidenumbers: true
autoscale: true
code: auto(95), Menlo, line-height(1.1)
background-color: #635C8C
text: #FFFFFF
header: #FFFFFF
table-separator: #FFFFFF
footnote-separator: #FFFFFF

[.hide-footer]
[.slidenumbers: false]

# `aeson-schemas`: Safely extract JSON data when data types are too cumbersome[^1]

### Brandon Chinn<br /><br />1 August 2020

[^1]: [http://hackage.haskell.org/package/aeson-schemas](http://hackage.haskell.org/package/aeson-schemas)

---

# Agenda

* Motivation (5 min)

* Using `aeson-schemas` (10 min)

* Implementing `aeson-schemas` (10 min)

    * Type-level programming 101

* Final Thoughts (5 min)

^
• First part go over background + usage
• Second part go over key insights that make this library work
• Structured talk to be easy to follow, as long as comfortable with basic Haskell concepts

---

# Motivation

---

# Motivation
## Parsing data with `aeson`

```hs
class ToJSON a where
  toJSON :: a -> Value

class FromJSON a where
  parseJSON :: Value -> Parser a
```

^
• `aeson` standard library
• Convert to/from JSON `Value`
• Focus on `FromJSON`

---

# Motivation
## Parsing data with `aeson`

[.column]

```json
{
    "users": [
        {
            "id": 1,
            "name": "Alice"
        },
        ...
    ]
}
```

[.column]

```hs
data User = User
  { id   :: Int
  , name :: String
  }
  deriving
    ( Show
    , Generic
    , FromJSON
    )
```

^
• Usually wouldn't implement manually
• Derive with `Generic`

---

# Motivation
## Parsing data with `aeson`

[.column]

```json
{
  "name": "Policy1",
  "permissions": [
    {
      "resource": {
        "name": "secretdata.txt",
        "owner": "john@example.com"
      },
      "access": "READ"
    }
  ]
}
```

[.column]

```hs
data Result = Result
  { name        :: String
  , permissions :: [Permission]
  }

data Permission = Permission
  { resource :: Maybe Resource
  , access   :: String
  }

data Resource = Resource
  { name  :: String
  , owner :: Maybe String
  }
```

^
• Can't inline data types (`[Permission]`, `Maybe Resource`)
• Duplicate `name` field
• Can change field names if manually derive, but still need all 3

---

# Motivation
## Querying a GraphQL API

[.column]

```
type Query {
    users: [User!]!
}
type User {
    id: ID!
    name: String!
    posts: [Post!]!
}
type Post {
    id: ID!
    name: String!
    createdAt: String!
}
```

[.column]

```
query {
    users {
        id
        name
        posts {
            id
            name
        }
    }
}
```

^
• Alternative to REST, send structured query that specifies the particular fields you want from the server
• Can omit `Post.createdAt`
• Response guaranteed to match the format of the query
• Haskell types?

---

# Motivation
## Querying a GraphQL API

[.column]

```hs
data Query = Query
  { users :: Maybe [User]
  }

data User = User
  { id    :: Maybe String
  , name  :: Maybe String
  , posts :: Maybe [Post]
  }

data Post = Post
  { id        :: Maybe ID
  , name      :: Maybe String
  , createdAt :: Maybe String
  }
```

[.column]

[.build-lists: true]

* Pros

    * Direct translation of GraphQL schema

* Cons

    * Handle `Nothing` / use `fromJust`
    * `id` field name shadows `Prelude.id`
    * Duplicate `name` field

^
• One-to-one mapping
• Always handle `Nothing`, even if a particular query returns field

---

# Motivation
## Querying a GraphQL API

[.column]

```hs
data Query1 = Query1
  { users :: [User1]
  }

data User1 = User1
  { id    :: String
  , name  :: String
  , posts :: [Post1]
  }

data Post1 = Post1
  { id   :: String
  , name :: String
  }
```

[.column]

[.build-lists: true]

* Pros

    * No more `Maybe`

* Cons

    * Redefine type per use
    * Record names still duplicated

^
• More run-time safety
• Could generate
• Won't scale (import X types per query)

---

[.build-lists: true]

# Problem Requirements

1. Type safe
1. Avoid polluting namespace
1. Nice query language

^ Not `aeson`'s fault, but clunkiness of Haskell data type definition

^
1. We need type safety (we're Haskell devs, after all!), it'd be nice for compiler to check key
2. Shadow `user` variable, reserved keywords (`type` or `data`)
3. Record fields + lenses not great for nested data

---

# Using `aeson-schemas`

---

# Using `aeson-schemas`

[.column]

[.code-highlight: all]
[.code-highlight: none]

```hs
import Data.Aeson.Schema (schema)

type MySchema = [schema|
  {
    users: List {
      id: Int,
      name: Text,
    },
  }
|]
```

[.column]

[.code-highlight: none]
[.code-highlight: all]

```hs
import Data.Aeson (decodeFileStrict)
import Data.Aeson.Schema (Object, get)

obj <- fromJust <$>
  decodeFileStrict "example.json"
    :: IO (Object MySchema)

-- outputs:
-- ["Alice", "Bob", "Claire"]
print [get| obj.users[].name |]
```

^
1st column:
• Define schema as `MySchema` with `schema` quasiquoter

^
2nd column:
• Decode `Object MySchema` with standard decode
• `get` quasiquoter

^
Problem requirements:
1. Type safe: `obj.users[].foo`
2. Namespace: just `MySchema`
3. `get` quasiquoter: nicer syntax

---

# Using `aeson-schemas`
## `schema` quasiquoter

[.column]

```hs
type BasicSchema = [schema|
  {
    a: Bool,
    b: Int,
    c: Double,
    d: Text,
    e: UTCTime,
  }
|]
```

[.column]

```hs
type ComplexSchema = [schema|
  {
    foo: List {
      a: Int,
      b: Maybe Text,
    },
    bar: List Maybe Bool,
  }
|]
```

^
• Specify any type that has a `FromJSON` instance
• Right associative: `bar` = `[Maybe Bool]`

---

# Using `aeson-schemas`
## `get` quasiquoter

[.code-highlight: all]
[.code-highlight: 1]
[.code-highlight: 3]
[.code-highlight: 5-7]

```hs
let users = [get| obj.a.b.users |]

map [get| .name |] users

-- compare:
--    map (fmap c . b) (a obj)
[get| obj.a[].b?.c |]
```

^
• `jq`-like syntax
• 1st line: nested keys, keys checked at compile-time
• 2nd line: lambda function, `users` = normal list
• 3rd line: apply ops through lists + Maybes, better syntax than plain Haskell

^ DEMO: `./example.sh`

---

# Using `aeson-schemas`

[.column]

### GraphQL query

```
query {
    users {
        id
        name
        posts {
            id
            name
        }
    }
}
```

[.column]

### `aeson-schemas` schema

```hs
type Query1 = [schema|
  {
    users: List {
      id: Text,
      name: Text,
      posts: List {
        id: Text,
        name: Text,
      },
    },
  }
|]
```

^
• 3 data types -> 1 type alias
• `id`, `name` record fields namespace

---

# Implementing `aeson-schemas`

^ Pause for questions here if time

^ Intro to type-level programming + general overview of `aeson-schemas`

---

# Type-level programming

| Value | Type | Kind[^2] |
|-------|------|------|
| `True`, `False` | `Bool` | `*` |
| `Just 1`, `Nothing` | `Maybe Int` | `*` |
| N/A | `Maybe` | `* -> *` |

[^2]: `*` is actually deprecated in favor of `Type` from `Data.Kind`, but I like how `*` looks better, so that's why I'm using it.

^
• Different types, same kind
• Only types with kind `*` can have values

---

# Type-level programming
## With `-XDataKinds`

| Value | Type | Kind |
|-------|------|------|
| `True`, `False` | `Bool` | `*` |
| N/A | `'True`, `'False` | `Bool` |

^
• Only values with type `Bool`
• Now also types with kind `Bool`
• Single quote = constructor used as type

---

# Type-level programming
## Demo: `Restaurant.hs`

^ DEMO: `./restaurant.sh`

^
• Restrict certain actions by encoding requirements in the types
• Types go away after compilation, use type classes to do different things based on type
• Type applications to know which `statusLabel` to print

---

# Type-level programming
## Type families

```hs
type family Foo a where
  Foo Int = [Int]
  Foo Bool = Maybe Bool

x :: Foo Int
x = [1, 2, 3]

y :: Foo Bool
y = Just True
```

^
• Functions on types instead of values
• `Foo Int` just an alias for `[Int]`, etc.

^ Should now know enough to implement `aeson-schemas`

---

# Implementing `aeson-schemas`

[.build-lists: true]

1. Define the schema
2. Parse JSON data into `Object`
3. Extract data from `Object`

---

# Implementing `aeson-schemas`
## Defining the schema

```hs
import GHC.TypeLits (Symbol)

data SchemaType
  = SchemaInt
  | SchemaText
  | SchemaList SchemaType
  | SchemaObject [(Symbol, SchemaType)]
```

^
• Data type to store schema in type
• Subset of `aeson-schemas` features
• `Symbol`: type-level `String`

---

# Implementing `aeson-schemas`
## Defining the schema

```hs
{-# LANGUAGE DataKinds #-}

type MySchema = 'SchemaObject
  '[ '( "users"
      , 'SchemaList (
          'SchemaObject
            '[ '("id", 'SchemaInt)
             , '("name", 'SchemaText)
             ]
        )
      )
   ]
```

^
• Can already do this!
• Single quotes for type-level lists + tuples

---

# Implementing `aeson-schemas`
## Parsing data into `Object`

[.code-highlight: 1]
[.code-highlight: 7-11]
[.code-highlight: 13-14]
[.code-highlight: 3-5]

```hs
data Object (schema :: SchemaType) = UnsafeObject (HashMap Text Dynamic)

instance (IsSchemaType schema, SchemaResult schema ~ Object schema)
    => FromJSON (Object schema) where
  parseJSON = parseValue @schema

type family SchemaResult (schema :: SchemaType) where
  SchemaResult 'SchemaInt = Int
  SchemaResult 'SchemaText = Text
  SchemaResult ('SchemaList inner) = [SchemaResult inner]
  SchemaResult ('SchemaObject schema) = Object ('SchemaObject schema)

class IsSchemaType (schema :: SchemaType) where
  parseValue :: Value -> Parser (SchemaResult schema)
```

^
`Object` type
• Stores schema as type variable (always `SchemaObject`)
• Will parse JSON `Value` to Haskell type, then store as `Dynamic`
• As long as we don't export `UnsafeObject`, `Dynamic` will correspond with schema

^
`SchemaResult`
• Type family to map schema to corresponding Haskell type
• `SchemaList` reapplies `SchemaResult` to `inner`
• `SchemaObject` corresponds to our `Object` type with the `SchemaObject` stored as its schema

^
`IsSchemaType`
• Type class for each `SchemaType` to parse a `Value` to corresponding Haskell type

^
`FromJSON`
• Parsing `Object` = parsing schema
• Bonus: pass path of keys to `parseValue` for nice error message
• DEMO: `./badDecode.sh`

---

# Implementing `aeson-schemas`
## Parsing data into `Object`

```hs
instance IsSchemaType 'SchemaInt where
  parseValue = Aeson.parseJSON -- :: Value -> Parser Int

instance IsSchemaType 'SchemaText where
  parseValue = Aeson.parseJSON -- :: Value -> Parser Text

instance IsSchemaType inner => IsSchemaType ('SchemaList inner) where
  parseValue (Aeson.Array a) = traverse (parseValue @inner) (Vector.toList a)
  parseValue _ = fail "..."
```

^
• Simple types like `Int` and `Text` can just use the standard `parseJSON`
• `SchemaList`: parse `Value`s in JSON array according to `inner`

---

# Implementing `aeson-schemas`
## Parsing data into `Object`

[.code-highlight: all]
[.code-highlight: 4]
[.code-highlight: 6]
[.code-highlight: 8]
[.code-highlight: 10]
[.code-highlight: 14-15]
[.code-highlight: all]

```hs
-- ref: SchemaObject [(Symbol, SchemaType)]
instance (...) => IsSchemaType ('SchemaObject ('(key, inner) ': rest)) where
  parseValue value@(Aeson.Object o) = do
    let key = Text.pack $ symbolVal (Proxy @key)

    inner <- parseValue @inner (HashMap.lookupDefault Aeson.Null key o)

    UnsafeObject rest <- parseValue @rest value

    return $ UnsafeObject $ HashMap.insert key (toDyn inner) rest

  parseValue _ = fail "..."

instance IsSchemaType ('SchemaObject '[]) where
  parseValue (Aeson.Object _) = return $ UnsafeObject HashMap.empty
  parseValue _ = fail "..."
```

^
• Convert type-level key to `String`
• Look up key in JSON object + parse according to `inner`
• Parse the rest of the schema
• Insert key + parsed value, after converting to `Dynamic`

^
• Base case: empty `Object`

---

# Implementing `aeson-schemas`
## Extracting data from `Object`

```hs
let o :: Object ('SchemaObject '[ '("foo", 'SchemaInt) ])
    o = ...

getKey @"foo" o :: Int
```

^ To extract data, we'll write a function that should work like this

---

# Implementing `aeson-schemas`
## Extracting data from `Object`

[.code-highlight: 1-4,6,7,8,13]
[.code-highlight: 9-12]

```hs
-- Fcf.Lookup    :: a -> [(a, b)] -> Fcf.Exp (Maybe b)
-- Fcf.FromMaybe :: a -> Maybe a -> Fcf.Exp a
-- Fcf.=<<       :: (a -> Fcf.Exp b) -> Fcf.Exp a -> Fcf.Exp b
-- Fcf.Eval      :: Fcf.Exp a -> a

type family LookupSchema (key :: Symbol) (schema :: SchemaType) where
  LookupSchema key ('SchemaObject schemaTypeMap) = Fcf.Eval (
    Fcf.FromMaybe (
        TypeError (
              'Text "Key '" ':<>: 'Text key
        ':<>: 'Text "' does not exist in the following schema:"
        ':$$: 'ShowType schemaTypeMap )
    ) =<< Fcf.Lookup key schemaTypeMap )
```

^
• Type family `Symbol -> SchemaType -> SchemaType`
• Straightforward, thanks to `first-class-families`
• Custom error message at compile time

---

# Implementing `aeson-schemas`
## Extracting data from `Object`

[.code-highlight: 1-3]
[.code-highlight: all]

```hs
getKey
  :: forall key initialSchema. (...)
  => Object initialSchema -> SchemaResult (LookupSchema key initialSchema)
getKey (UnsafeObject o) =
  fromMaybe (error "This should not happen") $
    fromDynamic (o ! Text.pack key)
  where
    key = symbolVal (Proxy @key)
```

^
• Explain type definition
• **Compiler can infer resulting type, given the initial schema and a key**
• Get `Dynamic` at `key`
• Convert `Dynamic` to the Haskell type inferred with `SchemaResult`
• Should never fail

---

# Implementing `aeson-schemas`

```hs
type MySchema = 'SchemaObject
  '[ '( "users",
        'SchemaList (
          'SchemaObject '[ '("id", 'SchemaInt), '("name", 'SchemaText) ]
         )
      )
   ]

o <- fromJust <$> decodeFileStrict "example.json" :: IO (Object MySchema)

let names :: [Text]
    names = map (getKey @"name") $ getKey @"users" o
```

^
• Define a schema at the type level
• Decode JSON file matching schema
• Safely extract keys from object

^ Quasiquoters just have to generate code that looks like that

---

# Final Thoughts

---

# Thank You

[.column]

![inline 100%](haskell-love.png)

[.column]

![inline](leapyear.svg)

> [https://leapyear.io](https://leapyear.io)

^ Many thanks to Haskell Love for giving me the opportunity to present today. This was my first conference talk, and it was truly an amazing experience for me.

^ I also want to thank my company, LeapYear. In my time there, I've grown so much as a developer, but also as a person. I definitely would not be up here if not for my amazing coworkers. If you'll allow a quick plug: we're based in San Francisco working on privacy-preserving analytics. We primarily work in Haskell, Scala, and Typescript, but we promote functional programming best practices throughout our codebase. We're also hiring, so if that sounds like a good time to you, check out our website and take a look at our job openings.

---

# Q & A

^ If you'd like to chat further, I will be available in a separate Zoom call. If not, feel free to reach out to me afterwards over Slack or Spatial Chat. Thank you for tuning in, and stay healthy!
