theme: Plain Jane
footer: Haskell Love 2020 | `aeson-schemas`
slidenumbers: true
autoscale: true
code: auto(95), Menlo, line-height(1.1)
background-color: #635C8C
text: #FFFFFF
header: #FFFFFF
footnote-separator: #FFFFFF

[.hide-footer]
[.slidenumbers: false]

# `aeson-schemas`: Safely extract JSON data when data types are too cumbersome[^1]

### Brandon Chinn<br /><br />1 August 2020

^ Hello, and welcome to my talk. Today, I'll be going over the `aeson-schemas` library, a library I wrote and open sourced at my company, LeapYear Technologies.

[^1]: [http://hackage.haskell.org/package/aeson-schemas](http://hackage.haskell.org/package/aeson-schemas)

---

# Agenda

* Motivation (5 min)

* Using `aeson-schemas` (5 min)

* Implementing `aeson-schemas` (10 min)

    * Type-level programming 101

* Final Thoughts + Q&A (10 min)

^ This is the agenda for today's talk.

^ The first two sections should be easy to follow if you've done any JSON operations in Haskell.

^ In the third section, I'm going to go over the general idea of how this library works. This library makes heavy use of type-level programming, but I won't assume any knowledge of data kinds or type families. So if you feel comfortable with basic Haskell concepts like data types and type classes, you should be able to follow along.

^ If you have any questions during the presentation, feel free to post in the Twitch chat. Our wonderful track leader will be moderating the questions and passing them along to me at the middle and end of the presentation.

---

# Motivation

^ First, some context around why I wrote this library.

---

## Parsing data with `aeson`

```hs
class ToJSON a where
  toJSON :: a -> Value

class FromJSON a where
  parseJSON :: Value -> Parser a
```

^ If you've used JSON data in your Haskell code before, you've probably used the `aeson` library. Among other things, `aeson` provides these type classes, which convert your custom data type to and from a JSON `Value`.

^ For this talk, I'm only going to focus on `FromJSON` and parsing JSON data.

---

## Parsing data with `aeson`

[.column]

```json
{
    "users": [
        {
            "id": 1,
            "name": "Alice"
        },
        {
            "id": 2,
            "name": "Bob"
        },
        {
            "id": 3,
            "name": "Claire"
        }
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

^ Typically, you won't be writing these instances directly. Usually, you'll derive them automatically using `Generic`. For example, this will automatically implement `FromJSON` for `User`, such that it'll parse a JSON object with an `id` integer field and a `name` string field.

---

## Parsing data with `aeson`

[.column]

```json
{
  "permissions": [
    {
      "resource": {
        "name": "secretdata.txt",
        "owner": {
          "name": "john@example.com"
        }
      },
      "access": "READ"
    }
  ]
}
```

[.column]

```hs
data Result = Result
  { permissions :: [Permission]
  } deriving (Show, Generic, FromJSON)

data Permission = Permission
  { resource :: Maybe Resource
  , access   :: String
  } deriving (Show, Generic, FromJSON)

data Resource = Resource
  { name  :: String
  , owner :: Maybe Owner
  } deriving (Show, Generic, FromJSON)

data Owner = Owner
  { name :: String
  } deriving (Show, Generic, FromJSON)
```

^ This is admittedly a rather contrived example, but it showcases a couple issues.

^ First, even if you derive `FromJSON` manually, you still need at least 3 of these data types, unless you're ok with manipulating nested tuples (hey, no judgment).

^ Second, both `Resource` and `Owner` have a `name` field, which causes problems if you want to use `name` as a function, even if you turn on the `DuplicateRecordFields` extension. Of course, the canonical solution is to prefix records like `resourceName` or `ownerName`, but then we'd have to do tricks to make `FromJSON` work again.

---

## Querying a GraphQL API

[.column]

### Schema

```
type User {
    id: ID!
    name: String!
    posts: [Post!]!
}

type Post {
    id: ID!
    name: String!
    author: User!
    createdAt: String!
}

type Query {
    users: [User!]!
    posts: [Post!]!
}
```

[.column]

### Query 1

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

### Query 2

```
query {
    posts {
        id
        name
        author {
            name
        }
        createdAt
    }
}
```

^ My main motivation for this library was to query a GraphQL API from Haskell. If you're not familiar with GraphQL, it's an alternative to REST where you send a request that specifies exactly which fields you want to query from the schema.

^ In the first query, we're querying all the users in the API and the posts they've written. Note that even though `Post` has a `createdAt` field, we can choose to omit it if we don't care about that information in this query.

^ But in the second query, we're querying the posts first, along with the name of their author. Here, we can choose to get a post's `createdAt` field, but maybe we don't care about the author's ID, so we can omit that.

^ So when querying a GraphQL API, how should we write our types in Haskell?

---

## Querying a GraphQL API

[.column]

```hs
data User = User
  { id    :: Maybe String
    -- ^ GraphQL 'ID' type is a String
  , name  :: Maybe String
  , posts :: Maybe [Post]
  } deriving (Show, Generic, FromJSON)

data Post = Post
  { id        :: Maybe String
  , name      :: Maybe String
  , author    :: Maybe User
  , createdAt :: Maybe String
  } deriving (Show, Generic, FromJSON)

data Query = Query
  { users :: Maybe [User]
  , posts :: Maybe [Post]
  } deriving (Show, Generic, FromJSON)
```

[.column]

Pros:

* Direct translation of GraphQL schema

Cons:

* Handle `Nothing` / use `fromJust`
* `id` field name shadows `Prelude.id`
* Duplicate `name` field

^ Well, since we can omit any field, we could write all of the types with `Maybe` fields. This gets us a one-to-one mapping between Haskell and GraphQL types, but, among many other problems, we'd have to handle `Nothing` every time we query a field, even if we know a particular query will return that field.

---

## Querying a GraphQL API

[.column]

```hs
data User1 = User1
  { id    :: String
  , name  :: String
  , posts :: [Post1]
  } deriving (Show, Generic, FromJSON)

data Post1 = Post1
  { id   :: String
  , name :: String
  } deriving (Show, Generic, FromJSON)

data Query1 = Query1
  { users :: [User1]
  } deriving (Show, Generic, FromJSON)
```

[.column]

```hs
data User2 = User2
  { name  :: String
  } deriving (Show, Generic, FromJSON)

data Post2 = Post2
  { id        :: String
  , name      :: String
  , author    :: User2
  , createdAt :: String
  } deriving (Show, Generic, FromJSON)

data Query2 = Query2
  { posts :: [Post2]
  } deriving (Show, Generic, FromJSON)
```

^ We could also write a separate type for each query, maybe even use Template Haskell to generate them. But it would definitely be a pain to use and would not scale.

---

[.build-lists: true]

# Problem Requirements

1. Type safe
1. Avoid polluting namespace
1. Nice query language

^ At this point, we've seen some of the limitations vanilla Haskell places on us, seen most clearly in the GraphQL use-case. Note that none of this is the fault of `aeson`, but rather due to the current clunkiness of Haskell data type definitions. Regardless, there is a less-than-ideal developer experience here, and it'd be nice to have an alternate solution. But what would we want in our solution?

^ First, and most importantly, is type safety. We're Haskell programmers, after all; if we don't have type safety, what are we?? The primary area we want to keep type safe is getting keys from objects. If I know what keys are in this JSON object, it'd be nice to have the compiler check that I'm using the correct keys.

^ Second, we should try to avoid polluting the namespace. It would be a shame to avoid using `user` as a variable name just because it's a key in a JSON object. It's also conceivable for a JSON object to contain keys that are reserved keywords in Haskell, like `type`.

^ Lastly, it would be nice to have a better query language. Record fields are notoriously annoying for dealing with nested data, and, in my opinion, lenses aren't much better.

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
• Define schema as `MySchema`
• `schema` quasiquoter

^
2nd column:
• Decode `Object MySchema` with standard decode
• `get` quasiquoter

^
Problem requirements:
• Type safe: `obj.users[].foo`
• Namespace: just `MySchema`
• `get` quasiquoter: nicer syntax

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
• query language for extracting data

---

# Implementing `aeson-schemas`

^ Pause for questions here

^ Intro to type-level programming + general overview of `aeson-schemas`

---

# Type-level programming

| Value | Type | Kind |
|-------|------|------|
| `True`, `False` | `Bool` | `*`[^2] |
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

```hs
import GHC.TypeLits (Symbol)

data SchemaType
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaMaybe SchemaType
  | SchemaList SchemaType
  | SchemaObject [(Symbol, SchemaType)]
```

^ First, we need to set up our data type that will store the JSON schema at the type level. For the sake of time, I won't be including all of the `aeson-schemas` features, just a basic implementation.

^ For now, our JSON schema will only allow `Bool`, `Int`, `Double`, and `Text` values. `SchemaMaybe SchemaBool` would represent a `Bool` value that can be nullable, `SchemaList SchemaInt` would represent a list of `Int` values, and `SchemaObject` can be used to make nested objects. `Symbol` here is just the type-level version of `String`s.

---

# Implementing `aeson-schemas`

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

^ And with just that single definition, we can now write a JSON schema as a type! Note the use of the single quotes. Since we're working at the type-level now, we need to make sure we're writing type-level lists and type-level tuples.

---

# Implementing `aeson-schemas`

```hs
data Object (schema :: SchemaType) = UnsafeObject (HashMap Text Dynamic)

-- From Data.Dynamic, rewritten here for reference
toDyn :: Typeable a => a -> Dynamic
fromDynamic :: Typeable a => Dynamic -> Maybe a
```

^ Now, a fancy type will do us no good if we can't load data with it. First, let's write an `Object` data type that will store the schema for its data. Instead of storing the unparsed JSON `Value`, however, we'll parse the `Value` and store it as `Dynamic`, which lets us store different types in the same `HashMap`.

^ `toDyn` will convert any value to `Dynamic`, and `fromDynamic` will return `Just` the value, or `Nothing`, if the `Dynamic` value contains a different type than the type you're asking for. Theoretically, we'll never get `Nothing`, because we're storing what type we expect for any values in the `schema`. We just need to make sure to not export the `UnsafeObject` constructor, so that a random user of our library won't accidentally modify this `HashMap` and break this invariant.

---

# Implementing `aeson-schemas`

```hs
type family SchemaResult (schema :: SchemaType) where
  SchemaResult 'SchemaBool = Bool
  SchemaResult 'SchemaInt = Int
  SchemaResult 'SchemaDouble = Double
  SchemaResult 'SchemaText = Text
  SchemaResult ('SchemaMaybe inner) = Maybe (SchemaResult inner)
  SchemaResult ('SchemaList inner) = [SchemaResult inner]
  SchemaResult ('SchemaObject schema) = Object ('SchemaObject schema)
```

^ Next, we need a type family to specify the type we want to parse for a given schema. Notice that `SchemaMaybe` and `SchemaList` need to apply `SchemaResult` again to their inner schema, so that `SchemaMaybe SchemaBool` correctly resolves to `Maybe Bool`. And lastly, a `SchemaObject` will be parsed as our `Object` type with the `SchemaObject` stored as its schema.

---

# Implementing `aeson-schemas`

```hs
class IsSchemaType (schema :: SchemaType) where
  parseValue :: Value -> Parser (SchemaResult schema)
```

[.column]

```hs
instance IsSchemaType 'SchemaBool where
  -- parseValue :: Value -> Parser Bool
  parseValue = parseJSON

instance IsSchemaType 'SchemaInt where
  -- parseValue :: Value -> Parser Int
  parseValue = parseJSON
```

[.column]

```hs
instance IsSchemaType 'SchemaDouble where
  -- parseValue :: Value -> Parser Double
  parseValue = parseJSON

instance IsSchemaType 'SchemaText where
  -- parseValue :: Value -> Parser Text
  parseValue = parseJSON
```

^ Now, we can write a type class that will parse JSON `Value`s for our schema types. These schemas are straightforward, since we can just use the normal `parseJSON` function.

---

# Implementing `aeson-schemas`

```hs
instance IsSchemaType inner => IsSchemaType ('SchemaMaybe inner) where
  -- parseValue :: Value -> Parser (Maybe (SchemaResult inner))
  parseValue Aeson.Null = return Nothing
  parseValue value = Just <$> parseValue @inner value

instance IsSchemaType inner => IsSchemaType ('SchemaList inner) where
  -- parseValue :: Value -> Parser [SchemaResult inner]
  parseValue (Aeson.Array a) = traverse (parseValue @inner) (Vector.toList a)
  parseValue _ = fail "..."
```

^ For `SchemaMaybe`, we'll explicitly handle JSON `null` values, and otherwise let the `inner` schema parse the value, wrapping it in `Just`.

^ For `SchemaList`, we require the value to be a JSON `Array` (failing if it's not) and then parse all the `Value`s in the list according to the `inner` schema.

---

# Implementing `aeson-schemas`

```hs
-- reference: SchemaObject [(Symbol, SchemaType)]

instance IsSchemaType ('SchemaObject '[]) where
  parseValue (Aeson.Object _) = return $ UnsafeObject HashMap.empty
  parseValue _ = fail "..."

instance (...) => IsSchemaType ('SchemaObject ('(key, inner) ': rest)) where
  parseValue value@(Aeson.Object o) = do
    let key = Text.pack $ symbolVal (Proxy @key)

    inner <- parseValue @inner (HashMap.lookupDefault Aeson.Null key o)

    UnsafeObject rest <- parseValue @rest value

    return $ UnsafeObject $ HashMap.insert key (toDyn inner) rest

  parseValue _ = fail "..."
```

^ Lastly, we need to handle `SchemaObject`, where most of the magic happens.

^ First, we use `symbolVal` to convert the type-level key into a `String` that we can use. Then, we lookup the key in the JSON object and parse the resulting `Value` according to the `inner` schema. Finally, we parse the rest of the schema and insert the key and parsed value, after converting the value to `Dynamic`.

---

# Implementing `aeson-schemas`

```hs
instance IsSchemaType schema => FromJSON (Object schema) where
  parseJSON = parseValue @schema
```

^ And now, with `parseValue` implemented, we can go ahead and write a `FromJSON` instance for `Object`. One thing that I didn't include here is I also pass in the path of keys to `parseValue`, so that if a JSON value doesn't parse successfully, we can show a nice error message that shows the path and schema that didn't parse.

^ DEMO: `./badDecode.sh`

---

# Implementing `aeson-schemas`

```hs
let o :: Object ('SchemaObject '[ '("foo", 'SchemaInt) ])
    o = ...

getKey @"foo" o :: Int
```

^ The last thing we need to do is be able to extract data out of the `Object`. To do this, we'll implement a `getKey` function that should work like this. You specify the key as a type application, and `getKey` should find the value associated with that key, converted to the correct type.

^ First, let's write a type family to find a value associated with a key.

---

# Implementing `aeson-schemas`

```hs
-- Fcf.Lookup    :: a -> [(a, b)] -> Fcf.Exp (Maybe b)
-- Fcf.FromMaybe :: a -> Maybe a -> Fcf.Exp a
-- Fcf.=<<       :: (a -> Fcf.Exp b) -> Fcf.Exp a -> Fcf.Exp b
-- Fcf.Eval      :: Fcf.Exp a -> a

type family LookupSchema (key :: Symbol) (schema :: SchemaType) where
  LookupSchema key ('SchemaObject schema) = Fcf.Eval
    ( Fcf.FromMaybe
        ( TypeError
          (     'Text "Key '"
          ':<>: 'Text key
          ':<>: 'Text "' does not exist in the following schema:"
          ':$$: 'ShowType schema
          )
        )
      =<< Fcf.Lookup key schema
    )
```

^ Thanks to the `first-class-families` package, this is really straightforward. All of these definitions look really similar to the normal functions we're used to, besides the `Exp` type.

^ One cool thing we can do here is use `TypeError` from `GHC.TypeLits` to throw an error at compile time with a custom error message. This will give us the helpful error message we saw earlier in the presentation, if the given key doesn't exist in the schema.

---

# Implementing `aeson-schemas`

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

^ Finally, we can implement `getKey`. I've simplified the code here a bit, but this gets the general gist across. First, we lookup the key in the initial schema, which gets us the schema at that key. Then, we get the `SchemaResult` for that schema, which tells us the corresponding Haskell type that we need to return.

^ *With this type definition, our compiler will be able to infer the resulting type, given the initial schema and key!*

^ To implement it, we simply take out the underlying `HashMap`, get the `Dynamic` at the given `key`, then convert the `Dynamic` to the Haskell type we've inferred with `SchemaResult`. Since we know we parsed all the JSON values with the correct types, this should never fail.

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

jsonData <- decodeFileStrict "example.json"

let o :: Object MySchema
    o = fromJust jsonData

let names :: [Text]
    names = map (getKey @"name") $ getKey @"users" o
```

^ And voila! We can now define a schema at the type level, decode a JSON file matching the schema, and safely extract keys from the object. For the sake of time, I won't be going over implementing the quasiquoters, but all the quasiquoters have to do is generate code that looks like this, and it should still work!

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
