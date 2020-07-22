theme: Plain Jane (LeapYear)
footer: `aeson-schemas` | Brandon Chinn | Haskell Love 2020
slidenumbers: true
code: auto(30), Monaco, line-height(1.1)
autoscale: true

[.hide-footer]
[.slidenumbers: false]

# `aeson-schemas`: Safely extract JSON data when data types are too cumbersome[^1]

### Brandon Chinn<br /><br />1 August 2020

^ Hello, and welcome to my talk. Today, I'll be going over the `aeson-schemas` library, a library I wrote and open sourced for my company, LeapYear Technologies.

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

^ If you have any questions during the presentation, feel free to post in the Twitch chat. I will leave time at the very end for questions, so if I don't get to it in the moment, I'll try to address it then.

---

# Motivation

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

^ First, and most importantly, is type safety. We're Haskell programmers, after all; if we don't have type safety, what are we?? The primary area of we want to keep type safe is getting keys from objects. If I know what keys are in this JSON object, it'd be nice to have the compiler check that I'm using the correct keys.

^ Second, we should try to avoid polluting the namespace. It would be a shame to avoid using `user` as a variable name just because it's a key in a JSON object. It's also conceivable for a JSON object to contain keys that are reserved keywords in Haskell, like `type`.

^ Lastly, it would be nice to have a better query language. Record fields are notoriously annoying for dealing with nested data, and, in my opinion, lenses aren't much better.

---

# Using `aeson-schemas`

[.column]

```hs
{-# LANGUAGES DataKinds #-}
{-# LANGUAGES QuasiQuotes #-}

import Data.Aeson (decodeFileStrict)
import Data.Aeson.Schema

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

```hs
main :: IO ()
main = do
  let f = "example.json"
  result <- decodeFileStrict f

  let obj :: Object MySchema
      obj = fromJust result

  -- outputs:
  -- ["Alice", "Bob", "Claire"]
  print [get| obj.users[].name |]
```

^ With those requirements in mind, `aeson-schemas` is the library I came up with. This code snippet gives a quick overview of what `aeson-schemas` can do.

^ First, we define the schema of the JSON data as `MySchema`, using the `schema` quasiquoter. Then, we can decode `Object MySchema` with standard `aeson` decoding functions.

^ Finally, we can use the `get` quasiquoter to extract values from the `Object`. In this case, we get the `users` key, which is a list of objects with the schema `id: Int, name: Text`. Then for each object in the list, get the `name` key, resulting in a `[Text]` value.

^ Going back to the problem requirements, it's type safe. If you try to get `obj.users[].foo`, you'll get an error *at compile-time*. The only thing it adds to the namespace is `MySchema`, and the `get` quasiquoter allows us to write nicer query than using vanilla Haskell functions would allow us.

---

TODO: schema DSL

---

TODO: get DSL

---

TODO: usage in graphql

---

# Implementing `aeson-schemas`

TODO: Bool,Int,Double,Text,Maybe,List,Object
TODO: parseValue
TODO: getKey
TODO: example using only DataKinds + getKey
NOTE: Don't implement quasiquoters

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
