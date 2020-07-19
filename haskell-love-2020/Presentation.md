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

# Using `aeson-schemas`

TODO: schema qq
TODO: get qq
TODO: usage in graphql

---

# Implementing `aeson-schemas`

TODO: SchemaObject + SchemaInt + SchemaBool
TODO: parseValue
TODO: getKey

---

# Final Thoughts

---

# Thank You

TODO: Haskell Love
TODO: LeapYear + plug

---

# Q & A

^ If you'd like to chat further, I will be available in a separate Zoom call. If not, feel free to reach out to me afterwards over Slack or Spatial Chat. Thank you for tuning in, and stay healthy!
