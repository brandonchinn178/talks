theme: Plain Jane
footer: LeapYear Tech Talk | 2021.06.15
slidenumbers: true
autoscale: true
code: auto(7), Menlo, line-height(1.1)
background-color: #102942
text: #FFFFFF
header: #FFFFFF
table-separator: #FFFFFF
footnote-separator: #FFFFFF

[.hide-footer]
[.slidenumbers: false]

# Preventing unreachable error branches with type safety

### Brandon Chinn<br /><br />15 June 2021

---

# Agenda

* Intro with personal project

* Application in LeapYear code

* Q + A

---

# Personal Project

---

# Personal Project
## Intro to Scattergories

1. Get list of categories + a letter
1. Everyone has 3 minutes to write down answers
1. Everyone's answers are revealed
    * Get a point if you wrote unique answer

---

# Personal Project
## Scattergories Implementation v1

[.column]

```hs
type PlayerName = Text
type CategoryName = Text

data Round = Round
  { players ::
      Map PlayerName PlayerAnswers
  , letter :: Char
  }
```

[.column]

```hs
data PlayerAnswers = PlayerAnswers
  { answers ::
      Map CategoryName Answer
  }

data Answer = Answer
  { answer :: Maybe Text
  , isValid :: Maybe Bool
  }
```

---

# Personal Project
## Scattergories Implementation v1

```hs
-- while players are answering
"Food"   => Answer Nothing Nothing
"Animal" => Answer Nothing Nothing

-- after player has answered
"Food"   => Answer (Just "Beet") Nothing
"Animal" => Answer (Just "Bear") Nothing

-- after scoring
"Food"   => Answer (Just "Beet") (Just True)
"Animal" => Answer (Just "Bear") (Just False)
```

^ For now, pretend everyone always answers every category

^ In this example, someone else put "Bear" for "Animal"

---

# Personal Project
## Scattergories Implementation v1

[.column]

```hs
-- should only be called after
-- everyone submits answers
getAnswers ::
  PlayerAnswers ->
  [(CategoryName, Text)]
getAnswers (PlayerAnswers answers) =
  Map.toList $ getAnswer <$> answers
  where
    getAnswer Answer{answer} =
      case answer of
        Just s -> s
        Nothing ->
          error "answer is Nothing"
```

[.column]

```hs
-- should only be called after
-- everyone is scored
getScore ::
  PlayerAnswers ->
  Int
getScore (PlayerAnswers answers) =
  sum $ score <$> answers
  where
    score Answer{isValid} =
      case isValid of
        Just True -> 1
        Just False -> 0
        Nothing ->
          error "isValid is Nothing"
```

^ Downside is an `error` branch that shouldn't happen if the function is called at the right stage of the game

---

# Personal Project
## Scattergories Implementation v2

```hs
data RoundStage = AnswersPending | AnswersDone | AnswersScored

data Round (stage :: RoundStage) = Round
  { players :: Map PlayerName (PlayerAnswers stage)
  , letter :: Char
  }

data PlayerAnswers (stage :: RoundStage) = PlayerAnswers
  { answers :: Map CategoryName (Answer stage)
  }

data Answer (stage :: RoundStage) where
  MaybeAnswer  :: Maybe Text   -> Answer 'AnswersPending
  Answer       :: Text         -> Answer 'AnswersDone
  ScoredAnswer :: Text -> Bool -> Answer 'AnswersScored
```

^ Solution: store stage of the game at the type-level

---

# Personal Project
## Scattergories Implementation v2

```hs
-- while players are answering
-- Map CategoryName (Answer 'AnswersPending)
"Food"   => MaybeAnswer Nothing
"Animal" => MaybeAnswer Nothing

-- after player has answered
-- Map CategoryName (Answer 'AnswersDone)
"Food"   => Answer "Beet"
"Animal" => Answer "Bear"

-- after scoring
-- Map CategoryName (Answer 'AnswersScored)
"Food"   => ScoredAnswer "Beet" True
"Animal" => ScoredAnswer "Bear" False
```

^ Note that there's only one possible constructor for the given type; correct by construction

---

# Personal Project
## Scattergories Implementation v2

[.column]

```hs
getAnswers :: PlayerAnswers 'AnswersDone -> [(CategoryName, Text)]
getAnswers (PlayerAnswers answers) = Map.toList $ getAnswer <$> answers
  where
    getAnswer (Answer answer) = answer

getScore :: PlayerAnswers 'AnswersScored -> Int
getScore (PlayerAnswers answers) = sum $ score <$> answers
  where
    score (ScoredAnswer _ isValid) = if isValid then 1 else 0
```

^ Note that these functions are enforced at the type level to only be runnable at the correct stage -- you can't call `getAnswers` if `PlayerAnswers` is not at the right stage of the round

^ No more error branches!

---

# Applying in LeapYear

---

# Applying in LeapYear
## Table schema in Data Manager

1. When a table is first created, schema types are `null`
1. When table is finished creating, schema types are populated (e.g. `INT(1, 10)`)
1. After table is finished creating, admin can edit schema bounds in Data Manager

---

# Applying in LeapYear
## Table schema in Data Manager

https://github.com/LeapYear/leapyear/pull/9136/files#r603658735

[.column]

```ts
// the TableColumn type we use everywhere;
// corresponds with the graphql type
type TableColumn = {
  name: string
  type: ColumnType | null
  bounds: ColumnBounds | null
  nullable: boolean | null
}
```

[.column]

```ts
// useEditTableSchema.ts
if (!column.type) {
  throw new Error(
    "unreachable: " +
    "table has null type when editing schema"
  )
}
```

^ In this PR, I needed to use a column's type, but typescript was complaining because type is nullable, even though I know this component is only rendered when type is not nullable

---

# Applying in LeapYear
## Table schema in Data Manager

```ts
type TableColumnGeneric<IsReady extends 'READY' | 'NOT_READY'> = {
  name: string
  type: IsReady extends 'READY' ? ColumnType : null
  bounds: IsReady extends 'READY' ? ColumnBounds : null
  nullable: IsReady extends 'READY' ? boolean : null
}

type TableColumnReady = TableColumnGeneric<'READY'>
type TableColumnNotReady = TableColumnGeneric<'NOT_READY'>

type TableColumn = TableColumnReady | TableColumnNotReady
```

---

# Appendix: Links + references

* https://github.com/brandonchinn178/categories-with-friends
* https://github.com/LeapYear/leapyear/pull/9136
* https://github.com/LeapYear/leapyear/pull/9141
* https://github.com/LeapYear/leapyear/pull/9140
* [`data-manager/src/views/data/databasePage/components/TableDetails/SchemaPanel/`](https://github.com/LeapYear/leapyear/tree/5d49e16fef02e02e592754aaa4bbfeb8dd7bdcea/data-manager/src/views/data/databasePage/components/TableDetails/SchemaPanel)

---

# Appendix: State transitions

```hs
-- | Attempt to finalize all the answers. If any answers
-- are still pending, returns Nothing.
finalizeAnswers ::
  PlayerAnswers 'AnswersPending ->
  Maybe (PlayerAnswers 'AnswersDone)

-- | Score answers with the given score sheet.
scoreAnswers ::
  PlayerAnswers 'AnswersDone ->
  Map CategoryName Bool ->
  PlayerAnswers 'AnswersScored
```

---

# Appendix: Type narrowing

```ts
type TableHelper<TableColumn> = {
  id: string
  columns: TableColumn[]
}

type TableNotReady = TableHelper<TableColumnNotReady>
type TableReady = TableHelper<TableColumnReady>
type Table = TableNotReady | TableReady

function isTableReady(table: Table): table is TableReady {
  return isTableColumnReady(table.columns[0])
}

function isTableColumnReady(column: TableColumn): column is TableColumnReady {
  return column.type !== null
}
```
