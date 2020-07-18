theme: Plain Jane (LeapYear)
footer: `aeson-schemas` | Brandon Chinn | Haskell Love 2020
slidenumbers: true

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

TODO: `aeson`
TODO: graphql

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
