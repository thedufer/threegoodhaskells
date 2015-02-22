# Wherein we document our tables

## Members
  - id - integer
  - email - string
  - unsubscribed - boolean
  - sendTime - integer
  - nextEmailDate - timestamp
  - createdAt - timestamp (unused)
  - updatedAt - timestamp (unused and not updated since switch to haskell)

## LoginCodes
  - id - integer
  - code - string
  - expires - timestamp
  - MemberId - integer
  - createdAt - timestamp (unused)
  - updatedAt - timestamp (unused and not updated since switch to haskell)

## Tokens
  - id - integer
  - token - string
  - MemberId - integer
  - createdAt - timestamp (unused)
  - updatedAt - timestamp (unused and not updated since switch to haskell)

## Posts
  - id - integer
  - text - string
  - date - timestamp (time portion should be 0)
  - token - string
  - MemberId - integer
  - createdAt - timestamp (unused)
  - updatedAt - timestamp (unused and not updated since switch to haskell)

