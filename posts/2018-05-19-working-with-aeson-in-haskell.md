---
    title: Working with Aeson (json) library in Haskell
    author: Nagarjuna Pamu
---


Aeson is the most widely used library for JSON parsing and JSON manipulation in `Haskell` ecosystem.

Aeson exposes following notable functions when you import `Data.Aeson`

```scala
decode :: FromJSON a => ByteString -> Maybe a
encode :: ToJSON a => a -> ByteString
eitherDecode :: FromJSON a => ByteString -> Either String a
```
