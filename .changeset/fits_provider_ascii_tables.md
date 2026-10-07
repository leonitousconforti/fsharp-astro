---
default: minor
---

# The type provider types ASCII tables

`FitsProvider` typed the columns of a BINTABLE only, so a TABLE extension in a sample came through
with its keywords and nothing else. It now gets the same `Rows`, `Row`, `Count` and per-column
members a binary table does, and `Table` hands back the untyped `AsciiTable`.

An `Iw` field is provided as `int64` and the `F`, `E` and `D` forms as `float`, whatever width they
have, which is what decoding one gives. TUNITn turns into a unit of measure on the floating point
fields the same way it does for a binary table.
