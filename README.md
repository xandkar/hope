[![Build Status](https://travis-ci.org/ibnfirnas/hope.svg?branch=master)](https://travis-ci.org/ibnfirnas/hope)

Higher Order Programming in Erlang
==================================

A quest for a "standard" library with uniform, composable abstractions.


Monads
------

Defined in `hope_gen_monad`, implemented as:

- `hope_result`: for composition of common functions returning
  `{ok, Val} | {error, Reason}`. An alternative to exceptions, which makes the
  error conditions apparent in the spec/signature. Analogous to Haskell's
  `Data.Either a b`, Jane Street Core's (OCaml) `('a, 'b) Result.t`, Rust's
  `Result<T, E>`
- `hope_option`: for expressing and composing the intention that the value may
  or may not be available. An alternative to the common `undefined` (which is
  equivalent to the dreaded `null`). Analogous to ML's (SML, OCaml, etc)
  `'a Option.t`, Rust's `Option<T>` and Haskell's `Data.Maybe a` [1].


Containers
----------

### Dictionary

Defined in `hope_gen_dictionary`, implemented as:

- `hope_kv_list`. Equivalent to orddict/proplist. Operations implemented with
  BIFs from `lists` module, where possible


[1]: http://en.wikipedia.org/wiki/Option_type
