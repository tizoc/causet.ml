The deriver's errors, each located on what is wrong. A [@nested] constructor's
kind, level and fields are its inner message's, as Rust's derive holds:

  $ cat > nested_kind.ml <<EOF
  > type a = Inner of b [@nested] [@kind "a.inner"]
  > [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl nested_kind.ml
  File "nested_kind.ml", line 1, characters 30-47:
  1 | type a = Inner of b [@nested] [@kind "a.inner"]
                                    ^^^^^^^^^^^^^^^^^
  Error: a constructor is either [@nested] or has a [@kind], not both
  [1]
  $ cat > nested_level.ml <<EOF
  > type a = Inner of b [@nested] [@level Warn]
  > [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl nested_level.ml
  File "nested_level.ml", line 1, characters 30-43:
  1 | type a = Inner of b [@nested] [@level Warn]
                                    ^^^^^^^^^^^^^
  Error: a [@nested] constructor logs as its inner message: no [@level]
  [1]
  $ cat > nested_fields.ml <<EOF
  > type a = Inner of b [@nested] [@fields peer]
  > [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl nested_fields.ml
  File "nested_fields.ml", line 1, characters 30-44:
  1 | type a = Inner of b [@nested] [@fields peer]
                                    ^^^^^^^^^^^^^^
  Error: a [@nested] constructor logs as its inner message: no [@fields]
  [1]

An attribute in the deriver's namespace that it does not know:

  $ cat > unknown.ml <<EOF
  > type a = Ping [@causet.knd "a.ping"]
  > [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl unknown.ml
  File "unknown.ml", line 1, characters 14-36:
  1 | type a = Ping [@causet.knd "a.ping"]
                    ^^^^^^^^^^^^^^^^^^^^^^
  Error: [@causet.knd]: expected [@kind], [@nested], [@level], [@fields] or [@debug_name]
  [1]

A level that is not one, and a logged field the record does not have:

  $ cat > level.ml <<EOF
  > type a = Ping [@level Loud]
  > [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl level.ml
  File "level.ml", line 1, characters 22-26:
  1 | type a = Ping [@level Loud]
                            ^^^^
  Error: expected Trace, Debug, Info, Warn or Error
  [1]
  $ cat > type_level.ml <<EOF
  > type a = Ping [@@deriving message ~name:"a" ~level:Loud]
  > EOF
  $ ./driver.exe -null -impl type_level.ml
  File "type_level.ml", line 1, characters 51-55:
  1 | type a = Ping [@@deriving message ~name:"a" ~level:Loud]
                                                         ^^^^
  Error: expected Trace, Debug, Info, Warn or Error
  [1]
  $ cat > no_field.ml <<EOF
  > type a = Dropped of { peer : int } [@fields peer, n = count]
  > [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl no_field.ml
  File "no_field.ml", line 1, characters 54-59:
  1 | type a = Dropped of { peer : int } [@fields peer, n = count]
                                                            ^^^^^
  Error: no field count
  [1]

A printer given and left out at once, or given for several types:

  $ cat > pp_no_pp.ml <<EOF
  > type a = Ping [@@deriving message ~name:"a" ~pp:Causet.Debug.opaque ~no_pp]
  > EOF
  $ ./driver.exe -null -impl pp_no_pp.ml
  File "pp_no_pp.ml", line 1, characters 48-67:
  1 | type a = Ping [@@deriving message ~name:"a" ~pp:Causet.Debug.opaque ~no_pp]
                                                      ^^^^^^^^^^^^^^^^^^^
  Error: ~pp gives the printer and ~no_pp leaves it out: use one
  [1]
  $ cat > pp_group.ml <<EOF
  > type a = Ping and b = Pong
  > [@@deriving message ~name:"a" ~pp:Causet.Debug.opaque]
  > EOF
  $ ./driver.exe -null -impl pp_group.ml
  File "pp_group.ml", line 2, characters 34-53:
  2 | [@@deriving message ~name:"a" ~pp:Causet.Debug.opaque]
                                        ^^^^^^^^^^^^^^^^^^^
  Error: ~pp is one type's printer: derive these types apart
  [1]

A type the deriver cannot print, and a record's options on a variant:

  $ cat > abstract.ml <<EOF
  > type a [@@deriving message ~name:"a"]
  > EOF
  $ ./driver.exe -null -impl abstract.ml
  File "abstract.ml", line 1, characters 0-37:
  1 | type a [@@deriving message ~name:"a"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: an abstract type has no derived printer: write one with ~pp, or use ~no_pp
  [1]
  $ cat > variant_kind.ml <<EOF
  > type a = Ping [@@deriving message ~name:"a" ~kind:"a.ping"]
  > EOF
  $ ./driver.exe -null -impl variant_kind.ml
  File "variant_kind.ml", line 1, characters 50-58:
  1 | type a = Ping [@@deriving message ~name:"a" ~kind:"a.ping"]
                                                        ^^^^^^^^
  Error: ~kind names a record's kind: give a constructor [@kind]
  [1]
  $ cat > variant_fields.ml <<EOF
  > type a = Ping [@@deriving message ~name:"a" ~fields:peer]
  > EOF
  $ ./driver.exe -null -impl variant_fields.ml
  File "variant_fields.ml", line 1, characters 52-56:
  1 | type a = Ping [@@deriving message ~name:"a" ~fields:peer]
                                                          ^^^^
  Error: ~fields names a record's fields: give a constructor [@fields]
  [1]
