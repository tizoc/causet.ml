(* XXH3-64 with seed 0 and the default secret, bit for bit the algorithm the
   Rust port names mount identities with, so the two ports give a path the
   same identity. Unsigned 64-bit arithmetic on [Int64]: addition,
   multiplication and xor wrap alike for signed and unsigned values; shifts
   are logical. *)

let ( +: ) = Int64.add
let ( *: ) = Int64.mul
let ( ^: ) = Int64.logxor
let ( >>: ) = Int64.shift_right_logical
let ( <<: ) = Int64.shift_left
let rotl x n = Int64.logor (x <<: n) (x >>: 64 - n)
let prime32_1 = 0x9E3779B1L
let prime32_2 = 0x85EBCA77L
let prime32_3 = 0xC2B2AE3DL
let prime64_1 = 0x9E3779B185EBCA87L
let prime64_2 = 0xC2B2AE3D27D4EB4FL
let prime64_3 = 0x165667B19E3779F9L
let prime64_4 = 0x85EBCA77C2B2AE63L
let prime64_5 = 0x27D4EB2F165667C5L

let secret =
  "\xb8\xfe\x6c\x39\x23\xa4\x4b\xbe\x7c\x01\x81\x2c\xf7\x21\xad\x1c\xde\xd4\x6d\xe9\x83\x90\x97\xdb\x72\x40\xa4\xa4\xb7\xb3\x67\x1f\xcb\x79\xe6\x4e\xcc\xc0\xe5\x78\x82\x5a\xd0\x7d\xcc\xff\x72\x21\xb8\x08\x46\x74\xf7\x43\x24\x8e\xe0\x35\x90\xe6\x81\x3a\x26\x4c\x3c\x28\x52\xbb\x91\xc3\x00\xcb\x88\xd0\x65\x8b\x1b\x53\x2e\xa3\x71\x64\x48\x97\xa2\x0d\xf9\x4e\x38\x19\xef\x46\xa9\xde\xac\xd8\xa8\xfa\x76\x3f\xe3\x9c\x34\x3f\xf9\xdc\xbb\xc7\xc7\x0b\x4f\x1d\x8a\x51\xe0\x4b\xcd\xb4\x59\x31\xc8\x9f\x7e\xc9\xd9\x78\x73\x64\xea\xc5\xac\x83\x34\xd3\xeb\xc3\xc5\x81\xa0\xff\xfa\x13\x63\xeb\x17\x0d\xdd\x51\xb7\xf0\xda\x49\xd3\x16\x55\x26\x29\xd4\x68\x9e\x2b\x16\xbe\x58\x7d\x47\xa1\xfc\x8f\xf8\xb8\xd1\x7a\xd0\x31\xce\x45\xcb\x3a\x8f\x95\x16\x04\x28\xaf\xd7\xfb\xca\xbb\x4b\x40\x7e"

let read64 s i = String.get_int64_le s i
let read32 s i = Int64.logand (Int64.of_int32 (String.get_int32_le s i)) 0xFFFFFFFFL

(* The 128-bit product of two unsigned 64-bit values, folded: low xor high. *)
let mul128_fold64 a b =
  let mask = 0xFFFFFFFFL in
  let a_lo = Int64.logand a mask and a_hi = a >>: 32 in
  let b_lo = Int64.logand b mask and b_hi = b >>: 32 in
  let lo_lo = a_lo *: b_lo in
  let hi_lo = a_hi *: b_lo in
  let lo_hi = a_lo *: b_hi in
  let hi_hi = a_hi *: b_hi in
  let cross = (lo_lo >>: 32) +: Int64.logand hi_lo mask +: lo_hi in
  let upper = (hi_lo >>: 32) +: (cross >>: 32) +: hi_hi in
  let lower = Int64.logor (cross <<: 32) (Int64.logand lo_lo mask) in
  lower ^: upper

let xorshift v s = v ^: (v >>: s)

let avalanche v =
  let v = xorshift v 37 in
  let v = v *: 0x165667919E3779F9L in
  xorshift v 32

let xxh64_avalanche v =
  let v = v ^: (v >>: 33) in
  let v = v *: prime64_2 in
  let v = v ^: (v >>: 29) in
  let v = v *: prime64_3 in
  v ^: (v >>: 32)

let strong_avalanche v len =
  let v = v ^: rotl v 49 ^: rotl v 24 in
  let v = v *: 0x9FB21C651E98DF25L in
  let v = v ^: ((v >>: 35) +: len) in
  let v = v *: 0x9FB21C651E98DF25L in
  xorshift v 28

let swap64 v =
  let b i = Int64.logand (v >>: 8 * i) 0xFFL in
  let r = ref 0L in
  for i = 0 to 7 do
    r := Int64.logor !r (b i <<: 8 * (7 - i))
  done;
  !r

let len64 s = Int64.of_int (String.length s)

let hash_1to3 s =
  let n = String.length s in
  let c i = Int64.of_int (Char.code s.[i]) in
  let combo =
    Int64.logor
      (Int64.logor (c 0 <<: 16) (c (n lsr 1) <<: 24))
      (Int64.logor (c (n - 1)) (Int64.of_int n <<: 8))
  in
  let flip = read32 secret 0 ^: read32 secret 4 in
  xxh64_avalanche (combo ^: flip)

let hash_4to8 s =
  let n = String.length s in
  let input1 = read32 s 0 and input2 = read32 s (n - 4) in
  let flip = read64 secret 8 ^: read64 secret 16 in
  let input64 = input2 +: (input1 <<: 32) in
  strong_avalanche (input64 ^: flip) (len64 s)

let hash_9to16 s =
  let n = String.length s in
  let flip1 = read64 secret 24 ^: read64 secret 32 in
  let flip2 = read64 secret 40 ^: read64 secret 48 in
  let lo = read64 s 0 ^: flip1 in
  let hi = read64 s (n - 8) ^: flip2 in
  avalanche (len64 s +: swap64 lo +: hi +: mul128_fold64 lo hi)

let mix16 s i si =
  let lo = read64 s i ^: read64 secret si in
  let hi = read64 s (i + 8) ^: read64 secret (si + 8) in
  mul128_fold64 lo hi

let hash_17to128 s =
  let n = String.length s in
  let acc = ref (len64 s *: prime64_1) in
  let add v = acc := !acc +: v in
  if n > 32 then (
    if n > 64 then (
      if n > 96 then (
        add (mix16 s 48 96);
        add (mix16 s (n - 64) 112));
      add (mix16 s 32 64);
      add (mix16 s (n - 48) 80));
    add (mix16 s 16 32);
    add (mix16 s (n - 32) 48));
  add (mix16 s 0 0);
  add (mix16 s (n - 16) 16);
  avalanche !acc

let hash_129to240 s =
  let n = String.length s in
  let acc = ref (len64 s *: prime64_1) in
  for i = 0 to 7 do
    acc := !acc +: mix16 s (16 * i) (16 * i)
  done;
  acc := avalanche !acc;
  for i = 8 to (n / 16) - 1 do
    acc := !acc +: mix16 s (16 * i) ((16 * (i - 8)) + 3)
  done;
  acc := !acc +: mix16 s (n - 16) (136 - 17);
  avalanche !acc

let stripe_len = 64
let secret_len = String.length secret

let accumulate_512 acc s i si =
  for k = 0 to 7 do
    let data = read64 s (i + (8 * k)) in
    let key = data ^: read64 secret (si + (8 * k)) in
    acc.(k lxor 1) <- acc.(k lxor 1) +: data;
    acc.(k) <- acc.(k) +: (Int64.logand key 0xFFFFFFFFL *: (key >>: 32))
  done

let scramble acc =
  for k = 0 to 7 do
    let key = read64 secret (secret_len - stripe_len + (8 * k)) in
    acc.(k) <- (xorshift acc.(k) 47 ^: key) *: prime32_1
  done

let hash_long s =
  let n = String.length s in
  let acc =
    [|
      prime32_3;
      prime64_1;
      prime64_2;
      prime64_3;
      prime64_4;
      prime32_2;
      prime64_5;
      prime32_1;
    |]
  in
  let stripes_per_block = (secret_len - stripe_len) / 8 in
  let block_len = stripe_len * stripes_per_block in
  let blocks = (n - 1) / block_len in
  for b = 0 to blocks - 1 do
    for st = 0 to stripes_per_block - 1 do
      accumulate_512 acc s ((b * block_len) + (st * stripe_len)) (st * 8)
    done;
    scramble acc
  done;
  let stripes = (n - 1 - (block_len * blocks)) / stripe_len in
  for st = 0 to stripes - 1 do
    accumulate_512 acc s ((blocks * block_len) + (st * stripe_len)) (st * 8)
  done;
  accumulate_512 acc s (n - stripe_len) (secret_len - stripe_len - 7);
  let result = ref (len64 s *: prime64_1) in
  for k = 0 to 3 do
    let si = 11 + (16 * k) in
    result :=
      !result
      +: mul128_fold64
           (acc.(2 * k) ^: read64 secret si)
           (acc.((2 * k) + 1) ^: read64 secret (si + 8))
  done;
  avalanche !result

let hash64 s =
  let n = String.length s in
  if n = 0 then xxh64_avalanche (read64 secret 56 ^: read64 secret 64)
  else if n <= 3 then hash_1to3 s
  else if n <= 8 then hash_4to8 s
  else if n <= 16 then hash_9to16 s
  else if n <= 128 then hash_17to128 s
  else if n <= 240 then hash_129to240 s
  else hash_long s
