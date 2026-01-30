(** OCaml bindings for f16, f32, f64, f128 floating-point types *)

(** IEEE 754 rounding modes *)
type rounding_mode =
  | NearestEven   (** Round to nearest, ties to even (default IEEE 754 mode) *)
  | ToZero        (** Round toward zero (truncate) *)
  | Up            (** Round toward +infinity (ceiling) *)
  | Down          (** Round toward -infinity (floor) *)
  | NearestAway   (** Round to nearest, ties away from zero *)

(* Convert fpclass int from C to OCaml type *)
let fpclass_of_int = function
  | 0 -> FP_normal
  | 1 -> FP_subnormal
  | 2 -> FP_zero
  | 3 -> FP_infinite
  | _ -> FP_nan

(* Convert rounding_mode to int for C *)
let int_of_rounding_mode = function
  | NearestEven -> 0
  | ToZero -> 1
  | Up -> 2
  | Down -> 3
  | NearestAway -> 4

(* ============================================================================
 * F16 - Half-precision (16-bit) floating-point
 * Uses native _Float16 when available, IEEE-754 compliant software fallback otherwise
 * ============================================================================ *)
module F16 = struct
  type t
  type bits = int

  external of_string : string -> t = "caml_f16_of_string"
  external add : t -> t -> t = "caml_f16_add"
  external sub : t -> t -> t = "caml_f16_sub"
  external mul : t -> t -> t = "caml_f16_mul"
  external div : t -> t -> t = "caml_f16_div"
  external rem : t -> t -> t = "caml_f16_rem"
  external to_bits : t -> int = "caml_f16_to_bits"
  external of_bits : int -> t = "caml_f16_of_bits"
  external to_float : t -> float = "caml_f16_to_float"
  external fpclass_raw : t -> int = "caml_f16_fpclass"
  external abs : t -> t = "caml_f16_abs"
  external round_raw : t -> int -> t = "caml_f16_round"

  let to_z t = Z.of_int (to_bits t)

  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f16_eq"
  external lt : t -> t -> bool = "caml_f16_lt"
  external le : t -> t -> bool = "caml_f16_le"

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
end

(* ============================================================================
 * F32 - Single-precision (32-bit) floating-point
 * ============================================================================ *)
module F32 = struct
  type t
  type bits = int32

  external of_string : string -> t = "caml_f32_of_string"
  external add : t -> t -> t = "caml_f32_add"
  external sub : t -> t -> t = "caml_f32_sub"
  external mul : t -> t -> t = "caml_f32_mul"
  external div : t -> t -> t = "caml_f32_div"
  external rem : t -> t -> t = "caml_f32_rem"
  external to_bits : t -> int32 = "caml_f32_to_bits"
  external of_bits : int32 -> t = "caml_f32_of_bits"
  external to_float : t -> float = "caml_f32_to_float"
  external fpclass_raw : t -> int = "caml_f32_fpclass"
  external abs : t -> t = "caml_f32_abs"
  external round_raw : t -> int -> t = "caml_f32_round"

  let to_z t = Z.of_int32 (to_bits t)

  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f32_eq"
  external lt : t -> t -> bool = "caml_f32_lt"
  external le : t -> t -> bool = "caml_f32_le"

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
end

(* ============================================================================
 * F64 - Double-precision (64-bit) floating-point
 * ============================================================================ *)
module F64 = struct
  type t
  type bits = int64

  external of_string : string -> t = "caml_f64_of_string"
  external add : t -> t -> t = "caml_f64_add"
  external sub : t -> t -> t = "caml_f64_sub"
  external mul : t -> t -> t = "caml_f64_mul"
  external div : t -> t -> t = "caml_f64_div"
  external rem : t -> t -> t = "caml_f64_rem"
  external to_bits : t -> int64 = "caml_f64_to_bits"
  external of_bits : int64 -> t = "caml_f64_of_bits"
  external to_float : t -> float = "caml_f64_to_float"
  external fpclass_raw : t -> int = "caml_f64_fpclass"
  external abs : t -> t = "caml_f64_abs"
  external round_raw : t -> int -> t = "caml_f64_round"

  let to_z t = Z.of_int64 (to_bits t)

  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f64_eq"
  external lt : t -> t -> bool = "caml_f64_lt"
  external le : t -> t -> bool = "caml_f64_le"

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
end

(* ============================================================================
 * F128 - Quad-precision (128-bit) floating-point
 * ============================================================================ *)
module F128 = struct
  type t
  type bits = int64 * int64  (* low, high *)

  external of_string : string -> t = "caml_f128_of_string"
  external add : t -> t -> t = "caml_f128_add"
  external sub : t -> t -> t = "caml_f128_sub"
  external mul : t -> t -> t = "caml_f128_mul"
  external div : t -> t -> t = "caml_f128_div"
  external rem : t -> t -> t = "caml_f128_rem"
  external to_bits_low : t -> int64 = "caml_f128_to_bits_low"
  external to_bits_high : t -> int64 = "caml_f128_to_bits_high"
  external of_bits_raw : int64 -> int64 -> t = "caml_f128_of_bits"
  external to_float : t -> float = "caml_f128_to_float"
  external fpclass_raw : t -> int = "caml_f128_fpclass"
  external abs : t -> t = "caml_f128_abs"
  external round_raw : t -> int -> t = "caml_f128_round"

  let to_bits t = (to_bits_low t, to_bits_high t)
  let of_bits (low, high) = of_bits_raw low high

  (** Convert to Z.t representing the full 128-bit value.
      The result is: high * 2^64 + low (treating low as unsigned) *)
  let to_z t =
    let low = to_bits_low t in
    let high = to_bits_high t in
    (* Convert low as unsigned 64-bit integer *)
    let z_low = Z.of_int64_unsigned low in
    (* high can be treated as signed since it's the upper bits *)
    let z_high = Z.of_int64 high in
    Z.add (Z.shift_left z_high 64) z_low

  let fpclass t = fpclass_of_int (fpclass_raw t)
  let round mode t = round_raw t (int_of_rounding_mode mode)

  external eq : t -> t -> bool = "caml_f128_eq"
  external lt : t -> t -> bool = "caml_f128_lt"
  external le : t -> t -> bool = "caml_f128_le"

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
  let ( = ) = eq
  let ( < ) = lt
  let ( <= ) = le
end
