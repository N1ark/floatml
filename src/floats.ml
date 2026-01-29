(** OCaml bindings for f16, f32, f64, f128 floating-point types *)

(* ============================================================================
 * F16 - Half-precision (16-bit) floating-point
 * ============================================================================ *)
module F16 = struct
  type t

  external of_string : string -> t = "caml_f16_of_string"
  external add : t -> t -> t = "caml_f16_add"
  external sub : t -> t -> t = "caml_f16_sub"
  external mul : t -> t -> t = "caml_f16_mul"
  external div : t -> t -> t = "caml_f16_div"
  external rem : t -> t -> t = "caml_f16_rem"
  external to_bits : t -> int = "caml_f16_to_bits"
  external of_bits : int -> t = "caml_f16_of_bits"
  external to_float : t -> float = "caml_f16_to_float"

  let to_z t = Z.of_int (to_bits t)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
end

(* ============================================================================
 * F32 - Single-precision (32-bit) floating-point
 * ============================================================================ *)
module F32 = struct
  type t

  external of_string : string -> t = "caml_f32_of_string"
  external add : t -> t -> t = "caml_f32_add"
  external sub : t -> t -> t = "caml_f32_sub"
  external mul : t -> t -> t = "caml_f32_mul"
  external div : t -> t -> t = "caml_f32_div"
  external rem : t -> t -> t = "caml_f32_rem"
  external to_bits : t -> int32 = "caml_f32_to_bits"
  external of_bits : int32 -> t = "caml_f32_of_bits"
  external to_float : t -> float = "caml_f32_to_float"

  let to_z t = Z.of_int32 (to_bits t)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
end

(* ============================================================================
 * F64 - Double-precision (64-bit) floating-point
 * ============================================================================ *)
module F64 = struct
  type t

  external of_string : string -> t = "caml_f64_of_string"
  external add : t -> t -> t = "caml_f64_add"
  external sub : t -> t -> t = "caml_f64_sub"
  external mul : t -> t -> t = "caml_f64_mul"
  external div : t -> t -> t = "caml_f64_div"
  external rem : t -> t -> t = "caml_f64_rem"
  external to_bits : t -> int64 = "caml_f64_to_bits"
  external of_bits : int64 -> t = "caml_f64_of_bits"
  external to_float : t -> float = "caml_f64_to_float"

  let to_z t = Z.of_int64 (to_bits t)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
end

(* ============================================================================
 * F128 - Quad-precision (128-bit) floating-point
 * ============================================================================ *)
module F128 = struct
  type t

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
  external size : unit -> int = "caml_f128_size"

  let of_bits ~low ~high = of_bits_raw low high

  (** Convert to Z.t representing the full 128-bit value.
      The result is: high * 2^64 + low (treating low as unsigned) *)
  let to_z t =
    let low = to_bits_low t in
    let high = to_bits_high t in
    (* Convert low as unsigned 64-bit integer *)
    let z_low = 
      if Int64.compare low 0L >= 0 then
        Z.of_int64 low
      else
        (* low is negative in two's complement, so add 2^64 *)
        Z.add (Z.of_int64 low) (Z.shift_left Z.one 64)
    in
    (* high can be treated as signed since it's the upper bits *)
    let z_high = Z.of_int64 high in
    Z.add (Z.shift_left z_high 64) z_low

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( mod ) = rem
end
