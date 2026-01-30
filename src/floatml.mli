(** IEEE 754 rounding modes *)
type rounding_mode =
  | NearestEven   (** Round to nearest, ties to even (default IEEE 754 mode) *)
  | ToZero        (** Round toward zero (truncate) *)
  | Up            (** Round toward +infinity (ceiling) *)
  | Down          (** Round toward -infinity (floor) *)
  | NearestAway   (** Round to nearest, ties away from zero *)

module type FloatType := sig
    type t
    type bits

    (** Convert a string to an F16 value *)
    val of_string : string -> t

    (** Addition *)
    val add : t -> t -> t

    (** Subtraction *)
    val sub : t -> t -> t

    (** Multiplication *)
    val mul : t -> t -> t

    (** Division *)
    val div : t -> t -> t

    (** Remainder *)
    val rem : t -> t -> t

    (** Convert from some bit representation  *)
    val of_bits : bits -> t

    (** Convert to some bit representation  *)
    val to_bits : t -> bits

    (** Convert to Z.t (exact bit representation) *)
    val to_z : t -> Z.t

    (** Convert to OCaml float for display *)
    val to_float : t -> float

    (** Classify a floating-point value according to IEEE 754 *)
    val fpclass : t -> Stdlib.fpclass

    (** Absolute value (IEEE 754 compliant - clears sign bit) *)
    val abs : t -> t

    (** Round to integer value according to rounding mode *)
    val round : rounding_mode -> t -> t


    (* Infix operators *)

    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( mod ) : t -> t -> t
end

(** F16 - IEEE 754 half-precision (16-bit) floating-point type

    Uses native _Float16 hardware support when available (modern ARM64, x86 with AVX512-FP16).
    Falls back to IEEE-754 compliant software emulation with proper rounding otherwise. *)
module F16 : FloatType with type bits = int

(** F32 - IEEE 754 single-precision (32-bit) floating-point type *)
module F32 : FloatType with type bits = int32

(** F64 - IEEE 754 double-precision (64-bit) floating-point type *)
module F64 : FloatType with type bits = int64

(** F128 - IEEE 754 quad-precision (128-bit) floating-point type
    Note: Implementation may use long double if native __float128 is unavailable *)
module F128 : FloatType with type bits = (int64 * int64)
