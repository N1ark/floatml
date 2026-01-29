(** F16 - IEEE 754 half-precision (16-bit) floating-point type *)
module F16 : sig
  type t
  
  (** Convert a string to an F16 value *)
  val of_string : string -> t
  
  (** Arithmetic operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  
  (** Convert to/from bit representation *)
  val to_bits : t -> int
  val of_bits : int -> t
  
  (** Convert to Z.t (exact bit representation) *)
  val to_z : t -> Z.t
  
  (** Convert to OCaml float for display *)
  val to_float : t -> float
  
  (** Infix operators *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
end

(** F32 - IEEE 754 single-precision (32-bit) floating-point type *)
module F32 : sig
  type t
  
  (** Convert a string to an F32 value *)
  val of_string : string -> t
  
  (** Arithmetic operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  
  (** Convert to/from bit representation *)
  val to_bits : t -> int32
  val of_bits : int32 -> t
  
  (** Convert to Z.t (exact bit representation) *)
  val to_z : t -> Z.t
  
  (** Convert to OCaml float for display *)
  val to_float : t -> float
  
  (** Infix operators *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
end

(** F64 - IEEE 754 double-precision (64-bit) floating-point type *)
module F64 : sig
  type t
  
  (** Convert a string to an F64 value *)
  val of_string : string -> t
  
  (** Arithmetic operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  
  (** Convert to/from bit representation *)
  val to_bits : t -> int64
  val of_bits : int64 -> t
  
  (** Convert to Z.t (exact bit representation) *)
  val to_z : t -> Z.t
  
  (** Convert to OCaml float for display *)
  val to_float : t -> float
  
  (** Infix operators *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
end

(** F128 - IEEE 754 quad-precision (128-bit) floating-point type
    Note: Implementation may use long double if native __float128 is unavailable *)
module F128 : sig
  type t
  
  (** Convert a string to an F128 value *)
  val of_string : string -> t
  
  (** Arithmetic operations *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  
  (** Convert to/from bit representation (as low and high 64-bit parts) *)
  val to_bits_low : t -> int64
  val to_bits_high : t -> int64
  val of_bits : low:int64 -> high:int64 -> t
  
  (** Convert to Z.t (exact bit representation, full 128 bits) *)
  val to_z : t -> Z.t
  
  (** Convert to OCaml float for display (with precision loss) *)
  val to_float : t -> float
  
  (** Get the size of the underlying type in bytes *)
  val size : unit -> int
  
  (** Infix operators *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( mod ) : t -> t -> t
end
