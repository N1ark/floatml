(** Test file for the floats library with assertions *)

open Floats

(** Tolerance for floating-point comparisons *)
let epsilon = 1e-6
let epsilon_f16 = 1e-2  (* F16 has lower precision *)

(** Check if float is infinity *)
let is_inf x = x = infinity || x = neg_infinity

(** Check if float is NaN *)
let is_nan x = x <> x

(** Test counter *)
let tests_run = ref 0
let tests_passed = ref 0

(** Assert helper that tracks test results *)
let assert_true msg condition =
  incr tests_run;
  if condition then begin
    incr tests_passed;
    Printf.printf "  PASS: %s\n" msg
  end else begin
    Printf.printf "  FAIL: %s\n" msg;
    failwith (Printf.sprintf "Assertion failed: %s" msg)
  end

(** Assert float equality within tolerance *)
let assert_float_eq ?(eps=epsilon) msg expected actual =
  let diff = Float.abs (expected -. actual) in
  assert_true (Printf.sprintf "%s (expected %f, got %f)" msg expected actual) (diff < eps)

(** Assert int equality *)
let assert_int_eq msg expected actual =
  assert_true (Printf.sprintf "%s (expected %d, got %d)" msg expected actual) (expected = actual)

(** Assert int32 equality *)
let assert_int32_eq msg expected actual =
  assert_true (Printf.sprintf "%s (expected 0x%08lx, got 0x%08lx)" msg expected actual) (expected = actual)

(** Assert int64 equality *)
let assert_int64_eq msg expected actual =
  assert_true (Printf.sprintf "%s (expected 0x%016Lx, got 0x%016Lx)" msg expected actual) (expected = actual)

(** Assert Z.t equality *)
let assert_z_eq msg expected actual =
  assert_true (Printf.sprintf "%s (expected %s, got %s)" msg (Z.to_string expected) (Z.to_string actual)) 
    (Z.equal expected actual)

let test_f16 () =
  print_endline "\n=== Testing F16 (half-precision) ===";
  Printf.printf "  (F16 using native hardware: %b)\n" (F16.is_native ());
  
  let a = F16.of_string "3.5" in
  let b = F16.of_string "1.5" in
  
  (* Test basic values *)
  assert_float_eq ~eps:epsilon_f16 "F16 parse 3.5" 3.5 (F16.to_float a);
  assert_float_eq ~eps:epsilon_f16 "F16 parse 1.5" 1.5 (F16.to_float b);
  
  (* Test arithmetic *)
  assert_float_eq ~eps:epsilon_f16 "F16 add" 5.0 (F16.to_float (F16.add a b));
  assert_float_eq ~eps:epsilon_f16 "F16 sub" 2.0 (F16.to_float (F16.sub a b));
  assert_float_eq ~eps:epsilon_f16 "F16 mul" 5.25 (F16.to_float (F16.mul a b));
  assert_float_eq ~eps:epsilon_f16 "F16 div" 2.333 (F16.to_float (F16.div a b));
  assert_float_eq ~eps:epsilon_f16 "F16 rem" 0.5 (F16.to_float (F16.rem a b));
  
  (* Test bit representation - 3.5 in F16 is 0x4300 *)
  assert_int_eq "F16 to_bits 3.5" 0x4300 (F16.to_bits a);
  assert_z_eq "F16 to_z 3.5" (Z.of_int 0x4300) (F16.to_z a);
  
  (* Test round-trip *)
  let bits = F16.to_bits a in
  let a' = F16.of_bits bits in
  assert_float_eq ~eps:epsilon_f16 "F16 round-trip" (F16.to_float a) (F16.to_float a');
  
  (* Test infix operators *)
  let open F16 in
  assert_float_eq ~eps:epsilon_f16 "F16 infix +" 5.0 (to_float (a + b));
  assert_float_eq ~eps:epsilon_f16 "F16 infix -" 2.0 (to_float (a - b));
  assert_float_eq ~eps:epsilon_f16 "F16 infix *" 5.25 (to_float (a * b));
  assert_float_eq ~eps:epsilon_f16 "F16 infix /" 2.333 (to_float (a / b))

(** Test IEEE-754 compliance for F16 *)
let test_f16_ieee754 () =
  print_endline "\n=== Testing F16 IEEE-754 compliance ===";
  
  (* Test well-known bit patterns *)
  
  (* Zero: 0x0000 *)
  let zero = F16.of_bits 0x0000 in
  assert_float_eq "F16 +0 value" 0.0 (F16.to_float zero);
  assert_int_eq "F16 +0 bits" 0x0000 (F16.to_bits zero);
  
  (* Negative zero: 0x8000 *)
  let neg_zero = F16.of_bits 0x8000 in
  assert_float_eq "F16 -0 value" 0.0 (F16.to_float neg_zero);
  assert_int_eq "F16 -0 bits" 0x8000 (F16.to_bits neg_zero);
  
  (* One: 0x3C00 (exp=15, mant=0 -> 2^0 * 1.0 = 1.0) *)
  let one = F16.of_bits 0x3C00 in
  assert_float_eq "F16 1.0 value" 1.0 (F16.to_float one);
  assert_int_eq "F16 1.0 bits" 0x3C00 (F16.to_bits one);
  
  (* Negative one: 0xBC00 *)
  let neg_one = F16.of_bits 0xBC00 in
  assert_float_eq "F16 -1.0 value" (-1.0) (F16.to_float neg_one);
  assert_int_eq "F16 -1.0 bits" 0xBC00 (F16.to_bits neg_one);
  
  (* Two: 0x4000 (exp=16, mant=0 -> 2^1 * 1.0 = 2.0) *)
  let two = F16.of_bits 0x4000 in
  assert_float_eq "F16 2.0 value" 2.0 (F16.to_float two);
  assert_int_eq "F16 2.0 bits" 0x4000 (F16.to_bits two);
  
  (* Infinity: 0x7C00 *)
  let inf = F16.of_bits 0x7C00 in
  assert_true "F16 +inf is infinity" (is_inf (F16.to_float inf));
  assert_int_eq "F16 +inf bits" 0x7C00 (F16.to_bits inf);
  
  (* Negative infinity: 0xFC00 *)
  let neg_inf = F16.of_bits 0xFC00 in
  assert_true "F16 -inf is infinity" (is_inf (F16.to_float neg_inf));
  assert_int_eq "F16 -inf bits" 0xFC00 (F16.to_bits neg_inf);
  
  (* NaN: 0x7E00 (quiet NaN) *)
  let nan_val = F16.of_bits 0x7E00 in
  assert_true "F16 NaN is NaN" (is_nan (F16.to_float nan_val));
  
  (* Smallest positive normal: 0x0400 (exp=1, mant=0 -> 2^-14) *)
  let min_normal = F16.of_bits 0x0400 in
  let expected_min_normal = ldexp 1.0 (-14) in  (* 2^-14 ≈ 6.103515625e-05 *)
  assert_float_eq ~eps:1e-10 "F16 min normal value" expected_min_normal (F16.to_float min_normal);
  assert_int_eq "F16 min normal bits" 0x0400 (F16.to_bits min_normal);
  
  (* Largest normal: 0x7BFF (exp=30, mant=0x3FF -> (2 - 2^-10) * 2^15 = 65504) *)
  let max_normal = F16.of_bits 0x7BFF in
  assert_float_eq ~eps:1.0 "F16 max normal value" 65504.0 (F16.to_float max_normal);
  assert_int_eq "F16 max normal bits" 0x7BFF (F16.to_bits max_normal);
  
  (* Smallest positive denormal: 0x0001 (exp=0, mant=1 -> 2^-24) *)
  let min_denormal = F16.of_bits 0x0001 in
  let expected_min_denormal = ldexp 1.0 (-24) in  (* 2^-24 ≈ 5.96e-08 *)
  assert_float_eq ~eps:1e-12 "F16 min denormal value" expected_min_denormal (F16.to_float min_denormal);
  assert_int_eq "F16 min denormal bits" 0x0001 (F16.to_bits min_denormal);
  
  (* Largest denormal: 0x03FF (exp=0, mant=0x3FF -> (2^-14) * (1 - 2^-10)) *)
  let max_denormal = F16.of_bits 0x03FF in
  let expected_max_denormal = ldexp 1.0 (-14) *. (1.0 -. ldexp 1.0 (-10)) in
  assert_float_eq ~eps:1e-10 "F16 max denormal value" expected_max_denormal (F16.to_float max_denormal);
  assert_int_eq "F16 max denormal bits" 0x03FF (F16.to_bits max_denormal);
  
  (* Test rounding: round to nearest, ties to even *)
  (* 1.0 + 2^-11 should round to 1.0 (round down, ties to even) *)
  (* 1.0 + 2^-11 + 2^-12 should round to 1.0 + 2^-10 (round up) *)
  
  (* Test that 1.5 is exact: 0x3E00 *)
  let one_half = F16.of_string "1.5" in
  assert_int_eq "F16 1.5 bits" 0x3E00 (F16.to_bits one_half);
  
  (* Test that 0.5 is exact: 0x3800 *)
  let half = F16.of_string "0.5" in
  assert_int_eq "F16 0.5 bits" 0x3800 (F16.to_bits half);
  
  (* Test overflow to infinity *)
  let big = F16.of_string "100000.0" in
  assert_true "F16 overflow to inf" (is_inf (F16.to_float big));
  
  (* Test underflow to zero *)
  let tiny = F16.of_string "1e-10" in
  assert_float_eq "F16 underflow to zero" 0.0 (F16.to_float tiny);
  
  (* Test arithmetic with special values *)
  let inf16 = F16.of_bits 0x7C00 in
  let zero16 = F16.of_bits 0x0000 in
  let one16 = F16.of_bits 0x3C00 in
  
  (* inf + 1 = inf *)
  assert_true "F16 inf + 1 = inf" (is_inf (F16.to_float (F16.add inf16 one16)));
  
  (* inf - inf = NaN *)
  assert_true "F16 inf - inf = NaN" (is_nan (F16.to_float (F16.sub inf16 inf16)));
  
  (* 0 * inf = NaN *)
  assert_true "F16 0 * inf = NaN" (is_nan (F16.to_float (F16.mul zero16 inf16)));
  
  (* 1 / 0 = inf *)
  assert_true "F16 1 / 0 = inf" (is_inf (F16.to_float (F16.div one16 zero16)));
  
  (* 0 / 0 = NaN *)
  assert_true "F16 0 / 0 = NaN" (is_nan (F16.to_float (F16.div zero16 zero16)))

let test_f32 () =
  print_endline "\n=== Testing F32 (single-precision) ===";
  
  let a = F32.of_string "3.5" in
  let b = F32.of_string "1.5" in
  
  (* Test basic values *)
  assert_float_eq "F32 parse 3.5" 3.5 (F32.to_float a);
  assert_float_eq "F32 parse 1.5" 1.5 (F32.to_float b);
  
  (* Test arithmetic *)
  assert_float_eq "F32 add" 5.0 (F32.to_float (F32.add a b));
  assert_float_eq "F32 sub" 2.0 (F32.to_float (F32.sub a b));
  assert_float_eq "F32 mul" 5.25 (F32.to_float (F32.mul a b));
  assert_float_eq "F32 div" 2.333333 (F32.to_float (F32.div a b));
  assert_float_eq "F32 rem" 0.5 (F32.to_float (F32.rem a b));
  
  (* Test bit representation - 3.5 in F32 is 0x40600000 *)
  assert_int32_eq "F32 to_bits 3.5" 0x40600000l (F32.to_bits a);
  assert_z_eq "F32 to_z 3.5" (Z.of_int32 0x40600000l) (F32.to_z a);
  
  (* Test round-trip *)
  let bits = F32.to_bits a in
  let a' = F32.of_bits bits in
  assert_float_eq "F32 round-trip" (F32.to_float a) (F32.to_float a');
  
  (* Test infix operators *)
  let open F32 in
  assert_float_eq "F32 infix +" 5.0 (to_float (a + b));
  assert_float_eq "F32 infix -" 2.0 (to_float (a - b));
  assert_float_eq "F32 infix *" 5.25 (to_float (a * b));
  assert_float_eq "F32 infix /" 2.333333 (to_float (a / b))

let test_f64 () =
  print_endline "\n=== Testing F64 (double-precision) ===";
  
  let a = F64.of_string "3.5" in
  let b = F64.of_string "1.5" in
  
  (* Test basic values *)
  assert_float_eq "F64 parse 3.5" 3.5 (F64.to_float a);
  assert_float_eq "F64 parse 1.5" 1.5 (F64.to_float b);
  
  (* Test arithmetic *)
  assert_float_eq "F64 add" 5.0 (F64.to_float (F64.add a b));
  assert_float_eq "F64 sub" 2.0 (F64.to_float (F64.sub a b));
  assert_float_eq "F64 mul" 5.25 (F64.to_float (F64.mul a b));
  assert_float_eq "F64 div" 2.333333333333333 (F64.to_float (F64.div a b));
  assert_float_eq "F64 rem" 0.5 (F64.to_float (F64.rem a b));
  
  (* Test bit representation - 3.5 in F64 is 0x400C000000000000 *)
  assert_int64_eq "F64 to_bits 3.5" 0x400C000000000000L (F64.to_bits a);
  assert_z_eq "F64 to_z 3.5" (Z.of_int64 0x400C000000000000L) (F64.to_z a);
  
  (* Test round-trip *)
  let bits = F64.to_bits a in
  let a' = F64.of_bits bits in
  assert_float_eq "F64 round-trip" (F64.to_float a) (F64.to_float a');
  
  (* Test infix operators *)
  let open F64 in
  assert_float_eq "F64 infix +" 5.0 (to_float (a + b));
  assert_float_eq "F64 infix -" 2.0 (to_float (a - b));
  assert_float_eq "F64 infix *" 5.25 (to_float (a * b));
  assert_float_eq "F64 infix /" 2.333333333333333 (to_float (a / b))

let test_f128 () =
  print_endline "\n=== Testing F128 (quad-precision) ===";
  Printf.printf "  (F128 size on this platform: %d bytes)\n" (F128.size ());
  
  let a = F128.of_string "3.5" in
  let b = F128.of_string "1.5" in
  
  (* Test basic values - note: may lose precision when converting to float *)
  assert_float_eq "F128 parse 3.5" 3.5 (F128.to_float a);
  assert_float_eq "F128 parse 1.5" 1.5 (F128.to_float b);
  
  (* Test arithmetic *)
  assert_float_eq "F128 add" 5.0 (F128.to_float (F128.add a b));
  assert_float_eq "F128 sub" 2.0 (F128.to_float (F128.sub a b));
  assert_float_eq "F128 mul" 5.25 (F128.to_float (F128.mul a b));
  assert_float_eq "F128 div" 2.333333333333333 (F128.to_float (F128.div a b));
  assert_float_eq "F128 rem" 0.5 (F128.to_float (F128.rem a b));
  
  (* Test round-trip *)
  let low = F128.to_bits_low a in
  let high = F128.to_bits_high a in
  let a' = F128.of_bits ~low ~high in
  assert_float_eq "F128 round-trip" (F128.to_float a) (F128.to_float a');
  
  (* Test to_z produces non-zero value *)
  let z = F128.to_z a in
  assert_true "F128 to_z is non-zero" (not (Z.equal z Z.zero));
  
  (* Test infix operators *)
  let open F128 in
  assert_float_eq "F128 infix +" 5.0 (to_float (a + b));
  assert_float_eq "F128 infix -" 2.0 (to_float (a - b));
  assert_float_eq "F128 infix *" 5.25 (to_float (a * b));
  assert_float_eq "F128 infix /" 2.333333333333333 (to_float (a / b))

let test_special_values () =
  print_endline "\n=== Testing special values ===";
  
  (* Test zero *)
  let zero32 = F32.of_string "0.0" in
  assert_float_eq "F32 zero" 0.0 (F32.to_float zero32);
  assert_int32_eq "F32 zero bits" 0x00000000l (F32.to_bits zero32);
  
  (* Test negative zero *)
  let neg_zero32 = F32.of_string "-0.0" in
  assert_float_eq "F32 -0.0 value" 0.0 (F32.to_float neg_zero32);
  assert_int32_eq "F32 -0.0 bits" 0x80000000l (F32.to_bits neg_zero32);
  
  (* Test one *)
  let one32 = F32.of_string "1.0" in
  assert_float_eq "F32 one" 1.0 (F32.to_float one32);
  assert_int32_eq "F32 one bits" 0x3F800000l (F32.to_bits one32);
  
  (* Test infinity *)
  let inf32 = F32.of_string "inf" in
  assert_true "F32 infinity is infinity" (is_inf (F32.to_float inf32));
  assert_int32_eq "F32 infinity bits" 0x7F800000l (F32.to_bits inf32);
  
  (* Test negative infinity *)
  let neg_inf32 = F32.of_string "-inf" in
  assert_true "F32 -infinity is infinity" (is_inf (F32.to_float neg_inf32));
  assert_int32_eq "F32 -infinity bits" 0xFF800000l (F32.to_bits neg_inf32);
  
  (* Test NaN *)
  let nan32 = F32.of_string "nan" in
  assert_true "F32 nan is nan" (is_nan (F32.to_float nan32))

let test_z_conversions () =
  print_endline "\n=== Testing Z.t conversions ===";
  
  (* Test that Z.t values are correct bit representations *)
  let f32 = F32.of_string "1.0" in
  let z32 = F32.to_z f32 in
  assert_z_eq "F32 1.0 to Z" (Z.of_string "1065353216") z32;  (* 0x3F800000 *)
  
  let f64 = F64.of_string "1.0" in
  let z64 = F64.to_z f64 in
  assert_z_eq "F64 1.0 to Z" (Z.of_string "4607182418800017408") z64;  (* 0x3FF0000000000000 *)
  
  (* Test F16 *)
  let f16 = F16.of_string "1.0" in
  let z16 = F16.to_z f16 in
  assert_z_eq "F16 1.0 to Z" (Z.of_int 0x3C00) z16

let () =
  print_endline "Running floats library tests...";
  
  test_f16 ();
  test_f16_ieee754 ();
  test_f32 ();
  test_f64 ();
  test_f128 ();
  test_special_values ();
  test_z_conversions ();
  
  Printf.printf "\n=== Results: %d/%d tests passed ===\n" !tests_passed !tests_run;
  if !tests_passed = !tests_run then
    print_endline "All tests passed!"
  else begin
    Printf.printf "FAILED: %d tests failed\n" (!tests_run - !tests_passed);
    exit 1
  end
