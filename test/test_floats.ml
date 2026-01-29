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
