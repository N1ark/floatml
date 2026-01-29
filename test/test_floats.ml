(** Test file for the floats library *)

open Floats

let () =
  print_endline "=== Testing F16 (half-precision) ===";
  let a16 = F16.of_string "3.5" in
  let b16 = F16.of_string "1.5" in
  Printf.printf "a16 = %f, b16 = %f\n" (F16.to_float a16) (F16.to_float b16);
  Printf.printf "add: %f\n" (F16.to_float (F16.add a16 b16));
  Printf.printf "sub: %f\n" (F16.to_float (F16.sub a16 b16));
  Printf.printf "mul: %f\n" (F16.to_float (F16.mul a16 b16));
  Printf.printf "div: %f\n" (F16.to_float (F16.div a16 b16));
  Printf.printf "rem: %f\n" (F16.to_float (F16.rem a16 b16));
  Printf.printf "bits: 0x%04x\n" (F16.to_bits a16);
  Printf.printf "to_z: %s\n" (Z.to_string (F16.to_z a16));
  
  (* Test infix operators *)
  let open F16 in
  Printf.printf "infix add: %f\n" (to_float (a16 + b16));
  Printf.printf "infix sub: %f\n" (to_float (a16 - b16));
  Printf.printf "infix mul: %f\n" (to_float (a16 * b16));
  Printf.printf "infix div: %f\n" (to_float (a16 / b16));
  print_newline ();

  print_endline "=== Testing F32 (single-precision) ===";
  let a32 = F32.of_string "3.14159" in
  let b32 = F32.of_string "2.71828" in
  Printf.printf "a32 = %f, b32 = %f\n" (F32.to_float a32) (F32.to_float b32);
  Printf.printf "add: %f\n" (F32.to_float (F32.add a32 b32));
  Printf.printf "sub: %f\n" (F32.to_float (F32.sub a32 b32));
  Printf.printf "mul: %f\n" (F32.to_float (F32.mul a32 b32));
  Printf.printf "div: %f\n" (F32.to_float (F32.div a32 b32));
  Printf.printf "rem: %f\n" (F32.to_float (F32.rem a32 b32));
  Printf.printf "bits: 0x%08lx\n" (F32.to_bits a32);
  Printf.printf "to_z: %s\n" (Z.to_string (F32.to_z a32));
  print_newline ();

  print_endline "=== Testing F64 (double-precision) ===";
  let a64 = F64.of_string "3.141592653589793" in
  let b64 = F64.of_string "2.718281828459045" in
  Printf.printf "a64 = %.15f, b64 = %.15f\n" (F64.to_float a64) (F64.to_float b64);
  Printf.printf "add: %.15f\n" (F64.to_float (F64.add a64 b64));
  Printf.printf "sub: %.15f\n" (F64.to_float (F64.sub a64 b64));
  Printf.printf "mul: %.15f\n" (F64.to_float (F64.mul a64 b64));
  Printf.printf "div: %.15f\n" (F64.to_float (F64.div a64 b64));
  Printf.printf "rem: %.15f\n" (F64.to_float (F64.rem a64 b64));
  Printf.printf "bits: 0x%016Lx\n" (F64.to_bits a64);
  Printf.printf "to_z: %s\n" (Z.to_string (F64.to_z a64));
  print_newline ();

  print_endline "=== Testing F128 (quad-precision) ===";
  Printf.printf "F128 size on this platform: %d bytes\n" (F128.size ());
  let a128 = F128.of_string "3.141592653589793238462643383279" in
  let b128 = F128.of_string "2.718281828459045235360287471352" in
  Printf.printf "a128 = %.15f (displayed as double)\n" (F128.to_float a128);
  Printf.printf "b128 = %.15f (displayed as double)\n" (F128.to_float b128);
  Printf.printf "add: %.15f\n" (F128.to_float (F128.add a128 b128));
  Printf.printf "sub: %.15f\n" (F128.to_float (F128.sub a128 b128));
  Printf.printf "mul: %.15f\n" (F128.to_float (F128.mul a128 b128));
  Printf.printf "div: %.15f\n" (F128.to_float (F128.div a128 b128));
  Printf.printf "rem: %.15f\n" (F128.to_float (F128.rem a128 b128));
  Printf.printf "bits_low:  0x%016Lx\n" (F128.to_bits_low a128);
  Printf.printf "bits_high: 0x%016Lx\n" (F128.to_bits_high a128);
  Printf.printf "to_z: %s\n" (Z.to_string (F128.to_z a128));
  print_newline ();

  print_endline "=== Testing bit conversion round-trips ===";
  
  (* F16 round-trip *)
  let x16 = F16.of_string "42.5" in
  let bits16 = F16.to_bits x16 in
  let y16 = F16.of_bits bits16 in
  Printf.printf "F16 round-trip: %f -> 0x%04x -> %f\n" 
    (F16.to_float x16) bits16 (F16.to_float y16);
  
  (* F32 round-trip *)
  let x32 = F32.of_string "42.5" in
  let bits32 = F32.to_bits x32 in
  let y32 = F32.of_bits bits32 in
  Printf.printf "F32 round-trip: %f -> 0x%08lx -> %f\n" 
    (F32.to_float x32) bits32 (F32.to_float y32);
  
  (* F64 round-trip *)
  let x64 = F64.of_string "42.5" in
  let bits64 = F64.to_bits x64 in
  let y64 = F64.of_bits bits64 in
  Printf.printf "F64 round-trip: %f -> 0x%016Lx -> %f\n" 
    (F64.to_float x64) bits64 (F64.to_float y64);
  
  (* F128 round-trip *)
  let x128 = F128.of_string "42.5" in
  let low = F128.to_bits_low x128 in
  let high = F128.to_bits_high x128 in
  let y128 = F128.of_bits ~low ~high in
  Printf.printf "F128 round-trip: %f -> (0x%016Lx, 0x%016Lx) -> %f\n" 
    (F128.to_float x128) low high (F128.to_float y128);
  
  print_newline ();
  print_endline "All tests passed!"
