/*
 * C stubs for f16, f32, f64, f128 float types
 * Provides OCaml bindings for arithmetic operations and bit conversions
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <fenv.h>

/* ============================================================================
 * F16 (half-precision) implementation
 * IEEE 754 half-precision: 1 sign bit, 5 exponent bits, 10 mantissa bits
 * ============================================================================ */

typedef uint16_t f16_t;

/* Convert float to f16 */
static f16_t float_to_f16(float f) {
    uint32_t bits;
    memcpy(&bits, &f, sizeof(bits));
    
    uint32_t sign = (bits >> 31) & 0x1;
    int32_t exp = ((bits >> 23) & 0xFF) - 127;
    uint32_t mant = bits & 0x7FFFFF;
    
    uint16_t result;
    
    if (exp == 128) {
        /* Inf or NaN */
        result = (sign << 15) | (0x1F << 10) | (mant ? (mant >> 13) | 0x1 : 0);
    } else if (exp > 15) {
        /* Overflow to infinity */
        result = (sign << 15) | (0x1F << 10);
    } else if (exp < -14) {
        /* Underflow or denormal */
        if (exp < -24) {
            result = sign << 15;
        } else {
            mant |= 0x800000;
            int shift = -exp - 14 + 13;
            result = (sign << 15) | (mant >> shift);
        }
    } else {
        result = (sign << 15) | ((exp + 15) << 10) | (mant >> 13);
    }
    
    return result;
}

/* Convert f16 to float */
static float f16_to_float(f16_t h) {
    uint32_t sign = (h >> 15) & 0x1;
    uint32_t exp = (h >> 10) & 0x1F;
    uint32_t mant = h & 0x3FF;
    
    uint32_t result;
    
    if (exp == 0) {
        if (mant == 0) {
            result = sign << 31;
        } else {
            /* Denormalized */
            exp = 1;
            while (!(mant & 0x400)) {
                mant <<= 1;
                exp--;
            }
            mant &= 0x3FF;
            result = (sign << 31) | ((exp + 127 - 15) << 23) | (mant << 13);
        }
    } else if (exp == 0x1F) {
        result = (sign << 31) | (0xFF << 23) | (mant << 13);
    } else {
        result = (sign << 31) | ((exp + 127 - 15) << 23) | (mant << 13);
    }
    
    float f;
    memcpy(&f, &result, sizeof(f));
    return f;
}

/* F16 custom block operations */
static struct custom_operations f16_ops = {
    "floats.f16",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F16_val(v) (*((f16_t *) Data_custom_val(v)))

static value alloc_f16(f16_t val) {
    value v = caml_alloc_custom(&f16_ops, sizeof(f16_t), 0, 1);
    F16_val(v) = val;
    return v;
}

CAMLprim value caml_f16_of_string(value s) {
    CAMLparam1(s);
    float f = strtof(String_val(s), NULL);
    CAMLreturn(alloc_f16(float_to_f16(f)));
}

CAMLprim value caml_f16_add(value a, value b) {
    CAMLparam2(a, b);
    float fa = f16_to_float(F16_val(a));
    float fb = f16_to_float(F16_val(b));
    CAMLreturn(alloc_f16(float_to_f16(fa + fb)));
}

CAMLprim value caml_f16_sub(value a, value b) {
    CAMLparam2(a, b);
    float fa = f16_to_float(F16_val(a));
    float fb = f16_to_float(F16_val(b));
    CAMLreturn(alloc_f16(float_to_f16(fa - fb)));
}

CAMLprim value caml_f16_mul(value a, value b) {
    CAMLparam2(a, b);
    float fa = f16_to_float(F16_val(a));
    float fb = f16_to_float(F16_val(b));
    CAMLreturn(alloc_f16(float_to_f16(fa * fb)));
}

CAMLprim value caml_f16_div(value a, value b) {
    CAMLparam2(a, b);
    float fa = f16_to_float(F16_val(a));
    float fb = f16_to_float(F16_val(b));
    CAMLreturn(alloc_f16(float_to_f16(fa / fb)));
}

CAMLprim value caml_f16_rem(value a, value b) {
    CAMLparam2(a, b);
    float fa = f16_to_float(F16_val(a));
    float fb = f16_to_float(F16_val(b));
    CAMLreturn(alloc_f16(float_to_f16(fmodf(fa, fb))));
}

CAMLprim value caml_f16_to_bits(value v) {
    CAMLparam1(v);
    CAMLreturn(Val_int(F16_val(v)));
}

CAMLprim value caml_f16_of_bits(value bits) {
    CAMLparam1(bits);
    CAMLreturn(alloc_f16((f16_t)Int_val(bits)));
}

CAMLprim value caml_f16_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double(f16_to_float(F16_val(v))));
}

/* ============================================================================
 * F32 (single-precision) implementation
 * IEEE 754 single-precision: 1 sign bit, 8 exponent bits, 23 mantissa bits
 * ============================================================================ */

typedef float f32_t;

static struct custom_operations f32_ops = {
    "floats.f32",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F32_val(v) (*((f32_t *) Data_custom_val(v)))

static value alloc_f32(f32_t val) {
    value v = caml_alloc_custom(&f32_ops, sizeof(f32_t), 0, 1);
    F32_val(v) = val;
    return v;
}

CAMLprim value caml_f32_of_string(value s) {
    CAMLparam1(s);
    float f = strtof(String_val(s), NULL);
    CAMLreturn(alloc_f32(f));
}

CAMLprim value caml_f32_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) + F32_val(b)));
}

CAMLprim value caml_f32_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) - F32_val(b)));
}

CAMLprim value caml_f32_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) * F32_val(b)));
}

CAMLprim value caml_f32_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(F32_val(a) / F32_val(b)));
}

CAMLprim value caml_f32_rem(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f32(fmodf(F32_val(a), F32_val(b))));
}

CAMLprim value caml_f32_to_bits(value v) {
    CAMLparam1(v);
    uint32_t bits;
    f32_t f = F32_val(v);
    memcpy(&bits, &f, sizeof(bits));
    CAMLreturn(caml_copy_int32(bits));
}

CAMLprim value caml_f32_of_bits(value bits) {
    CAMLparam1(bits);
    uint32_t b = Int32_val(bits);
    f32_t f;
    memcpy(&f, &b, sizeof(f));
    CAMLreturn(alloc_f32(f));
}

CAMLprim value caml_f32_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double((double)F32_val(v)));
}

/* ============================================================================
 * F64 (double-precision) implementation
 * IEEE 754 double-precision: 1 sign bit, 11 exponent bits, 52 mantissa bits
 * ============================================================================ */

typedef double f64_t;

static struct custom_operations f64_ops = {
    "floats.f64",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F64_val(v) (*((f64_t *) Data_custom_val(v)))

static value alloc_f64(f64_t val) {
    value v = caml_alloc_custom(&f64_ops, sizeof(f64_t), 0, 1);
    F64_val(v) = val;
    return v;
}

CAMLprim value caml_f64_of_string(value s) {
    CAMLparam1(s);
    double d = strtod(String_val(s), NULL);
    CAMLreturn(alloc_f64(d));
}

CAMLprim value caml_f64_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) + F64_val(b)));
}

CAMLprim value caml_f64_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) - F64_val(b)));
}

CAMLprim value caml_f64_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) * F64_val(b)));
}

CAMLprim value caml_f64_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(F64_val(a) / F64_val(b)));
}

CAMLprim value caml_f64_rem(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f64(fmod(F64_val(a), F64_val(b))));
}

CAMLprim value caml_f64_to_bits(value v) {
    CAMLparam1(v);
    uint64_t bits;
    f64_t d = F64_val(v);
    memcpy(&bits, &d, sizeof(bits));
    CAMLreturn(caml_copy_int64(bits));
}

CAMLprim value caml_f64_of_bits(value bits) {
    CAMLparam1(bits);
    uint64_t b = Int64_val(bits);
    f64_t d;
    memcpy(&d, &b, sizeof(d));
    CAMLreturn(alloc_f64(d));
}

CAMLprim value caml_f64_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double(F64_val(v)));
}

/* ============================================================================
 * F128 (quad-precision) implementation
 * IEEE 754 quad-precision: 1 sign bit, 15 exponent bits, 112 mantissa bits
 * Note: We use long double which may or may not be true 128-bit depending on platform
 * On x86-64 Linux, long double is 80-bit extended precision
 * On some platforms, __float128 may be available
 * ============================================================================ */

/* Use a 128-bit representation stored as two 64-bit values for portability */
typedef struct {
    uint64_t low;
    uint64_t high;
} f128_bits_t;

#if defined(__SIZEOF_FLOAT128__) || defined(__FLOAT128__)
typedef __float128 f128_t;
#define HAS_FLOAT128 1
#else
/* Fall back to long double */
typedef long double f128_t;
#define HAS_FLOAT128 0
#endif

static struct custom_operations f128_ops = {
    "floats.f128",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

#define F128_val(v) (*((f128_t *) Data_custom_val(v)))

static value alloc_f128(f128_t val) {
    value v = caml_alloc_custom(&f128_ops, sizeof(f128_t), 0, 1);
    F128_val(v) = val;
    return v;
}

CAMLprim value caml_f128_of_string(value s) {
    CAMLparam1(s);
#if HAS_FLOAT128
    f128_t f = strtof128(String_val(s), NULL);
#else
    f128_t f = strtold(String_val(s), NULL);
#endif
    CAMLreturn(alloc_f128(f));
}

CAMLprim value caml_f128_add(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) + F128_val(b)));
}

CAMLprim value caml_f128_sub(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) - F128_val(b)));
}

CAMLprim value caml_f128_mul(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) * F128_val(b)));
}

CAMLprim value caml_f128_div(value a, value b) {
    CAMLparam2(a, b);
    CAMLreturn(alloc_f128(F128_val(a) / F128_val(b)));
}

CAMLprim value caml_f128_rem(value a, value b) {
    CAMLparam2(a, b);
#if HAS_FLOAT128
    CAMLreturn(alloc_f128(fmodq(F128_val(a), F128_val(b))));
#else
    CAMLreturn(alloc_f128(fmodl(F128_val(a), F128_val(b))));
#endif
}

/* Return low 64 bits of f128 representation */
CAMLprim value caml_f128_to_bits_low(value v) {
    CAMLparam1(v);
    f128_t f = F128_val(v);
    uint64_t bits[2];
    memcpy(bits, &f, sizeof(f128_t) <= 16 ? sizeof(f128_t) : 16);
    CAMLreturn(caml_copy_int64(bits[0]));
}

/* Return high 64 bits of f128 representation */
CAMLprim value caml_f128_to_bits_high(value v) {
    CAMLparam1(v);
    f128_t f = F128_val(v);
    uint64_t bits[2] = {0, 0};
    memcpy(bits, &f, sizeof(f128_t) <= 16 ? sizeof(f128_t) : 16);
    CAMLreturn(caml_copy_int64(bits[1]));
}

CAMLprim value caml_f128_of_bits(value low, value high) {
    CAMLparam2(low, high);
    uint64_t bits[2];
    bits[0] = Int64_val(low);
    bits[1] = Int64_val(high);
    f128_t f;
    memcpy(&f, bits, sizeof(f128_t) <= 16 ? sizeof(f128_t) : 16);
    CAMLreturn(alloc_f128(f));
}

CAMLprim value caml_f128_to_float(value v) {
    CAMLparam1(v);
    CAMLreturn(caml_copy_double((double)F128_val(v)));
}

/* Return the size of f128_t for debugging */
CAMLprim value caml_f128_size(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_int(sizeof(f128_t)));
}
