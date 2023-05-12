#=
    https://observablehq.com/@benaubin/floating-point
    provide bits of precision (2) == significand bits - 1
            bits of exponent (5) 
            [bits of exponent + bits of precision == 7]


    exponentbits = 5, precisionbits = 2

    0x01 ↦ 0.0078125 (1 / 128)
    0x1C ↦ 128 (1 / 0x01)

    exponentbits = 5, significandbits = 3
    0x01 ↦ 

=#




pair(u8::Core.UInt8, x) = Pair(u8, Float16(x))

const Huge = pair(0x7E, 2^14)
const Tiny = pair(0x1/65536 # 1/2^16

values = [pair(0x00, 0),   pair(0x80, NaN),
          pair(0x
          pair(0x7F, Inf), pair(0xFF, -Inf),
          pair(0x7E, Huge), pair(0xFE, -Huge),
           
  
