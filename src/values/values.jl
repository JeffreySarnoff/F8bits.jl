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

values = [pair(0x00, 0),   pair(0x80, NaN),           # 0, NaN
          pair(0x01, 1/2^12), pair(0x81, -1/2^12),    # +Tiny, -Tiny
          pair(0x3E, 1-1/62), pair(0xBE, -(1-1/62)),  # +61//62, -61//62
          pair(0x3F, 1), pair(0xBF, -1),              # +1, -1
          pair(0x40, 1+1/62), pair(0xC0, -(1+1/62),   # +63//62, -63//62
          pair(0x7E, 2^12), pair(0xFE, -2^12),        # +Huge, -Huge
          pair(0x7F, Inf), pair(0xFF, -Inf),          # +Inf,  -Inf
       ]
           
  
