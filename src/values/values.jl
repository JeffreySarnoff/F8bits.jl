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

totalpatterns = 256
specialpatterns = 4  # zero, -inf, +inf, nan
extrapatterns   = 4  # ±huge, ±tiny
availablepatterns = totalpatterns - specialpatterns - extrapatterns # 248 = 2^3 * 31


pair(u8::Core.UInt8, x) = Pair(u8, Float16(x))

values = [pair(0x00, 0),   pair(0x80, NaN),             # 0, NaN
          pair(0x01, 1//2^9), pair(0x81, -1//2^9),      # +Tiny, -Tiny
          pair(0x02, 1-63//64), pair(0x82, -(1-63/64)), # minpos, minneg
          pair(0x3E, 1-1//64), pair(0xBE, -(1-1//64)),  # +63//64, -63//64
          pair(0x3F, 1), pair(0xBF, -1),                # +1, -1
          pair(0x40, 1+1//64), pair(0xC0, -(1+1//64)),  # +65//64, -65//64
          pair(0x7E, 2^9), pair(0xFE, -2^9),            # +Huge, -Huge
          pair(0x7F, Inf), pair(0xFF, -Inf),            # +Inf,  -Inf
       ]
           
  
