const Bias =  15
const Emax =  15
const Emin = -14
const Binades = 30

const SgnBits = 1
const ExpBits = 5
const SigBits = 2
const SgnMask = 0b1_00000_00
const ExpMask = 0b0_11111_00
const SigMask = 0b0_00000_11
const SgnShift = 7
const ExpShift = 2
const SigShift = 0

const Specials = 8
isspecial(x::UInt8) = (x & ExpMask) === ExpMask
isnormal(x::UInt8)  = 0b0_00001_00 <= (x & ExpMask) <= 0b0_11110_00
issubnormal(x::UInt8) = iszero(x & ExpMask) && !iszero(x & SigMask)
isinf(x::UInt8) =  (x & ~SgnMask) === 0b0_11111_10
signbit(x::UInt8) = !iszero(x & SgnMask)
isnan(x::UInt8) = x ===  0b1_11111_11

const NaN52           = 0b1_11111_11
const PosNaN          = 0b0_11111_11
const PosInf52        = 0b0_11111_10
const NegInf52        = 0b1_11111_10

const PosHuge         = 0b0_11111_01
const NegHuge         = 0b1_11111_01
const PosTiny         = 0b0_11111_00
const NegTiny         = 0b1_11111_00

const Zero52          = 0b0_00000_00
const MaxPosNormal    = 0b0_11110_11 # 57344 = 2^13 * 7
const MaxNegNormal    = 0b1_11110_11 # -57344
const MinPosNormal    = 0b0_00001_00 #  1/2^14
const MinNegNormal    = 0b1_00001_00 # -1/2^14
const MaxPosSubnormal = 0b0_00000_11 #  (3/4) * (1/2^14)
const MaxNegSubnormal = 0b1_00000_11 # -(3/4) * (1/2^14)
const MidPosSubnormal = 0b0_00000_10 #  (1/2) * (1/2^14)
const MidNegSubnormal = 0b1_00000_10 # -(1/2) * (1/2^14)
const MinPosSubnormal = 0b0_00000_01 #  1/2^16
const MinNegSubnormal = 0b1_00000_01 # -1/2^16

#=
# 30 groups of 4
0b0_00001_00 == 0x04       # (1 + 0/4) * 2^-14 (2^(1-Bias))
0b0_00001_01               # (1 + 1/4) * 2^-14
0b0_00001_10               # (1 + 2/4) * 2^-14
0b0_00001_11 == 0x07       # (1 + 3/4) * 2^-14
0b0_00010_00 == 0x08
..
0b0_11110_00 == 0x78 (120) # (1+0/4) * 2^15 (2^(30-Bias))
0b0_11110_01               # (1+1/4) * 2^15 
0b0_11110_10               # (1+1/2) * 2^15
0b0_11110_11 == 0x7b (123) # (1+3/4) * 2^15
=#

function value_normal(x::UInt8)
    iszero(x) && return(zero(Float16))
    isneg = signbit(x)
    significand = 1 + ((x & SigMask) // 4)
    exponent = ((x & ExpMask) >> 2) - Bias
    absvalue = 2.0^exponent * significand
    Float16(isneg ? -absvalue : absvalue)
end

function value_subnormal(x::UInt8)
    isneg = signbit(x)
    absvalue = (x & SigMask) * 1//2^14
    Float16(isneg ? -absvalue : absvalue)
end

function value_special(x::UInt8)
    isnan(x) && return Float16(NaN)
    if isinf(x)
       signbit(x) && return Float16(-Inf)
       return Float16(Inf)
    end
    throw( ErrorException("unknown special: ($(x))"))
end     
    
function valueof(x::UInt8)
    isnormal(x) && return value_normal(x)
    isspecial(x) && return value_special(x)
    return value_subnormal(x)
end

