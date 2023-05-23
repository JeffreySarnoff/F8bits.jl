export NaN8, Zero8, Inf8, 
       isinfnan, ispositive, isnonnegative, isnonpositive, isnegative, isnormal

import Base: reinterpret, 
             exponent_bits, significand_bits,
             sign_mask, exponent_mask, significand_mask,
             exponent_bias, exponent_one, exponent_half,
             UInt8, Int8,
             zero, one, iszero, isone,
             isnan, isinf, isreal, isfinite, isnormal, issubnormal,

abstract type AbstractFloat8 <: AbstractFloat end

#             E is the number of bits assigned to the exponent
struct Float8{E} <: AbstractFloat8
    x::UInt8
end

Float8{E}(x::Int8) = Float8{E}(reinterpret(UInt8, x))

value(float8::Float8{E}) = float8.x

UInt8(x::Float8{E}) where {E} = value(x)
Int8(x::Float8{E}) where {E} = reinterpret(Int8, value(x))
  
# following float.jl loosely

reinterpret(::Type{Unsigned},  ::Type{Float8{E}}) where {E} = UInt8
reinterpret(::Type{Signed},    ::Type{Float8{E}}) where {E} = Int8

reinterpret(::Type{Unsigned},  x::Float8{E}) where {E} = value(x)
reinterpret(::Type{Signed},    x::Float8{E}) where {E} = reinterpret(Int8, value(x))
reinterpret(::Type{Float8{E}}, x::UInt8)     where {E} = Float8{E}(x)
reinterpret(::Type{Float8{E}}, x::Int8)      where {E} = Float8{E}(reinterpret(UInt8, x))

sign_bits(::Type{Float8{E}}) where {E} = 0x01
exponent_bits(::Type{Float8{E}}) where {E} = UInt8(E)
significand_bits(::Type{Float8{E}}) where {E} = 0x07 - UInt8(E)

sign_shift(::Type{Float8{E}}) where {E} = 0x07
exponent_shift(::Type{Float8{E}}) where {E} = significand_bits(Float8{E})
signficand_shift(::Type{Float8{E}}) where {E} = 0x00

sign_mask_low(::Type{Float8{E}}) where {E} = 0x01
exponent_mask_low(::Type{Float8{E}}) where {E} = (0x01 << Base.exponent_bits(Float8{E})) - 0x01
significand_mask_low(::Type{Float8{E}}) where {E}  = (0x01 << Base.significand_bits(Float8{E})) - 0x01

sign_unmask_low(::Type{Float8{E}}) where {E} = ~sign_mask_low(Float8{E})
exponent_unmask_low(::Type{Float8{E}}) where {E} = ~exponent_mask_low(Float8{E})
significand_unmask_low(::Type{Float8{E}}) where {E}  = ~significand_mask_low(Float8{E})

sign_mask(::Type{Float8{E}}) where {E} = sign_mask_low(Float8{E}) << sign_shift(Float8{E})
significand_mask(::Type{Float8{E}}) where {E}  = significand_mask_low(Float8{E}) << significand_shift(Float8{E})
exponent_mask(::Type{Float8{E}}) where {E} = exponent_mask_low(Float8{E}) << exponent_shift(Float8{E})

sign_unmask(::Type{Float8{E}}) where {E} = ~sign_mask(Float8{E})
exponent_unmask(::Type{Float8{E}}) where {E} = ~exponent_mask(Float8{E})
significand_unmask(::Type{Float8{E}}) where {E}  = ~significand_mask(Float8{E})

sign_bits(x::Float8{E}) where {E} = sign_bits(Float8{E})
exponent_bits(x::Float8{E}) where {E} = exponent_bits(Float8{E})
significand_bits(x::Float8{E}) where {E} = significand_bits(Float8{E})

sign_shift(x::Float8{E}) where {E} = sign_shift(Float8{E})
exponent_shift(x::Float8{E}) where {E} = exponent_shift(Float8{E})
significand_shift(x::Float8{E}) where {E} = significand_shift(Float8{E})

sign_mask_low(x::Float8{E}) where {E} = sign_mask_low(Float8{E})
exponent_mask_low(x::Float8{E}) where {E} = exponent_mask_low(Float8{E})
significand_mask_low(x::Float8{E}) where {E} = significand_mask_low(Float8{E})

sign_unmask_low(x::Float8{E}) where {E} = sign_unmask_low(Float8{E})
exponent_unmask_low(x::Float8{E}) where {E} = exponent_unmask_low(Float8{E})
significand_unmask_low(x::Float8{E}) where {E} = significand_unmask_low(Float8{E})

sign_mask(x::Float8{E}) where {E} = sign_mask(Float8{E})
exponent_mask(x::Float8{E}) where {E} = exponent_mask(Float8{E})
significand_mask(x::Float8{E}) where {E} = significand_mask(Float8{E})

sign_unmask(x::Float8{E}) where {E} = sign_unmask(Float8{E})
exponent_unmask(x::Float8{E}) where {E} = exponent_unmask(Float8{E})
significand_unmask(x::Float8{E}) where {E} = significand_unmask(Float8{E})

unshift_sign(x::Float8{E}) where {E} = (value(x) & sign_mask(Float8{E})) >> sign_shift(Float8{E})
unshift_exponent(x::Float8{E}) where {E} = (value(x) & exponent_mask(Float8{E})) >> exponent_shift(Float8{E})
unshift_significand(x::Float8{E}) where {E} = (value(x) & significand_mask(Float8{E})) >> signficand_shift(Float8{E})

unshift_sign(::Type{Float8{E}}, x::UInt8) where {E} = (x & sign_mask(Float8{E})) >> sign_shift(Float8{E})
unshift_exponent(::Type{Float8{E}}, x::UInt8) where {E} = (x & exponent_mask(Float8{E})) >> exponent_shift(Float8{E})
unshift_significand(::Type{Float8{E}}, x::UInt8) where {E} = (x & significand_mask(Float8{E})) >> significand_shift(Float8{E})

shift_sign(x::Float8{E}) where {E} = (value(x) & sign_mask_low(Float8{E})) << sign_shift(Float8{E})
shift_exponent(x::Float8{E}) where {E} = (value(x) & exponent_mask_low(Float8{E})) << exponent_shift(Float8{E})
shift_significand(x::Float8{E}) where {E} = (value(x) & significand_mask_low(Float8{E})) << signficand_shift(Float8{E})

shift_sign(::Type{Float8{E}}, x::UInt8) where {E} = (x & sign_mask_low(Float8{E})) << sign_shift(Float8{E})
shift_exponent(::Type{Float8{E}}, x::UInt8) where {E} = (x & exponent_mask_low(Float8{E})) << exponent_shift(Float8{E})
shift_significand(::Type{Float8{E}}, x::UInt8) where {E} = (x & significand_mask_low(Float8{E})) << significand_shift(Float8{E})

exponent_one(::Type{T})  where {E, T<:Float8{E}} = exponent_mask(T) - sign_mask(T) >> 1
exponent_half(::Type{T}) where {E, T<:Float8{E}} = (exponent_one(T) >> significand_bits(T) - 0x01) << significand_bits(T)

const NaN8    = 0b1111_1111 # 0xff
const Zero8   = 0b0000_0000 # 0x00
const Inf8    = 0b0111_1110 # 0x7e
const PosInf8 = 0b0111_1110 # 0x7e
const NegInf8 = 0b1111_1110 # 0xfe

for I in (0, 1, 2, 3, 4, 5, 6, 7)
    E = Symbol(:E,I)
    for Const8 in (:NaN8, :Zero8, :Inf8, :PosInf8, :NegInf8)
        C8E = Symbol(Const8, E)
        @eval $C8E = Float8{$I}($Const8)
    end; 
end;

iszero(x::Float8{E}) = value(x) === Zero8
isnan(x::Float8{E}) = value(x) === NaN8
isinf(x::Float8{E}) = (value(x) & sign_unmask(Float8{E})) === Inf8
isreal(x::Float8{E}) = !isnan(x)
isfinite(x::Float8{E}) = !isinf(x) && !isnan(x)
isinfnan(x::Float8{E}) = isinf(x) || isnan(x)

signbit(x::Float8{E}) where {E} = (value(x) & sign_mask(Float8{E})) === sign_mask(Float8{E})

isnegative(x::Float8{E}) where {E} = signbit(x) && !isnan(x)
isnonnegative(x::Float8{E}) where {E} = !signbit(x) && !isnan(x)
ispositive(x::Float8{E}) where {E} = (!signbit(x) && !iszero(x)) && !isnan(x)
isnonpositive(x::Float8{E}) where {E} = (signbit(x) || iszero(x)) && !isnan(x)

function issubnormal(x::T) where {E, T<:Float8{E}}
    y = reinterpret(Unsigned, x)
    (iszero(y & exponent_mask(T)) & !iszero(y & significand_mask(T))
end
       
isnormal(x::Float8{E}) where {E} = !issubnormal(x) && !isinfnan(x)

Base.isone
Base.nextfloat
Base.prevfloat
Base.isinteger



Base
