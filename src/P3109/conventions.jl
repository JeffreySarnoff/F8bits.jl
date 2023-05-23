export NaN8E3, NaN8E4, NaN8E5,
       Zero8E3, Zero8E4, Zero8E5,
       Inf8E3, Inf8E4, Inf8E5,
       ispositive, isnonnegative, isnonpositive, isnegative,
       isinfnan, isposinf, isneginf, isnormal,
       isnoninteger, isfractional,  # isnoninteger(1.5) && !isfractional(0.5), isnoninteger(0.5) && isfractional(0.5)

import Base: reinterpret, 
             exponent_bits, significand_bits,
             sign_mask, exponent_mask, significand_mask,
             exponent_bias, exponent_one, exponent_half,
             UInt8, Int8,
             zero, one, iszero, isone,
             isnan, isinf, isreal, isfinite, isinteger, issubnormal,
             nextfloat, prevfloat

import Base: ==, !=, <, <=, >=, >,
             +, -, *, /,
             abs

abstract type AbstractFloat8 <: AbstractFloat end

#             E is the number of bits assigned to the exponent
struct Float8{E} <: AbstractFloat8
    x::UInt8
end

Float8{E}(x::Int8) = Float8{E}(reinterpret(UInt8, x))

value(float8::Float8{E}) = float8.x

exponent_bits(::Type{T}) where {E, T<:Float8{E}} = UInt8(E)
significand_bits(::Type{T}) where {E, T<:Float8{E}} = 0x07 - UInt8(E)
exponent_bits(float8::T) where {E, T<:Float8{E}} = exponent_bits(T)
significand_bits(float8::T) where {E, T<:Float8{E}} = significand_bits(T)

UInt8(x::T) where {E, T<:Float8{E}} = value(x)
Int8(x::T) where {E, T<:Float8{E}} = reinterpret(Int8, value(x))
  
# following float.jl loosely

reinterpret(::Type{Unsigned},  ::Type{T}) where {E, T<:Float8{E}} = UInt8
reinterpret(::Type{Signed},    ::Type{T}) where {E, T<:Float8{E}} = Int8

reinterpret(::Type{Unsigned},  x::T) where {E, T<:Float8{E}} = value(x)
reinterpret(::Type{Signed},    x::T) where {E, T<:Float8{E}} = reinterpret(Int8, value(x))
reinterpret(::Type{Float8{E}}, x::UInt8)     where {E} = Float8{E}(x)
reinterpret(::Type{Float8{E}}, x::Int8)      where {E} = Float8{E}(reinterpret(UInt8, x))

sign_bits(::Type{T}) where {E, T<:Float8{E}} = 0x01
exponent_bits(::Type{T}) where {E, T<:Float8{E}} = UInt8(E)
significand_bits(::Type{T}) where {E, T<:Float8{E}} = 0x07 - UInt8(E)

sign_shift(::Type{T}) where {E, T<:Float8{E}} = 0x07
exponent_shift(::Type{T}) where {E, T<:Float8{E}} = significand_bits(Float8{E})
signficand_shift(::Type{T}) where {E, T<:Float8{E}} = 0x00

sign_mask_low(::Type{T}) where {E, T<:Float8{E}} = 0x01
exponent_mask_low(::Type{T}) where {E, T<:Float8{E}} = (0x01 << Base.exponent_bits(Float8{E})) - 0x01
significand_mask_low(::Type{T}) where {E, T<:Float8{E}}  = (0x01 << Base.significand_bits(Float8{E})) - 0x01

sign_unmask_low(::Type{T}) where {E, T<:Float8{E}} = ~sign_mask_low(Float8{E})
exponent_unmask_low(::Type{T}) where {E, T<:Float8{E}} = ~exponent_mask_low(Float8{E})
significand_unmask_low(::Type{T}) where {E, T<:Float8{E}}  = ~significand_mask_low(Float8{E})

sign_mask(::Type{T}) where {E, T<:Float8{E}} = sign_mask_low(Float8{E}) << sign_shift(Float8{E})
significand_mask(::Type{T}) where {E, T<:Float8{E}}  = significand_mask_low(Float8{E}) << significand_shift(Float8{E})
exponent_mask(::Type{T}) where {E, T<:Float8{E}} = exponent_mask_low(Float8{E}) << exponent_shift(Float8{E})

sign_unmask(::Type{T}) where {E, T<:Float8{E}} = ~sign_mask(Float8{E})
exponent_unmask(::Type{T}) where {E, T<:Float8{E}} = ~exponent_mask(Float8{E})
significand_unmask(::Type{T}) where {E, T<:Float8{E}}  = ~significand_mask(Float8{E})

sign_bits(x::T) where {E, T<:Float8{E}} = sign_bits(Float8{E})
exponent_bits(x::T) where {E, T<:Float8{E}} = exponent_bits(Float8{E})
significand_bits(x::T) where {E, T<:Float8{E}} = significand_bits(Float8{E})

sign_shift(x::T) where {E, T<:Float8{E}} = sign_shift(Float8{E})
exponent_shift(x::T) where {E, T<:Float8{E}} = exponent_shift(Float8{E})
significand_shift(x::T) where {E, T<:Float8{E}} = significand_shift(Float8{E})

sign_mask_low(x::T) where {E, T<:Float8{E}} = sign_mask_low(Float8{E}) & value(x)
exponent_mask_low(x::T) where {E, T<:Float8{E}} = exponent_mask_low(Float8{E}) & value(x)
significand_mask_low(x::T) where {E, T<:Float8{E}} = significand_mask_low(Float8{E}) & value(x)

sign_unmask_low(x::T) where {E, T<:Float8{E}} = sign_unmask_low(Float8{E}) & value(x)
exponent_unmask_low(x::T) where {E, T<:Float8{E}} = exponent_unmask_low(Float8{E}) & value(x)
significand_unmask_low(x::T) where {E, T<:Float8{E}} = significand_unmask_low(Float8{E}) & value(x)

sign_mask(x::T) where {E, T<:Float8{E}} = sign_mask(Float8{E}) & value(x)
exponent_mask(x::T) where {E, T<:Float8{E}} = exponent_mask(Float8{E}) & value(x)
significand_mask(x::T) where {E, T<:Float8{E}} = significand_mask(Float8{E}) & value(x)

sign_unmask(x::T) where {E, T<:Float8{E}} = sign_unmask(Float8{E}) & value(x)
exponent_unmask(x::T) where {E, T<:Float8{E}} = exponent_unmask(Float8{E}) & value(x)
significand_unmask(x::T) where {E, T<:Float8{E}} = significand_unmask(Float8{E}) & value(x)

unshift_sign(x::T) where {E, T<:Float8{E}} = (value(x) & sign_mask(Float8{E})) >> sign_shift(Float8{E})
unshift_exponent(x::T) where {E, T<:Float8{E}} = (value(x) & exponent_mask(Float8{E})) >> exponent_shift(Float8{E})
unshift_significand(x::T) where {E, T<:Float8{E}} = (value(x) & significand_mask(Float8{E})) >> signficand_shift(Float8{E})

unshift_sign(::Type{Float8{E}}, x::UInt8) where {E} = (x & sign_mask(Float8{E})) >> sign_shift(Float8{E})
unshift_exponent(::Type{Float8{E}}, x::UInt8) where {E} = (x & exponent_mask(Float8{E})) >> exponent_shift(Float8{E})
unshift_significand(::Type{Float8{E}}, x::UInt8) where {E} = (x & significand_mask(Float8{E})) >> significand_shift(Float8{E})

shift_sign(x::T) where {E, T<:Float8{E}} = (value(x) & sign_mask_low(Float8{E})) << sign_shift(Float8{E})
shift_exponent(x::T) where {E, T<:Float8{E}} = (value(x) & exponent_mask_low(Float8{E})) << exponent_shift(Float8{E})
shift_significand(x::T) where {E, T<:Float8{E}} = (value(x) & significand_mask_low(Float8{E})) << signficand_shift(Float8{E})

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

iszero(x::T) where {E, T<:Float8{E}} = value(x) === Zero8
isnan(x::T) where {E, T<:Float8{E}} = value(x) === NaN8
isinf(x::T) where {E, T<:Float8{E}} = (value(x) & sign_unmask(Float8{E})) === Inf8
isposinf(x::T) where {E, T<:Float8{E}} = value(x) === Inf8
isneginf(x::T) where {E, T<:Float8{E}} = value(x) === NegInf8
isreal(x::T) where {E, T<:Float8{E}} = !isnan(x)
isfinite(x::T) where {E, T<:Float8{E}} = !isinf(x) && !isnan(x)
isinfnan(x::T) where {E, T<:Float8{E}} = isinf(x) || isnan(x)

signbit(x::T) where {E, T<:Float8{E}} = (value(x) & sign_mask(Float8{E})) === sign_mask(Float8{E})

isnegative(x::T) where {E, T<:Float8{E}} = signbit(x) && !isnan(x)
isnonnegative(x::T) where {E, T<:Float8{E}} = !signbit(x) && !isnan(x)
ispositive(x::T) where {E, T<:Float8{E}} = (!signbit(x) && !iszero(x)) && !isnan(x)
isnonpositive(x::T) where {E, T<:Float8{E}} = (signbit(x) || iszero(x)) && !isnan(x)

function issubnormal(float8::T) where {E, T<:Float8{E}}
    x = value(float8)
    (iszero(x & exponent_mask(T)) & !iszero(x & significand_mask(T))
end

isnormal(x::T) where {E, T<:Float8{E}} = !issubnormal(x) && !isinfnan(x)

function isinteger(x::T) where {E, T<:Float8{E}}
   ax = abs(x)
   (isinfnan(x) || ax < one(T)) && return false
   exponent_gte = FP_bias(E) + significand_bits(T)
   unshift_exponent(ax) >= exponent_gte
end

function notinfnan_isinteger(x::T) where {E, T<:Float8{E}}
   ax = abs(x)
   ax < one(T) && return false
   exponent_gte = FP_bias(E) + significand_bits(T)
   unshift_exponent(ax) >= exponent_gte
end

isnoninteger(x::T) where {E, T<:Float8{E}) = !isinfnan(x) && !notinfnan_isinteger(x)
isfractional(x::T) where {E, T<:Float8{E}} = !isinfnan(x) && abs(x) < one(T) && !iszero(x)

#=              
Base.isone
=#

function abs(x::T) where {E, T<:Float8{E}}
    (isnan(x) || isnonnegative(x)) && return x
    T(sign_unmask(x))
end
                     
function nextfloat(x::T) where {E, T<:Float8{E}}
    (isposinf(x) || isnan(x)) && return x
    isnonnegative(x) && return T(value(x) + 0x01)
    return T( ((value(x) & sign_unmask(x)) - 0x01) | sign_mask(T) )
end

function nextfloat(x::T, n::Int) where {E, T<:Float8{E}}
    n < 0 && return prevfloat(x, -n)
    for i in 1:n
        x = nextfloat(x)
    end
    x
end
              
function prevfloat(x::T) where {E, T<:Float8{E}}
    (isneginf(x) || isnan(x)) && return x
    ispositive(x) && return T(value(x) - 0x01)
    iszero(x) && return T(0x81)
    return T( ((value(x) & sign_unmask(x)) + 0x01) | sign_mask(T) )
end
       
function prevfloat(x::T, n::Int) where {E, T<:Float8{E}}
    n < 0 && return nextfloat(x, -n)
    for i in 1:n
        x = prevfloat(x)
    end
    x
end
       
FP8_bias(exponent_bits) = 2^(exponent_bits - 1)

FP8_exponent(exponent_bits, exponent) = exponent > 0 ? 2.0^(exponent - FP8_bias(exponent_bits)) :
                                                       2.0^(exponent - FP8_bias(exponent_bits)-1)

FP8_exponent(exponent_bits, exponent) = 2.0^(exponent - FP8_bias(exponent_bits))
              
FP8_significance(significand_bits, exponent, significand) = exponent > 0 ? 1 + significand / 2^significand_bits : 
                                                                           (significand==3 ? 4 : 
                                                                           (significand==2 ? 2 : 
                                                                           (significand == 1 ? 1  :
                                                                            0))) / 2^significand_bits

FP8_value(exponent_bits, exponent, significand) = FP8_significance(7-exponent_bits, exponent, significand) * 
                                                  FP8_exponent(exponent_bits, exponent)
