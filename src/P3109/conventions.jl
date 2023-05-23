import Base: reinterpret, 
             exponent_bits, significand_bits,
             sign_mask, exponent_mask, significand_mask

abstract type AbstractFloat8 <: AbstractFloat end

struct Float8{Ebits} <: AbstractFloat8
    x::UInt8
end

# following float.jl loosely

reinterpret(::Type{Unsigned},  Type{Float8{E})) where {E} = UInt8
reinterpret(::Type{Unsigned},  x::Float8{E}))   where {E} = reinterpret(UInt8, x)
reinterpret(::Type{Signed},    Type{Float8{E})) where {E} = Int8
reinterpret(::Type{Signed},    x::Float8{E}))   where {E} = reinterpret(Int8, x)
reinterpret(::Type{Float8{E}), x::UInt8)        where {E} = Float8{E}(x)
reinterpret(::Type{Float8{E}), x::Int8)         where {E} = Float8{E}(reinterpret(UInt8, x))

sign_bits{::Type{Float8{E}}) where {E} = 0x01
exponent_bits{::Type{Float8{E}}) where {E} = UInt8(E)
significand_bits{::Type{Float8{E}}) where {E} = 0x07 - UInt8(E)

sign_shift(::Type{Float8{E}}) where {E} = 0x07
exponent_shift(::Type{Float8{E}}) where {E} = significand_bits(Float8{E})
signficand_shift(::Type{Float8{E}}) where {E} = 0x00

sign_mask_low(::Type{Float8{E}) where {E} = 0x01
exponent_mask_low(::Type{Float8{E}) where {E} = (0x01 << Base.exponent_bits(Float8{E})) - 0x01
significand_mask_low(::Type{Float8{E}) where {E}  = (0x01 << Base.significand_bits(Float8{E})) - 0x01

sign_mask(::Type{Float8{E}) where {E} = sign_mask_low(Float8{E}) << sign_shift
significand_mask(::Type{Float8{E}) where {E}  = significand_mask_low(Float8{E}) << significand_shift
exponent_mask(::Type{Float8{E}) where {E} = exponent_mask_low(Float8{E}) << exponent_shift

sign_bits{x::Float8{E}) where {E} = sign_bits(Float8{E})
exponent_bits{x::Float8{E}) where {E} = exponent_bits(Float8{E})
significand_bits{x::Float8{E}) where {E} = significand_bits(Float8{E})

sign_shift(x::Float8{E}) where {E} = sign_shift(Float8{E})
exponent_shift(x::Float8{E}) where {E} = exponent_shift(Float8{E})
significand_shift(x::Float8{E}}) where {E} = significand_shift(Float8{E})

sign_mask(x::Float8{E}) where {E} = sign_mask(Float8{E})
exponent_mask(x::Float8{E}) where {E} = exponent_mask(Float8{E})
signoficand_mask(x::Float8{E}}) where {E} = significand_mask(Float8{E})



Base.significand_bits{::Type{Float8{E}}) where {E} = 0x07 - Base.exponent_bits(Float8{E})
exponent_lomask(::Type{Float8{E}}) where {E} = (0x01 << Base.exponent_bits(Float8{E})) - 0x01
Base.exponent_mask(::Type{Float8{E}}) where {E} = exponent_lomask(Float8{E}) << Base.significand_bits(Float8{E})
Base.significand_mask(::Type{Float8{E}}) where {E} = (0x01 << Base.significand_bits(Float8{E})) - 0x01
Base.sign_mask(::Type{Float8{E}}) where {E} = 0b1000_0000 # 0x80

raw_exponent(x::Float8{E}) where {E} = (x & Base.exponent_mask(Float8{E})) >> Base.significand_bits(Float8{E})
raw_significand(x::Float8{E}) where {E} = (x & Base.significand_mask(Float8{E}))
raw_sign(x::Float8{E}) where {E} = (x & Base.sign_mask(Float8{E})) >> 0x07

Base.signbit(x::Float8{E}) where {E} = !iszero(x & Base.sign_mask(Float8{E}))
Base.iszero(x::Float8{E}) where {E} = (x === 0x00)
isnonnegative(x::Float8{E}) where {E} = iszero(x & Base.sign_mask(Float8{E}))
ispositive(x::Float8{E}) where {E} = iszero(x & Base.sign_mask(Float8{E})) && !iszero(x)

NaN8    = 0b1111_1111 # 0xff
Zero8   = 0b0000_0000 # 0x00
Inf8    = 0b0111_1110 # 0x7e
NegInf8 = 0b1111_1110 # 0xfe

Base
