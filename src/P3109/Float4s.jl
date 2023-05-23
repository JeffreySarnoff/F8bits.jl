abstract type AbstractFloat4 <: AbstractFloat end

#             E is the number of bits assigned to the exponent
struct Float4{E} <: AbstractFloat4
    x::UInt8

    Float4{E}(x::UInt8) where {E} = new(x & 0x0F)
end

value(x::Float4{E}) where {E} = x.x

Float4{E}(x::Float4{E}) where {E} = x

Float4{E}(x::Int8) where {E}  = Float8{E}(reinterpret(UInt8, x))

Base.UInt8(x::T) where {E, T<:Float4{E}} = value(x)
Base.Int8(x::T) where {E, T<:Float4{E}} = reinterpret(Int8, value(x))

Base.exponent_bits(::Type{T}) where {E, T<:Float4{E}} = UInt8(E)
Base.significand_bits(::Type{T}) where {E, T<:Float4{E}} = 0x03 - UInt8(E)
Base.exponent_bits(x::T) where {E, T<:Float4{E}} = exponent_bits(T)
Base.significand_bits(x::T) where {E, T<:Float4{E}} = significand_bits(T)

