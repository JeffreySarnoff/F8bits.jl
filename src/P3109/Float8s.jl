abstract type AbstractFloat8 <: AbstractFloat end

#             E is the number of bits assigned to the exponent
struct Float8{E} <: AbstractFloat8
    x::UInt8
end

value(x::Float8{E}) where {E} = x.x

Float8{E}(x::Float8{E}) where {E} = x

Float8{E}(x::UInt16) where {E} = Float8{E}((x & 0x00FF) % UInt8)
Float8{E}(x::UInt32) where {E} = Float8{E}((x & 0x0000_00FF) % UInt8)
Float8{E}(x::UInt64) where {E} = Float8{E}((x & 0x0000_0000_0000_00FF) % UInt8)

Float8{E}(x::Int8) where {E}  = Float8{E}(reinterpret(UInt8, x))
Float8{E}(x::Int16) where {E} = !signbit(x) ? Float8{E}(reinterpret(UInt16, x)) : Float8{E}((reinterpret(UInt16, -x) % UInt8) | sign_mask(Float8{E})
Float8{E}(x::Int32) where {E} = !signbit(x) ? Float8{E}(reinterpret(UInt32, x)) : Float8{E}((reinterpret(UInt32, -x) % UInt8) | sign_mask(Float8{E})
Float8{E}(x::Int64) where {E} = !signbit(x) ? Float8{E}(reinterpret(UInt64, x)) : Float8{E}((reinterpret(UInt64, -x) % UInt8) | sign_mask(Float8{E})

Base.UInt8(x::T) where {E, T<:Float8{E}} = value(x)
Base.Int8(x::T) where {E, T<:Float8{E}} = reinterpret(Int8, value(x))

Base.exponent_bits(::Type{T}) where {E, T<:Float8{E}} = UInt8(E)
Base.significand_bits(::Type{T}) where {E, T<:Float8{E}} = 0x07 - UInt8(E)
Base.exponent_bits(x::T) where {E, T<:Float8{E}} = exponent_bits(T)
Base.significand_bits(x::T) where {E, T<:Float8{E}} = significand_bits(T)
