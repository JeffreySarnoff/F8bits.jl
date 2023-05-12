U8(x)  = UInt8(x)
U16(x) = UInt16(X)
U32(x) = UInt32(X)

I8(x)  = Int8(x)
I16(x) = Int16(x)
I32(x) = Int32(x)

F16(x) = Float16(x)
F32(x) = Float32(x)
F64(x) = Float64(x)

U8(x::Core.UInt8) = x
U8(x::Core.UInt16) = convert(Core.UInt8, clamp(x, typemin(Core.UInt8):typemax(Core.UInt8)))
U8(x::Core.UInt32) = convert(Core.UInt8, clamp(x, typemin(Core.UInt8):typemax(Core.UInt8)))

I8(x::Core.Int8) = x
I8(x::Core.Int16) = convert(Core.Int8, clamp(x, typemin(Core.Int8):typemax(Core.Int8)))
I8(x::Core.Int32) = convert(Core.Int8, clamp(x, typemin(Core.Int8):typemax(Core.Int8)))

U16(x) = convert(Core.UInt16, clamp(x, typemin(Core.UInt16):typemax(Core.UInt16)))
U32(x) = convert(Core.UInt32, clamp(x, typemin(Core.UInt32):typemax(Core.UInt32)))

I16(x) = convert(Core.UInt16, clamp(x, typemin(Core.Int16):typemax(Core.Int16)))
I32(x) = convert(Core.UInt32, clamp(x, typemin(Core.Int32):typemax(Core.Int32)))

