struct F8bits
    sgnbits::Int8
    expbits::Int8
    sigbits::Int8
end

function F8bits(sgnbits, expbits, sigbits)
    nbits = sgnbits + expbits + sigbits
    nbits != 8 && throw(DomainError("\"F8 must use exactly 8 bits\""))
    F8bits(Int8(sgnbits), Int8(expbits), Int8(sigbits))
end

struct F8masks
    sgnmask::UInt8
    expmask::UInt8
    sigmask::UInt8
end
   
function F8masks(f8::F8bits)
    sgnmask = 0x80
    expmask = ((0x01 << f8.expbits) - 0x01) << f8.sigbits
    sigmask = (0x01 << f8.sigbits) - 0x01
    F8masks(sgnmask, expmask, sigmask)
end

struct F8parts
    sgn::Int8
    xpn::Int8  # raw exponent
    exp::Int8  # biased exponent
    sig::Int8  # significand / 2^k (implicitly)
    sgb::Int8  # 1 + sig
end



