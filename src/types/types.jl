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

struct F8expon
    bias::Int8
    emin::Int8 # least exponent        (signed)
    emax::Int8 # greatest exponent    (signed)
    mine::Int8 # smallest raw exponent (skips reserved exponent value[s])
    maxe::Int8 # largest raw exponent  (skips reserved exponent value[s])
end
 
function F8expon(bias::Int, emin::Int, emax::Int, mine::Int, maxe::Int)
    F8expon(I8(bias), I8(emin), I8(emax), I8(mine), I8(maxe))
end
 
struct F8values
    bits::F8bits
    masks::F8masks
    expon::F8expon
end
    
