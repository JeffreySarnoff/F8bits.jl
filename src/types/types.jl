"""
     F8bits

bitcounts for each field of an 8-bit float
"""
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

"""
     F8masks

bitmasks for each field of an 8-bit float
"""
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

"""
     F8expon

characteristics of an 8-bit float's exponent field
"""
struct F8expon
    bias::Int8
    emin::Int8 # least exponent        (signed)
    emax::Int8 # greatest exponent     (signed)
    mine::Int8 # smallest raw exponent (skips reserved exponent value[s])
    maxe::Int8 # largest raw exponent  (skips reserved exponent value[s])
end
 
function F8expon(bias::Int, emin::Int, emax::Int, mine::Int, maxe::Int)
    F8expon(I8(bias), I8(emin), I8(emax), I8(mine), I8(maxe))
end

function F8expon(ebits, emin, emax)
    bias = -emin
    mine = 0
    maxe = emax + bias

    F8expon(bias, emin, emax, mine, maxe)
end

function F8expon(ebits)
    bias = (2^ebits - 1) - 1
    emin = -bias
    emax = (2^ebits - 1) - bias

    F8expon(ebits, emin, emax)
end

"""
     F8special

Which special values are available to this sort of 8-bit float
"""
struct F8special
    nan::UInt8
    nan2::UInt8
    poszero::UInt8
    negzero::UInt8
    projzero::UInt8
    posinf::UInt8
    neginf::UInt8
    projinf::UInt8
    poshuge::UInt8
    neghuge::UInt8
    projhuge::UInt8
    postiny::UInt8
    negtiny::UInt8
    projtiny::UInt8
end

function F8special(;nan=0xff, nan2=0xff, poszero=0xff, negzero=0xff, projzero=0x00,
                    posinf=0x7e, neginf=0xfe, projinf=0xff,
                    poshuge=0x7d, neghuge=0xfd, projhuge=0xff,
                    postiny=0x7c, negtiny=0xfc, projtiny=0xff, )
    F8special(nan, nan2, poszero, negzero, projzero,
                         posinf, neginf, projinf,
                         poshuge, neghuge, projhuge,
                         postiny, negtiny, projtiny ) 
end          
          
"""
     F8sortal

composite fields characterizing this sort of 8-bit float
"""
struct F8sortal
    bits::F8bits
    masks::F8masks
    expon::F8expon
    special::F8special
end
    

# ---------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------- #

bits152 = F8bits(1, 5, 2);
masks152 = F8masks(bits152);
expon152 = F8expon(bits152.expbits);
special152 = F8special();
sortal152 = F8sortal(bits152, masks152, expon152, special152);
          
bits143 = F8bits(1, 4, 3);
masks143 = F8masks(bits143);
expon143 = F8expon(bits143.expbits);
special143 = F8special();
sortal143 = F8sortal(bits143, masks143, expon143, special143);



          
          
          
