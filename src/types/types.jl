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
    raw_emin::Int8 # smallest raw exponent (skips reserved exponent value[s])
    raw_emax::Int8 # largest raw exponent  (skips reserved exponent value[s])
end
 
function F8expon(bias::Int, emin::Int, emax::Int, raw_emin::Int, raw_emax::Int)
    F8expon(I8(bias), I8(emin), I8(emax), I8(raw_emin), I8(raw_emax))
end

raw_exponent_min(exponent_bits) = 0
raw_exponent_max(exponent_bits) = 2^exponent_bits - 1
exponent_bias(exponent_bits)    = raw_exponent_max(exponent_bits) - 1
exponent_max(exponent_bits)     = exponent_bias(exponent_bits)
exponent_min(exponent_bits)     = 1 - exponent_max(exponent_bits)

function F8expon(exponent_bits)
    bias = exponent_bias(exponent_bits)
    emin = exponent_min(exponent_bits)
    emax = exponent_max(exponent_bits)
    raw_emin = raw_exponent_min(exponent_bits)
    raw_emax = raw_exponent_max(exponent_bits)
    
    F8expon(bias, emin, emax, raw_emin, raw_emax)
end
          
function F8expon(ebits, raw_emax)
    raw_emin = 0
    emax = raw_emax >> 1
    emin = 1 - emax
    bias = emax
    raw_emin = 0
    raw_emax = emax + bias

    F8expon(bias, emin, emax, raw_emin, raw_emax)
end

function F8expon(ebits)
    bias = (2^ebits - 1) - 1
    emin = -bias
    emax = (2^ebits - 1) - bias

    F8expon(ebits, emin, emax)
end


"""
     F8signif

characteristics of an 8-bit float's significand field
"""
struct F8signif
    smin::Int8 # least significand
    smax::Int8 # greatest significand
    raw_smin::Int8 # smallest raw significand (skips reserved significand value[s])
    raw_smax::Int8 # largest raw significand  (skips reserved significand value[s])
end
 
function F8signif(smin::Int, smax::Int, raw_smin::Int, raw_smax::Int)
    F8signif(I8(smin), I8(smax), I8(raw_smin), I8(raw_smax))
end

raw_significand_min(significand_bits) = 0
raw_significand_max(significand_bits) = 2^significand_bits - 1
significand_max(significand_bits)     = raw_significand_max(significand_bits) // 2^significand_bits
significand_min(significand_bits)     = 
raw_significand_min(signficand_bits) // 2^significand_bits


function F8signif(significand_bits)
    smin = significand_min(significand_bits)
    smax = significand_max(significand_bits)
    raw_emin = raw_significand_min(significand_bits)
    raw_emax = raw_significand_max(significand_bits)
    
    F8signif(smin, smax, raw_smin, raw_smax)
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
    signif::F8signif
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
signif152 = F8signif(bits152.sigbits);
special152 = F8special();
sortal152 = F8sortal(bits152, masks152, expon152, signif152, special152);
          
bits143 = F8bits(1, 4, 3);
masks143 = F8masks(bits143);
expon143 = F8expon(bits143.expbits);
signif143 = F8signif(bits143.sigbits);
special143 = F8special();
sortal143 = F8sortal(bits143, masks143, expon143, signif143, special143);



          
          
          
