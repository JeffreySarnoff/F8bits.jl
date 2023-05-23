# floating point exponent bias
bias_sym(exp_bits) = Int8( 2^(exp_bits - 1) )
bias_754(exp_bits) = Int8( bias_sym(exp_bits) - 1 )

# floating point exponent unbiased
unbias_sym(exp_bits, raw_exp) = raw_exp - bias_sym(exp_bits)
unbias_754(exp_bits, raw_exp) = raw_exp - bias_754(exp_bits)

# floating point exponent scaled
scale_sym(exp_bits, raw_exp) = (2.0f0)^unbias_sym(exp_bits, raw_exp)
scale_754(exp_bits, raw_exp) = (2.0f0)^unbias_754(exp_bits, raw_exp)
