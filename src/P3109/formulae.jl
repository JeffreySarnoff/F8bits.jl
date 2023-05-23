# floating point exponent bias
bias_sym(exp_bits) = Int8( 2^(exp_bits - 1) )
bias_754(exp_bits) = Int8( bias_sym(exp_bits) - 1 )

# floating point exponent unbiased
unbias_sym(exp_bits, raw_exp) = raw_exp - bias_sym(exp_bits)
unbias_754(exp_bits, raw_exp) = raw_exp - bias_754(exp_bits)

# floating point exponent scaled
scale_sym(exp_bits, raw_exp) = (2.0f0)^unbias_sym(exp_bits, raw_exp)
scale_754(exp_bits, raw_exp) = (2.0f0)^unbias_754(exp_bits, raw_exp)

# floating point significand without and with implicit bit
explicit_sig(sig_bits, raw_sig) = raw_sig / 2.0f0^sig_bits
implicit_sig(sig_bits, raw_sig) = 1.0f0 + explicit_sig(sig_bits, raw_sig)

# positive normal floating point values
pos_normal_754(exp_bits, sig_bits, raw_exp, raw_sig) = implicit_sig(sig_bits, raw_sig) * scale_754(exp_bits, raw_exp)
pos_normal_sym(exp_bits, sig_bits, raw_exp, raw_sig) = implicit_sig(sig_bits, raw_sig) * scale_sym(exp_bits, raw_exp)

# positive subnormal floating point values
pos_subnormal_754(exp_bits, sig_bits, raw_exp, raw_sig) = explicit_sig(sig_bits, raw_sig) * scale_754(exp_bits, raw_exp)
pos_subnormal_sym(exp_bits, sig_bits, raw_exp, raw_sig) = explicit_sig(sig_bits, raw_sig) * scale_sym(exp_bits, raw_exp)


