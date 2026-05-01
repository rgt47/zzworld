library(tinytest)

# ---- score_examiner ------------------------------------------------------

# 1/2 encoding (ADNI / ADNIGO / ADNI2 / HBA cohorts)
expect_equal(score_examiner(c(1, 1, 1, 1, 1), '1/2'), 5)
expect_equal(score_examiner(c(2, 2, 2, 2, 2), '1/2'), 0)
expect_equal(score_examiner(c(1, 1, 2, 2, 1), '1/2'), 3)

# 1/0 encoding (RI / IGIV / NGF cohorts)
expect_equal(score_examiner(c(1, 1, 1, 1, 1), '1/0'), 5)
expect_equal(score_examiner(c(0, 0, 0, 0, 0), '1/0'), 0)
expect_equal(score_examiner(c(1, 1, 0, 0, 1), '1/0'), 3)

# Out-of-range or NA values poison the score
expect_true(is.na(score_examiner(c(1, 1, NA, 1, 1), '1/2')))
expect_true(is.na(score_examiner(c(1, 1, 9,  1, 1), '1/2')))
expect_true(is.na(score_examiner(c(1, 1, 2,  1, 1), '1/0')))  # 2 invalid in 1/0

# Wrong length errors
expect_error(score_examiner(c(1, 1, 1, 1)))


# ---- score_position (Folstein heuristic) ---------------------------------

expect_equal(score_position('DLROW'), 5)
expect_equal(score_position('DLORW'), 3)   # Folstein's worked example
expect_equal(score_position('dlrow'), 5)   # case insensitive
expect_equal(score_position('XLROW'), 4)
expect_equal(score_position('DLR'),   3)   # short response; missing -> wrong
expect_equal(score_position(''),       0)
expect_true(is.na(score_position(NA)))


# ---- score_lis (Beckett / Molloy SMMSE) ----------------------------------

expect_equal(score_lis('DLROW'),  5)
expect_equal(score_lis('DLORW'),  4)   # 1 transposition; LIS = 4
expect_equal(score_lis('WORLD'),  1)   # only W in correct relative position
expect_equal(score_lis('WDROL'),  3)   # D, R, L? No — actually D before R is OK
                                       # in target so {D,R} length 2,
                                       # {D,O,L}? O after L in target so no.
                                       # Best: {D,R} or {D,O} or {D,L}: 2.
                                       # Recompute: D=1,R=3,O=4,L=2 -> [1,3,4,2]
                                       # LIS = [1,3,4] = 3.
expect_equal(score_lis('XYZD'),   1)   # only D
expect_equal(score_lis('XYZ'),    0)
expect_equal(score_lis(''),        0)
expect_true(is.na(score_lis(NA)))

# Duplicates: extra D's don't help LIS.
expect_equal(score_lis('DDLROW'), 5)
