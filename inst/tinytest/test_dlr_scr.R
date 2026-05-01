library(tinytest)

# DLROW (perfect) is unambiguous across all scoring rules.
expect_equal(dlr_scr('DLROW'), 5)

# DLORW: Folstein 1975 scored this 3 (a position-based heuristic).
# Tancredi-Sellers-Ulam scores it 4: one transposition costs one Ulam
# deduction, no other errors. The disagreement is a worked example of
# why the paper proposes a principled algorithm in the first place.
expect_equal(dlr_scr('DLORW'), 4)

# Case insensitivity and the 0->O / 1->L digit translation from SAS.
expect_equal(dlr_scr('dlrow'), 5)
expect_equal(dlr_scr('DLR0W'), 5)   # zero -> O
expect_equal(dlr_scr('D1ROW'), 5)   # one  -> L

# Missing letters: 5 - dlrmiss when no other errors.
expect_equal(dlr_scr('DLRO'),  4)   # one missing (W)
expect_equal(dlr_scr('DLR'),   3)   # two missing
expect_equal(dlr_scr('D'),     1)
expect_equal(dlr_scr(''),      0)   # blank -> 0 per Rush rules

# Bad characters cost a deduction each. XYZOW: 3 bad chars + 3 missing
# letters (D, L, R) sum to 6 deductions, capped at 5 -> score 0.
expect_equal(dlr_scr('XYZOW'),  0)
expect_equal(dlr_scr('DLROWX'), 4)  # one extra bad char

# Duplicates of good letters are penalized.
expect_equal(dlr_scr('DDLROW'), 4)

# Special codes from Rush convention: '8' refused, '9' don't-know.
expect_equal(dlr_scr('8'),  8)
expect_equal(dlr_scr('9'),  0)
expect_equal(dlr_scr('  '), 0)
expect_true(is.na(dlr_scr(NA)))

# Spanish target.
expect_equal(dlr_scr('ODNUM', target = 'MUNDO'), 5)

# Vectorized wrapper preserves length and NA handling.
out <- dlr_scr_vec(c('DLROW', 'DLORW', 'XYZ', NA))
expect_equal(length(out), 4L)
expect_equal(out[1], 5)
expect_equal(out[2], 4)   # see DLORW comment above
expect_true(is.na(out[4]))
