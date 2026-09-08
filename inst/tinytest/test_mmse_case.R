# --- regressions -------------------------------------------------

flds <- c("MMDATE", "MMYEAR", "MMMONTH", "MMDAY", "MMSEASON", "MMHOSPIT",
          "MMFLOOR", "MMCITY", "MMAREA", "MMSTATE", "MMBALL", "MMFLAG",
          "MMTREE", "MMDLTR", "MMLLTR", "MMRLTR", "MMOLTR", "MMWLTR",
          "MM6LTR", "MM7LTR", "MMBALLDL", "MMFLAGDL", "MMTREEDL",
          "MMWATCH", "MMPENCIL", "MMREPEAT", "MMHAND", "MMFOLD",
          "MMONFLR", "MMREAD", "MMWRITE", "MMDRAW")
att <- c("MMDLTR", "MMLLTR", "MMRLTR", "MMOLTR", "MMWLTR", "MM6LTR",
         "MM7LTR")
mk <- function(v7) {
  p <- stats::setNames(as.list(rep(1, length(flds))), flds)
  for (i in seq_along(att)) p[[att[i]]] <- v7[i]
  p
}

# calc_mmse() filtered attention letters with `grepl("[A-Z]", val)` on
# the raw value, which silently discarded lowercase entries. A correct
# response typed in lowercase scored MMWORLD 0 and MMSCORE 25 instead
# of 5 and 30 -- five points on a thirty-point dementia screen, and a
# different answer from score_world_backwards() on the same response.
upper <- calc_mmse(mk(c("D", "L", "R", "O", "W", "0", "0")))
lower <- calc_mmse(mk(c("d", "l", "r", "o", "w", "0", "0")))
mixed <- calc_mmse(mk(c("D", "l", "R", "o", "W", "0", "0")))
expect_equal(upper$MMWORLD, 5L, info = "uppercase DLROW scores 5")
expect_equal(lower$MMWORLD, upper$MMWORLD,
  info = "case does not change the attention sub-score")
expect_equal(mixed$MMWORLD, upper$MMWORLD,
  info = "mixed case does not change the attention sub-score")
expect_equal(lower$MMSCORE, upper$MMSCORE,
  info = "case does not change the MMSE total")

# The two entry points must agree on the same response.
expect_equal(as.numeric(lower$MMWORLD),
             as.numeric(score_world_backwards("dlrow")),
  info = "calc_mmse and score_world_backwards agree on a lowercase
          response")

# Numeric placeholder codes must still be skipped, not folded into the
# response string.
expect_equal(calc_mmse(mk(c("D", "L", "R", "O", "W", "0", "0")))$MMWORLD,
             calc_mmse(mk(c("D", "L", "R", "O", "W", "9", "9")))$MMWORLD,
  info = "numeric placeholders do not contribute letters")

# A wrong-order response still scores below a correct one.
expect_true(
  calc_mmse(mk(c("W", "O", "R", "L", "D", "0", "0")))$MMWORLD <
    upper$MMWORLD,
  info = "forward spelling scores below backward spelling")
