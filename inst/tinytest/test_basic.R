# Cross-cutting invariants. This file previously held a single
# expect_true(TRUE), which asserts nothing about the package.

# Every exported name the README advertises must actually exist and be
# exported. The README previously listed five functions, four of which
# did not exist and one of which was not exported.
ex <- getNamespaceExports("zzworld")
for (f in c("score_position", "score_lis", "score_examiner", "dlr_scr",
            "dlr_scr_vec", "score_world_backwards",
            "score_mundo_backwards", "calc_mmse", "calc_mmse_attention",
            "levenshtein_distance")) {
  expect_true(f %in% ex, info = paste(f, "is exported"))
}

# A perfect response scores 5 under every rule; a hopeless one scores 0.
for (f in list(score_position, score_lis, dlr_scr,
               score_world_backwards)) {
  expect_equal(as.numeric(f("DLROW")), 5,
    info = "a perfect response scores 5")
}
expect_equal(score_position("XXXXX"), 0, info = "no match scores 0")
expect_equal(score_lis("XXXXX"), 0, info = "no match scores 0")

# NA in, NA out, uniformly. score_world_backwards() and
# score_mundo_backwards() used to raise "missing value where
# TRUE/FALSE needed" from inside the edit-distance loop.
expect_true(is.na(score_position(NA_character_)),
  info = "score_position propagates NA")
expect_true(is.na(score_lis(NA_character_)),
  info = "score_lis propagates NA")
expect_true(is.na(dlr_scr(NA_character_)),
  info = "dlr_scr propagates NA")
expect_true(is.na(score_world_backwards(NA_character_)),
  info = "score_world_backwards propagates NA")
expect_true(is.na(score_mundo_backwards(NA_character_)),
  info = "score_mundo_backwards propagates NA")

# Levenshtein distance against an independent implementation.
if (requireNamespace("stringdist", quietly = TRUE)) {
  set.seed(11)
  pool <- c("D", "L", "R", "O", "W", "X", "A")
  for (i in 1:200) {
    a <- paste(sample(pool, sample(1:7, 1), TRUE), collapse = "")
    b <- paste(sample(pool, sample(1:7, 1), TRUE), collapse = "")
    expect_equal(levenshtein_distance(a, b),
                 as.integer(stringdist::stringdist(a, b, method = "lv")),
      info = "levenshtein_distance agrees with stringdist")
  }
}

# Ulam distance underlying dlr_scr satisfies d = n - LIS. Checked over
# every permutation of the five target letters, where the other three
# deduction terms are zero and the score is exactly 5 - Ulam.
lis_len <- function(v) {
  n <- length(v)
  if (n == 0L) return(0L)
  dp <- rep(1L, n)
  if (n > 1L) {
    for (i in 2:n) for (j in 1:(i - 1L)) {
      if (v[j] < v[i] && dp[j] + 1L > dp[i]) dp[i] <- dp[j] + 1L
    }
  }
  max(dp)
}
lmap <- c(W = 5L, O = 4L, R = 3L, L = 2L, D = 1L)
perm_list <- function(v) {
  if (length(v) == 1L) return(list(v))
  do.call(c, lapply(seq_along(v), function(i)
    lapply(perm_list(v[-i]), function(p) c(v[i], p))))
}
for (p in perm_list(c("D", "L", "R", "O", "W"))) {
  vals <- unname(lmap[p])
  expect_equal(dlr_scr(paste(p, collapse = "")),
               as.numeric(lis_len(vals)),
    info = "dlr_scr equals 5 - Ulam distance on a permutation")
}
