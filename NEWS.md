# zzworld 0.2.0

## Correctness fixes

* **`calc_mmse()` discarded lowercase responses, costing five points on
  the MMSE total.** The attention sub-score assembles the participant's
  letters by testing each field with `grepl("[A-Z]", val)`, which is
  meant to separate a letter from the numeric placeholder codes the
  other fields carry. Applied to the raw value it also dropped every
  lowercase letter, so a correct response entered as `d, l, r, o, w`
  produced an empty input string and scored `MMWORLD = 0`,
  `MMSCORE = 25`, against 5 and 30 for the same response in uppercase.
  Mixed case scored 3. On a thirty-point dementia screen where 24 and
  below is a common impairment threshold, that is the difference
  between a normal and an impaired result. The value is now folded to
  upper case before the test, which also makes `calc_mmse()` agree with
  `score_world_backwards()`; the two previously scored the same
  response differently.

* **`score_world_backwards()` and `score_mundo_backwards()` raised an
  opaque error on `NA`.** The missing value reached the edit-distance
  loop and surfaced as "missing value where TRUE/FALSE needed". They
  now return `NA_real_`, matching `score_position()`, `score_lis()` and
  `dlr_scr()`, which already did.

## Removed

* `edit.dist()` and `edit.distance()` are deleted. Neither was
  exported, documented, tested, or called from anywhere in the package.
  `edit.dist()` returned an error *message* as a character string for a
  response that was not five characters, so its return type depended on
  its input; `edit.distance()` indexed out of bounds on an empty string,
  because `for (i in 2:(n1 + 1))` counts down when `n1` is 0. The
  working implementation is the exported `levenshtein_distance()`.

## Documentation

* The README listed five functions, and every one of them was wrong:
  four (`edit_distance_valiente()`, `compare_scoring_methods()`,
  `mmse_calc()`, `gen_edit_distance_report()`) do not exist under those
  names, and `edit.dist()` existed but was not exported, so no reader
  could call any of them. The usage example invoked `edit.dist()` twice
  on the same input with two different comments. Replaced with the 17
  functions the package actually exports and a worked comparison of the
  scoring rules.

* `score_lis()` documented that "duplicates are counted at first
  occurrence". The implementation keeps every occurrence and takes the
  longest increasing subsequence over all of them, which is what the
  line method intends; the sentence now says so.

## Tests

* `test_basic.R` held a single `expect_true(TRUE)`. It now checks that
  every README-advertised function is exported, that a perfect response
  scores 5 under all four rules, that `NA` propagates uniformly, that
  `levenshtein_distance()` agrees with `stringdist` over 200 random
  pairs, and that `dlr_scr()` satisfies the Ulam identity
  `d = n - LIS` over all 120 permutations of the target letters.
* New `test_mmse_case.R` covers the case-folding defect.
* Suite grows from 50 assertions to 510.

# zzworld v0.1.0

* Initial public release.
