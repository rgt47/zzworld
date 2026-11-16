library(tinytest)

# ---- B1: one `target` convention across scorers --------------------------
#
# Up to 0.2.0, dlr_scr() took the forward word ('WORLD'/'MUNDO') while
# score_position() and score_lis() took the reversed spelling
# ('DLROW'/'ODNUM'), and none validated it. A perfect Spanish response
# scored 1 under score_lis(target = 'MUNDO') and 0 under
# dlr_scr(target = 'ODNUM'); target = 'DINOSAUR' was accepted silently.

lbl <- 'B1 target convention'

expect_equal(dlr_scr('ODNUM', target = 'MUNDO'), 5,
  info = paste(lbl, ': dlr_scr forward MUNDO'))
expect_equal(dlr_scr('ODNUM', target = 'mundo'), 5,
  info = paste(lbl, ': dlr_scr target is case-insensitive'))
expect_equal(score_lis('ODNUM', target = 'MUNDO'), 5,
  info = paste(lbl, ': score_lis forward MUNDO scores a perfect response 5'))
expect_equal(score_position('ODNUM', target = 'MUNDO'), 5,
  info = paste(lbl, ': score_position forward MUNDO scores 5'))
expect_equal(score_lis('DLROW', target = 'world'), 5,
  info = paste(lbl, ': score_lis target is case-insensitive'))
expect_equal(dlr_scr_vec(c('ODNUM', 'DLROW'), target = 'MUNDO'), c(5, 0),
  info = paste(lbl, ': dlr_scr_vec forward MUNDO'))

expect_silent(score_lis('DLORW'),
  info = paste(lbl, ': score_lis default target does not warn'))
expect_silent(score_position('DLORW'),
  info = paste(lbl, ': score_position default target does not warn'))
expect_equal(score_lis('DLORW'), 4,
  info = paste(lbl, ': score_lis default is English'))
expect_equal(score_position('DLORW'), 3,
  info = paste(lbl, ': score_position default is English'))

# Invalid targets error, including when an early return (NA response,
# special code, empty vector) would otherwise skip the target.
expect_error(dlr_scr('DLROW', target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': dlr_scr rejects arbitrary target'))
expect_error(dlr_scr(NA, target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': dlr_scr validates target before NA early return'))
expect_error(dlr_scr('8', target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': dlr_scr validates target before special codes'))
expect_error(dlr_scr_vec(character(0), target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': dlr_scr_vec validates target on empty input'))
expect_error(score_lis('DLROW', target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': score_lis rejects arbitrary target'))
expect_error(score_position('DLROW', target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': score_position rejects arbitrary target'))
expect_error(score_lis(NA, target = 'DINOSAUR'), 'WORLD',
  info = paste(lbl, ': score_lis validates target before NA early return'))
expect_error(score_lis('DLROW', target = c('WORLD', 'MUNDO')), 'WORLD',
  info = paste(lbl, ': score_lis rejects a length-2 target'))
expect_error(score_lis('DLROW', target = NA_character_), 'WORLD',
  info = paste(lbl, ': score_lis rejects an NA target'))

# dlr_scr() has always used the forward word, so a reversed target is a
# caller error there, not a legacy form.
expect_error(dlr_scr('ODNUM', target = 'ODNUM'), 'reversed',
  info = paste(lbl, ': dlr_scr rejects reversed target'))
expect_error(dlr_scr_vec('ODNUM', target = 'ODNUM'), 'reversed',
  info = paste(lbl, ': dlr_scr_vec rejects reversed target'))

# Backward compatibility: the reversed form still works in the two
# functions that used it, with a deprecation warning.
expect_warning(score_lis('ODNUM', target = 'ODNUM'), 'deprecated',
  info = paste(lbl, ': score_lis reversed target warns'))
expect_warning(score_position('DLROW', target = 'DLROW'), 'deprecated',
  info = paste(lbl, ': score_position reversed target warns'))
expect_equal(suppressWarnings(score_lis('ODNUM', target = 'ODNUM')), 5,
  info = paste(lbl, ': score_lis legacy ODNUM maps to MUNDO'))
expect_equal(suppressWarnings(score_position('ODMUN', target = 'odnum')),
  score_position('ODMUN', target = 'MUNDO'),
  info = paste(lbl, ': score_position legacy odnum maps to MUNDO'))


# ---- B2: score_*_backwards() are vectorized ------------------------------
#
# Up to 0.2.0 these scored only the first element of a vector and
# returned a scalar, silently.

lbl <- 'B2 vector input'

expect_equal(score_world_backwards(c('DLROW', 'XXXXX')), c(5, 0),
  info = paste(lbl, ': score_world_backwards scores each element'))
expect_equal(score_world_backwards(c('XXXXX', 'DLROW')), c(0, 5),
  info = paste(lbl, ': score_world_backwards order preserved'))
expect_equal(score_world_backwards(c('dlrow', NA, 'DLORW')), c(5, NA, 3),
  info = paste(lbl, ': score_world_backwards NA per element'))
expect_equal(score_mundo_backwards(c('ODNUM', 'XXXXX')), c(5, 0),
  info = paste(lbl, ': score_mundo_backwards scores each element'))
expect_equal(score_mundo_backwards(c(NA, 'ODNUM')), c(NA, 5),
  info = paste(lbl, ': score_mundo_backwards leading NA'))
expect_identical(score_world_backwards(character(0)), numeric(0),
  info = paste(lbl, ': score_world_backwards empty input'))
expect_identical(score_world_backwards('DLROW'), 5,
  info = paste(lbl, ': score_world_backwards scalar returns unnamed double'))
expect_identical(score_world_backwards(NA), NA_real_,
  info = paste(lbl, ': score_world_backwards scalar NA'))
