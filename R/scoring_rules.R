#' Examiner-Assigned Score (Rule A)
#'
#' Returns the sum of per-position correctness flags as recorded by the
#' examiner in the MMSE eCRF. Different ADCS / ADNI cohorts use
#' different encodings: ADNI, ADNIGO, ADNI2, HBA store correctness as
#' 1=Correct, 2=Incorrect, while RI, IGIV, NGF store it as 1=Correct,
#' 0=Incorrect. The encoding must be supplied.
#'
#' @param scores Numeric vector of length 5: the per-position scores
#'   `c(MMD, MML, MMR, MMO, MMW)` for one observation.
#' @param encoding Either `'1/2'` (ADNI/ADNIGO/ADNI2/HBA convention) or
#'   `'1/0'` (RI/IGIV/NGF convention).
#'
#' @return Numeric scalar 0-5, or `NA` if any value is outside the
#'   allowed codes.
#'
#' @examples
#' score_examiner(c(1, 1, 1, 1, 1), encoding = '1/2')   # 5
#' score_examiner(c(1, 1, 2, 2, 1), encoding = '1/2')   # 3
#' score_examiner(c(1, 1, 0, 0, 1), encoding = '1/0')   # 3
#'
#' @export
score_examiner <- function(scores, encoding = c('1/2', '1/0')) {
  encoding <- match.arg(encoding)
  if (length(scores) != 5L)
    stop('score_examiner() expects a length-5 vector')
  scores <- suppressWarnings(as.numeric(scores))
  allowed <- if (encoding == '1/2') c(1, 2) else c(0, 1)
  if (any(is.na(scores)) || any(!(scores %in% allowed))) return(NA_real_)
  sum(scores == 1)
}


#' Position-Based Score (Rule B, Folstein heuristic)
#'
#' One interpretation of Folstein's 1975 instruction "the number of
#' letters in correct order" reads as positional correctness: the score
#' is the count of positions at which the response letter matches the
#' target letter. Under this interpretation, `dlorw` scores 3 (D, L, W
#' at correct positions; O and R transposed). This matches the worked
#' example in the original paper. Folstein's instruction is ambiguous;
#' [score_lis()] implements the alternative (subsequence) reading.
#'
#' @param response Character scalar; the participant's verbatim
#'   response. Non-alphabetic characters are stripped, case is folded.
#' @param target Character scalar; the target backwards spelling
#'   (`'DLROW'` or `'ODNUM'`).
#'
#' @return Numeric scalar 0-5.
#'
#' @examples
#' score_position('DLROW')   # 5
#' score_position('DLORW')   # 3 — Folstein 1975 worked example
#' score_position('XLROW')   # 4
#'
#' @export
score_position <- function(response, target = 'DLROW') {
  if (is.na(response)) return(NA_real_)
  s <- toupper(gsub('[^A-Za-z]', '', as.character(response)))
  t_chars <- strsplit(toupper(target), '')[[1]]
  n <- length(t_chars)
  s_chars <- strsplit(s, '')[[1]]
  if (length(s_chars) < n)
    s_chars <- c(s_chars, rep('', n - length(s_chars)))
  s_chars <- s_chars[seq_len(n)]
  sum(s_chars == t_chars)
}


#' Longest-Correctly-Ordered Subsequence Score (Rule C)
#'
#' Implements the scoring rule shared by Beckett et al. (1997) and the
#' Molloy & Standish (1997) SMMSE "line method": the score is the
#' longest subsequence of the response whose letters appear in the
#' same order as in the target. Only target letters are considered;
#' duplicates are counted at first occurrence; non-target letters do
#' not penalize but also do not contribute. Equivalent to the longest
#' increasing subsequence (LIS) of the target-position values of the
#' response letters.
#'
#' @param response Character scalar; the participant's verbatim
#'   response. Non-alphabetic characters are stripped, case is folded.
#' @param target Character scalar; the target backwards spelling
#'   (`'DLROW'` or `'ODNUM'`).
#'
#' @return Numeric scalar 0-5.
#'
#' @references
#' Beckett LA, Wilson RS, Bennett DA, Morris MC (1997). Brief report on
#' WORLD scoring. *Neurology*.
#'
#' Molloy DW, Standish TIM (1997). A guide to the standardized
#' Mini-Mental State Examination. *International Psychogeriatrics*.
#'
#' @examples
#' score_lis('DLROW')   # 5
#' score_lis('DLORW')   # 4 — one transposition; LIS = D,L,R,W or D,L,O,W
#' score_lis('WORLD')   # 1 — only W is in correct relative position
#'
#' @export
score_lis <- function(response, target = 'DLROW') {
  if (is.na(response)) return(NA_real_)
  s <- toupper(gsub('[^A-Za-z]', '', as.character(response)))
  t_chars <- strsplit(toupper(target), '')[[1]]
  t_pos <- setNames(seq_along(t_chars), t_chars)
  s_chars <- strsplit(s, '')[[1]]
  good <- s_chars[s_chars %in% names(t_pos)]
  good <- good[!duplicated(good)]
  if (length(good) == 0L) return(0)
  vals <- unname(t_pos[good])
  n <- length(vals)
  dp <- rep(1L, n)
  if (n > 1L) {
    for (i in 2:n) for (j in 1:(i - 1L))
      if (vals[j] < vals[i] && dp[j] + 1L > dp[i])
        dp[i] <- dp[j] + 1L
  }
  max(dp)
}
