#' Tancredi-Sellers-Ulam WORLD Backwards Score
#'
#' Faithful R port of `mmse.dlr_scr.sas` (Dan Tancredi, Rush ADRC, 1996).
#' Computes a 0-5 score for a participant's WORLD-backwards spelling
#' response using a deduction framework: the score equals 5 minus the sum
#' of four orthogonal error components — Ulam's distance of the present
#' good letters from their target permutation, count of non-DLROW
#' characters, count of duplicated good letters, and count of missing
#' good letters — floored at zero. Ulam's distance is computed via
#' Sellers' algorithm as presented in Critchlow (1985).
#'
#' Special-case codes follow the Rush convention: when the cleaned
#' response contains no letters, a raw `'8'` returns 8 (refused), a raw
#' `'9'` or blank returns 0, and any other non-letter content returns
#' `NA`.
#'
#' @param response Character scalar. The participant's verbatim response
#'   (typically the concatenation of `MMDLTR`, `MMLLTR`, `MMRLTR`,
#'   `MMOLTR`, `MMWLTR`).
#' @param target Character scalar. `'WORLD'` (default, English) or
#'   `'MUNDO'` (Spanish — for ADCS bilingual MMSE administrations,
#'   corresponds to SAS `q12span = 1`).
#'
#' @return Numeric scalar: a score of 0-5, the special code 8 (refused),
#'   or `NA` (uncodable).
#'
#' @references
#' Folstein MF, Folstein SE, McHugh PR (1975). "Mini-mental state": A
#' practical method for grading the cognitive state of patients for the
#' clinician. *Journal of Psychiatric Research*, 12(3), 189-198.
#'
#' Critchlow DE (1985). *Metric Methods for Analyzing Partially Ranked
#' Data*. Lecture Notes in Statistics 34. Springer-Verlag, pp. 141-148.
#'
#' Tancredi D (1996). `int.dlr_scr` SAS module. Rush Alzheimer's Disease
#' Center, internal CHAP submodule library.
#'
#' @examples
#' dlr_scr('DLROW')   # 5 (perfect)
#' dlr_scr('DLORW')   # 4 — note Folstein's heuristic gives 3 here; the
#'                    # Tancredi algorithm assesses one transposition
#'                    # via Ulam distance (1 deduction).
#' dlr_scr('DLRO')    # 4 (one missing)
#' dlr_scr('XYZOW')   # 0 (three bad chars + three missing letters
#'                    # exceed the 5-point ceiling)
#'
#' @export
dlr_scr <- function(response, target = 'WORLD') {
  if (length(response) != 1L)
    stop('dlr_scr() is scalar; use vapply() or purrr::map_dbl() to vectorize')
  if (is.na(response)) return(NA_real_)
  raw <- as.character(response)
  s <- toupper(raw)
  s <- chartr('01', 'OL', s)
  s <- gsub('[^A-Z]', '', s)
  dlrtotl <- nchar(s)
  if (dlrtotl == 0L) {
    if (grepl('8', raw, fixed = TRUE)) return(8)
    if (grepl('9', raw, fixed = TRUE)) return(0)
    if (grepl('^\\s*$', raw))           return(0)
    return(NA_real_)
  }
  letter_map <- if (toupper(target) == 'MUNDO')
    c(M = 5L, U = 4L, N = 3L, D = 2L, O = 1L)
  else
    c(W = 5L, O = 4L, R = 3L, L = 2L, D = 1L)
  vals <- letter_map[strsplit(s, '')[[1]]]
  good <- vals[!is.na(vals)]
  dlrltr <- tabulate(good, nbins = 5L)
  firsts <- good[!duplicated(good)]
  dlrpres <- length(firsts)
  permut <- integer(dlrpres)
  dlrmiss <- 0L
  for (i in 1:5) {
    if (dlrltr[i] == 0L) {
      dlrmiss <- dlrmiss + 1L
    } else {
      j <- match(i, firsts)
      permut[j] <- i - dlrmiss
    }
  }
  dlr_ulam <- 0
  if (dlrpres > 1L) {
    k <- permut
    l <- seq_len(dlrpres)
    for (i in seq_len(dlrpres)) {
      min1 <- i + 1L
      if (k[i] == 1L)       min1 <- i - 1L
      if (l[1] + 1L < min1) min1 <- l[1] + 1L
      for (j in 2:dlrpres) {
        min2 <- l[j] + 1L
        if (k[i] == j && l[j - 1L] < min2) min2 <- l[j - 1L]
        if (min1 + 1L < min2)               min2 <- min1 + 1L
        l[j - 1L] <- min1
        min1      <- min2
      }
      l[dlrpres] <- min1
    }
    dlr_ulam <- l[dlrpres] / 2
  }
  dlrgood <- sum(dlrltr)
  dlr_bad <- dlrtotl - dlrgood
  dlr_dup <- dlrgood - dlrpres
  dlrerr  <- dlr_ulam + dlr_bad + dlr_dup + dlrmiss
  if (dlrerr >= 5) 0 else 5 - dlrerr
}


#' Tancredi-Sellers-Ulam Scorer (Vectorized)
#'
#' Vectorized convenience wrapper around [dlr_scr()].
#'
#' @param responses Character vector of participant responses.
#' @param target See [dlr_scr()].
#'
#' @return Numeric vector of the same length as `responses`.
#'
#' @examples
#' dlr_scr_vec(c('DLROW', 'DLORW', 'XYZ', NA))
#'
#' @export
dlr_scr_vec <- function(responses, target = 'WORLD') {
  vapply(responses, dlr_scr, FUN.VALUE = numeric(1L), target = target,
         USE.NAMES = FALSE)
}
