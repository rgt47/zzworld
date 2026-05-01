#' MMSE WORLD Backwards Scoring Functions
#'
#' R port of the ADCS EDC mmsecalc2.pm Perl module.
#' Original creation date: 2014-06-24
#'
#' @name mmse_calc

# Constants
.MISSING <- -1L
.NA_VAL <- -4L

.REQUIRED_FIELDS <- c(
  "MMDATE", "MMYEAR", "MMMONTH", "MMDAY", "MMSEASON", "MMHOSPIT", "MMFLOOR",
  "MMCITY", "MMAREA", "MMSTATE", "MMBALL", "MMFLAG", "MMTREE", "MMDLTR",
  "MMLLTR", "MMRLTR", "MMOLTR", "MMWLTR", "MM6LTR", "MM7LTR", "MMBALLDL",
  "MMFLAGDL", "MMTREEDL", "MMWATCH", "MMPENCIL", "MMREPEAT", "MMHAND",
  "MMFOLD", "MMONFLR", "MMREAD", "MMWRITE", "MMDRAW"
)

.REQUIRED_ATTENTION_FIELDS <- c(
  "MMDLTR", "MMLLTR", "MMRLTR", "MMOLTR", "MMWLTR", "MM6LTR", "MM7LTR"
)


#' Calculate Levenshtein (Edit) Distance
#'
#' Computes the Levenshtein distance between two strings using dynamic
#' programming.
#'
#' @param s1 Character string (correct word, e.g., "DLROW")
#' @param s2 Character string (input word from participant)
#'
#' @return Integer edit distance
#'
#' @examples
#' levenshtein_distance("DLROW", "DLROW")
#' levenshtein_distance("DLROW", "DRLOW")
#' levenshtein_distance("DLROW", "WORLD")
#'
#' @export
levenshtein_distance <- function(s1, s2) {
  t1 <- strsplit(s1, "")[[1]]
  t2 <- strsplit(s2, "")[[1]]
  n1 <- length(t1)
  n2 <- length(t2)
  if (n1 == 0L) return(n2)
  if (n2 == 0L) return(n1)

  d <- matrix(0L, nrow = n1 + 1, ncol = n2 + 1)
  d[, 1] <- 0:n1
  d[1, ] <- 0:n2

  for (i in 2:(n1 + 1)) {
    for (j in 2:(n2 + 1)) {
      d[i, j] <- d[i - 1, j] + 1L
      if (d[i, j - 1] + 1L < d[i, j]) {
        d[i, j] <- d[i, j - 1] + 1L
      }
      if (t1[i - 1] == t2[j - 1]) {
        if (d[i - 1, j - 1] < d[i, j]) {
          d[i, j] <- d[i - 1, j - 1]
        }
      } else {
        if (d[i - 1, j - 1] + 1L < d[i, j]) {
          d[i, j] <- d[i - 1, j - 1] + 1L
        }
      }
    }
  }

  d[n1 + 1, n2 + 1]
}


#' Calculate MMSE Attention Score (WORLD Backwards)
#'
#' Computes the attention sub-score for the MMSE WORLD backwards task.
#' Score is 5 minus the Levenshtein distance, with a floor of 0.
#'
#' @param correct_word Character string of the correct answer
#'   ("DLROW" for English, "ODNUM" for Spanish)
#' @param input_word Character string of the participant's response
#'
#' @return Integer score from 0 to 5
#'
#' @examples
#' calc_mmse_attention("DLROW", "DLROW")
#' calc_mmse_attention("DLROW", "DRLOW")
#' calc_mmse_attention("DLROW", "DLR")
#'
#' @export
calc_mmse_attention <- function(correct_word, input_word) {
  score <- 5L - levenshtein_distance(correct_word, input_word)
  max(0L, score)
}


#' Score WORLD Backwards Response
#'
#' Convenience wrapper for scoring English WORLD backwards responses.
#'
#' @param response Character string of participant's response
#'
#' @return Integer score from 0 to 5
#'
#' @examples
#' score_world_backwards("DLROW")
#' score_world_backwards("DRLOW")
#' score_world_backwards("DLORW")
#'
#' @export
score_world_backwards <- function(response) {
  response <- toupper(gsub("[^A-Za-z]", "", response))
  calc_mmse_attention("DLROW", response)
}


#' Score MUNDO Backwards Response (Spanish)
#'
#' Convenience wrapper for scoring Spanish MUNDO backwards responses.
#'
#' @param response Character string of participant's response
#'
#' @return Integer score from 0 to 5
#'
#' @examples
#' score_mundo_backwards("ODNUM")
#' score_mundo_backwards("ODMUN")
#'
#' @export
score_mundo_backwards <- function(response) {

  response <- toupper(gsub("[^A-Za-z]", "", response))
  calc_mmse_attention("ODNUM", response)
}


#' Calculate Full MMSE Score
#'
#' Computes the total MMSE score including the attention sub-score.
#' This is a direct port of the calc_mmse2 function from the Perl module.
#'
#' @param params Named list containing MMSE field values
#' @param wordlist Integer (1-3 for English/WORLD, 4-6 for Spanish/MUNDO)
#'
#' @return List with MMSCORE (total) and MMWORLD (attention sub-score)
#'
#' @export
calc_mmse <- function(params, wordlist = 1L) {
  if (wordlist < 1 || wordlist > 6) {
    stop("Invalid wordlist value (must be 1-6)")
  }

  attention_fields <- .REQUIRED_ATTENTION_FIELDS
  is_attention_fld <- attention_fields

  no_missing_attention <- TRUE
  for (fld in attention_fields) {
    val <- params[[fld]]
    if (is.null(val) || is.na(val) || nchar(val) == 0 ||
        val == .MISSING || val == .NA_VAL) {
      no_missing_attention <- FALSE
      break
    }
  }

  no_missing <- TRUE
  for (fld in .REQUIRED_FIELDS) {
    val <- params[[fld]]
    if (is.null(val) || is.na(val) ||
        (is.character(val) && nchar(val) == 0) ||
        val == .MISSING || val == .NA_VAL) {
      no_missing <- FALSE
      break
    }
  }

  attention_score <- .MISSING
  if (no_missing_attention) {
    input <- ""
    for (fld in attention_fields) {
      val <- params[[fld]]
      if (grepl("[A-Z]", val)) {
        input <- paste0(input, val)
      }
    }

    correct_word <- if (wordlist > 3) "ODNUM" else "DLROW"
    attention_score <- calc_mmse_attention(correct_word, input)
  }

  total_score <- .MISSING
  if (no_missing) {
    total_score <- 0L
    for (fld in .REQUIRED_FIELDS) {
      if (!(fld %in% is_attention_fld)) {
        if (params[[fld]] == 1) {
          total_score <- total_score + 1L
        }
      }
    }
    total_score <- total_score + attention_score
  }

  list(MMSCORE = total_score, MMWORLD = attention_score)
}
