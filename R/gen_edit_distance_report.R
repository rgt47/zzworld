## File-level note: R port of 1_gen_edit_distance_report.pl. Generates
## all possible letter combinations and computes edit distance scores
## for WORLD backwards (English) and MUNDO backwards (Spanish).

#' Generate All Letter Combinations with Edit Distance Scores
#'
#' Creates a data frame with all possible 7-letter combinations from the
#' valid letter set (correct letters + 'X' for wrong letter + '' for blank)
#' and computes the edit distance score for each.
#'
#' @param language Character, either "english" or "spanish"
#' @param output_file Optional file path to save CSV output
#'
#' @return Data frame with columns for each letter position, the combined
#'   word, and the edit distance score
#'
#' @examples
#' \dontrun{
#' english_combos <- gen_all_combinations("english")
#' head(english_combos)
#' }
#'
#' @export
gen_all_combinations <- function(language = "english", output_file = NULL) {
  if (language == "english") {
    letters_set <- c("D", "L", "R", "O", "W", "X", "")
    correct_word <- "DLROW"
  } else if (language == "spanish") {
    letters_set <- c("O", "D", "N", "U", "M", "X", "")
    correct_word <- "ODNUM"
  } else {
    stop("language must be 'english' or 'spanish'")
  }

  combos <- expand.grid(
    MMDLTR = letters_set,
    MMLLTR = letters_set,
    MMRLTR = letters_set,
    MMOLTR = letters_set,
    MMWLTR = letters_set,
    MM6LTR = letters_set,
    MM7LTR = letters_set,
    stringsAsFactors = FALSE
  )

  combos$letters <- apply(combos[, 1:7], 1, paste0, collapse = "")

  combos$editdist <- vapply(combos$letters, function(word) {
    calc_mmse_attention(correct_word, word)
  }, integer(1))

  if (!is.null(output_file)) {
    write.csv(combos, file = output_file, row.names = FALSE)
    message("Wrote ", nrow(combos), " rows to ", output_file)
  }

  combos
}


#' Generate Edit Distance Reports for Both Languages
#'
#' Convenience function to generate reports for both English and Spanish.
#'
#' @param output_dir Directory to save output files
#'
#' @return List with english and spanish data frames
#'
#' @examples
#' \dontrun{
#' reports <- gen_edit_distance_reports("/tmp")
#' }
#'
#' @export
gen_edit_distance_reports <- function(output_dir = tempdir()) {
  english <- gen_all_combinations(
    "english",
    file.path(output_dir, "english.csv")
  )

  spanish <- gen_all_combinations(
    "spanish",
    file.path(output_dir, "spanish.csv")
  )

  list(english = english, spanish = spanish)
}
