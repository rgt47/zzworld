## File-level note: R port of 2_gen_string_distance_report.R and
## 3_gen_diff_report.pl. Compares the custom edit distance
## implementation with the stringdist package and identifies
## discrepancies.

#' Compare Edit Distance with stringdist Package
#'
#' Adds stringdist scores to an existing combinations data frame and
#' computes the offset (difference) between methods.
#'
#' @param combos Data frame from gen_all_combinations()
#' @param correct_word The correct backwards spelling ("DLROW" or "ODNUM")
#'
#' @return Data frame with added stringdist and offset columns
#'
#' @examples
#' \dontrun{
#' combos <- gen_all_combinations("english")
#' compared <- add_stringdist_comparison(combos, "DLROW")
#' }
#'
#' @export
add_stringdist_comparison <- function(combos, correct_word) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop("Package 'stringdist' is required. Install with: install.packages('stringdist')")
  }

  combos$stringdist <- vapply(combos$letters, function(word) {
    sd <- stringdist::stringdist(correct_word, word, method = "lv")
    max(0L, 5L - as.integer(sd))
  }, integer(1))

  combos$offset <- combos$stringdist - combos$editdist

  combos
}


#' Generate Comparison Report
#'
#' Generates a full comparison report for a language, comparing the custom
#' Levenshtein implementation with the stringdist package.
#'
#' @param language Character, either "english" or "spanish"
#' @param output_file Optional file path to save CSV output
#'
#' @return Data frame with all columns including comparison
#'
#' @examples
#' \dontrun{
#' report <- gen_comparison_report("english", "/tmp/english_compare.csv")
#' }
#'
#' @export
gen_comparison_report <- function(language = "english", output_file = NULL) {
  correct_word <- if (language == "english") "DLROW" else "ODNUM"

  combos <- gen_all_combinations(language)
  result <- add_stringdist_comparison(combos, correct_word)

  if (!is.null(output_file)) {
    write.csv(result, file = output_file, row.names = FALSE)
    message("Wrote ", nrow(result), " rows to ", output_file)
  }

  result
}


#' Extract Discrepancies Between Scoring Methods
#'
#' Filters the comparison report to show only cases where the two
#' scoring methods disagree.
#'
#' @param comparison_df Data frame from gen_comparison_report() or
#'   add_stringdist_comparison()
#'
#' @return Data frame with only discrepant cases
#'
#' @examples
#' \dontrun{
#' report <- gen_comparison_report("english")
#' diffs <- get_discrepancies(report)
#' }
#'
#' @export
get_discrepancies <- function(comparison_df) {
  comparison_df[comparison_df$offset != 0, c("letters", "editdist",
                                              "stringdist", "offset")]
}


#' Generate Difference Report
#'
#' R port of 3_gen_diff_report.pl
#' Generates a report of discrepancies between scoring methods.
#'
#' @param input_file Path to comparison report CSV
#' @param output_file Path for output difference report
#'
#' @return Data frame with discrepancies
#'
#' @examples
#' \dontrun{
#' gen_diff_report("/tmp/english_compare.csv", "/tmp/english_diff.csv")
#' }
#'
#' @export
gen_diff_report <- function(input_file, output_file = NULL) {
  df <- read.csv(input_file, stringsAsFactors = FALSE)

  diffs <- df[df$offset != 0, c("letters", "offset")]

  if (!is.null(output_file)) {
    write.csv(diffs, file = output_file, row.names = FALSE)
    message("Wrote ", nrow(diffs), " discrepancies to ", output_file)
  }

  diffs
}


#' Run Full Comparison Pipeline
#'
#' Runs the complete comparison pipeline for both languages, generating
#' combination reports, comparison reports, and difference reports.
#'
#' @param output_dir Directory to save all output files
#'
#' @return List with summary statistics
#'
#' @examples
#' \dontrun{
#' results <- run_comparison_pipeline("/tmp/mmse_comparison")
#' }
#'
#' @export
run_comparison_pipeline <- function(output_dir = tempdir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  results <- list()

  for (lang in c("english", "spanish")) {
    message("Processing ", lang, "...")

    report <- gen_comparison_report(
      lang,
      file.path(output_dir, paste0(lang, "_compare_report.csv"))
    )

    diffs <- get_discrepancies(report)

    if (nrow(diffs) > 0) {
      write.csv(
        diffs,
        file.path(output_dir, paste0("diff_only_", lang, ".csv")),
        row.names = FALSE
      )
    }

    results[[lang]] <- list(
      total_combinations = nrow(report),
      discrepancies = nrow(diffs),
      score_distribution = table(report$editdist)
    )
  }

  message("\nSummary:")
  message("English: ", results$english$total_combinations, " combinations, ",
          results$english$discrepancies, " discrepancies")
  message("Spanish: ", results$spanish$total_combinations, " combinations, ",
          results$spanish$discrepancies, " discrepancies")

  results
}
