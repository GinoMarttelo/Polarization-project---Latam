# Compare a new run with the bundled reference tables using base R

check_results <- function() {
  keys <- list(
    sample_audit = "country",
    polarization_scaling = c("country", "component"),
    sample_by_camp = c("country", "own", "profile"),
    primary_results = c("country", "test"),
    secondary_results = c("country", "model", "test"),
    sensitivity_results = c("country", "variant", "test"),
    argentina_assigned_comparison_results = c("profile", "test"),
    relationship_slopes = c("country", "outcome", "predictor"),
    adjusted_curves = c("country", "outcome", "predictor", "quality"),
    outcome_descriptives = c("country", "outcome"),
    scale_reliability = c("country", "outcome"),
    factorial_tests = c("country", "contrast"),
    factorial_cell_means = c("country", "stance", "camp"),
    factorial_cell_counts = c("country", "stance", "camp"),
    fixed_effects_2x2_checks = c("country", "contrast"),
    mixed_model_checks = c("country", "model", "contrast"),
    argentina_2x3_means = c("stance", "camp"),
    argentina_2x3_joint_tests = "model term"
  )
  compare_table <- function(stem) {
    filename <- paste0(stem, ".csv")
    reference <- read.csv(file.path("reference_results", filename),
                          check.names = FALSE, stringsAsFactors = FALSE)
    generated <- read.csv(file.path("latam_results", filename),
                          check.names = FALSE, stringsAsFactors = FALSE)
    if (!identical(names(reference), names(generated)) ||
        nrow(reference) != nrow(generated)) {
      stop("Different table structure: ", filename)
    }
    sort_rows <- function(table) {
      table[do.call(order, c(table[keys[[stem]]], list(na.last = TRUE))), , drop = FALSE]
    }
    reference <- sort_rows(reference)
    generated <- sort_rows(generated)
    largest <- 0
    for (column in names(reference)) {
      expected <- reference[[column]]
      observed <- generated[[column]]
      if (!identical(is.na(expected), is.na(observed))) {
        stop("Different missingness: ", filename, ", ", column)
      }
      keep <- !is.na(expected)
      if (is.numeric(expected)) {
        difference <- if (any(keep)) max(abs(expected[keep] - observed[keep])) else 0
        # Integer counts must match exactly; Satterthwaite df can vary slightly
        count_columns <- c(
          "n", "n_ratings", "ratings", "evaluators", "reasons", "authors",
          "exported", "real_completions", "eligible", "named_opponents",
          "qualifying", "eligible_nonmissing", "model_n"
        )
        tolerance <- if (column %in% count_columns) {
          0
        } else if (column %in% c("df", "df2")) {
          .01
        } else if (column %in% c("p", "p_holm", "p_adjusted", "p.value")) {
          1e-6
        } else {
          1e-4
        }
        if (!is.finite(difference) || difference > tolerance) {
          stop("Difference exceeds tolerance: ", filename, ", ", column,
               " (difference = ", signif(difference, 5), ").")
        }
        largest <- max(largest, difference)
      } else if (!identical(expected[keep], observed[keep])) {
        stop("Different labels or status: ", filename, ", ", column)
      }
    }
    if ("status" %in% names(generated) && any(generated$status != "ok")) {
      stop("An estimation failure was recorded in ", filename)
    }
    data.frame(file = filename, rows = nrow(reference),
               maximum_difference = largest, status = "PASS")
  }
  comparisons <- do.call(rbind, lapply(names(keys), compare_table))
  write.csv(comparisons, file.path("latam_results", "reference_comparison.csv"),
            row.names = FALSE)
  print(comparisons, row.names = FALSE)
  message("All reference tables matched within the stated tolerances.")
}

check_results()
