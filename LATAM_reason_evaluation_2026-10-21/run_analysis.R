# Open the .Rproj file first, or run Rscript from this folder

run_bundle <- function() {
  if (!file.exists("latam_reason_evaluation.R") || !dir.exists("data")) {
    stop("Open LATAM_reason_evaluation.Rproj, then source this file again.")
  }
  if (getRversion() < "4.5.0") {
    stop("Use R 4.5.0 or newer for this analysis package.")
  }
  previous_libraries <- .libPaths()
  on.exit(.libPaths(previous_libraries), add = TRUE)
  if (dir.exists("r_library")) {
    .libPaths(c(normalizePath("r_library"), .libPaths()))
  }
  manifest <- read.csv("FILE_CHECKSUMS.csv", stringsAsFactors = FALSE)
  inputs <- manifest[startsWith(manifest$file, "data/"), ]
  stopifnot(nrow(inputs) == 6L)
  hashes <- unname(tools::md5sum(inputs$file))
  if (anyNA(hashes) || any(hashes != inputs$md5)) {
    stop("An input file is missing or changed. Extract a fresh copy of the ZIP.")
  }

  # Use this bundle's files even if another project set LATAM path variables
  paths <- c(
    LATAM_DATA_DIR = "data", LATAM_SURVEY_DIR = "data",
    LATAM_CORPUS_DIR = "data", LATAM_OUTPUT_DIR = "latam_results"
  )
  previous <- setNames(Sys.getenv(names(paths), unset = NA_character_), names(paths))
  on.exit({
    unset <- names(previous)[is.na(previous)]
    restore <- previous[!is.na(previous)]
    if (length(unset)) {
      Sys.unsetenv(unset)
    }
    if (length(restore)) {
      do.call(Sys.setenv, as.list(restore))
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(paths))

  source("latam_reason_evaluation.R", local = new.env(), encoding = "UTF-8")
  source("check_results.R", local = new.env(), encoding = "UTF-8")
}

run_bundle()
