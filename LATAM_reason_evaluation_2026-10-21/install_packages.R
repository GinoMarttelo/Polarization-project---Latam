# Run once from the extracted package folder

if (getRversion() < "4.5.0") {
  stop("Use R 4.5.0 or newer for this analysis package.")
}
if (!file.exists("LATAM_reason_evaluation.Rproj")) {
  stop("Open the bundled .Rproj file before running this installer.")
}
dir.create("r_library", showWarnings = FALSE)
.libPaths(c(normalizePath("r_library"), .libPaths()))

required <- c(
  "data.table", "jsonlite", "xml2", "Matrix", "sandwich",
  "lme4", "lmerTest", "emmeans", "ggplot2", "patchwork", "ragg"
)
missing <- required[!vapply(
  required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(missing)) {
  install.packages(missing, lib = "r_library", repos = "https://cloud.r-project.org")
}

unavailable <- required[!vapply(
  required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(unavailable)) {
  stop("Installation is incomplete. Check the messages above for: ",
       paste(unavailable, collapse = ", "))
}

versions <- data.frame(
  package = required,
  installed_version = vapply(required, function(package) {
    as.character(packageVersion(package))
  }, character(1))
)
print(versions, row.names = FALSE)
message("Packages are available. Next run source(\"run_analysis.R\").")
