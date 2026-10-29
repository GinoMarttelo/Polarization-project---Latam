REASON EVALUATION IN ARGENTINA AND COLOMBIA
Analysis package for Gino | 21 October 2026 exports

QUICK START

1. Extract the entire ZIP file. Keep the folders together.
2. Use R 4.5.0 or newer. The analysis was tested with R 4.5.0 on macOS;
   the package also uses ordinary R paths and functions for Windows and Linux.
3. In RStudio, open LATAM_reason_evaluation.Rproj. This sets the working
   directory to the package folder.
4. Run this once in the R console:

   source("install_packages.R")

   An internet connection is needed to download any missing R packages.
   Missing packages go in the local r_library/ folder. The installer leaves
   packages that are already installed in place.
5. Run the analysis:

   source("run_analysis.R")

The models take several minutes. The script prints progress as it works and
ends with a comparison against the bundled reference results. New results
are saved in latam_results/. The original input files are never changed.

Without RStudio, open a terminal in the extracted package folder and run:

   Rscript install_packages.R
   Rscript run_analysis.R

No Google Drive access, Python installation, or other project folder is needed.
The R installation and package downloads are separate from this ZIP.

WHAT IS INCLUDED

latam_reason_evaluation.R
    The complete analysis: raw-data preparation, registered primary models,
    sensitivity checks, separate Argentine "all equally" and "none" analyses,
    agreement x camp follow-ups, extended attitude models, and two figures.

run_analysis.R
    Uses the bundled inputs, runs the analysis, and compares the results.

install_packages.R
    Installs missing packages from CRAN and reports the versions being used.

check_results.R
    Compares newly generated tables with reference_results/. It can also be
    run on its own after an analysis. Differences beyond the stated numerical
    tolerances are reported as failures rather than silently ignored.

data/
    Six unchanged input files:
    - The Argentina and Colombia Qualtrics CSV exports from 21 October 2026.
    - The two survey QSF files used to decode the answer labels.
    - argentina.csv and colombia.csv, the frozen reason corpora.
    These files include participant-level source data for the study team.

report/
    The integrated Word report for Gino and Anto, with the two figures also
    available as separate PNG files.

documentation/
    The verified local transcription of AsPredicted #317907, approved
    16 October 2026. This is a transcription, not an AsPredicted-issued PDF.
    The original implementation decisions and the tested R/package versions
    are included for reference.

reference_results/
    Aggregate tables from the verified analysis. New runs write to a different
    folder, so these reference tables stay available for comparison.

FILE_CHECKSUMS.csv
    File sizes and MD5 checksums for the bundle. run_analysis.R checks all six
    data files against this manifest before fitting any models.

READING THE OUTPUT

primary_results.csv contains the ten registered tests with their joint Holm
correction. Positive agreement and camp coefficients mean higher quality
ratings for same-stance and own-camp reasons, respectively.

Attitude slopes are expressed per ten-point increase in reason quality.
A negative API coefficient means less affective polarization as reason quality
increases. Higher warmth, rationality, and moral-character scores are more
favorable. The API contains warmth, so those outcomes overlap.

The 2 x 2 models and extended attitude analyses are follow-ups. Their separate
correction families are identified in the output. The Argentine "all equally"
and "none" groups are compared with their assigned target camp; they do not
enter the primary named-opponent sample.

These are interim exports. The number of qualifying cases is 811 in Argentina
and 1,197 in Colombia, below the registered target of 1,500 in each country.
The analysis does not change the registered stopping rule.

VERSION AND RUNNING NOTES

The exact versions used for the reference results are listed in documentation/.
The installer uses currently available CRAN packages; it is not a version-locked
environment. Small numerical or font differences can occur across systems.
The result check allows very small numerical differences and reports them.
Cross-platform execution has not been independently tested on Windows or Linux.

If a package needs compilation, follow its installation message about system
tools. On Linux, xml2 and the graphics packages may need system libraries.
Install packages before starting the analysis.

Run the complete file, rather than selected lines in an old R session.
Opening the .Rproj file is the easiest way to keep relative paths correct.
Re-running replaces generated files in latam_results/.

This bundle reproduces the 21 October snapshot. To analyze a later export or
deliberately change a specification, edit a separate copy and review the input
checks and reference comparison accordingly.
