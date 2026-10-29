# Reason evaluation in Argentina and Colombia de antonella y gino 
# Analyses for the 21 October 2026 report
#
# Run this file in RStudio or with Rscript. Put the six input files listed below
# in a folder called data, or change the paths in the next section.
# Outputs go to a separate results folder; the source files are never changed.
#
# The primary models follow AsPredicted #317907, approved 16 October 2026.
# The 2 x 2 models and extended attitude analyses were added after inspecting
# the primary results. They have their own multiplicity corrections.
# Neither country had reached the registered target of 1,500 qualifying cases
# at this export. These analyses do not change the stopping rule.
#
# Contents
#   1. Paths and packages
#   2. Read, check, and score the data
#   3. Model helpers
#   4. Registered analyses and sensitivity checks
#   5. Agreement x camp follow-ups
#   6. Figures and output files


# 1. Paths and packages ----------------------------------------------------

# These defaults assume a single input folder. Environment variables are
# optional; they let the same script run on another computer without edits.
data_dir <- Sys.getenv("LATAM_DATA_DIR", unset = "data")
survey_dir <- Sys.getenv("LATAM_SURVEY_DIR", unset = data_dir)
corpus_dir <- Sys.getenv("LATAM_CORPUS_DIR", unset = data_dir)
output_dir <- Sys.getenv("LATAM_OUTPUT_DIR", unset = "latam_results")

input_files <- list(
  AR = list(
    responses = file.path(
      data_dir, "latam_arg_reason_evaluation_October+22+2026_15.14.csv"
    ),
    survey = file.path(survey_dir, "latam_arg_reason_evaluation (6).qsf"),
    corpus = file.path(corpus_dir, "argentina.csv")
  ),
  CO = list(
    responses = file.path(
      data_dir, "latam_col_reason_evaluation_October+21,+2026_07.20.csv"
    ),
    survey = file.path(survey_dir, "latam_col_reason_evaluation (6).qsf"),
    corpus = file.path(corpus_dir, "colombia.csv")
  )
)

# No provider-confirmed fraud list accompanied these exports. Add verified
# ResponseIds here if a provider supplies one; do not infer fraud from outcomes.
verified_fraud_ids <- list(AR = character(), CO = character())

packages <- c(
  "data.table", "jsonlite", "xml2", "Matrix", "sandwich",
  "lme4", "lmerTest", "emmeans", "ggplot2", "patchwork", "ragg"
)
missing_packages <- packages[!vapply(
  packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
if (length(missing_packages)) {
  stop("Install these packages first: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages(library(data.table))

missing_files <- unlist(input_files)[!file.exists(unlist(input_files))]
if (length(missing_files)) {
  stop("Check the input paths at the top of the script:\n",
       paste(missing_files, collapse = "\n"))
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
private_dir <- file.path(output_dir, "private")
dir.create(private_dir, showWarnings = FALSE, mode = "0700")


# 2. Read, check, and score the data ---------------------------------------

read_responses <- function(path) {
  responses <- fread(
    path, colClasses = "character", na.strings = NULL, encoding = "UTF-8"
  )
  if (anyDuplicated(names(responses)) || nrow(responses) < 3L ||
      !grepl("ImportId", responses[[1]][2], fixed = TRUE)) {
    stop("Expected a Qualtrics export with two metadata rows: ", path)
  }
  responses[-c(1, 2)]
}

plain_text <- function(text) {
  # Survey answer labels can contain HTML; the CSV contains the displayed text
  trimws(xml2::xml_text(xml2::read_html(paste0("<div>", text, "</div>"))))
}

read_codebook <- function(path) {
  survey <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  questions <- Filter(
    function(element) identical(element$Element, "SQ"), survey$SurveyElements
  )
  maps <- lapply(questions, function(element) {
    question <- element$Payload
    matrix_question <- identical(question$QuestionType, "Matrix")
    answers <- if (matrix_question) question$Answers else question$Choices
    if (!length(answers)) {
      return(integer())
    }
    labels <- vapply(answers, function(answer) {
      plain_text(answer$Display)
    }, character(1))
    codes <- vapply(names(answers), function(code) {
      recode <- if (!matrix_question) question$RecodeValues[[code]] else NULL
      as.integer(if (is.null(recode)) code else recode)
    }, integer(1))
    setNames(codes, labels)
  })
  names(maps) <- vapply(questions, function(element) {
    element$Payload$DataExportTag
  }, character(1))
  maps
}

decode_answers <- function(values, answer_map, question) {
  values <- trimws(values)
  observed <- unique(values[!is.na(values) & nzchar(values)])
  if (any(!observed %in% names(answer_map))) {
    stop("Unrecognized answer label in ", question, "; check the codebook.")
  }
  unname(answer_map[values])
}

bounded_number <- function(values, lower, upper) {
  numbers <- suppressWarnings(as.numeric(values))
  numbers[!is.finite(numbers) | numbers < lower | numbers > upper] <- NA_real_
  numbers
}

score_items <- function(responses, maps, question, items, minimum, reverse) {
  scores <- vapply(seq_len(items), function(item) {
    decode_answers(
      responses[[paste0(question, "_", item)]], maps[[question]], question
    )
  }, integer(nrow(responses)))
  available <- rowSums(!is.na(scores))
  average <- rowMeans(scores, na.rm = TRUE)
  average[available < minimum] <- NA_real_
  if (reverse) {
    average <- 7 - average
  }
  list(items = scores, available = available, score = average)
}

prepare_country <- function(country_code, files) {
  raw <- read_responses(files$responses)
  if (anyDuplicated(raw$ResponseId)) {
    stop("Repeated ResponseId in ", country_code, "; resolve before fitting.")
  }
  maps <- read_codebook(files$survey)
  corpus <- fread(files$corpus, encoding = "UTF-8")
  stopifnot(!anyDuplicated(corpus$reason_id))
  topics <- sort(unique(corpus$topic), method = "radix")
  corpus_camp <- decode_answers(corpus$author_camp, maps$camp_sort, "author camp")

  # This Colombian response is a documented staff test from the test log
  staff_test <- country_code == "CO" & raw$ResponseId == "R_3zlPEDHeVdW1ViF"
  is_test <- staff_test | raw$Status == "Survey Preview" |
    raw$DistributionChannel == "preview" | raw$review_only == "1" |
    grepl("^(TEST|PREVIEW)", raw$ticket, ignore.case = TRUE)
  completed <- raw$completion_outcome == "completed" & raw$Finished == "True"
  responses <- raw[completed & !is_test]
  if (any(responses$author_camp_displayed != "0")) {
    stop("The analysis assumes author camp was hidden; check ", country_code)
  }

  people <- data.table(
    country = country_code,
    pid = responses$ResponseId,
    own = decode_answers(responses$camp_sort, maps$camp_sort, "camp_sort"),
    target = as.integer(responses$post_other_camp),
    profile = responses$opposition_profile,
    age = bounded_number(
      decode_answers(responses$age, maps$age, "age"), 1, 6
    ),
    strength = decode_answers(
      responses$camp_strength, maps$camp_strength, "camp_strength"
    ),
    warmth = bounded_number(responses$warmth_other_1, 0, 100),
    anger = decode_answers(responses$anger, maps$anger, "anger"),
    understanding = decode_answers(
      responses$understanding, maps$understanding, "understanding"
    ),
    honesty = decode_answers(responses$honesty, maps$honesty, "honesty"),
    assistance = decode_answers(
      responses$outside_assistance, maps$outside_assistance, "outside_assistance"
    ),
    duration = bounded_number(responses[["Duration (in seconds)"]], 0, Inf) / 60
  )
  people[is.na(duration), duration := 0]
  opponent <- if (country_code == "AR") {
    decode_answers(responses$camp_opposed, maps$camp_opposed, "camp_opposed")
  } else {
    3L - people$own
  }
  camp_codes <- if (country_code == "AR") 1:4 else 1:2
  people[, named := as.integer(
    !is.na(own) & opponent %in% camp_codes & opponent != own
  )]
  people[, attention_flag := as.integer(honesty %in% c(1, 2, 4))]
  people[, uncertain_assistance := as.integer(assistance %in% c(5, 6))]

  scale_definitions <- list(
    rationality = list("rationality_other", 6L, 5L, TRUE, "rat"),
    character = list("morality_other", 6L, 5L, TRUE, "character"),
    own_character = list("morality_own", 6L, 5L, TRUE, "own_character"),
    evidence = list("evidence_other", 3L, 3L, FALSE, "evidence"),
    engagement = list("engagement_other", 3L, 3L, FALSE, "engagement")
  )
  # Character, evidence, and engagement scoring was specified at analysis time.
  # Rationality scoring follows the registration. Higher values are favorable.
  for (outcome in names(scale_definitions)) {
    definition <- scale_definitions[[outcome]]
    scored <- score_items(
      responses, maps, definition[[1]], definition[[2]],
      definition[[3]], definition[[4]]
    )
    people[, (outcome) := scored$score]
    people[, (paste0(outcome, "_items")) := scored$available]
    for (item in seq_len(definition[[2]])) {
      people[, (paste0(definition[[5]], item)) := scored$items[, item]]
    }
  }

  stances <- lapply(topics, function(topic) {
    question <- paste0("stance_", topic)
    decode_answers(responses[[question]], maps[[question]], question)
  })
  names(stances) <- topics
  rating_batches <- lapply(seq_len(20), function(position) {
    prefix <- sprintf("r%02d_", position)
    reason_id <- responses[[paste0(prefix, "id")]]
    reason_row <- match(reason_id, corpus$reason_id)
    topic <- corpus$topic[reason_row]
    stance <- vapply(seq_len(nrow(responses)), function(row) {
      if (is.na(topic[row])) NA_integer_ else stances[[topic[row]]][row]
    }, integer(1))
    author_camp <- corpus_camp[reason_row]
    exported_camp <- as.integer(responses[[paste0(prefix, "author_camp_code")]])
    known_reason <- !is.na(reason_row)
    # Use the frozen corpus as the authority for authors, topics, and positions
    metadata_fields <- c(
      id = "reason_id", topic = "topic", statement = "statement", text = "text",
      side = "semantic_side", source_side = "source_side", author_id = "author_id",
      author_camp = "author_camp"
    )
    for (export_field in names(metadata_fields)) {
      field <- metadata_fields[[export_field]]
      saved <- responses[[paste0(prefix, export_field)]]
      if (any(saved[known_reason] != corpus[[field]][reason_row[known_reason]])) {
        stop("Reason metadata disagree with the frozen corpus in ", country_code)
      }
    }
    if (any(exported_camp[known_reason] != author_camp[known_reason]) ||
        any(!stance[known_reason] %in% c(1, 2, 3, 5, 6, 7))) {
      stop("Invalid author camp or ineligible topic in ", country_code)
    }
    data.table(
      country = country_code, pid = people$pid, rid = reason_id,
      aid = corpus$author_id[reason_row], topic = topic,
      rating = bounded_number(responses[[paste0(prefix, "quality_1")]], 0, 100),
      same = as.integer(ifelse(stance > 4, "agree", "disagree") ==
                          corpus$semantic_side[reason_row]),
      opp = as.integer(author_camp == people$target),
      other = as.integer(author_camp != people$own & author_camp != people$target),
      order = position - 10.5, own = people$own, target = people$target,
      named = people$named, profile = people$profile
    )
  })
  ratings <- rbindlist(rating_batches)
  ratings <- ratings[!is.na(aid) & !is.na(rating)]
  setorder(ratings, pid, order)
  ratings <- unique(ratings, by = c("pid", "rid"))
  counts <- ratings[, .(valid_ratings = .N, invariant = as.integer(
    .N == 20L && uniqueN(rating) == 1L
  )), by = pid]
  people <- merge(people, counts, by = "pid", all.x = TRUE, sort = FALSE)
  people[is.na(valid_ratings), `:=`(valid_ratings = 0L, invariant = 0L)]
  raw_row <- match(people$pid, responses$ResponseId)
  base_eligible <- people$valid_ratings == 20L &
    responses$consent_agree[raw_row] == "Acepto participar" &
    responses$adult_resident[raw_row] == "Sí"
  people[, eligible := base_eligible & !honesty %in% 3L &
           !assistance %in% c(3L, 4L) &
           !pid %in% verified_fraud_ids[[country_code]]]
  people[, `:=`(ticket = responses$ticket[raw_row],
               end_date = responses$EndDate[raw_row])]
  setorder(people, end_date, pid)
  # Keep the earliest otherwise-eligible completion for each provider ticket
  repeated <- people[eligible & nzchar(ticket), .(pid, duplicate = duplicated(ticket))]
  people[pid %in% repeated[duplicate == TRUE, pid], eligible := FALSE]
  people <- people[eligible == TRUE]
  ratings <- ratings[pid %in% people$pid]
  setorder(people, country, pid)
  setorder(ratings, country, pid, order)

  stopifnot(
    all(people$own %in% camp_codes), all(people$target %in% camp_codes),
    all(people$target != people$own),
    all(people[named == 1, target] == opponent[match(
      people[named == 1, pid], responses$ResponseId
    )])
  )
  if (country_code == "AR") {
    stopifnot(all(people[named == 0, profile] %in% c("todos_por_igual", "ninguno")))
  }
  for (group in c("own", "opp")) {
    selected <- if (group == "own") ratings[opp == 0 & other == 0] else ratings[opp == 1]
    summaries <- selected[, .(count = .N, quality = mean(rating),
                             same_fraction = mean(same)), by = pid]
    row <- match(people$pid, summaries$pid)
    people[, (paste0("n", group)) := fifelse(
      is.na(row), 0L, summaries$count[row]
    )]
    people[, (paste0("q", group)) := summaries$quality[row]]
    people[, (paste0("same_", group)) := summaries$same_fraction[row]]
    for (topic_name in topics) {
      topic_counts <- selected[topic == topic_name, .N, by = pid]
      numerator <- topic_counts$N[match(people$pid, topic_counts$pid)]
      numerator[is.na(numerator)] <- 0L
      denominator <- people[[paste0("n", group)]]
      proportion <- numerator / denominator
      proportion[denominator == 0L] <- NA_real_
      people[, (paste0("topic_", group, "_", topic_name)) := proportion]
    }
  }

  # Fix the API scale in the eligible named-opponent sample before restricting
  # any outcome model. Reuse it for all sensitivities and assigned comparisons.
  components <- c("warmth", "anger", "understanding")
  scale_rows <- people$named == 1 & complete.cases(people[, components, with = FALSE])
  scale_sample <- people[scale_rows]
  scaling <- rbindlist(lapply(components, function(component) {
    data.table(
      country = country_code, component = component, n = nrow(scale_sample),
      mean = mean(scale_sample[[component]]), sd = sd(scale_sample[[component]]),
      sign = if (component == "anger") 1 else -1
    )
  }))
  stopifnot(all(scaling$sd > 0))
  for (row in seq_len(nrow(scaling))) {
    component <- scaling$component[row]
    people[, (paste0("z_", component)) := scaling$sign[row] *
             (get(component) - scaling$mean[row]) / scaling$sd[row]]
  }
  people[, polarization := rowMeans(.SD, na.rm = FALSE),
         .SDcols = paste0("z_", components)]
  # Lower API values mean less polarization. It is not restandardized after
  # averaging, and its warmth component overlaps the separate warmth outcome.
  qualifying_columns <- c(
    "qown", "qopp", "rationality", "warmth", "polarization", "age", "strength"
  )
  complete_primary <- complete.cases(people[, qualifying_columns, with = FALSE])
  people[, qualifying := as.integer(named == 1 & complete_primary)]
  audit <- data.table(
    country = country_code, exported = nrow(raw),
    real_completions = nrow(responses), eligible = nrow(people),
    named_opponents = sum(people$named), qualifying = sum(people$qualifying)
  )
  people[, c("ticket", "end_date", "eligible", "valid_ratings") := NULL]
  list(people = people, ratings = ratings, topics = topics,
       scaling = scaling, audit = audit)
}

message("Preparing and checking the two country datasets...")
prepared <- lapply(names(input_files), function(country_code) {
  prepare_country(country_code, input_files[[country_code]])
})
names(prepared) <- names(input_files)
people <- rbindlist(lapply(prepared, `[[`, "people"), fill = TRUE)
ratings <- rbindlist(lapply(prepared, `[[`, "ratings"))
topics_by_country <- lapply(prepared, `[[`, "topics")
setorder(people, country, pid)
setorder(ratings, country, pid, order)
fwrite(people, file.path(private_dir, "participants.csv"))
fwrite(ratings, file.path(private_dir, "ratings.csv"))
Sys.chmod(list.files(private_dir, full.names = TRUE), mode = "0600")
fwrite(rbindlist(lapply(prepared, `[[`, "audit")),
       file.path(output_dir, "sample_audit.csv"))
fwrite(rbindlist(lapply(prepared, `[[`, "scaling")),
       file.path(output_dir, "polarization_scaling.csv"))

# 3. Model helpers --------------------------------------------------------

matrix_rank <- function(design) {
  if (!ncol(design)) {
    return(0L)
  }
  norms <- sqrt(colSums(design^2))
  nonzero <- norms > 1e-8
  if (!any(nonzero)) {
    return(0L)
  }
  qr(sweep(design[, nonzero, drop = FALSE], 2, norms[nonzero], "/"),
     tol = 1e-9)$rank
}

independent_columns <- function(design) {
  # Preserve the original column order when removing redundant nuisance terms
  keep <- integer()
  current_rank <- 0L
  for (column in seq_len(ncol(design))) {
    new_rank <- matrix_rank(design[, c(keep, column), drop = FALSE])
    if (new_rank > current_rank) {
      keep <- c(keep, column)
      current_rank <- new_rank
    }
  }
  keep
}

identify_terms <- function(design, terms) {
  full_rank <- matrix_rank(design)
  setNames(vapply(terms, function(term) {
    term %in% colnames(design) && full_rank > matrix_rank(
      design[, setdiff(colnames(design), term), drop = FALSE]
    )
  }, logical(1)), terms)
}

infer_effect <- function(estimate, se, df) {
  critical <- qt(.975, df)
  data.table(
    estimate = as.numeric(estimate), se = as.numeric(se), df = df,
    ci_low = as.numeric(estimate - critical * se),
    ci_high = as.numeric(estimate + critical * se),
    p = as.numeric(2 * pt(-abs(estimate / se), df))
  )
}

absorb_effects <- function(values, evaluator, reason) {
  evaluator <- as.integer(factor(evaluator))
  reason <- as.integer(factor(reason))
  evaluator_count <- tabulate(evaluator)
  reason_count <- tabulate(reason)
  for (iteration in seq_len(1000)) {
    previous <- values
    values <- values - (rowsum(values, evaluator) / evaluator_count)[
      evaluator, , drop = FALSE
    ]
    values <- values - (rowsum(values, reason) / reason_count)[
      reason, , drop = FALSE
    ]
    change <- max(abs(values - previous))
    if (change < 1e-9) {
      return(list(values = values, iterations = iteration, change = change))
    }
  }
  stop("Fixed-effect absorption did not converge in 1,000 iterations.")
}

count_components <- function(evaluator, reason) {
  # The evaluator-reason graph determines the rank of the absorbed effects
  evaluator <- as.integer(factor(evaluator))
  reason <- as.integer(factor(reason)) + max(evaluator)
  parent <- seq_len(max(reason))
  size <- rep(1L, length(parent))
  for (row in seq_along(evaluator)) {
    left <- evaluator[row]
    right <- reason[row]
    while (parent[left] != left) {
      left <- parent[left]
    }
    while (parent[right] != right) {
      right <- parent[right]
    }
    if (left != right) {
      if (size[left] < size[right]) {
        parent[left] <- right
        size[right] <- size[right] + size[left]
      } else {
        parent[right] <- left
        size[left] <- size[left] + size[right]
      }
    }
  }
  sum(parent == seq_along(parent))
}

cluster_meat <- function(scores, group) {
  cluster_scores <- rowsum(scores, group, reorder = FALSE)
  clusters <- nrow(cluster_scores)
  if (clusters < 2L) {
    stop("Cluster inference requires at least two clusters.")
  }
  crossprod(cluster_scores) * clusters / (clusters - 1)
}

fit_rating_model <- function(data, design, focal_terms, verify = FALSE) {
  absorbed <- absorb_effects(cbind(rating = data$rating, design), data$pid, data$rid)
  outcome <- absorbed$values[, 1]
  full_design <- absorbed$values[, -1, drop = FALSE]
  identified <- identify_terms(full_design, focal_terms)
  kept <- independent_columns(full_design)
  model_design <- full_design[, kept, drop = FALSE]
  bread <- solve(crossprod(model_design))
  coefficients <- as.numeric(solve(crossprod(model_design),
                                   crossprod(model_design, outcome)))
  names(coefficients) <- colnames(model_design)
  residuals <- as.numeric(outcome - model_design %*% coefficients)
  evaluators <- uniqueN(data$pid)
  authors <- uniqueN(data$aid)
  reasons <- uniqueN(data$rid)
  components <- count_components(data$pid, data$rid)
  parameters <- ncol(model_design) + evaluators + reasons - components
  observations <- nrow(data)
  df <- min(evaluators, authors) - 1L
  if (observations <= parameters || df <= 0L) {
    stop("Insufficient residual degrees of freedom.")
  }
  scores <- model_design * residuals
  # CRV1: evaluator + author - evaluator-author intersection. The final
  # finite-parameter correction includes the rank of the absorbed effects.
  meat <- cluster_meat(scores, data$pid) + cluster_meat(scores, data$aid) -
    cluster_meat(scores, paste(data$pid, data$aid, sep = ":"))
  covariance <- (observations - 1) / (observations - parameters) *
    bread %*% meat %*% bread
  verification <- NULL
  if (verify) {
    # A full dummy-variable regression checks the absorbed coefficient estimates
    stopifnot(components == 1L)
    evaluator_dummies <- Matrix::sparseMatrix(
      i = seq_len(observations), j = as.integer(factor(data$pid)), x = 1,
      dims = c(observations, evaluators)
    )
    reason_dummies <- Matrix::sparseMatrix(
      i = seq_len(observations), j = as.integer(factor(data$rid)), x = 1,
      dims = c(observations, reasons)
    )[, -1, drop = FALSE]
    full <- cbind(evaluator_dummies, reason_dummies,
                  Matrix::Matrix(design[, kept, drop = FALSE], sparse = TRUE))
    dummy_coefficients <- Matrix::solve(
      Matrix::crossprod(full), Matrix::crossprod(full, data$rating)
    )
    error <- max(abs(tail(as.numeric(dummy_coefficients), length(kept)) - coefficients))
    stopifnot(error < 1e-7)
    verification <- list(dummy_coefficient_error = error)
  }
  list(
    coefficients = coefficients, covariance = covariance, df = df,
    identified = identified, n = evaluators, n_ratings = observations,
    diagnostics = list(
      evaluators = evaluators, authors = authors, reasons = reasons,
      parameters = parameters, components = components, df = df,
      iterations = absorbed$iterations, change = absorbed$change,
      dropped = setdiff(colnames(full_design), colnames(model_design)),
      min_variance = min(diag(covariance))
    ),
    verification = verification
  )
}

fit_post_model <- function(data, outcome, categorical_age = FALSE) {
  country_code <- unique(data$country)
  topics <- topics_by_country[[country_code]][-1]
  topic_columns <- c(paste0("topic_own_", topics), paste0("topic_opp_", topics))
  covariates <- c("qopp", "qown", "own", "age", "strength",
                  "same_own", "same_opp", topic_columns)
  data <- data[complete.cases(data[, c(outcome, covariates), with = FALSE])]
  design <- cbind(
    Intercept = 1, qopp = data$qopp, qown = data$qown,
    model.matrix(~factor(own), data)[, -1, drop = FALSE]
  )
  age_design <- if (categorical_age) {
    model.matrix(~factor(age), data)[, -1, drop = FALSE]
  } else {
    cbind(age = data$age)
  }
  design <- cbind(
    design, age_design, strength = data$strength,
    same_own = data$same_own, same_opp = data$same_opp,
    as.matrix(data[, ..topic_columns])
  )
  identified <- identify_terms(design, c("qopp", "qown"))
  original_names <- colnames(design)
  design <- design[, independent_columns(design), drop = FALSE]
  response <- data[[outcome]]
  bread <- solve(crossprod(design))
  coefficients <- as.numeric(bread %*% crossprod(design, response))
  names(coefficients) <- colnames(design)
  residuals <- as.numeric(response - design %*% coefficients)
  leverage <- rowSums((design %*% bread) * design)
  if (any(leverage >= 1 - 1e-10)) {
    stop("HC3 is undefined for a case with unit leverage.")
  }
  covariance <- bread %*% crossprod(design * (residuals / (1 - leverage))) %*% bread
  df <- nrow(design) - ncol(design)

  # Check the matrix calculation against lm() and sandwich's HC3 implementation
  check_fit <- lm(response ~ design - 1)
  check_covariance <- sandwich::vcovHC(check_fit, type = "HC3")
  coefficient_error <- max(abs(coef(check_fit) - coefficients))
  covariance_error <- max(abs(check_covariance - covariance))
  stopifnot(coefficient_error < 1e-7, covariance_error < 1e-7, df > 0)
  list(
    coefficients = coefficients, covariance = covariance, df = df,
    identified = identified, n = nrow(data), data = data, design = design,
    diagnostics = list(
      outcome = outcome, n = nrow(data), parameters = ncol(design), df = df,
      max_leverage = max(leverage),
      dropped = setdiff(original_names, colnames(design)),
      coefficient_error = coefficient_error, covariance_error = covariance_error
    )
  )
}

extract_term <- function(fit, term, multiplier = 1) {
  variance <- fit$covariance[term, term]
  if (!isTRUE(fit$identified[term]) || !is.finite(variance) || variance <= 0) {
    stop("Unidentified contrast or nonpositive focal variance: ", term)
  }
  infer_effect(fit$coefficients[term] * multiplier,
               sqrt(variance) * abs(multiplier), fit$df)
}

failed_effect <- function(error) {
  data.table(
    estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_,
    ci_high = NA_real_, p = 1, status = paste("Estimation failure:", error)
  )
}

run_rating_tests <- function(data, label, interaction = FALSE, verify = FALSE) {
  country_code <- unique(data$country)
  predictors <- c("same", "opp", if (country_code == "AR") "other", "order")
  design <- as.matrix(data[, ..predictors])
  tests <- c(same = "agreement", opp = "camp")
  if (interaction) {
    design <- cbind(design, same_opp = data$same * data$opp)
    tests <- c(tests, same_opp = "agreement_by_opponent")
    if (country_code == "AR") {
      design <- cbind(design, same_other = data$same * data$other)
      tests <- c(tests, same_other = "agreement_by_other")
    }
  }
  fit <- tryCatch(
    fit_rating_model(data, design, names(tests), verify = verify),
    error = function(error) list(error = conditionMessage(error))
  )
  results <- rbindlist(lapply(names(tests), function(term) {
    effect <- tryCatch({
      if (!is.null(fit$error)) stop(fit$error)
      estimate <- extract_term(fit, term, if (term == "opp") -1 else 1)
      estimate[, status := "ok"]
      estimate
    }, error = function(error) failed_effect(conditionMessage(error)))
    cbind(data.table(country = country_code, model = label, test = tests[[term]],
                     n = uniqueN(data$pid), n_ratings = nrow(data)), effect)
  }))
  list(results = results, diagnostics = fit$diagnostics,
       verification = fit$verification, error = fit$error)
}

run_post_test <- function(data, outcome, label, categorical_age = FALSE) {
  fit <- tryCatch(
    fit_post_model(data, outcome, categorical_age),
    error = function(error) list(error = conditionMessage(error))
  )
  effect <- tryCatch({
    if (!is.null(fit$error)) stop(fit$error)
    estimate <- extract_term(fit, "qopp", multiplier = 10)
    estimate[, status := "ok"]
    estimate
  }, error = function(error) failed_effect(conditionMessage(error)))
  results <- cbind(data.table(
    country = unique(data$country), model = label, test = outcome,
    n = if (is.null(fit$n)) NA_integer_ else fit$n, n_ratings = NA_integer_
  ), effect)
  list(results = results, diagnostics = fit$diagnostics, error = fit$error, fit = fit)
}

run_primary_family <- function(people, ratings, label, categorical_age = FALSE) {
  models <- list(rating = run_rating_tests(ratings, paste0(label, "_rating")))
  for (outcome in c("rationality", "warmth", "polarization")) {
    models[[outcome]] <- run_post_test(
      people, outcome, paste(label, outcome, sep = "_"), categorical_age
    )
  }
  list(
    results = rbindlist(lapply(models, `[[`, "results")),
    diagnostics = lapply(models, function(model) {
      list(details = model$diagnostics, error = model$error)
    })
  )
}

make_adjusted_curve <- function(fit, predictor) {
  # Hold other columns at their model-sample means. Restrict each curve to
  # the observed 5th-95th percentile of its quality predictor.
  limits <- quantile(fit$data[[predictor]], c(.05, .95), names = FALSE)
  quality <- seq(limits[1], limits[2], length.out = 101)
  new_design <- matrix(
    colMeans(fit$design), nrow = 101, ncol = ncol(fit$design), byrow = TRUE,
    dimnames = list(NULL, colnames(fit$design))
  )
  new_design[, predictor] <- quality
  adjusted_mean <- as.numeric(new_design %*% fit$coefficients)
  se <- sqrt(rowSums((new_design %*% fit$covariance) * new_design))
  data.table(
    quality = quality, adjusted_mean = adjusted_mean,
    ci_low = adjusted_mean - qt(.975, fit$df) * se,
    ci_high = adjusted_mean + qt(.975, fit$df) * se, n = fit$n,
    quality_mean = mean(fit$data[[predictor]]),
    quality_p05 = limits[1], quality_p95 = limits[2]
  )
}

table(people$country)

table(people$own[people$country == "CO"])
table(people$target[people$country == "CO"])

table(people$own[people$country == "AR"])
table(people$own[people$country == "AR"], people$target[people$country == "AR"])
table(people$target[people$country == "AR"])


# 4. Registered analyses and sensitivity checks ---------------------------

outcomes <- c("warmth", "polarization", "anger", "understanding",
              "rationality", "character", "evidence", "engagement")
primary_outcomes <- c("warmth", "polarization", "rationality")
primary <- secondary <- sensitivities <- assigned <- list()
slopes <- curves <- reliability <- descriptives <- diagnostics <- list()

for (country_code in c("AR", "CO")) {
  message("Fitting primary and attitude models: ", country_code)
  country_people <- people[country == country_code & named == 1]
  country_ratings <- ratings[country == country_code & named == 1]
  label <- paste0(country_code, "_primary_rating")
  rating_fit <- run_rating_tests(country_ratings, label, verify = TRUE)
  primary[[label]] <- rating_fit$results
  diagnostics[[label]] <- rating_fit[c("diagnostics", "verification", "error")]

  # Fit each attitude model once, then extract both quality slopes and curves.
  # A ten-point quality increment makes coefficients easier to read.
  for (outcome in outcomes) {
    label <- paste(country_code, "primary", outcome, sep = "_")
    post <- run_post_test(country_people, outcome, label)
    diagnostics[[label]] <- list(details = post$diagnostics, error = post$error)
    if (outcome %in% primary_outcomes) {
      primary[[label]] <- post$results
    }
    for (predictor in c("qopp", "qown")) {
      key <- paste(country_code, outcome, predictor, sep = "_")
      effect <- tryCatch({
        if (!is.null(post$error)) stop(post$error)
        estimate <- extract_term(post$fit, predictor, multiplier = 10)
        estimate[, status := "ok"]
        estimate
      }, error = function(error) failed_effect(conditionMessage(error)))
      slopes[[key]] <- cbind(data.table(
        country = country_code, outcome = outcome, predictor = predictor,
        n = post$results$n,
        family = if (predictor == "qopp" && outcome %in% primary_outcomes) {
          "Original 10 primary tests"
        } else {
          "Extended 26 exploratory slopes"
        }
      ), effect)
      if (effect$status == "ok") {
        curves[[key]] <- cbind(data.table(
          country = country_code, outcome = outcome, predictor = predictor
        ), make_adjusted_curve(post$fit, predictor))
      }
    }
    if (is.null(post$error)) {
      response <- post$fit$data[[outcome]]
      descriptives[[label]] <- data.table(
        country = country_code, outcome = outcome,
        eligible_nonmissing = sum(!is.na(country_people[[outcome]])),
        model_n = post$fit$n, mean = mean(response), sd = sd(response),
        minimum = min(response), maximum = max(response)
      )
    }
  }
  for (scale in c("rationality", "character", "own_character", "evidence", "engagement")) {
    items <- if (scale %in% c("rationality", "character", "own_character")) 6L else 3L
    prefix <- if (scale == "rationality") "rat" else scale
    item_columns <- paste0(prefix, seq_len(items))
    item_matrix <- as.matrix(country_people[, ..item_columns])
    item_matrix <- item_matrix[complete.cases(item_matrix), , drop = FALSE]
    reliability[[paste(country_code, scale)]] <- data.table(
      country = country_code, outcome = scale, n = nrow(item_matrix),
      alpha = items / (items - 1) *
        (1 - sum(apply(item_matrix, 2, var)) / var(rowSums(item_matrix)))
    )
  }
  interaction <- run_rating_tests(
    country_ratings, paste0(country_code, "_interaction"), interaction = TRUE
  )
  secondary[[paste0(country_code, "_interaction")]] <- interaction$results
  for (topic_name in topics_by_country[[country_code]]) {
    label <- paste(country_code, "topic", topic_name, sep = "_")
    topic_fit <- run_rating_tests(country_ratings[topic == topic_name], label)
    topic_fit$results[, topic := topic_name]
    secondary[[label]] <- topic_fit$results
    diagnostics[[label]] <- list(details = topic_fit$diagnostics, error = topic_fit$error)
  }
  # These exclusion combinations are sensitivity choices, not primary rules
  for (variant in c("attention", "speed_invariant", "categorical_age")) {
    subset <- switch(
      variant,
      attention = country_people[attention_flag == 0 & uncertain_assistance == 0],
      speed_invariant = country_people[duration >= 5 & invariant == 0],
      categorical_age = country_people
    )
    label <- paste(country_code, variant, sep = "_")
    check <- run_primary_family(
      subset, country_ratings[pid %in% subset$pid], label,
      categorical_age = variant == "categorical_age"
    )
    check$results[, variant := variant]
    sensitivities[[label]] <- check$results
    diagnostics[[label]] <- check$diagnostics
  }
}

# "All equally" and "none" identify no unique opponent. Analyze each profile
# against its assigned comparison camp, retaining the named-sample API scale.
for (profile_name in c("todos_por_igual", "ninguno")) {
  subset <- people[country == "AR" & profile == profile_name]
  label <- paste0("AR_", profile_name)
  comparison <- run_primary_family(
    subset, ratings[country == "AR" & pid %in% subset$pid], label
  )
  comparison$results[, profile := profile_name]
  assigned[[profile_name]] <- comparison$results
  diagnostics[[label]] <- comparison$diagnostics
}

primary <- rbindlist(primary)
stopifnot(nrow(primary) == 10L)
primary[, p_holm := p.adjust(p, "holm")]
sensitivities <- rbindlist(sensitivities)
sensitivities[, p_holm := p.adjust(p, "holm"), by = variant]
slopes <- rbindlist(slopes)
slopes[, p_adjusted := NA_real_]
exploratory_family <- "Extended 26 exploratory slopes"
stopifnot(nrow(slopes[family == exploratory_family]) == 26L)
slopes[family == exploratory_family, p_adjusted := p.adjust(p, "holm")]
for (row in which(slopes$family == "Original 10 primary tests")) {
  country_code <- slopes$country[row]
  outcome <- slopes$outcome[row]
  slopes$p_adjusted[row] <- primary[country == country_code & test == outcome, p_holm]
}
curves <- rbindlist(curves)

# Secondary topic tests and assigned-comparison tests retain descriptive raw
# p-values. Confidence intervals throughout are unadjusted two-sided 95% CIs.
fwrite(primary, file.path(output_dir, "primary_results.csv"))
fwrite(rbindlist(secondary, fill = TRUE), file.path(output_dir, "secondary_results.csv"))
fwrite(sensitivities, file.path(output_dir, "sensitivity_results.csv"))
fwrite(rbindlist(assigned), file.path(output_dir, "argentina_assigned_comparison_results.csv"))
fwrite(slopes, file.path(output_dir, "relationship_slopes.csv"))
fwrite(curves, file.path(output_dir, "adjusted_curves.csv"))
fwrite(rbindlist(reliability), file.path(output_dir, "scale_reliability.csv"))
fwrite(rbindlist(descriptives), file.path(output_dir, "outcome_descriptives.csv"))
fwrite(people[, .(n = .N, qualifying = sum(qualifying)), by = .(country, own, profile)],
       file.path(output_dir, "sample_by_camp.csv"))


# 5. Agreement x camp follow-ups ------------------------------------------

# These models use ratings as the outcome, with crossed random intercepts
# for evaluator and reason. Author camp was hidden from evaluators.
factorial_data <- merge(
  ratings[named == 1], people[, .(country, pid, age, strength)],
  by = c("country", "pid"), all.x = TRUE, sort = FALSE
)
factorial_data[, stance := factor(
  ifelse(same == 1, "Same stance", "Different stance"),
  levels = c("Same stance", "Different stance")
)]
factorial_data[, camp := factor(
  ifelse(opp == 1, "Named opponent", ifelse(other == 1, "Other camp", "Own camp")),
  levels = c("Own camp", "Named opponent", "Other camp")
)]
setorder(factorial_data, country, pid, order)
emmeans::emm_options(
  lmer.df = "satterthwaite", lmerTest.limit = Inf, disable.pbkrtest = TRUE
)

fit_factorial_model <- function(data, adjusted = FALSE, author_effect = FALSE) {
  data <- droplevels(as.data.frame(data))
  formula <- as.formula(paste(
    "rating ~ stance * camp + order",
    if (adjusted) "+ age + strength + factor(own)" else "",
    "+ (1 | pid) + (1 | rid)", if (author_effect) "+ (1 | aid)" else ""
  ))
  # Refit from the current data; do not reuse a model saved from an earlier run
  model <- lmerTest::lmer(
    formula, data = data, REML = FALSE,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
  )
  convergence <- model@optinfo$conv$lme4$messages
  if (!is.null(convergence) || any(!is.finite(lme4::fixef(model)))) {
    stop("Mixed-model convergence failure: ", paste(convergence, collapse = "; "))
  }
  list(
    model = model,
    diagnostics = list(
      formula = deparse(formula), n = nobs(model),
      evaluators = uniqueN(data$pid), reasons = uniqueN(data$rid),
      singular = lme4::isSingular(model), convergence = convergence,
      random_effect_sd = as.data.frame(lme4::VarCorr(model)),
      log_likelihood = as.numeric(logLik(model)), df_method = "Satterthwaite"
    )
  )
}

factorial_contrasts <- function(model, country_code, label, full_argentina = FALSE) {
  means <- emmeans::emmeans(
    model, ~stance * camp, at = list(order = 0), weights = "proportional"
  )
  cells <- as.data.frame(means)
  same <- cells$stance == "Same stance"
  own <- cells$camp == "Own camp"
  opponent <- cells$camp == "Named opponent"
  # Average the main contrasts equally across the other factor. The interaction
  # is the camp advantage at the same stance minus that at a different stance.
  contrasts <- list(
    agreement = .5 * (same & own) + .5 * (same & opponent) -
      .5 * (!same & own) - .5 * (!same & opponent),
    camp = .5 * own - .5 * opponent,
    interaction = as.numeric(same & own) - as.numeric(same & opponent) -
      as.numeric(!same & own) + as.numeric(!same & opponent),
    camp_same = as.numeric(same & own) - as.numeric(same & opponent),
    camp_different = as.numeric(!same & own) - as.numeric(!same & opponent),
    agreement_own = as.numeric(same & own) - as.numeric(!same & own),
    agreement_opponent = as.numeric(same & opponent) - as.numeric(!same & opponent)
  )
  if (full_argentina) {
    contrasts <- contrasts[c("agreement", "camp", "interaction")]
  }
  tests <- as.data.table(summary(
    emmeans::contrast(means, contrasts), infer = c(TRUE, TRUE), adjust = "none"
  ))
  tests[, `:=`(
    country = country_code, model = label,
    family = ifelse(contrast %in% c("agreement", "camp", "interaction"),
                    "factorial", "simple")
  )]
  cells <- as.data.table(cells)
  cells[, `:=`(country = country_code, model = label)]
  list(means = cells, tests = tests)
}

factorial_means <- factorial_tests <- mixed_checks <- fixed_checks <- cell_counts <- list()
for (country_code in c("AR", "CO")) {
  message("Fitting the agreement x camp follow-ups: ", country_code)
  all_ratings <- factorial_data[country == country_code]
  subset <- droplevels(all_ratings[other == 0])
  cell_counts[[country_code]] <- subset[, .(
    ratings = .N, evaluators = uniqueN(pid), reasons = uniqueN(rid),
    authors = uniqueN(aid), raw_mean = mean(rating), raw_sd = sd(rating)
  ), by = .(country, stance, camp)]
  label <- paste0(country_code, "_factorial_2x2")
  fit <- fit_factorial_model(subset)
  extracted <- factorial_contrasts(fit$model, country_code, label)
  factorial_means[[country_code]] <- extracted$means
  factorial_tests[[country_code]] <- extracted$tests
  diagnostics[[label]] <- fit$diagnostics
  for (variant in c("demographic", "author")) {
    check_data <- if (variant == "demographic") {
      subset[complete.cases(age, strength)]
    } else {
      subset
    }
    label <- paste(country_code, variant, sep = "_")
    check <- fit_factorial_model(
      check_data, adjusted = variant == "demographic", author_effect = variant == "author"
    )
    mixed_checks[[label]] <- factorial_contrasts(check$model, country_code, label)$tests
    diagnostics[[label]] <- check$diagnostics
  }
  if (country_code == "AR") {
    label <- "AR_full_2x3"
    check <- fit_factorial_model(all_ratings)
    extracted <- factorial_contrasts(check$model, country_code, label, full_argentina = TRUE)
    mixed_checks[[label]] <- extracted$tests
    diagnostics[[label]] <- check$diagnostics
    fwrite(extracted$means, file.path(output_dir, "argentina_2x3_means.csv"))
    fwrite(as.data.table(emmeans::joint_tests(check$model)),
           file.path(output_dir, "argentina_2x3_joint_tests.csv"))
  }

  # Check the 2 x 2 contrasts using the registered fixed effects and clustering
  design <- cbind(same = subset$same, opp = subset$opp,
                  same_opp = subset$same * subset$opp, order = subset$order)
  fixed_fit <- fit_rating_model(
    subset, design, c("same", "opp", "same_opp"), verify = TRUE
  )
  stopifnot(all(fixed_fit$identified))
  contrast_matrix <- rbind(
    agreement = c(1, 0, .5, 0), camp = c(0, -1, -.5, 0),
    interaction = c(0, 0, -1, 0), camp_same = c(0, -1, -1, 0),
    camp_different = c(0, -1, 0, 0), agreement_own = c(1, 0, 0, 0),
    agreement_opponent = c(1, 0, 1, 0)
  )
  estimates <- as.numeric(contrast_matrix %*% fixed_fit$coefficients)
  variances <- diag(contrast_matrix %*% fixed_fit$covariance %*% t(contrast_matrix))
  stopifnot(all(is.finite(variances)), all(variances > 0))
  effects <- infer_effect(estimates, sqrt(variances), fixed_fit$df)
  setnames(effects, c("se", "ci_low", "ci_high", "p"),
           c("SE", "lower.CL", "upper.CL", "p.value"))
  effects[, `:=`(
    country = country_code, model = paste0(country_code, "_FE_2x2"),
    contrast = rownames(contrast_matrix), t.ratio = estimate / SE,
    family = ifelse(rownames(contrast_matrix) %in% c("agreement", "camp", "interaction"),
                    "factorial", "simple")
  )]
  fixed_checks[[country_code]] <- effects
  diagnostics[[paste0(country_code, "_FE_2x2")]] <-
    fixed_fit[c("diagnostics", "verification")]
}

factorial_tests <- rbindlist(factorial_tests)
factorial_means <- rbindlist(factorial_means)
fixed_checks <- rbindlist(fixed_checks)
mixed_checks <- rbindlist(mixed_checks)
# Six factorial tests and eight simple effects, each family across countries.
# Additional mixed-model checks retain the earlier within-model corrections.
factorial_tests[, p_holm := p.adjust(p.value, "holm"), by = family]
fixed_checks[, p_holm := p.adjust(p.value, "holm"), by = family]
mixed_checks[, p_holm := p.adjust(p.value, "holm"), by = .(model, family)]
fwrite(factorial_tests, file.path(output_dir, "factorial_tests.csv"))
fwrite(factorial_means, file.path(output_dir, "factorial_cell_means.csv"))
fwrite(rbindlist(cell_counts), file.path(output_dir, "factorial_cell_counts.csv"))
fwrite(fixed_checks, file.path(output_dir, "fixed_effects_2x2_checks.csv"))
fwrite(mixed_checks, file.path(output_dir, "mixed_model_checks.csv"))


# 6. Figures and output files ---------------------------------------------

make_factorial_figure <- function(means) {
  means <- copy(means)
  means[, country_name := factor(country, levels = c("AR", "CO"),
                                  labels = c("Argentina", "Colombia"))]
  means[, stance := factor(stance, levels = c("Same stance", "Different stance"))]
  means[, camp := factor(camp, levels = c("Own camp", "Named opponent"))]
  dodge <- ggplot2::position_dodge(width = .18)
  ggplot2::ggplot(means, ggplot2::aes(stance, emmean, color = camp,
                                    shape = camp, group = camp)) +
    ggplot2::geom_line(position = dodge, linewidth = .65) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower.CL, ymax = upper.CL),
                           position = dodge, width = .065, linewidth = .8) +
    ggplot2::geom_point(position = dodge, size = 3.2) +
    ggplot2::scale_color_manual(values = c("Own camp" = "#298C8C", "Named opponent" = "#EA801C")) +
    ggplot2::scale_shape_manual(values = c("Own camp" = 16, "Named opponent" = 15)) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = .45)) +
    ggplot2::facet_wrap(~country_name, nrow = 1) +
    ggplot2::labs(x = NULL, y = "Estimated reason-quality rating (0-100)",
                  color = "Author camp", shape = "Author camp") +
    ggplot2::theme_bw(base_family = "serif", base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#EAEAEA", linewidth = .4),
      strip.background = ggplot2::element_rect(fill = "#DEDEDE"),
      strip.text = ggplot2::element_text(face = "bold", size = 14),
      axis.text = ggplot2::element_text(color = "#333333", size = 12),
      legend.position = "bottom"
    )
}

make_attitude_figure <- function(curves) {
  curves <- copy(curves[predictor == "qopp"])
  curves[, country_name := factor(country, levels = c("AR", "CO"),
                                   labels = c("Argentina", "Colombia"))]
  titles <- c(
    warmth = "Warmth", polarization = "Affective polarization index",
    anger = "Anger", understanding = "Understanding", rationality = "Rationality",
    character = "Moral character", evidence = "Openness to evidence",
    engagement = "Willingness to engage"
  )
  units <- c(warmth = "Score (0-100)", polarization = "API units",
             anger = "Score (0-6)", understanding = "Score (0-6)",
             rationality = "Score (1-6)", character = "Score (1-6)",
             evidence = "Score (1-6)", engagement = "Score (1-7)")
  panels <- lapply(seq_along(titles), function(index) {
    outcome_name <- names(titles)[index]
    data <- curves[outcome == outcome_name]
    data[, panel := paste(LETTERS[index], titles[index])]
    limits <- range(c(data$ci_low, data$ci_high))
    padding <- diff(limits) * .1
    plot <- ggplot2::ggplot(data, ggplot2::aes(
      quality, adjusted_mean, color = country_name,
      linetype = country_name, group = country_name
    )) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_low, ymax = ci_high),
                            fill = "#9D9D9D", color = NA, alpha = .22, show.legend = FALSE) +
      ggplot2::geom_line(linewidth = .85) +
      ggplot2::scale_color_manual(values = c(Argentina = "#298C8C", Colombia = "#EA801C")) +
      ggplot2::scale_linetype_manual(values = c(Argentina = "solid", Colombia = "22")) +
      ggplot2::scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100)) +
      ggplot2::coord_cartesian(ylim = limits + c(-padding, padding)) +
      ggplot2::facet_wrap(~panel) +
      ggplot2::labs(x = NULL, y = units[outcome_name], color = NULL, linetype = NULL) +
      ggplot2::theme_bw(base_family = "serif", base_size = 13) +
      ggplot2::theme(
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#EAEAEA", linewidth = .35),
        strip.background = ggplot2::element_rect(fill = "#DEDEDE"),
        strip.text = ggplot2::element_text(size = 13, face = "bold"),
        axis.title = ggplot2::element_text(size = 11.5),
        axis.text = ggplot2::element_text(size = 11, color = "#333333"),
        plot.margin = ggplot2::margin(3, 7, 3, 4)
      )
    if (outcome_name == "polarization") {
      plot <- plot + ggplot2::geom_hline(
        yintercept = 0, linetype = "dotted", color = "#777777", linewidth = .35
      )
    }
    plot
  })
  (patchwork::wrap_plots(panels, ncol = 2, guides = "collect") +
     patchwork::plot_annotation(
       caption = "Mean quality of opponent reasons (0-100)",
       theme = ggplot2::theme(plot.caption = ggplot2::element_text(
         family = "serif", size = 13, hjust = .5
       ))
     )) & ggplot2::theme(legend.position = "top")
}

message("Saving the report figures and diagnostics...")
ggplot2::ggsave(
  file.path(output_dir, "agreement_by_camp.png"), make_factorial_figure(factorial_means),
  width = 9, height = 4.8, dpi = 450, device = ragg::agg_png, bg = "white"
)
ggplot2::ggsave(
  file.path(output_dir, "attitude_associations.png"), make_attitude_figure(curves),
  width = 8.5, height = 9, dpi = 450, device = ragg::agg_png, bg = "white"
)
jsonlite::write_json(
  diagnostics, file.path(output_dir, "model_diagnostics.json"),
  pretty = TRUE, auto_unbox = TRUE, digits = 16, null = "null"
)
inputs <- unlist(input_files)
fwrite(data.table(input = names(inputs), file = basename(inputs),
                 md5 = unname(tools::md5sum(inputs))),
       file.path(output_dir, "input_fingerprints.csv"))
capture.output(sessionInfo(), file = file.path(output_dir, "R_session_info.txt"))
print(primary[, .(country, test, n, estimate, ci_low, ci_high, p_holm, status)])
message("Finished. Results are in ", normalizePath(output_dir))


#7. Checks----

View(descriptives[[8]])
