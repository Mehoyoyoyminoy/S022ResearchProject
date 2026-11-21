# Run from the project root: Rscript Milestone2/06_compare_onet_pilot.R
# Add the argument holdout to compare the independent audit scores.
labels <- c("technical", "analytical", "interpersonal", "managerial", "manual")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) <= 1L, all(args %in% c("pilot", "holdout")))
sample_name <- if (length(args)) args[[1L]] else "pilot"

read_scores <- function(filename) {
  scores <- read.csv(file.path("onet_data", sample_name, filename),
                     colClasses = "character", na.strings = c("", "NA"))
  stopifnot(all(c("onet_soc_code", labels) %in% names(scores)),
            all(!is.na(scores$onet_soc_code) & nzchar(scores$onet_soc_code)),
            !anyDuplicated(scores$onet_soc_code))
  values <- unlist(scores[labels], use.names = FALSE)
  stopifnot(all(is.na(values) | values %in% c("0", "1")))
  scores
}

files <- switch(sample_name,
                pilot = c("onet_human_pilot.csv", "onet_pilot_llm_labeled.csv"),
                holdout = c("onet_holdout_coded.csv", "onet_holdout_llm_labeled.csv"))
human <- read_scores(files[[1L]])
llm <- read_scores(files[[2L]])
stopifnot(setequal(human$onet_soc_code, llm$onet_soc_code))
llm <- llm[match(human$onet_soc_code, llm$onet_soc_code), ]

summary_table <- do.call(rbind, lapply(labels, function(label) {
  h <- human[[label]]
  m <- llm[[label]]
  scored <- !is.na(h) & !is.na(m)
  agreements <- sum(h[scored] == m[scored])
  data.frame(label, both_scored = sum(scored), agreements,
             disagreements = sum(scored) - agreements,
             human_1_llm_0 = sum(h[scored] == "1" & m[scored] == "0"),
             human_0_llm_1 = sum(h[scored] == "0" & m[scored] == "1"),
             human_only_blank = sum(is.na(h) & !is.na(m)),
             llm_only_blank = sum(!is.na(h) & is.na(m)),
             both_blank = sum(is.na(h) & is.na(m)),
             agreement_pct = if (any(scored)) round(100 * agreements / sum(scored), 1) else NA_real_)
}))
print(summary_table, row.names = FALSE)

for (label in labels) {
  h <- human[[label]]
  m <- llm[[label]]
  differs <- xor(is.na(h), is.na(m)) | (!is.na(h) & !is.na(m) & h != m)
  if (any(differs)) {
    print(data.frame(label, occupation = human$occupation_title[differs],
                     human = h[differs], llm = m[differs]), row.names = FALSE)
  }
}
