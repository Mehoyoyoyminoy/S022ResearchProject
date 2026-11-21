# Run from the project root. Requires ggplot2 and glmnet.
library(ggplot2)
library(glmnet)

output <- "Milestone2/figures"
dir.create(output, showWarnings = FALSE)
acs <- read.csv("Milestone1/tidy_data.csv")
acs$education_level <- factor(acs$education_level, levels = c(
  "Grade 12 or less", "Some college or associate's", "Bachelor's degree",
  "Graduate/professional degree"
))
degree <- aggregate(share_pct ~ subgroup, acs[as.integer(acs$education_level) >= 3, ], sum)
acs$subgroup <- factor(acs$subgroup, levels = degree$subgroup[order(degree$share_pct)])
education_plot <- ggplot(acs, aes(share_pct, subgroup, fill = education_level)) +
  geom_col(width = 0.72, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c("#D8E5EB", "#93B8C9", "#3E829D", "#154A63")) +
  scale_x_continuous(breaks = seq(0, 100, 25), labels = function(x) paste0(x, "%")) +
  labs(title = "Educational attainment varies across Asian American subgroups",
       subtitle = "Weighted shares among adults ages 24–64",
       x = NULL, y = NULL, fill = NULL,
       caption = "Source: 2023 American Community Survey via IPUMS USA.") +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())
ggsave(file.path(output, "acs_education.png"), education_plot, width = 8, height = 4.5, dpi = 300)
print(degree[order(degree$share_pct, decreasing = TRUE), ], row.names = FALSE)

human <- read.csv("onet_data/holdout/onet_holdout_coded.csv")
llm <- read.csv("onet_data/holdout/onet_holdout_llm_labeled.csv")
input <- read.csv("onet_data/cleaned/onet_llm_input.csv")
stopifnot(!anyDuplicated(human$onet_soc_code), !anyDuplicated(llm$onet_soc_code),
          setequal(human$onet_soc_code, llm$onet_soc_code))
human <- human[match(llm$onet_soc_code, human$onet_soc_code), ]
labels <- c("technical", "analytical", "interpersonal", "managerial", "manual")
audit <- do.call(rbind, lapply(labels, function(label) {
  scored <- complete.cases(human[[label]], llm[[label]])
  n <- sum(scored)
  agree <- sum(human[[label]][scored] == llm[[label]][scored])
  data.frame(label = tools::toTitleCase(label), n, agree, pct = 100 * agree / n)
}))
audit$text <- sprintf("%d/%d · %.1f%%", audit$agree, audit$n, audit$pct)
audit_plot <- ggplot(audit, aes(pct, reorder(label, pct))) +
  geom_col(width = 0.58, fill = "#237A78") +
  geom_text(aes(label = text), hjust = -0.12, size = 3.6) +
  scale_x_continuous(limits = c(0, 105), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "LLM agreement was lowest for analytical work",
       subtitle = "Independent human audit of O*NET occupation labels",
       x = NULL, y = NULL,
       caption = "Agreement excludes three catch-all occupations left blank by both scorers.") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
ggsave(file.path(output, "onet_holdout_agreement.png"), audit_plot, width = 7, height = 3.5, dpi = 300)

# Predict the frozen prompt's technical score on the scored holdout subset.
# Human scores and model rationale are excluded from all model features.
scored <- !is.na(llm$technical)
index <- match(llm$onet_soc_code[scored], input$onet_soc_code)
stopifnot(!anyNA(index))
y <- llm$technical[scored]
group <- input$soc2018_code[index]
tokens <- strsplit(gsub("[^a-z]+", " ", tolower(input$llm_text[index])), " +")
tokens <- lapply(tokens, function(words) unique(words[nchar(words) >= 3]))
set.seed(52)

group_folds <- function(groups, k) {
  unique_groups <- sort(unique(groups))
  assignment <- sample(rep(seq_len(k), length.out = length(unique_groups)))
  assignment[match(groups, unique_groups)]
}

predict_path <- function(train, test, penalties) {
  frequency <- table(unlist(tokens[train]))
  vocabulary <- names(frequency)[frequency >= 2]
  stopifnot(length(vocabulary) >= 2, length(unique(y[train])) == 2)
  encode <- function(rows) {
    matches <- lapply(tokens[rows], function(words) {
      indices <- match(words, vocabulary)
      indices[!is.na(indices)]
    })
    Matrix::sparseMatrix(i = rep(seq_along(rows), lengths(matches)),
                         j = unlist(matches), x = 1,
                         dims = c(length(rows), length(vocabulary)))
  }
  fit <- glmnet(encode(train), y[train], family = "binomial", alpha = 1,
                lambda = penalties)
  as.matrix(predict(fit, newx = encode(test), s = penalties, type = "response"))
}

rows <- seq_along(y)
outer <- group_folds(group, 5)
penalties <- 10 ^ seq(0, -3, length.out = 25)
baseline <- lasso <- numeric(length(y))
for (fold in sort(unique(outer))) {
  train <- rows[outer != fold]
  test <- rows[outer == fold]
  inner <- group_folds(group[train], 4)
  inner_prob <- matrix(NA_real_, length(train), length(penalties))
  for (validation in sort(unique(inner))) {
    inner_prob[inner == validation, ] <- predict_path(
      train[inner != validation], train[inner == validation], penalties
    )
  }
  # Choose lambda using inner-fold balanced accuracy, favoring more shrinkage on ties.
  inner_class <- inner_prob >= 0.5
  inner_y <- y[train]
  score <- (colMeans(inner_class[inner_y == 1, , drop = FALSE]) +
            colMeans(!inner_class[inner_y == 0, , drop = FALSE])) / 2
  lambda <- penalties[which.max(score)]
  lasso[test] <- predict_path(train, test, penalties)[, match(lambda, penalties)] >= 0.5
  baseline[test] <- mean(y[train]) > 0.5
}
metrics <- function(pred) {
  sensitivity <- mean(pred[y == 1] == 1)
  specificity <- mean(pred[y == 0] == 0)
  c(accuracy = mean(pred == y), balanced_accuracy = (sensitivity + specificity) / 2,
    sensitivity = sensitivity, specificity = specificity)
}
cat("\nTechnical label counts\n")
print(table(y))
cat("\nPooled out-of-fold performance\n")
print(round(rbind(majority_baseline = metrics(baseline), lasso = metrics(lasso)), 3))
