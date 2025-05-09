# Run Boruta on the Swedish dataset
# Save results with short labels to .RData files

# Load required libraries
library(Boruta)
library(ggplot2)
library(reshape2)

set.seed(1)

df <- read.csv(file = "./data/data_sw.csv")
df <- na.omit(df)
df$Vitality <- as.numeric(df$Vitality)

# PBAT labels
PBAT_labels <- c(
  "Variation+", "Social-", "Affect+", "Retention-", "Challenge-", "Health+",
  "Cognition-", "Attention+", "Motivation-", "Retention+", "Challenge+", "Variation-",
  "Cognition+", "Attention-", "Social+", "Motivation+", "Health-", "Affect-"
)
colnames(df)[3:20] <- PBAT_labels

STOPD_labels <- c(
  "SAD",
  "ANXIOUS",
  "STRESSED",
  "ANGRY",
  "NOSUPP"
)
colnames(df)[21:25] <- STOPD_labels

colnames(df)[29] <- "VITAL"
colnames(df)[30] <- "HEALTH"

# Define the predictors vector
Predictors <- PBAT_labels
boruta_outcomes <- c(STOPD_labels, "VITAL", "HEALTH")

results <- list() # Store results for each outcome
importance_results <- list() # To keep track of actual importance scores

for (outcome in boruta_outcomes) {
  test.set <- df[, c(outcome, Predictors)]

  boruta.train <- Boruta(as.formula(paste(outcome, "~.")), data = test.set, doTrace = 0)
  final.boruta <- TentativeRoughFix(boruta.train)
  boruta.df <- attStats(final.boruta)
  boruta.df$Predictor <- rownames(boruta.df)

  # Store full importance data for reference
  importance_results[[outcome]] <- boruta.df

  # Mark "Rejected" predictors' ranks as NA
  boruta.df$Rank <- ifelse(boruta.df$decision == "Rejected", NA,
    rank(-boruta.df$meanImp, na.last = "keep")
  )
  boruta.df$outcome <- outcome
  results[[outcome]] <- boruta.df[, c("Predictor", "Rank", "outcome")]
}

# Save results
# save(results, file = "./results/Boruta_results_short_labels_sw.RData")
# save(importance_results, file = "./results/Boruta_results_importance_short_labels_sw.RData")
