library(tidyverse)

outcomes <- c("Satisfaction", "Worthwhile", "Anxiety", "Happiness")
missing_data_strategy <- c("Multiple imputation", "Complete case")
exposure_marker <- c("Predicted probability", "Observed receipt")
timing <- c("Natural migration phase", "Tailored to eligibility x rollout date")
research_question <- c("Population-wide treatment", "Subgroup analyses", "Subgroup - interaction terms", "Mediation analyses")

expand_grid(
  outcomes, missing_data_strategy, exposure_marker, timing, research_question
)
