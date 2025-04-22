library(tidyverse)
library(caret)
library(pROC)         
library(glmnet)
library(rpart)
library(randomForest)


data <- read_csv("DATA (1).csv") %>%

  mutate(hyper = if_else(BPQ020 == 1, "yes", "no")) %>%

  select(hyper,
         RIDAGEYR,    
         INDFMPIR,    
         BMXBMI,      
         SMQ020,      
         DIQ010,      
         depressed    
  ) %>%
  drop_na()     



set.seed(123)
ctrl <- trainControl(
  method         = "cv",
  number         = 10,
  classProbs     = TRUE,
  summaryFunction= function(df, lev, model) {
    c(defaultSummary(df, lev, model),
      twoClassSummary(df, lev, model))
  }
)


glm0 <- train(
  hyper ~ .,
  data      = data,
  method    = "glm",
  family    = "binomial",
  metric    = "ROC",
  trControl = ctrl
)


x       <- model.matrix(hyper ~ . -1, data)
y       <- if_else(data$hyper=="yes", 1, 0)
cv_lasso<- cv.glmnet(x, y,
                     family = "binomial",
                     alpha  = 1,
                     nfolds = 10)

sel <- rownames(coef(cv_lasso, s = "lambda.1se"))[
  coef(cv_lasso, s = "lambda.1se")[,1] != 0
][-1]


glm_lasso <- train(
  as.formula(paste("hyper ~", paste(sel, collapse = "+"))),
  data      = data,
  method    = "glm",
  family    = "binomial",
  metric    = "ROC",
  trControl = ctrl
)


tree <- train(
  hyper ~ .,
  data      = data,
  method    = "rpart",
  metric    = "ROC",
  trControl = ctrl
)

rf <- train(
  hyper ~ .,
  data      = data,
  method    = "rf",
  metric    = "ROC",
  trControl = ctrl
)


models <- list(
  "Logistic Regression"        = glm0,
  "LASSO Logistic Regression"  = glm_lasso,
  "Decision Tree"              = tree,
  "Random Forest"   = rf
)

library(purrr)
library(dplyr)

summary_table <- imap_dfr(models, ~{
  res  <- .x$results
  best <- res %>% slice_max(ROC, n = 1)
  tibble(
    Model        = .y,
    AUC         = best$ROC,
    Accuracy    = best$Accuracy,
    Sensitivity = best$Sens,
    Specificity = best$Spec
  )
})

print(summary_table)





lasso_coefs <- coef(cv_lasso, s = "lambda.1se")
lasso_imp <- data.frame(
  variable    = rownames(lasso_coefs),
  coefficient = as.numeric(lasso_coefs)
) %>%
  filter(variable != "(Intercept)" & coefficient != 0) %>%
  arrange(desc(abs(coefficient)))
print(lasso_imp)

tree_imp <- varImp(tree, scale = FALSE)$importance %>%
  rownames_to_column("variable") %>%
  arrange(desc(Overall))
print(tree_imp)


rf_imp <- varImp(rf, scale = FALSE)$importance %>%
  rownames_to_column("variable") %>%
  arrange(desc(Overall))
print(rf_imp)













