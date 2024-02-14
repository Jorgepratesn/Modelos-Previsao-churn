var_dep_treino <- as.matrix(treino[,2])
var_explica_treino <- as.matrix(treino[,c(3:35)])
set.seed(1)
# Grid Search 
grid_xgb <- expand.grid(
  eta = c(seq(from = 0.01, to = 0.3, by =0.05 )),
  max_depth = c(seq(from = 1, to = 10, by =2 )))
set.seed(1)
for(i in 1:nrow(grid_xgb)) {
  set.seed(1)
  # Especificando o modelo
  xgb_grid <- xgb.cv(
    data = var_explica_treino,
    label = var_dep_treino,
    nfold=10,
    nrounds = 100,
    eta = grid_xgb$eta[i],
    max_depth = grid_xgb$max_depth[i],
    objective = "binary:logistic",
    metrics = "error",
    verbose = 0,
    early_stopping_rounds=10
  )
  # Coletando os erros de cada iteração
  grid_xgb$optimal_trees[i] <- which.min(xgb_grid$evaluation_log$test_error_mean)
  grid_xgb$min_error[i] <- min(xgb_grid$evaluation_log$test_error_mean)
}
# Identificando os erros mínimos 
grid_xgb %>% 
  dplyr::arrange(min_error) %>%
  head(10)
# Estimando o modelo final
xgb_final <- xgboost::xgboost(
  data = var_explica_treino,
  label = var_dep_treino,  
  eta = 0.26,
  max_depth = 9,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "error",
  verbose = 0,
  early_stopping_rounds=10
)
# Valores preditos na base de teste
var_dep_teste <- as.matrix(teste[,2])
var_explica_teste <- as.matrix(teste[,c(3:35)])
pred_xgb_teste_prob <- predict(xgb_final, var_explica_teste, type = "prob")
ROC_xgb_teste <- roc(response = var_dep_teste, 
                     predictor = pred_xgb_teste_prob)
ROC_xgb_teste

#Matriz de Confusão
cutoffs <- seq(0.1, 0.9, by = 0.05)
melhor_cutoff <- NULL
melhor_acuracia <- 0
for (cutoff in cutoffs) {
  preditos_xgb <- factor(ifelse(pred_xgb_teste_prob> cutoff, "1", "0"), levels = levels(teste$Churn))
  confusion_matrix_xgb <- caret::confusionMatrix(
    data = preditos_xgb,
    reference = as.factor(teste$Churn)
  )
  mc<-confusion_matrix_xgb$overall
  if (mc[1]>melhor_acuracia){
    melhor_acuracia <- mc[1]
    melhor_cutoff <- cutoff
  }
}
preditos_xgb <- factor(ifelse(pred_xgb_teste_prob> melhor_cutoff, "1", "0"), levels = levels(teste$Churn))
confusion_matrix_xgb <- caret::confusionMatrix(
  data = preditos_xgb,
  reference = as.factor(teste$Churn)
)
confusion_matrix_xgb

# Importancia da Variavel
imp_xgb <- data.frame(xgb.importance(model = xgb_final))
imp_xgb <- imp_xgb %>%
  arrange(desc(Gain))
imp_xgb
