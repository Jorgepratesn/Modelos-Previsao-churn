# GRIDSEARCH CROSS VALIDATION
levels(treino$Churn) <- make.names(levels(treino$Churn))
levels(teste$Churn) <- make.names(levels(teste$Churn))

valores_ntree <- seq(from = 100, to = 1000, by = 200)
valores_nsplit <- seq(from = 10, to = 100, by = 20)
ctrl <- trainControl(method = "cv", number = 10)  
melhor_acuracia <- 0
melhores_hiperparametros <- NULL
tabela_acuracia <- data.frame(mtry = NA, ntree = NA, nsplit = NA, acuracia = NA)
set.seed(1)
for (valor_ntree in valores_ntree) {
  for (valor_nsplit in valores_nsplit) {
    hyperparameters <- data.frame(mtry = c(4, 5))
    gridsearch_kfold <- train(Churn ~ . -CustomerID,
                              data = treino,
                              method = "rf",
                              trControl = ctrl,
                              tuneGrid = hyperparameters,
                              ntree = valor_ntree,
                              nsplit = valor_nsplit)
    acuracia_atual <- gridsearch_kfold$results$Accuracy[1]
    tabela_acuracia <- rbind(tabela_acuracia,
                             data.frame(mtry = hyperparameters$mtry[1],
                                        ntree = valor_ntree,
                                        nsplit = valor_nsplit,
                                        acuracia = acuracia_atual))
  }
}
print(tabela_acuracia)
set.seed(1)
rf <- randomForest::randomForest(
  Churn ~ . -CustomerID,
  data = treino,
  mtry=4,
  ntree = 500,
  nsplit=10
)

#Calculo da AUC do teste
preditos_rft_class <- predict(rf, teste)
preditos_rft_prob <- predict(rf, teste, type = "prob")


ROC_RF_teste <- roc(response = teste$Churn, 
                    predictor = preditos_rft_prob[,2])
ROC_RF_teste

#Accuracy
cutoffs <- seq(0.1, 0.9, by = 0.05)
melhor_cutoff <- NULL
melhor_acuracia <- 0
for (cutoff in cutoffs) {
  Acuracia <- teste %>%
    mutate(previsão = ifelse(preditos_rft_prob[, 2] > cutoff, 1, 0)) %>%
    mutate(Churn = ifelse(Churn == "X1", 1, 0)) %>%
    count(previsão == Churn) %>%
    pull(n) / nrow(teste)
  if (Acuracia[2] > melhor_acuracia) {
    melhor_acuracia <- Acuracia[2]
    melhor_cutoff <- cutoff
  }
}
print(melhor_acuracia)

#Matiz de cofusão
cutoffs <- seq(0.1, 0.9, by = 0.05)
melhor_cutoff <- NULL
melhor_acuracia <- 0
for (cutoff in cutoffs) {
  preditos <- factor(ifelse(preditos_rft_prob[, 2] > cutoff, "X1", "X0"), levels = levels(teste$Churn))
  confusion_matrix <- caret::confusionMatrix(
    data = preditos,
    reference = as.factor(teste$Churn)
  )
  mc<-confusion_matrix$overall
  if (mc[1]>melhor_acuracia){
    melhor_acuracia <- mc[1]
    melhor_cutoff <- cutoff
  }
}
preditos <- factor(ifelse(preditos_rft_prob[, 2] > melhor_cutoff, "X1", "X0"), levels = levels(teste$Churn))
confusion_matrix <- caret::confusionMatrix(
  data = preditos,
  reference = as.factor(teste$Churn)
)
confusion_matrix

#Importancia das variaveis
imp_rf <- data.frame(rf$importance)
imp_rf <- imp_rf %>%
  arrange(desc(MeanDecreaseGini))
imp_rf