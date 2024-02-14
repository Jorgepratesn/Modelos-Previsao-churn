#Modelo binominal
treino$CustomerID <- as.numeric(treino$CustomerID)
teste$CustomerID <- as.numeric(teste$CustomerID)

modelo_churn <- glm(formula = Churn ~ . -CustomerID, 
                       data = treino, 
                       family = "binomial")
summary(modelo_churn)
#stepwise modelo
set.seed(0)
modelo_churn_step <- step(object = modelo_churn,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(modelo_churn_step)

#curva ROC 
predicoes_churn <- predict(modelo_churn, newdata = teste, family = "binomial")
predicoes_churn_prob <- predict(modelo_churn, teste, type = "response")
predicoes_churn_step <- predict(modelo_churn_step, newdata = teste, family = "binomial")
predicoes_churn_step_prob <- predict(modelo_churn_step, teste, type = "response")

roc_churn <- roc(teste$Churn, predicoes_churn)
print(roc_churn)
roc_churn_step <- roc(teste$Churn, predicoes_churn_step)
print(roc_churn_step)

#Matriz de confusão
#Modelo binomial
cutoffs <- seq(0.1, 0.9, by = 0.05)
melhor_cutoff <- NULL
melhor_acuracia <- 0
for (cutoff in cutoffs) {
  preditos_glm <- factor(ifelse(predicoes_churn_prob > cutoff, "1", "0"), levels = levels(teste$Churn))
  confusion_matrix_glm <- caret::confusionMatrix(
    data = preditos_glm,
    reference = as.factor(teste$Churn)
  )
  mc<-confusion_matrix_glm$overall
  if (mc[1]>melhor_acuracia){
    melhor_acuracia <- mc[1]
    melhor_cutoff <- cutoff
  }
}
preditos_glm <- factor(ifelse(predicoes_churn_prob > melhor_cutoff, "1", "0"), levels = levels(teste$Churn))
confusion_matrix_glm <- caret::confusionMatrix(
  data = preditos_glm,
  reference = as.factor(teste$Churn)
)
confusion_matrix_glm