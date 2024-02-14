#Carregando pacotes
pacotes <- c('readxl','dplyr','tidyverse','Hmisc','reshape2','ggplot2','plotly','fastDummies','MASS','rlang',"pROC",'caret','randomForest','xgboost')

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Transformar colunas em factors
dados <- dados %>%
  mutate(
    CityTier = as.factor(CityTier),
    SatisfactionScore = as.factor(SatisfactionScore),
    Complain = as.factor(Complain),
    Churn = as.factor(Churn),
    CustomerID = as.factor(CustomerID)
  )

# Limpeza dos dados - Valores nulos
dados <- dados %>%
  mutate_if(is.numeric, list(~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

#Limpeza dos dados - Outliers
remove_outliers <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  limite_inferior <- Q1 - 3 * IQR
  limite_superior <- Q3 + 3 * IQR
  x <- ifelse(x < limite_inferior | x > limite_superior, NA, x)
  return(x)
} #Função para remover outliers
dados <- dados %>%
  mutate_all(~ remove_outliers(.)) %>%
  na.omit() # Aplicação da função para remover outliers

# Estatisticas Descritivas
# Variaveis Quantitativas
estatisticas_descritivas <- dados %>%
  select_if(is.numeric) %>%
  summary() %>%
  print() # Estatisticas Gerais
dados_churn_zero <- dados[dados$Churn == 1,]
estatisticas_descritivas <- dados_churn_zero %>%
  select_if(is.numeric) %>%
  summary() %>%
  print() # Estatisticas considerando somente clientes non churns
ggplot(dados, aes(x = CashbackAmount)) +
  geom_histogram(binwidth = 50, color = "red", fill = "white") +
  labs(title = "Histograma de CashbackAmount", x = "CashbackAmount", y = "Frequência") #histograma variaveis quantitativas
# Variaveis Qualitativa
dados2<-dados
dados2$Churn <- as.numeric(dados2$Churn)
variaveis_qualitativas <- names(dados2)[sapply(dados2, is.factor) | sapply(dados2, is.character)]
variaveis_qualitativas <- setdiff(variaveis_qualitativas, "CustomerID")
base_total <- nrow(dados)
tabela1 <- dados2 %>%
  pull(Complain) %>%
  table() %>%
  prop.table() %>%
  as.data.frame() 
colnames(tabela1) <- c("variável_categórica", "representatividade total")
tabela2 <- dados2 %>%
  group_by(Complain) %>%
  summarise(percentual_churn = mean(Churn == 1) * 100)
colnames(tabela2) <- c("variável_categórica", "percentual_churn")
tabela_final <- left_join(tabela1, tabela2,  by = "variável_categórica")
print(tabela_final)

# Matriz de correlação
dados_numericos <- dados %>%
  select_if(is.numeric)
rho <- rcorr(as.matrix(dados_numericos), type = "pearson")
corr_coef <- rho$r
heatmap_data <- as.data.frame(corr_coef) %>%
  rownames_to_column(var = "Var1") %>%
  gather(key = "Var2", value = "Correlacao", -Var1)
heatmap_plot <- ggplot(heatmap_data, aes(x = Var1, y = Var2, fill = Correlacao)) +
  geom_tile() +
  geom_text(aes(label = round(Correlacao, digits = 2)), vjust = 1) +
  scale_fill_viridis_c() +
  labs(title = "Mapa de Calor das Correlações de Pearson",
       x = "Variável 1", y = "Variável 2") +
  theme_minimal()
heatmap_plotly <- ggplotly(heatmap_plot)
print(heatmap_plotly)

#Padronização dos dados
dados <- dados %>%
  mutate_if(is.numeric, scale)

#dummies
dumizar_colunas <- function(data, colunas) {
  dummys <- dummy_columns(.data = data,
                          select_columns = colunas,
                          remove_selected_columns = TRUE,
                          remove_first_dummy = TRUE)
  return(dummys)
} #Função para dumizar variaveis qualitativas
dados <- dumizar_colunas(data = dados, colunas = variaveis_qualitativas) #aplicação da função
colnames(dados) <- gsub(" ", "_", colnames(dados))
colnames(dados) <- gsub("&", "", colnames(dados))

# Split validation
set.seed(1) # Garantir reprodutividade
bool_treino <- stats::runif(dim(dados)[1])>.25
treino <- dados[bool_treino,]
teste  <- dados[!bool_treino,]
