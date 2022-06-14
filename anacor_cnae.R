pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","reshape2","FactoMineR",
             "cabootcrs","knitr","kableExtra","gifski","gganimate","factoextra",
             "plot3D","viridis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

load("cnpj.RData")

# Apresentando os dados da base 'notasfatorial'
cnpj %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Estabelecendo a ACM -----------------------------------------------------

# Verificando o teste Qui-Quadrado entre o cruzamentos de variáveis a serem
# considerados

tab_nota_paulista <- table(cnpj$cnae,
                              cnpj$perc_credito)

qui2_nota_paulista <- chisq.test(tab_nota_paulista)
qui2_nota_paulista

# A ACM
ACM <- MCA(cnpj[, 2:3], method = "Indicador")

# Estabelecendo a Clusterização -------------------------------------------

# 1. Clustering
cluster_cnae <- kmeans(cnpj, centers = 2) 'CONFERIR SE É ASSIM'

# 2. Observando os resultados
fviz_cluster(cluster_cnae, data = cnpj)

plot <- fviz_cluster(cluster_cnae, data = cnpj)

View(plot)
