# Criação de cubo de dados, Self-Organizing Maps (SOM) e Classificação  --------------------------------------------------------------------
# Teste de Amostras  -----------------------------------------------------------------------------------------------------------------------
# Tile 034018 ------------------------------------------------------------------------------------------------------------------------------
# Conjunto de classes: grupo B --------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
#library(sitsdata) # Pacote para obter conjunto de dados de amostras
library(kohonen) # Pacote para plotar o mapa SOM

# Estabelecer diretório de trabalho  -------------------------------------------------------------------------------------------------------

## Estabelecer um diretório de trabalho (pasta do computador com seus arquivos)
### 1ª Opção:
setwd("C:/caminho/da/sua/pasta")  

### 2ª opção (manual):
# Session > Set Working Directory > Choose Directory

## Conferir o caminho do diretório de trabalho definido:
getwd() 

# Criar cubo de dados - Sentinel 2 ---------------------------------------------------------------------------------------------------------

cubo_tile_034018 <- sits_cube(
  source     = "BDC", # Fonte dos cubos de dados
  collection = "SENTINEL-2-16D", # Coleção de imagens
  tiles      = "034018", # Região definida pelo Tile
  start_date = "2020-01-01", # Data inicial 
  end_date   = "2020-12-31") # Data final (período de 1 ano)

# Salvar arquivo do cubo em formato rds ----------------------------------------------------------------------------------------------------

## Salvando os dados, não precisa gerar o cubo das amostras novamente

saveRDS(cubo_tile_034018, file = "cubo_tile_034018.rds") 
cubo_tile_034018 <- readRDS("cubo_tile_034018.rds")

# Calcular índices NDII e DBSI ----------------------------------------------------

tempdir_r <- "cube_operations"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

cubo_tile_034018_ndii <- sits_apply(cubo_tile_034018,
                             NDII = (B08 - B11) / (B08 + B11),
                             normalized = FALSE,
                             output_dir = tempdir_r,
                             progress = TRUE
)

sits_bands(cubo_tile_034018_ndii)

cubo_tile_034018_dbsi <- sits_apply(cubo_tile_034018_ndii,
                               DBSI = ((B11 - 1) - B03) / ((B11 - 1) + B03) - NDVI,
                               normalized = FALSE,
                               output_dir = tempdir_r,
                               progress = TRUE
)

sits_bands(cubo_tile_034018_dbsi)

## Salvar cubo com novos índices do grupo 4

saveRDS(cubo_tile_034018_dbsi, file = "cubo_tile_034018_g4.rds") 
cubo_tile_034018_g4 <- readRDS("cubo_tile_034018_g4.rds")

# Verificar informações sobre o cubo -------------------------------------------------------------------------------------------------------

view(cubo_tile_034018_g4) # Tabela com principais informações do cubo
view(cubo_tile_034018_g4$file_info[[1]]) # Tabela das informações contidas na coluna file_info

sits_bands(cubo_tile_034018_g4)
sits_timeline(cubo_tile_034018_g4)

# Cubo com amostras da máscara -------------------------------------------------------------------------------------------------------------

cubo_samples_tile_034018_g4 <- sits_get_data(
  cubo_tile_034018_g4, # Cubo geral com bandas e índices selecionados para o tile 034018
  samples = "Tile_034018_amostras_classificacao123_treinadas_manual_classes_B.shp", # Arquivo shapefile do tile 034018
  label_attr = "Classe", # Coluna que indica as classes das amostras (pontos)
  bands = c("B11", "DBSI", "NDII", "NDVI"), 
  memsize = 7,
  multicores = 2, # Número de núcleos a serem usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Esse cubo foi criado com amostras da máscara que pertencem ao tile vizinho 034018.
## As amostras são de supressão da vegetação e áreas vegetadas.

# Salvar arquivo do cubo com amostras ------------------------------------------------------------------------------------------------------

saveRDS(cubo_samples_tile_034018_g4, file = "cubo_samples_tile_034018_g4.rds") 
cubo_samples_tile_034018_g4 <- readRDS("cubo_samples_tile_034018_g4.rds")

sits_bands(cubo_samples_tile_034018_g4)

## Visualizar amostras no Google Earth

sits::sits_view(cubo_samples_tile_034018_g4)

## Visualizar tabelas e mapas

view(cubo_samples_tile_034018_g4) # Visualizar tabela das amostras
plot(cubo_samples_tile_034018_g4) 

## Visualizar padrões de séries temporais de cada classe

padroes_ts_samples_tile_034018_g4 <- sits_patterns(cubo_samples_tile_034018_g4) # Média harmônica das séries temporais com curva suavizada
view(padroes_ts_samples_tile_034018_g4$time_series[[1]])
p <- plot(padroes_ts_samples_tile_034018_g4)

library(ggplot2)

p + geom_line(linewidth = 1.2) + 
  theme_bw()

# Balanceamento de amostras ----------------------------------------------------------------------------------------------------------------

## A diferença entre classes cria um desequilíbrio que pode afetar negativamente o desempenho do seu
## modelo de classificação — particularmente a capacidade do modelo de identificar corretamente a 
## classe minoritária, que, neste caso, são as áreas sem vegetação.

## Modelos supervisionados tendem a favorecer a classe majoritária durante o treinamento. 
## Isso significa que:

## - O modelo pode alcançar alta acurácia geral, mas com baixa sensibilidade/recall para a classe 
## minoritária.

## - Pode subestimar a presença de áreas sem vegetação, o que pode ser crítico dependendo da sua 
## aplicação (ex: degradação, desmatamento, uso do solo etc.).

## Reduzir desigualdade no número de classes

library(FNN)

cubo_samples_tile_034018_bal_g4 <- sits_reduce_imbalance(
  cubo_samples_tile_034018_g4,
  n_samples_over = 170, 
  n_samples_under = 190) 

summary(cubo_samples_tile_034018_bal_g4)

# Gerar SOM do tile 034018 -----------------------------------------------------------------------------------------------------------------

## Definir cores das classes

cores <- sits_colors()
view(cores)

sits_colors_set(tibble(
  name = c("Supressao", "Veg_Natural"),
  color = c("#bf812d", "#01665e")))

# Clustering de séries temporais - SOM

som_cluster_034018_g4 <- sits_som_map(
  data = cubo_samples_tile_034018_bal_g4, # SOM feito com o nosso grupo de amostras 
  grid_xdim = 10, # Grade eixo x. Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 10, # Grade eixo y
  distance = "dtw", # Método de calcular a distância,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  rlen = 20) # Número de iterações (quantidade de vezes que o mapa é gerado)

# Gerar mapa SOM ---------------------------------------------------------------------------------------------------------------------------

view(som_cluster_034018_g4$data) # Tabela com coordenadas, classes, id das amostras e id neurônios

windows(width = 10, height = 6) # Abranger janela do windows para ver o gráfico

plot(som_cluster_034018_g4, band = "DBSI")
plot(som_cluster_034018_g4, band = "NDVI")
plot(som_cluster_034018_g4, band = "B11")
plot(som_cluster_034018_g4, band = "NDII")

# Seleção de neurônios no SOM --------------------------------------------------------------------------------------------------------------

samples_filt_034018_g4 <- som_cluster_034018_g4$data[som_cluster_034018_g4$data$id_neuron == 25, ]
view(samples_filt_034018_g4)

# Verificar a quantidade de amostras para cada classe --------------------------------------------------------------------------------------

summary(cubo_samples_tile_034018_g4) # Sem balanceamento
summary(cubo_samples_tile_034018_bal_g4) # Com balanceamento

## Resultado do balanceamento:

## - A diferença entre as classes é pequena (apenas 22 amostras).

## - Ambas as classes têm número suficiente de amostras (acima de 150) para que o SOM e o modelo 
## supervisionado consigam aprender padrões relevantes.

## - A proporção está próxima de 50/50, o que é ideal para modelos que são sensíveis ao 
## desequilíbrio.

# Detectar ruídos das amostras -------------------------------------------------------------------------------------------------------------

all_samples_034018_g4 <- sits_som_clean_samples(som_map = som_cluster_034018_g4, 
                                             keep = c("clean", "analyze", "remove"))
plot(all_samples_034018_g4)
summary(all_samples_034018_g4) # Mesma quantidade de amostras balanceadas

# Salvar tabela das amostras do SOM 

saveRDS(all_samples_034018_g4, file = "all_samples_som_034018.rds")
view(all_samples_034018_g4)

# Remover amostras ruidosas ----------------------------------------------------------------------------------------------------------------

samples_clean_034018_g4 <- 
  sits_som_clean_samples(som_cluster_034018_g4,
                         keep = c("clean", "analyze"))

view(samples_clean_034018_g4)
view(samples_clean_034018_g4$time_series)

plot(samples_clean_034018_g4)

summary(samples_clean_034018_g4) # Nova quantidade de amostras entre as classes

## O sits_som_clean_samples retira as amostras outliers que foram confundidas. 
## É necessário ter cautela com a função para não retirar muitas amostras de cada classe.

# Salvar tabela das amostras do SOM limpo

saveRDS(samples_clean_034018_g4, file = "samples_clean_034018_g4.rds")
view(samples_clean_034018_g4)

# Ver diferenças na quantidade de amostras antes e após a remoção --------------------------------------------------------------------------

summary(all_samples_034018_g4)
summary(samples_clean_034018_g4) # Manteve boa proporção entre as classes

# Gerar SOM dos dados sem ruídos -----------------------------------------------------------------------------------------------------------

som_cluster_limpo_034018_g4 <- sits_som_map(
  data = samples_clean_034018_g4, # SOM feito com o nosso grupo de amostras 
  grid_xdim = 10, # Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 10,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  distance = "dtw", # Método para calcular a distância
  rlen = 20) # Número de iterações

windows(width = 10, height = 6)

plot(som_cluster_limpo_034018_g4, band = "DBSI")
plot(som_cluster_limpo_034018_g4, band = "NDVI")
plot(som_cluster_limpo_034018_g4, band = "B11")
plot(som_cluster_limpo_034018_g4, band = "B04")

# Avaliar matriz de confusão das amostras antes e após limpeza -----------------------------------------------------------------------------

# Função de avaliação

avaliacao_som_034018_g4 <- sits_som_evaluate_cluster(som_cluster_034018)
avaliacao_som_limpo_034018_g4 <- sits_som_evaluate_cluster(som_cluster_limpo_034018)

# Gráficos

plot(avaliacao_som_034018_g4)
avaliacao_som_034018_g4

plot(avaliacao_som_limpo_034018_g4)
avaliacao_som_limpo_034018_g4

# Classificações ---------------------------------------------------------------------------------------------------------------------------

# Leitura de dados para classificação ------------------------------------------------------------------------------------------------------



## Selecionar bandas e datas do cubo de dados principal sem as amostras

cubo_select_tile_034018_g4 <- sits_select(data = cubo_tile_034018_g4,
                                       bands = c("B11", "DBSI", "NDII", "NDVI"))

view(cubo_select_tile_034018_g4$file_info)

# Treinar modelo Random Forest -------------------------------------------------------------------------------------------------------------

## Treinar modelo Random Forest das amostras limpas

set.seed(03024)

rf_model_034018_g4 <- sits_train(
  samples = samples_clean_034018_g4, # Treinamento de amostras limpas do tile 034018
  ml_method = sits_rfor()) # Modelo Random Forest

## Gráfico com as variávies mais importantes do modelo

plot(rf_model_034018_g4)

# Produzir mapa de probabilidades de classes -----------------------------------------------------------------------------------------------

tempdir_r <- "mapa_probabilidades_034018_g4"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

probs_034018_g4 <- sits_classify(
  data = cubo_select_tile_034018_g4, # Cubo principal com bandas e índices selecionados (sem amostras)
  ml_model = rf_model_034018_g4,
  multicores = 4,
  memsize = 15,
  output_dir = tempdir_r)

## Salvar dados do cubo de probabilidades

saveRDS(probs_034018_g4, file = "probs_034018_g4.rds")
probs_034018_g4 <- readRDS("probs_034018_g4.rds")

view(probs_034018_g4$file_info)

## Visualizar mapas de probabilidades

plot(probs_034018_g4, labels = "Supressao", palette = "YlOrRd")
plot(probs_034018_g4, labels = "Veg_Natural", palette = "Greens")

# Suavização do mapa de probabilidades -----------------------------------------------------------------------------------------------------

smooth_034018_g4 <- sits_smooth(
  cube = probs_034018_g4,
  multicores = 4,
  memsize = 14,
  output_dir = tempdir_r
)

## Salvar dados do cubo suavizado

saveRDS(smooth_034018_g4, file = "smooth_034018_g4.rds")
smooth_034018_g4 <- readRDS("smooth_034018_g4.rds")

plot(smooth_034018_g4, labels = "Supressao", palette = "YlOrRd")
plot(smooth_034018_g4, labels = "Veg_Natural", palette = "Greens")

# Rotulando o cubo de probabilidades - Classificações finais de amostras -------------------------------------------------------------------

map_class_034018_g4 <- sits_label_classification(
  cube = smooth_034018_g4, 
  output_dir = tempdir_r, 
  memsize = 14,
  multicores = 5
)

## Salvar dados do cubo classificado

saveRDS(map_class_034018_g4, file = "map_class_034018_g4.rds")
map_class_034018_g4 <- readRDS("map_class_034018_g4.rds")

plot(map_class_034018_g4,
     legend = c("Supressao" = "#bf812d",
                "Veg_Natural" = "#01665e"))

## Visualização dos dados em mapa interativo

sits_view(cubo_tile_034018_g4)

# Validação do modelo Random Forest --------------------------------------------------------------------------------------------------------

## Avaliação com validação cruzada (k-fold)

### Amostras originais

# set.seed(976) # Gera o mesmo resultado da validação a cada rodada
# 
# rfor_valid_034018_g4 <- sits_kfold_validate(
#   samples    = all_samples_034018_g4,
#   folds      = 5, # Número divisões do grupo, nesse caso em 5 conjuntos para treino e teste
#   ml_method  = sits_rfor(),
#   multicores = 5
# )
# 
# rfor_valid_034018_g4

### Amostras limpas

set.seed(5437214) # Gera o mesmo resultado da validação a cada rodada

rfor1_valid_034018_g4 <- sits_kfold_validate(
  samples    = samples_clean_034018_g4,
  folds      = 5, # Número divisões do grupo, nesse caso em 5 conjuntos para treino e teste
  ml_method  = sits_rfor(),
  multicores = 5
)

rfor1_valid_034018_g4

# Mapa de incerteza ------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mapa_incerteza_034018_g4"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_incerteza_034018_g4 <- sits_uncertainty(
  cube = probs_034018_g4, # Arquivo do cubo de probabilidades, resultado da sits_classify()
  type = "margin",
  output_dir = tempdir_r,
  memsize = 12,
  multicores = 4,
  progress = TRUE)

## Salvar dados do mapa de incerteza

saveRDS(map_incerteza_034018_g4, file = "map_incerteza_034018_g4.rds")
map_incerteza_034018_g4 <- readRDS("map_incerteza_034018_g4.rds")

plot(map_incerteza_034018_g4, palette = "PRGn")
