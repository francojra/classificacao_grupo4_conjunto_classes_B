
# Pacotes -----------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
#library(sitsdata) # Pacote para obter conjunto de dados de amostras
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer)
# library(torch)
# torch::install_torch()

# Criar cubo de dados - Sentinel 2 ---------------------------------------------------------------------------------------------------------

cubo_tile_034018_2024 <- sits_cube(
  source     = "BDC", # Fonte dos cubos de dados
  collection = "SENTINEL-2-16D", # Coleção de imagens
  tiles      = "034018", # Região definida pelo Tile
  start_date = "2020-01-01", # Data inicial 
  end_date   = "2024-12-31") # Data final (período de 1 ano)

sits_bands(cubo_tile_034018_2024)
sits_timeline(cubo_tile_034018_2024)

saveRDS(cubo_tile_034018_2024, file = "cubo_tile_034018_2024.rds") 
cubo_tile_034018_2024 <- readRDS("cubo_tile_034018_2024.rds")

# Criar cubo de dados das amostras - Grupo 2 -----------------------------------------------------------------------------------------------

## Grupo 2: bandas "B03", "B04", "B08", "B11"

cubo_caatinga_samples_g2_2024 <- sits_get_data(
  cubo_tile_034018_2024, # Cubo geral criado acima para o tile 034018
  samples = "Tile_034018_amostras_classificacao123_treinadas_manual_classes_B.shp", # Arquivo shapefile da pasta do diretório de trabalho
  label_attr = "classe_b", # Coluna que indica as classes das amostras (pontos)
  multicores = 4,
  bands = c("B11"), # Seleção das bandas que vão ser utilizadas
  progress = TRUE) # Acompanhar carregamento
