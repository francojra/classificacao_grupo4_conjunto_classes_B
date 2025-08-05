# Script padrão ----------------------------------------------------------------------------------------------------------------------------
# Tile 034018 ------------------------------------------------------------------------------------------------------------------------------
# Classificação externa da máscara 2019 ----------------------------------------------------------------------------------------------------
# Classificação do desmatamento 2020 -------------------------------------------------------------------------------------------------------
# Autoras do script: Jeanne e Daiany -------------------------------------------------------------------------------------------------------
# Data: 31/07/25 ---------------------------------------------------------------------------------------------------------------------------

# Configurações e pacotes ------------------------------------------------------------------------------------------------------------------

library(torch)
torch::install_torch()
library(luz)
library(sits)
#library(sitsdata)
library(tibble)

# Estabelecer diretório dos mapas classificado e PRODES

tempdir_r <- "map_classificado"
dir.create(tempdir_r, showWarnings = FALSE)
getwd() # O diretório deve apresentar a pasta acima criada

# Criar cubo do mapa classificado ----------------------------------------------------------------------------------------------------------

# Definir cores do mapa classificado

sits_colors_set(tibble(
  name = c("supressao", "veg_natural"),
  color = c("brown", "#01665e")))

# Gerar cubo do mapa classificado

caatinga_class <- sits_cube(
    source = "BDC",
    collection = "SENTINEL-2-16D",
    data_dir = tempdir_r, # A imagem classificada deve estar nesta pasta
    parse_info = c("satellite", "sensor", 
                   "tile", "start_date", "end_date",
                   "band", "version"),
    bands = "class",
    labels = c("1" = "supressao", # Definir os pixels da imagem
               "2" = "veg_natural"))

# Visualizar mapa 

plot(caatinga_class, 
     legend_text_size = 0.7)

# Criar cubo do mapa PRODES (Máscara) -----------------------------------------------------------------------------------------------------------

tempdir_r <- "cl_reclassification"
dir.create(tempdir_r, showWarnings = FALSE)
getwd()

### OBS.: Necessário renomear o nome da imagem .tif da pasta

prodes_2020 <- sits_cube(
    source = "BDC",
    collection = "SENTINEL-2-16D",
    tiles      = "034018",
    data_dir = tempdir_r,
    parse_info = c("product", "sensor", 
                    "tile", "start_date", "end_date",
                    "band", "version"),
    bands = "class",
    version = "v2", # Versão do mapa PRODES
    labels = c("1" = "supressao", "2" = "veg_natural"))

plot(prodes_2020) # A supressão está dentro da máscara

sits_view(prodes_2020)

# Verificar pixels e valores do mapa PRODES -------------------------------

library(terra)

## Necessário definir o diretório onde está a imagem:

r <- rast("SENTINEL-2_MSI_034018_2020-01-01_2020-12-18_class_v2.tif")
unique(values(r))

# Resultados dos valores 

# [1,]  NA   # áreas sem dados (geralmente fora do recorte ou mascaradas). 
## Máscara PRODES, área excluída da análise/classificação.
# [2,]   2   # provavelmente "Vegetacao"/ Vegetação remanescente ou secundária
# [3,]   1   # provavelmente "Supressao"

# Mapear a máscara do PRODES

plot(is.na(r), main = "Áreas mascaradas (NA)")
plot(r) # Aqui a supressao não aparece devido ela estar dentro da máscara
plot(!is.na(r))

# Reclassificação --------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "cl_reclassification"
dir.create(tempdir_r, showWarnings = FALSE)
getwd()

caatinga_rec_2020 <- sits_reclassify(
    cube = caatinga_class,
    mask = prodes_2020,
    rules = list("Mascara" = mask %in% c("NA"),
    memsize = 10,
    multicores = 3,
    output_dir = tempdir_r,
    version = "reclass")

## Todos os dados de desmatamento até 2019 estão sob a máscara do PRODES (em branco).
## Fora da máscara aparecem os dados de vegetação e os novos dados de supressão de 2020.
## Após inserir a máscara, toda a classificação é feita apenas fora dela.

plot(caatinga_rec_2020,
     legend_text_size = 0.7)
