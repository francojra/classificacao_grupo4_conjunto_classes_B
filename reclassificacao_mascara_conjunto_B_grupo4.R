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

## Definir cores das classes

sits_colors_set(tibble(
  name = c("supressao", "veg_natural"),
  color = c("#bf812d", "#01665e")))

# Visualizar mapa 

plot(caatinga_class, 
     legend_text_size = 0.7)

# Criar cubo do mapa PRODES (Máscara) -----------------------------------------------------------------------------------------------------------

tempdir_r <- "cl_reclassification"
dir.create(tempdir_r, showWarnings = FALSE)
getwd()

### OBS.: Necessário renomear o nome da imagem .tif da pasta

### Redefinir valor de NA por 3

library(terra)

r <- rast("SENTINEL-2_MSI_034018_2020-01-01_2020-12-18_class_v2.tif")

unique(values(r)) # Vrificar pixls e máscara
plot(is.na(r), main = "Valores NA")
plot(r)

# Criar uma máscara lógica onde os valores são 1 ou 2
r_masked <- mask(r, r %in% c(1, 2, NA), maskvalue=FALSE)

# NA é a máscara em branco
plot(r_masked) 

r_mask <- subst(r, NA, 3)
r_mask[is.na(r_mask[])] <- 3
unique(values(r_mask)) # Verificar pixels após redefinir NA da máscara por 3
plot(r_mask) # desmatamento 2019 - 1; vegetacao até 2020 - 2

prodes_2020 <- sits_cube(
    source = "BDC",
    collection = "SENTINEL-2-16D",
    tiles      = "034018",
    data_dir = tempdir_r,
    parse_info = c("product", "sensor", 
                    "tile", "start_date", "end_date",
                    "band", "version"),
    bands = "class",
    version = "v2", # Versão do mapa PRODES para não confundir com mapa classificado
    labels = c("1" = "supressao", "2" = "veg_natural", "3" = "mascara"))

view(prodes_2020$labels)

## Definir cores:

sits_colors_set(tibble(
  name = c("supressao", "veg_natural", "mascara"),
  color = c("#543005", "#35978f", "white")))

plot(prodes_2020) # A supressão está dentro da máscara com alguns pequenos pontos fora

# Reclassificação --------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "cl_reclassification"
dir.create(tempdir_r, showWarnings = FALSE)
getwd()

caatinga_rec_2020 <- sits_reclassify(
    cube = caatinga_class,
    mask = prodes_2020,
    rules = list("vegetação natural" = mask == "veg_natural",
      "supressao 2000 - 2019" = mask == "supressao",
                 "supressao 2020" = cube == "supressao"),
    multicores = 1,
    output_dir = tempdir_r,
    version = "reclass_final2")

## Reclassifica apenas os dados da mascara que é o desmatamento antes de 2020
## Mantem a classificação de vegetacao e supressao 2020 do cubo, nao reclassifica
## Todos os dados de desmatamento até 2019 estão sob a máscara do PRODES (em branco).
## Fora da máscara aparecem os dados de vegetação e os novos dados de supressão de 2020.
## Após inserir a máscara, toda a classificação é feita apenas fora dela.

sits_colors_set(tibble(
  name = c("supressao 2020", "vegetação natural", "supressao 2000 - 2019"),
  color = c("#543005", "#35978f", "white")))

plot(caatinga_rec_2020,
     legend_text_size = 0.85)

# ### ---------------------------------------------------------------------

# Considerando mascaras nas áreas externas a NA -----------------------------

tempdir_r <- "cl_reclassification"
dir.create(tempdir_r, showWarnings = FALSE)
getwd()

mask_cube <- sits_cube(
  source = "BDC",
  collection = "SENTINEL-2-16D",
  data_dir = tempdir_r,
  parse_info = c("satellite", "sensor", "tile", "start_date", 
                 "end_date", "band", "version"),
  bands = "class",
  version = "v2", ##essa é a máscara com valor 3 na área dora da máscara 
  labels = c("1" = "Mascara_Supressao", 
             "2" = "Mascara_Vegetacao", 
             "3" = "Fora_Mascara"),
  tiles = "034018",
  start_date = "2020-01-01",
  end_date = "2020-12-31"
)

# Nova reclassificação para unir as classes da máscara
# Reclassificar mantendo as classes fora da máscara e unificando o interior como "Mascara"

class_map_final2 <- sits_reclassify(
  cube = caatinga_class,
  mask = mask_cube,  # máscara com 1, 2, e 3 (Fora_Mascara)
  rules = list(
    "Mascara"     = mask %in% c("Mascara_Supressao", "Mascara_Vegetacao"), #vai agrupar vegetação e supressão da máscara
    "supressao"   = mask == "Fora_Mascara" & cube == "supressao",
    "veg_natural" = mask == "Fora_Mascara" & cube == "veg_natural"
  ),
  output_dir = tempdir_r,
  version = "class_map_final7"
)


sits_colors_set(tibble::tibble(
  name  = c("supressao", "veg_natural", "Mascara"),
  color = c("#8E44AD", "#2ECC71", "#BDC3C7")  # violeta, verde, cinza
))


plot(class_map_final2, legend_text_size = 0.8)
