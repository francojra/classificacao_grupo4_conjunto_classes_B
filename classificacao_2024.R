# Criar cubo de dados - Sentinel 2 ---------------------------------------------------------------------------------------------------------

cubo_tile_034018 <- sits_cube(
  source     = "BDC", # Fonte dos cubos de dados
  collection = "SENTINEL-2-16D", # Coleção de imagens
  tiles      = "034018", # Região definida pelo Tile
  start_date = "2024-01-01", # Data inicial 
  end_date   = "2024-12-31") # Data final (período de 1 ano)

sits_bands(cubo_tile_034018)
sits_timeline(cubo_tile_034018)