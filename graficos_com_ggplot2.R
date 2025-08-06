dados <- padroes_ts_samples_tile_034018_g4 %>%
  select(label, time_series) %>%
  unnest(time_series)


dados_long <- dados %>%
  pivot_longer(
    cols = c(B11, DBSI, NDII, NDVI),     # bandas a serem transformadas
    names_to = "banda",                  # nova coluna com o nome da banda
    values_to = "valor"                  # nova coluna com o valor da banda
  )

dados_long

ggplot(dados_long, aes(x = Index, y = valor, color = banda)) +
  geom_line(linewidth = 1.2) +
  theme_bw() +
  facet_wrap(~ label, labeller = labeller(label = c(
    "veg_natural" = "Vegetação Natural",
    "supressao" = "Supressão"
  ))) +
  labs(x = "Tempo", y = "Valores", color = "Banda") +
  theme(axis.text = element_text(color = "black", size = 9),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5),
        strip.text = element_text(face = "bold"))

