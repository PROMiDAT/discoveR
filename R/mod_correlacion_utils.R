############################### Generar Código ################################
code.cor <- function(color1, color2, color3) {
  paste0(
    "colores <- c('", color1, "', '", color2, "', '", color3, "')\n",
    "datos.plot <- round(cor(datos), 3)\n",
    "datos.plot %>% e_charts() %>%\n",
    "  e_correlations(\n",
    "    order = 'hclust', label = list(show = T),\n",
    "    inRange = list(color = c(col_min, col_med, col_max)),\n",
    "    itemStyle = list(\n",
    "      borderWidth = 2\n",
    "      borderColor = '#fff'\n",
    "    )\n",
    "  ) %>% e_datazoom(show = F) %>% e_tooltip() %>% e_show_loading()\n"
  )
}