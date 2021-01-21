############################### Generar Código ################################
code.cor <- function(color1, color2, color3) {
  paste0(
    "colores <- list(list(0, '", color1, "'), list(0.5, '", color2, "'), list(1, '", color3, "'))\n",
    "label.js <- JS(\n",
    "  'function() {return Highcharts.numberFormat(this.point.value, 2);}'\n",
    ")\n\n",
    "hchart(var.numericas(datos)) %>% hc_chart(zoomType = 'xy') %>%\n",
    "  hc_exporting(enabled = T, filename = 'correlaciones') %>%\n",
    "  hc_colorAxis(stops = NULL) %>%\n",
    "  hc_colorAxis(stops = colores, min = -1, max = 1) %>%\n",
    "  hc_plotOptions(\n",
    "    series = list(\n",
    "      dataLabels = list(enabled = TRUE, formatter = label.js)\n",     
    "    )\n",
    "  )\n"
  )
}