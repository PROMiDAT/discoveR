############################### Generar Código ################################
code.disp.2d <- function(vars, color) {
  res <- paste0(
    "datos.plot <- data.frame(x = datos[['", vars[1], "']], y = datos[['", vars[2], "']])\n\n",
    "hchart(datos.plot, 'point', hcaes(x = x, y = y), color = '", color, "') %>%\n",
    "  hc_chart(zoomType = 'xy') %>% hc_xAxis(title = list(text = '", vars[1], "')) %>%\n",
    "  hc_yAxis(title = list(text = ", vars[2], ")) %>%\n",
    "  hc_tooltip(\n",
    "    pointFormat = paste0('", vars[1], "', ': {point.x}<br>', '", vars[2], "', ': {point.y}'),\n",
    "    headerFormat = ''\n",     
    "  ) %>% hc_exporting(enabled = T, filename = 'dispersion')\n"
  )
}

code.disp.3d <- function(vars, color) {
  res <- paste0(
    "datos.plot <- data.frame(x = datos[['", vars[1], "']], y = datos[['", vars[2], "']], z = datos[['", vars[3], "']])\n\n",
    "plot_ly(\n",
    "  datos.plot, x = ~x, y = ~y, z = ~z, type = 'scatter3d',\n",
    "  mode = 'markers', marker = list(color = '", color, "'),\n",
    "  hovertemplate = paste0(\n",
    "    '", vars[1], "', ': %{x:}<br>', '", vars[2], "', ': %{y:}<br>',\n",
    "    '", vars[3], "', ': %{z:}<extra></extra>'\n",
    "  )) %>% config(displaylogo = F) %>%\n",
    "  layout(paper_bgcolor = 'black', scene = list(\n",
    "    xaxis = list(title = '", vars[1], "', gridcolor = 'white'),\n",
    "    yaxis = list(title = '", vars[2], "', gridcolor = 'white'),\n",
    "    zaxis = list(title = '", vars[3], "', gridcolor = 'white')))\n"
  )
}