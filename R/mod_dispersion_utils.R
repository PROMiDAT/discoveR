############################### Generar Código ################################
code.disp.2d <- function(vars, color) {
  res <- paste0(
    "datos.plot <- data.frame(x = datos[['", vars[1], "'], y = datos[['", vars[2], "']],\n",
    "                         id = row.names(datos))\n\n",
    "datos.plot %>% e_charts(x) %>% e_scatter(y, bind = id, symbol_size = 10) %>%\n",
    "  e_x_axis(x) %>% e_y_axis(y) %>% e_datazoom(show = F) %>%\n",
    "  e_color('", color, "') %>% e_axis_labels(x = '", vars[1], "', y = '", vars[2], "') %>%\n",
    "  e_tooltip(formatter = JS(\n",
    "    \"function(params) {\n",
    "       return('<b>' + params.name + '</b><br/>", vars[1], ": ' + params.value[0] + '<br/>", vars[2], ": ' + params.value[1])\n",
    "    }\")\n",
    "  ) %>% e_legend(F) %>% e_show_loading()\n"
  )
}

code.disp.3d <- function(vars, color) {
  res <- paste0(
    "datos.plot <- data.frame(x = datos[['", vars[1], "'], y = datos[['", vars[2], "']],\n",
    "                         z = datos[['", vars[3], "']], id = row.names(datos))\n\n",
    "datos.plot %>% e_charts(x) %>% e_scatter_3d(y, z, bind = id) %>% e_color('", color, "') %>%\n",
    "  e_x_axis_3d(name = '", vars[1], "', axisLine = list(lineStyle = list(color = 'white'))) %>%\n",
    "  e_y_axis_3d(name = '", vars[2], "', axisLine = list(lineStyle = list(color = 'white'))) %>%\n",
    "  e_z_axis_3d(name = '", vars[3], "', axisLine = list(lineStyle = list(color = 'white'))) %>%\n",
    "  e_tooltip(formatter = JS(\n",
    "    \"function(params) {\n",
    "       return('<b>' + params.name + '</b><br/>", vars[1], 
    ": ' + params.value[0] + '<br/>", vars[2], ": ' + params.value[1] + '<br/>", vars[3], ": ' + params.value[2])\n",
    "    }\")\n",
    "  ) %>% e_theme('dark') %>% e_show_loading()\n"
  )
}