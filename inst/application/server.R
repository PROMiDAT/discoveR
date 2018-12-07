# Server Script

shinyServer(function(input, output, session) {
  source('global.R', local = T)
  load("www/translation.bin")
  options(shiny.maxRequestSize=200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      scrollX = TRUE, language = list(
        search = HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = HTML('<i class="fa fa-backward"></i>'),
          "next" = HTML('<i class="fa fa-forward"></i>'),
          "first" =HTML('<i class="fa fa-fast-backward"></i>'), 
          "last" = HTML('<i class="fa fa-fast-forward"></i>')))
    )
  )

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    recover.cat()
    stopApp()
  })
  
  #' Carga Inicial
  #' @author Diego
  #' @return functions
  #' @export
  #'
  updateAceEditor(session, "fieldCodeResum", "summary(datos)")
  updateAceEditor(session, "fieldModelCor", modelo.cor())
  updateAceEditor(session, "fieldFuncJambu",
    paste0(extract.code("lead"), "\n", extract.code("codo.jambu")))
  updateAceEditor(session, "fieldFuncNum", extract.code("distribucion.numerico"))
  updateAceEditor(session, "fieldFuncCat", extract.code("distribucion.categorico"))
  updateAceEditor(session, "fieldCodeCentr", extract.code("calc.centros"))
  updateAceEditor(session, "fieldFuncHoriz", extract.code("centros.horizontal.todos"))
  updateAceEditor(session, "fieldFuncVert", extract.code("centros.vertical.todos"))
  updateAceEditor(session, "fieldFuncRadar",
    paste0(extract.code("coord_radar"), "\n", extract.code("centros.radar")))
  updateAceEditor(session, "fieldFuncKhoriz", extract.code("centros.horizontal.todos"))
  updateAceEditor(session, "fieldFuncKvert", extract.code("centros.vertical.todos"))
  updateAceEditor(session, "fieldFuncKradar", 
    paste0(extract.code("coord_radar"), "\n", extract.code("centros.radar")))
  
  updateData <- reactiveValues(
    datos = NULL, cor.modelo = NULL, pca.modelo = NULL,
    hc.modelo = NULL, k.modelo = NULL)

  updatePlot <- reactiveValues(
    calc.normal=default.calc.normal(), normal=NULL, disp=NULL, pca.ind=NULL,
    pca.var=NULL, pca.bi=NULL, cor=NULL, pca.cvc=NULL, mapa=NULL, dya.num=NULL,
    dya.cat=NULL, diag=NULL, horiz=NULL, vert=NULL, radar=NULL, cat=NULL,
    jambu=NULL, kmapa=NULL, khoriz=NULL, kvert=NULL, kradar=NULL, kcat=NULL)

  disp.ranges <- reactiveValues(x = NULL, y = NULL)
  ind.ranges <- reactiveValues(x = NULL, y = NULL)
  bi.ranges <- reactiveValues(x = NULL, y = NULL)
  mapa.ranges <- reactiveValues(x = NULL, y = NULL)
  kmapa.ranges <- reactiveValues(x = NULL, y = NULL)

  observe({
    updateMenu(init = T)
    close.menu()
  })
  
  #' Load Button Function
  #' @author Diego
  #' @return functions
  #' @export
  #'
  observeEvent(input$loadButton, {
    codigo.carga <- ""
    codigo.na <- ""
    tryCatch({
      codigo.carga <- code.carga(
        nombre.filas = input$rowname, ruta = input$file1$datapath,
        separador = input$sep, sep.decimal = input$dec,
        encabezado = input$header)
      datos.originales <<- isolate(eval(parse(text = codigo.carga)))
      if(ncol(datos.originales) <= 1){
        showNotification(
          paste0("ERROR: Check Separators"), duration = 10, type = "error")
        return(NULL)
      }
      if(any(is.na(datos.originales))) {
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA))
        isolate(eval(parse(text = codigo.na)))
        nombre.datos <<- paste0(stringi::stri_extract_first(
          str = input$file1$name, regex = ".*(?=\\.)"), 
          ifelse(input$deleteNA, ".sinNA.", ".conNA.")
        )
      } else {
        nombre.datos <<- stringi::stri_extract_first(
          str = input$file1$name, regex = ".*(?=\\.)")
      }
      datos.reporte[[nombre.datos]] <<- datos.originales
      init.replist(nombre.datos)
      updateData$datos <- datos.originales
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
       updateData$datos <- NULL
       datos.originales <<- NULL
       return(NULL)
    })
    updateAceEditor(session, "fieldCodeData",
                    value = paste0(codigo.carga, "\n", codigo.na))
  })

  #' Transform Button Function
  #' @author Diego
  #' @return functions
  #' @export
  #'
  observeEvent(input$transButton, {
    code.res <- ""
    datos <- datos.originales
    rep.vars <- c()
    for (var in colnames(datos.originales)) {
      if(input[[paste0("box", var, contador)]]) {
        rep.vars <- c(rep.vars, paste0(
          var, ":", input[[paste0("sel", var, contador)]]))
        if(input[[paste0("sel", var, contador)]] == "categorico" &
           class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "numerico" &
           !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "disyuntivo") {
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        code.res <- paste0(code.res, code.desactivar.aux(var), "\n")
      }
    }
    
    nombre.datos <<- paste0(nombre.datos, paste(rep.vars, collapse = "."))
    datos.reporte[[nombre.datos]] <<- datos
    init.replist(nombre.datos)
    createLog(nombre.datos, "basico", "reptransformar", 
              paste0(code.res, "\nstr(datos)"))
    isolate(eval(parse(text = code.res)))
    updateAceEditor(session, "fieldCodeTrans", value = code.res)
    updateData$datos <- datos
  })
  
  ###################################  Update  ################################
  #' Update on Language
  #' @author Diego
  #' @return functions
  #' @export
  #'
  observeEvent(input$idioma, {
    updateLabelInput(
      session, 
      c("idioma", "selidioma", "data", "basico", "resumen", "normalidad",
        "dispersion", "distribucion", "correlacion", "acp", "jerarquico",
        "kmedias", "reporte", "acercade", "cargar", "trans", "resumen", 
        "resumenvar", "normalidad", "plotnormal", "numericas", "categoricas", 
        "resultados", "individuos", "variables", "sobreposicion", "ayudacp", 
        "vee", "cci", "ccv", "cvc", "cp1", "cp2", "inercia", "dendograma", 
        "mapa", "horizontal", "vertical", "radar", "interpretacioncat", 
        "jambu", "tituloreporte", "codreporte",  "salida", "copyright", "info",
        "version", "opciones", "ejecutar", "selcolor", "selcolores", "selejes",
        "cargarchivo", "subir", "header", "Rownames", "separador", "puntocoma",
        "coma", "tab", "punto", "separadordec", "eliminana", "centrar", "si", 
        "no", "descargar", "aplicar", "agregarcluster", "selvar", "selmetodo",
        "seltipo", "cantcluster", "algoritmo", "nstart", "niter", "numerodim",
        "cosind", "cosvar", "titulo", "nombre", "codigo", "metododist", 
        "kiter", "buscar", "nodata", "anterior", "siguiente", "primero", 
        "ultimo", "numerico", "categorico", "codedist", "codecentros", 
        "codehoriz", "codevert", "coderadar", "codejambu", "inercia",
        "inerciainter", "inerciaintra", "todos", "fisher", "asimetria", 
        "selvars")
    )
    
    updateinitSelects("selHoriz", 1:input$cant.kmeans.cluster)
    updateinitSelects("sel.Khoriz", 1:input$cant.kmeans.cluster)
    updateinitSelects("selVert", colnames(var.numericas(datos)))
    updateinitSelects("sel.Kvert", colnames(var.numericas(datos)))
    updatePlot$pca.cvc <- code.pca.cvp(input$cvc.metodo, tr("cvc"))
    updatePlot$normal <- default.normal(
      "datos", input$sel.normal, input$col.normal, tr("curvanormal"))
    updatePlot$dya.cat <- def.code.cat(
      variable = input$sel.distribucion.cat,
      titulox = tr("cantidadcasos"), tituloy = tr("categorias"))
    updatePlot$calc.normal <- default.calc.normal(
      labelsi = tr("positivo"), labelno = tr("negativo"), 
      labelsin = tr("sinasimetria"))
    updatePlot$jambu <- def.code.jambu(
      k = input$iteracionesK, tituloy = tr("inerciainter"))
  })
  
  #' Update on Data
  #' @author Diego
  #' @return functions
  #' @export
  #'
  observeEvent(updateData$datos, {
    datos <<- updateData$datos
    updateMenu(datos)
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      updateData$cor.modelo <- correlacion
      output$txtcor <- renderPrint(print(correlacion))
      updateSelects(datos)
      nmax <- calc.maxK(datos)
      updateSliderInput(session, "iteracionesK", max = nmax, value = nmax)
    }, error = function(e) {
      print(paste0("ERROR: ", e))
      return(datos <- NULL)
    })
    
    output$contents = DT::renderDataTable(mostrarData())
    close.menu(is.null(datos))
  })
  
  #' Update on PCA
  #' @author Diego
  #' @return functions
  #' @export
  #'
  observeEvent(c(updateData$datos, input$switch.scale, input$slider.npc), {
    datos <- updateData$datos
    tryCatch({
      if(!is.null(datos)) {
        codigo <- def.pca.model(
          scale.unit = input$switch.scale, npc = input$slider.npc)
        updateAceEditor(session, "fieldCodePCAModelo", value = codigo)
        isolate(eval(parse(text = codigo)))
        updateData$pca.modelo <- pca.modelo
        output$txtpca <- renderPrint(print(unclass(pca.modelo)))
        updateSliderTextInput(session, "slider.ejes", choices =
                                c(1:input$slider.npc), selected = c(1,2))
        createLog(
          nombre.datos, "acp", "vacio", codigo, params = 
            paste("centrar=", input$switch.scale, 
                  "repdim=", input$slider.npc, 
                  collapse = "."))
      }
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Actualización del Modelo Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(updateData$datos, input$cant.cluster, input$sel.dist.method,
                 input$sel.hc.method), {
    codigo <- def.model(
      data = "datos", cant = input$cant.cluster,
      dist.method = input$sel.dist.method, hc.method = input$sel.hc.method)
    tryCatch ({
      if(!is.null(datos) && !is.null(input$cant.cluster)){
        isolate(eval(parse(text = codigo)))
        updateData$hc.modelo <- hc.modelo
        updateAceEditor(session, "fieldCodeModelo", value = codigo)
        output$txthc <- renderPrint(print(unclass(hc.modelo)))
        output$txtcentros <- renderPrint(print(unclass(centros)))
        createLog(
          nombre.datos, "rephc", "vacio", codigo, params = 
            paste("Clusters=", input$cant.cluster, 
                  "distancia=", input$sel.dist.method,
                  "metodo=", input$sel.hc.method, collapse = "."))
      }
    }, error = function(e) {
      print(paste0("ERROR: ", e))
      return(NULL)
    })

    if(!is.null(datos) && !is.null(input$cant.cluster)) {
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      color <- ifelse(input$selHoriz %in% c("", "todos"), "red",
                      nuevos.colores[as.numeric(input$selHoriz)])
      updatePlot$diag <- diagrama(input$cant.cluster, nuevos.colores)
      updatePlot$mapa <- cluster.mapa(nuevos.colores)
      updatePlot$horiz <- cluster.horiz(input$selHoriz, nuevos.colores, color)
      updatePlot$vert <- cluster.vert(input$selVert, nuevos.colores)
      updatePlot$radar <- cluster.radar(nuevos.colores)
      updatePlot$cat <- cluster.cat(input$selBar, nuevos.colores)
      updateAceEditor(session, "fieldCodeDendo", value = updatePlot$diag)
      updateAceEditor(session, "fieldCodeMapa", value = updatePlot$mapa)
      updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
      updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
      updateAceEditor(session, "fieldCodeRadar", value = updatePlot$radar)
      updateAceEditor(session, "fieldCodeBar", value = updatePlot$cat)
    }
  })
  
  #' Actualizacion del Modelo K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(updateData$datos, input$cant.kmeans.cluster,
                 input$num.iter, input$num.nstart, input$sel.algoritmo), {
    tryCatch ({
      codigo <- def.k.model(
        data = "datos", cant = input$cant.kmeans.cluster,  
        iter.max = input$num.iter, nstart = input$num.nstart,
        algorithm = input$sel.algoritmo)
      isolate(eval(parse(text = codigo)))
      updateData$k.modelo <- k.modelo
      updateAceEditor(session, "fieldCodeKModelo", value = codigo)
      output$txtk <- renderPrint(print(unclass(k.modelo)))
      createLog(
        nombre.datos, "kmedias", "vacio", codigo, 
        params = paste(
          "Clusters=", input$cant.kmeans.cluster, 
          "iter=", input$num.iter, "nstart=", input$num.nstart,
          "algoritmo=", input$sel.algoritmo, collapse = "."))
    }, error = function(e) {
      return(NULL)
    })

    if(!is.null(datos) && !is.null(input$cant.kmeans.cluster)) {
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      color <- ifelse(input$sel.Khoriz %in% c("", "todos"), "red",
                      nuevos.colores[as.numeric(input$sel.Khoriz)])
      updatePlot$kmapa <- cluster.mapa(nuevos.colores, F)
      updatePlot$khoriz <- cluster.horiz(
        input$sel.Khoriz, nuevos.colores, color, F)
      updatePlot$kvert <- cluster.vert(input$sel.Kvert, nuevos.colores, F)
      updatePlot$kradar <- cluster.radar(nuevos.colores, F)
      updatePlot$kcat <- cluster.cat(input$sel.Kbar, nuevos.colores, F)
      updateAceEditor(session, "fieldCodeKmapa", value = updatePlot$kmapa)
      updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
      updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
      updateAceEditor(session, "fieldCodeKradar", value = updatePlot$kradar)
      updateAceEditor(session, "fieldCodeKbar", value = updatePlot$kcat)
    }
  })

  #' Update on Table
  #' @author Diego
  #' @return functions
  #' @export
  #'
  mostrarData <- function() {
    if(isolate(input$idioma) == "es") {
      labelNum <- "Numérico"
      labelCat <- "Categórico"
    } else {
      labelNum <- "Numerical"
      labelCat <- "Categorical"
    }
    nombre.columnas <- c("ID", colnames(datos))
    tipo.columnas <- sapply(colnames(datos), function(i)
      ifelse(class(datos[,i]) %in% c("numeric", "integer"),
             paste0("<span data-id='numerico'>", labelNum, "</span>"), 
             paste0("<span data-id='categorico'>", labelCat, "</span>")))
    sketch = htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tags$tfoot(
        tags$tr(tags$th(), lapply(tipo.columnas, function(i) tags$th(HTML(i))))
      )
    ))
    DT::datatable(
      datos, selection = 'none', editable = TRUE,  container = sketch,
      options = list(dom = 'frtip', scrollY = "40vh")
    )
  }
  output$contents = DT::renderDataTable(NULL, server = T)

  #' Update on Transform Table
  #' @author Diego
  #' @return functions
  #' @export
  #'
  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0) {
      res <-  data.frame(Variables = colnames(datos),
                         Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i)
        paste0('<select id="sel', i, contador, '">',
               '<option value="categorico">', tr("categorico"), '</option>',
               '<option value="numerico" ',
               ifelse(class(datos[, i]) %in% c("numeric","integer"),
                      ' selected="selected"', ''), 
               '>', tr("numerico"), '</option>', '<option value="disyuntivo">',
               tr("disyuntivo"), '</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i)
        paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    } else {
      res <-  as.data.frame(NULL)
      showNotification(
        "ERROR: Check Separators", duration = 10, type = "error")
    }
    return(res)
  })

  output$transData = DT::renderDT({
    variables <- tr("variables")
    tipo <- tr("tipo")
    activa <- tr("activa")
    sketch = htmltools::withTags(table(
        tags$thead(tags$tr(tags$th(variables), tags$th(tipo), tags$th(activa)))
    ))
    DT::datatable(
      update.trans(), escape = FALSE, selection = 'none', container = sketch,
      rownames = F, options = list(
        dom = 't', paging = FALSE, ordering = FALSE, scrollY = "40vh"),
      callback = JS(paste0(
        "table.rows().every(function(i, tab, row) {\n",
        "  var $this = $(this.node());\n",
        "  $this.attr('id', this.data()[0]);\n",
        "  $this.addClass('shiny-input-checkbox');\n});\n",
        "Shiny.unbindAll(table.table().node());\n",
        "Shiny.bindAll(table.table().node());"))
    )
  }, server = F)

  #' Resumen numérico
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$resumen.completo = DT::renderDataTable({
    return(obj.resum())
  }, options = list(dom = 'ft', scrollX = TRUE), rownames = F)

  obj.resum <- eventReactive(updateData$datos, {
    datos <- updateData$datos
    createLog(nombre.datos, "basico", "resumen", "summary(datos)")
    data.frame(unclass(summary(datos)), check.names = FALSE,
               stringsAsFactors = FALSE)
  })

  output$resumen = renderUI({
    if(input$sel.resumen %in% colnames(var.numericas(datos))) {
      resumen.numerico(datos, input$sel.resumen)
    } else {
      resumen.categorico(datos, input$sel.resumen)
    }
  })

  #' Gráfico de Test de normalidad
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.normal = renderPlot({
    tryCatch({
      cod.normal <<- updatePlot$normal
      res <- isolate(eval(parse(text = cod.normal)))
      updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
      createLog(nombre.datos, "basico", "normalidad", cod.normal, 
                params = NULL, vars = input$sel.normal)
      return(res)
    }, error = function(e){
      if(ncol(var.numericas(datos)) <= 0){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(
      "datos", input$sel.normal, input$col.normal, tr("curvanormal"))
  })

  #' Resumen Test de normalidad
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$calculo.normal = DT::renderDT({
    tryCatch({
      datos <- updateData$datos
      codigo <- updatePlot$calc.normal
      res <- isolate(eval(parse(text = codigo)))
      updateAceEditor(session, "fieldCalcNormal", value = codigo)
      fisher <- tr("fisher")
      asimetria <- tr("asimetria")
      sketch = htmltools::withTags(table(
        tags$thead(tags$tr(tags$th(), tags$th(fisher), tags$th(asimetria)))
      ))
      DT::datatable(
        res, selection = 'none', container = sketch,
        options = list(dom = 'frtip', scrollY = "60vh")
      )
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  #' Gráfico de Dispersión
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.disp = renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
      if(!is.null(cod.disp) && cod.disp != "") {
        createLog(
          nombre.datos, "basico", "dispersion", cod.disp, params = NULL,
          vars = paste(input$select.var, collapse = " "))
      }
      return(isolate(eval(parse(text = cod.disp))))
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
           return(NULL)
      }
    })
  })
  
  output$plot.disp.zoom <- renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      res <- isolate(eval(parse(text = cod.disp)))
      res <- res + coord_cartesian(xlim = disp.ranges$x,
                                   ylim = disp.ranges$y, expand = FALSE)
      return(res)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$mostrar.disp.zoom = DT::renderDataTable({
    tryCatch({
      return(brushedPoints(datos[, input$select.var], input$zoom.disp))
    }, error = function(e) {
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })

  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  observeEvent(c(input$select.var, input$col.disp), {
    if(length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <- default.disp(
        data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })
  
  #' Gráfico de Distribuciones (Númericas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.num = renderPlot({
    tryCatch({
      cod.dya.num  <<- updatePlot$dya.num
      res <- isolate(eval(parse(text = cod.dya.num)))
      updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
      createLog(nombre.datos, "basico", "distribucion", cod.dya.num,  
                params = NULL, vars = input$sel.distribucion.num)
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 0){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })
  
  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })
  
  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <- def.code.num(
      data = "datos", color = paste0("'", input$col.dist, "'"), 
      variable = paste0("'", input$sel.distribucion.num, "'"))
  })
  
  output$mostrar.atipicos = DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out,
                   input$sel.distribucion.num, drop = F]
    datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
    datatable(datos, options = list(dom = 't', scrollX = TRUE, scrollY = "28vh",
                                    pageLength = nrow(datos))) %>%
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  })
  
  #' Gráfico de Distribuciones (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.cat = renderPlot({
    tryCatch({
      cod.dya.cat  <<- updatePlot$dya.cat
      res <- isolate(eval(parse(text = cod.dya.cat)))
      updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
      var <- isolate(input$sel.distribucion.cat)
      if(str_detect(var, pattern = "[[:digit:]]\\.(HC|CJ)") & 
         is.null(datos.originales[[var]])) {
        code <- paste0(
          "datos[['", var, "']] <- as.factor(paste0('HC', hc.modelo$clusters))",
          "\n", cod.dya.cat)
        createLog(
          nombre.datos, "rephc", "Cluster", code,  
          params = paste(
            "Clusters=", isolate(input$cant.cluster), "distancia=", 
            isolate(input$sel.dist.method), "metodo=", 
            isolate(input$sel.hc.method), collapse = "."))
      } else if(str_detect(var, pattern = "[[:digit:]]\\.(Kmedias|Kmeans)") &
                is.null(datos.originales[[var]])) {
        code <- paste0(
          "datos[['", var, "']] <- as.factor(paste0('K', k.modelo$cluster))\n",
          cod.dya.cat)
        createLog(
          nombre.datos, "kmedias", "Cluster", code, params = paste(
            "Clusters=", isolate(input$cant.kmeans.cluster), "iter=", 
            isolate(input$num.iter), "nstart=", isolate(input$num.nstart), 
            "algoritmo=", isolate(input$sel.algoritmo), collapse = "."))
      } else {
        createLog(nombre.datos, "basico", "distribucion", cod.dya.cat,  
                  params = NULL, vars = input$sel.distribucion.cat)
      }
      return(res)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(isolate(input$idioma), F)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })
  
  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })
  
  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <- def.code.cat(
      variable = input$sel.distribucion.cat)
  })
  
  #' Gráfico de Correlaciones
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.cor = renderPlot({
    tryCatch({
      updateData$cor.modelo
      cod.cor <- updatePlot$cor
      res <- isolate(eval(parse(text = cod.cor)))
      updateAceEditor(session, "fieldCodeCor", value = cod.cor)
      createLog(nombre.datos, "basico", "repcor", cod.cor)
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })
  
  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })
  
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(
      metodo = input$cor.metodo, tipo = input$cor.tipo)
  })

  #' Gráfico de PCA (Individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'

  output$plot.ind = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.ind <<- updatePlot$pca.ind
      res <- isolate(eval(parse(text = cod.pca.ind)))
      updateAceEditor(session, "fieldCodeInd", value = cod.pca.ind)
      createLog(
        nombre.datos, "acp", "repcaind", cod.pca.ind, 
        params = paste("centrar=", input$switch.scale, 
                       "repdim=", input$slider.npc, collapse = "."),
        vars = paste("coseno=", input$ind.cos, "ejes=", 
                     paste(input$slider.ejes, collapse = " "), collapse = " "))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
   })

   output$plot.ind.zoom <- renderPlot({
     tryCatch({
       ejex <- ind.ranges$x
       ejey <- ind.ranges$y
       if(is.null(ejex) & is.null(ejey)){
         return(NULL)
       } else {
         cod.ind <<- updatePlot$pca.ind
         res <- isolate(eval(parse(text = cod.ind)))
         res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE)
         return(res)
       }
     }, error = function(e) {
       return(NULL)
     })
   })

   output$mostrar.ind.zoom = DT::renderDataTable({
     tryCatch({
       dimensiones <- as.data.frame(pca.modelo$ind$coord)
       return(
         brushedPoints(
           df = dimensiones[, c(input$slider.ejes)], brush = input$zoom.ind,
           xvar = names(dimensiones)[input$slider.ejes[1]],
           yvar = names(dimensiones)[input$slider.ejes[2]])
       )
     }, error = function(e) {
       return(NULL)
     })
   }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                     pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.ind
    if (!is.null(brush)) {
      ind.ranges$x <- c(brush$xmin, brush$xmax)
      ind.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ind.ranges$x <- NULL
      ind.ranges$y <- NULL
    }
  })

  observeEvent(input$run.pcaInd, {
    updatePlot$pca.ind <- input$fieldCodeInd
  })

  observeEvent(c(input$col.pca.ind, input$ind.cos, input$slider.ejes), {
    updatePlot$pca.ind <- pca.individuos(
      ind.cos = input$ind.cos * 0.01, color = input$col.pca.ind, 
      ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.var = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.var <<- updatePlot$pca.var
      res <- isolate(eval(parse(text = cod.pca.var)))
      updateAceEditor(session, "fieldCodeVar", value = cod.pca.var)
      createLog(
        nombre.datos, "acp", "repcavar", cod.pca.var, 
        params = paste("centrar=", input$switch.scale, "repdim=", 
                       input$slider.npc), 
        vars = paste("coseno=", input$var.cos, "ejes=", 
                     paste(input$slider.ejes, collapse = " "),
                     collapse = " "))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.pcaVar, {
    updatePlot$pca.var <- input$fieldCodeVar
  })

  observeEvent(c(input$var.cos, input$col.pca.var, input$slider.ejes), {
    updatePlot$pca.var <- pca.variables(
      var.cos = input$var.cos * 0.01, color = input$col.pca.var,
      ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Sobreposición)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.biplot = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.bi <<- updatePlot$pca.bi
      res <- isolate(eval(parse(text = cod.pca.bi)))
      updateAceEditor(session, "fieldCodeBi", value = cod.pca.bi)
      createLog(
        nombre.datos, "acp", "repcabi", cod.pca.bi, 
        params = paste("centrar=", input$switch.scale, "repdim=", 
                       input$slider.npc), 
        vars = paste("repcosind=", input$ind.cos, "repcosvar=", input$var.cos,
                     "ejes=", paste(input$slider.ejes, collapse = " "),
                     collapse = " "))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })
  
  output$plot.bi.zoom <- renderPlot({
    tryCatch({
      ejex <- bi.ranges$x
      ejey <- bi.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        cod.bi <<- updatePlot$pca.bi
        res <- isolate(eval(parse(text = cod.bi)))
        res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE)
        return(res)
      }
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$mostrar.bi.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(
        df = dimensiones[, c(input$slider.ejes)], brush = input$zoom.bi,
        xvar = names(dimensiones)[input$slider.ejes[1]],
        yvar = names(dimensiones)[input$slider.ejes[2]])
      )
    }, error = function(e) {
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh", 
                    pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.bi
    if (!is.null(brush)) {
      bi.ranges$x <- c(brush$xmin, brush$xmax)
      bi.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      bi.ranges$x <- NULL
      bi.ranges$y <- NULL
    }
  })

  observeEvent(input$run.pcaBi, {
    updatePlot$pca.bi <- input$fieldCodeBi
  })

  observeEvent(c(input$col.pca.ind, input$ind.cos, input$var.cos,
                 input$col.pca.var, input$slider.ejes), {
    updatePlot$pca.bi <- pca.sobreposicion(
      ind.cos = input$ind.cos * 0.01, var.cos = input$var.cos * 0.01,
      col.ind = input$col.pca.ind, col.var = input$col.pca.var,
      ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Varianza Explicada para cada Eje)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotVEE = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.vee(tr("vee"), tr("dimensiones"), tr("porcvee"))
      updateAceEditor(session, "fieldCodeVEE", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      createLog(
        nombre.datos, "acp", "vee", codigo, 
        params = paste("centrar=", input$switch.scale, "dimensiones", 
                       input$slider.npc, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de los individuos)
  #' @author Diego
  #' @return plot
  #' @export

  output$plotCCI = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.cci(tr("cci"), tr("calidadcos"))
      updateAceEditor(session, "fieldCodeCCI", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      createLog(nombre.datos, "acp", "cci", codigo,  
                params = paste("centrar=", input$switch.scale, "dimensiones", 
                               input$slider.npc, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de las Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCCV = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.ccv(tr("ccv"), tr("calidadcos"))
      updateAceEditor(session, "fieldCodeCCV", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      createLog(nombre.datos, "acp", "ccv", codigo,  
                params = paste("centrar=", input$switch.scale, "dimensiones", 
                               input$slider.npc, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Correlación Variables con los Componenetes)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCVC = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- updatePlot$pca.cvc
      updateAceEditor(session, "fieldCodeCVC", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      createLog(nombre.datos, "acp", "cvc", codigo,  
                params = paste("centrar=", input$switch.scale, "dimensiones", 
                               input$slider.npc, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$cvc.metodo, {
    updatePlot$pca.cvc <- code.pca.cvp(input$cvc.metodo, tr("cvc"))
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 1)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC1 = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.pc1(tr("cp1"), tr("contribucion"))
      updateAceEditor(session, "fieldCodePC1", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      createLog(nombre.datos, "acp", "cp1", codigo,  
                params = paste("centrar=", input$switch.scale, "dimensiones", 
                               input$slider.npc, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 2)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC2 = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.pc2(tr("cp2"), tr("contribucion"))
      updateAceEditor(session, "fieldCodePC2", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      createLog(nombre.datos, "acp", "cp2", codigo,  
                params = paste("centrar=", input$switch.scale, "dimensiones", 
                               input$slider.npc, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Inercia Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  #'
  output$inercia.cj = renderUI({
    hc.modelo <<- updateData$hc.modelo
    datos <- isolate(updateData$datos)
    panel.inercia(updateData$hc.modelo$modelo, 
                  as.numeric(input$cant.cluster), datos = datos)
  })

  #' Gráfico de Clusterización Jerarquica (Diagrama)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.diag = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.diagrama <<- updatePlot$diag
      res <- isolate(eval(parse(text = code.diagrama)))
      createLog(
        nombre.datos, "rephc", "dendograma", code.diagrama,  
        params = paste(
          "Clusters=", input$cant.cluster, "distancia=", input$sel.dist.method,
          "metodo=", input$sel.hc.method, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcDendo, {
    updatePlot$diag <- input$fieldCodeDendo
  })

  #' Gráfico de Clusterización Jerarquica (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.mapa = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      pca.modelo <<- updateData$pca.modelo
      code.mapa <<- updatePlot$mapa
      res <- isolate(eval(parse(text = code.mapa)))
      createLog(
        nombre.datos, "rephc", "mapa", code.mapa,  
        params = paste(
          "Clusters=", input$cant.cluster, "distancia=", input$sel.dist.method,
          "metodo=", input$sel.hc.method, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  output$plot.mapa.zoom <- renderPlot({
    tryCatch({
      ejex <- mapa.ranges$x
      ejey <- mapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        cod.mapa <<- updatePlot$mapa
        res <- isolate(eval(parse(text = cod.mapa)))
        res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position="none")
        return(res)
      }
    }, error = function(e) {
      return(NULL)
    })
  })

  output$mostrar.mapa.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(
        df = dimensiones[, c(input$slider.ejes)], brush = input$zoom.mapa,
        xvar = names(dimensiones)[input$slider.ejes[1]],
        yvar = names(dimensiones)[input$slider.ejes[2]]))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.mapa
    if (!is.null(brush)) {
      mapa.ranges$x <- c(brush$xmin, brush$xmax)
      mapa.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      mapa.ranges$x <- NULL
      mapa.ranges$y <- NULL
    }
  })

  observeEvent(input$run.hcMapa, {
    updatePlot$mapa <- input$fieldCodeMapa
  })

  #' Gráfico de Clusterización Jerarquica (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.horiz = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.horiz <<- updatePlot$horiz
      res <- isolate(eval(parse(text = code.horiz)))
      createLog(
        nombre.datos, "rephc", "rephoriz", code.horiz, vars = input$selHoriz,  
        params = paste(
          "Clusters=", input$cant.cluster, "distancia=", input$sel.dist.method,
          "metodo=", input$sel.hc.method, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcHoriz, {
    updatePlot$horiz <- input$fieldCodeHoriz
  })

  observeEvent(input$selHoriz, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      color <- ifelse(input$selHoriz %in% c("", "todos"), "red",
                      nuevos.colores[as.numeric(input$selHoriz)])
      updatePlot$horiz <<- cluster.horiz(
        sel = input$selHoriz, colores = nuevos.colores, color = color)
      updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
    }
  })

  #' Gráfico de Clusterización Jerarquica (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.vert = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.vert <<- updatePlot$vert
      res <- isolate(eval(parse(text = code.vert)))
      createLog(
        nombre.datos, "rephc", "rephoriz", code.vert, vars = input$selVert,  
        params = paste(
          "Clusters=", input$cant.cluster, "distancia=", input$sel.dist.method,
          "metodo=", input$sel.hc.method, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcVert, {
    updatePlot$vert <- input$fieldCodeVert
  })

  observeEvent(input$selVert, {
    if(!is.null(datos) && !is.null(input$cant.cluster)) {
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      updatePlot$vert <<- cluster.vert(sel = input$selVert,
                                       colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
    }
  })

  #' Gráfico de Clusterización Jerarquica (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.radar = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.radar <<- updatePlot$radar
      res <- isolate(eval(parse(text = code.radar)))
      createLog(
        nombre.datos, "rephc", "repradar", code.radar, params = paste(
          "Clusters=", input$cant.cluster, "distancia=", input$sel.dist.method,
          "metodo=", input$sel.hc.method, collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcRadar, {
    updatePlot$radar <- input$fieldCodeRadar
  })

  #' Gráfico de Clusterización Jerarquica (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.bar.cat = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.cat <<- updatePlot$cat
      res <- isolate(eval(parse(text = code.cat)))
      createLog(
        nombre.datos, "rephc", "repcat", code.cat, vars = input$selBar,   
        params = paste(
          "Clusters=", input$cant.cluster, "distancia=", input$sel.dist.method,
          "metodo=", input$sel.hc.method, collapse = "."))
      return(res)
    }, warning = function(w) {
      showNotification(paste0("WARNING: ", w), duration = 10, type = "warning")
      return(NULL)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(isolate(input$idioma), F)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcBar, {
    updatePlot$cat <- input$fieldCodeBar
  })

  observeEvent(input$selBar, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      updatePlot$cat <<- cluster.cat(var = input$selBar,
                                     colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeBar", value = updatePlot$cat)
    }
  })

  #' Inercia K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$inercia.k = renderUI({
    k.modelo <<- updateData$k.modelo
    return(panel.inercia(esHC = F, k.modelo))
  })

  #' Gráfico de K-medias (Codo de Jambu)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.jambu = renderPlot({
    tryCatch({
      code.jambu <<- updatePlot$jambu
      isolate(eval(parse(text = code.jambu)))
      updateAceEditor(session, "fieldCodeJambu", value = code.jambu)
      res <- isolate(eval(parse(text = code.jambu)))
      createLog(nombre.datos, "kmedias", "jambu", code.jambu)
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Jambu, {
    updatePlot$jambu <- input$fieldCodeJambu
  })

  observeEvent(c(updateData$datos, input$iteracionesK), {
    updatePlot$jambu <- def.code.jambu(
      k = input$iteracionesK, tituloy = tr("inerciainter"))
  })

  #' Gráfico de K-medias (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kmapa = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      pca.modelo <<- updateData$pca.modelo
      code.kmapa <<- updatePlot$kmapa
      res <- isolate(eval(parse(text = code.kmapa)))
      createLog(
        nombre.datos, "kmedias", "mapa", code.kmapa, params = paste(
          "Clusters=", input$cant.kmeans.cluster, "iter=", input$num.iter, 
          "nstart=", input$num.nstart, "algoritmo=", input$sel.algoritmo, 
          collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  output$plot.kmapa.zoom <- renderPlot({
    tryCatch({
      ejex <- kmapa.ranges$x
      ejey <- kmapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        cod.kmapa <<- updatePlot$kmapa
        res <- isolate(eval(parse(text = cod.kmapa)))
        res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position="none")
        return(res)
      }
    }, error = function(e) {
      return(NULL)
    })
  })

  output$mostrar.kmapa.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(
        df = dimensiones[, c(input$slider.ejes)], brush = input$zoom.kmapa,
        xvar = names(dimensiones)[input$slider.ejes[1]],
        yvar = names(dimensiones)[input$slider.ejes[2]]))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.kmapa
    if (!is.null(brush)) {
      kmapa.ranges$x <- c(brush$xmin, brush$xmax)
      kmapa.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      kmapa.ranges$x <- NULL
      kmapa.ranges$y <- NULL
    }
  })

  observeEvent(input$run.Kmapa, {
    updatePlot$kmapa <- input$fieldCodeKmapa
  })

  #' Gráfico de K-medias (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.khoriz = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      code.khoriz <<- updatePlot$khoriz
      res <- isolate(eval(parse(text = code.khoriz)))
      createLog(
        nombre.datos, "kmedias", "rephoriz", code.khoriz, 
        vars = input$sel.Khoriz, params = paste(
          "Clusters=", input$cant.kmeans.cluster, "iter=", input$num.iter, 
          "nstart=", input$num.nstart, "algoritmo=", input$sel.algoritmo, 
          collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Khoriz, {
    updatePlot$khoriz <- input$fieldCodeKhoriz
  })

  observeEvent(input$sel.Khoriz, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      color <- ifelse(input$sel.Khoriz %in% c("", "todos"), "red",
                      nuevos.colores[as.numeric(input$sel.Khoriz)])
      updatePlot$khoriz <- cluster.horiz(esHC = F, sel = input$sel.Khoriz,
                                         colores = nuevos.colores, color = color)
      updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
    }
  })

  #' Gráfico de K-medias (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
    output$plot.kvert = renderPlot({
      tryCatch({
        k.modelo <<- updateData$k.modelo
        code.kvert <<- updatePlot$kvert
        res <- isolate(eval(parse(text = code.kvert)))
        createLog(
          nombre.datos, "kmedias", "repvert", code.kvert, 
          vars = input$sel.Kvert, params = paste(
            "Clusters=", input$cant.kmeans.cluster, "iter=", input$num.iter, 
            "nstart=", input$num.nstart, "algoritmo=", input$sel.algoritmo, 
            collapse = "."))
        return(res)
      }, error = function(e) {
        if(ncol(var.numericas(datos)) <= 1){
          error.variables(isolate(input$idioma), T)
        } else {
          showNotification(paste0("ERROR: ", e),
                           duration = 10, type = "error")
          return(NULL)
        }
      })
    })

  observeEvent(input$run.KVert, {
    updatePlot$kvert <- input$fieldCodeKvert
  })

  observeEvent(input$sel.Kvert, {
    if(!is.null(datos) && !is.null(input$cant.kmeans.cluster)) {
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      updatePlot$kvert <<- cluster.vert(esHC = F, sel = input$sel.Kvert,
                                         colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
    }
  })

  #' Gráfico de K-medias (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kradar = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      code.kradar <<- updatePlot$kradar
      res <- isolate(eval(parse(text = code.kradar)))
      createLog(
        nombre.datos, "kmedias", "repradar", code.kradar, params = paste(
          "Clusters=", input$cant.kmeans.cluster, "iter=", input$num.iter, 
          "nstart=", input$num.nstart, "algoritmo=", input$sel.algoritmo, 
          collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(isolate(input$idioma), T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Kradar, {
    updatePlot$kradar <- input$fieldCodeKradar
  })

  #' Gráfico de K-medias (Categórico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kcat = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      code.kcat <<- updatePlot$kcat
      res <- isolate(eval(parse(text = code.kcat)))
      createLog(
        nombre.datos, "kmedias", "repcat", code.kcat, vars = input$sel.Kbar, 
        params = paste(
          "Clusters=", input$cant.kmeans.cluster, "iter=", input$num.iter, 
          "nstart=", input$num.nstart, "algoritmo=", input$sel.algoritmo, 
          collapse = "."))
      return(res)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(isolate(input$idioma), F)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Kbar, {
    updatePlot$kcat <- input$fieldCodeKbar
  })

  observeEvent(input$sel.Kbar, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      updatePlot$kcat <<- cluster.cat(esHC = F, var = input$sel.Kbar,
                                       colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeKbar", value = updatePlot$kcat)
    }
  })

  #' Mostrar Colores (k-means)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(updateData$datos, input$cant.kmeans.cluster), {
    if(!is.null(datos) && !is.null(input$cant.kmeans.cluster)) {
      updateinitSelects("sel.Khoriz", 1:input$cant.kmeans.cluster)
      for (i in 1:10) {
        if(i <= input$cant.kmeans.cluster) {
          shinyjs::show(paste0("kColor", i))
        } else {
          shinyjs::hide(paste0("kColor", i))
        }
      }
    }
  })

  #' Mostrar Colores (Cluster jerarquico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(updateData$datos, input$cant.cluster), {
    if(!is.null(datos) && !is.null(input$cant.cluster)){
      updateinitSelects("selHoriz", 1:input$cant.cluster)
      for (i in 1:10) {
        if(i <= input$cant.cluster) {
          shinyjs::show(paste0("hcColor", i))
        } else {
          shinyjs::hide(paste0("hcColor", i))
        }
      }
    }
  })

  observeEvent(c(input$hcColor1, input$hcColor2, input$hcColor3, input$hcColor4,
                 input$hcColor5, input$hcColor6, input$hcColor7, input$hcColor8,
                 input$hcColor9, input$hcColor10), {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      color <- ifelse(input$selHoriz %in% c("", "todos"), "red",
                      nuevos.colores[as.numeric(input$selHoriz)])
      updatePlot$diag <- diagrama(cant = input$cant.cluster,
                                   colores = nuevos.colores)
      updatePlot$mapa <- cluster.mapa(colores = nuevos.colores)
      updatePlot$horiz <- cluster.horiz(
        sel = input$selHoriz, colores = nuevos.colores, color = color)
      updatePlot$vert <- cluster.vert(
        sel = input$selVert, colores = nuevos.colores)
      updatePlot$radar <- cluster.radar(colores = nuevos.colores)
      updatePlot$cat <- cluster.cat(
        var = input$selBar, colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeDendo", value = updatePlot$diag)
      updateAceEditor(session, "fieldCodeMapa", value = updatePlot$mapa)
      updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
      updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
      updateAceEditor(session, "fieldCodeRadar", value = updatePlot$radar)
      updateAceEditor(session, "fieldCodeBar", value = updatePlot$cat)
    }
  })

  observeEvent(c(
    input$kColor1, input$kColor2, input$kColor3, input$kColor4, input$kColor5,
    input$kColor6, input$kColor7, input$kColor8, input$kColor9, input$kColor10
  ), {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      color <- ifelse(input$sel.Khoriz %in% c("", "todos"), "red",
                      nuevos.colores[as.numeric(input$sel.Khoriz)])
      updatePlot$kmapa <- cluster.mapa(esHC = F, colores = nuevos.colores)
      updatePlot$khoriz <- cluster.horiz(
        esHC = F, sel = input$sel.Khoriz, colores = nuevos.colores, color = color)
      updatePlot$kvert <- cluster.vert(
        esHC = F, sel = input$sel.Kvert, colores = nuevos.colores)
      updatePlot$kradar <- cluster.radar(esHC = F, colores = nuevos.colores)
      updatePlot$kcat <- cluster.cat(
        esHC = F, var = input$sel.Kbar, colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeKmapa", value = updatePlot$kmapa)
      updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
      updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
      updateAceEditor(session, "fieldCodeKradar", value = updatePlot$kradar)
      updateAceEditor(session, "fieldCodeKbar", value = updatePlot$kcat)
    }
  })

  observeEvent(input$HCbutton, {
    ifelse(input$idioma == "es", aux <- "CJ", aux <- "HC")
    C.Jerarquica <- as.factor(paste0(aux, hc.modelo$clusters))
    datos[[paste0(length(levels(C.Jerarquica)), ".", aux)]] <<- C.Jerarquica
    output$contents = DT::renderDataTable(mostrarData())
    showNotification(tr("msjclusters"), duration = 5, type = "message")
    updateSelectInput(session, "sel.distribucion.cat",
                      choices = colnames(var.categoricas(datos)))
  })

  observeEvent(input$Kbutton, {
    ifelse(input$idioma == "es", aux <- ".Kmedias", aux <- ".Kmeans")
    Kmedias <- as.factor(paste0("K", k.modelo$cluster))
    datos[[paste0(length(levels(Kmedias)), aux)]] <<- Kmedias
    output$contents = DT::renderDataTable(mostrarData())
    showNotification(tr("msjclusters"), duration = 5, type = "message")
    updateSelectInput(session, "sel.distribucion.cat",
                      choices = colnames(var.categoricas(datos)))
  })

  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      write.csv(datos, file, row.names = input$rowname)
    }
  )

  observeEvent(c(input$principal, input$idioma), {
    if(input$principal == "reporte"){
      updateAceEditor(session, "fieldCodeReport", value = user.reporte())
    }
  })

  output$descargar <- downloadHandler(
    filename = function() {
      paste(input$textTitulo,'-', input$textNombre, '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;

      namermd <- paste(input$textTitulo,'-', input$textNombre, '.rmd', sep='')
      reporte <- paste0(def.reporte(input$textTitulo, input$textNombre), 
                        input$fieldCodeReport)
      writeLines(reporte, namermd)
      files <- c(namermd, files)

      src <- normalizePath(namermd)
      withCallingHandlers({
        overwrite.cat()
        salida.code <<- ""
        shinyjs::html("txtreport", salida.code)
        out <- rmarkdown::render(
          src,  params = NULL, rmarkdown::word_document(), envir = env.report)
      }, message = function(m) {
        salida.code <<- paste0(m$message, salida.code)
          shinyjs::html(id = "txtreport", html = salida.code)
        }
      )
      recover.cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)

      zip::zip(file, files)
    }
  )
})
