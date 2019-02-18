# Server Script

shinyServer(function(input, output, session) {
  source('global.R', local = T)
  load("www/translation.bin")
  options(shiny.maxRequestSize=200*1024^2)
  options(
    DT.options = list(
      aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
      scrollX = TRUE, language = list(
        search = shiny::HTML('<i class="fa fa-search"></i>'),
        info = "", emptyTable = "", zeroRecords = "",
        paginate = list(
          "previous" = shiny::HTML('<i class="fa fa-backward"></i>'),
          "next" = shiny::HTML('<i class="fa fa-forward"></i>'),
          "first" =shiny::HTML('<i class="fa fa-fast-backward"></i>'), 
          "last" = shiny::HTML('<i class="fa fa-fast-forward"></i>')))
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
  shinyAce::updateAceEditor(session, "fieldCodeResum", "summary(datos)")
  shinyAce::updateAceEditor(session, "fieldModelCor", modelo.cor())
  shinyAce::updateAceEditor(session, "fieldFuncJambu",
    paste0(extract.code("lead"), "\n", extract.code("codo.jambu")))
  shinyAce::updateAceEditor(session, "fieldFuncNum", 
                            extract.code("distribucion.numerico"))
  shinyAce::updateAceEditor(session, "fieldFuncCat", 
                            extract.code("distribucion.categorico"))
  shinyAce::updateAceEditor(session, "fieldCodeCentr", 
                            extract.code("calc.centros"))
  shinyAce::updateAceEditor(session, "fieldFuncHoriz", 
                            extract.code("centros.horizontal.todos"))
  shinyAce::updateAceEditor(session, "fieldFuncVert", 
                            extract.code("centros.vertical.todos"))
  shinyAce::updateAceEditor(
    session, "fieldFuncRadar", paste0(extract.code("centros.radar")))
  shinyAce::updateAceEditor(session, "fieldFuncKhoriz", 
                            extract.code("centros.horizontal.todos"))
  shinyAce::updateAceEditor(session, "fieldFuncKvert", 
                            extract.code("centros.vertical.todos"))
  shinyAce::updateAceEditor(
    session, "fieldFuncKradar", paste0(extract.code("centros.radar")))
  
  updateData <- shiny::reactiveValues(
    datos = NULL, cor.modelo = NULL, pca.modelo = NULL,
    hc.modelo = NULL, k.modelo = NULL)

  updatePlot <- shiny::reactiveValues(
    calc.normal=default.calc.normal(), normal=NULL, disp=NULL, pca.ind=NULL,
    pca.var=NULL, pca.bi=NULL, cor=NULL, pca.cvc=NULL, mapa=NULL, dya.num=NULL,
    dya.cat=NULL, diag=NULL, horiz=NULL, vert=NULL, radar=NULL, cat=NULL,
    jambu=NULL, Kmapa=NULL, Khoriz=NULL, Kvert=NULL, Kradar=NULL, Kcat=NULL)

  disp.ranges <- shiny::reactiveValues(x = NULL, y = NULL)
  ind.ranges <- shiny::reactiveValues(x = NULL, y = NULL)
  bi.ranges <- shiny::reactiveValues(x = NULL, y = NULL)
  mapa.ranges <- shiny::reactiveValues(x = NULL, y = NULL)
  kmapa.ranges <- shiny::reactiveValues(x = NULL, y = NULL)

  shiny::observe({
    updateMenu(init = T)
    close.menu()
  })
  
  #' Load Button Function
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(input$loadButton, {
    rowname <- shiny::isolate(input$rowname)
    ruta <- shiny::isolate(input$file1)
    sep <- shiny::isolate(input$sep)
    dec <- shiny::isolate(input$dec)
    encabezado <- shiny::isolate(input$header)
    deleteNA <- shiny::isolate(input$deleteNA)
    tryCatch({
      codigo.carga <- 
        code.carga(rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      eval(parse(text = codigo.carga))
      if(ncol(datos.originales) <= 1) {
        shiny::showNotification(
          "ERROR: Check Separators", duration = 10, type = "error")
        return(NULL)
      }
      nombre.datos <<- 
        paste0(ruta$name, ifelse(deleteNA, ".sinNA.", ".conNA."))
      datos.reporte[[nombre.datos]] <<- datos.originales
      init.replist(nombre.datos)
      
      shinyAce::updateAceEditor(session, "fieldCodeData", value = codigo.carga)
      updateMenu(datos.originales)
      updateSelects(datos.originales)
      updateData$datos <- datos.originales
      close.menu(is.null(datos.originales))
    }, error = function(e) {
      updateData$datos <- NULL
      datos.originales <<- NULL
      updateMenu(NULL)
      close.menu(is.null(NULL))
      mostrarError(e)
    })
  })

  #' Transform Button Function
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(input$transButton, {
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
    createLogBasico(nombre.datos, "Transformacion de los Datos", "str(datos)")
    eval(parse(text = code.res))
    shinyAce::updateAceEditor(session, "fieldCodeTrans", value = code.res)
    updateSelects(datos)
    updateData$datos <- datos
    updateMenu(datos)
  })
  
  ###################################  Update  ################################
  #' Update on Language
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(input$idioma, {
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
    
    updateinitSelects("selHoriz", 1:input$cant.cluster)
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
  
  output$contents = DT::renderDataTable({
    datos <- updateData$datos
    if(input$idioma == "es") {
      labelNum <- "Numérico"
      labelCat <- "Categórico"
    } else {
      labelNum <- "Numerical"
      labelCat <- "Categorical"
    }
    tryCatch({
      nombre.columnas <- c("ID", colnames(datos))
      tipo.columnas <- sapply(colnames(datos), function(i)
        ifelse(class(datos[,i]) %in% c("numeric", "integer"),
               paste0("<span data-id='numerico'>", labelNum, "</span>"), 
               paste0("<span data-id='categorico'>", labelCat, "</span>")))
      sketch = htmltools::withTags(table(
        tableHeader(nombre.columnas),
        tags$tfoot(
          tags$tr(tags$th(), lapply(tipo.columnas, function(i) tags$th(shiny::HTML(i))))
        )
      ))
      DT::datatable(
        datos, selection = 'none', editable = TRUE,  container = sketch,
        options = list(dom = 'frtip', scrollY = "40vh")
      )
    }, error = function(e) {
      return(NULL)
    })
  })

  #' Update Transform Table
  #' @author Diego
  #' @return functions
  #' @export
  #'
  update.trans <- eventReactive(input$loadButton, {
    datos <- updateData$datos
    contador <<- contador + 1
    res <- as.data.frame(NULL)
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
    datos <- updateData$datos
    createLogBasico(nombre.datos, "resumen", "summary(datos)")
    res <- data.frame(unclass(summary(datos)), check.names = FALSE,
                      stringsAsFactors = FALSE)
    return(res)
  }, options = list(dom = 'ft', scrollX = TRUE), rownames = F)

  output$resumen = shiny::renderUI({
    datos <- shiny::isolate(updateData$datos)
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
  output$plot.normal = shiny::renderPlot({
    datos <- updateData$datos
    tryCatch({
      cod.normal <<- updatePlot$normal
      res <- shiny::isolate(eval(parse(text = cod.normal)))
      shinyAce::updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
      createLogBasico(nombre.datos, "normalidad", cod.normal, input$sel.normal)
      return(res)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })

  shiny::observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  shiny::observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(
      "datos", input$sel.normal, input$col.normal, tr("curvanormal"))
  })

  #' Resumen Test de normalidad
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$calculo.normal = DT::renderDT({
    datos <- updateData$datos
    tryCatch({
      codigo <- updatePlot$calc.normal
      res <- shiny::isolate(eval(parse(text = codigo)))
      shinyAce::updateAceEditor(session, "fieldCalcNormal", value = codigo)
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
      mostrarError(e)
    })
  })

  shiny::observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  #' Gráfico de Dispersión
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.disp = shiny::renderPlot({
    datos <- updateData$datos
    tryCatch({
      cod.disp <- updatePlot$disp
      disp.plot <<- eval(parse(text = cod.disp))
      shinyAce::updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
      if(!is.null(disp.plot)) {
        createLogBasico(nombre.datos, "dispersion", cod.disp, 
                        paste(input$select.var, collapse = "-"))
      }
      return(disp.plot)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })
  
  output$plot.disp.zoom <- shiny::renderPlot({
    tryCatch({
      disp.plot + coord_cartesian(
        xlim = disp.ranges$x, ylim = disp.ranges$y, expand = FALSE)
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

  shiny::observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })

  shiny::observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  shiny::observeEvent(c(input$select.var, input$col.disp), {
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
  output$plot.num = shiny::renderPlot({
    datos <- updateData$datos
    tryCatch({
      cod.dya.num  <- updatePlot$dya.num
      res <- eval(parse(text = cod.dya.num))
      num.var <- shiny::isolate(input$sel.distribucion.num)
      shinyAce::updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
      createLogBasico(nombre.datos, "distribucion", cod.dya.num, num.var)
      return(res)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })
  
  shiny::observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <- def.code.num(
      data = "datos", color = paste0("'", input$col.dist, "'"), 
      variable = paste0("'", input$sel.distribucion.num, "'"))
  })
  
  output$mostrar.atipicos = DT::renderDataTable({
    datos <- shiny::isolate(updateData$datos)
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out,
                   input$sel.distribucion.num, drop = F]
    datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
    datatable(datos, options = list(
      dom = 't', scrollX = TRUE, scrollY = "28vh", pageLength = nrow(datos))) %>%
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  })
  
  #' Gráfico de Distribuciones (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.cat = shiny::renderPlot({
    datos <- updateData$datos
    tryCatch({
      cod.dya.cat <- updatePlot$dya.cat
      res <- eval(parse(text = cod.dya.cat))
      shinyAce::updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
      cat.var <- shiny::isolate(input$sel.distribucion.cat)
      return(res)
    }, error = function(e) {
      mostrarError(e, n.cat = ncol(var.categoricas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })
  
  shiny::observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <- def.code.cat(
      variable = input$sel.distribucion.cat)
  })
  
  #' Gráfico de Correlaciones
  #' @author Diego
  #' @return plot
  #' @export
  #'

  shiny::observeEvent(updateData$datos, {
    datos <- updateData$datos
    tryCatch({
      eval(parse(text = modelo.cor()))
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })
  
  output$plot.cor = shiny::renderPlot({
    datos <- updateData$datos
    tryCatch({
      updateData$cor.modelo
      cod.cor <- updatePlot$cor
      res <- shiny::isolate(eval(parse(text = cod.cor)))
      shinyAce::updateAceEditor(session, "fieldCodeCor", value = cod.cor)
      createLogBasico(nombre.datos, "correlacion", cod.cor)
      return(res)
    }, error = function(e) {
      mostrarError(e)
    })
  })
  
  shiny::observeEvent(input$run.code.cor, {
    updatePlot$cor <- shiny::isolate(input$fieldCodeCor)
  })
  
  shiny::observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(
      metodo = input$cor.metodo, tipo = input$cor.tipo)
  })
  
  #' Update PCA
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$ACPRun), {
    datos <- updateData$datos
    hay.num <- ncol(var.numericas(datos))
    
    centrado <- shiny::isolate(input$switch.scale)
    dimensiones <- shiny::isolate(input$slider.npc)
    ejes <- shiny::isolate(input$slider.ejes)
    ind.cos <- shiny::isolate(input$ind.cos) * 0.01
    ind.col <- shiny::isolate(input$col.pca.ind)
    var.cos <- shiny::isolate(input$var.cos) * 0.01
    var.col <- shiny::isolate(input$col.pca.var) 
    
    codigo <- def.pca.model(scale.unit = centrado, npc = dimensiones)
    idioma <- shiny::isolate(input$idioma)
    rep.acp <<- c(centrado, dimensiones)
    
    shinyAce::updateAceEditor(session, "fieldCodePCAModelo", value = codigo)
    
    if(!is.null(datos)) {
      pca.modelo <<- eval(parse(text = codigo))
      if(!is.null(pca.modelo)) {
        updateData$pca.modelo <- pca.modelo
        output$txtpca <- shiny::renderPrint(unclass(updateData$pca.modelo))
        createLogACP(nombre.datos, codigo, rep.acp, "modelo")
        
        updatePlot$pca.ind <- pca.individuos(ind.cos, ind.col, ejes)
        updatePlot$pca.var <- pca.variables(var.cos, var.col, ejes)
        updatePlot$pca.bi  <- 
          pca.sobreposicion(ind.cos, var.cos, ind.col, var.col, ejes)
      }
    }
  })
  
  observeEvent(input$slider.npc, {
    updateSliderInput(session, "slider.ejes", max = input$slider.npc, 
                      value = c(1, 2))
  })

  #' Gráfico de PCA (Individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.ind = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$pca.ind
    shinyAce::updateAceEditor(session, "fieldCodeInd", value = codigo)
    tryCatch({
      grafico.ind <<- eval(parse(text = codigo))
      ejes    <- paste(shiny::isolate(input$slider.ejes), collapse = ", ")
      cos     <- shiny::isolate(input$ind.cos) * 0.01
      createLogACP(nombre.datos, codigo, rep.acp,
                   paste0("Individuos ejes: ", ejes, ", cos: ", cos))
      return(grafico.ind)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.pcaInd, {
    updatePlot$pca.ind <- shiny::isolate(input$fieldCodeInd)
  })
  
  output$plot.ind.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- ind.ranges$x
      ejey <- ind.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        grafico.ind + 
          coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE)
       }
     }, error = function(e) {
       return(NULL)
     })
   })

   output$mostrar.ind.zoom = DT::renderDataTable({
     tryCatch({
       dimensiones <- as.data.frame(pca.modelo$ind$coord)
       ejes <- input$slider.ejes
       return(
         brushedPoints(
           df = dimensiones[, c(ejes)], brush = input$zoom.ind,
           xvar = names(dimensiones)[ejes[1]],
           yvar = names(dimensiones)[ejes[2]])
       )
     }, error = function(e) {
       return(NULL)
     })
   }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                     pageLength = nrow(datos)))

  shiny::observe({
    brush <- input$zoom.ind
    if (!is.null(brush)) {
      ind.ranges$x <- c(brush$xmin, brush$xmax)
      ind.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ind.ranges$x <- NULL
      ind.ranges$y <- NULL
    }
  })

  #' Gráfico de PCA (Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.var = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$pca.var
    shinyAce::updateAceEditor(session, "fieldCodeVar", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      ejes    <- paste(shiny::isolate(input$slider.ejes), collapse = ", ")
      cos     <- shiny::isolate(input$var.cos) * 0.01
      createLogACP(nombre.datos, codigo, rep.acp,
                   paste0("Variables ejes: ", ejes, ", cos: ", cos))
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.pcaVar, {
    updatePlot$pca.var <- shiny::isolate(input$fieldCodeVar)
  })

  #' Gráfico de PCA (Sobreposición)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.biplot = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$pca.bi
    shinyAce::updateAceEditor(session, "fieldCodeBi", value = codigo)
    tryCatch({
      grafico.bi <<- eval(parse(text = codigo))
      ejes    <- paste(shiny::isolate(input$slider.ejes), collapse = ", ")
      var.cos <- shiny::isolate(input$var.cos) * 0.01
      ind.cos <- shiny::isolate(input$ind.cos) * 0.01
      createLogACP(nombre.datos, codigo, rep.acp, paste0(
        "Sobreposicion ejes: ", ejes, ", var.cos: ", var.cos, 
        ", ind.cos: ", ind.cos))
      return(grafico.bi)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.pcaBi, {
    updatePlot$pca.bi <- shiny::isolate(input$fieldCodeBi)
  })
  
  output$plot.bi.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- bi.ranges$x
      ejey <- bi.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        grafico.bi + 
          coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE)
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

  shiny::observe({
    brush <- input$zoom.bi
    if (!is.null(brush)) {
      bi.ranges$x <- c(brush$xmin, brush$xmax)
      bi.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      bi.ranges$x <- NULL
      bi.ranges$y <- NULL
    }
  })

  #' Gráfico de PCA (Varianza Explicada para cada Eje)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotVEE = shiny::renderPlot({
    pca.modelo <<- updateData$pca.modelo
    tryCatch({
      codigo <- code.pca.vee(tr("vee"), tr("dimensiones"), tr("porcvee"))
      shinyAce::updateAceEditor(session, "fieldCodeVEE", value = codigo)
      grafico <- eval(parse(text = codigo))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Varianza Explicada para cada Eje")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de los individuos)
  #' @author Diego
  #' @return plot
  #' @export

  output$plotCCI = shiny::renderPlot({
    pca.modelo <<- updateData$pca.modelo
    codigo <- code.pca.cci(tr("cci"), tr("calidadcos"))
    shinyAce::updateAceEditor(session, "fieldCodeCCI", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Cosenos Cuadrados de los individuos")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de las Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCCV = shiny::renderPlot({
    pca.modelo <<- updateData$pca.modelo
    codigo <- code.pca.ccv(tr("ccv"), tr("calidadcos"))
    shinyAce::updateAceEditor(session, "fieldCodeCCV", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Cosenos Cuadrados de las Variables")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  #' Gráfico de PCA (Correlación Variables con los Componenetes)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCVC = shiny::renderPlot({
    pca.modelo <<- updateData$pca.modelo
    codigo <- updatePlot$pca.cvc
    shinyAce::updateAceEditor(session, "fieldCodeCVC", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Correlación Variables con los Componenetes")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  shiny::observeEvent(input$cvc.metodo, {
    updatePlot$pca.cvc <- code.pca.cvp(input$cvc.metodo, tr("cvc"))
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 1)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC1 = shiny::renderPlot({
    pca.modelo <<- updateData$pca.modelo
    codigo <- code.pca.pc1(tr("cp1"), tr("contribucion"))
    shinyAce::updateAceEditor(session, "fieldCodePC1", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Contribución de las variables de la Dimensión 1")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 2)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC2 = shiny::renderPlot({
    pca.modelo <<- updateData$pca.modelo
    codigo <- code.pca.pc2(tr("cp2"), tr("contribucion"))
    shinyAce::updateAceEditor(session, "fieldCodePC2", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Contribución de las variables de la Dimensión 2")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  #' Actualización del Modelo Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$CJRun), {
    datos            <- shiny::isolate(updateData$datos)
    cant             <- shiny::isolate(input$cant.cluster)
    dist.method      <- shiny::isolate(input$sel.dist.method)
    hc.method        <- shiny::isolate(input$sel.hc.method)
    cant.numericas   <- ncol(var.numericas(datos))
    cant.categoricas <- ncol(var.categoricas(datos))
    idioma           <- shiny::isolate(input$idioma)
    var.horiz        <- shiny::isolate(input$selHoriz)
    var.vert         <- shiny::isolate(input$selVert)
    var.cat          <- shiny::isolate(input$selBar)
    nuevos.colores   <- sapply(1:cant, function(i)
      paste0("'", input[[paste0("hcColor", i)]], "'"))
    
    codigo     <- def.model(data = "datos", cant, dist.method, hc.method)
    rep.hc     <<- c(cant, dist.method, hc.method)
    
    shinyAce::updateAceEditor(session, "fieldCodeModelo", value = codigo)
    tryCatch ({
      if(!is.null(datos) && !is.null(cant)) {
        eval(parse(text = codigo))
        updateData$hc.modelo <- hc.modelo
        output$inercia.cj = shiny::renderUI({
          panel.inercia(hc.modelo$modelo, as.numeric(cant), datos)
        })
        output$txthc <- shiny::renderPrint(print(unclass(hc.modelo)))
        output$txtcentros <- shiny::renderPrint(print(unclass(centros)))
        createLogCJ(nombre.datos, codigo, rep.hc, "modelo")
        
        updatePlot$diag  <- diagrama(cant, nuevos.colores)
        updatePlot$mapa  <- cluster.mapa(nuevos.colores)
        updatePlot$horiz <- cluster.horiz(var.horiz, nuevos.colores)
        updatePlot$vert  <- cluster.vert(var.vert, nuevos.colores)
        updatePlot$radar <- cluster.radar(nuevos.colores)
        updatePlot$cat   <- cluster.cat(var.cat, nuevos.colores)
      }
    }, error = function(e) {
      mostrarError(e)
    })
  })

  #' Gráfico de Clusterización Jerarquica (Diagrama)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.diag = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$diag
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeDendo", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogCJ(nombre.datos, codigo, rep.hc, "Dendograma")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.hcDiag, {
    updatePlot$diag <- shiny::isolate(input$fieldCodeDendo)
  })

  #' Gráfico de Clusterización Jerarquica (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.mapa = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$mapa
    pca.modelo <- shiny::isolate(updateData$pca.modelo)
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeMapa", value = codigo)
    tryCatch({
      grafico.hc.mapa <<- eval(parse(text = codigo))
      createLogCJ(nombre.datos, codigo, rep.hc, "Mapa")
      return(grafico.hc.mapa)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.hcMapa, {
    updatePlot$mapa <- shiny::isolate(input$fieldCodeMapa)
  })

  output$plot.mapa.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- mapa.ranges$x
      ejey <- mapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        grafico.hc.mapa + 
          coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position = "none")
      }
    }, error = function(e) {
      return(NULL)
    })
  })

  output$mostrar.mapa.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(
        df = dimensiones[, c(1, 2)], brush = input$zoom.mapa,
        xvar = names(dimensiones)[1], yvar = names(dimensiones)[2]))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  shiny::observe({
    brush <- input$zoom.mapa
    if (!is.null(brush)) {
      mapa.ranges$x <- c(brush$xmin, brush$xmax)
      mapa.ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      mapa.ranges$x <- NULL
      mapa.ranges$y <- NULL
    }
  })

  #' Gráfico de Clusterización Jerarquica (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.horiz = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$horiz
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeHoriz", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      var <- shiny::isolate(input$selHoriz)
      createLogCJ(nombre.datos, codigo, rep.hc, paste0("Horizontal: ", var))
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.hcHoriz, {
    updatePlot$horiz <- shiny::isolate(input$fieldCodeHoriz)
  })

  shiny::observeEvent(input$selHoriz, {
    var   <- input$selHoriz
    cant  <- shiny::isolate(input$cant.cluster)
    color <- sapply(1:cant, function(i) 
      paste0("'", shiny::isolate(input[[paste0("hcColor", i)]]), "'"))
    if(!var %in% c("", "todos")) color <- color[as.numeric(var)]
    updatePlot$horiz <- cluster.horiz(var, color)
  })

  #' Gráfico de Clusterización Jerarquica (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.vert = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$vert
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeVert", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      var     <- shiny::isolate(input$selVert)
      createLogCJ(nombre.datos, codigo, rep.hc, paste0("Vertical:", var))
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.hcVert, {
    updatePlot$vert <- shiny::isolate(input$fieldCodeVert)
  })
  
  shiny::observeEvent(input$selVert, {
    var   <- input$selVert
    cant  <- shiny::isolate(input$cant.cluster)
    color <- sapply(1:cant, function(i) 
      paste0("'", shiny::isolate(input[[paste0("hcColor", i)]]), "'"))
    updatePlot$vert <- cluster.vert(sel = var, colores = color)
  })

  #' Gráfico de Clusterización Jerarquica (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.radar = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$radar
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeRadar", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogCJ(nombre.datos, codigo, rep.hc, "Radar")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.hcRadar, {
    updatePlot$radar <- shiny::isolate(input$fieldCodeRadar)
  })

  #' Gráfico de Clusterización Jerarquica (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.bar.cat = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$cat
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeBar", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      var     <- shiny::isolate(input$selBar)
      createLogCJ(nombre.datos, codigo, rep.hc, paste0("Categoricas:", var))
      return(grafico)
    }, error = function(e) {
      mostrarError(e, n.cat = ncol(var.categoricas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.hcBar, {
    updatePlot$cat <- shiny::isolate(input$fieldCodeBar)
  })
  
  shiny::observeEvent(input$selBar, {
    var   <- input$selBar
    cant  <- shiny::isolate(input$cant.cluster)
    color <- sapply(1:cant, function(i) 
      paste0("'", shiny::isolate(input[[paste0("hcColor", i)]]), "'"))
    updatePlot$cat <- cluster.cat(var, color)
  })
  
  #' Actualizacion del Modelo K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$KRun), {
    datos            <- updateData$datos
    cant             <- shiny::isolate(input$cant.kmeans.cluster)
    iter.max         <- shiny::isolate(input$num.iter)
    nstart           <- shiny::isolate(input$num.nstart)
    algorithm        <- shiny::isolate(input$sel.algoritmo)
    cant.numericas   <- ncol(var.numericas(datos))
    cant.categoricas <- ncol(var.categoricas(datos))
    var.horiz        <- shiny::isolate(input$sel.Khoriz)
    var.vert         <- shiny::isolate(input$sel.Kvert)
    var.cat          <- shiny::isolate(input$sel.Kbar)
    color <- sapply(1:cant, function(i) 
      paste0("'", input[[paste0("kColor", i)]], "'"))
    
    rep.k <<- c(cant, iter.max, nstart, algorithm) 
    
    codigo <- def.k.model(data = "datos", cant, iter.max, nstart, algorithm)
    shinyAce::updateAceEditor(session, "fieldCodeKModelo", value = codigo)
    
    tryCatch ({
      if(!is.null(datos) && !is.null(cant)) {
        eval(parse(text = codigo))
        updateData$k.modelo <- k.modelo
        output$txtk <- shiny::renderPrint(print(unclass(k.modelo)))
        createLogK(nombre.datos, codigo, rep.k, "modelo")
        
        output$inercia.k = shiny::renderUI({
          panel.inercia(esHC = F, k.modelo)
        })
        updatePlot$Kmapa  <- cluster.mapa(color, F)
        updatePlot$Khoriz <- cluster.horiz(var.horiz, color, F)
        updatePlot$Kvert  <- cluster.vert(var.vert, color, F)
        updatePlot$Kradar <- cluster.radar(color, F)
        updatePlot$Kcat   <- cluster.cat(var.cat, color, F)
      }
    }, error = function(e) {
      mostrarError(e)
    })
  })
  
  #' Gráfico de K-medias (Codo de Jambu)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.jambu = shiny::renderPlot({
    datos <- updateData$datos
    code.jambu <- updatePlot$jambu
    datos      <- shiny::isolate(updateData$datos)
    k.modelo   <- shiny::isolate(updateData$k.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeJambu", value = code.jambu)
    tryCatch({
      grafico <- eval(parse(text = code.jambu))
      createLogK(nombre.datos, code.jambu, rep.k, "Jambu")
      return(grafico)
    }, error = function(e) {
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })

  shiny::observeEvent(input$run.Jambu, {
    updatePlot$jambu <- input$fieldCodeJambu
  })

  shiny::observeEvent(c(updateData$datos, input$iteracionesK), {
    updatePlot$jambu <- def.code.jambu(
      k = input$iteracionesK, tituloy = tr("inerciainter"))
  })

  #' Gráfico de K-medias (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kmapa = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$Kmapa
    pca.modelo <- shiny::isolate(updateData$pca.modelo)
    k.modelo <- shiny::isolate(updateData$k.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeKmapa", value = codigo)
    tryCatch({
      grafico.k.mapa <<- eval(parse(text = codigo))
      createLogK(nombre.datos, code, rep.k, "Mapa")
      return(grafico.k.mapa)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.Kmapa, {
    updatePlot$Kmapa <- shiny::isolate(input$fieldCodeKmapa)
  })

  output$plot.kmapa.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- kmapa.ranges$x
      ejey <- kmapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        grafico.k.mapa + 
          coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position="none")
      }
    }, error = function(e) {
      return(NULL)
    })
  })

  output$mostrar.kmapa.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(
        df = dimensiones[, c(1, 2)], brush = input$zoom.kmapa,
        xvar = names(dimensiones)[1], yvar = names(dimensiones)[2]))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  shiny::observe({
    brush <- input$zoom.kmapa
    if (!is.null(brush)) {
      kmapa.ranges$x <- c(brush$xmin, brush$xmax)
      kmapa.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      kmapa.ranges$x <- NULL
      kmapa.ranges$y <- NULL
    }
  })

  #' Gráfico de K-medias (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.khoriz = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$Khoriz
    k.modelo <- shiny::isolate(updateData$k.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeKhoriz", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      var <- shiny::isolate(input$sel.Khoriz)
      createLogK(nombre.datos, codigo, rep.k, paste0("Horizontal: ", var))
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.Khoriz, {
    updatePlot$Khoriz <- shiny::isolate(input$fieldCodeKhoriz)
  })
  
  shiny::observeEvent(input$sel.Khoriz, {
    var   <- input$sel.Khoriz
    cant  <- shiny::isolate(input$cant.kmeans.cluster)
    color <- sapply(1:cant, function(i) 
      paste0("'", shiny::isolate(input[[paste0("kColor", i)]]), "'"))
    if(!var %in% c("", "todos")) color <- color[as.numeric(var)]
    updatePlot$Khoriz <- cluster.horiz(esHC = F, var, color)
  })

  #' Gráfico de K-medias (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kvert = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$Kvert
    k.modelo <- shiny::isolate(updateData$k.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeKvert", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      var <- shiny::isolate(input$sel.Kvert)
      createLogK(nombre.datos, codigo, rep.k, paste0("Vertical: ", var))
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.KVert, {
    updatePlot$Kvert <- shiny::isolate(input$fieldCodeKvert)
  })
  
  shiny::observeEvent(input$sel.Kvert, {
    var   <- input$sel.Kvert
    cant  <- shiny::isolate(input$cant.kmeans.cluster)
    color <- sapply(1:cant, function(i) 
      paste0("'", shiny::isolate(input[[paste0("kColor", i)]]), "'"))
    updatePlot$Kvert <- cluster.vert(esHC = F, var, color)
  })

  #' Gráfico de K-medias (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kradar = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$Kradar
    k.modelo <- shiny::isolate(updateData$k.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeKradar", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      createLogK(nombre.datos, codigo, rep.k, "Radar")
      return(grafico)
    }, error = function(e) {
      datos <- shiny::isolate(updateData$datos)
      mostrarError(e, ncol(var.numericas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.Kradar, {
    updatePlot$Kradar <- shiny::isolate(input$fieldCodeKradar)
  })

  #' Gráfico de K-medias (Categórico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kcat = shiny::renderPlot({
    datos <- updateData$datos
    codigo <- updatePlot$Kcat
    k.modelo <- shiny::isolate(updateData$k.modelo)
    shinyAce::updateAceEditor(session, "fieldCodeKbar", value = codigo)
    tryCatch({
      grafico <- eval(parse(text = codigo))
      var <- shiny::isolate(input$sel.Kbar)
      createLogK(nombre.datos, codigo, rep.k, paste0("Categoricas: ", var))
      return(grafico)
    }, error = function(e) {
      mostrarError(e, ncol(var.categoricas(datos)))
    })
  })
  
  shiny::observeEvent(input$run.Kbar, {
    updatePlot$Kcat <- shiny::isolate(input$fieldCodeKbar)
  })
  
  shiny::observeEvent(input$sel.Kbar, {
    var   <- input$sel.Kbar
    cant  <- shiny::isolate(input$cant.kmeans.cluster)
    color <- sapply(1:cant, function(i) 
      paste0("'", shiny::isolate(input[[paste0("kColor", i)]]), "'"))
    updatePlot$Kcat <- cluster.cat(esHC = F, var, color)
  })

  #' Mostrar Colores (k-means)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$cant.kmeans.cluster), {
    datos <- updateData$datos
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
  shiny::observeEvent(c(updateData$datos, input$cant.cluster), {
    datos <- updateData$datos
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

  shiny::observeEvent(input$HCbutton, {
    datos <- shiny::isolate(updateData$datos)
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    ifelse(input$idioma == "es", aux <- "CJ", aux <- "HC")
    C.Jerarquica <- as.factor(paste0(aux, hc.modelo$clusters))
    datos[[paste0(length(levels(C.Jerarquica)), ".", aux)]] <- C.Jerarquica
    updateData$datos <<- datos
    shiny::showNotification(tr("msjclusters"), duration = 5, type = "message")
  })

  shiny::observeEvent(input$Kbutton, {
    datos <- shiny::isolate(updateData$datos)
    k.modelo <- shiny::isolate(updateData$k.modelo)
    ifelse(input$idioma == "es", aux <- ".Kmedias", aux <- ".Kmeans")
    Kmedias <- as.factor(paste0("K", k.modelo$cluster))
    datos[[paste0(length(levels(Kmedias)), aux)]] <- Kmedias
    shiny::showNotification(tr("msjclusters"), duration = 5, type = "message")
    updateData$datos <<- datos
  })

  output$downloaDatos <- shiny::downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      datos <- shiny::isolate(updateData$datos)
      write.csv(datos, file, row.names = input$rowname)
    }
  )

  shiny::observeEvent(c(input$principal, input$idioma), {
    if(input$principal == "reporte"){
      shinyAce::updateAceEditor(session, "fieldCodeReport", value = user.reporte())
    }
  })

  output$descargar <- shiny::downloadHandler(
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

      utils::zip(file, files)
    }
  )
})
