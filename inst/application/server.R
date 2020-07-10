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
    datos = NULL, pca.modelo = NULL, hc.modelo = NULL, k.modelo = NULL)

  disp.ranges  <- shiny::reactiveValues(x = NULL, y = NULL)
  ind.ranges   <- shiny::reactiveValues(x = NULL, y = NULL)
  bi.ranges    <- shiny::reactiveValues(x = NULL, y = NULL)
  mapa.ranges  <- shiny::reactiveValues(x = NULL, y = NULL)
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
    rowname    <- shiny::isolate(input$rowname)
    ruta       <- shiny::isolate(input$file1)
    sep        <- shiny::isolate(input$sep)
    dec        <- shiny::isolate(input$dec)
    encabezado <- shiny::isolate(input$header)
    deleteNA   <- shiny::isolate(input$deleteNA)
    tryCatch({
      codigo.carga <- code.carga(
        rowname, ruta$datapath, sep, dec, encabezado, deleteNA)
      if (isTRUE(getOption("shiny.testmode"))) {
        shinyAce::updateAceEditor(session, "fieldCodeData", value = "codigo")
      } else {
        shinyAce::updateAceEditor(session, "fieldCodeData", value = codigo.carga)
      }
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
    
    if (isTRUE(getOption("shiny.testmode"))) {
      code.res <- codigo.prueba()
      shinyAce::updateAceEditor(session, "fieldCodeTrans", value = code.res)
    } else {
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
          code.res <- paste0(code.res, code.desactivar(var), "\n")
        }
      }
      
      shinyAce::updateAceEditor(session, "fieldCodeTrans", value = code.res)
    }
    
    eval(parse(text = code.res))
    nombre.datos <<- paste0(nombre.datos, paste(rep.vars, collapse = "."))
    datos.reporte[[nombre.datos]] <<- datos
    init.replist(nombre.datos)
    createLogBasico(nombre.datos, "Transformacion de los Datos", "str(datos)")
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
    datos <- shiny::isolate(updateData$datos)
    updateLabelInput(
      session, 
      c("idioma", "selidioma", "data", "basico", "resumen", "normalidad",
        "dispersion", "distribucion", "correlacion", "acp", "jerarquico",
        "kmedias", "reporte", "acercade", "cargar", "trans", "resumen", "info",
        "resumenvar", "normalidad", "plotnormal", "numericas", "categoricas", 
        "resultados", "individuos", "variables", "sobreposicion", "ayudacp", 
        "vee", "cci", "ccv", "cvc", "cp", "inercia", "dendograma", "fisher",
        "mapa", "horizontal", "vertical", "radar", "interpretacioncat", "escalar",
        "jambu", "tituloreporte", "codreporte",  "salida", "copyright", 
        "version", "opciones", "ejecutar", "selcolor", "selcolores", "selejes",
        "cargarchivo", "subir", "header", "Rownames", "separador", "puntocoma",
        "coma", "tab", "punto", "separadordec", "eliminana", "centrar", "si", 
        "no", "descargar", "aplicar", "agregarcluster", "selvar", "selmetodo",
        "seltipo", "cantcluster", "numcluster", "algoritmo", "nstart", "niter",
        "numerodim", "cosind", "cosvar", "titulo", "nombre", "codigo", "kiter",
        "funciones", "metododist", "buscar", "nodata", "anterior", "selvars",
        "siguiente", "primero", "ultimo", "numerico", "categorico", "codedist",
        "codecentros", "codehoriz", "codevert", "coderadar", "codejambu", 
        "inercia", "inerciainter", "inerciaintra", "todos", "asimetria", 
        "metcluster", "nocentrar", "eliminar", "imputar", "jambu", "sil")
    )
    
    updateinitSelects("selHoriz", 1:input$cant.cluster)
    updateinitSelects("sel.Khoriz", 1:input$cant.kmeans.cluster)
    updateinitSelects("selVert", colnames(var.numericas(datos)))
    updateinitSelects("sel.Kvert", colnames(var.numericas(datos)))
  })
  
  output$contents = DT::renderDataTable({
    datos <- updateData$datos
    datos <- head(datos, 100)
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
  }, server = T)

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
    tipo      <- tr("tipo")
    activa    <- tr("activa")
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
    var   <- input$sel.normal
    color <- input$col.normal
    cod.normal <- default.normal(var, color, tr("curvanormal"))
    tryCatch({
      grafico <- eval(parse(text = cod.normal))
      shinyAce::updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
      createLogBasico(nombre.datos, "normalidad", cod.normal, var)
      return(grafico)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })

  #' Resumen Test de normalidad
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$calculo.normal = DT::renderDT({
    datos <- updateData$datos
    labelpos <- tr("positivo")
    labelneg <- tr("negativo")
    labelsin <- tr("sinasimetria")
    codigo <- default.calc.normal(labelpos, labelneg, labelsin)
    tryCatch({
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

  #' Gráfico de Dispersión
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.disp = shiny::renderPlot({
    datos <- updateData$datos
    var   <- input$select.var
    color <- input$col.disp
    cod.disp <- default.disp(var, color)
    tryCatch({
      grafico.disp <<- eval(parse(text = cod.disp))
      shinyAce::updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
      if(cod.disp != "") {
        createLogBasico(
          nombre.datos, "dispersion", cod.disp, paste(var, collapse = "-"))
      }
      return(grafico.disp)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })
  
  output$plot.disp.zoom <- shiny::renderPlot({
    tryCatch({
      grafico.disp + coord_cartesian(
        xlim = disp.ranges$x, ylim = disp.ranges$y, expand = FALSE)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$mostrar.disp.zoom = DT::renderDataTable({
    datos <- shiny::isolate(updateData$datos)
    var   <- shiny::isolate(input$select.var)
    tryCatch({
      DT::datatable(
        brushedPoints(datos[, var], input$zoom.disp),
        options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                       pageLength = nrow(datos)))
    }, error = function(e) {
      return(NULL)
    })
  })

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
  
  #' Gráfico de Distribuciones (Númericas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.num = shiny::renderPlot({
    datos <- updateData$datos
    var   <- input$sel.distribucion.num
    color <- input$col.dist  
    cod.dya.num <- def.code.num(var, color)
    shinyAce::updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
    tryCatch({
      grafico <- eval(parse(text = cod.dya.num))
      createLogBasico(nombre.datos, "distribucion", cod.dya.num, var)
      return(grafico)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })
  
  output$mostrar.atipicos = DT::renderDataTable({
    datos <- updateData$datos
    var   <- input$sel.distribucion.num
    atipicos <- boxplot.stats(datos[, var])
    datos <- datos[datos[, var] %in% atipicos$out, var, drop = F]
    datos <- datos[order(datos[, var]), , drop = F]
    DT::datatable(datos, options = list(
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
    var   <- input$sel.distribucion.cat
    cod.dya.cat <- def.code.cat(var)
    shinyAce::updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
    tryCatch({
      grafico <- eval(parse(text = cod.dya.cat))
      createLogBasico(nombre.datos, "distribucion", cod.dya.cat, var)
      return(grafico)
    }, error = function(e) {
      mostrarError(e, n.cat = ncol(var.categoricas(datos)))
    })
  })
  
  #' Gráfico de Correlaciones
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.cor = shiny::renderPlot({
    datos       <- updateData$datos
    metodo      <- input$cor.metodo
    tipo        <- input$cor.tipo
    cod.cor     <- correlaciones(metodo, tipo)
    shinyAce::updateAceEditor(session, "fieldCodeCor", value = cod.cor)
    tryCatch({
      output$txtcor <- shiny::renderPrint(cor(var.numericas(datos)))
      grafico <- shiny::isolate(eval(parse(text = cod.cor)))
      createLogBasico(nombre.datos, "correlacion", cod.cor)
      return(grafico)
    }, error = function(e) {
      mostrarError(e, n.num = ncol(var.numericas(datos)))
    })
  })
  
  #' Update PCA
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$run.pca), {
    datos <- updateData$datos
    
    centrado    <- shiny::isolate(input$switch.scale)
    dimensiones <- shiny::isolate(input$slider.npc)
    ejes        <- paste(shiny::isolate(input$slider.ejes), collapse = ", ")
    ind.cos     <- shiny::isolate(input$ind.cos) * 0.01
    ind.col     <- shiny::isolate(input$col.pca.ind)
    var.cos     <- shiny::isolate(input$var.cos) * 0.01
    var.col     <- shiny::isolate(input$col.pca.var) 
    
    codigo <- def.pca.model(scale.unit = centrado, npc = dimensiones)
    rep.acp <<- c(centrado, dimensiones)
    
    shinyAce::updateAceEditor(session, "fieldCodePCAModelo", value = codigo)
    
    if(!is.null(datos)) {
      eval(parse(text = codigo))
      if(!is.null(pca.modelo)) {
        output$txtpca <- shiny::renderPrint(unclass(pca.modelo))
        createLogACP(nombre.datos, codigo, rep.acp, "modelo")
        updateSelectInput(session, "pc1.dim", choices = 1:dimensiones)
        
        # Gráfico PCA Individuos
        output$plot.ind = shiny::renderPlot({
          codigo <- pca.individuos(ind.cos, ind.col, ejes)
          shinyAce::updateAceEditor(session, "fieldCodeInd", value = codigo)
          tryCatch({
            grafico.ind <<- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp,
                         paste0("repcaind=ejes:(", ejes, ");cosind:", ind.cos))
            return(grafico.ind)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico PCA Variables
        output$plot.var = shiny::renderPlot({
          codigo <- pca.variables(var.cos, var.col, ejes)
          shinyAce::updateAceEditor(session, "fieldCodeVar", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp,
                         paste0("repcavar=ejes:(", ejes, ");cosvar:", var.cos))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico PCA Sobreposición
        output$plot.biplot = shiny::renderPlot({
          codigo <- pca.sobreposicion(ind.cos, var.cos, ind.col, var.col, ejes)
          shinyAce::updateAceEditor(session, "fieldCodeBi", value = codigo)
          tryCatch({
            grafico.bi <<- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp, paste0(
              "repcabi=ejes:(", ejes, ");cosvar:", var.cos, ";cosind:", ind.cos))
            return(grafico.bi)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        #' Gráfico de PCA (Varianza Explicada para cada Eje)
        output$plotVEE = shiny::renderPlot({
          codigo <- code.pca.vee(tr("vee"), tr("dimensiones"), tr("porcvee"))
          shinyAce::updateAceEditor(session, "fieldCodeVEE", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp, "vee")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        #' Gráfico de PCA (Cosenos Cuadrados de los individuos)
        output$plotCCI = shiny::renderPlot({
          codigo <- code.pca.cci(tr("cci"), tr("calidadcos"))
          shinyAce::updateAceEditor(session, "fieldCodeCCI", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp, "cci")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        #' Gráfico de PCA (Cosenos Cuadrados de las Variables)
        output$plotCCV = shiny::renderPlot({
          codigo <- code.pca.ccv(tr("ccv"), tr("calidadcos"))
          shinyAce::updateAceEditor(session, "fieldCodeCCV", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp, "ccv")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        #' Gráfico de PCA (Correlación Variables con los Componenetes)
        output$plotCVC = shiny::renderPlot({
          codigo <- code.pca.cvp(input$cvc.metodo, tr("cvc"))
          shinyAce::updateAceEditor(session, "fieldCodeCVC", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp, "cvc")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        #' Gráfico de PCA (Contribución de las variables de la Dimensión)
        output$plotPC1 = shiny::renderPlot({
          dim <- input$pc1.dim
          codigo <- code.pca.pc1(tr("cp"), tr("contribucion"), dim)
          shinyAce::updateAceEditor(session, "fieldCodePC1", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogACP(nombre.datos, codigo, rep.acp, paste0("cp:", dim))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        updateData$pca.modelo <- pca.modelo
      }
    }
  })
  
  observeEvent(input$slider.npc, {
    updateSliderInput(session, "slider.ejes", max = input$slider.npc, 
                      value = c(1, 2))
  })

  #' Zoom PCA (Individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'
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
       pca.modelo  <- shiny::isolate(updateData$pca.modelo)
       dimensiones <- as.data.frame(pca.modelo$ind$coord)
       ejes <- shiny::isolate(input$slider.ejes)
       res <- brushedPoints(
         df = dimensiones[, c(ejes)], brush = input$zoom.ind,
         xvar = names(dimensiones)[ejes[1]],
         yvar = names(dimensiones)[ejes[2]])
       return(DT::datatable(res, options = list(
         dom = 't', scrollX = TRUE, scrollY = "20vh",
         pageLength = nrow(dimensiones))))
     }, error = function(e) {
       return(NULL)
     })
   })

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

  #' Zoom PCA (Sobreposición)
  #' @author Diego
  #' @return plot
  #' @export
  #'
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
      pca.modelo <- shiny::isolate(updateData$pca.modelo)
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      ejes <- shiny::isolate(input$slider.ejes)
      res <- brushedPoints(
        df = dimensiones[, c(input$slider.ejes)], brush = input$zoom.bi,
        xvar = names(dimensiones)[input$slider.ejes[1]],
        yvar = names(dimensiones)[input$slider.ejes[2]])
      return(DT::datatable(res, options = list(
        dom = 't', scrollX = TRUE, scrollY = "20vh",
        pageLength = nrow(dimensiones))))
    }, error = function(e) {
      return(NULL)
    })
  })

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
  
  #' Actualización del Modelo Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$run.hc), {
    datos       <- updateData$datos
    centrar     <- shiny::isolate(input$cj.scale)
    cant        <- shiny::isolate(input$cant.cluster)
    dist.method <- shiny::isolate(input$sel.dist.method)
    hc.method   <- shiny::isolate(input$sel.hc.method)
    colores     <- sapply(1:cant, function(i)
      paste0("'", input[[paste0("hcColor", i)]], "'"))
    
    codigo <- def.model(cant, dist.method, hc.method, centrar)
    shinyAce::updateAceEditor(session, "fieldCodeModelo", value = codigo)
    
    tryCatch ({
      if(!is.null(datos) && !is.null(cant)) {
        eval(parse(text = codigo))
        updateData$hc.modelo <- hc.modelo
        
        updateinitSelects("selHoriz", 1:cant)
        output$txthc <- shiny::renderPrint(print(unclass(hc.modelo)))
        output$txtcentros <- shiny::renderPrint(print(unclass(centros)))
        rep.hc <- c(cant, dist.method, hc.method)
        createLogCJ(nombre.datos, codigo, rep.hc, "modelo")
        
        output$inercia.cj = shiny::renderUI({
          panel.inercia(hc.modelo$modelo, as.numeric(cant), datos, centrar = centrar)
        })
        
        # Gráfico de Clusterización Jerarquica (Diagrama)
        output$plot.diag = shiny::renderPlot({
          codigo <- diagrama(colores)
          shinyAce::updateAceEditor(session, "fieldCodeDendo", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogCJ(nombre.datos, codigo, rep.hc, "dendograma")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de Clusterización Jerarquica (Mapa)
        output$plot.mapa = shiny::renderPlot({
          codigo     <- cluster.mapa(colores)
          pca.modelo <- shiny::isolate(updateData$pca.modelo)
          shinyAce::updateAceEditor(session, "fieldCodeMapa",  value = codigo)
          tryCatch({
            grafico.hc.mapa <<- eval(parse(text = codigo))
            createLogCJ(nombre.datos, codigo, rep.hc, "mapa")
            return(grafico.hc.mapa)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de Clusterización Jerarquica (Horizontal)
        output$plot.horiz = shiny::renderPlot({
          var    <- input$selHoriz
          porc   <- input$checkHoriz
          codigo <- cluster.horiz(var, colores, porc = porc)
          shinyAce::updateAceEditor(session, "fieldCodeHoriz", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogCJ(nombre.datos, codigo, rep.hc, paste0("horizontal:", var))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de Clusterización Jerarquica (Vertical)
        output$plot.vert = shiny::renderPlot({
          var    <- input$selVert
          porc   <- input$checkVert
          codigo <- cluster.vert(var, colores, porc = porc)
          shinyAce::updateAceEditor(session, "fieldCodeVert",  value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogCJ(nombre.datos, codigo, rep.hc, paste0("vertical:", var))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de Clusterización Jerarquica (Radar)
        output$plot.radar = shiny::renderPlot({
          codigo <- cluster.radar(colores)
          shinyAce::updateAceEditor(session, "fieldCodeRadar", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogCJ(nombre.datos, codigo, rep.hc, "radar")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de Clusterización Jerarquica (Catégoricas)
        output$plot.bar.cat = shiny::renderPlot({
          var    <- input$selBar
          porc <- input$checkBar
          codigo <- cluster.cat(var, colores, porc = porc)
          shinyAce::updateAceEditor(session, "fieldCodeBar", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogCJ(nombre.datos, codigo, rep.hc, paste0("categoricas:", var))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, n.cat = ncol(var.categoricas(datos)))
          })
        })
      }
    }, error = function(e) {
      mostrarError(e)
    })
  })

  #' Zoom de Clusterización Jerarquica (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
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
      pca.modelo <- shiny::isolate(updateData$pca.modelo)
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      res <- brushedPoints(
        df = dimensiones[, c(1, 2)], brush = input$zoom.mapa,
        xvar = names(dimensiones)[1], yvar = names(dimensiones)[2])
      return(DT::datatable(res, options = list(
        dom = 't', scrollX = TRUE, scrollY = "20vh",
        pageLength = nrow(dimensiones))))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  })

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
  
  #' Actualizacion del Modelo K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$run.k), {
    datos     <- updateData$datos
    centrar   <- shiny::isolate(input$k.scale)
    cant      <- shiny::isolate(input$cant.kmeans.cluster)
    iter.max  <- shiny::isolate(input$num.iter)
    nstart    <- shiny::isolate(input$num.nstart)
    algorithm <- shiny::isolate(input$sel.algoritmo)
    colores   <- sapply(1:cant, function(i) 
      paste0("'", input[[paste0("kColor", i)]], "'"))
    
    codigo <- def.k.model(cant, iter.max, nstart, algorithm, centrar)
    shinyAce::updateAceEditor(session, "fieldCodeKModelo", value = codigo)
    
    tryCatch ({
      if(!is.null(datos) && !is.null(cant)) {
        eval(parse(text = codigo))
        updateData$k.modelo <- k.modelo
        updateinitSelects("sel.Khoriz", 1:cant)
        output$txtk <- shiny::renderPrint(print(unclass(k.modelo)))
        rep.k <- c(cant, iter.max, nstart, algorithm) 
        createLogK(nombre.datos, codigo, rep.k, "modelo")
        
        # Gráfico de K-medias (Inercia)
        output$inercia.k = shiny::renderUI({
          panel.inercia(esHC = F, k.modelo)
        })
        
        # Gráfico de K-medias (Jambu)
        output$plot.jambu = shiny::renderPlot({
          metodo <- input$radiojambu
          codigo <- def.code.jambu(calc.maxK(datos), metodo)
          shinyAce::updateAceEditor(session, "fieldCodeJambu", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogK(nombre.datos, codigo, rep.k, "numcluster")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de K-medias (Mapa)
        output$plot.kmapa = shiny::renderPlot({
          codigo     <- cluster.mapa(colores, F)
          pca.modelo <- shiny::isolate(updateData$pca.modelo)
          shinyAce::updateAceEditor(session, "fieldCodeKmapa", value = codigo)
          tryCatch({
            grafico.k.mapa <<- eval(parse(text = codigo))
            createLogK(nombre.datos, codigo, rep.k, "mapa")
            return(grafico.k.mapa)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de K-medias (Horizontal)
        output$plot.khoriz = shiny::renderPlot({
          var    <- input$sel.Khoriz
          porc   <- input$checkKhoriz
          codigo <- cluster.horiz(var, colores, F, porc)
          shinyAce::updateAceEditor(session, "fieldCodeKhoriz", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogK(nombre.datos, codigo, rep.k, paste0("horizontal:", var))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de K-medias (Vertical)
        output$plot.kvert = shiny::renderPlot({
          var    <- input$sel.Kvert
          porc   <- input$checkKvert
          codigo <- cluster.vert(var, colores, F, porc)
          shinyAce::updateAceEditor(session, "fieldCodeKvert", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogK(nombre.datos, codigo, rep.k, paste0("vertical:", var))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        # Gráfico de K-medias (Radar)
        output$plot.kradar = shiny::renderPlot({
          codigo <- cluster.radar(colores, F)
          shinyAce::updateAceEditor(session, "fieldCodeKradar", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogK(nombre.datos, codigo, rep.k, "radar")
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.numericas(datos)))
          })
        })
        
        #' Gráfico de K-medias (Categórico)
        #' @author Diego
        #' @return plot
        #' @export
        #'
        output$plot.kcat = shiny::renderPlot({
          var    <- input$sel.Kbar
          porc   <- input$checkKbar
          codigo <- cluster.cat(var, colores, F, porc = porc)
          shinyAce::updateAceEditor(session, "fieldCodeKbar", value = codigo)
          tryCatch({
            grafico <- eval(parse(text = codigo))
            createLogK(nombre.datos, codigo, rep.k, paste0("categoricas:", var))
            return(grafico)
          }, error = function(e) {
            mostrarError(e, ncol(var.categoricas(datos)))
          })
        })
      }
    }, error = function(e) {
      mostrarError(e)
    })
  })
  
  #' Zoom K-medias (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
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
      pca.modelo  <- shiny::isolate(updateData$pca.modelo)
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      res <- brushedPoints(
        df = dimensiones[, c(1, 2)], brush = input$zoom.kmapa,
        xvar = names(dimensiones)[1], yvar = names(dimensiones)[2])
      return(DT::datatable(res, options = list(
        dom = 't', scrollX = TRUE, scrollY = "20vh",
        pageLength = nrow(dimensiones))))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  })

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
  
  #' Mostrar Colores (Cluster jerarquico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$cant.cluster, {
    cant <- input$cant.cluster
    if(!is.null(cant)) {
      mostrar.colores("hcColor", cant)
    }
  })

  #' Mostrar Colores (k-means)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$cant.kmeans.cluster, {
    cant <- input$cant.kmeans.cluster
    if(!is.null(cant)) {
      mostrar.colores("kColor", cant)
    }
  })

  #' Agregar Clusteres a tabla de datos (Cluster jerarquico)
  #' @author Diego
  #' @return nothing
  #' @export
  #'
  shiny::observeEvent(input$HCbutton, {
    datos <- shiny::isolate(updateData$datos)
    hc.modelo <- shiny::isolate(updateData$hc.modelo)
    idioma <- shiny::isolate(input$idioma)
    updateData$datos <- clusters.variable(datos, hc.modelo$clusters, idioma)
  })

  #' Agregar Clusteres a tabla de datos (K-medias)
  #' @author Diego
  #' @return nothing
  #' @export
  #'
  shiny::observeEvent(input$Kbutton, {
    datos <- shiny::isolate(updateData$datos)
    k.modelo <- shiny::isolate(updateData$k.modelo)
    idioma <- shiny::isolate(input$idioma)
    updateData$datos <- clusters.variable(datos, k.modelo$cluster, idioma, F)
  })

  #' Descarga tabla de datos
  #' @author Diego
  #' @return nothing
  #' @export
  #'
  output$downloaDatos <- shiny::downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      datos <- shiny::isolate(updateData$datos)
      write.csv(datos, file, row.names = input$rowname)
    }
  )

  #' Actualizar Reporte
  #' @author Diego
  #' @return nothing
  #' @export
  #'
  shiny::observeEvent(
    c(input$principal, input$idioma, input$textNombre, input$textTitulo), {
      titulo  <- input$textTitulo
      nombre  <- input$textNombre
      repcode <- paste0(def.reporte(titulo, nombre), user.reporte())
      if(input$principal == "reporte") {
        shinyAce::updateAceEditor(session, "fieldCodeReport", value = repcode)
      }
  })

  #' Generar Reporte
  #' @author Diego
  #' @return nothing
  #' @export
  #'
  output$descargar <- shiny::downloadHandler(
    filename = function() {
      paste(input$textTitulo,'-', input$textNombre, '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;

      namermd <- paste(input$textTitulo,'-', input$textNombre, '.rmd', sep='')
      reporte <- input$fieldCodeReport
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
      })
      recover.cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)

      zip::zipr(file, files)
    }
  )
})
