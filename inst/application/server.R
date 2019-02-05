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
    session, "fieldFuncRadar", paste0(extract.code("coord_radar"), "\n",
                                      extract.code("centros.radar")))
  shinyAce::updateAceEditor(session, "fieldFuncKhoriz", 
                            extract.code("centros.horizontal.todos"))
  shinyAce::updateAceEditor(session, "fieldFuncKvert", 
                            extract.code("centros.vertical.todos"))
  shinyAce::updateAceEditor(
    session, "fieldFuncKradar", paste0(extract.code("coord_radar"), "\n", 
                                       extract.code("centros.radar")))
  
  updateData <- shiny::reactiveValues(
    datos = NULL, cor.modelo = NULL, pca.modelo = NULL,
    hc.modelo = NULL, k.modelo = NULL)

  updatePlot <- shiny::reactiveValues(
    calc.normal=default.calc.normal(), normal=NULL, disp=NULL, pca.ind=NULL,
    pca.var=NULL, pca.bi=NULL, cor=NULL, pca.cvc=NULL, mapa=NULL, dya.num=NULL,
    dya.cat=NULL, diag=NULL, horiz=NULL, vert=NULL, radar=NULL, cat=NULL,
    jambu=NULL, kmapa=NULL, khoriz=NULL, kvert=NULL, kradar=NULL, kcat=NULL)

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
    codigo.carga <- ""
    codigo.na <- ""
    tryCatch({
      codigo.carga <- code.carga(
        nombre.filas = input$rowname, ruta = input$file1$datapath,
        separador = input$sep, sep.decimal = input$dec,
        encabezado = input$header)
      datos.originales <<- shiny::isolate(eval(parse(text = codigo.carga)))
      if(ncol(datos.originales) <= 1){
        shiny::showNotification(
          paste0("ERROR: Check Separators"), duration = 10, type = "error")
        return(NULL)
      }
      if(any(is.na(datos.originales))) {
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA))
        shiny::isolate(eval(parse(text = codigo.na)))
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
      shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      updateData$datos <- NULL
      datos.originales <<- NULL
      return(NULL)
    })
    shinyAce::updateAceEditor(
      session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))
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
    createLogBasico(nombre.datos, "Transformacion de los Datos",
                    paste0(code.res, "\nstr(datos)"))
    shiny::isolate(eval(parse(text = code.res)))
    shinyAce::updateAceEditor(session, "fieldCodeTrans", value = code.res)
    updateData$datos <- datos
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
  
  #' Update on Data
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(updateData$datos, {
    datos <<- updateData$datos
    updateMenu(datos)
    tryCatch({
      shiny::isolate(eval(parse(text = modelo.cor())))
      updateData$cor.modelo <- correlacion
      output$txtcor <- shiny::renderPrint(print(correlacion))
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
  
  #' Update on Table
  #' @author Diego
  #' @return functions
  #' @export
  #'
  mostrarData <- function() {
    if(shiny::isolate(input$idioma) == "es") {
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
        tags$tr(tags$th(), lapply(tipo.columnas, function(i) tags$th(shiny::HTML(i))))
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
      shiny::showNotification(
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
    createLogBasico(nombre.datos, "resumen", "summary(datos)")
    data.frame(unclass(summary(datos)), check.names = FALSE,
               stringsAsFactors = FALSE)
  })

  output$resumen = shiny::renderUI({
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
    tryCatch({
      cod.normal <<- updatePlot$normal
      res <- shiny::isolate(eval(parse(text = cod.normal)))
      shinyAce::updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
      createLogBasico(nombre.datos, "normalidad", cod.normal, input$sel.normal)
      return(res)
    }, error = function(e){
      if(ncol(var.numericas(datos)) <= 0){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
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
    tryCatch({
      datos <- updateData$datos
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
      shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
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
    tryCatch({
      cod.disp <<- updatePlot$disp
      shinyAce::updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
      if(!is.null(cod.disp) && cod.disp != "") {
        createLogBasico(nombre.datos, "dispersion", cod.disp, 
                        paste(input$select.var, collapse = "-"))
      }
      return(shiny::isolate(eval(parse(text = cod.disp))))
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
           return(NULL)
      }
    })
  })
  
  output$plot.disp.zoom <- shiny::renderPlot({
    tryCatch({
      cod.disp <<- updatePlot$disp
      res <- shiny::isolate(eval(parse(text = cod.disp)))
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
    tryCatch({
      cod.dya.num  <<- updatePlot$dya.num
      res <- shiny::isolate(eval(parse(text = cod.dya.num)))
      num.var <- shiny::isolate(input$sel.distribucion.num)
      shinyAce::updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
      createLogBasico(nombre.datos, "distribucion", cod.dya.num, num.var)
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 0){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
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
  output$plot.cat = shiny::renderPlot({
    tryCatch({
      cod.dya.cat  <<- updatePlot$dya.cat
      res <- shiny::isolate(eval(parse(text = cod.dya.cat)))
      shinyAce::updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
      cat.var <- shiny::isolate(input$sel.distribucion.cat)
      if(str_detect(var, pattern = "[[:digit:]]\\.(HC|CJ)") & 
         is.null(datos.originales[[var]])) {
        code <- paste0(
          "datos[['", var, "']] <- as.factor(paste0('HC', hc.modelo$clusters))",
          "\n", cod.dya.cat)
        #createLog(
        #  nombre.datos, "rephc", "Cluster", code,  
        #  params = paste(
        #    "Clusters=", shiny::isolate(input$cant.cluster), "distancia=", 
        #    shiny::isolate(input$sel.dist.method), "metodo=", 
        #    shiny::isolate(input$sel.hc.method), collapse = "."))
      } else if(str_detect(var, pattern = "[[:digit:]]\\.(Kmedias|Kmeans)") &
                is.null(datos.originales[[var]])) {
        code <- paste0(
          "datos[['", var, "']] <- as.factor(paste0('K', k.modelo$cluster))\n",
          cod.dya.cat)
        #createLog(
        #  nombre.datos, "kmedias", "Cluster", code, params = paste(
        #    "Clusters=", shiny::isolate(input$cant.kmeans.cluster), "iter=", 
        #    shiny::isolate(input$num.iter), "nstart=", shiny::isolate(input$num.nstart), 
        #    "algoritmo=", shiny::isolate(input$sel.algoritmo), collapse = "."))
      } else {
        createLogBasico(nombre.datos, "distribucion", cod.dya.cat, cat.var)
      }
      return(res)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(shiny::isolate(input$idioma), F)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
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
  output$plot.cor = shiny::renderPlot({
    tryCatch({
      updateData$cor.modelo
      cod.cor <- updatePlot$cor
      res <- shiny::isolate(eval(parse(text = cod.cor)))
      shinyAce::updateAceEditor(session, "fieldCodeCor", value = cod.cor)
      createLogBasico(nombre.datos, "correlacion", cod.cor)
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })
  
  shiny::observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })
  
  shiny::observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(
      metodo = input$cor.metodo, tipo = input$cor.tipo)
  })
  
  #' Update on PCA
  #' @author Diego
  #' @return functions
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$ACPRun), {
    datos <- updateData$datos
    centrado <- shiny::isolate(input$switch.scale)
    dimensiones <- shiny::isolate(input$slider.npc)
    codigo <- def.pca.model(scale.unit = centrado, npc = dimensiones)
    rep.acp <<- c(centrado, dimensiones)
    
    tryCatch({
      if(!is.null(datos)) {
        eval(parse(text = codigo))
        shinyAce::updateAceEditor(session, "fieldCodePCAModelo", value = codigo)
        updateData$pca.modelo <- pca.modelo
        output$txtpca <- shiny::renderPrint(print(unclass(pca.modelo)))
        updateSliderTextInput(session, "slider.ejes", choices =
                                c(1:input$slider.npc), selected = c(1, 2))
        createLogACP(nombre.datos, codigo, rep.acp, "modelo")
      }
    }, error = function(e) {
      shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  #' Gráfico de PCA (Individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.ind = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.ind <<- updatePlot$pca.ind
      cos <- shiny::isolate(input$ind.cos)
      ind.ejes <- paste(shiny::isolate(input$slider.ejes), collapse = ",")
      
      res <- shiny::isolate(eval(parse(text = cod.pca.ind)))
      shinyAce::updateAceEditor(session, "fieldCodeInd", value = cod.pca.ind)
      createLogACP(nombre.datos, cod.pca.ind, rep.acp,
                   paste0("Individuos ejes: ", ind.ejes, ", cos: ", cos))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
   })

   output$plot.ind.zoom <- shiny::renderPlot({
     tryCatch({
       ejex <- ind.ranges$x
       ejey <- ind.ranges$y
       if(is.null(ejex) & is.null(ejey)){
         return(NULL)
       } else {
         cod.ind <<- updatePlot$pca.ind
         res <- shiny::isolate(eval(parse(text = cod.ind)))
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

  shiny::observeEvent(input$run.pcaInd, {
    updatePlot$pca.ind <- input$fieldCodeInd
  })

  shiny::observeEvent(c(input$col.pca.ind, input$ind.cos, input$slider.ejes), {
    updatePlot$pca.ind <- pca.individuos(
      ind.cos = input$ind.cos * 0.01, color = input$col.pca.ind, 
      ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.var = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.var <<- updatePlot$pca.var
      cos <- shiny::isolate(input$ind.cos)
      ind.ejes <-paste(shiny::isolate(input$slider.ejes), collapse = ",")
      
      res <- shiny::isolate(eval(parse(text = cod.pca.var)))
      shinyAce::updateAceEditor(session, "fieldCodeVar", value = cod.pca.var)
      createLogACP(nombre.datos, cod.pca.var, rep.acp, 
                   paste0("Variables-ejes: ", ind.ejes, ", cos: ", cos))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  shiny::observeEvent(input$run.pcaVar, {
    updatePlot$pca.var <- input$fieldCodeVar
  })

  shiny::observeEvent(c(input$var.cos, input$col.pca.var, input$slider.ejes), {
    updatePlot$pca.var <- pca.variables(
      var.cos = input$var.cos * 0.01, color = input$col.pca.var,
      ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Sobreposición)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.biplot = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.bi <<- updatePlot$pca.bi
      cos <- shiny::isolate(input$ind.cos)
      ind.ejes <- paste(shiny::isolate(input$slider.ejes), collapse = ",")
      
      res <- shiny::isolate(eval(parse(text = cod.pca.bi)))
      shinyAce::updateAceEditor(session, "fieldCodeBi", value = cod.pca.bi)
      createLogACP(nombre.datos, cod.pca.bi, rep.acp, 
                   paste0("Sobreposicion-ejes: ", ind.ejes, ", cos: ", cos))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })
  
  output$plot.bi.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- bi.ranges$x
      ejey <- bi.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        cod.bi <<- updatePlot$pca.bi
        res <- shiny::isolate(eval(parse(text = cod.bi)))
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

  shiny::observeEvent(input$run.pcaBi, {
    updatePlot$pca.bi <- input$fieldCodeBi
  })

  shiny::observeEvent(c(input$col.pca.ind, input$ind.cos, input$var.cos,
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
  output$plotVEE = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.vee(tr("vee"), tr("dimensiones"), tr("porcvee"))
      shinyAce::updateAceEditor(session, "fieldCodeVEE", value = codigo)
      res <- shiny::isolate(eval(parse(text = codigo)))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Varianza Explicada para cada Eje")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de los individuos)
  #' @author Diego
  #' @return plot
  #' @export

  output$plotCCI = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.cci(tr("cci"), tr("calidadcos"))
      shinyAce::updateAceEditor(session, "fieldCodeCCI", value = codigo)
      res <- shiny::isolate(eval(parse(text = codigo)))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Cosenos Cuadrados de los individuos")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de las Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCCV = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.ccv(tr("ccv"), tr("calidadcos"))
      shinyAce::updateAceEditor(session, "fieldCodeCCV", value = codigo)
      res <- shiny::isolate(eval(parse(text = codigo)))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Cosenos Cuadrados de las Variables")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e),
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
  output$plotCVC = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- updatePlot$pca.cvc
      shinyAce::updateAceEditor(session, "fieldCodeCVC", value = codigo)
      res <- shiny::isolate(eval(parse(text = codigo)))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Correlación Variables con los Componenetes")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
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
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.pc1(tr("cp1"), tr("contribucion"))
      shinyAce::updateAceEditor(session, "fieldCodePC1", value = codigo)
      res <- shiny::isolate(eval(parse(text = codigo)))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Contribución de las variables de la Dimensión 1")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 2)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC2 = shiny::renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.pc2(tr("cp2"), tr("contribucion"))
      shinyAce::updateAceEditor(session, "fieldCodePC2", value = codigo)
      res <- shiny::isolate(eval(parse(text = codigo)))
      createLogACP(nombre.datos, codigo, rep.acp, 
                   "Contribución de las variables de la Dimensión 2")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Actualización del Modelo Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$CJRun), {
    cant <- shiny::isolate(input$cant.cluster)
    dist.method <- shiny::isolate(input$sel.dist.method)
    hc.method <- shiny::isolate(input$sel.hc.method)
    codigo <- def.model(data = "datos", cant, dist.method, hc.method)
    pca.modelo <<- shiny::isolate(updateData$pca.modelo)
    rep.hc <<- c(cant, dist.method, hc.method)
    
    tryCatch ({
      if(!is.null(datos) && !is.null(cant)) {
        eval(parse(text = codigo))
        shinyAce::updateAceEditor(session, "fieldCodeModelo", value = codigo)
        updateData$hc.modelo <- hc.modelo
        output$inercia.cj = shiny::renderUI({
          panel.inercia(hc.modelo$modelo, as.numeric(cant), datos = datos)
        })
        output$txthc <- shiny::renderPrint(print(unclass(hc.modelo)))
        output$txtcentros <- shiny::renderPrint(print(unclass(centros)))
        createLogCJ(nombre.datos, codigo, rep.hc, "modelo")
      }
    }, error = function(e) {
      shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Actualización de Gráficos Clusterización Jerarquica.
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(updateData$hc.modelo, {
    hc.modelo <<- updateData$hc.modelo
    cant <- shiny::isolate(input$cant.cluster)
    cant.numericas <- ncol(var.numericas(datos))
    cant.categoricas <- ncol(var.categoricas(datos))
    idioma <- shiny::isolate(input$idioma)
    var.horiz <- shiny::isolate(input$selHoriz)
    var.vert <- shiny::isolate(input$selVert)
    var.cat <- shiny::isolate(input$selBar)
    
    nuevos.colores <- sapply(1:cant, function(i)
      paste0("'", input[[paste0("hcColor", i)]], "'"))
    
    code.diag <- diagrama(cant, nuevos.colores)
    code.mapa <- cluster.mapa(nuevos.colores)
    code.horiz <- cluster.horiz(var.horiz, nuevos.colores)
    code.vert <- cluster.vert(var.vert, nuevos.colores)
    code.radar <- cluster.radar(nuevos.colores)
    code.cat <- cluster.cat(var.cat, nuevos.colores)
    
    shinyAce::updateAceEditor(session, "fieldCodeDendo", value = code.diag)
    shinyAce::updateAceEditor(session, "fieldCodeMapa", value = code.mapa)
    shinyAce::updateAceEditor(session, "fieldCodeHoriz", value = code.horiz)
    shinyAce::updateAceEditor(session, "fieldCodeVert", value = code.vert)
    shinyAce::updateAceEditor(session, "fieldCodeRadar", value = code.radar)
    shinyAce::updateAceEditor(session, "fieldCodeBar", value = code.cat)
    
    output$plot.diag = shiny::renderPlot({
      diag <- checkError(code.diag, idioma, cant.numericas)
      if(!is.null(diag))
        createLogCJ(nombre.datos, code.diag, rep.hc, "Dendograma")
      return(diag)
    })
    output$plot.mapa = shiny::renderPlot({
      updatePlot$mapa <- checkError(code.mapa, idioma, cant.numericas)
      if(!is.null(updatePlot$mapa))
        createLogCJ(nombre.datos, code.mapa, rep.hc, "Mapa")
      return(updatePlot$mapa)
    })
    output$plot.horiz = shiny::renderPlot({
      horiz <- checkError(code.horiz, idioma, cant.numericas)
      if(!is.null(horiz)) {
        rep.horiz <- paste0("Horizontal: ", var.horiz)
        createLogCJ(nombre.datos, code.horiz, rep.hc, rep.horiz)
      }
      return(horiz)
    })
    output$plot.vert = shiny::renderPlot({
      vert <- checkError(code.vert, idioma, cant.numericas)
      if(!is.null(vert)) {
        rep.vert <- paste0("Vertical: ", var.vert)
        createLogCJ(nombre.datos, code.vert, rep.hc, rep.vert)
      }
      return(vert)
    })
    output$plot.radar = shiny::renderPlot({
      radar <- checkError(code.radar, idioma, cant.numericas)
      if(!is.null(radar))
        createLogCJ(nombre.datos, code.radar, rep.hc, "Radar")
      return(radar)
    })
    output$plot.bar.cat = shiny::renderPlot({
      bar.cat <- checkError(code.cat, idioma, n.cat = cant.categoricas)
      if(!is.null(bar.cat)) {
        rep.bar <- paste0("Categoricas: ", var.cat)
        createLogCJ(nombre.datos, code.cat, rep.hc, rep.bar)
      }
      return(bar.cat)
    })
  })

  #' Gráfico de Clusterización Jerarquica (Diagrama)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.hcDendo, {
    output$plot.diag = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeDendo)
      diag <- checkError(codigo)
      if(!is.null(diag))
        createLogCJ(nombre.datos, codigo, rep.hc, "Dendograma")
      return(diag)
    })
  })

  #' Gráfico de Clusterización Jerarquica (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.hcMapa, {
    output$plot.mapa = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeMapa)
      updatePlot$mapa <- checkError(codigo)
      if(!is.null(updatePlot$mapa))
        createLogCJ(nombre.datos, codigo, rep.hc, "Mapa")
      return(updatePlot$mapa)
    })
  })

  output$plot.mapa.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- mapa.ranges$x
      ejey <- mapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        res <- shiny::isolate(updatePlot$mapa)
        res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position = "none")
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
  shiny::observeEvent(input$run.hcHoriz, {
    output$plot.horiz = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeHoriz)
      horiz <- checkError(codigo)
      if(!is.null(horiz))
        createLogCJ(nombre.datos, codigo, rep.hc, "Horizontal: proio")
      return(horiz)
    })
  })

  shiny::observeEvent(input$selHoriz, {
    sel.horiz <- input$selHoriz
    cant <- input$cant.cluster
    cant.numericas <- ncol(var.numericas(datos))
    idioma <- shiny::isolate(input$idioma)
    if(!is.null(datos) && !is.null(cant)) {
      color <- sapply(1:cant, function(i) 
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      if(!sel.horiz %in% c("", "todos")) color <- color[as.numeric(sel.horiz)]
      codigo <- cluster.horiz(sel.horiz, color)
      shinyAce::updateAceEditor(session, "fieldCodeHoriz", value = codigo)
      output$plot.horiz = shiny::renderPlot({
        horiz <- checkError(codigo, idioma, cant.numericas)
        if(!is.null(horiz)) {
          rep.horiz <- paste0("Horizontal: ", sel.horiz)
          createLogCJ(nombre.datos, codigo, rep.hc, rep.horiz)
        }
        return(horiz)
      })
    }
  })

  #' Gráfico de Clusterización Jerarquica (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.hcVert, {
    output$plot.vert = shiny::renderPlot({
      code <- shiny::isolate(input$fieldCodeVert)
      vert <- checkError(code)
      if(!is.null(vert))
        createLogCJ(nombre.datos, codigo, rep.hc, "Vertical: propio")
      return(vert)
    })
  })

  shiny::observeEvent(input$selVert, {
    selVert <- input$selVert
    cant <- input$cant.cluster
    cant.numericas <- ncol(var.numericas(datos))
    idioma <- shiny::isolate(input$idioma)
    if(!is.null(datos) && !is.null(cant)) {
      color <- sapply(1:cant, function(i) 
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      codigo <- cluster.vert(sel = selVert, colores = color)
      shinyAce::updateAceEditor(session, "fieldCodeVert", value = codigo)
      output$plot.vert = shiny::renderPlot({
        vert <- checkError(codigo, idioma, cant.numericas)
        if(!is.null(vert)) {
          rep.vert <- paste0("Vertical: ", selVert)
          createLogCJ(nombre.datos, codigo, rep.hc, rep.vert)
        }
        return(vert)
      })
    }
  })

  #' Gráfico de Clusterización Jerarquica (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.hcRadar, {
    output$plot.radar = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeRadar)
      radar <- checkError(codigo)
      if(!is.null(radar)) 
        createLogCJ(nombre.datos, codigo, rep.hc, "Radar")
      return(radar)
    })
  })

  #' Gráfico de Clusterización Jerarquica (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.hcBar, {
    output$plot.bar.cat = shiny::renderPlot({
      code <- shiny::isolate(input$fieldCodeBar)
      bar.cat <- checkError(code)
      if(!is.null(bar.cat)) 
        createLogCJ(nombre.datos, "", codigo, rep.hc, "Categoricas: Propio")
      return(bar.cat)
    })
  })

  shiny::observeEvent(input$selBar, {
    selBar <- input$selBar
    cant <- input$cant.cluster
    cant.numericas <- ncol(var.numericas(datos))
    idioma <- shiny::isolate(input$idioma)
    if(!is.null(datos) && !is.null(cant)) {
      color <- sapply(1:cant, function(i) 
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      codigo <- cluster.cat(var = selBar, colores = color)
      shinyAce::updateAceEditor(session, "fieldCodeBar", value = codigo)
      output$plot.bar.cat = shiny::renderPlot({
        bar.cat <- checkError(codigo, idioma, cant.numericas)
        if(!is.null(bar.cat)) {
          rep.bar <- paste0("Categoricas: ", selBar)
          createLogCJ(nombre.datos, codigo, rep.hc, rep.bar)
        }
        return(bar.cat)
      })
    }
  })
  
  #' Actualizacion del Modelo K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$KRun), {
    cant <- shiny::isolate(input$cant.kmeans.cluster)
    iter.max <- shiny::isolate(input$num.iter)
    nstart <- shiny::isolate(input$num.nstart)
    algorithm <- shiny::isolate(input$sel.algoritmo)
    pca.modelo <<- shiny::isolate(updateData$pca.modelo)
    rep.k <<- c(cant, iter.max, nstart, algorithm) 
    
    codigo <- def.k.model(data = "datos", cant, iter.max, nstart, algorithm)
    tryCatch ({
      if(!is.null(datos) && !is.null(cant)) {
        eval(parse(text = codigo))
        shinyAce::updateAceEditor(session, "fieldCodeKModelo", value = codigo)
        updateData$k.modelo <- k.modelo
        output$inercia.k = shiny::renderUI({
          panel.inercia(esHC = F, k.modelo)
        })
        output$txtk <- shiny::renderPrint(print(unclass(k.modelo)))
        createLogK(nombre.datos, codigo, rep.k, "modelo")
      }
    }, error = function(e) {
      shiny::showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })
  
  #' Actualización de Gráficos Kmedias.
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(updateData$k.modelo, {
    k.modelo <<- updateData$k.modelo
    cant <- shiny::isolate(input$cant.kmeans.cluster)
    cant.numericas <- ncol(var.numericas(datos))
    cant.categoricas <- ncol(var.categoricas(datos))
    idioma <- shiny::isolate(input$idioma)
    var.horiz <- shiny::isolate(input$sel.Khoriz)
    var.vert <- shiny::isolate(input$sel.Kvert)
    var.cat <- shiny::isolate(input$sel.Kbar)
    
    color <- sapply(1:cant, function(i) 
      paste0("'", input[[paste0("kColor", i)]], "'"))
    
    code.kmapa <- cluster.mapa(color, F)
    code.khoriz <- cluster.horiz(var.horiz, color, F)
    code.kvert <- cluster.vert(var.vert, color, F)
    code.kradar <- cluster.radar(color, F)
    code.kcat <- cluster.cat(var.cat, color, F)
    
    shinyAce::updateAceEditor(session, "fieldCodeKmapa", value = code.kmapa)
    shinyAce::updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
    shinyAce::updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
    shinyAce::updateAceEditor(session, "fieldCodeKradar", value = code.kradar)
    shinyAce::updateAceEditor(session, "fieldCodeKbar", value = code.kcat)
    
    output$plot.kmapa = shiny::renderPlot({
      updatePlot$kmapa <- checkError(code.kmapa, idioma, cant.numericas)
      if(!is.null(updatePlot$kmapa))
        createLogK(nombre.datos, code.kmapa, rep.k, "Mapa")
      return(updatePlot$kmapa)
    })
    output$plot.khoriz = shiny::renderPlot({
      khoriz <- checkError(code.khoriz, idioma, cant.numericas)
      if(!is.null(khoriz)) {
        rep.horiz <- paste0("Horizontal: ", var.horiz)
        createLogK(nombre.datos, code.khoriz, rep.k, rep.horiz)
      }
      return(khoriz)
    })
    output$plot.kvert = shiny::renderPlot({
      kvert <- checkError(code.kvert, idioma, cant.numericas)
      if(!is.null(kvert)) {
        rep.vert <- paste0("Vertical: ", var.vert)
        createLogK(nombre.datos, code.kvert, rep.k, rep.vert)
      }
      return(kvert)
    })
    output$plot.kradar = shiny::renderPlot({
      kradar <- checkError(code.kradar, idioma, cant.numericas)
      if(!is.null(kradar))
        createLogK(nombre.datos, code.kradar, rep.k, "Radar")
      return(kradar)
    })
    output$plot.kcat = shiny::renderPlot({
      bar.kcat <- checkError(code.kcat, idioma, n.cat = cant.categoricas)
      if(!is.null(bar.kcat)) {
        rep.bar <- paste0("Categoricas: ", var.cat)
        createLogK(nombre.datos, code.kcat, rep.k, rep.bar)
      }
      return(bar.kcat)
    })
  })
  
  #' Gráfico de K-medias (Codo de Jambu)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.jambu = shiny::renderPlot({
    tryCatch({
      code.jambu <<- updatePlot$jambu
      shiny::isolate(eval(parse(text = code.jambu)))
      shinyAce::updateAceEditor(session, "fieldCodeJambu", value = code.jambu)
      res <- shiny::isolate(eval(parse(text = code.jambu)))
      createLogK(nombre.datos, code.jambu, rep.k, "Jambu")
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(shiny::isolate(input$idioma), T)
      } else {
        shiny::showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
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
  shiny::observeEvent(input$run.Kmapa, {
    output$plot.kmapa = shiny::renderPlot({
      code <- shiny::isolate(input$fieldCodeKmapa)
      updatePlot$kmapa <- checkError(code)
      if(!is.null(updatePlot$kmapa))
        createLogK(nombre.datos, code, rep.k, "Mapa")
      return(updatePlot$kmapa)
    })
  })

  output$plot.kmapa.zoom <- shiny::renderPlot({
    tryCatch({
      ejex <- kmapa.ranges$x
      ejey <- kmapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        res <- shiny::isolate(updatePlot$kmapa)
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
  shiny::observeEvent(input$run.Khoriz, {
    output$plot.khoriz = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeKhoriz)
      horiz <- checkError(codigo)
      if(!is.null(horiz))
        createLogK(nombre.datos, codigo, rep.k, "Horizontal: propio")
      return(horiz)
    })
  })
  
  shiny::observeEvent(input$sel.Khoriz, {
    sel.horiz <- input$sel.Khoriz
    cant <- input$cant.kmeans.cluster
    cant.numericas <- ncol(var.numericas(datos))
    idioma <- shiny::isolate(input$idioma)
    if(!is.null(datos) && !is.null(cant)) {
      color <- sapply(1:cant, function(i) 
        paste0("'", input[[paste0("kColor", i)]], "'"))
      if(!sel.horiz %in% c("", "todos")) color <- color[as.numeric(sel.horiz)]
      codigo <- cluster.horiz(esHC = F, sel.horiz, color)
      shinyAce::updateAceEditor(session, "fieldCodeKhoriz", value = codigo)
      output$plot.khoriz = shiny::renderPlot({
        horiz <- checkError(codigo, idioma, cant.numericas)
        if(!is.null(horiz)) {
          rep.horiz <- paste0("Horizontal: ", sel.horiz)
          createLogK(nombre.datos, codigo, rep.k, rep.horiz)
        }
        return(horiz)
      })
    }
  })

  #' Gráfico de K-medias (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.KVert, {
    output$plot.kvert = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeKvert)
      vert <- checkError(codigo)
      if(!is.null(vert))
        createLogK(nombre.datos, codigo, rep.k, "Vertical: propio")
      return(vert)
    })
  })
  
  shiny::observeEvent(input$sel.Kvert, {
    sel.Kvert <- input$sel.Kvert
    cant <- input$cant.kmeans.cluster
    cant.numericas <- ncol(var.numericas(datos))
    idioma <- shiny::isolate(input$idioma)
    if(!is.null(datos) && !is.null(cant)) {
      color <- sapply(1:cant, function(i) 
        paste0("'", input[[paste0("kColor", i)]], "'"))
      codigo <- cluster.vert(esHC = F, sel.Kvert, color)
      shinyAce::updateAceEditor(session, "fieldCodeKvert", value = codigo)
      output$plot.kvert = shiny::renderPlot({
        vert <- checkError(codigo, idioma, cant.numericas)
        if(!is.null(vert)) {
          rep.vert <- paste0("Vertical: ", sel.Kvert)
          createLogK(nombre.datos, codigo, rep.k, rep.vert)
        }
        return(vert)
      })
    }
  })

  #' Gráfico de K-medias (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.Kradar, {
    output$plot.kradar = shiny::renderPlot({
      codigo <- shiny::isolate(input$fieldCodeKradar)
      radar <- checkError(codigo)
      if(!is.null(radar)) 
        createLogK(nombre.datos, codigo, rep.k, "Radar")
      return(radar)
    })
  })

  #' Gráfico de K-medias (Categórico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(input$run.Kbar, {
    output$plot.kcat = shiny::renderPlot({
      code <- shiny::isolate(input$fieldCodeKbar)
      bar.cat <- checkError(code)
      if(!is.null(bar.cat))
        createLogK(nombre.datos, codigo, rep.k, "Categoricas: propio")
      return(bar.cat)
    })
  })
  
  shiny::observeEvent(input$sel.Kbar, {
    sel.Kbar <- input$sel.Kbar
    cant <- input$cant.kmeans.cluster
    cant.numericas <- ncol(var.numericas(datos))
    idioma <- shiny::isolate(input$idioma)
    if(!is.null(datos) && !is.null(cant)) {
      color <- sapply(1:cant, function(i) 
        paste0("'", input[[paste0("kColor", i)]], "'"))
      codigo <- cluster.cat(esHC = F, var = sel.Kbar, colores = color)
      shinyAce::updateAceEditor(session, "fieldCodeKbar", value = codigo)
      output$plot.kcat = shiny::renderPlot({
        bar.cat <- checkError(codigo, idioma, cant.numericas)
        if(!is.null(bar.cat)) {
          rep.bar <- paste0("Categoricas: ", sel.Kbar)
          createLogK(nombre.datos, codigo, rep.k, rep.bar)
        }
        return(bar.cat)
      })
    }
  })

  #' Mostrar Colores (k-means)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  shiny::observeEvent(c(updateData$datos, input$cant.kmeans.cluster), {
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
    ifelse(input$idioma == "es", aux <- "CJ", aux <- "HC")
    C.Jerarquica <- as.factor(paste0(aux, hc.modelo$clusters))
    datos[[paste0(length(levels(C.Jerarquica)), ".", aux)]] <<- C.Jerarquica
    output$contents = DT::renderDataTable(mostrarData())
    shiny::showNotification(tr("msjclusters"), duration = 5, type = "message")
    updateSelectInput(session, "sel.distribucion.cat",
                      choices = colnames(var.categoricas(datos)))
  })

  shiny::observeEvent(input$Kbutton, {
    ifelse(input$idioma == "es", aux <- ".Kmedias", aux <- ".Kmeans")
    Kmedias <- as.factor(paste0("K", k.modelo$cluster))
    datos[[paste0(length(levels(Kmedias)), aux)]] <<- Kmedias
    output$contents = DT::renderDataTable(mostrarData())
    shiny::showNotification(tr("msjclusters"), duration = 5, type = "message")
    updateSelectInput(session, "sel.distribucion.cat",
                      choices = colnames(var.categoricas(datos)))
  })

  output$downloaDatos <- shiny::downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
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

      zip::zip(file, files)
    }
  )
})
