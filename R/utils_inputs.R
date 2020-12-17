radioSwitch <- function(id, label = NULL, names, values = NULL, val.def = T) {
  if(is.null(values)) values <- c(TRUE, FALSE)
  tags$div(
    class = "form-group", `data-shinyjs-resettable-type`="RadioButtons",
    `data-shinyjs-resettable-value` = names[1],
    if(!is.null(label)) {
      tags$label(class = "control-label", `for` = id, `data-id` = label)
    },
    tags$div(
      class = "radioGroupButtons btn-group-container-sw", id = id, `data-toggle`="buttons",
      tags$div(
        class = "btn-radiogroup",
        tags$button(
          class = ifelse(val.def, "btn radiobtn btn-radioswitch active",
                         "btn radiobtn btn-radioswitch"),
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          
          if(val.def) {
            tags$input(type="radio", autocomplete="off", name=id, value=values[1], checked = "checked",
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          } else {
            tags$input(type="radio", autocomplete="off", name=id, value=values[1],
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          },
          
          labelInput(names[[1]])
        )
      ),
      tags$div(
        class = "btn-radiogroup", role = "group",
        tags$button(
          class = ifelse(val.def,"btn radiobtn btn-radioswitch",
                         "btn radiobtn btn-radioswitch active"),
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          
          if(val.def) {
            tags$input(type="radio", autocomplete="off", name=id, value=values[2],
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          } else {
            tags$input(type="radio", autocomplete="off", name=id, value=values[2], checked = "checked",
                       style = "position: absolute;clip: rect(0,0,0,0);pointer-events: none;")
          },
          
          labelInput(names[[2]])
        )
      )
    )
  )
}

checkSwitch <- function(id, label = NULL, name) {
  tags$div(
    class = "form-group", `data-shinyjs-resettable-type`="RadioButtons", 
    `data-shinyjs-resettable-value` = name,
    if(!is.null(label)) {
      tags$label(class = "control-label", `for` = id, `data-id` = label)
    },
    tags$div(
      `data-toggle`="buttons",
      tags$div(
        class = "btn-radiogroup",
        tags$button(
          class = "btn radiobtn btn-radioswitch active",
          tags$span(class = "radio-btn-icon-yes", tags$i(class="glyphicon glyphicon-ok")),
          tags$span(class = "radio-btn-icon-no", tags$i(class="glyphicon glyphicon-remove")),
          tags$input(id=id, type="checkbox", checked = "checked", style = 
                       "position: absolute;clip: rect(0,0,0,0);pointer-events: none;"),
          labelInput(name)
        )
      )
    )
  )
}

codigo.monokai <- function(id, height) {
  aceEditor(
    id, mode = "r", theme = "monokai", value = "", 
    readOnly = T, height = height
  )
}

labelInput <- function(inputId, value = ""){
  tags$span(`data-id` = inputId, value)
}

updateLabelInput <- function (session, labelid, value = NULL) {
  message <- dropNulls(list(labelid = labelid))
  if(length(labelid) == 1) {
    labelid <- list(labelid)
  }
  ifelse(
    is.null(value), sentvalue <- labelid,
    ifelse(length(value) == 1, sentvalue <- list(value),
           sentvalue <- value))
  session$sendCustomMessage(
    type = 'updateLabel',
    message = list(ids = labelid, values = sentvalue))
}
