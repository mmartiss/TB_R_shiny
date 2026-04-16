filterUI <- function(id, title = "Data Table") {
  ns <- NS(id)
  tagList(
    h4(title),
    fluidRow(
      column(3, tags$label("Remove columns"), uiOutput(ns("col_remove_ui")), actionButton(ns("btn_remove"), "Remove", class="btn-danger btn-sm")),
      column(3, tags$label("Sort by"), uiOutput(ns("col_sort_ui")), radioButtons(ns("sort_dir"), NULL, c("Asc"="asc","Desc"="desc"), inline=T), actionButton(ns("btn_sort"), "Sort", class="btn-primary btn-sm")),
      column(3, tags$label("Rename Column"), uiOutput(ns("col_rename_select_ui")), textInput(ns("new_col_name"), NULL, placeholder="New name"), actionButton(ns("btn_rename"), "Rename", class="btn-info btn-sm")),
      column(3, tags$label("Batch Replace"), uiOutput(ns("replace_col_ui")), uiOutput(ns("replace_val_old_ui")), textInput(ns("replace_val_new"), NULL, placeholder="New value"), actionButton(ns("btn_replace"), "Replace All", class="btn-warning btn-sm")),
      column(3, tags$label("Filter rows"), uiOutput(ns("col_filter_ui")), uiOutput(ns("filter_value_ui")), actionButton(ns("btn_filter"), "Filter", class="btn-primary btn-sm"))
    ),
    br(),
    # Mygtukas species pridejimui
    uiOutput(ns("tax_join_ui")),
    br(),
    tags$label("Active filters:"), uiOutput(ns("active_filters_ui")),
    br(),
    fluidRow(
      column(12,
             actionButton(ns("btn_reset"), "Reset all", class="btn-warning btn-sm"),
             downloadButton(ns("dl_csv"), "CSV", class="btn-sm"),
             downloadButton(ns("dl_tsv"), "TSV", class="btn-sm"),
             actionButton(ns("btn_use"), "Use for analysis", class="btn-success btn-sm")
      )
    ),
    br(),
    DTOutput(ns("tbl"))
  )
}

filterServer <- function(id, original_data, tax_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    current_data   <- reactiveVal(NULL)
    active_filters <- reactiveVal(list())
    
    has_tax_reactive <- !is.null(tax_data) && is.function(tax_data)
    
    observe({
      req(original_data())
      current_data(original_data())
    })
    
    output$tax_join_ui <- renderUI({
      if (!has_tax_reactive) return(NULL)
      req(tax_data())
      actionButton(ns("btn_add_species"), "Add Species Column from Taxonomy", class="btn-info btn-sm", icon = icon("plus"))
    })
    
    observeEvent(input$btn_add_species, {
      req(current_data(), tax_data())
      df <- current_data()
      tax <- tax_data()
      
      # check, jei jau yra species
      if ("species" %in% names(df)) {
        showNotification("Species column already exists", type = "warning")
        return()
      }
      
      df$tax_id <- as.character(df$tax_id)
      tax$tax_id <- as.character(tax$tax_id)

      tax_to_join <- tax[, c("tax_id", "species")]
      
      df_new <- dplyr::left_join(df, tax_to_join, by = "tax_id")

      all_cols <- names(df_new)
      other_cols <- all_cols[!all_cols %in% c("tax_id", "species")]
      df_new <- df_new[, c("tax_id", "species", other_cols), drop = FALSE]

      current_data(df_new)
      showNotification("Species column added to the table", type = "message")
    })
    
    output$replace_col_ui <- renderUI({
      selectInput(ns("replace_col"), NULL, choices = names(current_data()))
    })
    
    output$replace_val_old_ui <- renderUI({
      req(input$replace_col)
      choices <- sort(unique(as.character(current_data()[[input$replace_col]])))
      selectInput(ns("replace_val_old"), NULL, choices = choices)
    })
    
    # Batch Replace Logika
    observeEvent(input$btn_replace, {
      req(input$replace_col, input$replace_val_old)
      df <- current_data()
      col <- input$replace_col
      df[[col]][as.character(df[[col]]) == input$replace_val_old] <- input$replace_val_new
      current_data(df)
      showNotification(paste("Replaced all occurrences in", col))
    })
    
    
    output$col_remove_ui <- renderUI({
      req(current_data())
      selectizeInput(ns("cols_remove"), NULL, choices = names(current_data()), multiple = TRUE)
    })
    
    output$col_sort_ui <- renderUI({
      req(current_data())
      selectizeInput(ns("col_sort"), NULL, choices = names(current_data()))
    })
    
    output$col_filter_ui <- renderUI({
      req(current_data())
      selectizeInput(ns("col_filter"), NULL, choices = names(current_data()), multiple = FALSE)
    })
    
    output$filter_value_ui <- renderUI({
      req(current_data(), input$col_filter)
      col <- current_data()[[input$col_filter]]
      if (is.numeric(col)) {
        sliderInput(ns("filter_val"), NULL,
                    min = min(col, na.rm = TRUE), max = max(col, na.rm = TRUE),
                    value = c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)))
      } else {
        unique_vals <- sort(unique(as.character(col)))
        selectizeInput(ns("filter_val"), NULL,
                       choices = c("(all)", unique_vals), selected = "(all)")
      }
    })
    
    output$col_rename_select_ui <- renderUI({
      req(current_data())
      selectInput(ns("col_to_rename"), NULL, choices = names(current_data()))
    })
    
    observeEvent(input$btn_rename, {
      req(input$col_to_rename, input$new_col_name)
      df <- current_data()
      names(df)[names(df) == input$col_to_rename] <- input$new_col_name
      current_data(df)
      updateTextInput(session, "new_col_name", value = "")
    })
    
    observeEvent(input$tbl_cell_edit, {
      info <- input$tbl_cell_edit
      df <- current_data()
      df[info$row, info$col + 1] <- info$value 
      current_data(df)
    })
    
    observeEvent(input$btn_filter, {
      req(input$col_filter, input$filter_val)
      filters <- active_filters()
      filters[[length(filters) + 1]] <- list(col = input$col_filter, val = input$filter_val)
      active_filters(filters)
    })
    
    observeEvent(input$btn_remove, {
      req(current_data(), input$cols_remove)
      df <- current_data()
      df <- df[, !names(df) %in% input$cols_remove, drop = FALSE]
      current_data(df)
    })
    
    observeEvent(input$btn_sort, {
      req(current_data(), input$col_sort)
      df <- current_data()
      idx <- order(df[[input$col_sort]], decreasing = input$sort_dir == "desc", na.last = TRUE)
      current_data(df[idx, , drop = FALSE])
    })
    
    observeEvent(input$btn_reset, {
      req(original_data())
      current_data(original_data())
      active_filters(list())
    })
    
    output$active_filters_ui <- renderUI({
      filters <- active_filters()
      if (length(filters) == 0) return(tags$em("No filters applied"))
      tagList(lapply(seq_along(filters), function(i) {
        f <- filters[[i]]
        tags$div(style = "margin-bottom:5px;",
                 tags$span(paste0(f$col, ": ", paste(f$val, collapse=" - "))),
                 actionButton(ns(paste0("remove_filter_", i)), "✖",
                              class = "btn btn-xs btn-danger", style = "margin-left:10px;"))
      }))
    })
    
    observe({
      filters <- active_filters()
      lapply(seq_along(filters), function(i) {
        observeEvent(input[[paste0("remove_filter_", i)]], {
          current <- active_filters()
          if (length(current) >= i) { current <- current[-i]; active_filters(current) }
        }, ignoreInit = TRUE)
      })
    })

    filtered_result <- reactive({
      df      <- current_data()
      req(df)
      filters <- active_filters()
      for (f in filters) {
        if (!f$col %in% names(df)) next
        col <- df[[f$col]]
        if (is.numeric(col)) {
          df <- df[!is.na(col) & col >= f$val[1] & col <= f$val[2], ]
        } else {
          if (!"(all)" %in% f$val) df <- df[as.character(col) %in% f$val, ]
        }
      }
      df
    })
    
    output$dl_csv <- downloadHandler(
      filename = function() paste0("data_", id, "_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(filtered_result(), file, row.names = FALSE)
    )
    
    output$dl_tsv <- downloadHandler(
      filename = function() paste0("data_", id, "_", Sys.Date(), ".tsv"),
      content  = function(file) write.table(filtered_result(), file, sep = "\t", row.names = FALSE, quote = FALSE)
    )
    
    output$tbl <- renderDT({
      req(filtered_result())
      datatable(filtered_result(), editable = "cell", options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE, filter = "top")
    })
    
    list(
      data    = filtered_result,
      btn_use = reactive(input$btn_use)
    )
  })
}