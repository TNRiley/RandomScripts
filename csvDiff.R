# app.R for CSV Structure Comparison & Merge

# --- 1. Load Libraries ---
library(shiny)
library(DT)
library(readr)
library(dplyr)
library(purrr)
library(janitor) # For compare_df_cols()
library(shinythemes) 
library(tools) # For file_path_sans_ext

# --- 2. UI Definition ---
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("CSV File Structure Comparator & Merger"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("csv_uploads", 
                "Upload Multiple CSV Files:",
                multiple = TRUE,
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      hr(),
      tags$p(strong("Instructions:")),
      tags$ul(
        tags$li("Upload two or more CSV files to compare and merge."),
        tags$li("View structural differences in the tabs."),
        tags$li("Use the button below to merge all uploaded files by row (stacking them). Missing columns will be filled with NA.")
      ),
      hr(),
      uiOutput("summary_stats_ui"),
      hr(),
      h4("Merge and Export"),
      downloadButton("download_merged_csv", "Download Merged Data as CSV", class = "btn-success btn-block") # Added class for styling
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("File Summaries", 
                 br(),
                 DT::dataTableOutput("file_summary_table")
        ),
        tabPanel("Column Comparison (Structure & Types)", 
                 br(),
                 tags$p("This table shows each unique column name found across all uploaded files. For each file, the data type of that column is shown (as inferred by R when reading the CSV). 'NA' indicates the column is not present in that file."),
                 DT::dataTableOutput("column_comparison_table")
        ),
        tabPanel("Unique & Common Columns (Names)",
                 br(),
                 fluidRow(
                   column(6,
                          h4("Columns Common to All Files:"),
                          verbatimTextOutput("common_columns_text")
                   ),
                   column(6,
                          h4("All Unique Column Names (Union):"),
                          verbatimTextOutput("union_columns_text")
                   )
                 ),
                 hr(),
                 h4("Columns Unique to Specific Files:"),
                 uiOutput("unique_columns_ui")
        )
      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  loaded_data <- reactiveVal(list(dfs = list(), names = character(0), clean_names = character(0)))
  
  observeEvent(input$csv_uploads, {
    req(input$csv_uploads)
    
    df_list_successful <- list()
    file_names_successful <- character(0)
    
    withProgress(message = 'Reading CSV files...', value = 0, {
      for (i in 1:nrow(input$csv_uploads)) { # Iterate using nrow for multiple file details
        incProgress(1/nrow(input$csv_uploads), detail = paste("Processing", input$csv_uploads$name[i]))
        current_file_path <- input$csv_uploads$datapath[i]
        current_file_name <- input$csv_uploads$name[i]
        tryCatch({
          df_list_successful[[length(df_list_successful) + 1]] <- read_csv(current_file_path, show_col_types = FALSE, lazy = FALSE)
          file_names_successful <- c(file_names_successful, current_file_name)
        }, error = function(e) {
          shiny::showNotification(paste("Error reading file:", current_file_name, "-", e$message), type = "error", duration = 10)
        })
      }
    })
    
    if (length(df_list_successful) > 0) {
      clean_file_names <- make.names(tools::file_path_sans_ext(file_names_successful), unique = TRUE)
      names(df_list_successful) <- clean_file_names # Name the list for janitor
      loaded_data(list(dfs = df_list_successful, names = file_names_successful, clean_names = clean_file_names))
    } else {
      loaded_data(list(dfs = list(), names = character(0), clean_names = character(0))) 
    }
  })
  
  output$file_summary_table <- DT::renderDataTable({
    data_info <- loaded_data()
    req(length(data_info$dfs) > 0)
    
    summaries <- map_dfr(seq_along(data_info$dfs), function(i) {
      tibble(
        `File Name` = data_info$names[i],
        `Rows` = nrow(data_info$dfs[[i]]),
        `Columns` = ncol(data_info$dfs[[i]])
      )
    })
    DT::datatable(summaries, options = list(pageLength = 10, searching = FALSE), rownames = FALSE)
  })
  
  column_comparison_result <- reactive({
    data_info <- loaded_data()
    req(length(data_info$dfs) >= 1) 
    # The list 'data_info$dfs' is already named with clean_file_names by the upload observer
    janitor::compare_df_cols(data_info$dfs) 
  })
  
  output$column_comparison_table <- DT::renderDataTable({
    comparison_df <- column_comparison_result()
    req(comparison_df)
    DT::datatable(comparison_df, 
                  options = list(pageLength = 25, scrollX = TRUE, autoWidth = TRUE),
                  rownames = FALSE,
                  caption = "Data types are R classes (e.g., 'character', 'numeric', 'Date', 'logical').")
  })
  
  output$summary_stats_ui <- renderUI({
    data_info <- loaded_data()
    if(length(data_info$dfs) == 0) return(tags$p("Upload files to see summary statistics."))
    comp_res <- tryCatch(column_comparison_result(), error = function(e) NULL) # Avoid error if only 1 file on initial load
    
    tagList(
      tags$h5("Overall Summary:"),
      tags$p(paste("Total files loaded:", length(data_info$dfs))),
      tags$p(paste("Total unique column names across all files:", 
                   if(!is.null(comp_res) && nrow(comp_res) > 0) nrow(comp_res) else "N/A (need at least one file)"))
    )
  })
  
  column_sets <- reactive({
    data_info <- loaded_data()
    req(length(data_info$dfs) >= 1)
    
    col_names_list <- map(data_info$dfs, names)
    if(length(col_names_list) == 0) return(list(common = character(0), union_all = character(0), unique_per_file = list()))
    
    common_cols <- if(length(col_names_list) > 0) Reduce(intersect, col_names_list) else character(0)
    union_cols <- if(length(col_names_list) > 0) Reduce(union, col_names_list) else character(0)
    
    unique_per_file <- list()
    # Use original file names for display in this section
    original_names_for_display <- data_info$names 
    
    if (length(col_names_list) > 1) { 
      for (i in seq_along(data_info$dfs)) {
        current_file_col_names <- col_names_list[[i]]
        other_files_col_names <- unlist(col_names_list[-i])
        unique_to_this_file <- setdiff(current_file_col_names, other_files_col_names)
        if (length(unique_to_this_file) > 0) {
          unique_per_file[[original_names_for_display[i]]] <- unique_to_this_file
        }
      }
    } else if (length(col_names_list) == 1) { 
      unique_per_file[[original_names_for_display[1]]] <- col_names_list[[1]]
    }
    
    list(common = sort(common_cols), union_all = sort(union_cols), unique_per_file = unique_per_file)
  })
  
  output$common_columns_text <- renderText({
    cols <- column_sets()$common
    if (length(cols) > 0) paste(cols, collapse = "\n") else "No columns are common to all uploaded files (or <2 files uploaded)."
  })
  
  output$union_columns_text <- renderText({
    cols <- column_sets()$union_all
    if (length(cols) > 0) paste(cols, collapse = "\n") else "No files uploaded yet."
  })
  
  output$unique_columns_ui <- renderUI({
    unique_data_list <- column_sets()$unique_per_file
    if (length(unique_data_list) == 0) {
      if (length(loaded_data()$dfs) < 2) {
        return(tags$p("Upload at least two files to compare unique columns."))
      }
      return(tags$p("No columns are strictly unique to any single file when compared to all others."))
    }
    
    ui_elements <- imap(unique_data_list, function(unique_cols_vector, file_display_name) {
      tagList(
        tags$h5(paste("Unique to '", file_display_name, "':")), # file_display_name is already the original name
        tags$pre(paste(unique_cols_vector, collapse = "\n"))
      )
    })
    do.call(tagList, ui_elements)
  })
  
  # --- Merge and Download Logic ---
  output$download_merged_csv <- downloadHandler(
    filename = function() {
      paste0("merged_data_", strftime(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      data_info <- loaded_data()
      req(length(data_info$dfs) > 0, message = "No files uploaded to merge.")
      
      if (length(data_info$dfs) == 1) {
        shiny::showNotification("Only one file uploaded. Exporting the single file.", type="warning", duration = 5)
        # Ensure list columns are handled even for single file export
        single_df_to_export <- data_info$dfs[[1]]
        for (col_name in names(single_df_to_export)) {
          if (is.list(single_df_to_export[[col_name]])) {
            single_df_to_export[[col_name]] <- sapply(single_df_to_export[[col_name]], function(x) {
              if (is.null(x) || length(x) == 0) return(NA_character_)
              paste(unlist(x), collapse = "; ") 
            })
          }
        }
        readr::write_csv(single_df_to_export, file, na = "")
        return()
      }
      
      merge_id <- showNotification("Merging files... this may take a moment.", type = "message", duration = NULL)
      on.exit(removeNotification(merge_id), add = TRUE)
      
      # Prepare a list of dataframes named with their original file names for the .id column
      dfs_to_bind_list <- data_info$dfs
      if (length(data_info$names) == length(dfs_to_bind_list)) {
        names(dfs_to_bind_list) <- data_info$names
      } else {
        # Fallback: use cleaned names or sequential numbers if original names aren't aligned
        names(dfs_to_bind_list) <- if (!is.null(data_info$clean_names) && length(data_info$clean_names) == length(dfs_to_bind_list)) {
          data_info$clean_names
        } else {
          paste0("file_", seq_along(dfs_to_bind_list))
        }
      }
      
      merged_df <- tryCatch({
        dplyr::bind_rows(dfs_to_bind_list, .id = "source_file_origin")
      }, error = function(e) {
        shiny::showNotification(paste("Error during merge:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      if (is.null(merged_df)) return()
      
      # Safeguard: Convert any remaining list columns to character before writing CSV
      # (bind_rows typically handles type coercion, but complex objects or all-NA list columns might remain)
      for (col_name in names(merged_df)) {
        if (is.list(merged_df[[col_name]])) {
          merged_df[[col_name]] <- sapply(merged_df[[col_name]], function(x_cell) {
            if (is.null(x_cell) || length(x_cell) == 0) return(NA_character_)
            # Handle cases where a cell might contain a vector of items
            paste(unlist(x_cell), collapse = "; ") 
          })
        }
      }
      
      readr::write_csv(merged_df, file, na = "")
      shiny::showNotification("Merged CSV file is ready for download.", type = "message", duration = 5)
    },
    contentType = "text/csv"
  )
}

# --- 4. Run the Application ---
shinyApp(ui = ui, server = server)