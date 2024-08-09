#' Initial Record Table
#'
#' This function generates a formatted table displaying the record counts 
#' for each citation source, including the number of records imported and 
#' the distinct records after deduplication.
#'
#' @param data A data frame containing the record counts for each citation source.
#'   It must include columns `Source`, `Records_Imported`, and `Distinct_Records`.
#'
#' @return A `gt` table object summarizing the record counts for each citation source.
#'
#' @details 
#' The function checks if the input data frame is empty and returns an empty `gt` table
#' if no data is present. Otherwise, it generates a formatted table with labeled columns
#' and adds footnotes explaining the meaning of each column.
#'
#' @import gt
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Total"),
#'   Records_Imported = c(100, 150, 250),
#'   Distinct_Records = c(90, 140, 230)
#' )
#' initial_record_table(sample_data)
initial_record_table <- function(data) {
  # Check if data is empty
  if (nrow(data) == 0) {
    return(gt::gt(data.frame(Source = character(), `Records Imported` = integer(), `Distinct Records` = integer())))
  }
  
  # Create the initial gt table
  data %>%
    gt::gt(rowname_col = "Source") %>%
    gt::tab_header(title = "Record Counts") %>%
    
    # Label the columns
    gt::cols_label(
      Records_Imported = "Records Imported",
      Distinct_Records = "Distinct Records"
    ) %>%
    
    # Add footnote for "Records Imported"
    gt::tab_footnote(
      footnote = "Number of records imported from each source.",
      locations = gt::cells_column_labels(
        columns = c("Records_Imported")
      )
    ) %>%
    
    # Add footnote for "Distinct Records"
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(
        columns = c("Distinct_Records")
      )
    )
}
