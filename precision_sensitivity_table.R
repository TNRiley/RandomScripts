#' Precision and Sensitivity Table
#'
#' This function generates a formatted table that displays the precision 
#' and sensitivity (recall) metrics for each citation source, along with 
#' distinct records and phase-specific counts such as "screened" and "final".
#'
#' @param data A data frame containing phase-specific counts and calculated metrics 
#'   for each citation source. It must include columns such as `Source`, 
#'   `Distinct_Records`, `final`, `Precision`, `Recall`, and optionally `screened`.
#'
#' @return A `gt` table object summarizing the precision and sensitivity 
#'   metrics for each citation source, with relevant footnotes and labels.
#'
#' @details 
#' The function first checks whether all values in the `screened` column are zero.
#' If so, the column is removed from the table. The table is then generated 
#' using the `gt` package, with labeled columns and footnotes explaining the metrics.
#'
#' @import gt
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Total"),
#'   Distinct_Records = c(100, 150, 250),
#'   final = c(80, 120, 200),
#'   Precision = c(80.0, 80.0, 80.0),
#'   Recall = c(40.0, 60.0, 100.0),
#'   screened = c(90, 140, 230)
#' )
#' precision_sensitivity_table(sample_data)
precision_sensitivity_table <- function(data) {
  # First, we check if all values in the "screened" column are 0
  all_zero_screened <- all(data$screened == 0)
  
  # If all values are zero, we remove the "screened" column from the data
  if (all_zero_screened) {
    data <- data[ , !(names(data) %in% "screened")]
  }
  
  # Create the initial gt table
  gt_table <- data %>%
    gt::gt(rowname_col = "Source") %>%
    gt::tab_header(title = "Record Counts & Precision/Sensitivity") %>%
    
    # Label the columns
    gt::cols_label(
      `Distinct_Records` = "Distinct Records",
      final = "Final Included",
      Precision = "Precision",
      Recall = "Sensitivity/Recall"
    ) %>%
    
    # Align columns to the right
    gt::cols_align(
      align = "right",
      columns = c("final", "Precision", "Recall")
    )
  
  # If the "screened" column isn't all zeros, add its specific labels, alignment, and footnotes
  if (!all_zero_screened) {
    gt_table <- gt_table %>%
      gt::cols_label(screened = "Screened Included") %>%
      gt::cols_align(align = "right", columns = "screened") %>%
      gt::tab_footnote(
        footnote = "Number of citations included after title/abstract screening.",
        locations = gt::cells_column_labels(columns = "screened")
      ) %>%
      gt::tab_footnote(
        footnote = "Total citations included after Ti/Ab Screening.",
        locations = gt::cells_body(columns = "screened", rows = "Total")
      )
  }
  
  # Add remaining footnotes and return the gt_table
  gt_table %>%
    # Add footnotes for the columns
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(columns = "Distinct_Records")
    ) %>%
    gt::tab_footnote(
      footnote = "Number of citations included after full text screening.",
      locations = gt::cells_column_labels(columns = "final")
    ) %>%
    gt::tab_footnote(
      footnote = "Number of final included citations / Number of distinct records.",
      locations = gt::cells_column_labels(columns = "Precision")
    ) %>%
    gt::tab_footnote(
      footnote = "Number of final included citations / Total number of final included citations.",
      locations = gt::cells_column_labels(columns = "Recall")
    ) %>%
    gt::tab_footnote(
      footnote = "Total citations discovered (after internal and cross-source deduplication).",
      locations = gt::cells_body(columns = "Distinct_Records", rows = "Total")
    ) %>%
    gt::tab_footnote(
      footnote = "Total citations included after full text screening.",
      locations = gt::cells_body(columns = "final", rows = "Total")
    ) %>%
    gt::tab_footnote(
      footnote = "Overall Precision = Number of final included citations / Total distinct records.",
      locations = gt::cells_body(columns = "Precision", rows = "Total")
    )
}
