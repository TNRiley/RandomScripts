#' Detailed Record Table
#'
#' This function generates a formatted summary table that displays detailed 
#' counts for each citation source, including the number of records imported, 
#' distinct records, unique records, and non-unique records, along with 
#' contribution percentages.
#'
#' @param data A data frame containing detailed counts for each citation source. 
#'   It must include columns such as `Source`, `Records Imported`, `Distinct Records`,
#'   `Unique records`, `Non-unique Records`, `Source Contribution %`, 
#'   `Source Unique Contribution %`, and `Source Unique %`.
#'
#' @return A `gt` table object summarizing the detailed record counts for each citation source.
#'
#' @details 
#' The function uses the `gt` package to create a formatted table with labeled columns 
#' and footnotes that explain the meaning of each column. It also checks that the column 
#' names in the input data match the expected structure.
#'
#' @import gt
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' sample_data <- data.frame(
#'   Source = c("Source1", "Source2", "Total"),
#'   `Records Imported` = c(100, 150, 250),
#'   `Distinct Records` = c(90, 140, 230),
#'   `Unique records` = c(50, 70, 120),
#'   `Non-unique Records` = c(40, 70, 110),
#'   `Source Contribution %` = c("39.1%", "60.9%", "100%"),
#'   `Source Unique Contribution %` = c("41.7%", "58.3%", "100%"),
#'   `Source Unique %` = c("55.6%", "50%", "52.2%")
#' )
#' detailed_record_table(sample_data)
detailed_record_table <- function(data) {
  # Ensure column names match those in the data
  data %>%
    gt::gt(rowname_col = "Source") %>%
    gt::tab_header(title = "Record Summary") %>%
    
    # Label the columns
    gt::cols_label(
      `Records Imported` = "Records Imported",
      `Distinct Records` = "Distinct Records",
      `Unique records` = "Unique records",
      `Non-unique Records` = "Non-unique Records",
      `Source Contribution %` = "Records Contributed %",
      `Source Unique Contribution %` = "Unique Records Contributed %",
      `Source Unique %` = "Unique Records %"
    ) %>%
    
    # Add footnotes for the columns
    gt::tab_footnote(
      footnote = "Number of raw records imported from each database.",
      locations = gt::cells_column_labels(
        columns = "Records Imported"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Number of records after internal source deduplication.",
      locations = gt::cells_column_labels(
        columns = "Distinct Records"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Number of records not found in another source.",
      locations = gt::cells_column_labels(
        columns = "Unique records"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Number of records found in at least one other source.",
      locations = gt::cells_column_labels(
        columns = "Non-unique Records"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Percent distinct records contributed to the total number of distinct records.",
      locations = gt::cells_column_labels(
        columns = "Source Contribution %"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Percent of unique records contributed to the total unique records.",
      locations = gt::cells_column_labels(
        columns = "Source Unique Contribution %"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Percentage of records that were unique from each source.",
      locations = gt::cells_column_labels(
        columns = "Source Unique %"
      )
    ) %>%
    gt::tab_footnote(
      footnote = "Total citations discovered (after internal and cross-source deduplication).",
      locations = gt::cells_body(
        columns = "Distinct Records",
        rows = "Total"
      )
    )
}
