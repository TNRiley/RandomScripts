#Updated CiteSource Functions for allowing full functionality with reimported data

#' Count Records from Unique Citations (tested on working example 8/9/2024)
#'
#' This function processes a dataset of unique citations, expands the `cite_source` column,
#' filters based on user-specified labels (if provided), and then calculates the number
#' of records imported and distinct records for each citation source. It also adds a
#' total row summarizing these counts.
#'
#' @param unique_citations A data frame containing the unique citations. 
#'   It must contain the columns `cite_source`, `cite_label`, and `duplicate_id`.
#' @param labels_to_include An optional character vector of labels to filter the citations. 
#'   If provided, only citations matching these labels will be included in the counts.
#'   Default is NULL, meaning no filtering will be applied.
#'
#' @return A data frame containing the counts of `Records Imported` and `Distinct Records` 
#'   for each citation source. The data frame also includes a "Total" row summing 
#'   the counts across all sources.
#'
#' @details 
#' The function first checks if the required columns are present in the input data frame.
#' It then expands the `cite_source` column to handle multiple sources listed in a 
#' single row and filters the dataset based on the provided labels (if any).
#' The function calculates the number of records imported (total rows) and the number 
#' of distinct records (unique `duplicate_id` values) for each citation source.
#' Finally, a total row is added to summarize the counts across all sources.
#'
#' @import dplyr
#' @import tidyr
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' citations <- data.frame(
#'   cite_source = c("Source1, Source2", "Source1", "Source3"),
#'   cite_label = c("Label1", "Label2", "Label3"),
#'   duplicate_id = c(1, 2, 3)
#' )
#' count_records(citations)
count_records <- function(unique_citations, labels_to_include = NULL) {
  
  # Check if necessary columns exist
  required_columns <- c("cite_source", "cite_label", "duplicate_id")
  if (!all(required_columns %in% colnames(unique_citations))) {
    stop("The dataset does not contain the required columns.")
  }
  
  # Split and expand the cite_source column
  df_expanded <- unique_citations %>%
    separate_rows(cite_source, sep = ",") %>%
    mutate(cite_source = trimws(cite_source))
  
  # Filter by user-specified labels if provided
  if (!is.null(labels_to_include) && length(labels_to_include) > 0) {
    pattern <- paste(labels_to_include, collapse = "|")
    df_filtered <- df_expanded %>%
      filter(grepl(pattern, cite_label, ignore.case = TRUE))
  } else {
    df_filtered <- df_expanded
  }
  
  # Check if df_filtered is empty
  if (nrow(df_filtered) == 0) {
    return(data.frame(Source = character(), Records_Imported = integer(), Distinct_Records = integer()))
  }
  
  # Count the occurrences of each source to determine the "Records Imported"
  records_imported <- df_filtered %>%
    group_by(cite_source) %>%
    summarise(Records_Imported = n(), .groups = 'drop')
  
  # Count the unique duplicate_id values for each source to determine the "Distinct Records"
  distinct_records <- df_filtered %>%
    group_by(cite_source) %>%
    summarise(Distinct_Records = n_distinct(duplicate_id), .groups = 'drop')
  
  # Merge the two dataframes to get the final result
  initial_counts <- left_join(records_imported, distinct_records, by = "cite_source")
  
  # Calculate the total counts
  total_records_imported <- sum(initial_counts$Records_Imported)
  total_distinct_records <- sum(initial_counts$Distinct_Records)
  
  # Add the total counts to the result dataframe
  total_row <- data.frame(cite_source = "Total",
                          Records_Imported = total_records_imported,
                          Distinct_Records = total_distinct_records)
  initial_counts <- bind_rows(initial_counts, total_row)
  
  # Rename columns for consistency with gt table
  initial_counts <- initial_counts %>%
    rename(Source = cite_source)
  
  # Return the final result
  return(initial_counts)
}
