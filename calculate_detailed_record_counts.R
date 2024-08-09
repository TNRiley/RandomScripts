#' Calculate Detailed Record Counts
#'
#' This function calculates detailed record counts from unique citations, including 
#' the number of records imported, distinct records, unique records, and non-unique records 
#' for each citation source. It also calculates contribution percentages for each source.
#'
#' @param unique_citations A data frame containing unique citations. 
#'   The data frame must include the columns `cite_source`, `cite_label`, and `duplicate_id`.
#' @param n_unique A data frame containing counts of unique records, typically filtered 
#'   by specific criteria (e.g., `cite_label == "search"`).
#' @param labels_to_include An optional character vector of labels to filter the citations. 
#'   If provided, only citations matching these labels will be included in the counts.
#'   Default is NULL, meaning no filtering will be applied.
#'
#' @return A data frame with detailed counts for each citation source, including:
#'   - `Records Imported`: Total number of records imported.
#'   - `Distinct Records`: Number of distinct records after deduplication.
#'   - `Unique records`: Number of unique records specific to a source.
#'   - `Non-unique Records`: Number of records found in other sources.
#'   - `Source Contribution %`: Percentage contribution of each source to the total distinct records.
#'   - `Source Unique Contribution %`: Percentage contribution of each source to the total unique records.
#'   - `Source Unique %`: Percentage of unique records within the distinct records for each source.
#'
#' @details 
#' The function first checks if the required columns are present in the input data frames.
#' It then expands the `cite_source` column, filters the data based on the provided labels (if any),
#' and calculates various counts and percentages for each citation source. The function also adds 
#' a total row summarizing these counts across all sources.
#'
#' @import dplyr
#' @import tidyr
#' @import scales
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' citations <- data.frame(
#'   cite_source = c("Source1, Source2", "Source1", "Source3"),
#'   cite_label = c("Label1", "Label2", "Label3"),
#'   duplicate_id = c(1, 2, 3)
#' )
#' n_unique <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   unique = c(10, 20, 30)
#' )
#' calculate_detailed_record_counts(citations, n_unique)
calculate_detailed_record_counts <- function(unique_citations, n_unique, labels_to_include = NULL) {
  
  # Check if necessary columns exist in unique_citations
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
    return(data.frame(Source = character(), `Records Imported` = integer(), `Distinct Records` = integer(), `Unique records` = integer(), `Non-unique Records` = integer()))
  }
  
  # Count the occurrences of each source to determine the "Records Imported"
  records_imported <- df_filtered %>%
    group_by(cite_source) %>%
    summarise(`Records Imported` = n(), .groups = 'drop')
  
  # Count the unique duplicate_id values for each source to determine the "Distinct Records"
  distinct_records <- df_filtered %>%
    group_by(cite_source) %>%
    summarise(`Distinct Records` = n_distinct(duplicate_id), .groups = 'drop')
  
  # Filter n_unique data to only include records with 'search' as the citation label
  n_unique_citations_count <- n_unique %>%
    filter(cite_label == "search") %>%
    group_by(cite_source) %>%
    summarise(`Unique records` = sum(unique), .groups = 'drop') %>%
    filter(cite_source != "") %>%
    arrange(cite_source)
  
  # Merge the three counts (initial, distinct, unique) into a single dataframe
  detailed_counts <- left_join(records_imported, distinct_records, by = "cite_source") %>%
    left_join(n_unique_citations_count, by = "cite_source")
  
  # Calculate the number of non-unique records by subtracting the number of unique records from the total records
  detailed_counts <- detailed_counts %>%
    mutate(`Non-unique Records` = `Distinct Records` - `Unique records`)
  
  # Calculate and add three percentages: the contribution of each source to the total,
  # the contribution of unique records of each source to the total unique records,
  # and the proportion of unique records in each source's distinct records
  detailed_counts <- detailed_counts %>%
    mutate(`Source Contribution %` = `Distinct Records` / sum(`Distinct Records`, na.rm = TRUE),
           `Source Unique Contribution %` = `Unique records` / sum(`Unique records`, na.rm = TRUE),
           `Source Unique %` = `Unique records` / `Distinct Records`)
  
  detailed_counts <- detailed_counts %>%
    mutate(
      `Source Contribution %` = as.numeric(`Source Contribution %`),
      `Source Unique Contribution %` = as.numeric(`Source Unique Contribution %`),
      `Source Unique %` = as.numeric(`Source Unique %`)
    ) %>%
    mutate(
      `Source Contribution %` = scales::percent(`Source Contribution %`, accuracy = 0.1),
      `Source Unique Contribution %` = scales::percent(`Source Unique Contribution %`, accuracy = 0.1),
      `Source Unique %` = scales::percent(`Source Unique %`, accuracy = 0.1)
    )
  
  # Calculate the totals
  total_records_imported <- sum(detailed_counts$`Records Imported`, na.rm = TRUE)
  total_distinct_records <- nrow(unique_citations)
  total_unique_records <- sum(detailed_counts$`Unique records`, na.rm = TRUE)
  total_nonunique_records <- sum(detailed_counts$`Non-unique Records`, na.rm = TRUE)
  
  # Add totals to the detailed_counts dataframe
  detailed_counts <- tibble::add_row(detailed_counts, 
                                  cite_source = "Total", 
                                  `Records Imported` = total_records_imported,
                                  `Distinct Records` = total_distinct_records,
                                  `Unique records` = total_unique_records,
                                  `Non-unique Records` = total_nonunique_records)
  
  # Rename columns for consistency
  detailed_counts <- detailed_counts %>%
    rename(Source = cite_source)
  
  # Return the final counts dataframe
  return(detailed_counts)
}
