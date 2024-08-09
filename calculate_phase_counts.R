#' Calculate Phase Counts with Precision and Recall
#'
#' This function calculates the distinct record counts, as well as screened 
#' and final record counts, for each citation source across different phases 
#' (e.g., "screened", "final"). It also calculates precision and recall metrics 
#' for each source.
#'
#' @param unique_citations A data frame containing unique citations. 
#'   It must include the columns `cite_source`, `cite_label`, and `duplicate_id`.
#' @param n_unique A data frame containing counts of unique records. 
#'   Typically filtered by specific criteria, such as `cite_label == "search"`.
#' @param db_colname The name of the column representing the citation source 
#'   in the `unique_citations` data frame.
#'
#' @return A data frame with phase counts and calculated precision and recall 
#'   for each citation source, including:
#'   - `Distinct Records`: The count of distinct records per source.
#'   - `screened`: The count of records in the "screened" phase.
#'   - `final`: The count of records in the "final" phase.
#'   - `Precision`: The precision metric calculated as `final / Distinct Records`.
#'   - `Recall`: The recall metric calculated as `final / Total final records`.
#'
#' @details 
#' The function starts by calculating the total distinct records, as well as 
#' the total "screened" and "final" records across all sources. It then 
#' calculates distinct counts for each source, followed by counts for "screened" 
#' and "final" records. Finally, it calculates precision and recall metrics and 
#' adds a total row summarizing these counts across all sources.
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @export
#'
#' @examples
#' # Example usage with a sample dataset
#' citations <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   cite_label = c("screened,final", "screened", "final"),
#'   duplicate_id = c(1, 2, 3)
#' )
#' n_unique <- data.frame(
#'   cite_source = c("Source1", "Source2", "Source3"),
#'   unique = c(10, 20, 30)
#' )
#' calculate_phase_counts(citations, n_unique, "cite_source")
calculate_phase_counts <- function(unique_citations, n_unique, db_colname) {
  
  # Step 1: Calculate and store the totals before expanding
  total_distinct_records <- n_distinct(unique_citations$duplicate_id)
  
  # Split the cite_label column and count any occurrence of "screened" and "final"
  total_screened <- unique_citations %>%
    tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
    filter(cite_label == "screened") %>%
    nrow()
  
  total_final <- unique_citations %>%
    tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
    filter(cite_label == "final") %>%
    nrow()
  
  # Step 2: Proceed with the regular calculation for distinct records by source
  distinct_count <- unique_citations %>%
    tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",\\s*") %>%
    filter(!(!!rlang::sym(db_colname) == "unknown" | !!rlang::sym(db_colname) == "")) %>%
    group_by(!!rlang::sym(db_colname)) %>%
    summarise(Distinct_Records = n_distinct(duplicate_id), .groups = "drop") %>%
    rename(Source = !!rlang::sym(db_colname))
  
  # Calculate the number of "screened" and "final" records for each source after expanding
  source_phase <- unique_citations %>%
    dplyr::select(!!rlang::sym(db_colname), cite_label, duplicate_id) %>%
    tidyr::separate_rows(!!rlang::sym(db_colname), sep = ",\\s*") %>%
    tidyr::separate_rows(cite_label, sep = ",\\s*") %>%
    distinct() %>%
    dplyr::filter(!(!!rlang::sym(db_colname) == "unknown" | !!rlang::sym(db_colname) == "")) %>%
    dplyr::mutate(screened = ifelse(cite_label == "screened", 1, 0),
                  final = ifelse(cite_label == "final", 1, 0)) %>%
    dplyr::group_by(!!rlang::sym(db_colname)) %>%
    dplyr::summarise(screened = sum(screened),
                     final = sum(final),
                     .groups = "drop") %>%
    dplyr::rename(Source = !!rlang::sym(db_colname))
  
  # Combine the distinct counts with the source_phase
  combined_counts <- left_join(distinct_count, source_phase, by = "Source")
  combined_counts[is.na(combined_counts)] <- 0
  
  # Step 3: Calculate Precision and Recall
  combined_counts <- combined_counts %>%
    mutate(Precision = ifelse(Distinct_Records != 0, round((final / Distinct_Records) * 100, 2), 0)) %>%
    rowwise() %>%
    mutate(Recall = ifelse(total_final != 0, round((final / total_final) * 100, 2), 0))
  
  # Step 4: Calculate the total row using the pre-expansion totals
  totals <- tibble::tibble(
    Source = "Total", 
    Distinct_Records = total_distinct_records,  # Using the pre-expansion distinct count
    screened = total_screened,  # Using the correct total for screened
    final = total_final,  # Using the correct total for final
    Precision = ifelse(total_distinct_records != 0, round((total_final / total_distinct_records) * 100, 2), 0),
    Recall = NA  # Recall set to NA for the total row
  )
  
  phase_counts <- bind_rows(combined_counts, totals)
  
  return(phase_counts)
}
