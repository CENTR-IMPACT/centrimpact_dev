#' Clean Alignment Data
#'
#' @description Cleans and processes alignment data from a CSV file.
#' @param dirty_data Data frame containing the raw alignment data
#' @param project_name Name of the project (used for file naming)
#' @param report_date Date of the report (used for file naming)
#' @param is_qualtrics Logical indicating if data is from Qualtrics (not currently used)
#' @return A data frame with cleaned alignment data
#' @importFrom dplyr filter select mutate case_when
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom logger log_info log_error log_warn log_trace log_debug
#' @export

clean_alignment_data <- function(dirty_data) {
  tryCatch(
    {
      logger::log_info("Starting alignment data cleaning...")
      logger::log_trace("Input data dimensions - Rows: {if (is.null(dirty_data)) 0 else nrow(dirty_data)}, Columns: {if (is.null(dirty_data)) 0 else ncol(dirty_data)}")
      logger::log_trace("Input data columns: {if (is.null(dirty_data)) 'NULL' else paste(names(dirty_data), collapse = ', ')}")

      # Check if input data is valid
      logger::log_trace("Validating input data")
      if (is.null(dirty_data) || nrow(dirty_data) == 0) {
        logger::log_error("Input data validation failed: data is empty or NULL")
        stop("Input data is empty or NULL")
      }

      # Initialize clean_data
      logger::log_info("Initializing clean_data")
      clean_data <- dirty_data
      logger::log_info("Initial data dimensions - Rows: {nrow(clean_data)}, Cols: {ncol(clean_data)}")
      
      if ("StartDate" %in% names(clean_data)) {
        logger::log_info("Processing Qualtrics data")
        logger::log_info("Original row count: {nrow(clean_data)}")
        logger::log_info("Column names: {paste(names(clean_data), collapse = ', ')}")
        # Store column names from first row
        #col_names <- as.character(unlist(clean_data[1, ]))
        #logger::log_info("Column names: {paste(col_names, collapse = ', ')}")
        
        # Remove first 3 rows (headers + 2 metadata rows)
        clean_data <- clean_data[-c(1, 2), ]
        
        # Set column names
        #names(clean_data) <- col_names
        
        logger::log_info("After removing metadata rows - Rows: {nrow(clean_data)}")
        logger::log_info("Column names: {paste(names(clean_data), collapse = ', ')}")
        if(nrow(clean_data) > 0) {
          logger::log_info("First data row: {paste(clean_data[1, ], collapse = ' | ')}")
        }
        # First filter out Status = 1 if Status column exists
        if ("Status" %in% names(clean_data)) {
          logger::log_info("Filtering out Status = 1")
          clean_data <- dplyr::filter(clean_data, Status != 1)
        }
        clean_data <- clean_data[, -c(1:17)]
        logger::log_info("Qualtrics data cleaned and structured successfully")
      } else {
        logger::log_info("Using raw alignment data (already cleaned or not needing cleaning)")
        #clean_data <- dirty_data
      }

      # Select the required columns
      clean_data <- dplyr::select(
        clean_data,
        dplyr::matches("role"),
        dplyr::ends_with("_1")
      )

      # Rename columns ending with _1 by removing the suffix
      logger::log_trace("Preparing to rename columns ending with _1")
      cols_to_rename <- grep("_1$", names(clean_data), value = TRUE)

      if (length(cols_to_rename) > 0) {
        logger::log_debug("Columns to rename: {paste(cols_to_rename, collapse = ', ')}")
        new_names <- stringr::str_remove(cols_to_rename, "_1$")
        names(clean_data)[names(clean_data) %in% cols_to_rename] <- new_names
        logger::log_info("Renamed {length(cols_to_rename)} columns by removing '_1' suffix")
      }

      # Then select the required columns
      clean_data <- dplyr::select(
        clean_data,
        dplyr::matches("role"),
        dplyr::matches("^[A-Za-z]+\\d+$|^[A-Za-z]+_[A-Za-z]+$|^[A-Za-z]+$|^[A-Za-z]+\\.[A-Za-z]+$|^[A-Za-z]+_[A-Za-z]+$|^[A-Za-z]+\\d+[A-Za-z]*$|^[A-Za-z]+\\.[A-Za-z]+\\d+$|^[A-Za-z]+_[A-Za-z]+\\d+$|^[A-Za-z]+\\d+[A-Za-z]+$|^[A-Za-z]+\\d+[A-Za-z]+\\d+$|^[A-Za-z]+\\.[A-Za-z]+\\.[A-Za-z]+$|^[A-Za-z]+_[A-Za-z]+_[A-Za-z]+$|^[A-Za-z]+\\.[A-Za-z]+\\.[A-Za-z]+\\d+$|^[A-Za-z]+_[A-Za-z]+_[A-Za-z]+\\d+$|^[A-Za-z]+\\.[A-Za-z]+\\d+[A-Za-z]*$|^[A-Za-z]+_[A-Za-z]+\\d+[A-Za-z]*$|^[A-Za-z]+\\.[A-Za-z]+\\.[A-Za-z]+\\d+[A-Za-z]*$|^[A-Za-z]+_[A-Za-z]+_[A-Za-z]+\\d+[A-Za-z]*$")
      )

      # Clean up column names by removing any prefix before and including a dot or underscore
      logger::log_trace("Cleaning up column names")
      names(clean_data) <- stringr::str_remove(names(clean_data), ".*[._]")

      # Process role column if it exists
      if ("role" %in% names(clean_data)) {
        logger::log_trace("Processing role column")
        role_values_before <- unique(clean_data$role)
        clean_data <- clean_data |>
          dplyr::mutate(
            role = dplyr::case_when(
              role == 1 ~ "researcher",
              role == 2 ~ "partner",
              TRUE ~ as.character(role) # Keep original value if not 1 or 2
            )
          )
        logger::log_info("Role column processed. Values before: {paste(role_values_before, collapse = ', ')}, after: {paste(unique(clean_data$role), collapse = ', ')}")
        logger::log_trace("Role value counts: {paste(table(clean_data$role), collapse = ', ')}")
      } else {
        logger::log_info("No 'role' column found to process")
        logger::log_trace("Available columns: {paste(names(clean_data), collapse = ', ')}")
      }

      logger::log_info("Successfully cleaned alignment data (non-Qualtrics path)")
      logger::log_trace("Cleaned data dimensions - Rows: {nrow(clean_data)}, Columns: {ncol(clean_data)}")
      logger::log_trace("Cleaned data columns: {paste(names(clean_data), collapse = ', ')}")

      # Pivot the data to long format
      logger::log_trace("Preparing to pivot data to long format")
      if (!is.null(clean_data) && nrow(clean_data) > 0) {
        cols_to_pivot <- setdiff(names(clean_data), "role")
        logger::log_trace("Columns to pivot: {paste(cols_to_pivot, collapse = ', ')}")

        if (length(cols_to_pivot) > 0) {
          logger::log_trace("Pivoting {length(cols_to_pivot)} columns to long format")
          clean_data <- clean_data |>
            tidyr::pivot_longer(
              cols = dplyr::all_of(cols_to_pivot),
              names_to = "alignment",
              values_to = "rating"
            )

          logger::log_info("Successfully pivoted alignment data. Found {nrow(clean_data)} rows.")
          logger::log_trace("Pivoted data structure: {utils::str(clean_data, max.level = 1)}")
          logger::log_debug("Alignment values: {paste(unique(clean_data$alignment), collapse = ', ')}")
          logger::log_debug("Rating range: {paste(range(clean_data$rating, na.rm = TRUE), collapse = ' to ')}")
        } else {
          logger::log_warn("No columns to pivot in alignment data after initial cleaning")
          logger::log_debug("Available columns: {if (is.null(clean_data)) 'NULL' else paste(names(clean_data), collapse = ', ')}")
        }
      } else {
        logger::log_warn("No data to process (empty or NULL) after initial cleaning in clean_alignment_data. Returning empty data frame.")
        logger::log_trace("clean_data is NULL or has zero rows")
        return(data.frame(role = character(), alignment = character(), rating = numeric())) # Return empty data frame with expected structure
      }

      logger::log_info("Alignment data cleaning completed successfully")
      logger::log_trace("Final data dimensions - Rows: {nrow(clean_data)}, Columns: {ncol(clean_data)}")
      logger::log_trace("Final column names: {paste(names(clean_data), collapse = ', ')}")

      # Log data frame sample safely
      # if (logger::log_threshold() <= logger::DEBUG) {
      #   logger::log_debug("Sample of final data (first 3 rows):")
      #   # Use message() for data frame output to avoid glue issues
      #   message(paste0("\n", paste(utils::capture.output(utils::head(clean_data, 3)), collapse = "\n"), "\n"))
      # }

      logger::log_trace("Returning cleaned alignment data")
      return(clean_data)
    },
    error = function(e) {
      error_msg <- conditionMessage(e)
      logger::log_error("Error in clean_alignment_data: {error_msg}")
      logger::log_trace("Error details: {deparse(e)}")
      # Re-throw the error to be caught by the calling function
      stop(error_msg)
    }
  )
}
