#' Calculate Intensity-Duration-Frequency (IDF) Curves
#'
#' This function computes IDF curves based on input data and returns a data.table containing the results.
#'
#' @param input_data A data.table containing the input data with columns for time and cell ID.
#' @return A data.table containing the computed IDF curves.
#'
#' @examples
#' \dontrun{
#'   calculate_idf_curves(input_data = my_data_table)
#' }
calculate_idf_curves <- function(input_data) {
  if (!is.data.table(input_data)) {
    stop("Input 'input_data' must be a data.table.")
  }
  melted_data <- melt(data = input_data, id.vars = "time", variable.name = "cell_id")

  split_data <- split(x = melted_data, f = melted_data$cell_id)

  calculate_idf <- function(data, return_periods = c(2, 5, 10, 25, 50, 100),
                            durations = c(1, 2, 5, 10, 24, 48),
                            aggregation_function = "mean", distribution = "gev", ...) {
    aggregated_data <- lapply(
      X = durations,
      FUN = function(duration) {
        result <- data[, .(time = time,
                           value = do.call(what = paste0("froll", aggregation_function),
                                           args = list(x = value,
                                                       n = duration,
                                                       align = "center",
                                                       fill = 0)))]
        result
      }
    )

    quantiles <- lapply(
      X = aggregated_data,
      FUN = function(aggregated) {
        max_values <- aggregated[, .(max_value = max(x = value, na.rm = TRUE)),
                                 by = year(x = time)]

        parameters <- tryCatch(
          {
            fit_distribution(data = max_values$max_value,
                             distribution = distribution,
                             n.points = 10,
                             norm = "N4",
                             constrain = FALSE)
          },
          error = function(e) {
            warning("Error fitting distribution. Skipping.")
            return(NULL)
          }
        )

        if (is.null(parameters)) return(NULL)

        probabilities <- 1 - 1/return_periods
        quantiles <- qgev(p = probabilities,
                          loc = parameters$loc,
                          scale = parameters$scale,
                          shape = parameters$shape)

        names(quantiles) <- return_periods
        as.list(quantiles)
      }
    )

    names(quantiles) <- durations
    quantiles_all <- rbindlist(l = quantiles, idcol = "duration")
    quantiles_idf <- melt(data = quantiles_all, id.vars = "duration", variable.name = "return_period")

    return(quantiles_idf)
  }

  idf_data <- lapply(X = split_data, FUN = calculate_idf)

  combined_idf_data <- rbindlist(l = idf_data, idcol = "cell_id", fill = TRUE)

  return(combined_idf_data)
}
