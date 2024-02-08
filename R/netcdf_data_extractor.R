#' Extract Data from NetCDF Files
#'
#' This function imports NetCDF files from a specified directory, extracts data based on provided cell IDs, and returns a combined data.table.
#'
#' @param directory_path Path to the directory containing NetCDF files.
#' @param cell_ids Cell IDs to extract data from each NetCDF file. Default is c(75, 125, 175, 225, 275, 325, 375, 425, 475).
#' @return A data.table containing the extracted data.
#'
#' @examples
#' \dontrun{
#'   extract_netcdf_data(directory_path = "/path/to/netcdf/files")
#'   extract_netcdf_data(directory_path = "/path/to/netcdf/files", cell_ids = c(1, 2, 3))
#' }
extract_netcdf_data <- function(directory_path, cell_ids = c(75, 125, 175, 225, 275, 325, 375, 425, 475)) {
  file_list <- list.files(path = directory_path,
                          recursive = TRUE,
                          pattern = ".nc",
                          full.names = TRUE)
  extracted_data <- lapply(
    X = file_list,
    FUN = function(file_path) {
      error <- tryCatch(
        {
          nc_file <- nc_open(filename = file_path)

          longitude <- ncvar_get(nc = nc_file, varid = "lon")
          latitude <- ncvar_get(nc = nc_file, varid = "lat")
          precipitation <- ncvar_get(nc = nc_file, varid = "pr")
          time_series <- nc.get.time.series(f = nc_file)

          nc_close(nc = nc_file)

          raster_data <- rast(x = precipitation)
          extent(raster_data) <- c(range(longitude), range(latitude))
          crs(raster_data) <- "epsg:4326"

          cell_coords <- xyFromCell(object = raster_data, cell = cell_ids)
          cell_values <- t(x = extract(x = raster_data, y = cell_coords))

          data <- data.table(time = time_series, cell_values)
        },
        error = function(error) {
          warning(paste("Error processing file:", file_path))
          return(NULL)
        }
      )

      if (inherits(x = error, what = "try-error")) {
        return(NULL)
      } else {
        return(data)
      }
    }
  )

  combined_data <- rbindlist(l = extracted_data, use.names = TRUE, fill = TRUE)

  return(combined_data)
}
