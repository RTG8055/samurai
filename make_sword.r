#' Get DDR data
#'
#' The DTCC Data Repository is a registered U.S. swap data repository that
#' allows market participants to fulfil their public disclosure obligations
#' under U.S. legislation. This function will give you the ability to download
#' trade-level data that is reported by market participants. The field names are
#' (and is assumed to be) the same for each asset class.
#'
#' @param date the date for which data is required as Date or DateTime object.
#'   Only the year, month and day elements of the object are used and it must of
#'   be length one.
#' @param asset_class the asset class for which you would like to download trade
#'   data. Valid inputs are \code{"CR"} (credit), \code{"IR"} (rates),
#'   \code{"EQ"} (equities), \code{"FX"} (foreign exchange), \code{"CO"}
#'   (commodities). This must be a string.
#' @param field_specs a valid column specification that is passed to
#'   [readr::read_csv()] with a default value provided by `ddr_field_specs()`
#' @return a tibble that contains the requested data. If no data exists
#'   on that date, an empty tibble is returned.
#' @examples
#' \dontrun{
#' library("lubridate")
#' fxddr(ymd(20170525), "IR") # Not empty
#' }
#' @references \href{https://rtdata.dtcc.com/gtr/}{DDR Real Time Dissemination
#' Platform}
#' @export
#' 
#' 
require(readr)
fxddr <- function(date, asset_class, field_specs = ddr_field_specs()) {
  assertthat::assert_that(
    lubridate::is.instant(date), length(date) == 1,
    assertthat::is.string(asset_class),
    asset_class %in% c("CR", "EQ", "FX", "IR", "CO")
  )
  on.exit(unlink(zip_path, recursive = TRUE))
  zip_path <- ddr_download(date, asset_class)
  on.exit(unlink(csv_path, recursive = TRUE), add = TRUE)
  csv_path <- unzip_(zip_path)
  if(is.na(csv_path)) {
    tibble::tibble()
  } else {
    # print(csv_path)
    readr::read_csv(csv_path, col_types = field_specs)
  }
}

ddr_download <- function(date, asset_class) {
  file_url <- ddr_url(date, asset_class)
  # print(file_url)
  zip_path <- file.path(tempdir(),
                        paste0(ddr_file_name(date, asset_class), ".zip"))
  tryCatch(expr = {
    res <- utils::download.file(file_url, zip_path, quiet = TRUE)
    if (res == 0) return(zip_path) else return(NA)},
    error = function(e) return(NA),
    warning = function(w) return(NA)
  )
}

#' @rdname ddr
#' @export
ddr_field_specs <- function() {
  readr::cols(
    .default = readr::col_character(),
    `Dissemination ID` = readr::col_integer(),
    `Original Dissemination ID` = readr::col_integer(),
    `Execution Timestamp` = readr::col_datetime(format = ""),
    `Effective Date` = readr::col_date(format = ""),
    `Expiration Date` = readr::col_date(format = ""),
    `Strike Price` = readr::col_number(),
    `Option Premium Amount` = readr::col_number(),
    `Option Premium Currency` = readr::col_character(),
    `Notional Amount 1`=readr::col_number(),
    `Notional Amount 2`=readr::col_number(),
    `Notional Currency 1`=readr::col_character(),
    `Notional Currency 2`=readr::col_character()
    
  )
}



ddr_file_name <- function (date, asset_class) {
  asset_map <- c("CR" = "CREDITS", 'EQ' = "EQUITIES", 'FX' = "FOREX",
                 'IR' = "RATES", 'CO' = "COMMODITIES")
  paste0("CFTC_CUMULATIVE_", asset_map[asset_class], "_", format(date, "%Y_%m_%d"))
}

unzip_ <- function(path) {
  if (is.na(path)) return(NA)
  if (!file.exists(path)) stop("The file ", path, " does not exist", call. = FALSE)
  extracted_files <- utils::unzip(path, exdir = tempdir(), list = TRUE)
  if (nrow(extracted_files) == 1) {
    utils::unzip(path, exdir = tempdir())
    return(file.path(tempdir(), extracted_files[["Name"]]))
  } else {
    return(NA)
  }
}

#stump <- "https://kgc0418-tdw-data2-0.s3.amazonaws.com/cftc/slices/"
ddr_url <- function (date, asset_class) {
  stump <- "https://kgc0418-tdw-data-0.s3.amazonaws.com/cftc/eod/"
  #https://kgc0418-tdw-data-0.s3.amazonaws.com/cftc/slices/CFTC_CUMULATIVE_FOREX_2021_02_23.zip
  paste0(stump, ddr_file_name(date, asset_class), ".zip")
}


# as.Date("2021-03-16")

# df<-fxddr(as.Date("2021-03-16"),"FX")
