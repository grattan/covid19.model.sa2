#' Update the time series data from pappubahry/COVID19
#'
#' @return
#' A named logical vector, indicating success for the given file.
#'
#' Invoked for its side-effect: changing the internal \code{fst} files
#' giving the time series of COVID-19 cases from
#' \url{https://github.com/HughParsonage/pappubahry-AU_COVID19}.
#'
#' Note that this changes the package's files: any original files will be
#' overwritten.
#'
#' @export

update_pappubahry <- function() {

  # When n > 3 we functionalize
  read_pappu <- function(nom) {
    stopifnot(!endsWith(nom, ".csv"))
    github_url <- paste0("https://github.com/pappubahry/AU_COVID19/raw/master/", nom, ".csv")
    dataraw.csv <- tempfile(fileext = ".csv")
    extdata.fst <- system.file("extdata", paste0(nom, ".fst"), package = packageName())

    result <-
      fread(github_url) %>%
      setnames("date", "Date", skip_absent = TRUE) %>%
      .[] %T>%
      fwrite(provide.file(dataraw.csv)) %>%
      .[, Date := as.Date(Date)] %>%

      # to avoid supurious executable warnings from `$ file `.
      .[, lgl := NA] %>%

      setkey(Date) %>%
      .[]
    if (file.exists(extdata.fst)) {
      write_fst(result, extdata.fst)
    }

    if (dir.exists("inst/extdata") &&
        Sys.getenv("USERNAME") == "hughp") {
      write_fst(result, paste0("inst/extdata/", nom, ".fst"))
    }

    TRUE
  }

  sapply(c("time_series_cases", "time_series_deaths",
           "time_series_recovered", "time_series_tests",
           "time_series_nsw_sources",
           "time_series_vic_sources"),
         read_pappu)

}


