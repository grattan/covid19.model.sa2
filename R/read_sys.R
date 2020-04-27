#' Read package system file
#'
#' @param file.fst
#' \describe{
#'   \item{\code{character(1)}}{
#'     A string representing the path to the file.
#'     For convenience, can be of the form \code{<ENV VAR>:/<branch>} where
#'     \code{<ENV VAR>} is the name of an environment variable set to a
#'     path.
#' }}
#'
#' @param columns
#' \describe{\item{\code{character(n)}}{
#' As in \code{\link[fst:write_fst]{read_fst}}, a subset of columns
#' from \code{file.fst}. It differs from \code{\link[fst:write_fst]{read_fst}}
#' by only taking the intersection of \code{columns} and the columns
#' actually available.
#' }}
#'
#'
#' @param pattern
#' \describe{\item{\code{NULL; character(1)}}{
#' A regular expression,
#' another way to select columns from
#' \code{file.fst}.
#'
#' If \code{NULL}, the default, no additional columns are selected.
#' }}
#'
#' @param fst2_progress Whether to write progress to the console.
#'
#' @param ... Arguments passed to \code{read_fst}.
#'
#'
#'
#'
#' @export read_sys


read_sys <- function(file.fst,
                     columns = NULL,
                     pattern = NULL,
                     fst2_progress = getOption("covid19.model.sa2.fst2_progress"),
                     ...) {
  checkmate::assert_string(file.fst, pattern = "\\.(csv|fst)$")
  file.fst <- file_fst(file.fst)
  fst_metadata <- fst::fst.metadata(file.fst)
  fst_cols <- fst_metadata[["columnNames"]]
  fst2_progress <- isTRUE(fst2_progress) && isnt_testing()

  if (is.null(columns) && is.null(pattern)) {
    columns <- fst_cols
  }

  if (!is.null(pattern)) {
    Columns <- grep(pattern, x = fst_cols, perl = TRUE, value = TRUE)
    Columns <- intersect(union(columns, Columns), fst_cols)
  } else {
    Columns <- intersect(columns, fst_cols)
  }

  start.time <- start_progress(fst2_progress, file.fst = file.fst)

  ans <- fst::read_fst(file.fst, columns = Columns, as.data.table = TRUE, ...)
  end_progress(start.time, fst2_progress, file.fst = file.fst)
  ans[]
}



file_fst <- function(file.fst) {
  fileFst <- sub("\\.fst", "-fst", file.fst)
  if (nzchar(out <- system.file("extdata", fileFst, package = packageName()))) {
    return(out)
  }



  regexpr_colon <- regexpr(":", file.fst, fixed = TRUE)
  #
  # Don't match on regexp_colon =
  #  -1 (doesn't exist)
  #   0
  if (!file.exists(file.fst) && regexpr_colon > 2L) {
    the_sys_getenv <- Sys.getenv(substr(file.fst, 0L, regexpr_colon - 1L),
                                 unset = NA_character_)
    if (anyNA(the_sys_getenv)) {
      stop("`file.fst = ", file.fst, "` starts with env var '",
           substr(file.fst, 0L, regexpr_colon - 1L),
           "' but this is unset.", call. = FALSE)
    }
    return(file.path(the_sys_getenv, substring(file.fst, regexpr_colon + 1L)))
  }
  file.fst
}

start_progress <- function(fst2_progress, file.fst) {
  start.time <- Sys.time()
  if (fst2_progress) {
    cat(crayon::green(as.character(format(Sys.time(), format = "%H:%M:%S"))),
        crayon::red(basename(file.fst)), "\t")
  }
  start.time
}

end_progress <- function(start.time, fst2_progress, file.fst) {
  finish.time <- Sys.time()
  a <- start.time
  b <- finish.time
  ds <- second(b) - second(a)
  dm <- minute(b) - minute(a)
  dh <- hour(b) - hour(a) - (dm < 0L)
  dm <- dm %% 60L
  dm <- dm - (ds < 0)
  ds <- ds %% 60L
  if (fst2_progress) {
    cat(crayon::blue(paste0("+",
                            formatC(dm,
                                    width = 2,
                                    format = "d",
                                    flag = "0"),
                            ":",
                            formatC(ds,
                                    width = 2,
                                    format = "d",
                                    flag = "0"))),
        "\n",
        sep = "")
  }
  invisible(finish.time)
}
