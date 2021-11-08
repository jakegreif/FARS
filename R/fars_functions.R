#' Read in FARS Data
#'
#' This is a simple function that reads in csv data and coerces it to a tibble
#' data frame object.
#'
#' @param filename The file path and/or file name of the csv file with FARS data
#' to read into R
#'
#' @return This function returns a tibble data frame containing FARS data, as
#' identified by the user.
#'
#' @importFrom readr tibble
#'
#' @examples
#' fars_read(filename = "accident_215.csv")
#'
#' @export
fars_read <- function(filename) {
  filepath <- paste0("data/", filename, sep = "")
  if(!file.exists(filepath))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filepath, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Generate File Name
#'
#' This is a simple function that appends a year, as defined by the user, to the
#' file name "accident_(year).csv.bz2".
#'
#' @param year The year that is appended to the file name.
#'
#' @return This function returns a file name with the year, as defined by the
#' user, appended.
#'
#' @examples
#' make_filename(year = 2021)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
   sprintf("accident_%d.csv.bz2", year)
 }

#' Read FARS files for one or more years
#'
#' This is a function that reads in FARS data and, based on the years provided
#' by the user, creates individual tibbles containing the month and year from
#' the observations in the corresponding FARS data.
#'
#' @param years The year(s) of FARS data to examine.
#'
#' @return This function returns a list of tibbles, each of which contains the
#' year and month from the observations in the corresponding FARS data.
#'
#' @importFrom dplyr
#'
#' @examples
#' fars_read_years(years = c(2013, 2014, 2015))
#' fars_read_years(years = 2015)
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    #filepath <- paste0("data/", file, sep = "")
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = "YEAR") %>%
        dplyr::select("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' FARS Summary
#'
#' This is a function that summarizes FARS data by the month and year. The user
#' defines the year(s) of FARS data to be summarized.
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a file name with the year, as defined by the
#' user, appended.
#'
#' @importFrom dplyr tidyr
#'
#' @examples
#' fars_summarize_years(years = c(2013, 2014, 2015))
#' fars_summarize_years(years = 2015)
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map accidents in a given state and year
#'
#' This is a function that takes a state and a year, and generates a map of the
#' state and then plots the areas where accidents occur.
#'
#' @param state.num A numerical code for a US state.
#' @param year The year of interest.
#'
#' @return This function returns a map of accients in a given state and year.
#'
#' @importFrom dplyr maps graphics
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
