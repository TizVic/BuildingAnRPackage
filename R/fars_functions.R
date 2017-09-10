#' Read FARS file
#' 
#' This function read the FARS data file using read_csv from readr package using automatic import
#' for data type. Using read_csv, it reads standard and compressed csv.
#'     
#' @param filename A character string with name of the file to read. 
#'                 If filename do not exists the function stops execution.
#'     
#' @return a data frame tbl (tibble) with data read from file.
#'      
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples \dontrun{
#'     filename <- "accident_2015.csv.bz2" 
#'     myData <- function(filename)
#' }
#'  
#' @note The function makes a check if the file exists.
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates file name in FARS standard
#' 
#' Creates file name in the format of FARS files (accident_YYYY.csv.bz2) where YYYY 
#' is the year in 4 digit format
#'
#' @param year A numeric representing the year in 4 digits format YYYY 
#' 
#' @return A character string with the file name to read in the format accident_YYYY.csv.bz2
#'
#' @examples \dontrun{
#'     filename <- make_filename(2013)
#' } 
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("data/accident_%d.csv.bz2", year)
}

#' Read FARS files by year(s)
#'
#' Read one or multiple year from files in the FARS format where FARS is
#' US National Highway Traffic Safety Administration's Fatality Analysis 
#' Reporting System, which is a nationwide census providing the American public yearly data regarding 
#' fatal injuries suffered in motor vehicle traffic crashes.
#' 
#' @param years a vector (atomic or list) of integer(s) representing year in 4 digits format YYYY. 
#' 
#' @return a list of tibble. If a year is invalid return NULL for that year.
#' 
#' @note The function output a warning in case of invalid year. 
#'       If all years are invalid returns a list of NULL
#' 
#' @importFrom dplyr mutate select 
#' @importFrom magrittr "%>%"
#' 
#' @examples \dontrun{
#'     myData <- fars_read_years(c(2013, 2015))
#'     }
#' 
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~ year) %>% 
                                dplyr::select_(~ MONTH, ~ year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarise number of accident per month and per year(s)
#' 
#' The function calculate from FARS files the number of accidents per month and per year.
#' It permits to compare months year by year. FARS is
#' US National Highway Traffic Safety Administration's Fatality Analysis 
#' Reporting System, which is a nationwide census providing the American public yearly data regarding 
#' fatal injuries suffered in motor vehicle traffic crashes.
#' 
#' @param years a vector (atomic or list) of integer(s) representing year in 4 digits format YYYY. 
#'    Years must be presents in FARS data. 
#'  
#' @return a tibble data frame in the wide format
#' 
#' @import  dplyr
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#' 
#' @examples \dontrun{
#'     output <- fars_summarize_years(c(2013,2014))
#'     print(output)
#' } 
#' 
#' @export         
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by_(~ year, ~ MONTH) %>% 
                dplyr::summarize_(n = n()) %>%
                tidyr::spread_(~ year, n)
}

#' Draw accidents on a map by year and by state
#' 
#' Draws a map of where the incidents of a given year occurred, 
#' based on the data of longitude and latitude present in the FARS data. FARS is
#' US National Highway Traffic Safety Administration's Fatality Analysis 
#' Reporting System, which is a nationwide census providing the American public yearly data regarding 
#' fatal injuries suffered in motor vehicle traffic crashes.
#' 
#' @param state.num A numeric value with the state number to analyse. 
#'    Must be present in FARS data 
#' @param year A numeric representing the year in 4 digits format YYYY.
#'    Must be present in FARS data 
#' 
#' @return NULL. The function plots a map with points marking accidents 
#' 
#' @note the function stops if state.num is not present in FARS data. 
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples \dontrun{ 
#'     fars_map_state(1,2013) 
#'     }
#'   
#' @export                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~ STATE == state.num)
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
