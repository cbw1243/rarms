#' Return values of the selected input variables
#'
#' @param param Name of input variable
#'
#' @return
#' A data set that contains possible values of the selected input variable.
#'
#' @export
#' @examples
#' \dontrun{
#' key <- 'Your key' # Specify your API key here.
#' paramSearch(param = 'state')
#' paramSearch(param = 'year')
#' paramSearch(param = 'category')
#' }

paramSearch <- function(param){
  if (!("key" %in% ls(envir = .GlobalEnv)) ) stop('Key is not found. Please set key = %your key here.%')
  key <- get("key", envir = .GlobalEnv)
  jsonlite::fromJSON(paste0('https://api.ers.usda.gov/data/arms/', param, '?api_key=', key))$data
}


