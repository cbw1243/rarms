#' Request argument lists
#'
#' @param param parameter names.
#'
#' @export
#' @examples
#' # key <- 'Your key'
#' # param_search(param = 'state')
#'
#' #' # param_search(param = 'year')
#'
#' #' # param_search(param = 'category')

param_search <- function(param){
  if (!("key" %in% ls(envir = .GlobalEnv)) ) stop('Key is not found. Please set key = %your key here.%')
  key <- get("key", envir = .GlobalEnv)
  jsonlite::fromJSON(paste0('https://api.ers.usda.gov/data/arms/', param, '?api_key=', key))$data
}


