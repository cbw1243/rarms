#' Request USDA ARMS data
#'
#' The primary function in the \code{rarms} package to send request to the USDA ARMS Data API.
#'
#' An API key is needed for sending the data requests. To apply for a key, go to this website: \url{https://www.ers.usda.gov/developer/}. Then click Register.
#'
#' To make valid request, users should provide valid values for the input variables (e.g., report). Users can refer to this
#' USDA website for information on the input variables (\url{https://www.ers.usda.gov/developer/data-apis/arms-data-api/}).
#' Users can also use \code{paramSearch} in the package to get possible valid values for each input variable (see examples in \code{paramSearch}).
#'
#' For most input variables, users can use id or codes instead of original names to simplify the requests. For example, the id and code for Arkansas is 05 and
#' ar, respectively. Users can let state = '05' or state = 'ar' to request data for Arkansas instead of letting state = 'Arkansas'.
#' Similarily, the id for the report named Farm Business Balance Sheet is 1, users can simply let report = 1 to request data from this report.
#'
#' @param state optional character vector. It can be full state names, or state id, or state code. Use NULL if for all states.
#' @param year  numerical/character vector. Which year(s) to request? This is required.
#' @param category optional character vector. Category names or IDs.
#' @param report optional vector. Report names or IDs.
#' @param variable optional vector. Variable names or IDs.
#' @param farmtype optional vector. Farm type or ID.
#'
#' @return
#' The function returns a list with three elements: (1) request status, (2) request information, (3) the requested data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' key <- 'Your key' # Specify your API key here.
#' test <- armsRequest(year = c(2005:2018), state = c('05', '06'), category = c('age', 'ftyppl'),
#'  report = c(1, 2), variable = c('kount', 'accrop'), farmtype = 'all farms')
#' data <- test$data # Get the data returned from the ARMS data API.
#' }

armsRequest <- function(state = NULL, year, category = NULL, report = NULL, variable = NULL, farmtype = NULL){

  if (!("key" %in% ls(envir = .GlobalEnv)) ) stop('Key is not found. Please set key = %your key here.%')
  key <- get("key", envir = .GlobalEnv)

  if(is.null(state)) {
    state <- 'all'
  }else{
    state <- paste0(state, collapse = ',')
  }

  year <- paste0(year, collapse = ',')
  variable <- paste0(variable, collapse = ',')
  category <- paste0(category, collapse = ',')
  report <- paste0(report, collapse = ',')
  farmtype <- gsub(' ', '+', tolower(farmtype))

  url <- paste0('https://api.ers.usda.gov/data/arms/surveydata?api_key=', key,
                '&variable=', variable,
                '&year=', year,
                '&state=', state,
                '&report=', report,
                '&category=', category,
                '&farmtype=', farmtype)
  data <- jsonlite::fromJSON(url)
  return(data)
}





