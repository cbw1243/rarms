#' Request USDA ARMS data
#'
#' The primary function in the \code{rarms} package to send request to the USDA ARMS Data API.
#' The function returns a list contains request status, request information, and the requested data.
#' To make valid request, users should specify correct parameters. The USDA website provides details on how to
#' specify parameters (\url{https://www.ers.usda.gov/developer/data-apis/arms-data-api/}). Users can also use
#' \code{param_search} to see possible values for each argument in \code{armsRequest} (see examples in \code{param_search})
#'
#' Note that for most arguments, users can use id or codes to make easier requests. For example, the id and code for Arkansas is 05 and
#' ar, respectively. Users can let state = '05' or state = 'ar' to request data for Arkansas. Similarily, the id for the report named Farm Business
#' Balance Sheet is 1, users can let report = 1 to request data from this report.
#'
#'
#'
#' @param state optional character vector. It could be full state names or abbreviated state names. All states if NULL.
#' @param year  numerical vector. Which years to request? Required.
#' @param category optional character vector. Category names or IDs.
#' @param report optional vector. Report names or IDs.
#' @param variable optional vector. Variable names or IDs.
#' @param farmtype optional vector. Farm type or ID.
#'
#' @export
#'
#' @examples
#' # key <- 'Your key'
#'
#' # test <- armsRequest(year = c(2005:2018), state = c('05', '06'), category = c('age', 'ftyppl'),
#' # report = c(1, 2), variable = c('kount', 'accrop'), farmtype = 'all farms')
#'
#' # data <- test$data

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





