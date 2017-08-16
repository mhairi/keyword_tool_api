#' Get Search Volume Data for Keywords
#'
#' Get search volume data though time, along with CPC and CMP data
#' for a set of up to 800 keywords.
#'
#' For more on how to use this see the Keyword Tool API documentation:
#' http://keywordtool.io/api/documentation
#'
#' @param keywords A vector of keywords to analyse. Max size 800.
#' @param api_key API key from keywordtool.io. If you have 'keyword_tool_key' set in options, then you do not need to provide this.
#' @param metrics_location Optional vector of location codes to get results from (see documentation for codes). Maximum 10 locations.
#' @param metrics_language Optional vector of language to search with. See `language_codes` for list of codes. Maximum 5 languages.
#' @param metrics_network Search network that will be used to pull the search volume data.
#' @param metrics_currency The currency that will be used to display cost-per-click (CPC) data. See 'currency_codes` for list of codes.
#' @param method Whether to request using get or post. Post is recommended for large requests.
#'
#' @return An object of class 'keyword_tool_result'.
#' @export
#'
#' @examples \dontrun{
#'
#' options(keyword_tool_key = '1234')
#'
#' results <-
#' get_search_volume(keywords = c('scottish salmon', 'scotland fishing'),
#'                   metrics_location = c(20339,20342, 2840), #Scotland, England and US
#'                   metrics_network = 'googlesearch',
#'                   metrics_currency = 'GBP')
#'
#'  str(results$content)
#'
#' }
get_search_volume <- function(keywords,
                              api_key          = NULL,
                              metrics_location = NULL,
                              metrics_language = NULL,
                              metrics_network  = c('googlesearchnetwork', 'googlesearch'),
                              metrics_currency = NULL,
                              method           = c('get', 'post')){


  if (is.null(api_key)){
    api_key <- options()$keyword_tool_key

    if (is.null(api_key)){
      stop('No API key given, and none set in options.')
    }
  }

  # Converting vector of keywords into JSON format
  if (length(keywords) > 800) stop('A maximum 800 keywords are accepted in a single API request.')

  keywords <- paste0('"', keywords, '"')
  keywords <- paste(keywords, collapse = ',')
  keywords <- paste0('[', keywords, ']')

  if (length(metrics_location) > 10) stop('A maximum 10 locations are accepted in a single API request.')
  if (length(metrics_language) > 5) stop('A maximum 5 languages are accepted in a single API request.')
  if (length(metrics_currency) > 1) stop('Only one currency value can be used in a request.')

  invalid_languages <- setdiff(metrics_language, language_codes$Value)
  if (length(invalid_languages) > 0) stop('Invalid language codes: ', invalid_languages)

  invalid_currencys <- setdiff(metrics_currency, currency_codes$Value)
  if (length(invalid_currencys) > 0) stop('Invalid currency codes: ', invalid_currencys)

  metrics_location <- paste(metrics_location, collapse = ',')
  metrics_language <- paste(metrics_language, collapse = ',')

  metrics_network <- match.arg(metrics_network)
  method <- match.arg(method)

  parameters <- list(
    'keyword'          = keywords,
    'apikey'           = api_key,
    'metrics_location' = metrics_location,
    'metrics_language' = metrics_language,
    'metrics_network'  = metrics_network,
    'metrics_currency' = metrics_currency,
    'output'           = 'json'
  )

  # Remove null parameters
  parameters <- Filter(function(x) !is.null(x), parameters)

  base_url <- 'http://api.keywordtool.io/v2/search/volume/google'

  if (method == 'get'){
    url <- httr::modify_url(base_url, query = parameters)
    response <- httr::GET(url)
  }

  if (method == 'post'){
    response <- httr::POST(base_url, body = parameters)
  }

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  content <- httr::content(response, "text")
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  if (!is.null(parsed$error)) {
    stop(httr::http_status(response)$message, '\n',
         'API Message: ', parsed$error$message, '\n',
         'API Code: ', parsed$error$code)
  }

  httr::stop_for_status(response)

  structure(
    list(
      content     = parsed,
      response    = response,
      raw_content = content,
      parameters  = as.list(match.call())
    ),
    class = "keyword_tool_result"
  )

}



