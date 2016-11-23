#' @export
print.keyword_tool_result <- function(x, ...){
  cat('Status: ', x$response$status_code, '\n', sep = '')
  cat('Date: ',   format(x$response$date, "%Y-%m-%d %H:%M"), '\n', sep = '')
  contents <- try(Map(length, x$content$results))
  if (!inherits(contents, 'try-error')){
    cat('Content Lengths - \n')
    print(contents)
  }
}
