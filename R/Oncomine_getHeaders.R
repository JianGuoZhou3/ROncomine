#' To get column headers
#'
#' @param data the data name
#'
#' @return string
#' @export
#'
#' @examples Oncomine_getHeaders(n)
Oncomine_getHeaders<-function(data){
  Headers=colnames(data)
  return(Headers)
}
