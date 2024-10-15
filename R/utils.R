#' @title do_Q function
#' @description This function parses json content returned from an HTTP request
#' @param q a character
#' @param jj a character
#' @return The output of the function is ...
#' @author Paolo Tagliolato, PhD (2021) \email{tagliolato.p@@irea.cnr.it}
#' @importFrom jsonlite stream_in
#' @importFrom jqr jq select
#' @importFrom dplyr select
#' @importFrom dtplyr lazy_dt
#' @importFrom magrittr %>%
#' @noMd
#' @noRd
#' @keywords internal
#'
### function do_q
do_Q <- function(q, jj) {
  jj %>%
    jqr::jq(as.character(q)) %>%
    textConnection(encoding = "UTF-8") %>%
    jsonlite::stream_in(simplifyDataFrame = TRUE, verbose = FALSE) %>%
    dtplyr::lazy_dt()
}

#' internal function to retrieve json content by url
#' @noMd
#' @noRd
#' @keywords internal
#' @importFrom httr2 request req_method req_headers req_retry req_perform resp_check_status resp_body_string
get_jj <- function(url, ...){
  # Normal mode
  export <- httr2::request(base_url = url, ...) %>%
    httr2::req_method("GET") %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 120) %>%
    httr2::req_perform()
  httr2::resp_check_status(export)
  jj <- httr2::resp_body_string(export)
  return(jj)
}
