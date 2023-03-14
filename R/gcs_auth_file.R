#' GCS Auth with json file
#'
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples
gcs_auth_file <- function(filename) {
  googleCloudStorageR::gcs_auth(filename)
}