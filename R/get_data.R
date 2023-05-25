# TODO: change to get_data
# TODO: save_to should only be there for the forecast data 
get_data <- function(country, filename, save_to) {
  bucket <- get_bucket_name(country)
  if (is.null(save_to)){
    googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                        parseFunction = googleCloudStorageR::gcs_parse_rds)
  } else {
    googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                        saveToDisk = save_to) 
  }
}