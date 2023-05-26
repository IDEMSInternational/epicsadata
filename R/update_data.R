# TODO: remove this function. We have renamed it as get_data.R now 
update_data <- function(country, filename, save_to) {
  bucket <- get_bucket_name(country)
  if (is.null(save_to)){
    googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                        parseFunction = googleCloudStorageR::gcs_parse_rds)
  } else {
    googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket,
                                        saveToDisk = save_to) 
  }
}