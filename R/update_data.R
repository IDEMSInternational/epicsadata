update_data <- function(country, filename, saveto) {
  bucket <- get_bucket_name(country)
  googleCloudStorageR::gcs_get_object(object_name = filename, bucket = bucket, 
                                      saveToDisk = saveto)
  invisible(saveto)
}