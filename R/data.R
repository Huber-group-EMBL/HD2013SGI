HD2013SGIdata <- function(dataset) {
  filename <- paste0(dataset, ".rds")
  resolved <- httr2::request("https://doi.org/10.5281/zenodo.21995286") |>
    httr2::req_method("HEAD") |>
    httr2::req_perform() |>
    httr2::resp_url()
  url <- paste0(resolved, "/files/", filename)

  cache <- tools::R_user_dir("HD2013SGI", "cache")
  if (!dir.exists(cache)) {
    dir.create(cache, recursive = TRUE)
  }
  file <- file.path(cache, filename)
  if (!file.exists(file)) {
    download.file(url, file, mode = "wb")
  }
  readRDS(file)
}
