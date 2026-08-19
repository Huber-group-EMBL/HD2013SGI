#' Download a dataset from the HD2013SGI package
#'
#' This function downloads a dataset from the HD2013SGI package and caches it
#' locally. If the dataset has already been downloaded, it will be loaded from
#' the cache instead of downloading it again.
#'
#' It replaces the previous datasets that were included in the package.
#'
#' @param dataset The name of the dataset to download. This should be one of the
#' following: "datamatrix", "datamatrixfull", "featuresPerWell", "Interactions",
#' "mainEffects", "nrOfInteractionsPerTarget", "QueryAnnotation",
#' "stabilitySelection", or "TargetAnnotation".
#'
#' @returns The requested dataset
#'
#' @examples
#' \donttest{
#' HD2013SGIdata("datamatrix")
#' }
HD2013SGIdata <- function(
  dataset = c(
    "datamatrix",
    "datamatrixfull",
    "featuresPerWell",
    "Interactions",
    "mainEffects",
    "nrOfInteractionsPerTarget",
    "QueryAnnotation",
    "stabilitySelection",
    "TargetAnnotation"
  )
) {
  dataset <- match.arg(dataset)
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
