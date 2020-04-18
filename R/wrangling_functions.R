#' Sort through a directory of Telonics reports and read in a compiled dataset
#' of most recent broadcasts.
#'
#' @param  tidy Creates a tibble by default, \code{tidy = FALSE} returns a \code{data.frame}.
#' @param path Path of Telonics report direcory.
#' @examples
#' wrangle_telonics(path = "~/telonics/reports")
#' wrangle_telonics(path = "~/telonics/reports", tidy = FALSE)
#' @export

wrangle_telonics <- function (path, tidy = TRUE) {
  old <- getwd()
  setwd(path)
  files <- list.files()
  files <- subset(files, !grepl("kml", files))
  files <- subset(files, !grepl("Stat", files))
  new_files <- c()
  for (i in levels(as.factor(gsub("([0-9]+).*$", "\\1", files)))) {
    collar <- files[gsub("([0-9]+).*$", "\\1", files) == i]
    collar <- collar[as.numeric(gsub("([0-9]+).*$", "\\1", substr(collar, 9, 13))) ==
                       max(as.numeric(gsub("([0-9]+).*$", "\\1", substr(collar, 9, 13))))]
    new_files <- c(new_files, collar)
  }
  csv_list <- list()
  for (i in 1:length(new_files)) {
    csv_list[[i]] <- read.csv(new_files[i], header = TRUE, skip = 23, na.strings = "")
    csv_list[[i]]$ctn <- gsub("([0-9]+).*$", "\\1", new_files[i])
  }
  new_file <- do.call(rbind, csv_list)
  if (tidy) {
    new_file <- tibble::tibble(new_file)
  }
  setwd(old)
  new_file
}
