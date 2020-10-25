#' Compile Telonics Reports
#'
#' @param tidy Creates a tibble by default, \code{tidy = FALSE} returns a \code{data.frame}.
#' @param path Path of Telonics report direcory.
#' @examples
#' wrangle_telonics(path = "~/telonics/reports")
#' wrangle_telonics(path = "~/telonics/reports", tidy = FALSE)
#' @export

wrangle_telonics <- function (path, tidy = TRUE) {
  old <- getwd()
  setwd(path)
  files <- subset(list.files(), grepl("Complete", list.files()))
  new_files <- c()
  for (i in levels(as.factor(gsub("([0-9]+).*$", "\\1", files)))) {
    collar <- files[gsub("([0-9]+).*$", "\\1", files) == i]
    collar <- collar[as.numeric(gsub("([0-9]+).*$", "\\1", substr(collar, 9, 13))) ==
                       max(as.numeric(gsub("([0-9]+).*$", "\\1", substr(collar, 9, 13))))]
    message(paste0("Data from file ", collar, " was retrieved."))
    new_files <- c(new_files, collar)
  }
  csv_list <- list()
  for (i in 1:length(new_files)) {
    csv_list[[i]] <- read.csv(new_files[i], header = TRUE, skip = 22, na.strings = "", stringsAsFactors = FALSE)
    csv_list[[i]]$ctn <- gsub("([0-9]+).*$", "\\1", new_files[i])
  }
  new_file <- do.call(dplyr::bind_rows, csv_list)
  if (tidy) {
    new_file <- tibble::tibble(new_file)
    message(paste0("\nOutput is a tibble of size ",
                  as.character(dim(new_file)[1]),
                  " x ",
                  as.character(dim(new_file)[2]),
                  " from ",
                  as.character(length(csv_list)),
                  " collars."))
  } else {
    message(paste0("\nOutput is a data.frame of size ",
                   as.character(dim(new_file)[1]),
                   " x ",
                   as.character(dim(new_file)[2]),
                   " from ",
                   as.character(length(csv_list)),
                   " collars."))
  }
  setwd(old)
  new_file
}


####################### NEXT IDEA: Meta_telonics -> write metadate based on telonics columns after cleaning.
