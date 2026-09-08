# Rebuild the six bundled earnings series from the official ABS workbooks.
# Run from the package root. readxl is a preparation dependency only.
library(data.table)
release <- "may-2026"
base_url <- paste0("https://www.abs.gov.au/statistics/labour/",
                   "earnings-and-working-conditions/average-weekly-earnings-australia/",
                   release, "/")
series <- list(c("A84990050R", "A84990044V"),
               c("A84998735A", "A84998729F"),
               c("A85002157R", "A85002148L"))
for (table in seq_along(series)) {
  path <- tempfile(fileext = ".xlsx")
  download.file(paste0(base_url, sprintf("630200%d.xlsx", table)), path, mode = "wb")
  sheet <- readxl::read_excel(path, sheet = "Data1", col_names = FALSE,
                              col_types = "text", .name_repair = "minimal")
  for (sid in series[[table]]) {
    column <- which(vapply(sheet, function(x) identical(x[10L], sid), logical(1L)))
    stopifnot(length(column) == 1L, sheet[[column]][2L] == "$",
              sheet[[column]][5L] == "Biannual")
    date <- as.IDate(as.numeric(sheet[[1L]][-(1:10)]), origin = "1899-12-30")
    value <- as.double(sheet[[column]][-(1:10)])
    stopifnot(!anyNA(date), all(is.finite(value)), all(value > 0),
              all(diff(12L * year(date) + month(date)) == 6L))
    fwrite(data.table(date, value), file.path("inst", "extdata", paste0(sid, ".tsv")),
           sep = "\t")
  }
  unlink(path)
}
