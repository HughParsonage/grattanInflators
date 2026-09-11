# Prepare the bundled earnings snapshot using stable ABS series IDs.
# Install this checkout first, then run from the package root:
#   Rscript data-raw/earnings.R
#   Rscript data-raw/earnings.R catalogue [catalogue Git ref] [output directory]
# Or source this file and call prepare_earnings(). See earnings.md.
prepare_earnings <- function(source = c("readabs", "catalogue"),
                             catalogue_ref = "master",
                             output_dir = file.path("inst", "extdata")) {
  source <- match.arg(source)
  ids <- grattanInflators::content2series_id(c("awe", "awote"),
                                            c("original", "seasonal", "trend"))
  if (length(ids) != 6L || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Install the current grattanInflators checkout before preparing earnings data.")
  }
  staging <- tempfile("earnings-preparation-")
  dir.create(staging)
  on.exit(unlink(staging, recursive = TRUE))
  staged <- file.path(staging, paste0(ids, ".tsv"))

  if (source == "readabs") {
    if (!requireNamespace("readabs", quietly = TRUE)) {
      stop("Install readabs or use source = 'catalogue'.")
    }
    # Let readabs discover the latest release and parse the ABS metadata.
    # Bypass its local cache so a previous preparation cannot mask new data.
    data <- data.table::as.data.table(readabs::read_abs_series(
      ids, check_local = FALSE, retain_files = FALSE, show_progress_bars = FALSE
    ))
    stopifnot(all(c("series_id", "date", "value", "unit") %in% names(data)))
    for (i in seq_along(ids)) {
      rows <- data[data[["series_id"]] == ids[i]]
      if (!nrow(rows) || anyNA(rows$unit) || any(rows$unit != "$")) {
        stop("Missing earnings observations or unexpected units for ", ids[i])
      }
      index <- data.table::data.table(date = data.table::as.IDate(rows$date), value = rows$value)
      data.table::setorder(index, date)
      data.table::fwrite(index, staged[i], sep = "\t")
    }
  } else {
    if (length(catalogue_ref) != 1L || is.na(catalogue_ref) || !nzchar(catalogue_ref)) {
      stop("catalogue_ref must be one Git branch, tag or commit SHA.")
    }
    for (i in seq_along(ids)) {
      url <- grattanInflators:::find_hughparsonage_abs_catalogue(ids[i])
      url <- sub("/raw/master/", paste0("/raw/", catalogue_ref, "/"), url, fixed = TRUE)
      status <- utils::download.file(url, staged[i], mode = "wb", quiet = TRUE)
      if (status != 0L) stop("Download failed for ", ids[i])
    }
  }

  # Use the package's TSV reader and validation for both sources, including
  # trimming boundary scaffolding while rejecting missing interior values.
  # Fetch and validate every series before replacing any bundled file.
  indices <- Map(function(path, sid) {
    index <- grattanInflators:::read_cached_series(path, sid)
    if (grattanInflators:::date2freq(index$date) != 2L || any(index$value <= 0)) {
      stop("Expected positive half-yearly earnings observations for ", sid)
    }
    index
  }, staged, ids)
  for (i in seq_along(ids)) {
    data.table::fwrite(indices[[i]], staged[i], sep = "\t")
  }
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
    stop("Could not create output directory: ", output_dir)
  }
  if (!all(file.copy(staged, output_dir, overwrite = TRUE))) {
    stop("Could not write all prepared series to ", output_dir)
  }
  data.table::data.table(
    series_id = ids,
    source = source,
    catalogue_ref = if (source == "catalogue") catalogue_ref else NA_character_,
    first_date = as.Date(vapply(indices, function(x) as.integer(min(x$date)), integer(1L)),
                         origin = "1970-01-01"),
    last_date = as.Date(vapply(indices, function(x) as.integer(max(x$date)), integer(1L)),
                        origin = "1970-01-01"),
    observations = vapply(indices, nrow, integer(1L))
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 3L) stop("Usage: Rscript data-raw/earnings.R [readabs|catalogue] [Git ref] [output directory]")
  print(prepare_earnings(
    source = if (length(args)) args[1L] else "readabs",
    catalogue_ref = if (length(args) >= 2L) args[2L] else "master",
    output_dir = if (length(args) >= 3L) args[3L] else file.path("inst", "extdata")
  ))
}
