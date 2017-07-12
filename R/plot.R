#' Create an interactive dygraph/plot of processed HOBO logger data
#'
#' @param hobo hobo data.frame created by \code{\link{read_hobo_csv}}
#' @param stn character string indicating a relatively short, but descriptive name for the
#'  data logger location. This must be specified. See examples.
#' @param flag number of daily measurements *below which* temperature calculations are
#'  flagged to indicate a potentially inadequate number of observations. Default (36) flags
#'  daily values based on 75\% or less of expected observations (i.e., 48 30-min observations
#'  per day). Flagged days are indicated by a red bar along the x-axis.
#' @param save logical (default `FALSE`) indicating whether to save the time series plot
#'  to a standalone `html` file named based on `stn` argument. The `html` is saved to
#'  the current working directory unless `out_dir` is specified.
#' @param out_dir character string indicating the desired directory to output the `html`
#'  file if `save = TRUE`. Defaults to current working directory.
#' @param verbose logical (default `TRUE`) indicating whether to suppress messaging
#'  indicating where output `html` file was saved, if `save = TRUE`#'
#' @return a \code{\link[dygraphs]{dygraph}} interactive time series plot of daily average,
#'  minimum, and maximum stream temperatures
#' @export
#' @examples
#' \dontrun{
#' test <- read_hobo_csv(system.file("extdata", "test.csv", package = "r4streams"))
#' plot(test, "Test deployment")
#' plot(test, "Another test", save = TRUE, out_dir = "./test_output")
#' plot(test, "Third test, slightly more descriptive", save = TRUE,
#'      out_dir = "./test_output")
#' }

plot.hobo <- function(hobo, stn = NULL, flag = 36, save = FALSE,
                      out_dir = NULL, verbose = TRUE) {

  if (is.null(stn)) stop("Please provide a string for the `stn` argument ",
                         "indicating the logger location:\n",
                         "This will be used in the plot title and output filename.\n",
                         "For example: 'Reelfoot Lake NWR' or 'Noxubee NWR'.")

  if (!inherits(hobo, "hobo"))
    stop("Input must be created by `read_csv_hobo`.  See `?read_csv_hobo`.")

  dat <- hobo %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
    group_by(date) %>%
    summarize(n = n(),
              avg = mean(temp_f, na.rm = TRUE),
              min = min(temp_f, na.rm = TRUE),
              max = max(temp_f, na.rm = TRUE)) %>%
    select(date, n, avg, min, max) %>%
    ungroup() %>% as.data.frame()

  sparse <- rep(0, nrow(dat))
  sparse[which(dat$n < flag)] <- 1

  dat_ts <- dat %>% select(-n) %>%
    zoo::read.zoo(format = "%Y-%m-%d") %>%
    round(1)

  colors <- c("#66c2a5", "#8da0cb", "#fc8d62") # Green, blue, red

  p <- dygraph(dat_ts, main = paste(stn, "stream temperatures")) %>%
    dyOptions(colors = colors,
              axisLineWidth = 2, connectSeparatedPoints = FALSE,
              strokeWidth = 2) %>%
    dyAxis("y", label = "Water temperature (\u00b0F)", labelWidth = 24) %>%
    dySeries("avg", label = "AVG") %>%
    dySeries("min", label = "MIN") %>%
    dySeries("max", label = "MAX") %>%
    dyRibbon(data = sparse, top = 0,
             palette = c("#FFFFFF", "#FF0000")) %>%
    dyLegend(show = "follow", width = 275) %>%
    dyCSS(system.file("extdata", "stream_temp_dygraph.css", package = "r4streams")) %>%
    dyRangeSelector(height = 30, strokeColor = "")#, dateWindow = init_window)

  if (save) {
    if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
      install.packages("htmlwidgets", quiet = TRUE)
    }
    out_fn <- paste(gsub(" ", "_", stn), "stream_temps.html", sep = "_")

    htmlwidgets::saveWidget(p, file = out_fn)

    # Now move it...
    if (!is.null(out_dir)) {
      if (!dir.exists(out_dir)) dir.create(out_dir)
      file.rename(out_fn, file.path(out_dir, out_fn))
    }

    if (verbose)
      message(paste(strwrap(paste("Saved interactive water temperature plot for",
                                  shQuote(stn), "to:", normalizePath(file.path(out_dir, out_fn)))),
                    collapse = "\n"))
  }

  return(p)

}
