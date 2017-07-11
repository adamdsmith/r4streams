#' Read HOBO logger csv files exported from HOBOware Pro
#'
#' @param hobo_csv character path to input `csv` file. If not specified, user is prompted to
#'  browse for the input `csv` file.
#'
#' @return a data.frame of logger serial #, date/time of logged temperature, and temperature
#'  (Fahrenheit)
#' @export
#' @examples
#' test <- read_hobo_csv(system.file("extdata", "test.csv", package = "r4streams"))
#' # No csv file specified; user asked to browse
#' test <- read_hobo_csv()

read_hobo_csv <- function (hobo_csv = NULL) {

  if (is.null(hobo_csv)) {
    ## Retrieve HOBO csv file
    hobo_csv <- utils::choose.files(default = "*.csv",
                                 caption = "Select HOBO .csv file.",
                                 multi = FALSE)
    if (length(hobo_csv) == 0) stop("Function cancelled.  No HOBO csv file selected.")
  } else if (!file.exists(hobo_csv)) stop("Cannot find that input file.")

  # Informed by https://github.com/RyanLab/microclimloggers
  # Extract first two lines using an encoding that removes BOM characters
  # at start of file, if present
  con <- file(hobo_csv, encoding="UTF-8")
  header <- readLines(con=con, n=2)
  close(con)

  #Split second header line containing column names, logger serial number, and time zone
  header_bits <- unlist(strsplit(header[2], '",\\"'))

  # Extract serial numbers
  SNs <-  stringr::str_extract(header_bits, '(?<=S\\/N:\\s)[0-9]+')
  SN <- unique(SNs[!is.na(SNs)])
  if (length(SN) > 1) stop("multiple serial numbers in file header")

  # Extract timezone
  tz <- stringr::str_extract(header_bits[grep("Date Time", header_bits)], "GMT[+-][0-9][0-9]") %>%
    ifelse(substr(., 5, 5) == 0, sub("0", "", .), .)
  tz <- paste0("Etc/", tz)

  # Read data, parse timestamp, and process relevant data
  hobo <- utils::read.csv(hobo_csv, skip=2, header=FALSE, stringsAsFactors = FALSE, na.strings = "") %>%
    # Parse timestamp
    mutate(dt = lubridate::mdy_hms(.[[2]], tz = tz),
           date = format(dt, "%m/%d/%Y"),
           time = format(dt, "%H:%M"),
           temp_f = .[[3]],
           logger_sn = SN) %>%
    select(logger_sn, dt, date, time, temp_f)

  hobo <- hobo[complete.cases(hobo), ]

  class(hobo) <- c("hobo", "data.frame")
  attr(hobo, "tzone") <- tz

  return(hobo)
}
