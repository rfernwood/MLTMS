#' Process and export data
#' 
#' Takes the triangular data in Frank.xlsx and converts it to long format,
#'   appending dev (development period in months) and calendar year in the
#'   process.
process_and_export <- function(input_path, output_path) {
  require(tidyverse)
  require(readxl)
  data <- input_path %>%
    excel_sheets() %>%
    set_names() %>%
    map(~ read_xlsx(path = input_path, sheet = .x)) %>%
    map( ~ .x %>%
           gather(dev, value, `12`:`528`) %>% # convert to long format
           mutate_at(vars(c("AccidentYear", "ClaimNb"), starts_with("Report")), 
                     as.integer) %>%
           rename(accident_year = AccidentYear,
                  claim_number = ClaimNb,
                  status = Status) %>%
           # calculate calendar year
           mutate(calendar_year = accident_year + as.integer(dev) / 12 - 1) %>%
           filter(value > 0)
    )
  
  liability_data <- data[["Liability"]] %>%
    rename(transaction_type = Transaction,
           occupancy = Occupancy,
           report_year = ReportingYear,
           cover_type = CoverType) %>%
    mutate(occupancy = occupancy %>% # convert occupancy to '01', '02', etc.
             gsub("^Lob", "", .) %>%
             str_pad(2, side = "left", pad = "0"),
           lob = "Liability")
  
  motor_data <- data[["Motor"]] %>%
    rename(transaction_type = Type,
           report_year = ReportYear) %>%
    mutate(lob = "Motor")
  
  # combined the datasets
  combined_data <- bind_rows(liability_data, motor_data)
  write_csv(combined_data, output_path)
}

process_and_export("data/Frank.xlsx", "data/combined_data_full.csv")
