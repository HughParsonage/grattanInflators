## code to prepare `DATASET` dataset goes here
library(readabs)
CPI <- read_cpi()
WPI <- read_payrolls(series = "industry_wages")
WPI %>%
  .[state %ein% "Australia"] %>%
  .[industry %ein% "All industries"] %>%
  .[age %ein% "All ages"] %>%
  .[sex %ein% "Persons"]
