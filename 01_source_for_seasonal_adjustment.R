library(tidyverse)
library(lubridate)
source(here::here("R", "functions.R"))
rd_files <- list.files(here::here("raw_data"))
if (!file.exists(here::here("processed_data"))) dir.create(here::here("processed_data"))

#read in the data, convert to long format------------
unadjusted <- readxl::read_excel(here::here("raw_data", rd_files[endsWith(rd_files, "Unadj.xlsx")]))%>%
  mutate(date = tsibble::yearmonth(date))%>%
  pivot_longer(cols= -date, names_to = "name", values_to = "value")

#read in the reference week data------------- 
#(IF REFERENCE WEEK DATA NOT AVAILABLE COMMENT OUT NEXT 8 LINES)------
refwk_df <-readxl::read_excel(here::here("raw_data", "lfs_refwk.xlsx"))%>%
  mutate(date = as.Date(date))
ref_start <- c(year(min(refwk_df$date)), month(min(refwk_df$date)))
months <- colnames(refwk_df)[colnames(refwk_df) != "date"]
refwk_ts <- ts(zoo::zoo(refwk_df[, months], 
              order.by=refwk_df$date), 
              frequency = 12, 
              start = ref_start)

#nest by industry and map data to the adjustment function-----------
nested <- unadjusted%>%
  group_nest(name)%>%
  mutate(stl = map(data, possibly_season_adj, model = "stl"),
         x11 = map(data, possibly_season_adj, with_ref_week = TRUE, model = "x11") #IF REFERENCE WEEK DATA NOT AVAILABLE CHANGE TRUE TO FALSE.-----
         )
#unnest the seasonally adjusted data and calculate average of STL and X11--------
adjusted <- nested%>%
  select(-data)%>%
  unnest(cols = c(stl, x11), names_sep = "_")%>%
  mutate(season_adjust=map2_dbl(stl_season_adjust, x11_season_adjust, na_mean))%>%
  select(date = stl_date, name, season_adjust)
#save the adjusted data------------
adjusted_wide <- adjusted%>%
  pivot_wider(id_cols = date, names_from = name, values_from = season_adjust)
write_excel_csv(adjusted_wide, here::here("processed_data", "adjusted.xlsx"))

#OPTIONAL compare to other adjustment (COMMENT OUT if not desired)-------
source(here::here("R","compare_with_other.R"))
