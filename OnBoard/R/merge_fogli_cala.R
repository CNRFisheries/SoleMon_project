setwd("C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard")
library(dplyr)
library(purrr)
library(readxl)
library(stringr)

station.files=list.files("data/fogli_cala")


list.hauls=NULL
for(i in 1:length(station.files)){
  haul_id=str_remove(str_remove(station.files[i],'StationTablet_'),'.xlsx')
  xdat=read_excel(paste0("data/fogli_cala/",station.files[i]))
  xdat=xdat[!is.na(xdat$Station),]
  xdat.more=read_excel(paste0("data/fogli_cala/",station.files[i]), 
                       sheet = "NotesCala")
  xdat.more=xdat.more[1,]
  xdat.more$`Tara A (kg)`=ifelse(xdat.more$`Tara A (kg)`=='no', 0,xdat.more$`Tara A (kg)`)
  xdat.more$`Tara D (kg)`=ifelse(xdat.more$`Tara D (kg)`=='no', 0,xdat.more$`Tara D (kg)`)
  xdat.more$`WRapA (kg)`=as.numeric(xdat.more$`WRapA (kg)`)
  xdat.more$`Tara A (kg)`=ifelse(is.na(xdat.more$`Tara A (kg)`), 0,xdat.more$`Tara A (kg)`)
  xdat.more$`Tara D (kg)`=ifelse(is.na(xdat.more$`Tara D (kg)`), 0,xdat.more$`Tara D (kg)`)
  xdat.more$`WRapA (kg)`=as.numeric(xdat.more$`WRapA (kg)`)
  # day, haul, id,note, inizio, fine, valid, DB, country, peso_rapido_A, peso_rapido_D, peso_subcampione_a, peso_subcampione_b
  xdat$id=i
  xdat$valid=1
  xdat=xdat[,c(2,1,23,16,3,17,24)]
  names(xdat)=c('day', 'haul', 'id','note', 'inizio', 'fine', 'valid')
  xdat$DB=NA
  xdat$country='ITA'
  xdat$peso_rapido_A=xdat.more$`WRapA (kg)`-xdat.more$`Tara A (kg)`
  xdat$peso_rapido_D=xdat.more$`WRapD(kg)`-xdat.more$`Tara D (kg)`
  xdat$peso_subcampione_a=xdat.more$`WBenthos(kg)`
  xdat$peso_subcampione_b=NA
  if(!is.na(xdat$fine)){
    xdat$duration=as.numeric(xdat$fine-xdat$inizio)  
  }else{
    xdat$duration=15
  }
  list.hauls=rbind(list.hauls, xdat)
  
  # check data taken onboard
  sample.dat=read_excel(paste0("data/fogli_cala/",station.files[i]), 
             sheet = "Samples onboard")
  if(nrow(sample.dat)>0){
  
   write.csv(sample.dat, file=paste0('data/onboard_measures/haul_',haul_id ,'_onboard_meas.csv'),row.names = F) 
  }
  

}
writexl::write_xlsx(list.hauls, '/data/haul_order.xlsx')

# Create files for Trusts that relate to the fogli cala #

library(tidyr)
library(writexl)
library(lubridate)

setwd("C:/Users/a.palermino/OneDrive - CNR/github/SoleMon_project/OnBoard/data/fogli_cala")

# --- OUTPUT DIR ---
out_dir <- file.path(getwd(), "fogli_cala_trust")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("All excell will be saved in:\n", out_dir, "\n")

# Same parameters for each row#
survey_val <- "SOLEMON2025"
vessel_val <- "DES"

area_from_station <- function(station) {
  ifelse(station %in% c("45", "45bis"), "SLO", "ITA17")
}

# Function to read the time correctly and import it as trust manages to read it #

parse_date_any <- function(x) {
  xn <- suppressWarnings(as.numeric(x))
  out <- rep(as.Date(NA), length(x))
  
  # Case 1: Excel serial number (or numeric text)
  is_num <- !is.na(xn)
  out[is_num] <- as.Date(xn[is_num], origin = "1899-12-30")
  
  # Case 2: strings data
  xch <- trimws(as.character(x))
  xch[!is_num] <- ifelse(xch[!is_num] == "", NA_character_, xch[!is_num])
  
  parsed <- suppressWarnings(parse_date_time(
    xch[!is_num],
    orders = c("d/m/Y", "d-m-Y", "Y-m-d", "Y/m/d", "m/d/Y"),
    tz = "UTC"
  ))
  out[!is_num] <- as.Date(parsed)
  
  out
}

parse_time_any <- function(x) {
  x0 <- trimws(as.character(x))
  x0[x0 == ""] <- NA_character_
  
  # if it contains a date + time, keep only the time part
  x0 <- ifelse(!is.na(x0) & grepl("\\s", x0), sub("^.*\\s", "", x0), x0)
  
  # if it is an Excel number (even as a string) and does NOT contain “:” -> fraction of a day
  num <- suppressWarnings(as.numeric(x0))
  is_num <- !is.na(num) & !grepl(":", x0)
  
  out <- rep(NA_character_, length(x0))
  
  if (any(is_num)) {
    secs <- round(num[is_num] * 86400)
    out[is_num] <- sprintf("%02d:%02d:%02d",
                           (secs %/% 3600) %% 24,
                           (secs %/% 60) %% 60,
                           secs %% 60)
  }
  
  x1 <- x0[!is_num]
  x1 <- gsub("\\.", ":", x1)  # 14.35 -> 14:35
  x1 <- ifelse(!is.na(x1) & grepl("^\\d{1,2}:\\d{2}$", x1), paste0(x1, ":00"), x1)
  x1 <- ifelse(!is.na(x1) & grepl("^\\d{1}:\\d{2}:\\d{2}$", x1), paste0("0", x1), x1)
  
  out[!is_num] <- x1
  out
}

# 1) Merge StationTablet_* -> StationTablet_all.xlsx


files <- list.files(pattern = "^StationTablet_.*\\.(xlsx|xls)$", full.names = TRUE)
files <- files[!grepl("^~\\$", basename(files))]
if (length(files) == 0) stop("Nessun file StationTablet_*.xlsx trovato nella cartella!")

read_first_row <- function(f, sheet_idx) {
  read_excel(f, sheet = sheet_idx, col_types = "text") %>% slice(1)
}

station_all <- map_dfr(files, function(f) {
  haul_id <- basename(f) %>%
    str_remove("^StationTablet_") %>%
    str_remove("\\.(xlsx|xls)$")
  
  bind_cols(tibble(file = basename(f), haul_id = haul_id), read_first_row(f, 1))
})

notescala_all <- map_dfr(files, function(f) {
  haul_id <- basename(f) %>%
    str_remove("^StationTablet_") %>%
    str_remove("\\.(xlsx|xls)$")
  
  bind_cols(tibble(file = basename(f), haul_id = haul_id), read_first_row(f, 2))
})

stationtablet_all_path <- file.path(out_dir, "StationTablet_all.xlsx")
write_xlsx(
  list(Station = station_all,
       NotesCala = notescala_all
       ), 
  file.path(out_dir, "StationTablet_all.xlsx"))

# 2) StationData 2025 + StratumCode/Length(m) filling from 2024. Trust does not allow NA in these columns. They will be modified later on trust.

st <- read_excel(stationtablet_all_path, sheet = "Station", col_types = "text")

StationData <- st %>%
  transmute(
    Survey = survey_val,
    Area = area_from_station(Station),
    StationName = Station,
    Date = DateTime,
    StratumCode = NA_character_,
    `Length(m)` = NA_character_,
    Vessel = vessel_val,
    WindDirection = WindDirection,
    WindSpeed = NA_character_,
    SeaCondition = SeaCondition,
    WheaterCond = WheaterCond,
    BottomType = NA_character_,
    Notes = ifelse(is.na(Route) | Route == "", "Route", paste("Route", Route)),
    Validy = "Valid",
    Status = "Approved"
  )

StationDate2024_path <- "C:/Users/a.palermino/OneDrive - CNR/github/fogli_cala/StationDate2024.xlsx"
StationDate2024 <- read_excel(StationDate2024_path, col_types = "text") %>%
  mutate(StationName = as.character(StationName))

StationData_filled <- StationData %>%
  mutate(StationName = as.character(StationName)) %>%
  left_join(
    StationDate2024 %>% select(StationName, StratumCode, `Length(m)`),
    by = "StationName",
    suffix = c("", ".2024")
  ) %>%
  mutate(
    StratumCode = coalesce(StratumCode.2024, StratumCode),
    `Length(m)` = coalesce(`Length(m).2024`, `Length(m)`)
  ) %>%
  select(-StratumCode.2024, -`Length(m).2024`)

stationdata_path <- file.path(out_dir, "StationData2025.xlsx")
write_xlsx(list(StationData = StationData_filled), file.path(out_dir, "StationDate2025.xlsx"))

# 3) StationGear 2025


#  Wal(m)
wal_col <- names(st)[grepl("wal\\s*\\(m\\)", names(st), ignore.case = TRUE)][1]
if (is.na(wal_col)) stop("Nessuna colonna Wal(m) trovata nel foglio Station")

wal_by_station <- st %>%
  mutate(Station = trimws(as.character(Station))) %>%
  group_by(Station) %>%
  summarise(Wal = dplyr::first(na.omit(.data[[wal_col]])), .groups = "drop")

base_gear <- StationData_filled %>%
  transmute(
    Survey   = survey_val,
    Area     = Area,
    Station  = trimws(as.character(StationName)),
    GearNum  = "1",
    GearCode = "RAP",
    `HoS(m)` = "3.589999914",
    `VeS(m)` = "0.25",
    `SwL(m)` = NA_character_,
    `Wal(m)` = NA_character_,
    Support  = "0",
    Valid    = "Yes",
    Notes    = NA_character_,
    UserIns  = NA_character_
  ) %>%
  left_join(wal_by_station, by = "Station") %>%
  mutate(`Wal(m)` = Wal) %>%
  select(-Wal)

StationGear2025 <- bind_rows(base_gear, base_gear %>% mutate(GearNum = "2"))
stationgear_path <- file.path(out_dir, "StationGear2025.xlsx")
write_xlsx(list(StationGear2025 = StationGear2025),file.path(out_dir, "StationGear2025.xlsx"))


# 4) StationPoint 2025 (Start + End) 

# Start
StationPoint_start <- st %>%
  transmute(
    Survey = survey_val,
    Area = area_from_station(Station),
    StationName = Station,
    Date = as.POSIXct(
      paste(as.character(parse_date_any(DateTime)), parse_time_any(HourTimeStart)),
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%S"
    ),
    `Lat(°)` = `Lat(°)Start`,
    `Lat(')` = `Lat(')Start`,
    `N/S` = "N",
    `Lon(°)` = `Lon(°)Start`,
    `Lon(')` = `Lon(')Start`,
    `E/W` = "E",
    Route = Route,
    `Speed(kn)` = `Speed (kn)`,
    `Depth(m)` = `Depth Start (m)`,
    UserIns = NA_character_
  )

# End
StationPoint_end <- st %>%
  transmute(
    Survey = survey_val,
    Area = area_from_station(Station),
    StationName = Station,
    Date = as.POSIXct(
      paste(as.character(parse_date_any(DateTime)), parse_time_any(HourTimeEnd)),
      tz = "UTC",
      format = "%Y-%m-%d %H:%M:%S"
    ),
    `Lat(°)` = `Lat(°)End`,
    `Lat(')` = `Lat(')End`,
    `N/S` = "N",
    `Lon(°)` = `Lon(°)End`,
    `Lon(')` = `Lon(')End`,
    `E/W` = "E",
    Route = Route,
    `Speed(kn)` = `Speed (kn)`,
    `Depth(m)` = `Depth End (m)`,
    UserIns = NA_character_
  )

StationPoint <- bind_rows(StationPoint_start, StationPoint_end)

# Save
stationpoint_path <- file.path(out_dir, "StationPoint2025.xlsx")
write_xlsx(list(StationPoint2025 = StationPoint), file.path(out_dir,"StationPoint2025.xlsx"))

# =========================
# DONE
# =========================
cat("FINITO!\nOutput salvati in:\n", out_dir, "\n", sep = "")








