#----------------------------------------
# Conducts QC checks on CUVA wetland data. Output is reported in QC_VIBI_report.Rmd
#----------------------------------------
#
# Params to turn on when running within script. Otherwise set params in Rmd.
#
# library(wetlandHTLN)
# library(tidyverse)
# library(knitr) # for kable functions
# library(kableExtra) # for additional kable features
# #library(htmltools) # check what this is for before turning on
#
# importData()
#
# year_curr = 2025
# all_years = FALSE
# year_range = if(all_years == TRUE){2008:year_curr} else {year_curr}
# spp_list = "DB"
# year_hist = 2008:(year_curr-1)
# all_years = F

#---- Functions ----
# Summarize results of QC check
QC_check <- function(df, tab, check, type){
  result <- data.frame("Data" = tab, "Description" = check, "Num_Records" = nrow(df), "check_type" = type)
}

# function to make tables via kable
make_kable <- function(df, cap){
  QC_table <- if(nrow(df) > 0){
    if(nrow(df) > 1){
    kable(df, format = 'html', align = 'c', caption = cap)  |>
      kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                    full_width = TRUE, position = 'left', font_size = 12) |>
      row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
      collapse_rows(1, valign = 'top') |>
      row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
    } else if(nrow(df) == 1){
      kable(df, format = 'html', align = 'c', caption = cap)  |>
        kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                      full_width = TRUE, position = 'left', font_size = 12) |>
        row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
        row_spec(nrow(df), extra_css = 'border-bottom: 1px solid #000000;')
      }
    } else NULL
}

# Determine whether to include/drop tab in rmd output
tab_include <- function(df){ifelse(nrow(df) > 0, TRUE, FALSE)}

# Determine if table exists or is null used in eval for rmd
check_null <- function(table){
  if(!is.null(table)){table}
}

check_null_print <- function(table, tab_level = 4, tab_title){
  if(!is.null(table)){cat(paste0(rep("#", tab_level), collapse = ""), " ", tab_title, " {.tabset} ", "\n\n")}
  check_null(table)
}

#---- Compile Data ----
# import database tables to check raw data (ie, not views)
tryCatch(
  db <- DBI::dbConnect(drv = odbc::odbc(), dsn = "HTLN_wetlands"),
  error = function(e){stop(paste0("Unable to connect to DSN to check tbl_SamplingEvents vs tbl_SamplingPeriods."))})

  tbls <- c("tbl_SamplingEvents", "tbl_SamplingPeriods", "tbl_VIBI_Herb",
            "tbl_VIBI_Herb_Biomass", "tbl_VIBI_Woody", "tbl_BigTrees",
            "tlu_WetlndSpeciesList")
  tbl_import <- lapply(seq_along(tbls), function(x){
    tab1 <- tbls[x]
    tab <- DBI::dbReadTable(db, tab1)
    return(tab)})

  DBI::dbDisconnect(db)
  tbl_import <- setNames(tbl_import, tbls)
  list2env(tbl_import, envir = .GlobalEnv)

tluspp <- if(spp_list == "FQAI"){
  read.csv("./data/FQAI_species_list.csv")
} else {tlu_WetlndSpeciesList}

tbl_SamplingEvents$year <- format(as.Date(tbl_SamplingEvents$StartDate, format = "%m/%d/%Y", tz = "America/New_York"), "%Y")
tbl_SamplingPeriods$year <- format(as.Date(tbl_SamplingPeriods$StartDate, format = "%m/%d/%Y", tz = "America/New_York"), "%Y")
tbl_VIBI_Herb$year <- format(as.Date(substr(tbl_VIBI_Herb$EventID, 11, 19), format = "%Y%b%d", tz = "America/New_York"), "%Y")
tbl_VIBI_Herb_Biomass$year <- format(as.Date(substr(tbl_VIBI_Herb_Biomass$EventID, 11, 19), format = "%Y%b%d", tz = "America/New_York"), "%Y")
tbl_VIBI_Woody$year <- format(as.Date(substr(tbl_VIBI_Woody$EventID, 11, 19), format = "%Y%b%d", tz = "America/New_York"), "%Y")
tbl_BigTrees$year <- format(as.Date(substr(tbl_BigTrees$EventID, 11, 19), format = "%Y%b%d", tz = "America/New_York"), "%Y")

tbl_SamplingEvents_curr <- tbl_SamplingEvents |> filter(year %in% year_range)
tbl_SamplingPeriods_curr <- tbl_SamplingPeriods |> filter(year %in% year_range)
tbl_VIBI_Herb_curr <- tbl_VIBI_Herb |> filter(year %in% year_range)
tbl_VIBI_Herb_Biomass_curr <- tbl_VIBI_Herb_Biomass |> filter(year %in% year_range)
tbl_VIBI_Woody_curr <- tbl_VIBI_Woody |> filter(year %in% year_range)
tbl_BigTrees_curr <- tbl_BigTrees |> filter(year %in% year_range)

tbl_SamplingEvents_hist <- if(all_years == FALSE){tbl_SamplingEvents |> filter(!year %in% year_range)} else {tbl_SamplingEvents}
tbl_SamplingPeriods_hist <- if(all_years == FALSE){tbl_SamplingPeriods |> filter(!year %in% year_range)} else {tbl_SamplingPeriods}
tbl_VIBI_Herb_hist <- if(all_years == FALSE){tbl_VIBI_Herb |> filter(!year %in% year_range)} else {tbl_VIBI_Herb}
tbl_VIBI_Herb_Biomass_hist <- if(all_years == FALSE){tbl_VIBI_Herb_Biomass |> filter(!year %in% year_range)} else {tbl_VIBI_Herb_Biomass}
tbl_VIBI_Woody_hist <- if(all_years == FALSE){tbl_VIBI_Woody |> filter(!year %in% year_range)} else {tbl_VIBI_Woody}
tbl_BigTrees_hist <- if(all_years == FALSE){tbl_BigTrees |> filter(!year %in% year_range)} else {tbl_BigTrees}

#---- Individual View checking ----
#----- Locations -----
loc <- get("locations", env = HTLN_wetlands)
locv <- loc |> filter(FeatureTypes %in% c("VIBIPlotID", "VIBIplotID"))

# Check for LocationIDs that don't match convention of PARKWetlnd
locid_typos <- loc |> filter(!substr(LocationID, 1, 10) %in% c("CUVAWetlnd", "TAPRWetlnd")) |> select(LocationID, FeatureTypes, FeatureID)
QC_table <- QC_check(locid_typos, "Locations", "LocationIDs that don't follow PARKWetlnd naming convention.", "error")

tbl_locid_typos <- make_kable(locid_typos, "LocationIDs that don't follow PARKWetlnd naming convention (note the capitalization).")

# Check for the solo FeatureTypes (ie likely typo b/c only one of them)
feat_typos1 <- data.frame(table(loc$FeatureTypes)) |> filter(Freq == 1)
feat_typos <- loc |> filter(FeatureTypes %in% feat_typos1$Var1) |> select(LocationID, FeatureTypes, FeatureID) |> mutate(Num_sites = 1)

QC_table <- rbind(QC_table,
            QC_check(feat_typos, "Locations", "FeatureTypes only recorded once (likely typo).", "error"))
tbl_feat_typos <- make_kable(feat_typos, "FeatureTypes only recorded once (likely typo)- check capitalization.")

# Check for non-alphanumeric symbols in the FeatureID
feat_symb <- data.frame(loc[grepl("[^[:alnum:]]", loc$FeatureID), c("LocationID", "FeatureTypes", "FeatureID")])

QC_table <- rbind(QC_table,
                  QC_check(feat_symb, "Locations", "Special symbols in FeatureID. Consider revising.", "error"))

tbl_feat_symb <- make_kable(feat_symb, "Special symbols in FeatureID. Consider revising.")

# Check where DomVeg_Lev1_orig missing
miss_domveg <- locv |>
  filter(is.na(DomVeg_Lev1_orig)) |>
  select(LocationID, FeatureTypes, FeatureID, X1oPlants, DomVeg_Lev1 = DomVeg_Lev1_orig)

QC_table <- rbind(QC_table,
                  QC_check(miss_domveg, "Locations", "DomVeg_Lev1 missing a value needed for VIBI thresholds.", "error"))

tbl_miss_domveg <- make_kable(miss_domveg, "DomVeg_Lev1 missing a value needed for VIBI thresholds. Note that the DomVegID is also missing for those records.")

# Check where DomVeg_Lev1 doesn't mathe X1oPlants, the column used by importData to make the DomVeg_Lev1 column
conflict_domveg <- locv |>
  select(LocationID, FeatureTypes, FeatureID, X1oPlants, DomVeg_Lev1 = DomVeg_Lev1_orig) |>
  filter(!is.na(DomVeg_Lev1)) |>
  filter(!((DomVeg_Lev1 == "Emergent" & X1oPlants == "PEM") |
            (DomVeg_Lev1 == "Shrub" & X1oPlants == "PSS") |
              (DomVeg_Lev1 == "Forest" & X1oPlants == "PFO")))

QC_table <- rbind(QC_table,
                  QC_check(conflict_domveg, "Locations", "DomVeg_Lev1 doesn't match expected value for X1oPlants.", "error"))

tbl_conflict_domveg <- make_kable(conflict_domveg, "DomVeg_Lev1 doesn't match expected value for X1oPlants.")

# Check that dividing # modules and AreaHA result in 0.01ha
area_error <- locv |> mutate(mod_area = as.numeric(AreaHA)/as.numeric(TotalMods)) |> filter(mod_area != 0.01) |>
  select(LocationID, FeatureID, TotalMods, AreaHA, PlotConfig, mod_area)

QC_table <- rbind(QC_table,
                  QC_check(area_error, "Locations", "AreaHA and TotMods combination results in modules != 0.01ha.", "error"))

tbl_area_error <- make_kable(area_error, "AreaHA and TotalMods combination results in modules != 0.01ha")

# Check that IntensMods <= TotalMOds
intern_check <- locv |> filter(IntensMods > TotalMods) |>
  select(LocationID, FeatureID, TotalMods, IntensMods, PlotConfig)

QC_table <- rbind(QC_table,
                  QC_check(intern_check, "Locations", "IntensMods is greater than TotalMods.", "error"))

tbl_intern_check <- make_kable(intern_check, "IntensMods is greater than TotalMods")
# Check SamplingPeriods table for StartDate format- catch any that don't follow %m/%d/%Y

# check if locations checks returned at least 1 record to determine whether to include that tab in report
loc_check <- QC_table |> filter(Data %in% "Locations" & Num_Records > 0)
loc_include <- tab_include(loc_check)

#----- SamplingEvents/Periods/Locations checks -----
# Find EventIDs in Herbs, Woody, BigTree, Biomass that are missing from the SamplingEvents table

# Check that tbl_SamplingEvents$PeriodIDs are included in tbl_SamplingPeriods$PeriodID
miss_periods <- tbl_SamplingEvents_curr[!unique(tbl_SamplingEvents_curr$PeriodID) %in% unique(tbl_SamplingPeriods_curr$PeriodID),]

QC_table <- rbind(QC_table,
                  QC_check(miss_periods, "Periods/Events", "tbl_SamplingEvents.PeriodID missing from tbl_SamplingPeriods.PeriodID.", "error"))

tbl_miss_periods <- make_kable(miss_periods, "tbl_SamplingEvents$PeriodID missing from tbl_SamplingPeriods$PeriodID.")

# Find EventIDs in data tables missing from tbl_SamplingEvents
tbl_evs <- rbind(tbl_VIBI_Herb_curr |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_VIBI_Herb", pres = "X"),
                 tbl_VIBI_Woody_curr |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_VIBI_Woody", pres = "X"),
                 tbl_BigTrees_curr |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_BigTrees", pres = "X"),
                 tbl_VIBI_Herb_Biomass_curr |> select(LocationID, EventID) |> unique() |> mutate(table = "tbl_VIBI_Herb_Biomass", pres = "X")) |>
  pivot_wider(names_from = table, values_from = pres)

miss_samp_evs <- tbl_evs[!tbl_evs$EventID %in% tbl_SamplingEvents_curr$EventID,]

QC_table <- rbind(QC_table,
                  QC_check(miss_samp_evs, "Periods/Events", "EventIDs in VIBI tables that are missing from tbl_SamplingEvents.", "error"))


tbl_miss_samp_evs <- make_kable(miss_samp_evs, "EventIDs in VIBI tables that are missing from tbl_SamplingEvents.")

# Missing start dates in tbl_SamplingPeriods
miss_spd <- tbl_SamplingPeriods_curr |> filter(is.na(StartDate)) |>
  select(PeriodID, ParkCode, ProjectCode, Season, StartDate)

QC_table <- rbind(QC_table,
                  QC_check(miss_spd, "Periods/Events", "Sample Periods missing a StartDate", "error"))

tbl_miss_spd <- make_kable(miss_spd, "Sample Periods missing a StartDate")

# Create matrix of EventIDs by data type# CreateStartDate matrix of EventIDs by data type
samp_evs <- tbl_SamplingEvents_curr[,c("PeriodID", "EventID", "StartDate")]
samp_pds <- tbl_SamplingPeriods_curr[,c("PeriodID", "ProtocolVersion", "StartDate")]

samp_comb <- full_join(samp_pds, samp_evs, by = c("PeriodID"), suffix = c("_pd", "_ev")) |> select(-ProtocolVersion)
samp_comb$PeriodDate <- as.Date(sub("CUVAWetlnd", "", samp_comb$PeriodID), format = "%Y%b%d")
#samp_comb$PeriodDate <- as.Date(samp_comb$StartDate_pd, format = "%Y-%m-%d")
samp_comb$PeriodYear <- as.numeric(format(samp_comb$PeriodDate, format = "%Y"))
samp_comb$SampleDate <- as.Date(samp_comb$StartDate_ev, format = "%m/%d/%Y")

samp_comb2 <- samp_comb |> select(PeriodID, EventID, PeriodDate, PeriodYear, SampleDate) |>
  filter(PeriodYear %in% year_range)
herbs <- left_join(tbl_VIBI_Herb_curr, samp_comb2, by = "EventID")
woody <- left_join(tbl_VIBI_Woody_curr, samp_comb2, by = "EventID")
bmass <- left_join(tbl_VIBI_Herb_Biomass_curr, samp_comb2, by = "EventID")
bigtrees <- left_join(tbl_BigTrees_curr, samp_comb2, by = "EventID")

# Handle missing biomass, because data are entered later
if(nrow(bmass) > 0){
# Sample matrix with all table joins
sample_evs <- rbind(herbs |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                      mutate(table = "tbl_VIBI_Herb"),
                    woody |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                      mutate(table = "tbl_VIBI_Woody"),
                    bmass |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                      mutate(table = "tbl_VIBI_Herb_Biomass"),
                    bigtrees |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                      mutate(table = "tbl_BigTrees")) |>
  group_by(LocationID, PeriodID, PeriodYear, PeriodDate, table) |>
  summarize(date = as.character(first(SampleDate)),
            .groups = 'drop') |>
  pivot_wider(names_from = table, values_from = date)

sample_evs$Num_Samp_Mods <- rowSums(!is.na(sample_evs[,c("tbl_VIBI_Herb", "tbl_VIBI_Woody",
                                                         "tbl_VIBI_Herb_Biomass", "tbl_BigTrees")]))

sample_evs2 <- left_join(locv |> select(LocationID, FeatureID, X1oPlants), sample_evs, by = "LocationID") |>
  select(LocationID, FeatureID, X1oPlants, PeriodID, PeriodYear, PeriodDate, Herbs = tbl_VIBI_Herb,
         Biomass = tbl_VIBI_Herb_Biomass, Woody = tbl_VIBI_Woody, BigTrees = tbl_BigTrees, Num_Samp_Mods) |>
  arrange(FeatureID, PeriodYear) |> data.frame()

sample_evs3 <- sample_evs2 |> select(-PeriodDate) |> filter(!is.na(PeriodYear))

tbl_sample_evs <- kable(sample_evs3, format = 'html', align = 'c',
                        caption = paste0("HTLN wetland sampling matrix. Highlighted rows have fewer than 3 samples",
                                         " associated with a given PeriodID. Sample Dates are recorded for each table column."),
                         col.names = c("LocationID", "FeatureID", "DomVeg", "PeriodID", "PeriodYear",
                                       "Herbs", "Biomass", "Woody", "BigTrees", "Num Samples")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(4:ncol(sample_evs3), background = ifelse(sample_evs3$Num_Samp_Mods < 3, "#F2F2A0", "#ffffff")) |>
  collapse_rows(1:2, valign = 'top') |>
  row_spec(nrow(sample_evs3), extra_css = 'border-bottom: 1px solid #000000;') |>
  column_spec(3, width = "150px")

} else if(nrow(bmass) == 0){
  # Sample matrix with all table joins
  sample_evs <- rbind(herbs |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                        mutate(table = "tbl_VIBI_Herb"),
                      woody |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                        mutate(table = "tbl_VIBI_Woody"),
                      # bmass |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                      #   mutate(table = "tbl_VIBI_Herb_Biomass"),
                      bigtrees |> select(LocationID, PeriodID, PeriodYear, PeriodDate, SampleDate) |> unique() |>
                        mutate(table = "tbl_BigTrees")) |>
    group_by(LocationID, PeriodID, PeriodYear, PeriodDate, table) |>
    summarize(date = as.character(first(SampleDate)),
              .groups = 'drop') |>
    pivot_wider(names_from = table, values_from = date)

  sample_evs$Num_Samp_Mods <- rowSums(!is.na(sample_evs[,c("tbl_VIBI_Herb", "tbl_VIBI_Woody",
                                                           #"tbl_VIBI_Herb_Biomass",
                                                           "tbl_BigTrees")]))

  sample_evs2 <- left_join(locv |> select(LocationID, FeatureID, X1oPlants), sample_evs, by = "LocationID") |>
    mutate(Biomass = NA_character_) |>
    select(LocationID, FeatureID, X1oPlants, PeriodID, PeriodYear, PeriodDate, Herbs = tbl_VIBI_Herb,
           Biomass, Woody = tbl_VIBI_Woody, BigTrees = tbl_BigTrees, Num_Samp_Mods) |>
    arrange(FeatureID, PeriodYear) |> data.frame()

  sample_evs3 <- sample_evs2 |> select(-PeriodDate) |> filter(!is.na(PeriodYear))

  tbl_sample_evs <- kable(sample_evs3, format = 'html', align = 'c',
                          caption = paste0("HTLN wetland sampling matrix. Highlighted rows have fewer than 3 samples",
                                           " associated with a given PeriodID. Sample Dates are recorded for each table column."),
                          col.names = c("LocationID", "FeatureID", "DomVeg", "PeriodID", "PeriodYear",
                                        "Herbs", "Biomass", "Woody", "BigTrees", "Num Samples")) |>
    kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                  full_width = TRUE, position = 'left', font_size = 12) |>
    row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
    column_spec(4:ncol(sample_evs3), background = ifelse(sample_evs3$Num_Samp_Mods < 3, "#F2F2A0", "#ffffff")) |>
    collapse_rows(1:2, valign = 'top') |>
    row_spec(nrow(sample_evs3), extra_css = 'border-bottom: 1px solid #000000;') |>
    column_spec(3, width = "150px")
}

# Find PeriodIDs without EventIDs
# no_evs <- samp_comb |> filter(is.na(EventID))
#
# QC_table <- rbind(QC_table,
#                   QC_check(no_evs, "Periods/Events",
#                            "PeriodIDs in tbl_SamplingEvents that have no EventIDs associated with Herb, Biomass, Woody or BigTrees tables.", "error"))
#
#
# tbl_no_evs <- make_kable(no_evs, "PeriodIDs in tbl_SamplingEvents that have no EventIDs associated with Herb, Biomass, Woody or BigTrees tables.")

# Check SamplingPeriods table for StartDate format- catch any that don't follow %m/%d/%Y
tbl_SamplingPeriods_curr$StartDate_check <- format(as.Date(tbl_SamplingPeriods_curr$StartDate, format = "%Y-%m-%d"), format = "%Y-%m-%d")
tbl_SamplingEvents_curr$StartDate_check <- format(as.Date(tbl_SamplingEvents_curr$StartDate, tryFormats = c("%m/%d/%Y", "%m/%d/%y")), format = "%Y-%m-%d")

unparsed_dates <- tbl_SamplingEvents_curr |>
  filter(StartDate_check < as.Date("2008-01-01", format = "%Y-%m-%d") |
                                   StartDate_check > as.Date(Sys.Date(), format = "%Y-%m-%d") |
                                   is.na(StartDate_check)) |>
  select(EventID, StartDate, StartDate_check)

odd_dates1 <- unparsed_dates |> filter(is.na(StartDate_check)) |> select(EventID, StartDate)

QC_table <- rbind(QC_table,
                  QC_check(odd_dates1, "Periods/Events", "tbl_SamplingEvents with StartDates that can't parse.", "error"))

tbl_odd_dates1 <- make_kable(odd_dates1, "tbl_SamplingEvents with StartDates that can't parse.")

odd_dates2 <- unparsed_dates |> filter(!is.na(StartDate_check)) |> select(EventID, StartDate, StartDate_check)

QC_table <- rbind(QC_table,
                  QC_check(odd_dates2, "Periods/Events", "tbl_SamplingEvents with StartDates that don't parse properly.", "error"))

tbl_odd_dates2 <- make_kable(odd_dates2, "tbl_SamplingEvents with StartDates that don't parse properly or don't make sense.")

# check if periods/events checks returned at least 1 record to determine whether to include that tab in report
pev_check <- QC_table |> filter(Data %in% "Periods/Events" & Num_Records > 0)
pev_include <- tab_include(pev_check)

#----- Herbs -----
# Check for blanks in ScientificName and CovCode, include Comment_Herb
herb_blanks <- tbl_VIBI_Herb_curr |> filter(is.na(Species) | is.na(CovCode) | is.na(ModNo)) |>
  select(VIBI_Herb_ID, EventID, LocationID, Species, ModNo, CovCode, Comments)

QC_table <- rbind(QC_table,
                  QC_check(herb_blanks, "VIBI Herbs", "VIBI herb data with blanks in required fields.", "error"))

tbl_herb_blanks <- make_kable(herb_blanks, "VIBI herb data with blanks in required fields.")

# Check for Herb LocationIDs that don't have a match in the tbl_Locations
herb_locid <- anti_join(tbl_VIBI_Herb_curr, locv, by = "LocationID")

QC_table <- rbind(QC_table,
                  QC_check(herb_locid, "VIBI Herbs", "Tbl_VIBI_Herb.LocationIDs that don't have a match in tbl_Locations.LocationID.", "error"))

tbl_herb_locid <- make_kable(herb_locid, "tbl_VIBI_Herb.LocationIDs that don't have a match in tbl_Locations.LocationID.")

# Check for duplicate species within a given module
herb_dups <- tbl_VIBI_Herb_curr |> group_by(LocationID, EventID, ModNo, Species) |>
  summarize(num_records = sum(!is.na(CovCode)), .groups = "drop") |>
  filter(num_records > 1)

if(nrow(herb_dups) > 0){
  assign("duplicate_herb_spp", herb_dups, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(herb_dups, "VIBI Herbs", "Duplicate species records within the same LocationID, EventID, and Module.", "error"))

tbl_herb_dups <- make_kable(herb_dups, paste0("Duplicate species records within the same LocationID, EventID, and Module.",
"Out data frame is saved as 'duplicate_herb_spp' in global environment for easier review."))

# Find species recorded that are not on FQAI or tlu list
herb_miss <- anti_join(tbl_VIBI_Herb_curr, tluspp, by = c("Species" = "SCIENTIFIC_NAME")) |>
  arrange(Species, LocationID, EventID, ModNo) |> select(VIBI_Herb_ID, LocationID, EventID, Species, ModNo, Comments)

if(nrow(herb_miss) > 0){
  assign("unmatched_herb_spp", herb_miss, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(herb_miss, "VIBI Herbs", paste0("Species in tbl_VIBI_Herb that are not in the ", spp_list, " species list."),
                           "error"))

tbl_herb_miss <- make_kable(herb_miss, paste0("Species in tbl_VIBI_Herb that are not in the ", spp_list, " species list. ",
                                              "Output data frame is saved as 'unmatched_herb_spp' in global environment for easier review."))

# Find species that have only been recorded once (all_years = T) or were recorded for the first time in the latest year (all_years = F)
if(all_years == TRUE){
herb_once1 <- tbl_VIBI_Herb_curr |> group_by(Species) |>
  summarize(num_recs = sum(!is.na(CovCode))) |> filter(num_recs == 1) |> select(Species)

herb_once <- tbl_VIBI_Herb_curr |> filter(Species %in% herb_once1$Species) |> arrange(Species, LocationID, EventID)

QC_table <- rbind(QC_table,
                  QC_check(herb_once, "VIBI Herbs", "Species that have only been recorded once in tbl_VIBI_Herb.", "check"))

tbl_herb_once <- make_kable(herb_once, "Species that have only been recorded once in tbl_VIBI_Herb. Not necessarily an error, but good to check.")
} else {

herb_hist_list <- sort(unique(tbl_VIBI_Herb_hist$Species))
herb_new1 <- tbl_VIBI_Herb_curr |> filter(!Species %in% herb_hist_list)
herb_new2 <- right_join(locv |> select(LocationID, FeatureID), herb_new1, by = "LocationID") |>
  select(LocationID, FeatureID, EventID, year, Species, ModNo, CovCode, Comments) |>
  arrange(ModNo)

herb_new <- herb_new2 |>
  pivot_wider(names_from = ModNo, values_from = CovCode, names_prefix = "ModNo_") |>
  arrange(FeatureID, Species)

QC_table <- rbind(QC_table,
                  QC_check(herb_new, "VIBI Herbs", paste0("Species recorded for first time in ", year_curr, ". Numbers under Modules are cover classes."), "check"))

tbl_herb_once <- kable(herb_new, format = 'html', align = 'c',
                       caption = paste0("Species recorded for first time in ", year_curr, "."))  |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  column_spec(7:ncol(herb_new), border_left = T, border_right = T, color = "dimgrey") |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  collapse_rows(1:4, valign = 'top') |>
  row_spec(nrow(herb_new), extra_css = 'border-bottom: 1px solid #000000;')

# Herherb_new# Herbs new to a plot
herb_hist <- tbl_VIBI_Herb_hist |> select(LocationID, Species) |> mutate(hist = "X") |> unique()
herb_current <- tbl_VIBI_Herb_curr |> select(LocationID, EventID, year, Species, ModNo) |> mutate(curr = "X") |> unique()

herb_comb <- full_join(herb_hist, herb_current, by = c("LocationID", "Species")) |>
  mutate(new_spp = ifelse(is.na(hist) & !is.na(curr), "X", NA_character_)) |>
  filter(new_spp == "X") |>
  left_join(locv |> select(LocationID, FeatureID), by = "LocationID") |>
  select(LocationID, FeatureID, EventID, year, Species, ModNo) |>
  arrange(ModNo) |>
  mutate(present = "X") |>
  pivot_wider(names_from = ModNo, values_from = "present", names_prefix = "ModNo_") |>
  arrange(FeatureID, Species)

QC_table <- rbind(QC_table,
                  QC_check(herb_comb, "VIBI Herbs",
                           paste0("Species recorded for the first time in ", year_curr, " in a given plot."), "check"))

tbl_herb_plot_once <- kable(herb_comb, format = 'html', align = 'c',
                            caption = paste0("Species recorded for the first time in ", year_curr, " in a given plot."))  |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  column_spec(6:ncol(herb_comb), border_left = T, border_right = T, color = "dimgrey") |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  collapse_rows(1:4, valign = 'top') |>
  row_spec(nrow(herb_comb), extra_css = 'border-bottom: 1px solid #000000;')

}

# check if Herb checks returned at least 1 record to determine whether to include that tab in report
herb_check <- QC_table |> filter(Data %in% "VIBI Herbs" & Num_Records > 0)
herb_include <- tab_include(herb_check)

#----- Biomass -----
# Find blanks in DryWt_g
bmass_blanks <- tbl_VIBI_Herb_Biomass_curr |> filter(is.na(DryWt))

QC_table <- rbind(QC_table,
                  QC_check(bmass_blanks, "VIBI Biomass", "Blanks in DryWt field in tbl_VIBI_Herb_Biomass.", "error"))

tbl_bmass_blanks <- make_kable(bmass_blanks, "Blanks in DryWt field in tbl_VIBI_Herb_Biomass.")

# Find biomass eventIDs that don't have corresponding herb eventIDs for a given year
non_evs <-
  if(nrow(bmass) > 0){
  tbl_evs |> filter(!(tbl_VIBI_Herb %in% "X" & tbl_VIBI_Herb_Biomass %in% "X")) |>
  select(-tbl_VIBI_Woody, -tbl_BigTrees) |> filter(tbl_VIBI_Herb_Biomass %in% "X")
  } else {data.frame(LocationID = NA_character_,
                     EventID = NA_character_,
                     tbl_VIBI_Herb = NA_character_,
                     tbl_VIBI_Herb_Biomass = NA_character_)[-1,]}

if(nrow(non_evs) > 0){
  assign("unmatched_biomass_events", non_evs, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(non_evs, "VIBI Biomass",
                           paste0("Biomass sampling events that don't have a corresponding Herb sampling event in the same year. ",
                                  "Out data frame is saved as 'unmatched_biomass_events' in global environment for easier review."),
                           "check"))

tbl_non_evs <- make_kable(non_evs, paste0("Biomass sampling events that don't have a corresponding Herb sampling event in the same year. ",
                                          "Out data frame is saved as 'unmatched_biomass_events' in global environment for easier review."))

# Check for Biomass LocationIDs that don't have a match in the tbl_Locations
bmass_locid <- anti_join(tbl_VIBI_Herb_Biomass_curr, locv, by = "LocationID")

QC_table <- rbind(QC_table,
                  QC_check(bmass_locid, "VIBI Biomass",
                           "Tbl_VIBI_Herb_Biomass.LocationIDs that don't have a match in tbl_Locations.LocationID.", "error"))

tbl_bmass_locid <- make_kable(bmass_locid, "Tbl_VIBI_Herb_Biomass.LocationIDs that don't have a match in tbl_Locations.LocationID.")


# Check for negative biomass weights
bmass_neg <- tbl_VIBI_Herb_Biomass_curr |> filter(DryWt < 0)

QC_table <- rbind(QC_table,
                  QC_check(bmass_neg, "VIBI Biomass", "Negative DryWt value in tbl_VIBI_Herb_Biomass, likely a flag.", "error"))

tbl_bmass_neg <- make_kable(bmass_neg, "Negative DryWt value in tbl_VIBI_Herb_Biomass, likely a flag.")

# Check for duplicate samples within a given module/corner
bmass_dups <- tbl_VIBI_Herb_Biomass_curr |> group_by(LocationID, EventID, Module, Corner) |>
  summarize(counts = sum(!is.na(DryWt)), .groups = 'drop') |> filter(counts > 1)

QC_table <- rbind(QC_table,
                  QC_check(bmass_dups, "VIBI Biomass", "Duplicate DryWt records within the same LocationID, EventID, Module, and Corner.",
                           "error"))

tbl_bmass_dups <- make_kable(bmass_dups, "Duplicate DryWt records within the same LocationID, EventID, Module, and Corner.")

# Find biomass weights > 99% recorded
if(all_years == TRUE){
wt_99a <- tbl_VIBI_Herb_Biomass_curr |>
  filter(DryWt > 0) |>
  mutate(wt_99pct = quantile(DryWt, probs = 0.99, na.rm = T)) |>
  filter(DryWt > wt_99pct)

wt_99 <- wt_99a |> arrange(LocationID, Module, Corner) |>
  select(VIBI_Herb_Biomass_ID, EventID, LocationID, Module, Corner, DryWt, wt_99pct)

QC_table <- rbind(QC_table,
                  QC_check(wt_99, "VIBI Biomass", "Dry Weights that are >99% of all weights that have been recorded.", "check"))

tbl_wt_99 <- make_kable(wt_99, "Dry Weights that are >99% of all weights that have been recorded.")
} else {

wt99 <- quantile(tbl_VIBI_Herb_Biomass_hist$DryWt, probs = 0.99, na.rm = T)

wt_99 <- tbl_VIBI_Herb_Biomass_curr |> filter(DryWt > wt99) |>
  left_join(locv |> select(LocationID, FeatureID), by = "LocationID") |>
  mutate(DryWt_99pct = wt99) |>
  select(LocationID, FeatureID, EventID, year, Module, Corner, DryWt, DryWt_99pct) |>
  arrange(FeatureID, year, Module, Corner)

QC_table <- rbind(QC_table,
                  QC_check(wt_99, "VIBI Biomass", paste0("Dry Weights in ", year_curr, " that are >99% of weights that have been previously recorded."), "check"))

tbl_wt_99 <- make_kable(wt_99, paste0("Dry Weights in ", year_curr, " that are >99% of weights that have been previously recorded.", "check"))

}

# check if Biomass checks returned at least 1 record to determine whether to include that tab in report
biomass_check <- QC_table |> filter(Data %in% "VIBI Biomass" & Num_Records > 0)
biomass_include <- tab_include(biomass_check)

#----- Woody -----
# Find FeatureIDs in Woody table that don't match Locations via LocationID
woody_fid_check <- full_join(locv |> select(LocationID, FeatureID),
                             tbl_VIBI_Woody_curr |> select(LocationID, FeatureID),
                             by = "LocationID",
                             suffix = c("_loc", "_wdy")) |>
  filter(FeatureID_loc != FeatureID_wdy)

QC_table <- rbind(QC_table,
                  QC_check(woody_fid_check, "VIBI Woody", "FeatureIDs in tbl_VIBI_Woody that don't match FeatureIDs in tbl_Locations.", "error"))

tbl_woody_fid_check <- make_kable(woody_fid_check, "FeatureIDs in tbl_VIBI_Woody that don't match FeatureIDs in tbl_Locations, based on join of LocationID field. If two FeatureIDs look identical, there's likely a space in one of them, which R reads as different.")

# Check for blanks in ScientificName, DiamID, and Count
woody_blanks <- tbl_VIBI_Woody_curr |> filter(is.na(Scientific_Name) | is.na(Count) | is.na(DiamID))

QC_table <- rbind(QC_table,
                  QC_check(woody_blanks, "VIBI Woody", "Blanks in Scientific_Name, DiamID, or Count fields in tbl_VIBI_Woody.", "error"))

tbl_woody_blanks <- make_kable(woody_blanks, "Blanks in Scientific_Name, DiamID, or Count fields in tbl_VIBI_Woody.")

# Check for Woody LocationIDs that don't have a match in the tbl_Locations
woody_locid <- anti_join(tbl_VIBI_Woody_curr, locv, by = "LocationID")

QC_table <- rbind(QC_table,
                  QC_check(woody_locid, "VIBI Woody", "Tbl_VIBI_Woody.LocationIDs that don't have a match in tbl_Locations.LocationID.", "error"))

tbl_woody_locid <- make_kable(woody_locid, "Tbl_VIBI_Woody.LocationIDs that don't have a match in tbl_Locations.LocationID.")

# Check for -9999 Counts
woody9s <- tbl_VIBI_Woody_curr |> filter(Count < 0)

QC_table <- rbind(QC_table,
                  QC_check(woody9s, "VIBI Woody", "Negative 9999 counts, indicating flag.", "error"))

tbl_woody9s <- make_kable(woody9s, "Negative 9999 counts, indicating a flag.")

# Check for DiamIDs that don't match tlu b/c of capitalization
corr_diamids <- c("BIG", "C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10")

diamid_check <- tbl_VIBI_Woody_curr |> filter(!DiamID %in% corr_diamids)

QC_table <- rbind(QC_table,
                  QC_check(diamid_check, "VIBI Woody", "DiamIDs that don't match lookup table options, mostly differences in capitalization or due to blanks.", "error"))

tbl_diamid_check <- make_kable(diamid_check, "DiamIDs that don't match lookup table options, mostly differences in capitalization or due to blanks.")

# Check for duplicate species within a given module
woody_dups <- tbl_VIBI_Woody_curr |> group_by(LocationID, EventID, Module_No, DiamID, Scientific_Name) |>
  summarize(counts = sum(!is.na(Count)), .groups = 'drop') |> filter(counts > 1)

if(nrow(woody_dups) > 0){
  assign("duplicate_woody_spp", woody_dups, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(woody_dups, "VIBI Woody",
                           "Species in tbl_VIBI_Woody that have duplicate records within the same LocationID, EventID, Module, and DiamID.", "error"))

tbl_woody_dups <- make_kable(woody_dups, paste0("Species in tbl_VIBI_Woody that have duplicate records within the same LocationID, EventID, Module, and DiamID.",
                                              "Out data frame is saved as 'duplicate_woody_spp' in global environment for easier review."))

# Find counts > 99% of ever recorded per DiamID
if(all_years == TRUE){
diam_99a <- tbl_VIBI_Woody_curr |>
  mutate(DiamID = toupper(DiamID)) |> filter(Count > 0) |>
  group_by(DiamID) |> mutate(count_99pct = quantile(Count, probs = 0.99, na.rm = T)) |>
  filter(Count > count_99pct)

diamid_sort <- data.frame(DiamID = corr_diamids, order = c(12, 1:11))

diam_99 <- left_join(diam_99a, diamid_sort, by = "DiamID") |> arrange(order, LocationID, Module_No, Scientific_Name) |>
  select(VIBI_Woody_ID, EventID, LocationID, Module_No, Scientific_Name, DiamID, Count, count_99pct)

QC_table <- rbind(QC_table,
                  QC_check(diam_99, "VIBI Woody", "Counts within DiamID that are >99% of all records in that size class that have been recorded.", "check"))

tbl_diam_99 <- make_kable(diam_99, "Counts within DiamID that are >99% of all records in that size class that have been recorded. Not necessarily an error, but good to check")
} else {

count99_sz <- tbl_VIBI_Woody_hist |> group_by(DiamID) |>
  summarize(Count_99pct = round(quantile(Count, probs = 0.99, na.rm = T), 0))

count99_curr <- left_join(tbl_VIBI_Woody_curr, count99_sz, by = "DiamID") |>
  filter(Count > Count_99pct) |>
  select(LocationID, FeatureID, EventID, year, Species = Scientific_Name, DiamID, Count, Count_99pct) |>
  arrange(FeatureID, year, DiamID)

QC_table <- rbind(QC_table,
                  QC_check(count99_curr, "VIBI Woody", paste0("Counts in ", year_curr,
                                                              " within DiamID that are >99% of counts that have been previously recorded.
                                                              The Count field is the database value. The Count_99pct is the threshold for the check."), "check"))

tbl_diam_99 <- make_kable(count99_curr, paste0("Counts in ", year_curr, " within DiamID that are >99% of counts that have been previously recorded.
                                               The Count field is the database value. The Count_99pct is the threshold for the check."))
}

# Find counts > 99% overall
if(all_years == TRUE){
count_99a <- tbl_VIBI_Woody_curr |>
  filter(Count > 0) |>
  mutate(count_99pct = quantile(Count, probs = 0.99, na.rm = T)) |>
  filter(Count > count_99pct)

count_99 <- count_99a |> arrange(LocationID, Module_No, Scientific_Name) |>
  select(VIBI_Woody_ID, EventID, LocationID, Module_No, Scientific_Name, DiamID, Count, count_99pct)

QC_table <- rbind(QC_table,
                  QC_check(count_99, "VIBI Woody", "Counts that are >99% of all counts that have been recorded.", "check"))

tbl_count_99 <- make_kable(count_99, "Counts that are >99% of all counts that have been recorded. Not necessarily an error, but good to check")
} else {
  #++++ ENDED HERE ++++
  count_99 <- tbl_VIBI_Woody_hist |>
    mutate(Count_99pct = round(quantile(Count, probs = 0.99, na.rm = T), 0)) |>
    filter(Count > Count_99pct) |>
    select(LocationID, FeatureID, EventID, year, Species = Scientific_Name, DiamID, Count, Count_99pct) |>
    arrange(FeatureID, year, DiamID)

  count99 <- tbl_VIBI_Woody_hist |> summarize(Count_99pct = round(quantile(Count, probs = 0.99, na.rm = T), 0))

  count99_curr <- tbl_VIBI_Woody_curr |> mutate(Count_99pct = count99$Count_99pct) |>
    filter(Count > Count_99pct) |>
    select(LocationID, FeatureID, EventID, year, Species = Scientific_Name, DiamID, Count, Count_99pct) |>
    arrange(FeatureID, year, DiamID)

  QC_table <- rbind(QC_table,
                    QC_check(count99_curr, "VIBI Woody", paste0("Counts in ", year_curr, " that are >99% of counts that have been previously recorded.
                                                                 The Count field is the database value. The Count_99pct is the threshold for the check."), "check"))

  tbl_count_99 <- make_kable(count99_curr, paste0("Counts in ", year_curr, " that are >99% of counts that have been previously recorded.
                                                   The Count field is the database value. The Count_99pct is the threshold for the check."))
}

# Find species recorded that are not on FQAI or tlu list
woody_miss <- anti_join(tbl_VIBI_Woody_curr, tluspp, by = c("Scientific_Name" = "SCIENTIFIC_NAME")) |>
  arrange(Scientific_Name, LocationID, EventID, Module_No) |>
  select(VIBI_Woody_ID, LocationID, EventID, Scientific_Name, Module_No, DiamID, Count)

if(nrow(woody_miss) > 0){
  assign("unmatched_woody_spp", woody_miss, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(woody_miss, "VIBI Woody", paste0("Species in tbl_VIBI_Woody that are not in the ", spp_list, " species list."), "error"))

tbl_woody_miss <- make_kable(woody_miss, paste0("Species in tbl_VIBI_Woody that are not in the ", spp_list, " species list.",
                                                "Output data frame is saved as 'unmatched_woody_spp' in global environment for easier review."))

# Find species that have only been recorded once (all_years = T) or were recorded for the first time in the latest year (all_years = F)
if(all_years == TRUE){
woody_once1 <- tbl_VIBI_Woody_curr |> group_by(Scientific_Name) |>
  summarize(num_recs = sum(!is.na(Count))) |> filter(num_recs == 1) |> select(Scientific_Name)

woody_once <- tbl_VIBI_Woody_curr |> filter(Scientific_Name %in% woody_once1$Scientific_Name) |> arrange(Scientific_Name, LocationID, EventID)

QC_table <- rbind(QC_table,
                  QC_check(woody_once, "VIBI Woody", "Species that have only been recorded once in tbl_VIBI_Woody.", "check"))

tbl_woody_once <- make_kable(woody_once, "Species that have only been recorded once in tbl_VIBI_Woody. Not necessarily an error, but good to check.")
} else {
  # woody spp new to park
  woody_hist_list <- sort(unique(tbl_VIBI_Woody_hist$Scientific_Name))
  woody_new1 <- tbl_VIBI_Woody_curr |> filter(!Scientific_Name %in% woody_hist_list)
  woody_new <- woody_new1 |> group_by(LocationID, FeatureID, EventID, year, Species = Scientific_Name, ModNo = Module_No) |>
    summarize(Count = sum(Count, na.rm = T), .groups = 'drop') |>
    arrange(ModNo) |>
    pivot_wider(names_from = ModNo, values_from = Count, names_prefix = "ModNo_") |>
    arrange(FeatureID, Species)

  QC_table <- rbind(QC_table,
                    QC_check(woody_new, "VIBI Woody", paste0("Species recorded for first time in ", year_curr, ". Numbers under Modules are total stem counts."), "check"))

  tbl_woody_once <- kable(woody_new, format = 'html', align = 'c',
                          caption = paste0("Species recorded for first time in ", year_curr, "."))  |>
    kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                  full_width = TRUE, position = 'left', font_size = 12) |>
    column_spec(6:ncol(woody_new), border_left = T, border_right = T, color = "dimgrey") |>
    row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
    collapse_rows(1:3, valign = 'top') |>
    row_spec(nrow(woody_new), extra_css = 'border-bottom: 1px solid #000000;')


  # woody spp new to a plot
  woody_hist <- tbl_VIBI_Woody_hist |> select(LocationID, Species = Scientific_Name) |> mutate(hist = "X") |> unique()
  woody_current <- tbl_VIBI_Woody_curr |> select(LocationID, Species = Scientific_Name, ModNo = Module_No) |> mutate(curr = "X") |> unique()

  woody_comb <- full_join(woody_hist, woody_current, by = c("LocationID", "Species")) |>
    mutate(new_spp = ifelse(is.na(hist) & !is.na(curr), "X", NA_character_)) |>
    filter(new_spp == "X") |>
    select(LocationID, Species, ModNo) |>
    left_join(locv |> select(LocationID, FeatureID), by = "LocationID") |>
    select(LocationID, FeatureID, Species, ModNo) |>
    arrange(ModNo) |>
    mutate(present = "X") |>
    pivot_wider(names_from = ModNo, values_from = "present", names_prefix = "ModNo_") |>
    arrange(FeatureID, Species)

  QC_table <- rbind(QC_table,
                    QC_check(woody_comb, "VIBI Woody",
                             paste0("Species recorded for the first time in ", year_curr, " in a given plot."), "check"))

  tbl_woody_plot_once <- kable(woody_comb, format = 'html', align = 'c',
          caption = paste0("Species recorded for first time in ", year_curr, "."))  |>
    kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                  full_width = TRUE, position = 'left', font_size = 12) |>
    column_spec(4:ncol(woody_comb), border_left = T, border_right = T, color = "dimgrey") |>
    row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
    collapse_rows(1:2, valign = 'top') |>
    row_spec(nrow(woody_comb), extra_css = 'border-bottom: 1px solid #000000;')
}

# check if Woody checks returned at least 1 record to determine whether to include that tab in report
woody_check <- QC_table |> filter(Data %in% "VIBI Woody" & Num_Records > 0)
woody_include <- tab_include(woody_check)

#----- Big Trees -----
# Check for blanks in ScientificName or DBH
bigt_blanks <- tbl_BigTrees_curr |> filter(is.na(Scientific_Name) | is.na(DBH))

QC_table <- rbind(QC_table,
                  QC_check(bigt_blanks, "Big Trees", "Blanks in Scientific_Name or DBH fields in tbl_BigTrees.", "error"))

tbl_bigt_blanks <- make_kable(bigt_blanks, "Blanks in Scientific_Name or DBH fields in tbl_BigTrees.")

# Check on BigTree counts that don't match between Woody and BigTree tables.
bigt_sum <- tbl_BigTrees_curr |> group_by(LocationID, EventID, ModNo, Scientific_Name) |>
  summarize(count_bigt = sum(!is.na(DBH)), .groups = 'drop')

woody_comb <- full_join(tbl_VIBI_Woody |> filter(DiamID %in% c("BIG", "Big")),
                       bigt_sum,
                       by = c("LocationID", "EventID", "Module_No" = "ModNo",
                              "Scientific_Name")) |>
  filter(Count != count_bigt) |>
  select(VIBI_Woody_ID, LocationID, EventID, Module_No, Scientific_Name, Count_Woody = Count, Count_BigTrees = count_bigt) |>
  arrange(LocationID, EventID, Scientific_Name)

QC_table <- rbind(QC_table,
                  QC_check(woody_comb, "Big Trees", "Counts between tbl_VIBI_Woody and tbl_BigTrees that don't match.", "error"))

tbl_woody_comb <- make_kable(woody_comb, "Counts between tbl_VIBI_Woody and tbl_BigTrees that don't match.")

# Check for BigTree LocationIDs that don't have a match in the tbl_Locations
bigt_locid <- anti_join(tbl_BigTrees_curr, locv, by = "LocationID")

QC_table <- rbind(QC_table,
                  QC_check(bigt_locid, "Big Trees", "Tbl_BigTrees.LocationIDs that don't have a match in tbl_Locations.LocationID, likely because of a space.", "error"))

tbl_bigt_locid <- make_kable(bigt_locid, "Tbl_BigTrees.LocationIDs that don't have a match in tbl_Locations.LocationID, likely because of a space.")

# Check for negative DBH values
bigt_neg <- tbl_BigTrees_curr |> filter(DBH < 0)

QC_table <- rbind(QC_table,
                  QC_check(bigt_neg, "Big Trees", "Negative DBH values, likely indicating a flag.", "error"))

tbl_bigt_neg <- make_kable(bigt_neg, "Negative DBH values, likely indicating a flag.")

# Find DBH > 99% of ever recorded
if(all_years == TRUE){
dbh_99a <- tbl_BigTrees_curr |>
  filter(DBH > 0) |>
  mutate(DBH_99pct = quantile(DBH, probs = 0.99, na.rm = T)) |>
  filter(DBH > DBH_99pct)

dbh_99 <- dbh_99a |> arrange(DBH, LocationID, ModNo, Scientific_Name) |>
  left_join(locv |> select(LocationID, FeatureID), by = "LocationID") |>
  select(ID, EventID, LocationID, ModNo, Scientific_Name, DBH, DBH_99pct)

QC_table <- rbind(QC_table,
                  QC_check(dbh_99, "Big Trees", "DBH measurements that are >99% of all that have been recorded.", "check"))

tbl_dbh_99 <- make_kable(dbh_99, "DBH measurements that are >99% of all that have been recorded. Not necessarily an error, but good to check")
} else {

dbh99 <- quantile(tbl_BigTrees_hist$DBH, probs = 0.99, na.rm = T)

dbh_99 <- tbl_BigTrees_curr |> filter(DBH > dbh99) |>
  left_join(locv |> select(LocationID, FeatureID), by = "LocationID") |>
  select(LocationID, FeatureID, EventID, year, ModNo, Species = Scientific_Name, DBH)

QC_table <- rbind(QC_table,
                  QC_check(dbh_99, "Big Trees", paste0("DBH measurements in ", year_curr, " that are >99% of previously recorded DBHs."), "check"))

tbl_dbh_99 <- make_kable(dbh_99, paste0("DBH measurements in ", year_curr, " that are >99% of previously recorded DBHs."))

}

# Find species recorded that are not on tlu_WetlndSpecies_List
bigt_miss <- anti_join(tbl_BigTrees_curr, tluspp, by = c("Scientific_Name" = "SCIENTIFIC_NAME")) |>
  arrange(Scientific_Name, LocationID, EventID, ModNo) |> select(ID, EventID, LocationID, ModNo, Scientific_Name, DBH)

QC_table <- rbind(QC_table,
                  QC_check(bigt_miss, "Big Trees", paste0("Species in tbl_BigTrees that are not in the ", spp_list, " species list."), "error"))

tbl_bigt_miss <- make_kable(bigt_miss, paste0("Species in tbl_BigTrees that are not in the ", spp_list, " species list."))

# Find species that have only been recorded once
if(all_years == TRUE){
bigt_once1 <- tbl_BigTrees_curr |> group_by(Scientific_Name) |>
  summarize(num_recs = sum(!is.na(DBH))) |> filter(num_recs == 1) |> select(Scientific_Name)

bigt_once <- tbl_BigTrees |> filter(Scientific_Name %in% bigt_once1$Scientific_Name) |> arrange(Scientific_Name, LocationID, EventID)

QC_table <- rbind(QC_table,
                  QC_check(bigt_once, "Big Trees", "Species that have only been recorded once in tbl_BigTrees.", "check"))

tbl_bigt_once <- make_kable(bigt_once, "Species that have only been recorded once in tbl_BigTrees. Not necessarily an error, but good to check.")
} else {
  # bigtrees spp new to park
  bigt_hist_list <- sort(unique(tbl_BigTrees_hist$Scientific_Name))
  bigt_new1 <- tbl_BigTrees_curr |> filter(!Scientific_Name %in% bigt_hist_list)
  bigt_new <- bigt_new1 |>
    left_join(locv |> select(LocationID, FeatureID), by = "LocationID") |>
    group_by(LocationID, FeatureID, EventID, year, Species = Scientific_Name, ModNo) |>
    summarize(Count = sum(!is.na(DBH), na.rm = T), .groups = 'drop') |>
    arrange(ModNo) |>
    pivot_wider(names_from = ModNo, values_from = Count, names_prefix = "ModNo_") |>
    arrange(FeatureID, Species)

  QC_table <- rbind(QC_table,
                    QC_check(bigt_new, "VIBI Woody", paste0("Species recorded for first time in ", year_curr, ". Numbers under Modules are total stem counts."), "check"))

  tbl_bigt_once <- make_kable(bigt_new, paste0("Species recorded for first time in ", year_curr, "."))
}

# check if Big Tree checks returned at least 1 record to determine whether to include that tab in report
bigt_check <- QC_table |> filter(Data %in% "Big Trees" & Num_Records > 0)
bigt_include <- tab_include(bigt_check)

#----- Species list -----
# check that OH_STATUS == "adventive" and SHADE = "advent" match
# adv_check <- tlu_WetlndSpeciesList |> filter(OH_STATUS == "adventive" & !SHADE == "advent")
#
# QC_table <- rbind(QC_table,
#                   QC_check(adv_check, "tlu_Wetlnd_SpeciesList",
#                            "Species with OH_STATUS = 'adventive', but SHADE != 'advent'. Not sure if this is a worthwhile check or not.", "check"))
#
# tbl_adv_check <- make_kable(adv_check, "Species with OH_STATUS = 'adventive', but SHADE != 'advent'. Not sure if this is a worthwhile check or not.")
#
# Find species recorded in Herb and Woody tables that aren't on FQAI list- will continue to use this
# to find species added during sampling.
herb_spp <- tbl_VIBI_Herb_curr |> select(LocationID, EventID, ModuleNo = ModNo, ScientificName = Species) |>
  mutate(Samp_Module = "Herbs", pres = "X")
woody_spp <- tbl_VIBI_Woody_curr |> select(LocationID, EventID, ModuleNo = Module_No, ScientificName = Scientific_Name) |>
  mutate(Samp_Module = "Woody", pres = "X")
bigt_spp <- tbl_BigTrees_curr |> select(LocationID, EventID, ModuleNo = ModNo, ScientificName = Scientific_Name) |>
  mutate(Samp_Module = "BigTrees", pres = "X")

all_spp <- rbind(herb_spp, woody_spp, bigt_spp) |> unique() |>
  pivot_wider(names_from = Samp_Module, values_from = pres) |>
  arrange(ScientificName)

miss_spp <- anti_join(all_spp, tluspp, by = c("ScientificName" = "SCIENTIFIC_NAME"))

if(nrow(miss_spp) > 0){
  assign("unmatched_tluWSL_data", miss_spp, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(miss_spp, "Species List",
                           paste0("Species in Herb, Woody, or Big Tree tables that aren't listed in tlu_WetlndSpeciesList.",
                                  "Out data frame is saved as 'unmatched_tluWSL_data' in global environment for easier review.
                                  If the ScientificName is blank, it means there are blanks in the tables that can be deleted."), "error"))

tbl_miss_spp <- make_kable(miss_spp, paste0("Species in Herb, Woody, or Big Tree tables that aren't listed in tlu_WetlndSpeciesList.",
                                            "Out data frame is saved as 'unmatched_tluWSL_data' in global environment for easier review.
                                            If the ScientificName is blank, it means there are blanks in the tables that can be deleted."))

# Find species on tlu_WetlndSpecies_list that aren't on FQAI list
miss_spp2 <- anti_join(tlu_WetlndSpeciesList, tluspp, by = "SCIENTIFIC_NAME") |>
  select(SCIENTIFIC_NAME, ACRONYM, COMMON_NAME)

if(nrow(miss_spp2) > 0){
  assign("unmatched_tluWSL_FQAI", miss_spp, envir = .GlobalEnv)
}

QC_table <- rbind(QC_table,
                  QC_check(miss_spp2, "Species List",
                           paste0("Species in tlu_WetlndSpeciesList that are not on the FQAI list. This is a second check to make sure they're not misspelled. ",
                                  "Out data frame is saved as 'unmatched_tluWSL_FQAI' in global environment for easier review."), "error"))

tbl_miss_spp2 <- make_kable(miss_spp2, paste0("Species in tlu_WetlndSpeciesList that are not on the FQAI list. This is a second check to make sure they're not misspelled. ",
                                          "Out data frame is saved as 'unmatched_tlu_WetlndSpeciesList' in global environment for easier review."))


# check if species list checks returned at least 1 record to determine whether to include that tab in report
spp_check <- QC_table |> filter(Data %in% "Species List" & Num_Records > 0)
spp_include <- tab_include(spp_check)

#+++++ Compile final QC Table
QC_check_table <-  kable(QC_table, format = 'html', align = 'c', caption = "QC checking results",
                         col.names = c("Data Tab", "Check Description", "Number of Records", "Check Type")) |>
  kable_styling(fixed_thead = TRUE, bootstrap_options = c('condensed'),
                full_width = TRUE, position = 'left', font_size = 12) |>
  row_spec(0, extra_css = "border-top: 1px solid #000000; border-bottom: 1px solid #000000;") |>
  column_spec(3, width = "150px") |>
  column_spec(2:ncol(QC_table), background =
                ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "error", "#F2F2A0",
                       ifelse(QC_table$Num_Records > 0 & QC_table$check_type == "check", "#b7d8ef", "#ffffff"))) |>
  collapse_rows(1, valign = 'top') |>
  row_spec(nrow(QC_table), extra_css = 'border-bottom: 1px solid #000000;')

