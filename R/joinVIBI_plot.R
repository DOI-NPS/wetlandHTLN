#' @title joinVIBI_plot: join plot-level VIBI metrics
#'
#' @include getPlots.R
#' @include getHerbs.R
#' @include getWoody.R
#' @include getBiomass.R
#'
#' @importFrom dplyr arrange between case_when filter first full_join group_by left_join mutate select summarize
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#'
#' @description This function summarizes plot-level VIBI and filters by plot, year, and plot types.
#' NOTE 1: THIS FUNCTION ONLY WORKS WHERE 4 INTENSIVE MODULES ARE SAMPLED. This function is intended to provide
#' results that are comparable with other datasets that use the VIBI and are based on 4 intensive modules.
#' For module-level VIBI scores, use joinVIBI_module.
#'
#' NOTE 2: The Biomass metric assumes the area sampled is always 0.1m2. Currently, if there's only a biomass
#' record for a given year and no herb or woody data, those records are dropped.
#'
#' NOTE 3: Metric calculations and thresholds follow INTEGRATED WETLAND ASSESSMENT PROGRAM Part 9: Field Manual
#' for the Vegetation Index of Biotic Integrity for Wetlands v. 1.5. Pages 17 - 20 and Table 2 were most useful.
#'
#' @param years Numeric. Filter on years of survey. Default is all.
#'
#' @param survey_type Filter on survey type of plots. Options are "all" (default), "reference",
#' "survey", "survey, womc", "womc", and "womc, reference". Can choose multiple options.
#'
#' @param hgm_class Filter on HGM class. Options are "all" (default), "Depression",
#' "Impoundment", "Riverine", "Slope". Can choose multiple options.
#'
#' @param dom_veg1 Filter on level 1 dominant vegetation type. Options are "all" (default), "emergent",
#' "forest", "shrub". Can choose multiple options. Note that emergent-coastal was not included as an option
#' in this VIBI, but wouldn't take much to add as an option.
#'
#'
#' @param plotID Quoted string. Default is 'all'. If specified will return data for only plots specified.
#' Can choose multiple plots. Based on FeatureID in database.
#'
#' @param region Quoted string. Default is "NCNE". Specifies the Army Corps Region for OH. Options are "EMP", "MW", and "NCNE".
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#' # calculate plot-level VIBI for all sites (default).
#' vibi <- joinVIBI_plot()
#' write.csv(vibi, "./testing_scripts/vibiplot.csv", row.names=F)
#'
#' # calculate plot-level VIBI for plot 124 and year 2023
#' vibi124 <- joinVIBI_plot(plotID = "124", years = 2023)
#'
#' # calculate plot-level VIBI for all sites sampled in 2023
#' vibi23 <- joinVIBI_plot(years = 2023)
#'
#' # calculate plot-level VIBI for all emergent wetlands
#' vibi_emerg <- joinVIBI_plot(dom_veg1 = "emergent")
#'
#' # calculate plot-level VIBI for depressional wetlands
#' vibi_dep <- joinVIBI_plot(hgm_class = "Depression")
#'
#'
#' }
#'
#' @return Returns a data frame of VIBI calculations for each plot with 4 intensive modules
#' @export
#'

joinVIBI_plot <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                    survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                    plotID = 'all', region = "NCNE"){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)
  region <- match.arg(region, choices = c("NCNE", "EMP", "MW"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)

  #---- Compile Plot and visit-level data ----
  env <- if(exists("HTLN_wetlands")){HTLN_wetlands} else {.GlobalEnv}

  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, intens_mods = 4)

  tryCatch(tluSpecies <- get("tluSpecies", envir = env),
           error = function(e){stop("tlu_WetlndSpeciesList not found. Please run importData() first.")})

  # Compile the loc/visit table to left-join with final results
  plots_abbr <- plots[,c("LocationID", "FeatureTypes", "FeatureID", "Park", "County", "TotalMods", "IntensMods",
                         "PlotConfig", "AreaHA",
                         "X1oPlants", "X1oHGM", "DomVegID", "HGM_ID",
                         "DomVeg_Lev1", "DomVeg_Lev2", "DomVeg_Lev3")]

  #---- Compile Herb Metrics ----
  herbs1 <- getHerbs(years = years, survey_type = survey_type, hgm_class = hgm_class,
                     dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all', intens_mods = 4)

  if(nrow(herbs1) == 0){stop(
    paste0(
    "The combination of function arguments returned an empty data frame.",
    " Check that the combination of plotID and years have survey records, and that the plots you chose have 4 IntensMods.")
    )}

  # Set wet status based on the column chosen, like in the macros code. Note that the macros
  # code only replaces blanks in the ACOE regional columns for OBL, FACW, FACU, FAC, and UPL.
  # Codes not replaced are: (FAC), (FACU-), (FACU), (FACU+), (FACW), (UPL), FAC+, FACU-, FACW+
  # The spreadsheet then pattern matches FACW with * * wildcards to bring in () and +/-
  # in the calculation for statewide metric scores, but that won't work for blank regional
  # scores.
  herbs1$WETreg <- ifelse(is.na(herbs1[,region]), herbs1$WET, herbs1[,region])
  herbs1$WETreg <- ifelse(is.na(herbs1$WETreg), "ND", herbs1$WETreg)
  # dropping () and +/- indicators from WETreg that came in from WET to match spreadsheet calcs.
  herbs1$WETreg[grepl("\\(|\\)|\\-|\\+", herbs1$WETreg)] <- NA_character_
  #table(herbs1$WETreg, herbs1$NCNE, useNA = 'always')

  # Extract genus from scientific name
  herbs1$genus <- gsub("([A-Za-z]+).*", "\\1", herbs1$ScientificName)

  # Until this is fixed in the database, handling multiple dates/eventIDs for the same
  # sample period by taking the first.
  # find years with multiple eventIDs in a given site
  mult_evs <- herbs1 |> select(LocationID, FeatureID, SampleYear, EventID, SampleDate) |>  unique() |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(num_evs = sum(!is.na(EventID)),
              EventID = first(EventID),
              SampleDate = first(SampleDate),
              .groups = 'drop') |>
    filter(num_evs > 1)

  if(nrow(mult_evs) > 0){
    warning(paste0("There are ", nrow(mult_evs), " sites with multiple dates and EventIDs within a year for VIBI Herbs. ",
                   "Using the first chronological date and EventID. Affected FeatureIDs/EventIDs: ",
                   "\n",
                   paste0("LocationID: ", mult_evs[, 1], " ",
                          "FeatureID: ", mult_evs[, 2], " ",
                          "SampleYear: ", mult_evs[, 3],
                          collapse = "\n ")))

    for(i in seq_along(1:nrow(mult_evs))){
    row = mult_evs[i,]
    herbs1$EventID[herbs1$LocationID == row$LocationID &
                     herbs1$FeatureID == row$FeatureID &
                       herbs1$SampleYear == row$SampleYear] <- row$EventID

    herbs1$SampleDate[herbs1$LocationID == row$LocationID &
                       herbs1$FeatureID == row$FeatureID &
                         herbs1$SampleYear == row$SampleYear] <- row$SampleDate

    }
  }

  # Calc Relative Cover
  herbs_rc <- herbs1 |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(tot_cov = (sum(MidPoint, na.rm = T)), .groups = 'drop') # Divide by 4 at the end for avg plot cover


  # join back with larger dataset
  herbs <- left_join(herbs1, herbs_rc,
                     by = c("LocationID", "FeatureID", "EventID", "SampleDate",
                            "SampleYear", "DomVeg_Lev1")) |>
    filter(!is.na(MidPoint)) |>  # dropping FeatureID 1007 from 2023 for Mod NA with blank Species and cover.
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, DomVeg_Lev2,
             genus, FAMILY, OH_STATUS, LIFEFORM, SHADE, FORM, WET, WETreg, COFC, HABIT,
             ScientificName) |>
    summarize(spp_cov = sum(MidPoint),
              rel_cov = spp_cov/first(tot_cov),
              tot_cov = first(tot_cov),
              cov_wt_c = rel_cov*first(COFC),
              .groups = "drop") |>
    mutate(# wetness index
      wet_wt = case_when(WET == "OBL" ~ 1,
                         WET %in% "FACW" ~ 0.667,
                         WET == "FAC" ~ 0.5,
                         WET == "FACU" ~ 0.333,
                         WET == "UPL" ~ 0,
                         TRUE ~ 0),
      wet_wt_reg = case_when(WETreg == "OBL" ~ 1,
                             WETreg == "FACW" ~ 0.667,
                             WETreg == "FAC" ~ 0.5,
                             WETreg == "FACU" ~ 0.333,
                             WETreg == "UPL" ~ 0,
                             TRUE ~ 0),
      wet_rel_cov = rel_cov * wet_wt,
      wet_rel_cov_reg = rel_cov * wet_wt_reg) |>
    data.frame()

  # Create list of wet indicators from tluSpecies
  wet <- unique(tluSpecies$WET[grepl("OBL|FACW", tluSpecies$WET)])

  # Create table to left_join with herb vibi metrics; There are no sample qualifiers for sampled, but non present
  # So assuming if there's a record in this df below, and the community matches, the VIBI should be 0 for herb vibis
  herbs_lj <- herbs |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear) |> unique()

  # herbs_check <- herbs |> group_by(LocationID, FeatureID, EventID, SampleYear) |>
  #   summarize(sum_rel = sum(rel_cov))
  # table(herbs_check$sum_rel, useNA = 'always') # all sum to 1

  # Using macros cases to determine the exact ranges of VIBI scores (ie whether <= or < vs >= or >)
  # Carex Community E, SH
  carex1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, genus, ScientificName) |>
    filter(genus == "Carex") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, genus) |>
    summarize(Num_Carex = sum(!is.na(genus)), .groups = 'drop') |>
    mutate(Carex_Score = case_when(is.na(Num_Carex) ~ NA_real_,
                                   Num_Carex <= 1 ~ 0,
                                   between(Num_Carex, 2, 3) ~ 3,
                                   Num_Carex == 4 ~ 7,
                                   Num_Carex >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no Carex were found and remove score for forest
  carex <- left_join(herbs_lj, carex1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  carex$Num_Carex[is.na(carex$Num_Carex)] <- 0
  carex$Carex_Score[carex$Num_Carex == 0] <- 0
  carex$Carex_Score[carex$DomVeg_Lev1 == "forest"] <- NA

  # cyperaceae Community Ecoastal only
  cyper1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
           DomVeg_Lev1, FAMILY, ScientificName) |>
    #filter(DomVeg_Lev1 == "emergent-coastal") |>
    filter(FAMILY == "Cyperaceae") |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, FAMILY) |>
    summarize(Num_Cyper = sum(!is.na(FAMILY)), .groups = 'drop') |>
    mutate(Cyper_Score = case_when(is.na(Num_Cyper) ~ NA_real_,
                                   Num_Cyper <= 1 ~ 0,
                                   between(Num_Cyper, 2, 3) ~ 3,
                                   between(Num_Cyper, 4, 6) ~ 7,
                                   Num_Cyper >= 7 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no Cyperaceae were found and remove score for other communities
  cyper <- left_join(herbs_lj, cyper1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  cyper$Num_Cyper[is.na(cyper$Num_Cyper)] <- 0
  cyper$Cyper_Score[cyper$Num_Cyper == 0] <- 0
  cyper$Cyper_Score[!cyper$DomVeg_Lev1 %in% "emergent-coastal"] <- NA


  # dicot Community E SH
  dicot1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           OH_STATUS, LIFEFORM, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & LIFEFORM == "DI") |> unique() |> # native dicots
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Num_Dicot = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Dicot_Score = case_when(is.na(Num_Dicot) ~ NA_real_,
                                   DomVeg_Lev1 == "emergent" & Num_Dicot <= 10 ~ 0,
                                   DomVeg_Lev1 == "emergent" & between(Num_Dicot, 11, 17) ~ 3,
                                   DomVeg_Lev1 == "emergent" & between(Num_Dicot, 18, 24) ~ 7,
                                   DomVeg_Lev1 == "emergent" & Num_Dicot >= 25 ~ 10,

                                   DomVeg_Lev1 == "shrub" & Num_Dicot <= 9 ~ 0,
                                   DomVeg_Lev1 == "shrub" & between(Num_Dicot, 10, 14) ~ 3,
                                   DomVeg_Lev1 == "shrub" & between(Num_Dicot, 15, 23) ~ 7,
                                   DomVeg_Lev1 == "shrub" & Num_Dicot >= 24 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no dicots were found and drop scores for forest
  dicot <- left_join(herbs_lj, dicot1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  dicot$Num_Dicot[is.na(dicot$Num_Dicot)] <- 0
  dicot$Dicot_Score[dicot$Num_Dicot == 0] <- 0
  dicot$Dicot_Score[dicot$DomVeg_Lev1 == "forest"] <- NA

  # Shade community F
  shade1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           OH_STATUS, SHADE, ScientificName) |>
    unique() |>
    #filter(DomVeg_Lev1 == "forest") |>
    filter(#OH_STATUS == "native" & # spreadsheet doesn't
           SHADE %in% c("shade", "partial")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Num_Shade = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Shade_Score = case_when(is.na(Num_Shade) ~ NA_real_,
                                   Num_Shade <= 7 ~ 0,
                                   between(Num_Shade, 8, 13) ~ 3,
                                   between(Num_Shade, 14, 20) ~ 7,
                                   Num_Shade >= 21 ~ 10,
                                   TRUE ~ NA_real_))


  # Add 0s where no shade spp were found in forest and drop scores for non-forest
  shade <- left_join(herbs_lj, shade1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  shade$Num_Shade[is.na(shade$Num_Shade)] <- 0
  shade$Shade_Score[shade$Num_Shade == 0] <- 0
  shade$Shade_Score[!shade$DomVeg_Lev1 %in% "forest"] <- NA

  # Shrub- region Community E, SH
  shrub_reg1 <- herbs |># filter(FeatureID == "242VK1" & SampleYear == 2023) |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           OH_STATUS, FORM, WETreg, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WETreg %in% wet) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Num_Shrub_reg = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score_reg = case_when(is.na(Num_Shrub_reg) ~ NA_real_,
                                       Num_Shrub_reg <= 1 ~ 0,
                                       Num_Shrub_reg == 2 ~ 3,
                                       between(Num_Shrub_reg, 3, 4) ~ 7,
                                       Num_Shrub_reg >= 5 ~ 10,
                                       TRUE ~ NA_real_))

  # Add 0s where no Shrub were found and drop scores from forest
  shrub_reg <- left_join(herbs_lj, shrub_reg1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  shrub_reg$Num_Shrub_reg[is.na(shrub_reg$Num_Shrub_reg)] <- 0
  shrub_reg$Shrub_Score_reg[shrub_reg$Num_Shrub_reg == 0] <- 0
  shrub_reg$Shrub_Score_reg[shrub_reg$DomVeg_Lev1 == "forest"] <- NA

  # Shrub- statewide
  shrub1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           OH_STATUS, FORM, WET, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & FORM == "shrub" & WET %in% wet) |>
    #select(-OH_STATUS, -WET, -FORM) |>
    unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Num_Shrub = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(Shrub_Score = case_when(is.na(Num_Shrub) ~ NA_real_,
                                   Num_Shrub <= 1 ~ 0,
                                   Num_Shrub == 2 ~ 3,
                                   between(Num_Shrub, 3, 4) ~ 7,
                                   Num_Shrub >= 5 ~ 10,
                                   TRUE ~ NA_real_))

  # Add 0s where no Shrub were found and drop scores from forest
  shrub <- left_join(herbs_lj, shrub1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  shrub$Num_Shrub[is.na(shrub$Num_Shrub)] <- 0
  shrub$Shrub_Score[shrub$Num_Shrub == 0] <- 0
  shrub$Shrub_Score[shrub$DomVeg_Lev1 == "forest"] <- NA

  # Hydrophyte richness- region # native FACW and OBL
  hydrop_reg1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           OH_STATUS, WETreg, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & WETreg %in% wet) |>
    #select(-OH_STATUS, -WETreg) |>
    unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Num_Hydro_reg = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Hydro_Score_reg = case_when(is.na(Num_Hydro_reg) ~ NA_real_,
                                       DomVeg_Lev1 == "emergent" & Num_Hydro_reg <= 10 ~ 0,
                                       DomVeg_Lev1 == "emergent" & between(Num_Hydro_reg, 11, 20) ~ 3,
                                       DomVeg_Lev1 == "emergent" & between(Num_Hydro_reg, 21, 30) ~ 7,
                                       DomVeg_Lev1 == "emergent" & Num_Hydro_reg >= 31 ~ 10,

                                       DomVeg_Lev1 == "shrub" & Num_Hydro_reg <= 9 ~ 0,
                                       DomVeg_Lev1 == "shrub" & between(Num_Hydro_reg, 10, 14) ~ 3,
                                       DomVeg_Lev1 == "shrub" & between(Num_Hydro_reg, 15, 20) ~ 7,
                                       DomVeg_Lev1 == "shrub" & Num_Hydro_reg >= 21 ~ 10,

                                       TRUE ~ NA_real_))

  # Add 0s where no hydrophytes were found and drop scores from forest
  hydrop_reg <- left_join(herbs_lj, hydrop_reg1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  hydrop_reg$Num_Hydro_reg[is.na(hydrop_reg$Num_Hydro_reg)] <- 0
  hydrop_reg$Hydro_Score_reg[hydrop_reg$Num_Hydro_reg == 0] <- 0
  hydrop_reg$Hydro_Score_reg[hydrop_reg$DomVeg_Lev1 == "forest"] <- NA

  # hydro - statewide
  hydrop1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           OH_STATUS, WET, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent", "shrub")) |>
    filter(OH_STATUS == "native" & WET %in% wet) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Num_Hydro = sum(!is.na(ScientificName)), .groups = 'drop') |>
    mutate(Hydro_Score = case_when(is.na(Num_Hydro) ~ NA_real_,
                                         DomVeg_Lev1 == "emergent" & Num_Hydro <= 10 ~ 0,
                                         DomVeg_Lev1 == "emergent" & between(Num_Hydro, 11, 20) ~ 3,
                                         DomVeg_Lev1 == "emergent" & between(Num_Hydro, 21, 30) ~ 7,
                                         DomVeg_Lev1 == "emergent" & Num_Hydro >= 31 ~ 10,

                                         DomVeg_Lev1 == "shrub" & Num_Hydro <= 9 ~ 0,
                                         DomVeg_Lev1 == "shrub" & between(Num_Hydro, 10, 14) ~ 3,
                                         DomVeg_Lev1 == "shrub" & between(Num_Hydro, 15, 20) ~ 7,
                                         DomVeg_Lev1 == "shrub" & Num_Hydro >= 21 ~ 10,

                                         TRUE ~ NA_real_))

  # Add 0s where no hydrophytes were found in emergent or shrub wetlands
  hydrop <- left_join(herbs_lj, hydrop1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  hydrop$Num_Hydro[is.na(hydrop$Num_Hydro)] <- 0
  hydrop$Hydro_Score[hydrop$Num_Hydro == 0] <- 0
  hydrop$Hydro_Score[hydrop$DomVeg_Lev1 == "forest"] <- NA

  # annual : perennial ratio - did not explicitly state native, so not including OH_STATUS in summary
  ap_ratio1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, HABIT, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("emergent")) |>
    filter(HABIT %in% c("AN", "PE")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, HABIT, DomVeg_Lev1) |>
    summarize(Num_Spp = sum(!is.na(ScientificName)), .groups = 'drop') |>
    pivot_wider(names_from = HABIT, values_from = Num_Spp, values_fill = 0) |>
    mutate(AP_Ratio = ifelse(PE > 0, AN/PE, 1),
           AP_Score = case_when(is.na(AP_Ratio) ~ NA_real_,
                                AP_Ratio > 0.48 ~ 0,
                                between(AP_Ratio, 0.32, 0.48) ~ 3,
                                between(AP_Ratio, 0.20, 0.32) ~ 7,
                                AP_Ratio <= 0.20 ~ 10,
                                TRUE ~ NA_real_
                                ))

  # Add 0s where no ann/per were found and drop scores from non-emergent
  ap_ratio <- left_join(herbs_lj, ap_ratio1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  ap_ratio$AP_Ratio[is.na(ap_ratio$AP_Ratio)] <- 0
  ap_ratio$AP_Score[ap_ratio$AP_Ratio == 0] <- 0
  ap_ratio$AP_Score[!ap_ratio$DomVeg_Lev1 %in% "emergent"] <- NA

  # Seedless Vascular Plant metric (# ferns and fern allies)
  svp1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, FORM, ScientificName) |>
    #filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(FORM %in% c("fern")) |> unique() |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, FORM, DomVeg_Lev1) |>
    summarize(Num_SVP = sum(!is.na(ScientificName)), .groups= "drop") |>
    mutate(SVP_Score = case_when(is.na(Num_SVP) ~ NA_real_,
                                 Num_SVP == 0 ~ 0,
                                 Num_SVP == 1 ~ 3,
                                 Num_SVP == 2 ~ 7,
                                 Num_SVP >= 3 ~ 10,
                                 TRUE ~ NA_real_))

  # Add 0s where no sporophytes were found and drop emergent scores
  svp <- left_join(herbs_lj, svp1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  svp$Num_SVP[is.na(svp$Num_SVP)] <- 0
  svp$SVP_Score[svp$Num_SVP == 0] <- 0
  svp$SVP_Score[svp$DomVeg_Lev1 == "emergent"] <- NA

  # FQAI Equation 7, page 7 of
    # https://dam.assets.ohio.gov/image/upload/epa.ohio.gov/Portals/35/wetlands/Ohio_FQAI.pdf
  FQAI <- herbs |>
    filter(!is.na(COFC)) |> # drop species without Coefs
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, ScientificName, COFC) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |> unique() |>
    summarize(CC = sum(COFC),
              NumSpp = sum(!is.na(ScientificName)),
              FQAI = CC/sqrt(NumSpp),
              .groups = 'drop') |>
    mutate(FQAI_Score = case_when(is.na(FQAI) ~ NA_real_,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI <= 9.9 ~ 0,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI > 9.9 & FQAI <= 14.3 ~ 3,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI > 14.3 & FQAI <= 21.4 ~ 7,
                                  DomVeg_Lev1 %in% c("emergent", "shrub") & FQAI > 21.4 ~ 10,

                                  DomVeg_Lev1 %in% c("forest") & FQAI <= 14.0 ~ 0,
                                  DomVeg_Lev1 %in% c("forest") & FQAI > 14.0 & FQAI <= 19.0 ~ 3,
                                  DomVeg_Lev1 %in% c("forest") & FQAI > 19.0 & FQAI <= 24.0 ~ 7,
                                  DomVeg_Lev1 %in% c("forest") & FQAI > 24.0 ~ 10,
                                  TRUE ~ NA_real_),

           FQAI_Score_FQ = case_when(is.na(FQAI) ~ NA_real_,
                                     FQAI <= 10 ~ 0,
                                     FQAI >= 10 & FQAI < 30 ~ ((FQAI - 10)/20) * 50,
                                     FQAI >= 30 ~ 50,
                                     TRUE ~ NA_real_))
  CovWt_CofC <- herbs |>
    mutate(cov_wt_C = rel_cov * COFC) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov) |>
    summarize(cov_wt_C = sum(cov_wt_C, na.rm = T), .groups = 'drop') |> # avg to match spreadsheet
    mutate(#Cov_Wt_C_Score = case_when(is.na(cov_wt_C) ~ NA_real_, # I don't think this is actually scored outside of FQ
           #                          cov_wt_C == 0 ~ 0,
           #                          cov_wt_C > 0 & cov_wt_C <= 6 ~ 3,
           #                          cov_wt_C > 6 ~ 10),
           Cov_Wt_C_Score_FQ = case_when(tot_cov < 0.75 & DomVeg_Lev1 == "forest" ~
                                            (((tot_cov/0.75) * cov_wt_C)/6) * 50, #from spreadsheet
                                         is.na(cov_wt_C) ~ NA_real_,
                                         cov_wt_C == 0 ~ 0,
                                         cov_wt_C > 0 & cov_wt_C <= 6 ~ (cov_wt_C/6) * 50,
                                         cov_wt_C > 6 ~ 50))

  # % Bryophyte using rel_cov
  pct_bryo1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, ScientificName, FORM, rel_cov) |>
    #filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    filter(FORM == "bryo") |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(Pct_Bryo = (sum(rel_cov, na.rm = T)), .groups = 'drop') |> # to match spreadsheet.
    mutate(Pct_Bryo_Score = case_when(is.na(Pct_Bryo) ~ NA_real_,
                                     Pct_Bryo <= 0.01 ~ 0,
                                     Pct_Bryo > 0.01 & Pct_Bryo <= 0.03 ~ 3,
                                     Pct_Bryo > 0.03 & Pct_Bryo <= 0.06 ~ 7,
                                     Pct_Bryo > 0.06 ~ 10,
                                     TRUE ~ NA_real_))

  # Add 0s where no bryos were found and drop score from emergent
  pct_bryo <- left_join(herbs_lj, pct_bryo1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pct_bryo$Pct_Bryo[is.na(pct_bryo$Pct_Bryo)] <- 0
  pct_bryo$Pct_Bryo_Score[pct_bryo$Pct_Bryo == 0] <- 0
  pct_bryo$Pct_Bryo_Score[pct_bryo$DomVeg_Lev1 %in% "emergent"] <- NA

  # % Hydrophyte using rel_cov: OH_STATUS = native, SHADE = shade or partial, WET/WETreg = FACW (FACW) OBL
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  #wet <- unique(tluSpecies$WET[grepl("OBL|FACW", tluSpecies$WET)]) # not using this anymore, as wet_wt is used instead, where UPL = 0

  pct_hydro_reg1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, ScientificName,
           OH_STATUS, SHADE, WETreg, tot_cov, rel_cov) |>
    filter(OH_STATUS == "native" & WETreg %in% wet ) |> # &
    #      #SHADE %in% c("partial", "shade")) |> # VIBI spreadsheet doesn't filter on SHADE
    #filter(DomVeg_Lev1 %in% c("forest")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov, ScientificName) |>
    summarize(rel_cov = sum(rel_cov), .groups = 'drop') |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Hydro_reg = sum(rel_cov), .groups = 'drop') |>
    mutate(Pct_Hydro_Score_reg = case_when(is.na(Pct_Hydro_reg) ~ NA_real_,
                                          tot_cov < 0.10 ~ 0, # first case
                                          Pct_Hydro_reg <= 0.10 ~ 0,
                                          Pct_Hydro_reg > 0.10 & Pct_Hydro_reg <= 0.15 ~ 3,
                                          Pct_Hydro_reg > 0.15 & Pct_Hydro_reg <= 0.28 ~ 7,
                                          Pct_Hydro_reg > 0.28 ~ 10,
                                          TRUE ~ NA_real_))

  # Add 0s where no hydros were found and drop scores from non-forest
  pct_hydro_reg <- left_join(herbs_lj, pct_hydro_reg1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pct_hydro_reg$Pct_Hydro_reg[is.na(pct_hydro_reg$Pct_Hydro_reg)] <- 0
  pct_hydro_reg$Pct_Hydro_Score_reg[pct_hydro_reg$Pct_Hydro_reg == 0] <- 0
  pct_hydro_reg$Pct_Hydro_Score_reg[!pct_hydro_reg$DomVeg_Lev1 %in% "forest"] <- NA


  pct_hydro1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, ScientificName,
           OH_STATUS, SHADE, WET, tot_cov, rel_cov) |>
    filter(OH_STATUS == "native" & WET %in% wet) |> # &
    #      #SHADE %in% c("partial", "shade")) |> # VIBI spreadsheet doesn't filter on SHADE
    #filter(DomVeg_Lev1 %in% c("forest")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov, ScientificName) |>
    summarize(rel_cov = sum(rel_cov), .groups = 'drop') |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Hydro = sum(rel_cov), .groups = 'drop') |>
    mutate(Pct_Hydro_Score = case_when(is.na(Pct_Hydro) ~ NA_real_,
                                          tot_cov < 0.10 ~ 0, # first case
                                          Pct_Hydro <= 0.10 ~ 0,
                                          Pct_Hydro > 0.10 & Pct_Hydro <= 0.15 ~ 3,
                                          Pct_Hydro > 0.15 & Pct_Hydro <= 0.28 ~ 7,
                                          Pct_Hydro > 0.28 ~ 10,
                                          TRUE ~ NA_real_))


  # Add 0s where no hydros were found and drop scores from non-forest
  pct_hydro <- left_join(herbs_lj, pct_hydro1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pct_hydro$Pct_Hydro[is.na(pct_hydro$Pct_Hydro)] <- 0
  pct_hydro$Pct_Hydro_Score[pct_hydro$Pct_Hydro == 0] <- 0
  pct_hydro$Pct_Hydro_Score[!pct_hydro$DomVeg_Lev1 %in% "forest"] <- NA

  # % sensitive - rel cover of COFC >=6, for DomVeg_Lev1 = shrub, buttonbush is not included as %sensitive
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  pct_sens1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           ScientificName, COFC, tot_cov, rel_cov) |>
    filter(COFC >= 6) |>
    filter(!(DomVeg_Lev1 == "shrub" & ScientificName %in% "Cephalanthus occidentalis")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Sens = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_Sens_Score = case_when(is.na(Pct_Sens) ~ NA_real_,
                                     tot_cov < 0.10 ~ 0, # first case,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens <= 0.025 ~ 0,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens > 0.025 & Pct_Sens <= 0.10 ~ 3,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens > 0.10 & Pct_Sens <= 0.15 ~ 7,
                                     DomVeg_Lev1 %in% c("emergent") & Pct_Sens > 0.15 ~ 10,

                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens <= 0.02 ~ 0,
                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens > 0.02 & Pct_Sens <= 0.06 ~ 3,
                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens > 0.06 & Pct_Sens <= 0.13 ~ 7,
                                     DomVeg_Lev1 %in% c("shrub") & Pct_Sens > 0.13 ~ 10,

                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens <= 0.035 ~ 0,
                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens > 0.035 & Pct_Sens <= 0.12 ~ 3,
                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens > 0.12 & Pct_Sens <= 0.30 ~ 7,
                                     DomVeg_Lev1 %in% c("forest") & Pct_Sens > 0.30 ~ 10,
                                     TRUE ~ NA_real_))

  # Add 0s where no sens were found
  pct_sens <- left_join(herbs_lj, pct_sens1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pct_sens$Pct_Sens[is.na(pct_sens$Pct_Sens)] <- 0
  pct_sens$Pct_Sens_Score[pct_sens$Pct_Sens == 0] <- 0


  # % tolerant
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  pct_tol1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1,
           ScientificName, COFC, tot_cov, rel_cov) |>
    filter(COFC <= 2) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_cov) |>
    summarize(Pct_Tol = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_Tol_Score = case_when(is.na(Pct_Tol) ~ NA_real_,
                                    tot_cov < 0.10 ~ 0, # first case
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol >= 0.60 ~ 0,
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol >= 0.40  & Pct_Tol < 0.60 ~ 3,
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol >= 0.20 & Pct_Tol < 0.40 ~ 7,
                                    DomVeg_Lev1 %in% c("emergent") & Pct_Tol < 0.20 ~ 10,

                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol >= 0.45 ~ 0,
                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol >= 0.30  & Pct_Tol < 0.45 ~ 3,
                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol >= 0.15 & Pct_Tol < 0.30 ~ 7,
                                    DomVeg_Lev1 %in% c("forest") & Pct_Tol < 0.15 ~ 10,

                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol >= 0.15 ~ 0,
                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol >= 0.10  & Pct_Tol < 0.15 ~ 3,
                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol >= 0.05 & Pct_Tol < 0.10 ~ 7,
                                    DomVeg_Lev1 %in% c("shrub") & Pct_Tol < 0.05 ~ 10,
                                    TRUE ~ NA_real_))

  # Add 0s where no tols were found
  pct_tol <- left_join(herbs_lj, pct_tol1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pct_tol$Pct_Tol[is.na(pct_tol$Pct_Tol)] <- 0
  pct_tol$Pct_Tol_Score[pct_tol$Pct_Tol == 0] <- 0

  # Invasive graminoids: Phalaris arundinaceae, Typha spp. Phragmites australis
  # *if total cover(sum of cover values for all species observed in sample plot is <10%, all % metrics scored as 0)
  inv_grams <- c("Phalaris arundinacea", "Phragmites australis ssp. australis",
                 "Typha angustifolia", "Typha latifolia", "Typha x glauca",
                 "Typha minima", "Typha sp.")

  pct_invgram1 <- herbs |>
    select(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, DomVeg_Lev2,
           ScientificName, tot_cov, rel_cov) |>
    filter(ScientificName %in% inv_grams) |> #unique() |>
    # filter(DomVeg_Lev1 %in% c("emergent") |
    #          (DomVeg_Lev1 == "shrub" & DomVeg_Lev2 == "Bog Shrub Swamp")) |>
    # leatherleaf bog isn't in the CUVA data, but including it here because of pg 19 of
    # EPA manual states that this metric replaces the subcanopy IV metric for this community.
    # Based on the VIBI spreadsheet and the HTLN database, Bog Shrub Swamp seems to cover
    # that, although there's not a site like this in the data.
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, DomVeg_Lev2, tot_cov) |>
    summarize(Pct_InvGram = sum(rel_cov, na.rm = T), .groups = 'drop') |>
    mutate(Pct_InvGram_Score = case_when(is.na(Pct_InvGram) ~ NA_real_,
                                        tot_cov < 0.10 ~ 0, # first case
                                        Pct_InvGram >= 0.31 ~ 0,
                                        Pct_InvGram >= 0.15 & Pct_InvGram < 0.31 ~ 3,
                                        Pct_InvGram >= 0.03 & Pct_InvGram < 0.15 ~ 7,
                                        Pct_InvGram < 0.03 ~ 10,
                                        TRUE ~ NA_real_))

  # Add 0s where no invgrams were found and drop scores from non-emergent
  pct_invgram <- left_join(herbs_lj, pct_invgram1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pct_invgram$Pct_InvGram[is.na(pct_invgram$Pct_InvGram)] <- 0
  pct_invgram$Pct_InvGram_Score[pct_invgram$Pct_InvGram == 0] <- 0
  pct_invgram$Pct_InvGram_Score[pct_invgram$DomVeg_Lev1 == "forest"] <- NA
  pct_invgram$Pct_InvGram_Score[pct_invgram$DomVeg_Lev1 == "shrub" &
                                 (is.na(pct_invgram$DomVeg_Lev2)  |
                                  !pct_invgram$DomVeg_Lev2 == "Bog Shrub Swamp")] <- NA

  #---- Compile Woody and Big Tree metrics ----
  # For woody metrics to work, need to drop big tree records from tbl_VIBI_Woody and
  # then row bind the Big Tree DBH records
  woody1 <- getWoody(years = years, survey_type = survey_type, hgm_class = hgm_class,
                     dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all', intens_mod = 4)

  woody1$Count[woody1$Count == -9999] <- NA_real_
  woody1 <- woody1 |> mutate(ba_cm2 = (pi*(DBH_MidPt/2)^2)*Count)
  # Set wet status based on the column chosen, like in the macros code
  woody1$WETreg <- ifelse(is.na(woody1[,region]), woody1$WET, woody1[,region])
  woody1$WETreg <- ifelse(is.na(woody1$WETreg), "ND", woody1$WETreg)
  woody1$AreaHA <- as.numeric(woody1$AreaHA)
  woody2 <- woody1 |> filter(!DiamID %in% "BIG")

  bigt1 <- getBigTrees(years = years, survey_type = survey_type, hgm_class = hgm_class,
                       dom_veg1 = dom_veg1, plotID = plotID, nativity = 'all', intens_mod = 4) |>
    mutate(BIG_ba_cm2 = pi*(DBH_cm/2)^2)

  # Set wet status based on the column chosen, like in the macros code
  bigt1$WETreg <- ifelse(is.na(bigt1[,region]), bigt1$WET, bigt1[,region])
  bigt1$WETreg <- ifelse(is.na(bigt1$WETreg), "ND", bigt1$WETreg)
  bigt1$AreaHA <- as.numeric(bigt1$AreaHA)

  bigt <- bigt1 |> group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                            AreaHA, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS,
                            SHADE, FORM, WET, WETreg,
                            ScientificName) |>
    summarize(BIG_BA = sum(BIG_ba_cm2, na.rm = T),
              BIG_Count = sum(!is.na(DBH_cm)),
              DiamID = "BIG",
              DiamVal = ">= 40",
              .groups = 'drop')

  woody_comb <- rbind(woody2 |> select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                                       AreaHA, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS, SHADE, FORM,
                                       # WET, WETreg, # don't need b/c covered by first filter
                                       DiamID, DiamVal, ScientificName, Count, ba_cm2),
                      bigt |> select(LocationID, FeatureID, EventID, SampleDate, SampleYear,
                                     AreaHA, DomVeg_Lev1, DomVeg_Lev2, OH_STATUS, SHADE, FORM,
                                     # WET, WETreg, # don't need b/c covered by first filter
                                     DiamID, DiamVal, ScientificName, Count = BIG_Count,
                                     ba_cm2 = BIG_BA))

  # Calc rel. density
  woody_rc <- woody_comb |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(tot_stems = sum(Count, na.rm = T),
              tot_stems_per_ha = tot_stems/first(AreaHA),
              tot_ba_cm2 = sum(ba_cm2, na.rm = T),
              .groups = 'drop')

  woody <- left_join(woody_comb, woody_rc,
                     by = c("LocationID", "FeatureID", "EventID", "SampleDate",
                            "SampleYear", "DomVeg_Lev1")) |>
    filter(!is.na(Count)) # dropping FeatureID 305 from 2015 for Mod 3 Carya ovata C5 with -9999.
    # It's also a duplicate record, as there's a Carya ovata in the same size class with a count in Mod 3.

  # Until this is fixed in the database, handling multiple dates/eventIDs for the same
  # sample period by taking the first.
  # find years with multiple eventIDs in a given site
  mult_evs_w <- woody |> select(LocationID, FeatureID, SampleYear, EventID, SampleDate) |>  unique() |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(num_evs = sum(!is.na(EventID)),
              EventID = first(EventID),
              SampleDate = first(SampleDate),
              .groups = 'drop') |>
    filter(num_evs > 1)

  if(nrow(mult_evs_w)>0){
    warning(paste0("There are ", nrow(mult_evs_w), " sites with multiple dates and EventIDs within a year for VIBI Herbs. ",
                   "Using the first chronological date and EventID. Affected FeatureIDs/EventIDs: ",
                   "\n",
                   paste0("LocationID: ", mult_evs_w[, 1], " ",
                          "FeatureID: ", mult_evs_w[, 2], " ",
                          "SampleYear: ", mult_evs_w[, 3],
                          collapse = "\n ")))

    for(i in seq_along(1:nrow(mult_evs_w))){
      row = mult_evs_w[i,]
      woody$EventID[woody$LocationID == row$LocationID &
                      woody$FeatureID == row$FeatureID &
                       woody$SampleYear == row$SampleYear] <- row$EventID

      woody$SampleDate[woody$LocationID == row$LocationID &
                        woody$FeatureID == row$FeatureID &
                          woody$SampleYear == row$SampleYear] <- row$SampleDate

    }
  }

  # Create table to left_join with herb vibi metrics; There are no sample qualifiers for sampled, but non present
  # So assuming if there's a record in this df below, and the community matches, the VIBI should be 0 for woody vibis
  woody_lj <- woody |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear) |> unique()

  # woody_check <- woody |> group_by(FeatureID, EventID, SampleYear) |>
  #   summarize(rel_sum = sum(rel_stems))
  #
  # table(woody_check$rel_sum, useNA = 'always') # all sum to 1

  # Pole Timber rel dens
  #**If no or only a few woody stems >1m tall in sample plot or if stems per ha <10, score metric as 0.
  # Interpreting this as <= 3 stems >1m tall
  pole1 <- woody |> #filter(DomVeg_Lev1 == "forest") |>
    filter(DiamID %in% c("C5", "C6", "C7")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(RelDen_SmTree = sum(Count)/first(tot_stems),
              tot_stems_per_ha = first(tot_stems_per_ha),
              .groups = 'drop') |>
    mutate(SmTree_Score = case_when(is.na(RelDen_SmTree) ~ NA_real_,
                                    tot_stems_per_ha < 10 ~ 0, # ** Table 2
                                    RelDen_SmTree >= 0.32 ~ 0,
                                    RelDen_SmTree >= 0.22 & RelDen_SmTree < 0.32 ~ 3,
                                    RelDen_SmTree >= 0.11 & RelDen_SmTree < 0.22 ~ 7,
                                    RelDen_SmTree < 0.11 ~ 10,
                                    TRUE ~ NA_real_))

  # Add 0s where no pole trees were found and drop scores from forest
  pole <- left_join(woody_lj, pole1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  pole$RelDen_SmTree[is.na(pole$RelDen_SmTree)] <- 0
  pole$SmTree_Score[pole$RelDen_SmTree == 0] <- 0
  pole$SmTree_Score[!pole$DomVeg_Lev1 %in% "forest"] <- NA

  # Canopy and Subcanopy IV
  # Had to rbind all woody <40cm to Big Trees records to get correct DBH and BA for IV
  IV <- woody |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear,
             DomVeg_Lev1, DomVeg_Lev2, ScientificName, OH_STATUS, FORM, SHADE, DiamID) |>
    summarize(Count = sum(Count),
              BA = sum(ba_cm2),
              tot_ba_cm2 = first(tot_ba_cm2),
              tot_stems = first(tot_stems),
              tot_stems_per_ha = first(tot_stems_per_ha),
              .groups = 'drop') |>
    pivot_wider(values_from = c(Count, BA), names_from = DiamID, values_fill = 0,
                names_glue = "{DiamID}_{.value}") |>
    data.frame()

  # Add class columns potentially missing
  count_cols <- c("C0_Count", "C1_Count", "C2_Count", "C3_Count", "C4_Count",
                  "C5_Count", "C6_Count", "C7_Count", "C8_Count", "C9_Count",
                  "C10_Count", "BIG_Count")
  ba_cols <- c("C0_BA", "C1_BA", "C2_BA", "C3_BA", "C4_BA",
               "C5_BA", "C6_BA", "C7_BA", "C8_BA", "C9_BA",
               "C10_BA", "BIG_BA")

  miss_cols <- setdiff(c(count_cols, ba_cols), c(names(IV)))
  IV[miss_cols] <- 0

  # Calc rel class freq
  IV$rel_class_freq = rowSums(IV[,count_cols] > 0, na.rm = T) / length(count_cols) # 12; in case # classes changes
  IV$rel_ba = rowSums(IV[,ba_cols], na.rm = T)/IV$tot_ba_cm2
  IV$rel_dens = rowSums(IV[,count_cols], na.rm = T)/IV$tot_stems
  IV[,c("rel_class_freq", "rel_ba", "rel_dens")][is.na(IV[,c("rel_class_freq", "rel_ba", "rel_dens")])] <- 0
  IV$IV = (IV$rel_class_freq + IV$rel_ba + IV$rel_dens)/3

  #ivcheck <- IV |> filter(FeatureID == "242VK2" & SampleYear == 2023)

  #---- Subcanopy IV ----
  # for F and SH
  # Manual states that subcan IV is the sum IV of
  # 1. native shade tolerant subcanopy species (FORM shrub and sm tree), plus
  # 2. IV of native FAC subcanopy (shrub and sm tree) species.
  # However, in the spreadsheet there's no calc of the 2nd, likely assuming
  # it's covered by the first, because it's all the same except wetness
  # For leatherleaf bogs, substitute invasive graminoid metric
  # ** If no or only a few woody stems >1m tall in sample plot or if stems per ha <10, score metric as 0.

  subcan_IV1 <- IV |>
    #filter(DomVeg_Lev1 %in% c("forest", "shrub")) |>
    #filter(!(DomVeg_Lev2 %in% "Bog Shrub Swamp")) |>
    filter(OH_STATUS == "native") |>
    filter(SHADE %in% c("partial", "shade")) |>
    filter(FORM %in% c("shrub", "sm tree")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1, tot_stems_per_ha) |>
    summarize(SubcanIV_num = sum(IV),
              SubcanIV_den = sum(IV > 0), # sums a logical statement, so gives count
              SubcanIV = SubcanIV_num/SubcanIV_den,
              .groups = 'drop') |>
    mutate(SubcanIV_Score = case_when(tot_stems_per_ha < 10 ~ 0, #** Table 2
                                      DomVeg_Lev1 == "forest" & SubcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.02 & SubcanIV <= 0.072 ~ 3,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.072 & SubcanIV <= 0.13 ~ 7,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.13 ~ 10,

                                      DomVeg_Lev1 == "shrub" & SubcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.02 & SubcanIV <= 0.05 ~ 3,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.05 & SubcanIV <= 0.10 ~ 7,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.10 ~ 10,

                                      TRUE ~ NA_real_))

  # Add 0s where no subcanopy trees were found and drop scores from emergent
  subcan_IV <- left_join(woody_lj, subcan_IV1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  subcan_IV$SubcanIV[is.na(subcan_IV$SubcanIV)] <- 0
  subcan_IV$SubcanIV_Score[subcan_IV$SubcanIV == 0] <- 0
  subcan_IV$SubcanIV_Score[subcan_IV$DomVeg_Lev1 == "emergent"] <- NA
  subcan_IV$SubcanIV_Score[subcan_IV$DomVeg_Lev2 == "Bog Shrub Swamp"] <- NA


  #---- Canopy IV ----
  canopy_IV1 <- IV |>
    #filter(DomVeg_Lev1 %in% c("forest")) |>
    filter(OH_STATUS == "native") |>
    filter(FORM %in% c("tree")) |>
    group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, DomVeg_Lev1) |>
    summarize(CanopyIV_num = sum(IV),
              CanopyIV_den = sum(IV > 0), # sums a logical statement, so gives count
              CanopyIV = CanopyIV_num/CanopyIV_den,
              .groups = 'drop') |>
    mutate(CanopyIV_Score = case_when(CanopyIV >= 0.21 ~ 0,
                                      CanopyIV >= 0.17 & CanopyIV < 0.21 ~ 3,
                                      CanopyIV >= 0.14 & CanopyIV < 0.17 ~ 7,
                                      CanopyIV > 0 & CanopyIV < 0.14 ~ 10,
                                      CanopyIV == 0 ~ 0, # from *** in Table 2
                                      TRUE ~ NA_real_))

  # canIV_check <- IV |> filter(FeatureID == "242VK2" & SampleYear == 2023) |>
  #   filter(OH_STATUS == "native") |> filter(FORM %in% "tree") |>
  #   group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, ScientificName) |>
  #   summarize(CanopyIV = sum(IV)/(sum(IV > 0)))

  # Add 0s where no canopy trees were found and drop scores from non-forest
  canopy_IV <- left_join(woody_lj, canopy_IV1, by = c("LocationID", "FeatureID", "SampleYear", "DomVeg_Lev1"))
  canopy_IV$CanopyIV[is.na(canopy_IV$CanopyIV)] <- 0
  canopy_IV$CanopyIV_Score[canopy_IV$CanopyIV == 0] <- 0
  canopy_IV$CanopyIV_Score[!canopy_IV$DomVeg_Lev1 %in% "forest"] <- NA

  # % Unvegetated
  # **** This metric should be calculated for wetland mitigation sites where perennial
  # hydrophyte vegetation is not well established or where g/m2 of biomass is less than 100.
  # It can also be used as a biomass metric substitute for mitigation wetlands
  # or other emergent sites where biomass cannot be collected (Table 2)
  # It does not appear to be a metric collected by CUVA, so I won't include it. I'm guessing it's
  # because there are no mitigation wetlands with minimal plant establishment

  #---- Compile Biomass Metrics ----
  # Assumes area sampled is always 0.01m2 to get grams/m2
  bmass1 <- getBiomass(years = years, survey_type = survey_type, hgm_class = hgm_class,
                      dom_veg1 = dom_veg1, plotID = plotID, intens_mod = 4)

  bmass <- bmass1 |>
           filter(DomVeg_Lev1 == "emergent") |>
           mutate(weight_g_m2 = DryWt_g/0.1) |>
           group_by(LocationID, FeatureID, EventID, SampleDate, SampleYear, TotalMods, AreaHA, DomVeg_Lev1) |>
           summarize(num_bmass_samp = sum(!is.na(DryWt_g)),
                     Avg_Bmass = sum(weight_g_m2)/num_bmass_samp,
                     .groups = 'drop') |>
           mutate(Biomass_Score = case_when(is.na(Avg_Bmass) ~ NA_real_,
                                            Avg_Bmass > 800 ~ 0,
                                            Avg_Bmass >= 451 & Avg_Bmass <= 800 ~ 3,
                                            Avg_Bmass >= 201 & Avg_Bmass < 451 ~ 7,
                                            Avg_Bmass >= 100 & Avg_Bmass < 201 ~ 10,
                                            Avg_Bmass < 100 ~ 0,
                                            TRUE ~ NA_real_
                                            ))

  # Until this is fixed in the database, handling multiple dates/eventIDs for the same
  # sample period by taking the first.

  # find years with multiple eventIDs in a given site
  mult_evs_b <- bmass |> select(LocationID, FeatureID, SampleYear, EventID, SampleDate) |>  unique() |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(num_evs = sum(!is.na(EventID)),
              EventID = first(EventID),
              SampleDate = first(SampleDate),
              .groups = 'drop') |>
    filter(num_evs > 1)

  if(nrow(mult_evs_b) > 0){
    warning(paste0("There are ", nrow(mult_evs_b), " sites with multiple dates and EventIDs within a year for VIBI Herbs. ",
                   "Using the first chronological date and EventID. Affected FeatureIDs/EventIDs: ",
                   "\n",
                   paste0("LocationID: ", mult_evs_b[, 1], " ",
                          "FeatureID: ", mult_evs_b[, 2], " ",
                          "SampleYear: ", mult_evs_b[, 3],
                          collapse = "\n ")))

    for(i in seq_along(1:nrow(mult_evs_b))){
      row = mult_evs_b[i,]
      bmass$EventID[bmass$LocationID == row$LocationID &
                      bmass$FeatureID == row$FeatureID &
                        bmass$SampleYear == row$SampleYear] <- row$EventID

      bmass$SampleDate[bmass$LocationID == row$LocationID &
                        bmass$FeatureID == row$FeatureID &
                          bmass$SampleYear == row$SampleYear] <- row$SampleDate

    }
  }

  # Not adding 0s for Biomass, because not every module is sampled for biomass every year.
  # I don't have a way to logically changes 0s to NA

  #---- Combine metrics for VIBI score and final rating ----

  # Create table to left_join with vibi metrics; Dropped EventID and SampleDate because not identical withins sample period
  herbs_recs <- herbs |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, SampleDate, EventID) |> mutate(herb = 1) |> unique()
  woody_recs <- woody |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, SampleDate, EventID) |> mutate(woody = 1) |> unique()
  bmass_recs <- bmass |> select(LocationID, FeatureID, DomVeg_Lev1, SampleYear, SampleDate, EventID) |> mutate(bmass = 1) |> unique()

  full_evs1 <- purrr::reduce(list(herbs_recs, woody_recs, bmass_recs), full_join,
                            by = c("LocationID", "FeatureID", "DomVeg_Lev1", "SampleYear", "SampleDate", "EventID"))

  full_evs1$num_samps <- rowSums(full_evs1[,c("herb", "woody", "bmass")], na.rm = T)

  # Drop records that only have biomass sampled
  full_evs <- full_evs1 |> filter(!(bmass == 1 & num_samps == 1)) |> select(-herb, -woody, -bmass, -num_samps)

  # Removed EventID, SampleDate, from list, because not identical across herb, woody, biomass data
  vibi_list <- list(
    carex |> select(LocationID, FeatureID, SampleYear, Num_Carex, Carex_Score),
    cyper |> select(LocationID, FeatureID, SampleYear, Num_Cyper, Cyper_Score),
    dicot |> select(LocationID, FeatureID, SampleYear, Num_Dicot, Dicot_Score),
    shade |> select(LocationID, FeatureID, SampleYear, Num_Shade, Shade_Score),
    shrub_reg |> select(LocationID, FeatureID, SampleYear, Num_Shrub_reg, Shrub_Score_reg),
    shrub |> select(LocationID, FeatureID, SampleYear, Num_Shrub, Shrub_Score),
    hydrop_reg |> select(LocationID, FeatureID, SampleYear, Num_Hydro_reg, Hydro_Score_reg),
    hydrop |> select(LocationID, FeatureID, SampleYear, Num_Hydro, Hydro_Score),
    ap_ratio |> select(LocationID, FeatureID, SampleYear, AP_Ratio, AP_Score),
    svp |> select(LocationID, FeatureID, SampleYear, Num_SVP, SVP_Score),
    FQAI |> select(LocationID, FeatureID, SampleYear, NumSpp, FQAI, FQAI_Score, FQAI_Score_FQ),
    CovWt_CofC |> select(LocationID, FeatureID, SampleYear, tot_cov, cov_wt_C, Cov_Wt_C_Score_FQ),
    pct_bryo |> select(LocationID, FeatureID, SampleYear, Pct_Bryo, Pct_Bryo_Score),
    pct_hydro_reg |> select(LocationID, FeatureID, SampleYear, Pct_Hydro_reg, Pct_Hydro_Score_reg),
    pct_hydro |> select(LocationID, FeatureID, SampleYear, Pct_Hydro, Pct_Hydro_Score),
    pct_sens |> select(LocationID, FeatureID, SampleYear, Pct_Sens, Pct_Sens_Score),
    pct_tol |> select(LocationID, FeatureID, SampleYear, Pct_Tol, Pct_Tol_Score),
    pct_invgram |> select(LocationID, FeatureID, SampleYear, Pct_InvGram, Pct_InvGram_Score),
    pole |> select(LocationID, FeatureID, SampleYear, RelDen_SmTree, SmTree_Score),
    subcan_IV |> select(LocationID, FeatureID, SampleYear, SubcanIV, SubcanIV_Score),
    canopy_IV |> select(LocationID, FeatureID, SampleYear, CanopyIV, CanopyIV_Score),
    bmass |> select(LocationID, FeatureID, SampleYear, Avg_Bmass, Biomass_Score)
  )

  vibi_comb <- purrr::reduce(vibi_list, full_join,
                       by = c("LocationID", "FeatureID", "SampleYear")) |> data.frame()

  vibi_comb2 <- left_join(full_evs, vibi_comb, by = c("LocationID", "FeatureID", "SampleYear")) |>
    data.frame()

  vibi_comb2$Avg_Plot_Cover <- vibi_comb2$tot_cov / 4 # Only allowing sites with 4 intense modules

  vibi_score_cols <- c("Carex_Score", "Cyper_Score", "Dicot_Score", "Shade_Score",
                       "Shrub_Score", "Hydro_Score", "SVP_Score", "AP_Score", "FQAI_Score",
                       "Pct_Bryo_Score", "Pct_Hydro_Score", "Pct_Sens_Score", "Pct_Tol_Score",
                       "Pct_InvGram_Score", "SmTree_Score", "SubcanIV_Score",
                       "CanopyIV_Score", "Biomass_Score")

  vibi_score_reg_cols <- c("Carex_Score", "Cyper_Score", "Dicot_Score", "Shade_Score",
                          "Shrub_Score_reg", "Hydro_Score_reg", "SVP_Score", "AP_Score", "FQAI_Score",
                          "Pct_Bryo_Score", "Pct_Hydro_Score_reg", "Pct_Sens_Score", "Pct_Tol_Score",
                          "Pct_InvGram_Score", "SmTree_Score", "SubcanIV_Score",
                          "CanopyIV_Score", "Biomass_Score")

  vibi_fq_scores <- c("FQAI_Score_FQ", "Cov_Wt_C_Score_FQ")

  vibi_comb3 <- left_join(plots_abbr, vibi_comb2, by = c("LocationID", "FeatureID", "DomVeg_Lev1"))

  vibi_comb3$VIBI_Score_State <- rowSums(vibi_comb3[,vibi_score_cols], na.rm = T)
  vibi_comb3$VIBI_Score_ACOEReg <- rowSums(vibi_comb3[,vibi_score_reg_cols], na.rm = T)
  vibi_comb3$VIBI_Score_FQ <- rowSums(vibi_comb3[,vibi_fq_scores], na.rm = T)

  #---- Informational Parameters ----
  # tree stems per ha
  trees_per_ha <- woody |> filter(OH_STATUS == "native" & FORM == "tree") |> # spreadsheet didn't include sm tree
    group_by(LocationID, FeatureID, SampleYear, AreaHA) |>
    summarize(tree_stems = sum(Count),
              trees_per_ha = tree_stems / first(AreaHA),
              .groups = 'drop') |>
    select(-AreaHA)

  # shrub stems per ha
  shrubs_per_ha <- woody |> filter(OH_STATUS == "native" & FORM == "shrub") |> # spreadsheet didn't include sm tree
    group_by(LocationID, FeatureID, SampleYear, AreaHA) |>
    summarize(shrub_stems = sum(Count),
              shrubs_per_ha = shrub_stems / first(AreaHA),
              .groups = 'drop') |>
    select(-AreaHA)

  # % buttonbush
  pct_button <- herbs |> filter(ScientificName %in% "Cephalanthus occidentalis") |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(pct_button = sum(rel_cov, na.rm = T), .groups = 'drop')

  # % native perennial hydrophytes
  pct_nat_hydro <- herbs |> filter(OH_STATUS == "native" & WETreg %in% wet & HABIT %in% c("W", "PE")) |> # spreadsheet uses region wetland indicator
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(pct_nat_hydro = sum(rel_cov, na.rm = T),
              .groups = 'drop')

  # % native perennials
  pct_nat_peren <- herbs |> filter(OH_STATUS == "native" & HABIT %in% c("W", "PE")) |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(pct_nat_peren = sum(rel_cov, na.rm = T),
              .groups = 'drop')

  # % perennial
  pct_peren <- herbs |> filter(HABIT %in% c("W", "PE")) |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(pct_peren = sum(rel_cov, na.rm = T),
              .groups = 'drop')

  # % aventives
  pct_advent <- herbs |> filter(OH_STATUS %in% c("adventive", "cryptogenic")) |>
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(pct_advent = sum(rel_cov, na.rm = T),
              .groups = 'drop')

  # Wetness Index
  wet_index <- herbs |> # spreadsheet uses reg wetland indicator
    group_by(LocationID, FeatureID, SampleYear) |>
    summarize(wet_index = sum(wet_rel_cov, na.rm = T),
              .groups = 'drop')

 info_list <- list(trees_per_ha, shrubs_per_ha, pct_button, pct_nat_hydro, pct_nat_peren, pct_peren, pct_advent, wet_index)

 info_comb <- purrr::reduce(info_list, full_join, by = c("LocationID", "FeatureID", "SampleYear"))

 info_comb2 <- left_join(full_evs, info_comb, by = c("LocationID", "FeatureID", "SampleYear")) |>
   data.frame()

 info_cols <- c("trees_per_ha", "shrubs_per_ha", "pct_button", "pct_nat_hydro", "pct_nat_peren", "pct_peren",
                "pct_advent", "wet_index")

 info_comb2[,info_cols][is.na(info_comb2[,info_cols])] <- 0

 info_comb3 <- info_comb2[,c("LocationID", "FeatureID", "SampleYear", info_cols)]

 comb_dat <- full_join(vibi_comb3, info_comb3, by = c("LocationID", "FeatureID", "SampleYear")) |>
   arrange(FeatureID, SampleYear) |>
   filter(!is.na(SampleYear))

 vibi_cols <- c("Num_Carex", "Carex_Score", "Num_Cyper", "Cyper_Score", "Num_Dicot", "Dicot_Score",
                "Num_Shade", "Shade_Score", "Num_Shrub", "Shrub_Score", "Num_Shrub_reg", "Shrub_Score_reg",
                "Num_Hydro", "Hydro_Score", "Num_Hydro_reg", "Hydro_Score_reg",
                "Num_SVP", "SVP_Score", "AP_Ratio", "AP_Score",
                "FQAI", "FQAI_Score", "FQAI_Score_FQ", "cov_wt_C", "Cov_Wt_C_Score_FQ",
                "Pct_Bryo", "Pct_Bryo_Score", "Pct_Hydro", "Pct_Hydro_Score", "Pct_Hydro_reg", "Pct_Hydro_Score_reg",
                "Pct_Sens", "Pct_Sens_Score", "Pct_Tol", "Pct_Tol_Score", "Pct_InvGram", "Pct_InvGram_Score",
                "RelDen_SmTree", "SmTree_Score", "SubcanIV", "SubcanIV_Score", "CanopyIV", "CanopyIV_Score",
                "Avg_Bmass", "Biomass_Score", "VIBI_Score_State", "VIBI_Score_ACOEReg", "VIBI_Score_FQ",
                "Avg_Plot_Cover")

 site_cols <- c("LocationID", "FeatureTypes", "FeatureID", "Park", "County", "TotalMods", "IntensMods",
                "PlotConfig", "AreaHA", "SampleYear", "SampleDate", "EventID",
                "DomVeg_Lev1", "DomVeg_Lev2", "DomVeg_Lev3",
                 "X1oPlants", "X1oHGM")

 comb_dat1 <- comb_dat[,c(site_cols, vibi_cols, info_cols)]

 # Adding a final check for dates that don't match across Herbs, Woody and Biomass. Not an easy way to fix
 # dates across the data modules, so just adding a warning when it happens.

 # find years with multiple eventIDs in a given site
 mult_evs_f <- comb_dat1 |> select(LocationID, FeatureID, SampleYear, EventID, SampleDate) |>  unique() |>
   group_by(LocationID, FeatureID, SampleYear) |>
   summarize(num_evs = sum(!is.na(EventID)),
             EventID = first(EventID),
             SampleDate = first(SampleDate),
             .groups = 'drop') |>
   filter(num_evs > 1)

 if(nrow(mult_evs_f) > 0){
  warning(paste0("Multiple sample dates were detected for the following sites within the same year across Herbs, Woody or Biomass",
                 " resulting in multiple rows of VIBI scores for a given site with a year. ",
                 "\n",
                 paste0(mult_evs_f[,c("FeatureID", "EventID")], collapse = "\n")))
 }

 final_dat <- comb_dat1 |> arrange(FeatureID, SampleYear)

 return(data.frame(final_dat))
}
