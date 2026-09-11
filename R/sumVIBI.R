#' @title sumVIBI: summarize plot-level VIBI metrics
#'
#' @include joinVIBI_module.R
#' @importFrom dplyr arrange group_by select summarize
#'
#' @description This function summarizes plot-level VIBI by averaging module-level VIBI scores, and
#' filters by plot, year, and plot types. Note that the Biomass metric assumes the area sampled is
#' always 0.1m2. Metric calculations and thresholds follow INTEGRATED WETLAND ASSESSMENT PROGRAM
#' Part 9: Field Manual for the Vegetation Index of Biotic Integrity for Wetlands v. 1.5.
#' Pages 17 - 20 and Table 2 were most useful. For plot-level VIBI scores that are comparable with
#' OH VIBI scores from other sites, use joinVIBI_plot(). Note however, that the joinVIBI_plot() only
#' rates sites with 4 intensive modules. This function allows sites with fewer than 4 modules to be
#' assessed on the same scale as sites with 4 modules. The VIBI scores with this function will be lower
#' than the original VIBI spreadsheet approach, because the VIBI spreadsheet sums across the 4 modules,
#' rather than averages. VIBI scores are applied after the module-level metrics are averaged to a plot-level value.
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
#' "forest", "shrub". Can choose multiple options.
#'
#' @param plotID Quoted string. Default is 'all'. If specified will return data for only plots specified.
#' Can choose multiple plots. Based on FeatureID in database.
#'
#' @param region Quoted string. Default is "NCNE". Specifies the Army Corps Region for OH.
#' Options are "EMP", "MW", and "NCNE".
#'
#' @param intens_mods Filter on total number of intensive modules. Ranges from 1 to 4. Can select multiple.
#' Default is 1:4 (all).
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#' # average modules to plot-level VIBI for all sites (default).
#' vibi <- sumVIBI()
#'
#' # average modules to plot-level VIBI for all sites sampled in 2023
#' vibi23 <- sumVIBI(years = 2023)
#'
#' # average modules to plot-level VIBI for all emergent wetlands
#' vibi_emerg <- sumVIBI(dom_veg1 = "emergent")
#'
#' # average modules to plot-level VIBI for depressional wetlands
#'
#' vibi_dep <- sumVIBI(hgm_class = "Depression")
#'
#'
#' }
#'
#' @return Returns a data frame of average VIBI calculations for each plot
#' @export
#'

sumVIBI <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                    survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                    plotID = 'all', region = "NCNE", intens_mods = 1:4){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)
  region <- match.arg(region, choices = c("NCNE", "EMP", "MW"))
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)
  stopifnot(class(intens_mods) == "numeric" | class(intens_mods) == "integer")

  #---- Summarize vibi at plot level ----
  env <- if(exists("HTLN_wetlands")){HTLN_wetlands} else {.GlobalEnv}

  vibi <- suppressWarnings(
    joinVIBI_module(years = years, survey_type = survey_type, hgm_class = hgm_class, dom_veg1 = dom_veg1,
                          plotID = plotID, region = region, intens_mods = intens_mods)
    )

  sum_vibi <- vibi |>
    group_by(LocationID, FeatureTypes, FeatureID, Park, County, TotalMods,
             PlotConfig, AreaHA, SampleYear, SampleDate, EventID,
             DomVeg_Lev1, DomVeg_Lev2, DomVeg_Lev3, X1oPlants, X1oHGM) |>
    summarize(Num_Carex = mean(Num_Carex, na.rm = T),
              Num_Cyper = mean(Num_Cyper, na.rm = T),
              Num_Dicot = mean(Num_Dicot, na.rm = T),
              Num_Shade = mean(Num_Shade, na.rm = T),
              Num_Shrub = mean(Num_Shrub, na.rm = T),
              Num_Shrub_reg = mean(Num_Shrub_reg, na.rm = T),
              Num_Hydro = mean(Num_Hydro, na.rm = T),
              Num_Hydro_reg = mean(Num_Hydro_reg, na.rm = T),
              Num_SVP = mean(Num_SVP, na.rm = T),
              AP_Ratio = mean(AP_Ratio, na.rm = T),
              FQAI = mean(FQAI, na.rm = T),
              cov_wt_C = mean(cov_wt_C, na.rm = T),
              Pct_Bryo = mean(Pct_Bryo, na.rm = T),
              Pct_Hydro = mean(Pct_Hydro, na.rm = T),
              Pct_Hydro_reg = mean(Pct_Hydro_reg, na.rm = T),
              Pct_Sens = mean(Pct_Sens, na.rm = T),
              Pct_Tol = mean(Pct_Tol, na.rm = T),
              Pct_InvGram = mean(Pct_InvGram, na.rm = T),
              RelDen_SmTree = mean(RelDen_SmTree, na.rm = T),
              SubcanIV = mean(SubcanIV, na.rm = T),
              CanopyIV = mean(CanopyIV, na.rm = T),
              Avg_Bmass = mean(Avg_Bmass, na.rm = T),
              Avg_Plot_Cover = mean(Avg_Plot_Cover, na.rm = T),
              trees_per_ha = mean(trees_per_ha, na.rm = T),
              shrubs_per_ha = mean(shrubs_per_ha, na.rm = T),
              pct_button = mean(pct_button, na.rm = T),
              pct_nat_hydro = mean(pct_nat_hydro, na.rm = T),
              pct_nat_peren = mean(pct_nat_peren, na.rm = T),
              pct_peren = mean(pct_peren, na.rm = T),
              pct_advent = mean(pct_advent, na.rm = T),
              wet_index = mean(wet_index, na.rm = T),
              tot_stems_per_ha = mean(tot_stems_per_ha, na.rm = T),
              .groups = 'drop') |>
    mutate(Carex_Score = case_when(is.na(Num_Carex) ~ NA_real_,
                                   Num_Carex <= 1 ~ 0,
                                   between(Num_Carex, 2, 3) ~ 3,
                                   Num_Carex == 4 ~ 7,
                                   Num_Carex >= 5 ~ 10,
                                   TRUE ~ NA_real_),

           Cyper_Score = case_when(is.na(Num_Cyper) ~ NA_real_,
                                   Num_Cyper <= 1 ~ 0,
                                   between(Num_Cyper, 2, 3) ~ 3,
                                   between(Num_Cyper, 4, 6) ~ 7,
                                   Num_Cyper >= 7 ~ 10,
                                   TRUE ~ NA_real_),

           Dicot_Score = case_when(is.na(Num_Dicot) ~ NA_real_,
                                   DomVeg_Lev1 == "emergent" & Num_Dicot <= 10 ~ 0,
                                   DomVeg_Lev1 == "emergent" & between(Num_Dicot, 11, 17) ~ 3,
                                   DomVeg_Lev1 == "emergent" & between(Num_Dicot, 18, 24) ~ 7,
                                   DomVeg_Lev1 == "emergent" & Num_Dicot >= 25 ~ 10,

                                   DomVeg_Lev1 == "shrub" & Num_Dicot <= 9 ~ 0,
                                   DomVeg_Lev1 == "shrub" & between(Num_Dicot, 10, 14) ~ 3,
                                   DomVeg_Lev1 == "shrub" & between(Num_Dicot, 15, 23) ~ 7,
                                   DomVeg_Lev1 == "shrub" & Num_Dicot >= 24 ~ 10,
                                   TRUE ~ NA_real_),

           Shade_Score = case_when(is.na(Num_Shade) ~ NA_real_,
                                   Num_Shade <= 7 ~ 0,
                                   between(Num_Shade, 8, 13) ~ 3,
                                   between(Num_Shade, 14, 20) ~ 7,
                                   Num_Shade >= 21 ~ 10,
                                   TRUE ~ NA_real_),

           Shrub_Score_reg = case_when(is.na(Num_Shrub_reg) ~ NA_real_,
                                       Num_Shrub_reg <= 1 ~ 0,
                                       Num_Shrub_reg == 2 ~ 3,
                                       between(Num_Shrub_reg, 3, 4) ~ 7,
                                       Num_Shrub_reg >= 5 ~ 10,
                                       TRUE ~ NA_real_),

           Shrub_Score = case_when(is.na(Num_Shrub) ~ NA_real_,
                                   Num_Shrub <= 1 ~ 0,
                                   Num_Shrub == 2 ~ 3,
                                   between(Num_Shrub, 3, 4) ~ 7,
                                   Num_Shrub >= 5 ~ 10,
                                   TRUE ~ NA_real_),

           Hydro_Score_reg = case_when(is.na(Num_Hydro_reg) ~ NA_real_,
                                       DomVeg_Lev1 == "emergent" & Num_Hydro_reg <= 10 ~ 0,
                                       DomVeg_Lev1 == "emergent" & between(Num_Hydro_reg, 11, 20) ~ 3,
                                       DomVeg_Lev1 == "emergent" & between(Num_Hydro_reg, 21, 30) ~ 7,
                                       DomVeg_Lev1 == "emergent" & Num_Hydro_reg >= 31 ~ 10,

                                       DomVeg_Lev1 == "shrub" & Num_Hydro_reg <= 9 ~ 0,
                                       DomVeg_Lev1 == "shrub" & between(Num_Hydro_reg, 10, 14) ~ 3,
                                       DomVeg_Lev1 == "shrub" & between(Num_Hydro_reg, 15, 20) ~ 7,
                                       DomVeg_Lev1 == "shrub" & Num_Hydro_reg >= 21 ~ 10,
                            TRUE ~ NA_real_),

           Hydro_Score = case_when(is.na(Num_Hydro) ~ NA_real_,
                                   DomVeg_Lev1 == "emergent" & Num_Hydro <= 10 ~ 0,
                                   DomVeg_Lev1 == "emergent" & between(Num_Hydro, 11, 20) ~ 3,
                                   DomVeg_Lev1 == "emergent" & between(Num_Hydro, 21, 30) ~ 7,
                                   DomVeg_Lev1 == "emergent" & Num_Hydro >= 31 ~ 10,

                                   DomVeg_Lev1 == "shrub" & Num_Hydro <= 9 ~ 0,
                                   DomVeg_Lev1 == "shrub" & between(Num_Hydro, 10, 14) ~ 3,
                                   DomVeg_Lev1 == "shrub" & between(Num_Hydro, 15, 20) ~ 7,
                                   DomVeg_Lev1 == "shrub" & Num_Hydro >= 21 ~ 10,

                                   TRUE ~ NA_real_),
           AP_Score = case_when(is.na(AP_Ratio) ~ NA_real_,
                                AP_Ratio > 0.48 ~ 0,
                                between(AP_Ratio, 0.32, 0.48) ~ 3,
                                between(AP_Ratio, 0.20, 0.32) ~ 7,
                                AP_Ratio <= 0.20 ~ 10,
                                TRUE ~ NA_real_),

           SVP_Score = case_when(is.na(Num_SVP) ~ NA_real_,
                                 Num_SVP == 0 ~ 0,
                                 Num_SVP == 1 ~ 3,
                                 Num_SVP == 2 ~ 7,
                                 Num_SVP >= 3 ~ 10,
                                 TRUE ~ NA_real_),

           FQAI_Score = case_when(is.na(FQAI) ~ NA_real_,
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
                                     TRUE ~ NA_real_),

           Cov_Wt_C_Score_FQ = case_when(Avg_Plot_Cover < 0.75 & DomVeg_Lev1 == "forest" ~
                                           (((Avg_Plot_Cover/0.75) * cov_wt_C)/6) * 50, #from spreadsheet
                                         is.na(cov_wt_C) ~ NA_real_,
                                         cov_wt_C == 0 ~ 0,
                                         cov_wt_C > 0 & cov_wt_C <= 6 ~ (cov_wt_C/6) * 50,
                                         cov_wt_C > 6 ~ 50),

           Pct_Bryo_Score = case_when(is.na(Pct_Bryo) ~ NA_real_,
                                      Pct_Bryo <= 0.01 ~ 0,
                                      Pct_Bryo > 0.01 & Pct_Bryo <= 0.03 ~ 3,
                                      Pct_Bryo > 0.03 & Pct_Bryo <= 0.06 ~ 7,
                                      Pct_Bryo > 0.06 ~ 10,
                                      TRUE ~ NA_real_),

           Pct_Hydro_Score_reg = case_when(is.na(Pct_Hydro_reg) ~ NA_real_,
                                           Avg_Plot_Cover < 0.10 ~ 0, # first case
                                           Pct_Hydro_reg <= 0.10 ~ 0,
                                           Pct_Hydro_reg > 0.10 & Pct_Hydro_reg <= 0.15 ~ 3,
                                           Pct_Hydro_reg > 0.15 & Pct_Hydro_reg <= 0.28 ~ 7,
                                           Pct_Hydro_reg > 0.28 ~ 10,
                                           TRUE ~ NA_real_),

           Pct_Hydro_Score = case_when(is.na(Pct_Hydro) ~ NA_real_,
                                       Avg_Plot_Cover < 0.10 ~ 0, # first case
                                       Pct_Hydro <= 0.10 ~ 0,
                                       Pct_Hydro > 0.10 & Pct_Hydro <= 0.15 ~ 3,
                                       Pct_Hydro > 0.15 & Pct_Hydro <= 0.28 ~ 7,
                                       Pct_Hydro > 0.28 ~ 10,
                                       TRUE ~ NA_real_),

           Pct_Sens_Score = case_when(is.na(Pct_Sens) ~ NA_real_,
                                      Avg_Plot_Cover < 0.10 ~ 0, # first case,
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
                                      TRUE ~ NA_real_),

           Pct_Tol_Score = case_when(is.na(Pct_Tol) ~ NA_real_,
                                     Avg_Plot_Cover < 0.10 ~ 0, # first case
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
                                     TRUE ~ NA_real_),

           Pct_InvGram_Score = case_when(is.na(Pct_InvGram) ~ NA_real_,
                                         Avg_Plot_Cover < 0.10 ~ 0, # first case
                                         Pct_InvGram >= 0.31 ~ 0,
                                         Pct_InvGram >= 0.15 & Pct_InvGram < 0.31 ~ 3,
                                         Pct_InvGram >= 0.03 & Pct_InvGram < 0.15 ~ 7,
                                         Pct_InvGram < 0.03 ~ 10,
                                         TRUE ~ NA_real_),

           SmTree_Score = case_when(is.na(RelDen_SmTree) ~ NA_real_,
                                    tot_stems_per_ha < 10 ~ 0, # ** Table 2
                                    RelDen_SmTree >= 0.32 ~ 0,
                                    RelDen_SmTree >= 0.22 & RelDen_SmTree < 0.32 ~ 3,
                                    RelDen_SmTree >= 0.11 & RelDen_SmTree < 0.22 ~ 7,
                                    RelDen_SmTree < 0.11 ~ 10,
                                    TRUE ~ NA_real_),

           SubcanIV_Score = case_when(tot_stems_per_ha < 10 ~ 0, #** Table 2
                                      DomVeg_Lev1 == "forest" & SubcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.02 & SubcanIV <= 0.072 ~ 3,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.072 & SubcanIV <= 0.13 ~ 7,
                                      DomVeg_Lev1 == "forest" & SubcanIV > 0.13 ~ 10,

                                      DomVeg_Lev1 == "shrub" & SubcanIV <= 0.02 ~ 0,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.02 & SubcanIV <= 0.05 ~ 3,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.05 & SubcanIV <= 0.10 ~ 7,
                                      DomVeg_Lev1 == "shrub" & SubcanIV > 0.10 ~ 10,
                                      TRUE ~ NA_real_),

           CanopyIV_Score = case_when(CanopyIV >= 0.21 ~ 0,
                                      CanopyIV >= 0.17 & CanopyIV < 0.21 ~ 3,
                                      CanopyIV >= 0.14 & CanopyIV < 0.17 ~ 7,
                                      CanopyIV > 0 & CanopyIV < 0.14 ~ 10,
                                      CanopyIV == 0 ~ 0, # from *** in Table 2
                                      TRUE ~ NA_real_),

           Biomass_Score = case_when(is.na(Avg_Bmass) ~ NA_real_,
                                     Avg_Bmass > 800 ~ 0,
                                     Avg_Bmass >= 451 & Avg_Bmass <= 800 ~ 3,
                                     Avg_Bmass >= 201 & Avg_Bmass < 451 ~ 7,
                                     Avg_Bmass >= 100 & Avg_Bmass < 201 ~ 10,
                                     Avg_Bmass < 100 ~ 0,
                                     TRUE ~ NA_real_)
    )


  # There's likely a more efficient way to do this, but want to make sure the DomVeg_Levs are handled properly
  sum_vibi$Num_Carex[is.na(sum_vibi$Num_Carex)] <- 0
  sum_vibi$Carex_Score[sum_vibi$Num_Carex == 0] <- 0
  sum_vibi$Carex_Score[sum_vibi$DomVeg_Lev1 == "forest"] <- NA

  sum_vibi$Num_Cyper[is.na(sum_vibi$Num_Cyper)] <- 0
  sum_vibi$Cyper_Score[sum_vibi$Num_Cyper == 0] <- 0
  sum_vibi$Cyper_Score[!sum_vibi$DomVeg_Lev1 %in% "emergent-coastal"] <- NA

  sum_vibi$Num_Dicot[is.na(sum_vibi$Num_Dicot)] <- 0
  sum_vibi$Dicot_Score[sum_vibi$Num_Dicot == 0] <- 0
  sum_vibi$Dicot_Score[sum_vibi$DomVeg_Lev1 == "forest"] <- NA

  sum_vibi$Num_Shade[is.na(sum_vibi$Num_Shade)] <- 0
  sum_vibi$Shade_Score[sum_vibi$Num_Shade == 0] <- 0
  sum_vibi$Shade_Score[!sum_vibi$DomVeg_Lev1 %in% "forest"] <- NA

  sum_vibi$Num_Shrub_reg[is.na(sum_vibi$Num_Shrub_reg)] <- 0
  sum_vibi$Shrub_Score_reg[sum_vibi$Num_Shrub_reg == 0] <- 0
  sum_vibi$Shrub_Score_reg[sum_vibi$DomVeg_Lev1 == "forest"] <- NA

  sum_vibi$Num_Shrub[is.na(sum_vibi$Num_Shrub)] <- 0
  sum_vibi$Shrub_Score[sum_vibi$Num_Shrub == 0] <- 0
  sum_vibi$Shrub_Score[sum_vibi$DomVeg_Lev1 == "forest"] <- NA

  sum_vibi$Num_Hydro_reg[is.na(sum_vibi$Num_Hydro_reg)] <- 0
  sum_vibi$Hydro_Score_reg[sum_vibi$Num_Hydro_reg == 0] <- 0
  sum_vibi$Hydro_Score_reg[sum_vibi$DomVeg_Lev1 == "forest"] <- NA

  sum_vibi$Num_Hydro[is.na(sum_vibi$Num_Hydro)] <- 0
  sum_vibi$Hydro_Score[sum_vibi$Num_Hydro == 0] <- 0
  sum_vibi$Hydro_Score[sum_vibi$DomVeg_Lev1 == "forest"] <- NA

  sum_vibi$AP_Ratio[is.na(sum_vibi$AP_Ratio)] <- 0
  sum_vibi$AP_Score[sum_vibi$AP_Ratio == 0] <- 0
  sum_vibi$AP_Score[!sum_vibi$DomVeg_Lev1 %in% "emergent"] <- NA

  sum_vibi$Num_SVP[is.na(sum_vibi$Num_SVP)] <- 0
  sum_vibi$SVP_Score[sum_vibi$Num_SVP == 0] <- 0
  sum_vibi$SVP_Score[sum_vibi$DomVeg_Lev1 == "emergent"] <- NA

  sum_vibi$Pct_Bryo[is.na(sum_vibi$Pct_Bryo)] <- 0
  sum_vibi$Pct_Bryo_Score[sum_vibi$Pct_Bryo == 0] <- 0
  sum_vibi$Pct_Bryo_Score[sum_vibi$DomVeg_Lev1 %in% "emergent"] <- NA

  sum_vibi$Pct_Hydro_reg[is.na(sum_vibi$Pct_Hydro_reg)] <- 0
  sum_vibi$Pct_Hydro_Score_reg[sum_vibi$Pct_Hydro_reg == 0] <- 0
  sum_vibi$Pct_Hydro_Score_reg[!sum_vibi$DomVeg_Lev1 %in% "forest"] <- NA

  sum_vibi$Pct_Hydro[is.na(sum_vibi$Pct_Hydro)] <- 0
  sum_vibi$Pct_Hydro_Score[sum_vibi$Pct_Hydro == 0] <- 0
  sum_vibi$Pct_Hydro_Score[!sum_vibi$DomVeg_Lev1 %in% "forest"] <- NA

  sum_vibi$Pct_Sens[is.na(sum_vibi$Pct_Sens)] <- 0
  sum_vibi$Pct_Sens_Score[sum_vibi$Pct_Sens == 0] <- 0

  sum_vibi$Pct_Tol[is.na(sum_vibi$Pct_Tol)] <- 0
  sum_vibi$Pct_Tol_Score[sum_vibi$Pct_Tol == 0] <- 0

  sum_vibi$Pct_InvGram[is.na(sum_vibi$Pct_InvGram)] <- 0
  sum_vibi$Pct_InvGram_Score[sum_vibi$Pct_InvGram == 0] <- 0
  sum_vibi$Pct_InvGram_Score[sum_vibi$DomVeg_Lev1 == "forest"] <- NA
  sum_vibi$Pct_InvGram_Score[sum_vibi$DomVeg_Lev1 == "shrub" &
                                  (is.na(sum_vibi$DomVeg_Lev2)  |
                                     !sum_vibi$DomVeg_Lev2 == "Bog Shrub Swamp")] <- NA

  sum_vibi$RelDen_SmTree[is.na(sum_vibi$RelDen_SmTree)] <- 0
  sum_vibi$SmTree_Score[sum_vibi$RelDen_SmTree == 0] <- 0
  sum_vibi$SmTree_Score[!sum_vibi$DomVeg_Lev1 %in% "forest"] <- NA

  sum_vibi$SubcanIV[is.na(sum_vibi$SubcanIV)] <- 0
  sum_vibi$SubcanIV_Score[sum_vibi$SubcanIV == 0] <- 0
  sum_vibi$SubcanIV_Score[sum_vibi$DomVeg_Lev1 == "emergent"] <- NA
  sum_vibi$SubcanIV_Score[sum_vibi$DomVeg_Lev2 == "Bog Shrub Swamp"] <- NA

  sum_vibi$CanopyIV[is.na(sum_vibi$CanopyIV)] <- 0
  sum_vibi$CanopyIV_Score[sum_vibi$CanopyIV == 0] <- 0
  sum_vibi$CanopyIV_Score[!sum_vibi$DomVeg_Lev1 %in% "forest"] <- NA

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

  sum_vibi$VIBI_Score_State <- rowSums(sum_vibi[,vibi_score_cols], na.rm = T)
  sum_vibi$VIBI_Score_ACOEReg <- rowSums(sum_vibi[,vibi_score_reg_cols], na.rm = T)
  sum_vibi$VIBI_Score_FQ <- rowSums(sum_vibi[,vibi_fq_scores], na.rm = T)

  return(data.frame(sum_vibi))

  }
