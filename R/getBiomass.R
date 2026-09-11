#' @title getBiomass: get biomass data
#'
#' @description This function filters biomass data data by plot, year, and plot types.
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
#' @param intens_mods Filter on total number of intensive modules. Ranges from 1 to 4. Can select multiple.
#' Default is 1:4 (all).
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#' # return all biomass plant data
#' biomass <- getBiomass()
#'
#' # return only 2023 data
#' biomass23 <- getBiomass(years = 2023)
#'
#' # return 2010 and later
#' biomass10 <- getBiomass(years = 2010:2024)
#'
#' # return only reference sites
#' ref <- getBiomass(survey_type = c("reference", "womc, reference"))
#'
#' # return only wetlands of management concern
#' womc <- getBiomass(survey_type = c("womc", "womc, reference", "survey, womc"))
#'
#' # return only depressional wetlands
#' depr <- getBiomass(hgm_class = "Depression")
#'
#' return only forested vegetation types
#' forest <- getBiomass(dom_veg1 = "forest")
#'
#' # return non-forested vegetation types
#' nonfor <- getBiomass(dom_veg1 = c("shrub", "emergent"))
#'
#' # return biomass for subset of plots
#' biomass_plots <- getBiomass(plotID = c("1007", "1017", "1043"))
#'
#' }
#'
#' @return Returns a data frame of biomass count data
#' @export

getBiomass <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                       survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                       plotID = 'all', intens_mods = 1:4){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)
  stopifnot(class(intens_mods) == "numeric" | class(intens_mods) == "integer")

  #---- Compile data ----
  env <- if(exists("HTLN_wetlands")){HTLN_wetlands} else {.GlobalEnv}

  tryCatch(biomass <- get("biomassVIBI", envir = env),
           error = function(e){stop("tbl_VIBI_Herb_Biomass not found. Please run importData() first.")})

  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, intens_mods = intens_mods)

  plot_ids <- plots$LocationID

  biomass1 <- biomass[biomass$LocationID %in% plot_ids, ]

  biomass2 <- biomass1[biomass1$SampleYear %in% years, ]

  return(data.frame(biomass2))
}
