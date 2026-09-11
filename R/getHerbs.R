#' @title getHerbs: get herb data
#'
#' @importFrom dplyr filter
#'
#' @description This function filters herb data by plot, year, plot types, species, and species groupings.
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
#' @param nativity Quoted string. Filter on native status. Options are "all" (default), "adventive",
#' "cryptogeni", or "native". Can choose multiple statuses.
#'
#' @param intens_mods Filter on total number of intensive modules. Ranges from 1 to 4. Can select multiple.
#' Default is 1:4 (all).
#'
#' @examples
#' \dontrun{
#' # run first
#' importData()
#'
#' # return all herb data
#' herbs <- getHerbs()
#'
#' # return only 2023 data
#' herb23 <- getHerbs(years = 2023)
#'
#' # return 2020 and later
#' herb20 <- getHerbs(years = 2020:2024)
#'
#' # return only reference sites
#' ref <- getHerbs(survey_type = c("reference", "womc, reference"))
#'
#' # return only wetlands of management concern
#' womc <- getHerbs(survey_type = c("womc", "womc, reference", "survey, womc"))
#'
#' # return only depressional wetlands
#' depr <- getHerbs(hgm_class = "Depression")
#'
#' # return only forested vegetation types
#' forest <- getHerbs(dom_veg1 = "forest")
#'
#' # return non-forested vegetation types
#' nonfor <- getHerbs(dom_veg1 = c("shrub", "emergent"))
#'
#' # return native species only
#' nat <- getHerbs(nativity = "native")
#'
#' # return herbs for subset of plots
#' herb_plots <- getHerbs(plotID = c("1007", "1017", "1034", "1036", "1043"))
#'
#' }
#'
#' @return Returns a data frame of herb cover data
#'
#' @export
#'

getHerbs <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
                     survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                     plotID = 'all', nativity = 'all', intens_mods = 1:4){

  #---- Bug handling ----
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)
  nativity <- match.arg(nativity, choices = c("all", "adventive", "cryptogeni", "native"), several.ok = T)
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)
  stopifnot(class(intens_mods) == "numeric" | class(intens_mods) == "integer")

  #---- Compile data ----
  env <- if(exists("HTLN_wetlands")){HTLN_wetlands} else {.GlobalEnv}

  tryCatch(herbs <- get("herbVIBI", envir = env),
           error = function(e){stop("tbl_VIBI_Herb not found. Please run importData() first.")})

  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, intens_mods = intens_mods)

  plot_ids <- plots$LocationID

  herbs1 <- herbs[herbs$LocationID %in% plot_ids,]

  herbs2 <- herbs1 |> filter(SampleYear %in% years)

  herbs3 <- if(any(nativity == 'all')){herbs2
  } else {herbs2 |> dplyr::filter(OH_STATUS %in% nativity)}

  return(data.frame(herbs3))
}

