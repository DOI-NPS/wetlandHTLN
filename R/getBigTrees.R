#' @title getBigTrees: get big tree data
#'
#' @importFrom dplyr filter
#'
#' @description This function filters big tree data by plot, year, plot types, and nativity.
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
#' # return all big tree data
#' bigtree <- getBigTrees()
#'
#' # return only 2023 data
#' bigtree23 <- getBigTrees(years = 2023)
#'
#' # return 2020 and later
#' bigtree20 <- getBigTrees(years = 2020:2024)
#'
#' # return only reference sites
#' ref <- getBigTrees(survey_type = c("reference", "womc, reference"))
#'
#' # return only wetlands of management concern
#' womc <- getBigTrees(survey_type = c("womc", "womc, reference", "survey, womc"))
#'
#' # return only depressional wetlands
#' depr <- getBigTrees(hgm_class = "Depression")
#'
#' # return only forested vegetation types
#' forest <- getBigTrees(dom_veg1 = "forest")
#'
#' # return non-forested vegetation types
#' nonfor <- getBigTrees(dom_veg1 = c("shrub", "emergent"))
#'
#' # return native species only
#' nat <- getBigTrees(nativity = "native")
#'
#' # return bigtree species for subset of plots
#' bigtree_plots <- getBigTrees(plotID = c("1007", "1017", "1034", "1036", "1043"))
#'
#' }
#'
#' @return Returns a data frame of bigtree count data
#' @export

getBigTrees <- function(years = 2008:as.numeric(format(Sys.Date(), format = "%Y")),
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

  tryCatch(bigtree <- get("bigtreesVIBI", envir = env),
           error = function(e){stop("tbl_BigTrees not found. Please run importData() first.")})

  plots <- getPlots(plot_type = "VIBIplotID", survey_type = survey_type, hgm_class = hgm_class,
                    dom_veg1 = dom_veg1, plotID = plotID, intens_mods = intens_mods)

  plot_ids <- plots$LocationID

  bigtree1 <- bigtree[bigtree$LocationID %in% plot_ids,]

  bigtree2 <- bigtree1 |> filter(SampleYear %in% years)

  bigtree3 <- if(any(nativity == 'all')){bigtree2
  } else {bigtree2 |> dplyr::filter(OH_STATUS %in% nativity)}

  names(bigtree3)[names(bigtree3)]

  return(data.frame(bigtree3))
}

