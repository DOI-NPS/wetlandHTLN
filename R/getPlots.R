#' @title getPlots: get Plot data
#'
#' @description This function filters plot-level location data data by plot and plot types.
#'
#' @param plot_type Filter on plot type. Options are "all", "VIBIplotID", (default), "oramID",
#' "wellID", and "wetlndID". Can choose multiple options.
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
#' # return all plots
#' plots <- getPlots()
#'
#' # return only reference sites
#' ref <- getPlots(survey_type = c("reference", "womc, reference"))
#'
#' # return only wetlands of management concern
#' womc <- getPlots(survey_type = c("womc", "womc, reference", "survey, womc"))
#'
#' # return only depressional wetlands
#' depr <- getPlots(hgm_class = "Depression")
#'
#' # return only forested vegetation types
#' forest <- getPlots(dom_veg1 = "forest")
#'
#' # return non-forested vegetation types
#' nonfor <- getPlots(dom_veg1 = c("shrub", "emergent"))
#'
#' # return plots for subset of plots
#' plots <- getPlots(plotID = c("1007", "1017", "1043"))
#' }
#'
#' @return Returns a data frame of plot data
#' @export

getPlots <- function(plot_type = "VIBIplotID", survey_type = 'all', hgm_class = 'all', dom_veg1 = 'all',
                     plotID = 'all', intens_mods = 1:4){

  #---- Bug handling ----
  plot_type <- match.arg(plot_type, several.ok = T,
                         choices = c("all", "VIBIplotID", "oramID", "wellID", "wetlndID"))
  survey_type <- match.arg(survey_type, several.ok = T,
                           choices = c("all", "reference", "survey", "survey, womc", "womc", "womc, reference"))
  hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)

  stopifnot(class(intens_mods) == "numeric" | class(intens_mods) == "integer")

  #---- Compile data ----
  env <- if(exists("HTLN_wetlands")){HTLN_wetlands} else {.GlobalEnv}

  tryCatch(plot <- get("locations", envir = env),
           error = function(e){stop("tbl_Locations not found. Please run importData() first.")})

  plot1 <- if(any(plot_type == 'all')){plot
    } else {plot[plot$FeatureTypes %in% plot_type,]}

  plot2 <- if(any(survey_type == 'all')){plot1
  } else {plot1[plot1$SurveyType %in% survey_type,]}

  plot2$HGMClass <- NA_character_
  plot2$HGMClass[plot2$HGM_ID %in% c("IA", "IB")] <- "Depression"
  plot2$HGMClass[plot2$HGM_ID %in% c("IIA", "IIB")] <- "Impoundment"
  plot2$HGMClass[plot2$HGM_ID %in% c("IIIA", "IIIB", "IIIC")] <- "Riverine"
  plot2$HGMClass[plot2$HGM_ID %in% c("IVA", "IVB", "IVC")] <- "Slope"

  plot3 <- if(any(hgm_class == 'all')){plot2
  } else {plot2[plot2$HGMClass %in% hgm_class,]}

  plot4 <- if(any(dom_veg1 == 'all')){plot3
  } else {plot3[plot3$DomVeg_Lev1 %in% dom_veg1,]}

  plot5 <- if(any(plotID == 'all')){plot4
  } else {plot4[plot4$FeatureID %in% plotID,]}

  plot6 <- plot5[plot5$IntensMods %in% intens_mods,]

  return(data.frame(plot6))
}

