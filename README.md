# wetlandHTLN
This R package was developed to analyze freshwater wetland data collected in Cuyahoga Valley National Park by the 
Heartland Network (HTLN). 

The R package can be installed using `pak::pkg_install('doi-nps/wetlandHTLN')`. Previous archived versions 
of this R package can be found at <a href="www.github.com/katemmiller/wetlandHTLN">www.github.com/katemmiller/wetlandHTLN</a>

This package was designed to query HTLN wetland data collected using Ohio EPA <a href = 'https://dam.assets.ohio.gov/image/upload/epa.ohio.gov/Portals/35/wetlands/Part9_field_manual_v1_5rev15aug15.pdf'>
Field Manual for the Vegetation Index of Biotic Integrity v1.5</a> and calculates the <a href="https://dam.assets.ohio.gov/image/upload/epa.ohio.gov/Portals/35/401/VIBI_FQ_FINAL.pdf">
Vegetation Index of Biotic Integrity</a> for specified sites.

The first function to start with is the `importData()` function, which imports tables from the Heartland Network 
database and compiles flat files to be incorporated into the data package. You can either set a data source name 
(DSN) called "HTLN_wetlands" to import the data, or can specify a file name. Eventually this will be enabled to 
import the data package csvs from IRMA.

The following 'get' functions allow you to compile and query the different modules of the wetland data:
<ul>
<li>getPlots.R: query plot level data.</li>
<li>getBigTrees.R: query trees tallied that are >= 30cm DBH</li>
<li>getBiomass.R: query biomass samples</li>
<li>getHerbs.R: query herb data</li>
<li>getWoody.R: query woody stem data</li>
</ul>
    
The following functions calculate the OH VIBI:
<ul>
<li>joinVIBI_module.R: calculates VIBI by individual module.</li>
<li>joinVIBI_plot.R: calculates VIBI at the plot level. A plot must have 4 modules sampled to use this metric. 
This should match scores with OH VIBI spreadsheet.</li>
<li>sumVIBI.R: summarizes plot level VIBI by averaging module-level VIBI scores. This allows plots with fewer than 4 modules
to be more equally treated than plots with 4 modules.</li>
</ul>
