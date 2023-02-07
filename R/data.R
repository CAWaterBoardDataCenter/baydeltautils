#' A lookup table of sensors available on the California Data Exchange Center
#' website.
#'
#' @format A data frame with 37 rows and 3 variables:
#' \describe{
#'   \item{sensor}{sensor identifier, integer}
#'   \item{variable}{name of variable, string}
#'   \item{unit}{measurement units, string}
#' }
#' @source \url{http://cdec.water.ca.gov}
"CDECSensors"

#' The set of files that comprise the Dayfow dataset. Check the Dayflow website
#' referenced below for updates.
#'
#' Years up until 1996 are contained in pre-merged files; years 1997 and later
#' are individual files.
#'
#' Contents last updated July 16, 2018.
#'
#' @format A list with two named fields:
#' \describe{
#'   \item{merged}{list of URLs for multi-year files, with elements named yyyy_yyyy}
#'   \item{single}{list of URLs for single-year files, with elements named yyyy}
#' }
#' @source \url{https://water.ca.gov/Programs/Environmental-Services/Compliance-Monitoring-And-Assessment/Dayflow-Data}
"dayflowFiles"

#' Full Dayflow data set for water years 1956-2017.
#'
#' Most values are average daily net flows in cubic feet per second. Full
#' documentation available at
#' \url{https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/Environmental-Services/Compliance-Monitoring--Assessment/Dayflow/Files/Publications/Current-Dayflow-Documentation.pdf}.
#'
#' Last updated July 16, 2018.
#'
#' @format A data frame with 22646 rows and 29 columns:
#' \describe{
#'  \item{year}{calendar year}
#'  \item{mo}{month (integer)}
#'  \item{date}{date (Date)}
#'  \item{sac}{Sacramento River flow at Freeport}
#'  \item{yolo}{Yolo Bypass at Woodland}
#'  \item{csmr}{Cosumnes River}
#'  \item{moke}{Mokelumne River}
#'  \item{misc}{Miscellaneous streams}
#'  \item{sjr}{San Joaquin River at Vernalis}
#'  \item{east}{Eastern Delta inflow}
#'  \item{tot}{Total Delta inflow}
#'  \item{ccc}{Contra Costa Water District diversions}
#'  \item{swp}{State Water Project exports}
#'  \item{cvp}{Central Valley Project exports}
#'  \item{nbaq}{North Bay Aqueduct exports}
#'  \item{out}{Net Delta outflow}
#'  \item{exports}{Total Delta exports}
#'  \item{gcd}{Delta gross channel depletions}
#'  \item{prec}{Delta precipitation}
#'  \item{misdv}{Miscellaneous Delta diversions}
#'  \item{cd}{Delta net channel depletions}
#'  \item{xgeo}{Delta Cross Channel plus Georgiana Slough}
#'  \item{west}{San Joaquin River at Jersey Point}
#'  \item{rio}{Sacramento River at Rio Vista}
#'  \item{expin}{Export/Inflow ratio}
#'  \item{diver}{Percent water diverted from the Delta}
#'  \item{effec}{Effective inflow to the western/central Delta}
#'  \item{effdiv}{Effective percent diverted from the western/central Delta}
#'  \item{x2}{Estimated distance from Golden Gate to 2 ppt salinity, km}
#' }
#' @source \url{https://water.ca.gov/Programs/Environmental-Services/Compliance-Monitoring-And-Assessment/Dayflow-Data}
"dayflow"

#' Sacramento Valley Unimpaired Flow Model data at designated locations.
#'
#' [TODO: complete documentation] Last updated January 13, 2017.
#'
#' @format A data frame with 50688 rows and 9 columns:
#' \describe{
#'  \item{date}{date (Date)}
#'  \item{variable}{location of the unimpaired flow estimate (DSS part B)}
#'  \item{value}{unimpaired flow}
#'  \item{unit}{unit of measurement}
#'  \item{a}{DSS part A}
#'  \item{c}{DSS part C}
#'  \item{e}{DSS part E}
#'  \item{f}{DSS part F}
#'  \item{type}{DSS TYPE, e.g., "PER-AVER"}
#' }
#' @source \url{https://d3.water.ca.gov/owncloud/index.php/s/VHQUbo2vlhxfScG}
"svufm_ufc"

#' Unimpaired flow location mapping.
#'
#' Map a subset of common unimpaired flow locations among C2VSim, SVUFM, and
#' SacWAM. Last updated January 13, 2017.
#'
#' @format A data frame with 23 rows and 4 columns:
#' \describe{
#'  \item{name}{descriptive name for each location}
#'  \item{c2vsim}{Reach designation for C2VSim coarse-grid model}
#'  \item{svufm}{SVUFM location name}
#'  \item{sacwam}{SacWAM location name}
#'  }
"uf_mapping"
