#' SCImago taxonomy not working
#'
#'
#' @format A dataframe with nodes:
#' \describe{
#' \item{Country}{Name of the country}
#' \item{Occupation}{Occupation according to the taxonomy of Pantheon}
#' \item{Value}{ Quantity of globally famous people that were born in that country} 
#' }
#' @source Raw data before the aggregation was queried from 
#' \url{http://www.scimagojr.com/} in 2016.
#' @references SCImago. (2007). SJR-SCImago Journal & Country Rank.
#' @keywords dataset
#' @examples
#' data(scimago)
#' str(scimago)
#' summary(scimago)
#' pantheon[pantheon$Country=="Chile",]
"pantheon"
#'
#'
#'
#' rs_ucsd
#'
#'
#' @format An igraph object with proximities for the Research Space in UCSD classification:
#' \describe{
#' \item{Country}{Name of the country}
#' \item{Occupation}{Occupation according to the taxonomy of Pantheon}
#' \item{Value}{ Quantity of globally famous people that were born in that country} 
#' }
#' @source Research space of paper on Scientometrics 
#' \url{http://www.scimagojr.com/} in 2016.
#' @references Guevara,Hartmann,Aristaran, Mendoza, Hidalgo, 2016.
#' @keywords dataset
#' @examples
#' 
#'
#'#' scimago_nodes 
#'
#'
#' @format A dataframe with nodes for the SCImago classification:
#' \describe{
#' \item{Country}{Name of the country}
#' \item{Occupation}{Occupation according to the taxonomy of Pantheon}
#' \item{Value}{ Quantity of globally famous people that were born in that country} 
#' }
#' @source Raw data before the aggregation was queried from 
#' \url{http://www.scimagojr.com/} in 2016.
#' @references SCImago. (2007). SJR-SCImago Journal & Country Rank.
#' @keywords dataset
#' @examples
#' data(scimago)
#' str(scimago)
#' summary(scimago)
#' pantheon[pantheon$Country=="Chile",]
#' 
#' 
#' #'#' ucsd_nodes 
#'
#'
#' @format A dataframe with nodes of UCSD classification of Science:
#' \describe{
#' \item{Country}{Name of the country}
#' \item{Occupation}{Occupation according to the taxonomy of Pantheon}
#' \item{Value}{ Quantity of globally famous people that were born in that country} 
#' }
#' @source Raw data before the aggregation was queried from 
#' \url{http://www.scimagojr.com/} in 2016.
#' @references SCImago. (2007). SJR-SCImago Journal & Country Rank.
#' @keywords dataset
#' @examples
#' data(scimago)
#' str(scimago)
#' summary(scimago)
#' pantheon[pantheon$Country=="Chile",]
