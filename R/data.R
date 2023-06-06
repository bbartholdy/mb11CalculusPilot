# can't produce doc if data aren\t exported by namespace

#' Middenbeemster dental calculus data
#'
#' Dental calculus scores collected from Middenbeemster (MB11), a rural 19th century
#' archaeological site in the Netherlands.
#'
#' @format ## `calculus_full`
#' A data frame with 41 rows and 97 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{t11:t48_buc/lin/ipx}{An integer denoting the calculus score (0-3) on
#'   a given tooth surface: tooth number (FDI notation)_surface.
#'   buc = buccal; lin = lingual, ipx = inter-proximal.}
#'   ...
#' }
#' @source insert publication DOI
"calculus_full"

#' Middenbeemster dental caries data
#'
#' Dental caries scores collected from Middenbeemster (MB11), a rural 19th century
#' archaeological site in the Netherlands.
#'
#' @format ## `caries`
#' A data frame with 41 rows and 33 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{t11:t48}{a character string denoting the location of a caries lesion
#'   on a given tooth (FDI notation), with multiple lesions separated by `;`}
#' }
#' @source insert publication DOI
"caries"


#' Middenbeemster periodontitis data
#'
#' Periodontitis scores collected from Middenbeemster (MB11), a rural 19th century
#' archaeological site in the Netherlands. Scores were recorded on a scale of
#' 0-3 based on amount of alveolar bone loss.
#'
#' @format ## `periodont`
#' A data frame with 41 rows and 33 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{t11:t48}{an integer denoting periodontitis score (0-3) for a given tooth (FDI notation)}
#' }
#' @source insert publication DOI
"periodont"

#' Middenbeemster periapical lesion data
#'
#' Data on periapical lesions collected from Middenbeemster (MB11), a rural 19th century
#' archaeological site in the Netherlands.
#'
#' @format ## `periap`
#' A data frame with 41 rows and 33 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{t11:t48}{a character string denoting the location of a periapical lesion
#'   on a given tooth (FDI notation), with multiple lesions separated by `;`}
#' }
#' @source insert publication DOI
"periap"

