# can't produce doc if data aren\t exported by namespace

#' Sample metadata
#'
#' Metadata for the samples analysed by UHPLC-MS/MS.
#' @format ## `metadata`
#' A data frame with 44 rows and 8 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{sample}{Dental calculus sample number}
#'   \item{element}{FDI notation for tooth/teeth sampled for each individual.}
#'   \item{KZ_sample}{Dental calculus sample number on tubes from previous study.}
#'   \item{KZ_element}{FDI notation for tooth/teeth sampled from the previous study for each individual.}
#'   \item{replicated}{Whether or not the sample was replicated in a separate UHPLC-MS/MS analysis.}
#'   \item{batch1_weight}{weight (mg) of dental calculus sample analysed in the first batch.}
#'   \item{batch2_weight}{weight (mg) of dental calculus sample analysed in the replication batch.}
#'   ...
#' }
#' @source https://doi.org/10.5281/zenodo.8010630
"metadata"

#' Lower limits of quantitation (LLOQ)
#'
#' LLOQ values for the targeted compounds.
#' @format ## `lloq`
#' A data frame with 76 rows and 2 columns:
#' \describe{
#'   \item{compound}{Name of the compound.}
#'   \item{lloq}{Lower limit of quantitation (ng)}
#'   ...
#' }
#' @source https://doi.org/10.5281/zenodo.8010630
"lloq"

#' UHPLC analysis data from both batches
#'
#' Contains the results for each sample from each extraction step (washes, and dissolution)
#' analysed by UHPLC in the first and second (replication) batch.
#' @format ## `uhplc_data_comb`
#' A data frame with 42 rows and 91 columns:
#' \describe{
#'   \item{sample}{number on the sample tube.}
#'   \item{<compound>_wash{1..3}_batch{1..2}}{Absolute quantity (ng) of compound detected in sample washes.}
#'   \item{<compound>_calc_batch{1..2}}{Absolute quantity (ng) of compound detected after sample dissolution.}
#'   \item{batch{1..2}_weight}{Weight of the calculus sample in mg.}
#'   ...
#' }
#' @source https://doi.org/10.5281/zenodo.8010630
"uhplc_data_comb"


#' Middenbeemster sample demographics
#'
#' Age and sex of the individuals with dental calculus scored in `mb11_calculus`.
#'
#' @format ## `demography`
#' A data frame with 41 rows and 6 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{age}{Age category of the individual. eya = early young adult (18-24);
#'   lya = late young adult (25-34); ma = middle adult (35-49); old = old adult (50+).}
#'   \item{sex}{Biological sex of individual estimated using osteological methods.
#'   f = female; pf = probable female; pm = probable male; m = male.}
#'   ...
#' }
#' @source https://doi.org/10.5281/zenodo.7649151
"demography"

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
#' @source https://doi.org/10.5281/zenodo.8010630
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
#' @source https://doi.org/10.5281/zenodo.8010630
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
#' @source https://doi.org/10.5281/zenodo.8010630
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
#' @source https://doi.org/10.5281/zenodo.8010630
"periap"

#' Middenbeemster dental inventory
#'
#' Data on dental status.
#'
#' @format ## `dental_inv`
#' A data frame with 41 rows and 33 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{t11:t48}{a character string denoting the status of a tooth.
#'   p = present, m = missing (for unknown reason), aml = antemortem loss, dna = tooth missing because it is being sampled for DNA.}
#' }
#' @source https://doi.org/10.5281/zenodo.7649151
"dental_inv"

#' Middenbeemster sinusitis
#'
#' Data on sinusitis diagnosis of individuals from Middenbeemster based on skeletal lesions.
#'
#' @format ## `sinusitis_clean`
#' A data frame with 42 rows and 3 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{CMS}{Presence (TRUE) or absence (FALSE) of lesions associated with chronic maxillary sinusitis.}
#'   \item{IPR}{Presence (TRUE) or absence (FALSE) of periosteal reaction on visceral surface of ribs.}
#' }
#' @source https://doi.org/10.5281/zenodo.7649151
"sinusitis_clean"

#' Middenbeemster pathological conditions
#'
#' Data on various disease diagnoses of individuals from Middenbeemster based on skeletal lesions.
#'
#' @format ## `path_cond_clean`
#' A data frame with 41 rows and 13 columns:
#' \describe{
#'   \item{id}{Skeletal ID}
#'   \item{OA}{Presence (TRUE) or absence (FALSE) of lesions associated with osteoarthritis.}
#'   \item{IVDD}{Presence (TRUE) or absence (FALSE) of lesions associated with intervertebral disc disease.}
#'   \item{TB}{Presence (TRUE) or absence (FALSE) of lesions associated with tuberculosis.}
#'   \item{Mastoiditis}{Presence (TRUE) or absence (FALSE) of lesions associated with mastoiditis.}
#'   \item{DISH}{Presence (TRUE) or absence (FALSE) of lesions associated with DISH.}
#'   \item{VOP}{Presence (TRUE) or absence (FALSE) of lesions associated with vertebral osteophytosis.}
#'   \item{SN}{Presence (TRUE) or absence (FALSE) of lesions associated with Schmorl's nodes.}
#'   \item{DDD}{Presence (TRUE) or absence (FALSE) of lesions associated with degenerative disc disease.}
#'   \item{PNBF}{Presence (TRUE) or absence (FALSE) of periosteal new bone formation.}
#'   \item{OD}{Presence (TRUE) or absence (FALSE) of lesions associated with osteochondritis dissecans.}
#'   \item{CF}{Presence (TRUE) or absence (FALSE) of cribra femora.}
#'   \item{CO}{Presence (TRUE) or absence (FALSE) of cribra orbitalia.}
#' }
#' @source https://doi.org/10.5281/zenodo.7649151
"path_cond_clean"
