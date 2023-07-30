#' Calculate calculus index
#'
#' @details Calculate the calculus index for each quadrant of an individual.
#' Based on Greene et al. 2005.
#' @param .data Data frame in long format, with one tooth surface per row, and one calculus score per surface.
#' @param simple Whether data should be grouped by id (TRUE) or id and quadrant (FALSE).
#' @importFrom dplyr group_by mutate summarise ungroup
#' @return Returns a data frame with four rows per individual containing the calculated calculus
#' index for each quadrant (CI-UP, CI-LP, CI-UA, CI-LA).
#' @export

calculus_index <- function(.data, simple = FALSE){

  if(simple == FALSE){
    group_data <- .data %>%
      group_by(id, quadrant)
  }
  else{
    group_data <- .data %>%
      group_by(id)
  }

  group_data %>%
  # minimally, each surface must be scorable on at least one side of the dentition
    dentition_checker() %>%
    mutate(
      index = case_when(
        region == "maxilla" & position == "anterior" ~ "CI-UA",
        region == "maxilla" & position == "posterior" ~ "CI-UP",
        region == "mandible" & position == "anterior" ~ "CI-LA",
        region == "mandible" & position == "posterior" ~ "CI-LP",
      )
    ) %>%
    #group_by(id) %>%
    summarise(
      n_surf = sum(!is.na(score)),
      n_teeth = n_surf / 3,
      score_sum = sum(score, na.rm = T),
      calc_index = score_sum / n_surf
    ) %>%
    ungroup()
}

#' Calculate caries ratio
#'
#' @details Calculate the caries ratio for each tooth type. Calculated as the number
#' of caries divided by the number of visible surfaces.
#'
#' @param .data Data frame in long format, with one tooth surface per row, and one calculus score per surface.
#' @param .caries Caries count variable.
#' @param ... Can be used to add arguments to `group_by()`. Group by tooth, tooth type, individual, etc.
#' @importFrom dplyr group_by summarise
#' @return Returns a data frame with caries ratio per group.
#' @export

caries_ratio <- function(.data, .caries, ...){
  .data %>%
    group_by(..., .add = T) %>% # .add = T is unnecessary. can be added with ... in call to function
    summarise(
      #n_teeth = n(),
      n_teeth = sum(!is.na( {{ .caries }} ), na.rm = T),
      count = sum( {{ .caries }} , na.rm = T),
      ratio = count / n_teeth
    )
}

#' Calculate antemortem tooth loss
#'
#' @details Calculate the caries ratio for each tooth type. Calculated as the number
#' of caries divided by the number of visible surfaces.
#'
#' @param .data Data frame in long format, with one tooth surface per row, and one calculus score per surface.
#' @param .status character. dental status (p, m, aml, dna).
#' @param ... Can be used to add grouping variable(s). Group by tooth, tooth type, individual, etc.
#' @importFrom dplyr group_by summarise
#' @return Returns a data frame with amtl ratio per group.
#' @export

amtl_ratio <- function(.data, .status, ...){ # add .missing = "m", .loss = "aml" arguments
  .data %>%
    group_by(...) %>% # .add = T is required if using group_by prior to call to amtl_ratio()
    # remove group_by and add elipses to summarise()?
    summarise(
      n_teeth = sum( {{ .status }} != "m"),
      #n_teeth = sum(!is.na( {{ .amtl }} ), na.rm = T),
      count = sum( {{ .status }} == "aml"),
      ratio = count / n_teeth # need to remove the postmortem loss teeth? What if a tooth is missing? how to calculate??
    )
}
