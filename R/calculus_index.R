#' Calculate calculus index
#'
#' @details Calculate the calculus index for each quadrant of an individual.
#' Based on Greene et al. 2005.
#' @param .data Data frame in long format, with one tooth surface per row, and one calculus score per surface.
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
#' @param ... Can be used to add grouping variable(s). Group by tooth, tooth type, individual, etc.
#' @return Returns a data frame with caries ratio per group.
#' @export

caries_ratio <- function(.data, .caries, ...){
  .data %>%
    group_by(..., .add = T) %>%
    summarise(
      #n_teeth = n(),
      n_teeth = sum(!is.na( {{ .caries }} ), na.rm = T),
      count = sum( {{ .caries }} , na.rm = T),
      ratio = count / n_teeth
    )
}
