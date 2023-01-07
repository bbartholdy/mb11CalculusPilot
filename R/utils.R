#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Produce detailed tooth region information from tooth number (FDI notation)
#'
#' @param .data data frame containing tooth number (t + FDI notation)
#' @return Returns a data frame with tooth number, region (maxilla, mandible),
#' position (anterior, posterior), side, tooth_type (incisor, canine, etc.)
#' @export tooth_position

tooth_position <- function(.data){
  # vectors to categorise teeth by position and type (FDI notation)
  maxilla <- c(paste0("t", 11:18), paste0("t", 21:28))
  mandible <- c(paste0("t", 31:38), paste0("t", 41:48))
  all_teeth <- c(maxilla, mandible)
  left <- all_teeth[str_detect(all_teeth, "2.|3.")]
  posterior <- all_teeth[str_detect(all_teeth, ".[4-8]")]
  molar <- all_teeth[str_detect(all_teeth, "\\d([6-8])")]
  premolar <- all_teeth[str_detect(all_teeth, "\\d([4-5])")]
  canine <- all_teeth[str_detect(all_teeth, "\\d(3)")]
  incisor <- all_teeth[str_detect(all_teeth, "\\d([1-2])")]

  central_incisor <- all_teeth[str_detect(all_teeth, "\\d(1)")]
  lateral_incisor <- all_teeth[str_detect(all_teeth, "\\d(2)")]
  premolar_1 <- all_teeth[str_detect(all_teeth, "\\d(4)")]
  premolar_2 <- all_teeth[str_detect(all_teeth, "\\d(5)")]
  molar_1 <- all_teeth[str_detect(all_teeth, "\\d(6)")]
  molar_2 <- all_teeth[str_detect(all_teeth, "\\d(7)")]
  molar_3 <- all_teeth[str_detect(all_teeth, "\\d(8)")]

  .data %>%
    mutate(
      region = if_else(tooth %in% maxilla, "maxilla", "mandible"),
      side = if_else(tooth %in% left, "left", "right"),
      position = if_else(tooth %in% posterior, "posterior", "anterior"),
      class = case_when(
        tooth %in% incisor ~ "incisor",
        tooth %in% canine ~ "canine",
        tooth %in% premolar ~ "premolar",
        tooth %in% molar ~ "molar"
        ),
      type = case_when( # inefficient...
        tooth %in% central_incisor ~ "i1",
        tooth %in% lateral_incisor ~ "i2",
        tooth %in% canine ~ "c",
        tooth %in% premolar_1 ~ "pm1",
        tooth %in% premolar_2 ~ "pm2",
        tooth %in% molar_1 ~ "m1",
        tooth %in% molar_2 ~ "m2",
        tooth %in% molar_3 ~ "m3",
        ),
      quadrant = case_when(
        region == "maxilla" & position == "anterior" ~ "UA",
        region == "maxilla" & position == "posterior" ~ "UP",
        region == "mandible" & position == "anterior" ~ "LA",
        region == "mandible" & position == "posterior" ~ "LP",
        )
    )
}

#' Convert wide data frame containing dental info to long format and add tooth
#' position variables.
#'
#' @param .data A data frame containing one column with a unique identifier and
#' multiple columns with calculus scores from each surface of a tooth.
#' @param cols Which columns to pivot into longer format. These should be the columns
#' containing the scores for each tooth.
#' @return Returns a data frame with tooth number, region (maxilla, mandible),
#' position (anterior, posterior), side, tooth_type (incisor, canine, etc.)
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @export dental_longer

dental_longer <- function(.data, cols){
  if(ncol(select(.data, {{ cols }} )) > 32){
    .data %>%
      pivot_longer(
        {{ cols }},
        names_to = c("tooth", "surface"),
        names_pattern = "(.*)_(.*)",
        values_to = c("score")
      ) %>%
      tooth_position()
  } else {
    .data %>%
      pivot_longer(
        {{ cols }},
        names_to = "tooth",
        values_to = "score"
      ) %>%
      tooth_position()
  }

}

#' Dentition checker
#'
#' Checks if the data frame satisfies the criterion that all three surfaces must
#' be present between both sides of the dentition.
#'
#' @param .data A data frame containing one column with a unique identifier and
#' multiple columns with calculus scores from each surface of a tooth.
#' @return Returns a warning if data frame does not fulfill criterion.
#' @export dentition_checker
dentition_checker <- function(.data){
  # tooth should be removed if a surface is missing
  # Minimally, each surface must be scorable on at least one side of the dentition to obtain the index.
  selector <- .data %>%
    group_by(id,type, tooth, side) %>%
    summarise(n_surfaces = n()) %>%
    arrange(desc(n_surfaces)) %>%
    group_by(id, type, side) %>%
    slice(which.max(n_surfaces)) %>%
    select(id, tooth, side)

  missing_surfaces <- .data %>%
    group_by(id,tooth) %>%
    summarise(
      n_surfaces = sum(!is.na(score))) %>%
    filter(n_surfaces < 3) %>%
    select(id, tooth)

  warning(paste(nrow(missing_surfaces), "teeth removed due to missing surfaces"))

  .data %>%
    anti_join(missing_surfaces, by = c("id", "tooth"))
}


#' Function to convert compound abbreviations to full name

# compound_name_repair <- function(.data){
#   # repair abbreviated compound names
#     .data %>%
#     rename(
#       "THCA-A" = "thca-a",
#       "Cocaine" = "cocaine",
#       "Caffeine" = "caffeine",
#       "Theophylline" = "theophyl",
#       "Cotinine" = "cotinine",
#       "Nicotine" = "nicotine",
#       "Salicylic acid" = "salicyl",
#       "CBN" = "cbn",
#       "THCVA" = "thcva",
#       "THC" = "thc",
#       "CBD" = "cbd"
#     )
# }

# compound_name_repair <- function(.data, ...){
#   # repair abbreviated compound names
#   .data %>%
#     pivot_wider( {{ ... }} ) %>%
#     rename(
#       "THCA-A" = "thca-a",
#       "Cocaine" = "cocaine",
#       "Caffeine" = "caffeine",
#       "Theophylline" = "theophyl",
#       "Cotinine" = "cotinine",
#       "Nicotine" = "nicotine",
#       "Salicylic acid" = "salicyl",
#       "CBN" = "cbn",
#       "THCVA" = "thcva",
#       "THC" = "thc",
#       "CBD" = "cbd"
#     )
# }

# compound_name_repair <- function(.data, var){
#
#   # repair abbreviated compound names
#   compound_name_abbrev <- data.frame(
#     name = c("THCA-A", "Cocaine", "Caffeine", "Theophylline", "Cotinine", "Nicotine",
#              "Salicylic acid", "CBN", "THCVA", "THC", "CBD"),
#     abbrev = c("thca-a", "cocaine", "caffeine", "theophyl", "cotinine", "nicotine", "salicyl",
#                "cbn", "thcva", "thc", "cbd")
#   )
#
#   .data %>%
#     left_join(compound_name_abbrev, by = c(var = "abbrev"))
# }


# compound_name_repair <- function(variable,value){
#   compound_names <- list(
#     "thca-a" = "THCA-A",
#     "cocaine" = "Cocaine",
#     "caffeine" = "Caffeine",
#     "theophyl" = "Theophylline",
#     "cotinine" = "Cotinine",
#     "nicotine" = "Nicotine",
#     "salicyl" = "Salicylic acid",
#     "cbn" = "CBN",
#     "thcva" = "THCVA",
#     "thc" = "THC",
#     "cbd" = "CBD"
#   )
#   return(compound_names[value])
# }

#' correlation statements
#'
#' @param .data correlation tibble. Long format.
#' @param strength character. Strength of correlation ("weak", "moderate", or "strong").
#' @export
cor_statement <- function(.data, strength){

  corr_tib <- .data %>%
    #as_tibble(rownames = "var") %>%
    #pivot_longer(-var, values_to = "corr") %>%
    mutate(
      strength = case_when(
        abs(corr) >= 0.8 ~ "strong",
        abs(corr) < 0.8 & abs(corr) >= 0.4 ~ "moderate",
        abs(corr) < 0.4 ~ "weak"
      ),
      direction = case_when(
        corr > 0 ~ "positive",
        corr < 0 ~ "negative"
      )
    ) %>%
    filter(corr != 1)

  if(nrow(filter(corr_tib, strength == strength)) == 0){
    correlations <- glue("No {strength} correlations were found")
  } else {
    correlations <- corr_tib %>%
      filter(strength == {{ strength }}) %>%
      #distinct(rho, .keep_all = T) %>%
      slice(seq(from = 2, to = nrow(.), by = 2)) %>%  # awkward solution to distinct not working
      mutate(statement = glue("{var} and {name} ({signif(corr, 3)})")) %>%
      .$statement %>%
      paste(collapse = ", ")
  }
  return(correlations)
}

#' @export
compound_names <- c(
  "thca-a" = "THCA-A",
  "cocaine" = "Cocaine",
  "caffeine" = "Caffeine",
  "theophyl" = "Theophylline",
  "cotinine" = "Cotinine",
  "nicotine" = "Nicotine",
  "salicyl" = "Salicylic acid",
  "cbn" = "CBN",
  "thcva" = "THCVA",
  "thc" = "THC",
  "cbd" = "CBD"
)

prettified_names <- c(
  "thca-a" = "THCA-A",
  "cocaine" = "Cocaine",
  "caffeine" = "Caffeine",
  "theophyl" = "Theophylline",
  "cotinine" = "Cotinine",
  "nicotine" = "Nicotine",
  "salicyl" = "Salicylic acid",
  "cbn" = "CBN",
  "thcva" = "THCVA",
  "thc" = "THC",
  "cbd" = "CBD",
  "caries_ratio" = "Caries",
  "periodont_status" = "Periodontitis",
  "calc_index" = "Calculus",
  "pipe_notches" = "PN",
  "age" = "Age-at-death"
)

compound_name_abbrev <- data.frame(
  name = c("THCA-A", "Cocaine", "Caffeine", "Theophylline", "Cotinine", "Nicotine",
           "Salicylic acid", "CBN", "THCVA", "THC", "CBD"),
  abbrev = c("thca-a", "cocaine", "caffeine", "theophyl", "cotinine", "nicotine", "salicyl",
             "cbn", "thcva", "thc", "cbd")
)
