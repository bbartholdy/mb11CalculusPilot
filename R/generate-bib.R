#' Generate the bibliography
#'
#' @importFrom rbbt bbt_detect_citations bbt_write_bib
#' @importFrom here here

generate_bib <- function(){
  cite_keys <- bbt_detect_citations(list.files(here("inst/paper/"), "*.qmd", full.names = T))
  ignore_keys <- grep("fig-|tbl-", cite_keys, value = T)
  bbt_write_bib(
    here("inst/paper/references.bib"),
    keys = cite_keys,
    ignore = ignore_keys,
    overwrite = T
  )
}
