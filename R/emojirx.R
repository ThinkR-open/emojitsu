
#' convert space separated runes into their character representation
#'
#' @param runes vector of space separated runes
#'
#' @examples
#' runes_to_char( "U+1F468" )
#'
#' @importFrom tibble tibble
#' @importFrom stringi stri_unescape_unicode
#' @importFrom stringr str_replace_all str_split
#' @importFrom purrr map_chr
#' @importFrom magrittr %>%
#' @export
from_runes <- function(runes){
  str_split(runes, " ") %>%
    map_chr( ~{
      str_replace_all(.x, "U[+]", "") %>%
        sprintf( "\\U%08s", . ) %>%
        paste(collapse="") %>%
        stri_unescape_unicode()
    } )
}

#' runes
#'
#' @param str character representation of runes, e.g `"U+1F468"`
#'
#' @export
runes <- function(x){
  # TODO: assert this is valid runes
  structure(x, class = "unes")
}

#' @export
print.runes <- function(x, ...){
  #TODO: print in color shwing how many bytes the rune needs
  cat( x, "\n")
  invisible(x)
}

#' @export
emoji <- function( txt ){
  structure( txt, class = "emoji" )
}

zero_width_join <- function(..., .data =c(...)){
  paste( .data, collapse = from_runes("U+200D") )
}

#' @export
print.emoji <- function(x, ...){
  cat(x, "\n")
  invisible(x)
}
