
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

emoji <- function( txt ){
  structure( txt, class = "emoji" )
}

#' @export
man <- emoji( from_runes("U+1F468") )

#' @export
woman <- emoji( from_runes("U+1F469") )

#' @export
boy <- emoji( from_runes("U+1F466") )

#' @export
girl <- emoji( from_runes("U+1F467") )

zero_width_join <- function(..., .data =c(...)){
  paste( .data, collapse = from_runes("U+200D") )
}

heart <- from_runes("U+2764 U+FE0F")

kiss_mark <- from_runes( "U+1F48B")

#' Family emoji sequences
#'
#' @param x man or woman
#' @param y man or woman
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang quo_name enquo
#' @rdname family_sequence
#'
#' @examples
#' \dontrun{
#' couple_with_heart( man, woman)
#' kiss( man, man )
#' }
#'
#' @export
couple_with_heart <- function(x = woman, y = man){
  adults <- c(man, woman)
  assert_that( x %in% adults)
  assert_that( y %in% adults)

  # only 3 cases are supported, so we just swap the 4th case
  if( x == man && y == woman){
    x <- woman
    y <- man
  }

  emoji( zero_width_join(x, heart, y) )
}

#' @rdname family_sequence
#' @export
kiss <- function(x = man, y = woman){
  adults <- c(man, woman)
  assert_that( x %in% adults)
  assert_that( y %in% adults)

  # special (default case)
  if( x == man && y == woman){
    emoji( from_runes("U+1F48F"))
  } else {
    emoji( zero_width_join(x, heart, kiss_mark, y) )
  }
}

#' @rdname family_sequence
#' @export
family <- function(...){
  members <- c(...)

  # special default case
  if( length(members) == 0){
    emoji( from_runes("U+1F46A") )
  } else {
    # TODO: check that there are 1 or 2 parents and 1 or two kids
    # TODO: when parents are one of each, check that man comes first (don't blame me)
    emoji( zero_width_join(.data = members) )
  }

}


#' @export
print.emoji <- function(x, ...){
  cat(x, "\n")
  invisible(x)
}

globalVariables( c("man", "woman", "boy", "girl") )



