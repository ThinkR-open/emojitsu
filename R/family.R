
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
runes_to_char <- function(runes){
  str_split(runes, " ") %>%
    map_chr( ~{
      str_replace_all(.x, "U[+]", "") %>%
        sprintf( "\\U%08s", . ) %>%
        paste(collapse="") %>%
        stri_unescape_unicode()
    } )
}


family_runes <- c( man   = "U+1F468", woman = "U+1F469", boy   = "U+1F466", girl  = "U+1F467" )

#' Emoji sequence for couple with heart
#'
#' @param x man or woman
#' @param y man or woman
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang quo_name enquo
#' @export
couple_with_heart <- function(x, y){
  # TODO: make sure x and y are man or woman
  x <- quo_name(enquo(x))
  y <- quo_name(enquo(y))

  adults <- c("man", "woman")
  assert_that( x %in% adults)
  assert_that( y %in% adults)

  # only 3 cases are supported, so we just swap the 4th case
  if( x == "man" && y == "woman"){
    x <- "woman"
    y <- "man"
  }

  runes <- paste( family_runes[x], "U+200D U+2764 U+FE0F U+200D" , family_runes[y] )
  char  <- runes_to_char( runes )
  structure(char, class = "emoji")
}

#' @export
print.emoji <- function(x, ...){
  cat(x, "\n")
  invisible(x)
}

globalVariables( c("man", "woman") )



