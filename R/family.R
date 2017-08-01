
#' @export
man <- emoji( from_runes("U+1F468") )

#' @export
woman <- emoji( from_runes("U+1F469") )

#' @export
boy <- emoji( from_runes("U+1F466") )

#' @export
girl <- emoji( from_runes("U+1F467") )


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
#' holding_hands( man, woman )
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

man_and_woman_holding_hands <- emoji( from_runes("U+1F46B") )
two_men_holding_hands <- emoji( from_runes("U+1F46C") )
two_women_holding_hands <- emoji( from_runes("U+1F46D") )

#' @rdname family_sequence
#' @export
holding_hands <- function(x, y){
  adults <- c(man, woman)
  assert_that( x %in% adults)
  assert_that( y %in% adults)

  if( x == man && y == man){
    two_men_holding_hands
  } else if( x == woman && y == woman ){
    two_women_holding_hands
  } else {
    man_and_woman_holding_hands
  }

}
