
#' Skin tone modifier functions
#'
#' @param txt An emoji to alter skin tone to
#' @return an emoji with modified skin tone (if appropriate)
#'
#' @rdname skin_tones
#' @export
light <- function( txt ) emoji( paste0( txt, from_runes("U+1F3FB") ) )

#' @rdname skin_tones
#' @export
medium_light <- function( txt ) emoji( paste0( txt, from_runes("U+1F3FC") ) )

#' @export
#' @rdname skin_tones
medium <- function( txt ) emoji( paste0( txt, from_runes("U+1F3FD") ) )

#' @export
#' @rdname skin_tones
medium_dark <- function(txt) emoji( paste0( txt, from_runes("U+1F3FE") ) )

#' @export
#' @rdname skin_tones
dark <- function(txt) emoji( paste0( txt, from_runes("U+1F3FF") ) )
