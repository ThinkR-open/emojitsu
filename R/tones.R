
#' Skin tone modifier functions
#'
#' @param txt An emoji to alter skin tone to
#' @param tone tone to apply
#' @return an emoji with modified skin tone (if appropriate)
#'
#' @rdname skin_tones
#' @export
light_skin_tone <- function( txt ) emoji( paste0( txt, from_runes("U+1F3FB") ) )

#' @rdname skin_tones
#' @export
medium_light_skin_tone <- function( txt ) emoji( paste0( txt, from_runes("U+1F3FC") ) )

#' @export
#' @rdname skin_tones
medium_skin_tone <- function( txt ) emoji( paste0( txt, from_runes("U+1F3FD") ) )

#' @export
#' @rdname skin_tones
medium_dark_skin_tone <- function(txt) emoji( paste0( txt, from_runes("U+1F3FE") ) )

#' @export
#' @rdname skin_tones
dark_skin_tone <- function(txt) emoji( paste0( txt, from_runes("U+1F3FF") ) )

#' @export
#' @rdname skin_tones
skin_tone <- function(txt, tone = c("light", "medium_light", "medium", "medium_dark", "dark") ){
  tone <- match.arg(tone)
  switch( tone,
    light = light_skin_tone(txt),
    medium_light = medium_light_skin_tone(txt),
    medium = medium_skin_tone(txt),
    medium_dark = medium_dark_skin_tone(txt),
    dark = dark_skin_tone(txt)
  )
}
