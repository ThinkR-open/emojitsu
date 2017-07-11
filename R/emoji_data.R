

#' parse emoji data
#'
#' @importFrom magrittr %>%
#' @importFrom readr read_lines locale
#' @importFrom stringr str_subset str_split str_trim str_replace
#' @importFrom tibble tibble
#' @importFrom dplyr mutate pull select
#' @importFrom purrr map_chr
#' @importFrom tidyr unnest
#' @export
parse_emoji_data <- function( file ){
  read_lines( file , locale = locale(encoding="utf-8") ) %>%
    str_subset("^[^#]") %>%
    tibble( spec = .) %>%
    mutate(
      data        = str_split(spec, ";"),
      char_range  = map_chr(data, 1) %>% str_trim(),
      range_parts = str_split(char_range, "[.][.]"),
      code_point  = parse16(range_parts),
      value       = map_chr(data, 2) %>% str_split("#") %>% map_chr(1) %>% str_trim(),
      description = spec %>% str_replace("^.*[)][[:space:]]+", "" )
    ) %>%
    select( char_range, code_point, value, description ) %>%
    unnest()
}


#' @export
parse_emoji_sequence <- function(file){
  read_lines( file , locale = locale(encoding="utf-8") ) %>%
    str_subset("^[^#]") %>%
    tibble( spec = . ) %>%
    mutate(
      data = str_split( spec, ";" ),
      sequence = str_trim(map_chr(data, 1)),
      parts = str_split(sequence, " ")
    )
}
