library(tidyverse)
library(rvest)
library(stringi)

#' Parse Emoji List file
#'
#' @importFrom magrittr %>% not
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when select
#' @importFrom rvest html_attr html_nodes html_node html_text
#' @importFrom stringr str_replace str_split str_detect str_replace_all
#' @importFrom stringi stri_unescape_unicode
#' @importFrom xml2 read_html
#' @importFrom purrr map_chr
#' @export
parse_emoji_list <- function(
  emoji_list = "http://unicode.org/emoji/charts/emoji-list.html",
  full_emoji_list = "http://unicode.org/emoji/charts/full-emoji-list.html"
) {

  # most of the information is in the first file
  html  <- read_html(emoji_list)
  names <- html %>% html_nodes("td.name")

  # but then the other file can be used to identify vendor coverage
  table <- read_html(full_emoji_list) %>% html_node("table")

  vendor <- function(table, idx = 4){
    selector <- sprintf( "tr td:nth-child(%d)", idx )
    table %>%
      html_nodes(selector) %>%
      str_detect("miss") %>%
      not()
  }

  escape_runes <- function(runes){
    str_replace_all(runes, "U[+]", "") %>%
      sprintf( "\\U%08s", . ) %>%
      paste(collapse="")
  }

  runes <- html %>% html_nodes("td.code a") %>% html_text()
  escaped <- runes %>% str_split(" ") %>% map_chr(escape_runes)

  tibble(
    emoji     = stri_unescape_unicode(escaped),
    name      = names[c(TRUE, FALSE)] %>% html_text(),
    keywords  = names[c(FALSE, TRUE)] %>% html_text() %>% str_split( " [|] "),
    runes     = runes,
    escaped   = escaped,
    apple     = vendor(table, 4),
    google    = vendor(table, 5),
    twitter   = vendor(table, 6),
    one       = vendor(table, 7),
    facebook  = vendor(table, 8),
    messenger = vendor(table, 9),
    samsung   = vendor(table, 10),
    windows   = vendor(table, 11)
  )
}
emojis <- parse_emoji_list()
use_data( emojis, overwrite = TRUE)
