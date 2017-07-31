#' Parse Emoji List file
#'
#' @param url url for emoji list file
#'
#' @importFrom magrittr %>% not
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when select
#' @importFrom rvest html_attr html_nodes html_node html_text
#' @importFrom stringr str_replace str_split str_detect str_replace_all
#' @importFrom xml2 read_html
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

  codes <- html %>% html_nodes("td.code a") %>% html_text() %>% str_replace_all("U[+]", "") %>% str_split(" ")
  emojis <- tibble(
    emoji     = encode_utf8(codes),
    name      = names[c(TRUE, FALSE)] %>% html_text(),
    keywords  = names[c(FALSE, TRUE)] %>% html_text() %>% str_split( " [|] "),
    codes     = codes,
    apple     = vendor(table, 4),
    google    = vendor(table, 5),
    twitter   = vendor(table, 6),
    one       = vendor(table, 7),
    facebook  = vendor(table, 8),
    messenger = vendor(table, 9),
    samsung   = vendor(table, 10),
    windows   = vendor(table, 11)
  )
  emojis
}
