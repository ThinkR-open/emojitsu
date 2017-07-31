scrap_emojipedia <- function(url= "https://emojipedia.org/apple/ios-10.3/") {

  nodes <- read_html(url) %>%
    html_nodes("li img")

  emojis <- tibble(
    title   = nodes %>% html_attr("title"),
    src     = nodes %>% html_attr("src"),
    datasrc = nodes %>% html_attr("data-src")
  ) %>%
    mutate(
      src = case_when(
        src == "/static/img/lazy.svg" ~ datasrc,
        TRUE                          ~  src
      ),
      code_points = src %>% str_replace( "^.*_(.*)[.]png$", '\\1' ) %>% str_split('-')
    ) %>%
    select( -datasrc )

  emojis
}
