library(emojirx)

emoji_data <- parse_emoji_data( "data-raw/emoji-data.txt" )
use_data( emoji_data, overwrite = TRUE)
