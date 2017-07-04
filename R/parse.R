#' Parse a WhatsApp chat history
#'
#' @param path A path to an exported WhatsApp chat.
#' @importFrom readr read_table
#' @importFrom stringr str_sub
#' @import magrittr
#' @import dplyr
#' @importFrom lubridate dmy_hms
#' @importFrom rlang .data
#' @importFrom purrr map
#' @export
parse_whatsapp <- function(path = "raw_data/_chat_example") {
  read_in <- read_table(path,
             col_names = c("date", "time", "author_and_message"),
             col_types = "ccc")
  read_in %>%
    mutate(datetime = dmy_hms(paste(str_sub(date, 1, -1), time)),
           split_author_message = str_locate(author_and_message, ":")[,1],
           author = str_sub(author_and_message, 1, split_author_message -1),
           message = str_sub(author_and_message, split_author_message + 1),
           short_message = str_sub(message, 1, 20)) %>%
    select(.data$datetime, .data$author, .data$short_message, .data$message)
}

#' @importFrom purrr map_dbl
enrich_chat <- function(chat) {
  chat %>%
    mutate(term_table = map(message, term_table),
           number_of_words = map_dbl(term_table, ~sum(.x$freq)))
}


#' Create a tidy term frequency table
#' @param text A character vector or an object inheriting from `TextDocument`.
#' @param sort Unquoted name of column to sort by. Either term or freq.
#' @param ... Further arguments passed to [tm::termFreq()].
#' @importFrom tm termFreq
term_table <- function(text, sort = term, ...) {
  srt <- enquo(sort)
  out <- termFreq(text, ...)
  if (length(out) < 1) return(tibble())
  data_frame(term_final = names(out),
             freq = unname(out),
             term = order(term_final)) %>%
    arrange(!!srt) %>%
    mutate(term = term_final) %>%
    select(.data$term, .data$freq)
}
