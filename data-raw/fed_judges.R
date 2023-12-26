## code to prepare `fedlit` dataset goes here
library(tidyverse)

get_judges <- function(letter_url){
  rvest::read_html(letter_url) |>
    rvest::html_element(
      css = "body > div.l-page > div > div > div > div.view-content") |>
    rvest::html_elements("a") |>
    rvest::html_attrs() -> judge_stems
  paste0("https://www.fjc.gov", judge_stems)
}

get_text <- function(url){
  print(url)
  Sys.sleep(.1)
  url |>
    rvest::read_html() |>
    rvest::html_element(
      css = "body > div.l-page > div > div > div > div") |>
    rvest::html_text2()
}

start_of_match <- function(string, pattern){
  stringr::str_locate(string, pattern)[ , 1]
}

# function to get the first part of a string based on a match
str_remove_after <- function(string, pattern){
  location <- start_of_match(string, pattern )
  location[is.na(location)] <- nchar(string)[is.na(location)]
  stringr::str_sub( string, start = 0, end = location)
}

str_remove_before <- function(string, pattern){
  location <- start_of_match(string, pattern )
  location[is.na(location)] <- nchar(string)[is.na(location)]
  location <- location + nchar(pattern)
  stringr::str_sub( string, start = location)
}

# using a function factor here so that I can avoid editing this so much while
# testing. these are nice for interactive use!
ff <- function(key_words, rest = ".*?[0-9]{4}[:punct:]" ){
  patterns <-glue::glue(
    "({{key_words}}{{rest}})"
    , key_words = key_words
    , group_name = group_name
    , rest = rest
    , .open = "{{"
    , .close = "}}"
  )
  purrr::partial(stringr::str_extract_all, pattern = patterns)
}

# base url with pages for each letter:
url <- "https://www.fjc.gov/history/judges/search/glossary-search/"

letters <- letters
urls <- paste0(url, letters)

# use the get_judges function to get the list of links for all the judges
judge_urls <- purrr::map(urls, get_judges) |>  purrr::list_c()

# get it all
get_text_safely <- purrr::safely(get_text, otherwise = NA)

#this takes a bit. may need to do it safely
texts_results <- purrr:::map(judge_urls, get_text_safely)

names(texts_results) <- judge_urls
save(texts_results, file = "texts.Rdata")

texts <- purrr::map_chr(texts_results, "result")

texts_results |>
  map("result") |>
  str_remove_after("Education") |>
  str_remove_after("Supreme Court Oath:") |>
  str_remove_after("Other Federal Judicial Service:") |>
  str_remove_after("Professional Career")-> short_texts

#this gets the dates for various court changes
get_dates <- ff(c("[nN]ominated"
                  , "[cC]ommission"
                  , "[sS]tatus"
                  , "[:space:][Aa]ssigned"
                  , "[Rr]eassigned"
                  , "[tT]erminated")
)

dates <- purrr::map(short_texts, get_dates)

# This kinda works
get_dates_df <- function(list){
  list[c(2,4,5)] |>
    purrr::reduce(c) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::reduce(dplyr::bind_rows) |>
    tidyr::separate_wider_delim(
      value
      , delim = " on "
      , names = c("action", "date")
    )
}

safely_get_dates_df <- purrr::safely(get_dates_df, NA)
dates_df <- purrr::map(dates, safely_get_dates_df)

# we need this later to make sure that the data sets stay consistent. The
# problem is that a few of these judges were never confirmed, and others were in
# weird courts that don't exist anymore
select <- map_lgl(dates_df |>  map("error"), is.null)

errors <- purrr::map(dates_df, "error") |>
  purrr::discard(is.null)

dates_df <- purrr::map(dates_df, "result") |>
  purrr::discard( \(x) any(is.na(x)))

purrr::imap(dates_df, .f =  \(x, idx){
  x |>
    dplyr::mutate(id = idx)}
) -> dates_df

df <- dplyr::bind_rows(dates_df)
df$date <- df$date |> str_sub(1, -2)
df$date <- lubridate::mdy(df$date)

# these could both  local to the get courts thing if needed, but I feel like i
# might reuse this patter for getting judge names
get_layer  <- function(list, comb = c(1,2), layer){
  list[comb] |>
    purrr::map(layer)
}

handle_confirmation <- function(list, n_discomf){
  if(rlang::is_empty(list[[3]])){
    list
  } else {
    get_layer(list, layer = n_discomf + 1)
  }
}
# this works except for the not confirmed people. need to handle how to drop
# them then drop them off actually need the dates to do this perfectly but I
# don't think it matters because no judge has ever failed to be nominated to a
# lower court before going on to be added to the surpremes
get_court_df <- function(list){

  n_not_confirmed <- length(list[[3]])

  list |>
    handle_confirmation(n_discomf = n_not_confirmed) |>
    purrr::reduce(c) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::reduce(dplyr::bind_rows) |>
    tidyr::separate_wider_delim(
      value
      , delim = ","
      , names = c("title", "court")
      , too_many = "merge"
    )
}

# putting aside the tough edge cases from above for now. 23 out of 4000 is okay,
# but will fix soon.
get_courts <- ff(c("Justice", "Judge", "not confirmed"), rest = ".*?\n")
courts <- purrr::map(short_texts, get_courts)
courts <- courts[which(select)]
courts_df <- purrr::map(courts, get_court_df)

purrr::imap(courts_df, .f =  \(x, idx){
  x |>
    dplyr::mutate(id = idx)
}) -> courts

courts <- dplyr::bind_rows(courts)

judge_urls <- names(texts_results)

judge_names <- stringr::str_remove(
  judge_urls
  , "https://www.fjc.gov/history/judges/"
)
# these are last, first second etc.
judge_names <- judge_names[which(select)]

nominations <-
  dates[which(select)] |>
  map(1) |>
  map(as_tibble) |>
  imap(\(x, idx) x |> mutate(id = idx)) |>
  bind_rows()

nominations <- nominations |>
  tidyr::separate_wider_delim(
    value
    , delim = " on "
    , names = c("nominator", "nomination_date")
    , too_many = "merge"
  )

terminations <-
  dates[which(select)] |>
  map(6) |>
  map(as_tibble) |>
  imap(\(x, idx) x |> mutate(id = idx)) |>
  bind_rows() |>
  rename(termination_date = value)

senior_status <-
  dates[which(select)] |>
  map(3) |>
  map(as_tibble) |>
  imap(\(x, idx) x |> mutate(id = idx)) |>
  bind_rows() |>
  distinct() |>
  rename(senior_date = value)

judge_df <- tibble(
  judge_names = judge_names
  , judge_urls = judge_urls[select]
)

judge_df$id <- seq(1, nrow(judge_df))

#group by ids and then add a rownumber to create a court-level id.
add_group_id <- function(df){
  df |>
    group_by(id) |>
    mutate(
      row_id = row_number()
      , full_id = paste0(id, "-", row_id)
    ) |>
    select(-row_id)
}

df <- add_group_id(df)
courts <- add_group_id(courts)
terminations <- add_group_id(terminations)
nominations <- add_group_id(nominations)

left_join(df, courts |> ungroup() |> select(-id), by = "full_id") -> df
left_join(df, nominations |> ungroup() |> select(-id), by = "full_id") -> df
left_join(df, terminations |> ungroup() |> select(-id), by = "full_id") -> df
left_join(df, senior_status |> ungroup(), by = "id") -> df
left_join(df, judge_df, by = "id") -> df

df <- df |>
  mutate(termination_date = str_remove_before(termination_date, "on ")
         , nominator = str_remove_before(nominator, "by ")
         , senior_date = str_remove_before(senior_date, "on "))

df |>
  mutate(across(ends_with("_date"), \(x) stringr::str_sub(x, 1,-2 ))) |>
  mutate(across(ends_with("_date"), lubridate::mdy)) |>
  mutate(action = stringr::str_trim(action)) -> fed_judges

usethis::use_data(fed_judges, overwrite = TRUE)
