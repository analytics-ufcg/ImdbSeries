source("R/episodes.R")

series_to_fetch = read_csv("data/series_urls.csv")

for(i in seq(1, NROW(series_to_fetch), by = 4)){
  output_file = paste0("data/series_from_imdb-", i, ".csv")
  if(!file.exists(output_file)){
    series_data = series_to_fetch %>% 
      dplyr::slice(i:min(i + 4, NROW(series_to_fetch))) %>% 
      dplyr::group_by(series_name) %>% 
      dplyr::do(tryCatch({flog.info(paste("Getting", .$series_name, .$imdb_id));
        get_all_episodes(.$imdb_id)}, 
        error = function(e) data.frame(NA)))
    series_data %>% 
      dplyr::select(-2) %>% 
      readr::write_csv(output_file)
  }
}

files = list.files("./data/", "^series_from_imdb-", full.names = TRUE)
there_should_be = floor(NROW(series_to_fetch) / 4)
if(length(files) != there_should_be){
  message("Not all series were fetch. There should be ", there_should_be, " files, but there are ", length(files))
} else {
  all_data = dplyr::tibble(file = files) %>% 
    dplyr::group_by(file) %>% 
    dplyr::do(read_csv(.$file, 
                col_types = readr::cols(    
                  series_name = readr::col_character(),
                  Episode = readr::col_character(),
                  UserRating = readr::col_double(),
                  UserVotes = readr::col_number(),
                  series_ep = readr::col_integer(),
                  link = readr::col_character(),
                  r1 = readr::col_double(),
                  r10 = readr::col_double(),
                  r2 = readr::col_double(),
                  r3 = readr::col_double(),
                  r4 = readr::col_double(),
                  r5 = readr::col_double(),
                  r6 = readr::col_double(),
                  r7 = readr::col_double(),
                  r8 = readr::col_double(),
                  r9 = readr::col_double(),
                  season = readr::col_integer(),
                  season_ep = readr::col_integer()
                ))) %>% 
    dplyr::ungroup()
  all_data %>% 
    dplyr::select(-1, -20) %>%
    dplyr::mutate(link = paste0("http://www.imdb.com", link)) %>% 
    dplyr::select(series_name, 
           Episode,
           series_ep, 
           season, 
           season_ep,
           url = link,
           UserRating, 
           UserVotes,
           r1, 
           r2, 
           r3, 
           r4, 
           r5, 
           r6, 
           r7, 
           r8, 
           r9, 
           r10) %>% 
    dplyr::filter(complete.cases(.)) %>% 
    dplyr::distinct(series_name, 
             Episode,
             season, 
             season_ep,
             url, 
             .keep_all = TRUE) %>% 
    readr::write_csv("data/series_from_imdb.csv")
}

