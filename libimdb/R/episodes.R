require("dplyr")
require("tidyr")
require("rvest")

#' @title Fetches expenditures from deputy using its id
#' @description Fetches expenditures from deputy with his/her parlamentary quota in
#' the last six months.
#' @param dep_id deputy's ID
#' @return Dataframe containing details about the deputy's expenditures
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(dep_id = 178957)
#' @rdname fetch_despesas_deputado
#' @export
get_episode_ratings <- function(url){

  ratings_url = paste0("http://www.imdb.com/", url, "ratings")
  # message(paste("Getting", ratings_url))
  ratings = read_html(ratings_url) %>% 
    html_node("table") %>% 
    html_table(fill=TRUE)
  names(ratings) = c("Votes", "Percentage", "Rating")
  ratings[-1,] %>% 
    mutate(Votes = as.numeric(Votes), 
           Rating = paste0("r", Rating), 
           Percentage = Votes / sum(Votes)) %>% 
    select(-Votes) %>%
    spread(key = Rating, value = Percentage) %>% 
    return()
}




get_all_episodes <- function(series_imdb_id){
  
  title_url = paste0("http://www.imdb.com/title/", series_imdb_id, "/epdate?ref_=ttep_ql_4")
  base_page = read_html(title_url) 
  
  episodes = base_page %>% 
    html_node("#tn15content") %>% 
    html_node("table") %>% 
    html_table(fill=TRUE) %>% 
    select(-5) %>% 
    as.tibble() %>% 
    mutate(UserVotes = as.character(UserVotes))
  
  links = base_page %>% 
    html_node("#tn15content") %>% 
    html_node("table") %>% 
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  episode_data = tibble(link = links) %>% 
    filter(!grepl("vote", links))
  
  episode_ratings = episode_data %>% 
    group_by(link) %>% 
    do(get_episode_ratings(.$link)) 
  
  episodes_full = left_join(episodes %>% mutate(series_ep = 1:NROW(episodes)), 
                            episode_ratings %>% ungroup() %>% mutate(series_ep = 1:NROW(episode_ratings))) %>% 
    mutate(imdb_id = sprintf("%.2f", `#`)) %>% 
    separate(imdb_id, into = c("season", "imdb_ep")) %>% 
    select(-imdb_ep) %>% 
    group_by(season) %>% 
    mutate(season_ep = 1:n())
  
  return(episodes_full)
}   

# https://github.com/analytics-ufcg/rcongresso
# https://github.com/hadley/ggplot2movies
# https://github.com/nazareno/imdb-series
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# http://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
