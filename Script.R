library(rvest)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)

wc_squads <- read_html("https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_squads")

tables <- wc_squads %>% 
  html_table()

teams <- c("Ecuador", "Netherlands", "Qatar", "Senegal",
           "England", "Iran", "United States", "Wales",
           "Argentina", "Mexico", "Poland", "Saudi Arabia",
           "Australia", "Denmark", "France", "Tunisia",
           "Costa Rica", "Germany", "Japan", "Spain",
           "Belgium", "Canada", "Croatia", "Morocco",
           "Brazil", "Cameroon", "Serbia", "Switzerland",
           "Ghana", "Portugal", "South Korea", "Uruguay")

df <- 
  map_df(1:32, ~ {
    tables[[.x]] %>% 
      mutate(Country = teams[.x]) %>%
      select(Player, Country, Club)
  }) %>% 
  rownames_to_column("id")

df_network <- 
  map_df(df$id, ~ {
    
    country <- filter(df, id == .x) %>% 
      pull(Country) %>% 
      unique()
    
    club <- filter(df, id == .x) %>% 
      pull(Club) %>% 
      unique()
    
    country_teammates <- filter(df, Country == country, id != .x)
    
    club_teammates <- filter(df, Club == club, id != .x)
    
    bind_rows(country_teammates, club_teammates) %>% 
      rename(to = id) %>% 
      mutate(from = .x) %>% 
      distinct() %>% 
      select(from, to)
    
  })

wc_graph <- as_tbl_graph(df_network) %>% 
  left_join(df, by = c("name" = "id"))

ggraph(wc_graph) +
  geom_edge_fan(alpha = .25) + 
  geom_node_point(aes(colour = Country)) +
  theme_graph()

wc_eigen <-
eigen_centrality(
  wc_graph,
  directed = FALSE,
  scale = TRUE,
  weights = NULL,
  options = arpack_defaults
)
