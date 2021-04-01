library(tidyverse)
library(jsonlite)
library(janitor)
library(cowplot)

# Set environment
WORKING_DIRECTORY <- '~/Desktop/MyData/'
setwd(WORKING_DIRECTORY)

# turn this into a shiny app
# they should only add files with StreamingHistory, and YourLibrary
# pages can be top summary, artists dive, viral songs, listening habits
# artists dive has top artists, middle artists, barely listened to artists
# can also do steady listens (which most of mine are not)
# would be cool for them to be able to export graphics

# Define helper functions 

#' Format string to be plot-ready
#' @param string string: string to format 
#' @return string: cleaned string for label
str_to_lab <- function(string){
  str_to_title(str_replace_all(string, "_", " "))
}

`%notin%` <- Negate(`%in%`)

SPOTIFY_COLORS <- c(
  'black' = '#000000', 
  'white' = '#FFFFFF',
  'green' = '#42D760',
  'gray' = '#797C7D',
  'blue' = '#2E77D0'
)


# Load streaming files
dat <- list.files(pattern = 'StreamingHistory*') %>%
  map_df(~jsonlite::fromJSON(.)) %>% 
  as_tibble() %>% 
  janitor::clean_names()

# Questions - 
# when do you listen most each day? minutes per day over the course of the year? Did that change during the pandemic?
# which songs were most viral for you? What's the difference between top artist and second artist?

# Minutes per day 
# Within a day 

time_per_day <- dat %>% 
  mutate(date_listened = lubridate::date(end_time)) %>% 
  group_by(date_listened) %>%
  summarize(hours_listened = sum(ms_played) / (1000*60*60)) 

time_per_day %>% 
  summary()

mean_hrs_per_day <- time_per_day %>%
  pull(hours_listened) %>% 
  mean()

# Distribution
time_per_day %>% 
  ggplot(aes(x = hours_listened)) +
  geom_histogram(bins = 50, fill = SPOTIFY_COLORS['black']) +
  geom_vline(xintercept = mean_hrs_per_day, 
             color = SPOTIFY_COLORS['green'],
             size = 2) +
  theme_cowplot() +
  labs(title = 'Distribution of hours listened to Spotify',
       y = 'Frequency   ', 
       x = paste0('\n', str_to_lab('hours_listened'))) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# By Day
time_per_day %>% 
  ggplot(aes(y = hours_listened, x = date_listened)) +
  geom_line(color = SPOTIFY_COLORS['black'], size = 1) +
  geom_hline(yintercept = mean_hrs_per_day, 
             color = SPOTIFY_COLORS['green'],
             size = 2) +
  theme_cowplot() +
  labs(title = 'Hours listened to Spotify over Dec 2019 - Dec 2020',
       y = 'Hours Listened', 
       x = paste0('\n', str_to_lab('date_listened'))) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# By Month
time_per_month <- time_per_day %>% 
  mutate(months_date_format = lubridate::floor_date(date_listened, 'month')) %>% 
  group_by(months_date_format) %>%
  summarize(hours_listened = sum(hours_listened)) %>% 
  arrange(months_date_format) %>% 
  mutate(order = row_number(), 
         months_text_format = fct_reorder(paste0(lubridate::month(months_date_format, label = T)
                                                 , ' ',
                                                 lubridate::year(months_date_format)), order), 
         months_for_plot = str_extract(months_text_format, '([A-z])+'))

mean_hrs_per_month <- time_per_month %>%
  pull(hours_listened) %>% 
  mean()

time_per_month %>% 
  ggplot(aes(y = hours_listened, x = months_text_format)) +
  geom_bar(fill = SPOTIFY_COLORS['black'], stat = 'identity') +
  geom_hline(yintercept = mean_hrs_per_month, 
             color = SPOTIFY_COLORS['green'],
             size = 2) +
  theme_cowplot() +
  labs(title = 'Hours listened to Spotify over Dec 2019 - Dec 2020',
       y = 'Hours Listened', 
       x = paste0('\n', str_to_lab('month_listened'))) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5), 
        axis.text.x = element_text(angle = 20, vjust = 0.5)) 

# Artists
artist_stats <- 
  dat %>% 
  mutate(date_listened = lubridate::date(end_time)) %>% 
  group_by(artist_name) %>% 
  summarize(hours_listened = sum(ms_played) / (1000*60*60),
            times_listened = n(), 
            days_listened = length(unique(.$date_listened[.$artist_name == artist_name])))

# top artists
artist_stats %>% 
  arrange(desc(hours_listened)) %>% 
  mutate(order = row_number()) %>%
  filter(artist_name %notin% c('Consider This from NPR',
                               'On The Media', 
                               'Coronavirus Global Update',
                               'America Dissected: Coronavirus ', 
                               'The Daily', 
                               'All Songs Considered',
                               'Hidden Brain', 
                               'The Indicator from Planet Money',
                               'FiveThirtyEight Politics',
                               'Pod Save America'
                               )) %>%
  top_n(50, hours_listened) %>%
  mutate(artist_name = fct_rev(fct_reorder(artist_name, order))) %>%
  ggplot(aes(y = hours_listened, x = artist_name)) +
  geom_bar(fill = SPOTIFY_COLORS['black'], stat = 'identity') +
  coord_flip() +
  theme_cowplot() +
  labs(title = 'Top Artists Dec 2019 - Dec 2020',
       x = 'Artist Name', 
       y = paste0('\n', str_to_lab('hours_listened'))) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) 


# bottom artists
artist_stats %>% 
  arrange(hours_listened) %>% 
  mutate(order = row_number()) %>%
  filter(artist_name %notin% c('Consider This from NPR',
                               'On The Media', 
                               'Coronavirus Global Update',
                               'America Dissected: Coronavirus ', 
                               'The Daily', 
                               'All Songs Considered',
                               'Hidden Brain', 
                               'The Indicator from Planet Money',
                               'FiveThirtyEight Politics',
                               'Pod Save America'
  )) %>%
  filter(order <= 50) %>%
  mutate(artist_name = (fct_reorder(artist_name, order))) %>%
  ggplot(aes(y = hours_listened, x = artist_name)) +
  geom_bar(fill = SPOTIFY_COLORS['black'], stat = 'identity') +
  coord_flip() +
  theme_cowplot() +
  labs(title = 'Barely Listened Artists Dec 2019 - Dec 2020',
       x = 'Artist Name', 
       y = paste0('\n', str_to_lab('hours_listened'))) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) 

# middle artists 
artist_stats %>% 
  arrange(hours_listened) %>% 
  mutate(order = row_number()) %>%
  filter(artist_name %notin% c('Consider This from NPR',
                               'On The Media', 
                               'Coronavirus Global Update',
                               'America Dissected: Coronavirus ', 
                               'The Daily', 
                               'All Songs Considered',
                               'Hidden Brain', 
                               'The Indicator from Planet Money',
                               'FiveThirtyEight Politics',
                               'Pod Save America'
  )) %>%
  filter(order >= length(artist_stats$artist_name)/2 - 25 & order <= length(artist_stats$artist_name)/2 + 25) %>%
  mutate(artist_name = (fct_reorder(artist_name, order))) %>%
  ggplot(aes(y = hours_listened, x = artist_name)) +
  geom_bar(fill = SPOTIFY_COLORS['black'], stat = 'identity') +
  coord_flip() +
  theme_cowplot() +
  labs(title = 'Mid Listened Artists Dec 2019 - Dec 2020',
       x = 'Artist Name', 
       y = paste0('\n', str_to_lab('hours_listened'))) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) 

# artists over time
top_artists <- artist_stats %>% 
  filter(artist_name %notin% c('Consider This from NPR',
                               'On The Media', 
                               'Coronavirus Global Update',
                               'America Dissected: Coronavirus ', 
                               'The Daily', 
                               'All Songs Considered',
                               'Hidden Brain', 
                               'The Indicator from Planet Money',
                               'FiveThirtyEight Politics',
                               'Pod Save America'
  )) %>%
  top_n(6, hours_listened) %>% 
  select(artist_name) %>% 
  pull() %>% 
  unique()

months <- time_per_month %>% 
  select(months_text_format, order)

 artists_months <-  dat %>% 
  mutate(date_listened = lubridate::date(end_time),
         months_date_format = lubridate::floor_date(date_listened, 'month')) %>% 
  group_by(artist_name, months_date_format) %>% 
  summarize(hours_listened = sum(ms_played) / (1000*60*60),
            times_listened = n()) 
 
 artists_months %>% 
   filter(artist_name %in% top_artists) %>% 
   ggplot(aes(x = months_date_format, y = hours_listened)) + 
   geom_bar(stat = 'identity', fill = SPOTIFY_COLORS['black']) + 
   scale_x_date(labels = scales::date_format('%m-%Y'),
                limits = as.Date(c('2019-11-15', '2020-12-15'))) +
   facet_wrap(~artist_name) +
   theme_cowplot() +
   labs(title = 'Top 6 Artists Hours Listened by Month Dec 2019 - Dec 2020',
        x = 'Month', 
        y = paste0('\n', str_to_lab('hours_listened')))

 # Song patterns 
 
# Top songs listens by date 
songs_listen_time <- dat %>% 
   mutate(track_and_artist = paste0(track_name, ' [', artist_name, ']')) %>% 
   group_by(track_and_artist) %>% 
   summarize(total_hours_listened = sum(ms_played) / (1000*60*60)) %>% 
   arrange(desc(total_hours_listened)) 
 
top_songs_names <- songs_listen_time %>%
   ungroup() %>%
   top_n(6, total_hours_listened) %>% 
   select(track_and_artist) %>% 
   pull()

dat %>% 
  mutate(track_and_artist = paste0(track_name, ' [', artist_name, ']')) %>% 
  filter(track_and_artist %in% top_songs_names) %>% 
  mutate(date_listened = lubridate::date(end_time)) %>% 
  group_by(track_and_artist, date_listened) %>% 
  summarize(total_minutes_listened = sum(ms_played) / (1000*60)) %>%
  group_by(track_and_artist) %>% 
  mutate(cumulative_minutes_listened = cumsum(total_minutes_listened)) %>%
  ggplot(aes(x = date_listened, y = cumulative_minutes_listened)) +
  geom_line(color = SPOTIFY_COLORS['black'], size = 1.1) + 
  scale_x_date(labels = scales::date_format('%m-%Y'),
               limits = as.Date(c('2019-11-15', '2020-12-15'))) +
  facet_wrap(~track_and_artist) +
  theme_cowplot() +
  labs(title = 'Top Songs Listens by Date Dec 2019 - Dec 2020',
       x = '\n Date', 
       y = paste0(str_to_lab('cumulative_minutes_listened \n')))

# Middling songs listens by date 
songs_with_repeat_listens <- dat %>% 
  mutate(track_and_artist = paste0(track_name, ' [', artist_name, ']')) %>% 
  mutate(date_listened = lubridate::date(end_time)) %>% 
  group_by(track_and_artist) %>% 
  count() %>%
  filter(n >= 20) %>%
  ungroup()

repeat_listens_sample <- songs_with_repeat_listens %>% 
  filter(track_and_artist %notin% top_songs_names) %>%
  slice_sample(n = 6)

song_names_with_repeat_listens <- repeat_listens_sample %>% 
  select(track_and_artist) %>%
  pull()

median_listening_time <- songs_listen_time %>% 
  select(total_hours_listened) %>%
  pull() %>%
  median()

songs_listen_time %>% 
  filter(round(total_hours_listened, 3) == round(median_listening_time, 3))

middling_songs_names <- songs_listen_time %>%
  filter(round(total_hours_listened, 3) == round(median_listening_time, 3)) %>%
  ungroup() %>%
  arrange(desc(total_hours_listened)) %>%
  mutate(rank = row_number()) %>% 
  top_n(6, total_hours_listened) %>% 
  select(track_and_artist) %>% 
  pull()

dat %>% 
  mutate(track_and_artist = paste0(track_name, ' [', artist_name, ']')) %>% 
  filter(track_and_artist %in% song_names_with_repeat_listens) %>% 
  mutate(date_listened = lubridate::date(end_time)) %>% 
  group_by(track_and_artist, date_listened) %>% 
  summarize(total_minutes_listened = sum(ms_played) / (1000*60)) %>%
  group_by(track_and_artist) %>% 
  mutate(cumulative_minutes_listened = cumsum(total_minutes_listened)) %>%
  ggplot(aes(x = date_listened, y = cumulative_minutes_listened)) +
  geom_line(color = SPOTIFY_COLORS['black'], size = 1.1) + 
  scale_x_date(labels = scales::date_format('%m-%Y'),
               limits = as.Date(c('2019-11-15', '2020-12-15'))) +
  facet_wrap(~track_and_artist) +
  theme_cowplot() +
  labs(title = 'Top Songs Listens by Date Dec 2019 - Dec 2020',
       x = '\n Date', 
       y = paste0(str_to_lab('cumulative_minutes_listened \n')))

  