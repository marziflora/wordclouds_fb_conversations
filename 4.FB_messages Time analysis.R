
library(dplyr)
library(gt)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(ggpubr)
library(purrr)
library(readxl)
library(qdapRegex)
library(tibble)
library(httr)
library(rvest)
library(stringi)
library(textclean)
library(tidytext)
library(wordcloud)
library(tm)
library(janitor)
############################################################################
path <- paste0(getwd(), "/fb_messages/messages")
dirs <- list.dirs(path) %>%  as_tibble()
names <- c("") #name+surname of people to analyse

conversation_files <- names %>%  map_df( function(i) { 
  person_dirs <- dirs %>%  filter(str_detect(value, i))
  html_names <- person_dirs %>% map(function(j) {
    path_to_html <- list.files(j, pattern = ".html", full.names = TRUE) })
  data.frame(person = i, link = html_names)   })
################################################################################
content <- conversation_files$person %>%  unique() %>%  map(function(i) {
  print(paste("Extracting person:", i))
  content <- conversation_files %>%  filter(person==i) %>%  pull(value) %>% map_df(function(file_name) {
    ########################################
    content <- 
      read_html(file_name) %>% 
      html_nodes(xpath='//div[@class = "pam _3-95 _2pi0 _2lej uiBoxWhite noborder"]') %>% 
      html_text() %>%
      as.tibble()  

    return (content)  }) %>%  bind_rows()
  
  return (content)  
  })
######################################### Transformation:
transformed_data <- content %>% length() %>% seq() %>%   map( function(i) {
  print(i)
  transformed <- content[[i]] %>%
    separate(value,
             into = c("Speaker"),
             sep = " ", remove = FALSE, fill = "left", convert = TRUE) %>% 
    mutate(Date = str_extract(value, '[AaĄąBbCcĆćDdEeĘęFfGgHhIiJjKkLlŁłMmNnŃńOoÓóPpRrSsŚśTtUuWwYyZzŹźŻż]{1,3} [0-9]{1,2}, [0-9]{4}')) %>% 
    mutate(month= substr(Date, 0, 3),
           month = case_when(
             month == 'sty' ~ '01',
             month == 'lut' ~ '02',
             month == 'mar' ~ '03',
             month == 'kwi' ~ '04',
             month == 'maj' ~ '05',
             month == 'cze' ~ '06',
             month == 'lip' ~ '07',
             month == 'sie' ~ '08',
             month == 'wrz' ~ '09',
             month == 'paź' ~ '10',
             month == 'lis' ~ '11',
             month == 'gru' ~ '12'),
           year=str_extract(Date, '[0-9]{4}'),
           day=substr(str_trim(str_extract(Date, " [0-9]{1,2},")),0,2),
           Date = as.Date(paste0(day,"-", month, "-", year), format='%d-%m-%Y') %>% 
             
             str_replace_all(., '[AaĄąBbCcĆćDdEeĘęFfGgHhIiJjKkLlŁłMmNnŃńOoÓóPpRrSsŚśTtUuWwYyZzŹźŻż]{1,3} [0-9]{1,2}, [0-9]{4}.*', "") %>%    
             str_trim()) %>% 
    mutate(week_year = strftime(Date, format = "%Y-%V"),
           week = strftime(Date, format = "%V")) %>% 
    select(Date, Speaker, message, week_year, year, week)
  return (transformed)
})

############################################################ How many messages, per person
data_for_plots <-
transformed_data %>% length() %>% seq() %>%  map_df( function(i) {
  person <-  transformed_data[[i]]$Speaker %>%  unique()  %>%  setdiff("Marzena")
  grouped_data <- transformed_data[[i]] %>%
    group_by(Speaker, week, year) %>%  summarise(no_mess = n(), sum_length_mess = sum(str_length(message)))  %>% mutate(person=person)
  grouped_data
}) 
  

data_for_plots %>% 
  mutate(Speaker = if_else(Speaker=='Marzena', "Mine", "Person2")) %>% 
  filter( !(year==2022&week>30)) %>% 
    ggplot(aes(week, no_mess, fill=Speaker)) + geom_col(position='dodge') + theme_bw() + 
  facet_wrap(~paste(year), ncol=1) + #, scale='free_y') +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

######################

data_for_plots %>% 
  mutate(Speaker = if_else(Speaker=='Marzena', "Mine", "Person2")) %>% 
  filter( !(year==2022&week>30)) %>% 
  filter(year>2015, !is.na(week)) %>% 
  ggplot(aes(week, sum_length_mess, fill=Speaker)) + geom_col(position='dodge') + theme_bw() + 
  facet_wrap(~paste(year), ncol=1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
