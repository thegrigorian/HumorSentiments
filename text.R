

#Loading the rvest package
library(rvest)
library(tidytext)
library(dplyr)
library(textreadr)
library(tidyverse)
###########################
##### scraping



### getting all the monolugue links from 4 pages into the data frame titles
titles <- data.frame()
textall <- {}
for (j in (0:3))
{url <- paste0('https://www.icomedytv.com/comedy-scripts/funny/humorous/comedy-monologues?page=',j)
webpage <- read_html(url)
title_data_html <- html_nodes(webpage,'#block-system-main .view-content a')
title_data <- html_text(title_data_html)
title_data <- tolower(title_data)
title_data <- str_replace_all(title_data, "[[:punct:]]", "")
title_data <- str_replace_all(title_data,"\\s+","-")
title_data <- paste0('https://www.icomedytv.com/comedy-scripts/funny/humorous/comedy-monologues/',title_data)
title_data <- data.frame(title_data)
titles <- rbind(titles, title_data)
}
textall <- {}

for (i in (1:182)) {

  urlmon <- toString(titles[i,1])
  webpage <- read_html(urlmon)
  text_html <- html_nodes(webpage,'.field-body p')
  text <- toString(html_text(text_html))
  textall[i] <- tibble(text)
  print(textall)
  print(i)
}







# first scraping the monologue titles from the first page
# Specifying the url for desired website to be scraped
url <- 'https://www.icomedytv.com/comedy-scripts/funny/humorous/comedy-monologues'
webpage <- read_html(url)
title_data_html <- html_nodes(webpage,'#block-system-main .view-content a')
title_data <- html_text(title_data_html)
title_data <- tolower(title_data)
title_data <- str_replace_all(title_data, "[[:punct:]]", "")
title_data <- str_replace_all(title_data,"\\s+","-")
title_data <- paste0('https://www.icomedytv.com/comedy-scripts/funny/humorous/comedy-monologues/',title_data)
title_data[i]
textall <- {}

for (i in (1:50)) {

  urlmon <- title_data[i]
  webpage <- read_html(urlmon)
  text_html <- html_nodes(webpage,'.field-body p')
  text <- toString(html_text(text_html))
  textall[i] <- tibble(text)
  print(textall)
  print(i)
}


####### text mining


text <- textall %>%
  data_frame(text = .)

text <- text %>%
  unnest_tokens(word, text)

text

# loading the stop words
data(stop_words)

text <- text %>%
  anti_join(stop_words)

text %>%
  count(word, sort = TRUE)



library(ggplot2)

text %>%
  count(word, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

get_sentiments("afinn")
get_sentiments("nrc")
get_sentiments("bing")



nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

text %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


library(tidyr)

text %>%
  inner_join(get_sentiments("bing")) %>%
   count(word, sort = TRUE, sentiment) %>%
   spread(sentiment, n, fill = 0) %>%
   mutate(sentiment = positive - negative)


text %>%
  inner_join(get_sentiments("afinn")) %>%
#  group_by(word) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")


bing_and_nrc <- bind_rows(text %>%
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          text %>%
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentiment %in% c("positive",
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_and_nrc




bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")




