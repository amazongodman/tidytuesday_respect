library(ggthemes)
library(tidyverse)

netflix_titles <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv")

netflix_titles %>% 
  names()

netflix_titles %>% 
  head()

netflix_titles %>% 
  glimpse()

netflix_titles %>% 
  count(release_year) %>% 
  ggplot(aes(x=release_year,y=n))+
  geom_step()+
  theme_wsj()

netflix_titles$cast[1:3]

#install.packages("janitor")
library(janitor)

options(scipen=100)

netflix_titles %>% 
  mutate(spl = str_split(cast,", ")) %>% 
  unnest(spl) %>%
  tabyl(spl) %>% 
  arrange(desc(percent)) %>% 
  head(20)

#other‚É‚¢‚ê‚é‚Ì‚Í
#0.0004
#‚­‚ç‚¢‚ª‚æ‚³‚»‚¤

netflix_titles %>% 
  mutate(spl = str_split(director,", ")) %>% 
  unnest(spl) %>%
  tabyl(spl) %>% 
  arrange(desc(percent)) %>% 
  head(20)

#other‚É‚¢‚ê‚é‚Ì‚Í
#0.001

library(glue)

netflix_titles %>% 
  mutate(spl = str_split(cast,",")) %>% 
  unnest(spl) %>% 
  mutate(new_cast_col = fct_lump_prop(spl,0.0004,other_level = "low_freq")) %>% 
  filter(new_cast_col != "low_freq") %>% 
  count(new_cast_col) %>% 
  ggplot(aes(x=fct_reorder(new_cast_col,n),y=n,fill=new_cast_col))+
  geom_col()+
  labs(title="freq cast")+
  coord_flip()+
  xlab("cast name")+
  theme(legend.position = "None")+
  theme_wsj()


netflix_titles %>% 
  mutate(spl = str_split(director,",")) %>% 
  unnest(spl) %>% 
  mutate(new_director_col = fct_lump_prop(spl,0.001,other_level = "low_freq")) %>% 
  filter(new_director_col != "low_freq") %>% 
  count(new_director_col) %>% 
  ggplot(aes(x=fct_reorder(new_director_col,n),y=n,fill=new_director_col))+
  geom_col()+
  labs(title="freq director")+
  coord_flip()+
  xlab("director name")+
  theme(legend.position = "None")+
  theme_wsj()







library(tidytext)


main_cast <- netflix_titles %>% 
  mutate(spl = str_split(cast,",")) %>% 
  unnest(spl) %>% 
  mutate(new_cast_col = fct_lump_prop(spl,0.0004,other_level = "low_freq")) %>% 
  filter(new_cast_col != "low_freq") %>% 
  count(new_cast_col,sort = T) %>% 
  slice(1:5) %>% 
  pull(new_cast_col) %>% 
  unique()


cast_title <-function(x){
netflix_titles %>% 
  filter(str_detect(cast,as.character(main_cast[x]))) %>% 
  unnest_tokens(word,title) %>% 
  anti_join(stop_words) %>% 
  count(director,word) %>% 
  filter(n>1) %>% 
  ggplot(aes(x=word,y=n))+
  geom_col()+
  coord_flip()+
  labs(title=glue("[{ main_cast[x] }] played in title"))+
    xlab("TV or movie title")
}

cast_title(1)

cast_title(2)+
  theme_wsj()

cast_title(3)
cast_title(4)










main_dire <- netflix_titles %>% 
  mutate(spl = str_split(director,",")) %>% 
  unnest(spl) %>% 
  mutate(new_director_col = fct_lump_prop(spl,0.001,other_level = "low_freq")) %>% 
  filter(new_director_col != "low_freq") %>% 
  count(new_director_col,sort = T) %>% 
  slice(1:5) %>% 
  pull(new_director_col) %>% 
  unique()


netflix_titles %>% 
  filter(director %in% main_dire) %>% 
  unnest_tokens(word,title) %>% 
  anti_join(stop_words) %>% 
  count(director,word) %>% 
  filter(n>1) %>% 
  ggplot(aes(x=word,y=n))+
  geom_col()+
  facet_wrap(~director)+
  coord_flip()
  
  
  



library(lubridate)




netflix_titles %>% 
  mutate(a = mdy(date_added))


p1 <- netflix_titles %>% 
  separate(date_added,into = c("month","day","year"),remove = F) %>% 
#  select(date_added,month,day,year) %>% 
  mutate(a = parse_date_time(str_c(year,month,day,sep = "-"), orders = c("ybd"))) %>% 
  mutate(b = parse_date_time(str_c(year,month,"01",sep = "-"), orders = c("ybd"))) %>% 
  group_by(type,b) %>% 
  summarise(n=n(),.groups = "drop") %>% 
  filter(!is.na(b)) %>% 
  ggplot(aes(x=b,y=n,color=type))+
  geom_line()+
  theme(legend.position = "top")+
  ggtitle("Netflix date_added count")+
  theme_wsj()

#install.packages("gganimate")
library(gganimate)
library(glue)


#p1 + 
#  transition_states(b)

p1 + 
  transition_reveal(b)


anim_save("net_frex.gif")

#p1 + 
#  transition_time(b)

getwd()
  




