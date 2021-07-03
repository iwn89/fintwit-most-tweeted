library(tidyverse)
library(rtweet)
library(tau)
library(tm)
library(wordcloud2)
library(Rblpapi)
library(formattable)

#connect to bloomberg
blpConnect()

#function to collect tweets from a large number of users, waiting when limit on amount of twitter data downloaded is hit
tweet_extractor <- function(users){
  output_list <- list()
  for (i in 1:length(users)) {
    if(i %% 170 == 0) {
      rl = rate_limit('rate_limit')
      message("Waiting for ", round(rl$reset,1), " minugit config --global user.name "Your Name Here"tes for rate reset!")
      Sys.sleep(as.numeric(rl$reset, "secs"))
      }
    if(i %% 10 == 0) {message(i)}
    output_list[[i]] <- tryCatch(
      {get_timeline(users[i], n = 1000)[,c('user_id','created_at','screen_name','text','reply_to_status_id','is_quote','is_retweet')]},
      error = function(e){return(NULL)},
      warning = function(w){suppressWarnings(return(NULL))}
    )
  }
  output = bind_rows(output_list)
  return(output)
}

#define "fintwit " as those followed by billbrewsterscg
follows = get_friends("billbrewsterscg") %>% pull(user_id)

tweet_extractor(follows) -> df

#filter to the relevant period
df %>% filter(created_at >= '2021-06-01' & created_at < '2021-07-01') -> df

#store data
write_csv(df,"~/R/twitter_data/twitter_data_202106")

#optional read from csv if already created
read_csv("~/R/twitter_data/twitter_data_202106") -> df

#filter down to original posts (no replys or retweets)
df %>% filter(is.na(reply_to_status_id), !is_quote, !is_retweet) %>% select(screen_name,created_at,text) -> df

#pull cashtags from each tweet and touchup
df %>% 
  mutate(cashtags = str_extract_all(pull(df,text), regex(" ([$][a-zA-Z]+)"))) %>% 
  unnest(cols = c(cashtags)) %>% 
  mutate(cashtags = str_trim(cashtags),
         cashtags = toupper(cashtags)) %>% 
  unique() -> df

#standardize cashtags
df %>% 
  mutate(cashtags = if_else(cashtags %in% c('$BRKA','$BRKB'),'$BRK',
                            if_else(cashtags == '$GOOGL','$GOOG',cashtags))) -> df


#calculate adjusted times used and top user
df %>% 
  group_by(screen_name,cashtags) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  group_by(cashtags) %>% 
  filter(n == max(n)) %>% 
  rename(n_max_user = n) %>% 
  right_join(count(df,cashtags, sort = TRUE),by='cashtags') %>% 
  left_join(summarize(group_by(mutate(tally(group_by(df,screen_name,cashtags)),n= n^.4),cashtags),n_adj = sum(n)),by='cashtags') %>% 
  arrange(desc(n_adj)) %>% 
  nest(top_users = c(screen_name)) %>%
  unnest_wider(top_users) %>% 
  unnest_wider(screen_name) %>% 
  unite("top_users", ...1:...7, remove = FALSE, na.rm = TRUE,sep = ", ") %>% 
  rename(cashtag = cashtags,
         times_used_total = n_adj,
         times_used_by_top_users = n_max_user) %>% 
  select(cashtag, times_used_total, top_users, times_used_by_top_users) -> df

#combine with market cap data and adjust
df %>% 
  head(500) %>% 
  mutate(country = 'US') %>% 
  mutate(country = if_else(cashtag %in% c('$CSU','$TOI'),'CN',
                           if_else(cashtag == '$ADYEN','NA',
                                   if_else(cashtag == '$WINE','LN',
                                           if_else(cashtag == '$EVO','SS',country))))) %>% 
  mutate(ticker = paste(str_sub(cashtag,2,-1),country,'EQUITY')) -> df.temp

df.temp %>% 
  pull(ticker) %>% 
  bdp('CUR_MKT_CAP') %>% 
  rownames_to_column("ticker") %>% 
  right_join(df.temp,by='ticker') %>% 
  as_tibble() -> df

df %>% 
  rename(mkt_cap = CUR_MKT_CAP) %>% 
  #mutate(mkt_cap = if_else(is.na(mkt_cap),10e9,mkt_cap)) %>% 
  filter(!is.na(mkt_cap), !cashtag %in% c('$BTC','$ETH')) %>% 
  mutate(mkt_cap_adj = mkt_cap^.2/100) %>% 
  mutate(times_used_total = times_used_total/mkt_cap_adj) %>% 
  arrange(desc(times_used_total)) %>% 
  select(cashtag,times_used_total,top_users, times_used_by_top_users) -> df

#create table
df %>% 
  filter(!cashtag %in% c('$SPY')) %>% 
  ungroup() %>% 
  mutate(times_used_total = round(times_used_total,0),
         id = seq(1,nrow(.))) %>% 
  head(25) %>% 
  select(id,cashtag,times_used_total,top_users,times_used_by_top_users) %>% 
  formattable(align='c')
  


#create wordcloud
df %>% 
  select(cashtag,times_used_total) %>% 
  rename(word = cashtag,
         freq = times_used_total) %>% 
  head(100) %>% 
  wordcloud2(color='random-dark')


df %>% 
  filter(created_at >= '2021-01-01') %>% 
  pull(text) %>% 
  paste(collapse = ' ') -> text

text = gsub("\\.","",text)
text = gsub("\\,","",text)
text = gsub("\\?","",text)
text = gsub("\\!","",text)
text = gsub("\\n","",text)
text = gsub("\n\n","",text)
text = tolower(text)

words<-strsplit(text," ")

words<-table(unlist(words))

words = tibble(names = names(words),freq = c(words))

words %>% 
  filter(grepl("\\$", names)) %>% 
  filter(!grepl("\\-*\\d+\\.*\\d*", names)) %>% 
  arrange(desc(freq)) %>% 
  filter(names != "$") %>% 
  filter(freq >= 10) %>% 
  head(20)





