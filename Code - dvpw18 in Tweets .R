# DVPW 2018 in Tweets

library(ggplot2)
library(scales)
library(Cairo)
library(rtweet)
library(xtable)
library(xlsx)
library(tm)
library(data.table)
library(stringr)
library(dplyr)
library(tidytext)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stopwords)
library(topicmodels)
library(stm)
library(textstem)
library(stringr)


setwd("C:\\Users\\koenig\\Documents\\R\\dvpw18 in Tweets")
load("C:\\Users\\koenig\\Documents\\R\\twittertoken.RDa") 
load("dvpw_search_final.RDa")

##############################
## Combine Twitter Searches ##
##############################

# filenames <- list.files(pattern = "dvpw18_search*")
# 
# dvpw_search <- data.table()
# # 
# # for (i in 1:length(filenames)){
# #   load(filenames[i])
# #   search <- mget(ls(pattern="dvpw18_search"))
# #   searches <- list(dvpw_search, search)
# #   dvpw_search <- rbindlist(searches)
# #   rm(list = ls(search))
# #   rm(searches)
# # }
# # # Loop to combine dataframes (as data.tables for faster handling) 
# # #   -without keeping them all in the environment
# # #  -> not functional as of now
# 
# 
# for (i in 1:length(filenames)){
#   load(filenames[i])
# }
# searches <- mget(ls(pattern="dvpw18_search"))
# dvpw_search <- rbindlist(searches)
# rm(list = ls(searches))
# 
# dvpw_search <- unique(dvpw_search, fromLast = T, by="status_id")
# # remove duplicate tweets. identified by status id 
# # (otherwise differing rt/fav counts for the same treat identify a unique entry)
# # fromLast=T to keep highest retweet/favourite count (last search)
# 
# save(dvpw_search, file="dvpw_search_final.RDa")


##############################
# plot 12 busiest tweeps #
##############################
  # (original code by Felix Haass, tweaked)

df <- as.data.frame(table(dvpw_search$screen_name), stringsAsFactors = F)
  # 535 users total
df <- tail(df[order(df$Freq), ], 12)

# get real names
df <- data.frame(df, row.names = seq_along(1:12), stringsAsFactors = F)
for(i in 1:nrow(df)) {
  user <- lookup_users(df[i, "Var1"],  token=twitter_token)
  df[i, "realname"] <-   user$name
}


# create display
df$disp_name <- paste0(df$realname, " \n(@", df$Var1, ")")

# save image
CairoPNG("12_busiest_dvpw18_tweeps.png", height=1000, width = 1000, pointsize=30)
par(mar=c(5, 8, 4, 2))
barplot(df$Freq, 
        names.arg=df$disp_name,
        horiz=T, 
        las=1,
        main="Top 12 #dvpw* Tweeps",
        xlab="Tweets",
        xlim=c(0,600),
        space=0.6, 
        col="navy", 
        border = FALSE, cex.names = .6)

dev.off()



#######################
# Most Popular Tweets #
#######################
# (original code by Felix Haass, tweaked and expanded)

# add popularity count
dvpw_search$popularity <- (dvpw_search$favorite_count + dvpw_search$retweet_count)

# order & subset top 5 in RTs
ordered <- dvpw_search[order(dvpw_search$retweet_count, decreasing = TRUE), ]
top5retweets <- head(ordered[ordered$is_retweet == FALSE, ], 5)

# order & subset top 5 in likes
ordered <- dvpw_search[order(dvpw_search$favorite_count, decreasing = TRUE), ]
top5likes <- head(ordered[ordered$is_retweet == FALSE, ], 5)

# order & subset top 5 in RTs
ordered <- dvpw_search[order(dvpw_search$popularity, decreasing = TRUE), ]
top5popularity <- head(ordered[ordered$is_retweet == FALSE, ], 5)


# shorten data & generate full twitter link
top5retweets <- top5retweets[,c(1:5, 13)]
top5retweets$link <- paste0("https://twitter.com/", top5retweets$screen_name, "/status/", top5retweets$status_id)
top5likes <- top5likes[,c(1:5, 12)]
top5likes$link <- paste0("https://twitter.com/", top5likes$screen_name, "/status/", top5likes$status_id)


# get real names
top5retweets <- data.frame(top5retweets, row.names = seq_along(1:5), stringsAsFactors = F)
for(i in 1:nrow(top5retweets)) {
  user <- lookup_users(top5retweets[i, "screen_name"],  token=twitter_token)
  top5retweets[i, "realname"] <-   user$name
}

top5likes <- data.frame(top5likes, row.names = seq_along(1:5), stringsAsFactors = F)
for(i in 1:nrow(top5likes)) {
  user <- lookup_users(top5likes[i, "screen_name"],  token=twitter_token)
  top5likes[i, "realname"] <-   user$name
}


# create display
top5retweets$disp_name <- paste0(top5retweets$realname, " \n(@", top5retweets$screen_name, ")")
top5likes$disp_name <- paste0(top5likes$realname, " \n(@", top5likes$screen_name, ")")


# in xlsx
write.xlsx(top5retweets,file="top5retweets.xlsx")
write.xlsx(top5likes,file="top5likes.xlsx")


###################
# Volume Timeline #
###################
# (original code by Felix Haass, tweaked)

minDate <- min(dvpw_search$created_at)
maxDate <- max(dvpw_search$created_at)
dateBreaks <- seq(minDate, maxDate, by=60 * 60)
dateBreaks <- c(dateBreaks, maxDate + 60 * 60)
tweetCount <- hist(dvpw_search$created_at, breaks=dateBreaks, plot=FALSE)                             
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]

# prepare plot data
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count))

# save plot
CairoPNG("dvpw18_Twitter_trend.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(plotData) +
  geom_bar(aes(x=dates, y=tweets), stat="identity") +
  scale_y_continuous("Number of tweets") +
  scale_x_datetime(date_breaks="1 day") +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="DVPW Tweets over time \n") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-25 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-26 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-27 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-28 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-29 00:00:00 UTC"))), color = "firebrick") +
  geom_text(label = "Day 1", aes(x = as.POSIXct("2018-09-25 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Day 2", aes(x = as.POSIXct("2018-09-26 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Day 3", aes(x = as.POSIXct("2018-09-27 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Day 4", aes(x = as.POSIXct("2018-09-28 13:00:00 UTC"), y = 300), size = 2.5) 
  

dev.off()


###### Nur Konferenztage, inklusive #TeamTakeOver ######

minDate <- as.POSIXct("2018-09-24 00:00:00 UTC")
maxDate <- as.POSIXct("2018-09-30 00:00:00 UTC")
dateBreaks <- seq(minDate, maxDate, by=60 * 60)
dateBreaks <- c(dateBreaks, maxDate + 60 * 60)

conference <- subset(dvpw_search, created_at>minDate&created_at<maxDate, select=1:42)
tweetCount_conference <- hist(conference$created_at, breaks=dateBreaks, plot=FALSE)

no_takeover <- subset(conference, screen_name!="dvpw")
no_takeover <- subset(no_takeover, str_detect(no_takeover$text, "RT @dvpw:")==F)
tweetCount_notakeover <- hist(no_takeover$created_at, breaks=dateBreaks, plot=FALSE)
takeover <- subset(conference, screen_name=="dvpw")
tweetCount_takeover <- hist(takeover$created_at, breaks=dateBreaks, plot=FALSE)
takeoverRT <- subset(conference, str_detect(conference$text, "RT @dvpw:")==T)
tweetCount_takeoverRT <- hist(takeoverRT$created_at, breaks=dateBreaks, plot=FALSE)

binBreaks <- tweetCount_conference$breaks[1:length(tweetCount_conference$breaks)-1]


# prepare plot data
plotData_takeover <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], 
                                tweets=as.numeric(tweetCount_takeover$count))
plotData_takeoverRT <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], 
                                tweets=as.numeric(tweetCount_takeoverRT$count))
plotData_notakeover <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], 
                                tweets=as.numeric(tweetCount_notakeover$count))
plotData_conference <- bind_rows(plotData_takeover, plotData_takeoverRT, plotData_notakeover, .id = "takeover")
  # Plotdata: Takeover = 1: dvpw-Tweet; 2: dvpw-Retweet; 3: Restliche Tweets

# save plot
CairoPNG("dvpw18_Twitter_trend_TeamTakeOver.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(plotData_conference) +
  geom_bar(aes(x=dates, y=tweets, fill = takeover),  stat="identity")+
  scale_fill_manual(name="", values = c("#0072B2", "#56B4E9", "#999999"), labels=c("DVPW Tweets", "DVPW Retweets", "Restliche Tweets"))+
  scale_y_continuous("Anzahl Tweets") +
  scale_x_datetime(name="Datum", date_breaks="1 day") +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="Tweets des offiziellen DVPW Accounts und deren Retweets \n") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-25 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-26 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-27 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-28 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-09-29 00:00:00 UTC"))), color = "firebrick") +
  geom_text(label = "Tag 1", aes(x = as.POSIXct("2018-09-25 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Tag 2", aes(x = as.POSIXct("2018-09-26 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Tag 3", aes(x = as.POSIXct("2018-09-27 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Tag 4", aes(x = as.POSIXct("2018-09-28 13:00:00 UTC"), y = 300), size = 2.5) 


dev.off()



############
# Hashtags #
############

# hashtag <- table(tolower(unlist(dvpw_search$hashtags)))
# hashtag <- data.table(hashtag)
# hashtag_top <- hashtag[!str_detect(hashtag$V1,"dvpw"),]
# hashtag_top <- tail(hashtag_top[order(hashtag_top$N), ], 20)

hashtag_noRT <- subset(dvpw_search, is_retweet==F) 
hashtag_noRT <- table(tolower(unlist(hashtag_noRT$hashtags))) %>% as.data.table()
hashtag_noRT_top <- hashtag_noRT[!str_detect(hashtag_noRT$V1,"dvpw"),] 
hashtag_noRT_top <- tail(hashtag_noRT_top[order(hashtag_noRT_top$N), ], 20)

hashtag_top10 <- tail(hashtag_noRT_top[order(hashtag_noRT_top$N), ], 10)

# save image
  # (hashtags inklusive retweets, ohne offizielle dvpw-Tags)
CairoPNG("10_busiest_dvpw18_hashtags.png", height=1000, width = 1000, pointsize=30)
par(mar=c(5, 8, 4, 2))
barplot(hashtag_top10$N, 
        names.arg=hashtag_top10$V1,
        horiz=T, 
        las=1,
        main="10 beliebteste #dvwp* Hashtags",
        xlab="Anzahl Tweets",
        space=0.6, 
        col="navy", 
        border = FALSE, cex.names = .6)

dev.off()



#######################
# Language processing #
#######################


### Preparing the text data -------------------------------------------------

# Tidytext the data

# exclude retweets and unnest text (keep screen_name and status_id of the tweet)
#    keep status_id to use as document indicator, screen_name may be useful for further analysis
text_noRT <- subset(dvpw_search, is_retweet==F, select = c("text", "status_id", "screen_name")) %>% unnest_tokens(word, text)
text_noRT$text <- iconv(text_noRT$text, to="UTF-8")

# Stopwords
# general language-dependant stopwords
stopwords_de <- get_stopwords(language = "de")
stopwords_en <- get_stopwords(language = "en")
# application-dependant stopwords
more_stopwords <- tibble(word = c("dvpw18", "t.co", "https","amp", "dvpw","dvpwkongress", "dvpw2018", "dass", "beim", "mal"))

text_noRT <- text_noRT %>% anti_join(stopwords_de) %>% anti_join(stopwords_en) %>% anti_join(more_stopwords)


# Stemming and Lemmatizing
# utilize treetragger for lemma dictionary. Needs installation
# (http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/)

# Dictionaries for enlgish and german 
#install.koRpus.lang("de")
library(koRpus.lang.de)   # must install and load german language support as koRpus package
dictionary_de <- make_lemma_dictionary(text_noRT$word, engine = "treetagger", lang="de")
dictionary_en <- make_lemma_dictionary(text_noRT$word, engine = "treetagger", lang="en")

# lemmatize german words
text_noRT$lemmatized <- lemmatize_words(text_noRT$word, dictionary = dictionary_de)
# lemmatize english words not lemmatized before
text_noRT$lemmatized <- lemmatize_words(text_noRT$lemmatized, dictionary = dictionary_en)

# lemmatize by hand what's missing
# bundespr√§sidenten -> bundespr√§sident
text_noRT$lemmatized[text_noRT$word=="bundespr‰sidenten"] <-"bundespr‰sident"
text_noRT$lemmatized[text_noRT$word=="bundespr‰sident'en"] <-"bundespr‰sident"

# lower case
text_noRT$lemmatized <- tolower(text_noRT$lemmatized)


## Word count ----------------------------

# total
words <- count(text_noRT, lemmatized, sort = T)
top_words <- head(words, 20)

# per document (tweet)
word_count <- count(text_noRT, status_id, screen_name, lemmatized, sort = TRUE)


## Structural Topic Modelling -------------


## prepare Data

# Make Document-Term-Matrix
dtm <- cast_dtm(data=word_count, term=lemmatized, document=status_id, value=n)
# read in Corpus (dtm)
stm_data <- readCorpus(dtm, type="slam")

# Subset without retweets
noRT <- subset(dvpw_search, is_retweet==F) 

# add metadata
metadata <- subset(noRT, select = c("created_at", "hashtags", "status_id"))  # use subset without retweets to match text corpus

# unlist hashtags & set to lower case
metadata$hashtags <- vapply(metadata$hashtags, paste, collapse = " ", character(1L))
metadata$hashtags <- tolower(metadata$hashtags)

# remove #dvpw, #dvpw18 and #dvpw2018 hashtags (constitute the sample)
metadata$hashtags <- str_remove_all(metadata$hashtags, "dvpw2018")
metadata$hashtags <- str_remove_all(metadata$hashtags, "dvpw18")
metadata$hashtags <- str_remove_all(metadata$hashtags, "dvpw")
metadata$hashtags <- str_remove_all(metadata$hashtags, ",")

# set metadata format
colnames(metadata)[colnames(metadata)=="status_id"] <- "docname" 
metadata$hashtags <- as.factor(metadata$hashtags)

# dates as integer nr. of days
metadata$created_at <- as.Date(metadata$created_at)
startdate <- as.Date("2018-09-14")
metadata$day <-  difftime(metadata$created_at, startdate, units = "days") %>% as.integer()

# final data for stm
plotRemoved(stm_data$documents, lower.thresh = seq(1, 50, by=10))  # check words removed for threshold 
prep_stm <- prepDocuments(documents=stm_data$documents, vocab=stm_data$vocab, meta=metadata, lower.thresh = 1)
      # go with a safe 1 as threshold to keep the amount of words larger than 10% of the total corpus (might need revision in the future)


## topic modelling

# try to find appropraite number of topics
Ks <- searchK(documents = prep_stm$documents, vocab = prep_stm$vocab, K = c(5,10,15,20, 30, 50, 70, 100), gamma.prior = "L1", 
              prevalence =~ hashtags + s(day), data = prep_stm$meta, init.type = "Spectral")
plot(Ks)


# model
stm20 <- stm(documents = prep_stm$documents, vocab = prep_stm$vocab, K = 20, prevalence =~ hashtags + s(day), 
             data = prep_stm$meta, gamma.prior = "L1", init.type = "Spectral" )

plot(stm20, type="summary")

  #  20 topics look good statistically, but produce some somewhat nonensical results. also harder to read (too much input). go with 10 for readability and consistency

stm <- stm(documents = prep_stm$documents, vocab = prep_stm$vocab, K = 10, prevalence =~ hashtags + s(day), 
             data = prep_stm$meta, gamma.prior = "L1", init.type = "Spectral" )

# visualize (terms arranged by beta)
CairoPNG("dvpw-Topics.png", height=800, width = 1400, pointsize=30)
par(mar=c(5, 1, 2, 1))

plot(stm, type="summary", main = "Top Themen der #dvpw*", xlab = "Gesch‰tzte Proportionen")

dev.off()


# alternative Visualization (terms arranged by beta)
stm_td <- tidy(stm)
top_terms <- stm_td %>% group_by(topic) %>% top_n(5, beta) %>% ungroup() %>% arrange(topic, -beta)
stm_topics_10 <- top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 1))

# Find distinctive tweets for topics
  # in order to display these, we must:
  # remove documents that were removed during prepDocuments()
corpus_data <- noRT[-prep_stm$docs.removed,]
  # get the documents in the same order as in the smt() data (the order in the dtm is easier to access and the same as in the stm)
corpus_data <- corpus_data[order(match(corpus_data$status_id,dtm$dimnames$Docs)),]

findThoughts(stm, texts = corpus_data$text, topics = c(1:10), n = 10)   

labelTopics(stm)

