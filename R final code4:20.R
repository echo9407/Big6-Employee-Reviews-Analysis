#PART 1: Data Cleaning
df = read.csv('employee_reviews.csv', stringsAsFactors = FALSE)
str(df)
library(dplyr)

# Transform dates column into datetype date
df$dates = as.Date(df$dates, format = " %b %d, %Y")

# Transform company column into factor type
df$company = as.factor(df$company)

# Transform ratings columns into numeric, replace "none" by NA
colSums(df[9:15]=="none")
df$work.balance.stars[df$work.balance.stars == "none"] <- NA
df$culture.values.stars[df$culture.values.stars == "none"] <- NA
df$carrer.opportunities.stars[df$carrer.opportunities.stars == "none"] <- NA
df$comp.benefit.stars[df$comp.benefit.stars == "none"] <- NA
df$senior.mangemnet.stars[df$senior.mangemnet.stars == "none"] <- NA
df[,c(9:15)] <- sapply(df[,c(9:15)], as.numeric)

# Split the job.title column into status and position, and drop job.title column

split = strsplit(as.character(df$job.title),' - ')
df$status = sapply(split, "[", 1)
df$position = sapply(split, "[", 2)
df = df[,-5]
head(df$status)
head(df$position)

# Check the location column, it is very clean!
summary(as.data.frame(table(df$location)))

# Check missing values, fill with 'none' for character-type variables
# Will keep missing values in numeric variables(ratings) for further exploration. 

colSums(is.na(df))
df$summary[is.na(df$summary)] = "none"
df$advice.to.mgmt[is.na(df$advice.to.mgmt)] = "none"
df$dates[is.na(df$dates)] = "9999-01-01"   # random dates

# Export Data
write.csv(df, file = "clean_employee_reviews.csv",row.names=FALSE)



#PART 2:Exploratory Data Analysis

#read clean dataset
df = read.csv('clean_employee_reviews.csv')

as.data.frame(table(df$company))   # there are six company in this dataset

#convert data types
df$summary <- as.character(df$summary)
df$cons <- as.character(df$cons)
df$pros <- as.character(df$pros)

# separate tables according to companies
df_gg = df[df$company =='google',]
df_amz = df[df$company =='amazon',]
df_apl = df[df$company =='apple',]
df_fb = df[df$company =='facebook',]
df_ms = df[df$company =='microsoft',]
df_nf = df[df$company =='netflix',]


library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)

# overall rating comparison (mean)
p1 <- df %>%
  group_by(company) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=company, y= avg_rating, fill =company)) + geom_bar(stat="identity")+coord_flip()+labs(x = "", y = "", title = "Average Overall Rating")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

# work balance comparison (mean)
p2 <- df %>%
  filter(!is.na(work.balance.stars)) %>% #remove missing values
  group_by(company) %>%
  summarize(avg_wbs = mean(work.balance.stars)) %>%
  ggplot(aes(x=company, y= avg_wbs, fill = company)) + geom_bar(stat="identity")+coord_flip()+labs(x = "", y = "", title = "Average Work Balance Rating")+geom_text(aes(y = avg_wbs/2, label = round(avg_wbs,2)))


# culture values comparison (mean)
p3 <- df %>%
  group_by(company) %>%
  filter(!is.na(culture.values.stars)) %>%  #remove missing values
  summarize(avg_cvs = mean(culture.values.stars)) %>%
  ggplot(aes(x=company, y= avg_cvs, fill = company)) + geom_bar(stat="identity")+coord_flip()+labs(x = "", y = "", title = "Average Culture Values Rating")+geom_text(aes(y = avg_cvs/2, label = round(avg_cvs,2)))


# career opportunities comparison (mean)
 p4 <- df %>%
  group_by(company) %>%
  filter(!is.na(carrer.opportunities.stars)) %>% #remove missing values
  summarize(avg_cos = mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x=company, y= avg_cos, fill = company)) + geom_bar(stat="identity")+coord_flip()+labs(x = "", y = "", title = "Average Carrer Opportunities Rating")+geom_text(aes(y = avg_cos/2, label = round(avg_cos,2)))


# company benefits comparison (mean)
p5<- df %>%
  group_by(company) %>%
  filter(!is.na(comp.benefit.stars)) %>% #remove missing values
  summarize(avg_cbs = mean(comp.benefit.stars)) %>%
  ggplot(aes(x=company, y= avg_cbs, fill = company)) + geom_bar(stat="identity")+coord_flip()+ labs(x = "", y = "", title = "Average Company Benefit Rating")+geom_text(aes(y = avg_cbs/2, label = round(avg_cbs,2)))


# senior management comparison (mean)
p6<- df %>%
  group_by(company) %>%
  filter(!is.na(senior.mangemnet.stars)) %>% #remove missing values
  summarize(avg_sms = mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x=company, y= avg_sms, fill = company)) + geom_bar(stat="identity")+coord_flip()+ labs(x = "", y = "", title = "Average Senior Managment Rating")+geom_text(aes(y = avg_sms/2, label = round(avg_sms,2)))

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

# boxplot to compare employee's opion about the 6 aspects by each company

p7 <- ggplot(df, aes(x = company, y = overall.ratings)) +
  geom_boxplot( fill = "blue2", alpha = 0.7)+coord_flip()
p8 <- ggplot(df, aes(x = company, y = work.balance.stars)) +
  geom_boxplot( fill = "green4", alpha = 0.7)+coord_flip()
p9 <-ggplot(df, aes(x = company, y = culture.values.stars)) +
  geom_boxplot(fill = "gold2", alpha = 0.7)+coord_flip()
p10 <-ggplot(df, aes(x = company, y = carrer.opportunities.stars)) +
  geom_boxplot(fill = "slategray4", alpha = 0.7)+coord_flip()
p11 <-ggplot(df, aes(x = company, y = comp.benefit.stars)) +
  geom_boxplot(fill = "purple4", alpha = 0.7)+coord_flip()
p12 <-ggplot(df, aes(x = company, y = senior.mangemnet.stars)) +
  geom_boxplot(fill = "red4", alpha = 0.7)+coord_flip()

grid.arrange(p7, p8, p9, p10, p11, p12, ncol = 3)


# Correlation plot of ratings

library(corrplot)
df1 <- na.omit(df)
corrplot(cor(df1[,9:14]),method='color',type='upper',addCoef.col = "gray8",diag = T)


#PART 3: TEXT MINING

#PART 3.1: Text Mining: Word Frequency

#on columns 'pros' and 'cons' reviews

library(stringr)

#calculate average number of characters in Pros for each company

p13 <- df %>%
  group_by(company) %>%
  summarize(avg_char_pros = mean(nchar(pros))) %>%
  ggplot(aes(x=company, y= avg_char_pros, fill = company)) + geom_bar(stat="identity")+coord_flip()+ labs(x = "", y = "", title = "Average Number of Characters in Pros")+geom_text(aes( y = avg_char_pros/2,label = round(avg_char_pros,0)))

#calculate average number of characters in Cons for each company

p14 <- df %>%
  group_by(company) %>%
  summarize(avg_char_cons = mean(nchar(cons))) %>%
  ggplot(aes(x=company, y= avg_char_cons, fill = company)) + geom_bar(stat="identity")+coord_flip()+ labs(x = "", y = "", title = "Average Number of Characters in Cons") +geom_text(aes(y = avg_char_cons/2, label = round(avg_char_cons,0)))

grid.arrange(p13,p14, ncol=1)

#Inspect review length (pros & cons) and ratings: are longer reviews associated with higher overall ratings?

cor(nchar(df$pros),df$overall.ratings) #0.0436
cor(nchar(df$cons),df$overall.ratings) #-0.294


#Most commmon words people used to describe their company: Which words are used most frequently in the reviews for each company?

library(qdap)

#Google TOP 10 frequent words 

#First use the tm package's traditional English stop words, then add other unvaluable stopwords from the result.

gg_pros_words <- as.data.frame(freq_terms(text.var=df_gg$pros,top=100,stopwords =c(tm::stopwords("english"), "work","google","company","job","can","get","lot","working")))
gg_cons_words <- as.data.frame(freq_terms(text.var=df_gg$cons,top=100,stopwords =c (tm::stopwords("english"),"work","google","company","can","get","much","really","lot","good","sometimes","will","like","job","working")))

p15 <- gg_pros_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "forestgreen")+ labs(x = "Count", y = "Word", title = "Google: Pros")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))
p16 <-gg_cons_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+FREQ),y=FREQ))+geom_bar(stat="identity",fill = "firebrick")+ labs(x = "Count", y = "Word", title = "Google: Cons")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

grid.arrange(p15,p16, ncol=1)

#Amazon TOP 10 frequent words 

amz_pros_words <- as.data.frame(freq_terms(text.var=df_amz$pros,top=100,stopwords = c(tm::stopwords("english"), "work","amazon","company","job","can","get","lot","working")))
amz_cons_words <- as.data.frame(freq_terms(text.var=df_amz$cons,top=100,stopwords = c(tm::stopwords("english"), "work","amazon","company","can","get","much","really","lot","good","sometimes","will","like","job","working")))

p17 <- amz_pros_words %>% slice(1:10) %>% ggplot( aes(x=reorder(WORD,+FREQ),y=FREQ))+geom_bar(stat="identity",fill = "forestgreen")+ labs(x = "Count", y = "Word", title = "Amazon: Pros")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))
p18 <- amz_cons_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+FREQ),y=FREQ))+geom_bar(stat="identity",fill = "firebrick")+ labs(x = "Count", y = "Word", title = "Amazon: Cons")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

grid.arrange(p17,p18, ncol=1)

#Apple TOP 10 frequent words 

apl_pros_words <- as.data.frame(freq_terms(text.var=df_apl$pros,top=100,stopwords =c(tm::stopwords("english"), "work","apple","company","job","can","get","lot","working")))
apl_cons_words <- as.data.frame(freq_terms(text.var=df_apl$cons,top=100,stopwords = c(tm::stopwords("english"), "work","apple","company","can","get","much","really","lot","good","sometimes","will","like","job","working")))

p19 <- apl_pros_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+FREQ),y=FREQ))+geom_bar(stat="identity",fill = "forestgreen")+ labs(x = "Count", y = "Word", title = "Apple: Pros")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))
p20 <- apl_cons_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+FREQ),y=FREQ))+geom_bar(stat="identity",fill = "firebrick")+ labs(x = "Count", y = "Word", title = "Apple: Cons")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

grid.arrange(p19,p20, ncol=1)

#FACEBOOK TOP 10 frequent words 

fb_pros_words <- as.data.frame(freq_terms(text.var=df_fb$pros,top=100,stopwords = c(tm::stopwords("english"), "work","facebook","company","job","can","get","lot","working","really")))
fb_cons_words <- as.data.frame(freq_terms(text.var=df_fb$cons,top=100,stopwords = c(tm::stopwords("english"), "work","facebook","company","can","get","much","really","lot","good","sometimes","will","like","job","working")))

p21 <-fb_pros_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "forestgreen")+ labs(x = "Count", y = "Word", title = "FACEBOOK: Pros")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

p22 <-fb_cons_words %>% slice(1:10) %>%ggplot( aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "firebrick")+ labs(x = "Count", y = "Word", title = "FACEBOOK: Cons")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

grid.arrange(p21,p22, ncol=1)

#MICROSOFT TOP 10 frequent words 

ms_pros_words <- as.data.frame(freq_terms(text.var=df_ms$pros,top=100,stopwords = c(tm::stopwords("english"), "work","microsoft","company","job","can","get","lot","working","really")))
ms_cons_words <- as.data.frame(freq_terms(text.var=df_ms$cons,top=100,stopwords = c(tm::stopwords("english"), "work","microsoft","company","can","get","much","really","lot","good","sometimes","will","like","job","working")))


p23 <- ms_pros_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "forestgreen")+ labs(x = "Count", y = "Word", title = "Microsoft: Pros")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

p24 <- ms_cons_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "firebrick")+ labs(x = "Count", y = "Word", title = "Microsoft: Cons")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

grid.arrange(p23,p24, ncol=1)


#NETFLIX TOP 10 frequent words 

nf_pros_words <- as.data.frame(freq_terms(text.var=df_nf$pros,top=100,stopwords = c(tm::stopwords("english"), "work","netflix","company","job","can","get","lot","working","really")))
nf_cons_words <- as.data.frame(freq_terms(text.var=df_nf$cons,top=100,stopwords = c(tm::stopwords("english"), "work","netflix","company","can","get","much","really","lot","good","sometimes","will","like","job","working")))

p25 <- nf_pros_words %>% slice(1:10) %>% ggplot(aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "forestgreen")+ labs(x = "Count", y = "Word", title = "Netflix: Pros")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

p26 <- nf_cons_words %>% slice(1:10) %>%ggplot(aes(x=reorder(WORD,+ FREQ), y=FREQ))+geom_bar(stat="identity",fill = "firebrick")+ labs(x = "Count", y = "Word", title = "Netflix: Cons")+coord_flip()+geom_text(aes(y = FREQ/2,label = round(FREQ,0)))

grid.arrange(p25,p26, ncol=1)

#WordcloudData to visulize the word frequency
library(wordcloud)
library(tidytext)

#Wordckoud for Google pros and cons

par(mfrow=c(1,2),mar=c(0,0,0,0))
wordcloud(gg_pros_words$WORD,gg_pros_words$FREQ,scale=c(2,0.5),random.order = FALSE,colors=c("limegreen","springgreen4","yellowgreen","seagreen3","green4"))
wordcloud(gg_cons_words$WORD,gg_cons_words$FREQ,scale=c(2,0.3),random.order = FALSE,colors=c("indianred1","indianred2","indianred3","indianred4","indianred"))

#Wordckoud for Amazon pros and cons

par(mfrow=c(1,2),mar = c(0, 0,0,0))
wordcloud(amz_pros_words$WORD,amz_pros_words$FREQ,scale=c(2,0.5),random.order = FALSE,colors=c("limegreen","springgreen4","yellowgreen","seagreen3","green4"))
wordcloud(amz_cons_words$WORD,amz_cons_words$FREQ,scale=c(2,0.2),random.order = FALSE,colors=c("indianred1","indianred2","indianred3","indianred4","indianred"))

#Wordckoud for Apple pros and cons

par(mfrow=c(1,2),mar=c(0,0,0,0))
wordcloud(apl_pros_words$WORD,apl_pros_words$FREQ,scale=c(2,0.5),random.order = FALSE, colors=c("limegreen","springgreen4","yellowgreen","seagreen3","green4"))
wordcloud(apl_cons_words$WORD,apl_cons_words$FREQ,scale=c(2,0.3),random.order = FALSE,colors=c("indianred1","indianred2","indianred3","indianred4","indianred"))

#Wordckoud for FACEBOOK pros and cons

par(mfrow=c(1,2),mai = c(0, 0, 0, 0))
wordcloud(fb_pros_words$WORD,fb_pros_words$FREQ,scale=c(2,0.4),random.order = FALSE,colors=c("limegreen","springgreen4","yellowgreen","seagreen3","green4"))
wordcloud(fb_cons_words$WORD,fb_cons_words$FREQ,scale=c(2,0.4),random.order = FALSE,colors=c("indianred1","indianred2","indianred3","indianred4","indianred"))

#Wordckoud for Microsoft pros and cons

par(mfrow=c(1,2),mai = c(0, 0, 0, 0))
wordcloud(ms_pros_words$WORD,ms_pros_words$FREQ,scale=c(2,0.5),random.order = FALSE,colors=c("limegreen","springgreen4","yellowgreen","seagreen3","green4"))
wordcloud(ms_cons_words$WORD,ms_cons_words$FREQ,scale=c(2,0.2),random.order = FALSE,colors=c("indianred1","indianred2","indianred3","indianred4","indianred"))

#Wordckoud for Netflix pros and cons

par(mfrow=c(1,2),oma = c(0, 0, 0, 0))
wordcloud(nf_pros_words$WORD,nf_pros_words$FREQ,scale=c(2,0.4),random.order = FALSE,colors=c("limegreen","springgreen4","yellowgreen","seagreen3","green4"))
wordcloud(nf_cons_words$WORD,nf_cons_words$FREQ,scale=c(2,0.3),random.order = FALSE,colors=c("indianred1","indianred2","indianred3","indianred4","indianred"))



#PART 3.2: Text Mining: Sentimental Analysis

#on column 'summary' to answer the question:

#What is the relationship between positive/negative words in summary and overall ratings?

df_gg%>%
  select(X,summary, overall.ratings) %>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment) 

#Fraction of Positive Words

#compute a fraction of positive words and all positive and negative words for each review and visualize in picture

#Google summary by sentiment proportion, break down by overall ratings

p27 <- df_gg %>%
  select(X,summary,overall.ratings)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall.ratings,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=overall.ratings, y=proportion, fill = sentiment))+geom_col()+labs(x = "Overall Ratings", y = "Proportion", title = "Google: summary by sentiment")

#Amazon summary by sentiment proportion, break down by overall ratings

p28 <- df_amz %>%
  select(X,summary,overall.ratings)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall.ratings,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=overall.ratings, y=proportion, fill = sentiment))+geom_col()+labs(x = "Overall Ratings", y = "Proportion", title = "Amazon: summary by sentiment")

#Apple summary by sentiment proportion, break down by overall ratings

p29 <- df_apl %>%
  select(X,summary,overall.ratings)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall.ratings,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=overall.ratings, y=proportion, fill = sentiment))+geom_col()+labs(x = "Overall Ratings", y = "Proportion", title = "Apple: summary by sentiment")

#FB summary by sentiment proportion, break down by overall ratings

p30 <- df_fb %>%
  select(X,summary,overall.ratings)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall.ratings,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=overall.ratings, y=proportion, fill = sentiment))+geom_col()+labs(x = "Overall Ratings", y = "Proportion", title = "FACEBOOK: summary by sentiment")

#Microsoft summary by sentiment proportion, break down by overall ratings

p31 <- df_ms %>%
  select(X,summary,overall.ratings)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall.ratings,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=overall.ratings, y=proportion, fill = sentiment))+geom_col()+labs(x = "Overall Ratings", y = "Proportion", title = "Microsoft: summary by sentiment")

#Netflix summary by sentiment proportion, break down by overall ratings

p32 <- df_nf %>%
  select(X,summary,overall.ratings)%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(overall.ratings,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x=overall.ratings, y=proportion, fill = sentiment))+geom_col()+labs(x = "Overall Ratings", y = "Proportion", title = "Netflix: summary by sentiment")

grid.arrange(p27, p28, p29, p30, p31, p32, ncol = 3)

#Correlation between positive words in summary and overall ratings

#From the picture we can see the proportion of positive words in summary is positively correlated to the overall ratings. Let's quantify this relationship with the whole dataset to answer the question:

#If summary with a lot of positive words are rated favorably?

df%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(X,overall.ratings)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,overall.ratings)) #0.441


#Word by Emotion(nrc lexicon): let's also inspect word by emotion and visualize the result

p33 <- df_gg%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+labs(x = "Word", y = "Count", title = "Google: Summary by Emotion")

p34 <- df_amz%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+labs(x = "Word", y = "Count", title = "Amazon: Summary by Emotion")

p35 <- df_apl%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+labs(x = "Word", y = "Count", title = "Apple: Summary by Emotion")

p36 <- df_fb%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+labs(x = "Word", y = "Count", title = "FACEBOOK: Summary by Emotion")

p37 <- df_ms%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+labs(x = "Word", y = "Count", title = "Microsoft: Summary by Emotion")

p38 <- df_nf%>%
  group_by(X)%>%
  unnest_tokens(output = word, input = summary)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+labs(x = "Word", y = "Count", title = "Netflix: Summary by Emotion")


grid.arrange(p33, p34, p35, p36, p37, p38, ncol = 2)


#Text Mining Part Summary: Positive and Negative Comparison Cloud for the whole dataset on summary to answer the question:

#What do employee like and dislike about a company in general?

wordcloudData = 
  df%>%
  group_by(X)%>%
  unnest_tokens(output=word,input=summary)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.4),max.words = 300, rot.per=0)


#PART 4: Time Series Analysis to answer the question:
#How did the ratings change over years for each company?

#PART 4.1: Do current employees review the companies higher than the former ones?

#Google: Average Overall Rating  by Status (current/former employee)
p39 <-df_gg %>%
  group_by(status) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=status, y= avg_rating, fill = status)) + geom_bar(stat="identity")+labs(x = "", y = "", title = "Google: Average Overall Rating  by Status")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

#Amazon: Average Overall Rating by Status
p40 <-df_amz %>%
  group_by(status) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=status, y= avg_rating, fill = status)) + geom_bar(stat="identity")+labs(x = "", y = "", title = "Amazon: Average Overall Rating by Status")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

#Apple: Average Overall Rating by Status
p41 <- df_apl %>%
  group_by(status) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=status, y= avg_rating, fill = status)) + geom_bar(stat="identity")+labs(x = "", y = "", title = "Apple: Average Overall Rating by Status")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

#Facebook: Average Overall Rating by Status
p42 <- df_fb %>%
  group_by(status) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=status, y= avg_rating, fill = status)) + geom_bar(stat="identity")+labs(x = "", y = "", title = "FACEBOOK: Average Overall Rating by Status")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

#Microsoft: Average Overall Rating by Status
p43 <- df_ms %>%
  group_by(status) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=status, y= avg_rating, fill = status)) + geom_bar(stat="identity")+labs(x = "", y = "", title = "Microsoft: Average Overall Rating by Status")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

#Netflix: Average Overall Rating by Status
p44 <- df_nf %>%
  group_by(status) %>%
  summarize(avg_rating = mean(overall.ratings)) %>%
  ggplot(aes(x=status, y= avg_rating, fill = status)) + geom_bar(stat="identity")+labs(x = "", y = "", title = "Netflix: Average Overall Rating by Status")+geom_text(aes(y = avg_rating/2, label = round(avg_rating,2)))

grid.arrange(p39, p40, p41, p42, p43, p44, ncol = 2)





#PART 4.2: Time Series: ratings trend over years for each company


library(lubridate)

#GOOGLE OVER TIME:

#Google: Average Overall Ratings
p45 <- df_gg %>%
  select(dates,overall.ratings) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(overall.ratings)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "green4", size = 2)+labs(x="",y="",title ="Google: Average Overall Ratings" )

#Google: Average Work Balance Stars
p46 <- df_gg %>%
  select(dates,work.balance.stars) %>%
  filter(!is.na(work.balance.stars)) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(work.balance.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "green4", size = 2)+labs(x="",y="",title ="Google: Average Work Balance Stars" )


#Google: Average Culture Values Stars
p47<-df_gg %>%
  select(dates,culture.values.stars) %>%
  filter(!is.na(culture.values.stars)) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(culture.values.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "green4", size = 2)+labs(x="",y="",title ="Google: Average Culture Values Stars" )


#Google: Average Carrer Opportunities Stars
p48 <-df_gg %>%
  select(dates,carrer.opportunities.stars) %>%
  filter(!is.na(carrer.opportunities.stars)) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "green4", size = 2)+labs(x="",y="",title ="Google: Average Carrer Opportunities Stars" )

#Google: Average Company Benifit Stars
p49 <-df_gg %>%
  select(dates,comp.benefit.stars) %>%
  filter(!is.na(comp.benefit.stars)) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(comp.benefit.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "green4", size = 2)+labs(x="",y="",title ="Google: Average Company Benifit Stars" )

#Google: Senior Management Stars
p50 <-df_gg %>%
  select(dates,senior.mangemnet.stars) %>%
  filter(!is.na(senior.mangemnet.stars)) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "green4", size = 2)+labs(x="",y="",title ="Google: Senior Management Stars" )

grid.arrange(p45, p46, p47, p48, p49, p50, ncol = 2)

#AMAZON OVER TIME:

#Amazon: Average Overall Ratings

p51 <- df_amz %>%
  select(dates,overall.ratings) %>%
  filter(!is.na(overall.ratings)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>% #to remove outliers "9999-01-01"
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(overall.ratings)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "gold2", size = 2)+labs(x="",y="",title ="Amazon: Average Overall Ratings" )

#Amazon: Average Work Balance Stars
p52 <- df_amz %>%
  select(dates,work.balance.stars) %>%
  filter(!is.na(work.balance.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(work.balance.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "gold2", size = 2)+labs(x="",y="",title ="Amazon: Average Work Balance Stars" )


#Amazon: Average Culture Values Stars
p53<-df_amz %>%
  select(dates,culture.values.stars) %>%
  filter(!is.na(culture.values.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(culture.values.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "gold2", size = 2)+labs(x="",y="",title ="Amazon: Average Culture Values Stars" )


#Amazon: Average Carrer Opportunities Stars
p54 <-df_amz %>%
  select(dates,carrer.opportunities.stars) %>%
  filter(!is.na(carrer.opportunities.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "gold2", size = 2)+labs(x="",y="",title ="Amazon: Average Carrer Opportunities Stars" )

#Amazon: Average Company Benifit Stars
p55 <-df_amz %>%
  select(dates,comp.benefit.stars) %>%
  filter(!is.na(comp.benefit.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(comp.benefit.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "gold2", size = 2)+labs(x="",y="",title ="Amazon: Average Company Benifit Stars" )

#Amazon: Senior Management Stars
p56 <-df_amz %>%
  select(dates,senior.mangemnet.stars) %>%
  filter(!is.na(senior.mangemnet.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "gold2", size = 2)+labs(x="",y="",title ="Amazon: Senior Management Stars" )

grid.arrange(p51, p52, p53, p54, p55, p56, ncol = 2)


#APPLE OVER TIME:

#Apple: Average Overall Ratings

p57 <- df_apl %>%
  select(dates,overall.ratings) %>%
  filter(!is.na(overall.ratings)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>% #to remove outliers "9999-01-01"
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(overall.ratings)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "slategray4", size = 2)+labs(x="",y="",title ="Apple: Average Overall Ratings" )

#Apple: Average Work Balance Stars
p58 <- df_apl %>%
  select(dates,work.balance.stars) %>%
  filter(!is.na(work.balance.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(work.balance.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "slategray4", size = 2)+labs(x="",y="",title ="Apple: Average Work Balance Stars" )


#Apple: Average Culture Values Stars
p59<-df_apl %>%
  select(dates,culture.values.stars) %>%
  filter(!is.na(culture.values.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(culture.values.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "slategray4", size = 2)+labs(x="",y="",title ="Apple: Average Culture Values Stars" )


#Apple: Average Carrer Opportunities Stars
p60 <-df_apl %>%
  select(dates,carrer.opportunities.stars) %>%
  filter(!is.na(carrer.opportunities.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "slategray4", size = 2)+labs(x="",y="",title ="Apple: Average Carrer Opportunities Stars" )

#Apple: Average Company Benifit Stars
p61 <-df_apl %>%
  select(dates,comp.benefit.stars) %>%
  filter(!is.na(comp.benefit.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(comp.benefit.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "slategray4", size = 2)+labs(x="",y="",title ="Apple: Average Company Benifit Stars" )

#Apple: Senior Management Stars
p62 <-df_apl %>%
  select(dates,senior.mangemnet.stars) %>%
  filter(!is.na(senior.mangemnet.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "slategray4", size = 2)+labs(x="",y="",title ="Apple: Senior Management Stars" )

grid.arrange(p57, p58, p59, p60, p61, p62, ncol = 2)


#FACEBOOK OVER TIME:

#FB: Average Overall Ratings

p63 <- df_fb %>%
  select(dates,overall.ratings) %>%
  filter(!is.na(overall.ratings)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>% #to remove outliers "9999-01-01"
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(overall.ratings)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "blue2", size = 2)+labs(x="",y="",title ="FACEBOOK: Average Overall Ratings" )

#FB: Average Work Balance Stars
p64 <- df_fb %>%
  select(dates,work.balance.stars) %>%
  filter(!is.na(work.balance.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(work.balance.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "blue2", size = 2)+labs(x="",y="",title ="FACEBOOK: Average Work Balance Stars" )


#FB: Average Culture Values Stars
p65<-df_fb %>%
  select(dates,culture.values.stars) %>%
  filter(!is.na(culture.values.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(culture.values.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "blue2", size = 2)+labs(x="",y="",title ="FACEBOOK: Average Culture Values Stars" )


#FB: Average Carrer Opportunities Stars
p66 <-df_fb %>%
  select(dates,carrer.opportunities.stars) %>%
  filter(!is.na(carrer.opportunities.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "blue2", size = 2)+labs(x="",y="",title ="FACEBOOK: Average Carrer Opportunities Stars" )

#FB: Average Company Benifit Stars
p67 <-df_fb %>%
  select(dates,comp.benefit.stars) %>%
  filter(!is.na(comp.benefit.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(comp.benefit.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "blue2", size = 2)+labs(x="",y="",title ="FACEBOOK: Average Company Benifit Stars" )

#FB: Senior Management Stars
p68 <-df_fb %>%
  select(dates,senior.mangemnet.stars) %>%
  filter(!is.na(senior.mangemnet.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "blue2", size = 2)+labs(x="",y="",title ="FACEBOOK: Senior Management Stars" )

grid.arrange(p63, p64, p65, p66, p67, p68, ncol = 2)



#MICROSOFT OVER TIME:

#MS: Average Overall Ratings

p69 <- df_ms %>%
  select(dates,overall.ratings) %>%
  filter(!is.na(overall.ratings)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>% #to remove outliers "9999-01-01"
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(overall.ratings)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "purple4", size = 2)+labs(x="",y="",title ="Microsoft: Average Overall Ratings" )

#MS: Average Work Balance Stars
p70 <- df_ms %>%
  select(dates,work.balance.stars) %>%
  filter(!is.na(work.balance.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(work.balance.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "purple4", size = 2)+labs(x="",y="",title ="Microsoft: Average Work Balance Stars" )


#MS: Average Culture Values Stars
p71<-df_ms %>%
  select(dates,culture.values.stars) %>%
  filter(!is.na(culture.values.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(culture.values.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "purple4", size = 2)+labs(x="",y="",title ="Microsoft: Average Culture Values Stars" )


#MS: Average Carrer Opportunities Stars
p72 <-df_ms %>%
  select(dates,carrer.opportunities.stars) %>%
  filter(!is.na(carrer.opportunities.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "purple4", size = 2)+labs(x="",y="",title ="Microsoft: Average Carrer Opportunities Stars" )

#MS: Average Company Benifit Stars
p73 <-df_ms %>%
  select(dates,comp.benefit.stars) %>%
  filter(!is.na(comp.benefit.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(comp.benefit.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "purple4", size = 2)+labs(x="",y="",title ="Microsoft: Average Company Benifit Stars" )

#MS: Senior Management Stars
p74 <-df_ms %>%
  select(dates,senior.mangemnet.stars) %>%
  filter(!is.na(senior.mangemnet.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "purple4", size = 2)+labs(x="",y="",title ="Microsoft: Senior Management Stars" )

grid.arrange(p69, p70, p71, p72, p73, p74, ncol = 2)



#NETFLIX OVER TIME:

#NF: Average Overall Ratings

p75 <- df_nf %>%
  select(dates,overall.ratings) %>%
  filter(!is.na(overall.ratings)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>% #to remove outliers "9999-01-01"
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(overall.ratings)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "red4", size = 2)+labs(x="",y="",title ="Netflix: Average Overall Ratings" )

#NF: Average Work Balance Stars
p76 <- df_nf %>%
  select(dates,work.balance.stars) %>%
  filter(!is.na(work.balance.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(work.balance.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "red4", size = 2)+labs(x="",y="",title ="Netflix: Average Work Balance Stars" )


#NF: Average Culture Values Stars
p77<-df_nf %>%
  select(dates,culture.values.stars) %>%
  filter(!is.na(culture.values.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average=mean(culture.values.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "red4", size = 2)+labs(x="",y="",title ="Netflix: Average Culture Values Stars" )


#NF: Average Carrer Opportunities Stars
p78 <-df_nf %>%
  select(dates,carrer.opportunities.stars) %>%
  filter(!is.na(carrer.opportunities.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(carrer.opportunities.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "red4", size = 2)+labs(x="",y="",title ="Netflix: Average Carrer Opportunities Stars" )

#NF: Average Company Benifit Stars
p79 <-df_nf %>%
  select(dates,comp.benefit.stars) %>%
  filter(!is.na(comp.benefit.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(comp.benefit.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "red4", size = 2)+labs(x="",y="",title ="Netflix: Average Company Benifit Stars" )

#NF: Senior Management Stars
p80 <-df_nf %>%
  select(dates,senior.mangemnet.stars) %>%
  filter(!is.na(senior.mangemnet.stars)) %>%
  filter(as.Date(dates) < as.Date("2019-12-31")) %>%
  group_by(year=floor_date(as.Date(dates), "year")) %>%
  summarise(average =mean(senior.mangemnet.stars)) %>%
  ggplot(aes(x = year, y = average))+geom_line(color = "red4", size = 2)+labs(x="",y="",title ="Netflix: Senior Management Stars" )

grid.arrange(p75, p76, p77,p78, p79, p80, ncol = 2)

#compare overall ratings over time by company
grid.arrange(p45, p51, p57,p63, p69, p75, ncol = 2)




