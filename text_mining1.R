 packages

library(tidyverse)
library(stringr)
library(tidytext)
library(xlsx)

## data load

#Abstract 포함, pubmed 양식
a <- readLines("raw_data/pubmed-cervicalca-set.txt", encoding="UTF-8")
a <-  a %>% as_tibble()

## csv data , abstract 미포함
b <- read_csv(file='raw_data/csv-cervicalca-set.csv',col_names = T)
b$PMID <- as.character(b$PMID)

#PMID location 
num <- a %>% mutate(number=c(1:nrow(a))) %>% 
                 filter(str_detect(value, "PMID-")) %>% select(number) 
num <- rbind(num, nrow(a))

# list -each paper 별로 분류
c <- list()
for (i in 1:143) {
    c1 <- a[num$number[i]:(num$number[i+1]-1),]
    c <- append(c, c1)
}

# Abstract extraction ->   csv data와 join  
ab <- c() 
for (i in 1:143) {
m <- c[i]$value %>% as_tibble() 
n0 <- grep("AB\\s*-", m$value)  
n1 <- grep("^[a-zA-Z]{1,}\\s*-", m$value)-n0
n1 <- n1 %>% as_tibble() %>% filter(value>0) %>% min
m1 <- paste(m[n0:(n0+n1-1),], sep=" ") 
m1 <- str_replace_all(m1, "\",|\"|^c|\\)|\\(|\n", "") %>% str_squish()
m2 <- c(str_sub(m$value[1], 7, 14), m1)
ab <- rbind(ab, m2)
}
ab <- ab %>% as_tibble()
names(ab) <- c("PMID", "Abstract")
ab_data <- b %>% inner_join(ab, by="PMID")

# selection from abstract & title
w <- ab_data %>% select(PMID, Title, Abstract)

for (i in 1:143) {
    w$Abstract[i] <- ifelse(str_detect(w$Abstract[i], "survival") & (str_detect(w$Abstract[i], "progression") | str_detect(w$Abstract[i], "disease")), 
                            w$Abstract[i], NA)  #progression free survival, disease free survival
    w$Title[i] <- ifelse(str_detect(w$Title[i], regex("meta", ignore_case = T))|str_detect(w$Title[i], regex("relapse", ignore_case = T))|str_detect(w$Title[i], regex("recur", ignore_case = T)), 
                         NA ,w$Title[i])  #pimary
}

w <- w %>% filter(complete.cases(Abstract)) %>% filter(complete.cases(Title))
ab_data <- ab_data %>% select(-Title, -Abstract) %>% inner_join(w, by="PMID")
write.xlsx(ab_data, file="dataset1.xlsx", sheetName="therapy",append=TRUE)
full <- ab_data %>% select(PMID, Abstract) %>% rename("ID"="PMID")

remove_words <- c("clinicaltrialsgov", "first posted", "nct","registered", "registration", "china",
                  "university", "department", "hospital", "patient", "patients", "affiliated", "affiliation", "sun", "center","centre", "trial","trials","status", 
                  "college", "center", "multicenter", "conclusion","conclusions", "endpoint","endpoints", "manuscript", "medicine",
                  "background", "authors", "randomized", "rate","rates","fudan", "guangdong","guangzhou", "guo","xia","eligible","phase","population",
                  "study","analyzd","analysis", "analyses","included","include","methods","method", "controlled","company","years","year","months","month","days","day",
                  "treatment","treatments","cycles","cycle", "arms", "arm", "division", "school", "copyright", "japan", "korea", "regimen", "openlabel", "control", "design",
                  "address", "email", "placebo", "permissions", "oxford", "rights", "society", "cell", "interimｓ", "medical", "total","health","evidence","boehringer",
                  "faculty","elizabeth","rct","weeks","week","women","disease","therapy","toxicity", "toxicities", "studies","differences","difference", "disease","life","risk",
                  "data","gynecology","gynecologic","obstetrics","federation", "addition", "findings", "time", "ratio","evaluate","levels", "size", "role", "aim", "fiveyear", 
                  "benefit", "events", "safety","impact", "threeyear","criteria","duration","node","nodes","baseline","period","type", "increase","stages","space","factor","factors",
                  "scores","quality","efficacy","effects","confidence","grade","adverse","dose","pone","terms","outcome","approach","diameter","seventeen","materials","improvement",
                  "combination","externalbeam","involvement","combinedtherapy","pfour","procedure","applications","sixthree","fouryear","iaone","participants","beam","range","andor",
                  "complications","allocation","field","morbidity","mortality","skin","earlystage","fivefour","trend","analyze","level","performance","age","term","incidence","failure",
                  "completion")
# packages
library(tm)
library(qdap)
library(SnowballC) # 어근 추출 stemDocument
library(RColorBrewer)
library(wordcloud)
library(stopwords) # 불용어 목록 smart, snowball...
library(udpipe) # cleanNLP -POS tag
library(tidygraph)
library(widyr)
library(ggraph)

##  clean data function
tm_clean <- function(corpus) {
corpus <- VCorpus(VectorSource(corpus))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, 
                      c(stopwords("en", source ="snowball"),
                        stopwords("en", source ="marimo"),
                        stopwords("en", source ="smart"),
                        stopwords("en", source ="nltk"),
                        stopwords("en", source ="stopwords-iso"),
                        remove_words
                         ))
corpus <- TermDocumentMatrix(corpus)
corpus <- as.matrix(corpus) 
corpus <- sort(rowSums(corpus), decreasing=T)
corpus <- tibble(word =names(corpus), freq =corpus)
return(corpus)
}


# english - udpipe ; POS tagging
text <- c()
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
for(i in 1:nrow(ab_data)) {
x <- udpipe_annotate(ud_model, x=tm_clean(ab_data$Title[i])$word) 
w <- x %>% as_tibble() %>% filter(upos=="NOUN" |upos=="ADJ" |upos=="VERB") %>% select(token, upos) 
w <- w %>% mutate(ID=rep(ab_data$PMID[i], nrow(w)))
text <- bind_rows(text, w)
}

# LDA 
text %>% rename("word"="token") %>% filter(upos =="NOUN") %>% select(ID, word) %>% add_count(word) %>% ggplot(aes(n))+geom_bar()

text_count <- text %>% rename("word"="token") %>% filter(upos =="NOUN") %>% select(ID, word) %>% add_count(word) %>% filter(n<25)
text_count %>% count(ID, word, sort=T) %>% print(n=200)
text_count <- text_count %>% mutate(word = recode(word,
                                                  "radiation"="radiotherapy",
                                                  "relapse"="recurrence",
                                                  "recurrences"="recurrence",
                                                  "irradiation"="radiotherapy",
                                                  "tumor"="cancer",
                                                  "tumors"="cancer",
                                                  "cancers"="cancer",
                                                  "carcinoma"="cancer",
                                                  "chemoradiation"="chemoradiotherapy",
                                                  "radiochemotherapy"="chemoradiotherapy",
                                                  "crt"="chemoradiotherapy",
                                                  "ccrt"="chemoradiotherapy",
                                                  "diseasefree"="dfs",
                                                  "progressionfree"="pfs",
                                                  "hysterectomy"="surgery",
                                                  "adenocarcinoma"="cancer",
                                                  "metstatatic"="metastasis",
                                                  "metastases"="metastasis"
                                                  ))
dtm <- text_count %>% cast_dtm(document=ID, term=word, value=n)
# optimizing topic number
library(ldatuning)
models <- FindTopicsNumber(dtm=dtm, topics=2:25, return_models = T, control = list(seed =1234))
FindTopicsNumber_plot(models)

library(topicmodels)
lda_model <- LDA(dtm, k=10, mehtod="Gibbs", control=list(seed =1234))
glimpse(lda_model)
term_topic <- tidy(lda_model, matrix="beta")
terms(lda_model, 20) %>% data.frame()
top_term_topic <- term_topic %>% 
    group_by(topic) %>%
    slice_max(beta, n=10)

library(scales)
ggplot(top_term_topic, aes(x=reorder_within(term, beta, topic), y=beta),
       fill = factor(topic)) +
    geom_col(show.legend = F) +
    facet_wrap(~ topic, scales = "free", ncol=4) +
    coord_flip()+
    scale_x_reordered() +
    scale_y_continuous(n.breaks = 4, labels = number_format(accuracy = 0.01)) +
    labs(x = NULL)

term_topic %>% filter(topic ==1) %>% arrange(-beta)
term_topic %>% group_by(topic) %>% slice_max(beta, n=10)

doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic %>% count(topic)

doc_class <- doc_topic %>% group_by(document) %>% slice_max(gamma, n=1)

# topic keywords
top_terms <- term_topic %>% group_by(topic) %>% slice_max(beta, n=6, with_ties = F) %>% summarise(term = paste(term, collapse=","))
new_topic <- ab_data %>% left_join(doc_class, by=c("PMID"="document")) %>% select(PMID, topic, Title) 
new_topic %>% filter(topic ==3) 
new_topic %>% count(topic) %>% left_join(top_terms, by ="topic") 



new_topic %>% filter(topic)

## 살펴보기
full %>% left_join(doc_class, by=c("ID"="document")) %>% filter(topic ==9) %>% head(50) %>% pull(Abstract)

