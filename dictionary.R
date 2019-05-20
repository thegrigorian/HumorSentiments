library(plyr)
dictionary <- read.csv("rusentilex_2017.csv", header = FALSE)
dictionary <- dictionary[c(1,4)]
colnames(dictionary) <- c("word", "sentiment")
levels(dictionary$sentiment)

dictionary$sentiment <- revalue(dictionary$sentiment, 
                                c( " fact"= NA, " negative"="negative", " feeling" = NA, " negaitve" = "negative", " negative opinion"="negative", " negative. fact" = "negative", " neutral"=NA, " posiive"="positive",    " posititve"="positive", " positive" ="positive",  " positive/negative"=NA,  " postitive"="positive",    " postive" = "positive",  " за что деньги"=NA,  " настроение то что нужный"=NA, " пострадать"=NA, 
                                   " расти"=NA, "negative "="negative", "positive "="positive"))

dictionary <- na.omit(dictionary)
dictionary <- dictionary[c(3,4)]
dictionary <- dictionary %>% 
  mutate(score=ifelse(dictionary$sentiment=="positive", 1, -1))


write.csv(dictionary, file = "dictionary.csv")