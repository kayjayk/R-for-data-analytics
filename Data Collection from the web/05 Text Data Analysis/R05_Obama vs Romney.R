install.packages("tm", dependencies = TRUE)
install.packages("wordcloud", dependencies = TRUE)
install.packages("arules", dependencies = TRUE)
install.packages("arulesViz", dependencies = TRUE)
install.packages("igraph", dependencies = TRUE)

library(tm)
library(wordcloud)
library(arules)
library(arulesViz)
library(igraph)

# Load the data
load("SpeechData.RData")

# 1. Wordcloud ----------------------------------------------------
# Transform the data into Obama and Romney
obama.idx <- which(SpeechData$Speaker == "Obama")
romney.idx <- which(SpeechData$Speaker == "Romney")

obama.speech <- SpeechData[obama.idx, 2]
romney.speech <- SpeechData[romney.idx, 2]

obama.speech <- paste(obama.speech, collapse = " ")
romney.speech <- paste(romney.speech, collapse = " ")

obama.sentence <- strsplit(obama.speech, ".", fixed = TRUE)
obama.sentence <- as.data.frame(obama.sentence, stringsAsFactors = FALSE)
names(obama.sentence) <- "sentence"

romney.sentence <- strsplit(romney.speech, ".", fixed = TRUE)
romney.sentence <- as.data.frame(romney.sentence, stringsAsFactors = FALSE)
names(romney.sentence) <- "sentence"

# Construct corpuses
# VectorSource specifies that the source is character vectors.
obamaCorpus <- Corpus(VectorSource(obama.sentence$sentence)) # tm패키지를 이용해서 corpus형태로 바꾸자.
romneyCorpus <- Corpus(VectorSource(romney.sentence$sentence))

# Preprocessing
# 1: to lower case
obamaCorpus <- tm_map(obamaCorpus, content_transformer(tolower))
romneyCorpus <- tm_map(romneyCorpus, content_transformer(tolower))

# 2: remove puntuations
obamaCorpus <- tm_map(obamaCorpus, content_transformer(removePunctuation))
romneyCorpus <- tm_map(romneyCorpus, content_transformer(removePunctuation))

# 3. remove numbers
obamaCorpus <- tm_map(obamaCorpus, content_transformer(removeNumbers))
romneyCorpus <- tm_map(romneyCorpus, content_transformer(removeNumbers))

# 4. remove stopwords (SMART stopwords list)
myStopwords <- c(stopwords("SMART"), "american", "america")

obamaCorpus <- tm_map(obamaCorpus, removeWords, myStopwords)
romneyCorpus <- tm_map(romneyCorpus, removeWords, myStopwords)

# 5. Stemming
obamaCorpus <- tm_map(obamaCorpus, stemDocument)
romneyCorpus <- tm_map(romneyCorpus, stemDocument)

myStopwords <- c("american", "america")
obamaCorpus <- tm_map(obamaCorpus, removeWords, myStopwords)
romneyCorpus <- tm_map(romneyCorpus, removeWords, myStopwords)

# Term-Document Matrix
obamaTDM <- TermDocumentMatrix(obamaCorpus, control = list(minWordLength = 1))
romneyTDM <- TermDocumentMatrix(romneyCorpus, control = list(minWordLength = 1))

# Term-Document Matrix
obamaTDM #2249 by 1144의 매트릭스인데, 0이아닌 엘리먼트가 9636개.
romneyTDM

as.matrix(obamaTDM)[11:30,11:30]
as.matrix(romneyTDM)[11:30,11:30]

# Frequently used words
findFreqTerms(obamaTDM, lowfreq=15)
findFreqTerms(romneyTDM, lowfreq=15)

# Construct a Word Cloud with Obama's speeches
obama.wcmat <- as.matrix(obamaTDM)

# calculate the frequency of words
obama.word.freq <- sort(rowSums(obama.wcmat), decreasing=TRUE)
obama.keywords <- names(obama.word.freq)
obama.wcdat <- data.frame(word = obama.keywords, freq = obama.word.freq)

pal <- brewer.pal(8, "Dark2")
wordcloud(obama.wcdat$word, obama.wcdat$freq, min.freq=3, scale = c(4, 0.1), 
          rot.per = 0.1, col=pal, random.order=F)

# Construct a Word Cloud with Romney's speeches
romney.wcmat <- as.matrix(romneyTDM)

# calculate the frequency of words
romney.word.freq <- sort(rowSums(romney.wcmat), decreasing=TRUE)
romney.keywords <- names(romney.word.freq)
romney.wcdat <- data.frame(word = romney.keywords, freq = romney.word.freq)

pal <- brewer.pal(8, "Dark2")
wordcloud(romney.wcdat$word, romney.wcdat$freq, min.freq=3, scale = c(4, 0.1), 
          rot.per = 0.1, col=pal, random.order=F)

# Association Rules for Obama Speeches
obama.tran <- as.matrix(t(obamaTDM)) #t함수가 transpose 함수. term doc matrix를 역행렬로 만든다음 matrix로 바꿔라.
obama.tran <- as(obama.tran, "transactions") #data form을 obama.tran matrix를 transaction 형태로 바꿔라.

obama.rules <- apriori(obama.tran, parameter=list(minlen=2,supp=0.007, conf=0.7)) #minlen은 X단어가 나오면 Y가 나온다는 규칙을 찾을 단어의 수.??
#실행하면 30 rule나오는데 supp, conf를 0.005, 0.5 로 낮추면 128개(더많이) 나옴
inspect(obama.rules)
#2,3번째 rule은 무시.
#hard가 쓰인 문장의 비율이 1퍼센트 정도된다는 의미.
#hard가 사용된 문장에서 70퍼센트의 비율로 work가 쓰임.
#lift는 hard work 통게적으로 독립됐다고 가정했을 때보다 9.8배나 많이 쓰였다? 는 뜻.

# Plot the rules
plot(obama.rules, method="graph") #들어가는 화살표가 조건절. #나가는 화살표가 결과절.

# Association Rules for Romney's speeches
romney.tran <- as.matrix(t(romneyTDM))
romney.tran <- as(romney.tran, "transactions")

romney.rules <- apriori(romney.tran, parameter=list(minlen=2,supp=0.0045, conf=0.7))
inspect(romney.rules)

# Plot the rules
plot(romney.rules, method="graph")
