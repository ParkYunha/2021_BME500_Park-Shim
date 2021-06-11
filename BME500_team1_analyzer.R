#---------------------------------------------#
#------------ BME500 Term Project ------------#
#------------ Team1: 박윤하 심현서 -----------#
#---------------------------------------------#


### 초기 세팅 ###
list.files()
getwd()
setwd("/Users/yunha/Desktop/BME500_R/project")


### 7개 저널에 대한 csv 파일 목록 ###
journals <- c("ISR", "JMIS", "MISQ", "IJEIS", "JCIT", "JIS", "JGIM")
journals.top <- c("ISR", "JMIS", "MISQ")
journals.etc <- c("IJEIS", "JCIT", "JIS", "JGIM")
journals.csv <- c("ISR.csv", "JMIS.csv", "MISQ.csv",
              "IJEIS.csv", "JCIT.csv", "JIS.csv", "JGIM.csv")



### 필요한 패키지 탑재 ###

library(pander)
library(quanteda)
library(tidytext)
library(tibble)
library(dplyr)
library(tm)
library(stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)



### analyzer 구현 ###

### 불용어 설정 ###
mystopwords <- c(stopwords("english"), c("the", "for", "from", "and", 
                                         "editorial", "policy", "covers", "front",
                                         "information"))

#####################################################################
#### Task 1: 저널별로 특성을 보여줌 (전체 기간에 대해 빈도 분석) ####
#####################################################################


analyzerWhole <- function(journal) {
  
  filename <- paste(journal, ".csv", sep = "")
  
  #-- read csv file as a dataframe
  papers.df <- read.csv(filename)
  
  #--- make a Corpus
  papers.corpus <- VCorpus(VectorSource(papers.df$title))
  
  papers.corpus <- tm_map(papers.corpus, content_transformer(tolower))
  papers.corpus <- tm_map(papers.corpus, removeWords, mystopwords)
  papers.corpus <- tm_map(papers.corpus, removePunctuation)
  papers.corpus <- tm_map(papers.corpus, removeNumbers)
  papers.corpus <- tm_map(papers.corpus, stripWhitespace)
  papers.corpus <- tm_map(papers.corpus, content_transformer(trimws))
  
  
  #--- make document-term matrix
  papers.dtm <- DocumentTermMatrix(papers.corpus)
  termfreq <- colSums(as.matrix(papers.dtm))
  termfreq.df <- data.frame(word = names(termfreq), frequency = termfreq)
  
  
  #--  많이 출현한 상위 20개 키워드 추출 
  termfreq10 <- termfreq[head(order(termfreq, decreasing = TRUE), n = 20)]
  termfreq10.df <- data.frame(word = names(termfreq10), frequency = termfreq10)
  
  
  #--- Draw term frequency graph (bar) -> save as a jpg file
  ggplot(termfreq10.df,
         aes(x = reorder(word, frequency), y = frequency, fill = word)) +
    geom_col(color = "dimgray", width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = frequency), size = 3.5, color = "black", hjust = -0.3) +
    labs(x = NULL, y = "Term Frequency (count)", 
         title = paste("Frequency Analysis of ", journal, " (top 20 words, 2000 - 2021)", sep = "")) +
    theme(plot.title = element_text(face = "bold.italic")) + 
    coord_flip()
  ggsave(file = paste(journal, "_whole.jpg", sep = ""))
  
  
  #--- Draw wordcloud -> save as a png file
  set.seed(123)
  png(paste(journal, "_whole_cloud.png", sep = ""), 
      width = 700, height = 700)
  wordcloud(words = names(termfreq), freq = termfreq, scale = c(7, 1), min.freq = 10,
            rot.per = 0.1, random.order = FALSE, random.color = FALSE,
            colors = brewer.pal(6, "Dark2"))
  dev.off()
}


for (i in journals) {
  analyzerWhole(i)
  print(paste(i, "completed"))
}





#########################################################
#### Task 2,3: 저널 그룹에 따른 시대별 특성을 보여줌 ####
###############  (5개년 구간별 빈도 분석)  ##############
#########################################################


period2000 <- c(2000, 2001, 2002, 2003, 2004, 2005)
period2006 <- c(2006, 2007, 2008, 2009, 2010)
period2011 <- c(2011, 2012, 2013, 2014, 2015)
period2016 <- c(2016, 2017, 2018, 2019, 2020, 2021)



### 여러 저널을 합쳐서 하나의 데이터프레임으로 만들고, 분석할 수 있는 함수 ###

analyzerSeveralPartials <- function(journals, name, years) {
  
  filenames <- paste(journals, ".csv", sep = "")
  numOfFiles <- length(filenames)
  period <- paste(toString(years[1]), "-", toString(years[length(years)]))
  
  #--- read csv file as a dataframe
  papers.df <- data.frame(matrix(nrow = 0, ncol = 2))
  for (file in filenames) {
    curr.df <- read.csv(file)
    #--- 입력된 연도에 해당하는 데이터만 추출한다
    curr.df <- select(filter(curr.df, between(published_year, years[1], years[length(years)])), 
                       c(title, published_year))
    papers.df <- rbind(papers.df, curr.df)
  }

  
  #--- make a Corpus
  papers.corpus <- VCorpus(VectorSource(papers.df$title))
  
  papers.corpus <- tm_map(papers.corpus, content_transformer(tolower))
  papers.corpus <- tm_map(papers.corpus, removeWords, mystopwords)
  papers.corpus <- tm_map(papers.corpus, removePunctuation)
  papers.corpus <- tm_map(papers.corpus, removeNumbers)
  papers.corpus <- tm_map(papers.corpus, stripWhitespace)
  papers.corpus <- tm_map(papers.corpus, content_transformer(trimws))
  
  
  #--- make document-term matrix
  papers.dtm <- DocumentTermMatrix(papers.corpus)
  termfreq <- colSums(as.matrix(papers.dtm))
  termfreq.df <- data.frame(word = names(termfreq), frequency = termfreq)
  
  
  #--  많이 출현한 상위 15개 키워드 추출 
  termfreq10 <- termfreq[head(order(termfreq, decreasing = TRUE), n = 15)]
  termfreq10.df <- data.frame(word = names(termfreq10), frequency = termfreq10)
  
  
  #--- Draw term frequency graph (bar) -> save as a jpg file
  graph <- ggplot(termfreq10.df,
                  aes(x = reorder(word, frequency), y = frequency, fill = word)) +
    geom_col(color = "dimgray", width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = frequency), size = 2.5, color = "black", hjust = -0.3) +
    labs(x = NULL, y = "Term Frequency (count)", 
         title = paste(name, " (", period, ")", sep = "")) +
    theme(plot.title  = element_text(face = "bold.italic",
                                     size = 10, color = "brown"),
          axis.title = element_text(size = 8, color = "gray")) +
    coord_flip()
  return(graph)
}



##############################################
### Task 2: 그룹별 키워드 트렌드 비교 분석 ###
##############################################


top2000 <- analyzerSeveralPartials(journals.top, "Top Journals", period2000)
top2006 <- analyzerSeveralPartials(journals.top, "Top Journals", period2006)
top2011 <- analyzerSeveralPartials(journals.top, "Top Journals", period2011)
top2016 <- analyzerSeveralPartials(journals.top, "Top Journals", period2016)

etc2000 <- analyzerSeveralPartials(journals.etc, "ETC Journals", period2000)
etc2006 <- analyzerSeveralPartials(journals.etc, "ETC Journals", period2006)
etc2011 <- analyzerSeveralPartials(journals.etc, "ETC Journals", period2011)
etc2016 <- analyzerSeveralPartials(journals.etc, "ETC Journals", period2016)


# 각 그룹별 그래프를 합쳐서 파일로 저장하기
graph_top <- grid.arrange(top2000, top2006, top2011, top2016,
                          nrow = 2, ncol = 2,
                          top = "Analysis by Age of Top Journals (top 15 words)")
ggsave(file = "Analysis by Age of Top Journals.jpg", plot = graph_top)

graph_etc <- grid.arrange(etc2000, etc2006, etc2011, etc2016,
                          nrow = 2, ncol = 2,
                          top = "Analysis by Age of ETC Journals (top 15 words)")
ggsave(file = "Analysis by Age of ETC Journals.jpg", plot = graph_etc)




########################################################
### Task 3: 전체(7개) 저널에 대한 키워드 트렌드 분석 ###
########################################################

total2000 <- analyzerSeveralPartials(journals, "Journals Total", period2000)
total2006 <- analyzerSeveralPartials(journals, "Journals Total", period2006)
total2011 <- analyzerSeveralPartials(journals, "Journals Total", period2011)
total2016 <- analyzerSeveralPartials(journals, "Journals Total", period2016)

graph_total <- grid.arrange(total2000, total2006, total2011, total2016,
                          nrow = 2, ncol = 2,
                          top = "Analysis by Age of All Journals (top 15 words)")
ggsave(file = "Analysis by Age of All Journals.jpg", plot = graph_total)











##############################################
### 하나의 저널에 대해, 특정 구간으로 잘라서 분석하기 위한 함수 ###
# -> JIS 의 상승세를 분석하기 위해 사용함 


  analyzerPartial <- function(journal, years) {
    filename <- paste(journal, ".csv", sep = "")
    period <-
      paste(toString(years[1]), "-", toString(years[length(years)]))
    
    #--- read csv file as a dataframe
    papers.df <- read.csv(filename)
    
    #--- 입력된 연도에 해당하는 데이터만 추출한다
    papers.df <-
      select(filter(papers.df, between(published_year, years[1], years[length(years)])),
             c(title, published_year))
    
    #--- make a Corpus
    papers.corpus <- VCorpus(VectorSource(papers.df$title))
    
    papers.corpus <-
      tm_map(papers.corpus, content_transformer(tolower))
    papers.corpus <- tm_map(papers.corpus, removeWords, mystopwords)
    papers.corpus <- tm_map(papers.corpus, removePunctuation)
    papers.corpus <- tm_map(papers.corpus, removeNumbers)
    papers.corpus <- tm_map(papers.corpus, stripWhitespace)
    papers.corpus <-
      tm_map(papers.corpus, content_transformer(trimws))
    
    
    #--- make document-term matrix
    papers.dtm <- DocumentTermMatrix(papers.corpus)
    termfreq <- colSums(as.matrix(papers.dtm))
    termfreq.df <-
      data.frame(word = names(termfreq), frequency = termfreq)
    
    
    #--  많이 출현한 상위 15개 키워드 추출
    termfreq15 <-
      termfreq[head(order(termfreq, decreasing = TRUE), n = 15)]
    termfreq15.df <-
      data.frame(word = names(termfreq15), frequency = termfreq15)
    
    
    #--- Draw term frequency graph (bar) -> save as a jpg file
    graph <- ggplot(termfreq15.df,
                    aes(x = reorder(word, frequency), y = frequency, fill = word)) +
      geom_col(color = "dimgray", width = 0.6, show.legend = FALSE) +
      geom_text(aes(label = frequency), size = 2.5, color = "black", hjust = -0.3) +
      labs(x = NULL, y = "Term Frequency (count)", 
           title = paste(journal, " (", period, ")", sep = "")) +
      theme(plot.title  = element_text(face = "bold.italic",
                                       size = 10, color = "brown"),
            axis.title = element_text(size = 8, color = "gray")) +
      coord_flip()
    return(graph)
    
    
  }
  
  jis2000 <- analyzerPartial("JIS", period2000)
  jis2006 <- analyzerPartial("JIS", period2006)
  jis2011 <- analyzerPartial("JIS", period2011)
  jis2016 <- analyzerPartial("JIS", period2016)
  
  graph_jis <- grid.arrange(jis2000, jis2006, jis2011, jis2016,
                            nrow = 2, ncol = 2,
                            top = "Analysis by Age of JIS (top 15 words)")
  ggsave(file = "Analysis by Age of JIS.jpg", plot = graph_jis)
  
  



  
  












