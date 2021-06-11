## Library 불러오기
library(xml2)
library(XML)
library(stringr)
library(dplyr)

### Top Journals
## MISQ
# Parsing
url_MISQ <- "https://misq.org/misq/downloads/?___store=library"
html_MISQ <- read_html(url_MISQ)
html.parsed_MISQ <- htmlParse(html_MISQ)

# Nodeset 추출: MISQ 논문 제목
title_MISQ <- xpathSApply(html.parsed_MISQ,
                          "//div[@class='title']/a[@title='Download Article']",
                          xmlValue)
df_MISQ <- data.frame(title = title_MISQ) # dataframe 생성 후 논문 title 추가하기

# Nodeset 추출: MISQ 각 논문 별 Volume
num_paper <- xpathSApply(html.parsed_MISQ,
                         "//div[@class='article']/div[@class='title']/span[@class='number']",
                         xmlValue)

num_paper[1:8] <- 46 # forthcoming 논문은 46호로 변경
num_paper <- as.factor(as.numeric(str_extract(num_paper, "[0-9]{1,2}"))) # 호수만 추출해서 팩터화
df_MISQ$num_paper <- num_paper # dataframe에 호수 추가하기

# Dataframe 생성하기
for (i in 1:45){ # published_year column을 논문 volume에 해당하는 연도로 변경
  df_MISQ$published_year[df_MISQ$num_paper==i]<-1976 + i
}
df_MISQ$published_year[1:8] <- 2021 
df_MISQ <- df_MISQ[,-2] # 논문 volume은 불필요하므로 삭제

df_MISQ <- select(filter(df_MISQ, between(published_year, 2000, 2021)), 
                    c(title, published_year))

#####################
#####################
## ISR
# 2021
# Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2021 <- "https://pubsonline.informs.org/toc/isre/32/1"
html_ISR_2021 <- read_html(url_ISR_2021)
html.parsed_ISR_2021<- htmlParse(html_ISR_2021)

title_ISR_2021 <- xpathSApply(html.parsed_ISR_2021,
                              "//h5[@class='issue-item__title']/a",
                              xmlValue)
title_ISR_2021<- title_ISR_2021[-c(1,16,17)] # 분석에 불필요한 데이터 삭제

# 2020
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2020_1 <- "https://pubsonline.informs.org/toc/isre/31/1"
html_ISR_2020_1 <- read_html(url_ISR_2020_1)
html.parsed_ISR_2020_1<- htmlParse(html_ISR_2020_1)

title_ISR_2020_1 <- xpathSApply(html.parsed_ISR_2020_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2020_1<- title_ISR_2020_1[-c(1,15,16,17)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2020_2 <- "https://pubsonline.informs.org/toc/isre/31/2"
html_ISR_2020_2 <- read_html(url_ISR_2020_2)
html.parsed_ISR_2020_2<- htmlParse(html_ISR_2020_2)

title_ISR_2020_2 <- xpathSApply(html.parsed_ISR_2020_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2020_2<- title_ISR_2020_2[-c(1,20,21)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2020_3 <- "https://pubsonline.informs.org/toc/isre/31/3"
html_ISR_2020_3 <- read_html(url_ISR_2020_3)
html.parsed_ISR_2020_3<- htmlParse(html_ISR_2020_3)

title_ISR_2020_3 <- xpathSApply(html.parsed_ISR_2020_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2020_3<- title_ISR_2020_3[-c(1,21,22)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2020_4 <- "https://pubsonline.informs.org/toc/isre/31/4"
html_ISR_2020_4 <- read_html(url_ISR_2020_4)
html.parsed_ISR_2020_4<- htmlParse(html_ISR_2020_4)

title_ISR_2020_4 <- xpathSApply(html.parsed_ISR_2020_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2020_4<- title_ISR_2020_4[-c(1,24,25)]

# 2019
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2019_1 <- "https://pubsonline.informs.org/toc/isre/30/1"
html_ISR_2019_1 <- read_html(url_ISR_2019_1)
html.parsed_ISR_2019_1<- htmlParse(html_ISR_2019_1)

title_ISR_2019_1 <- xpathSApply(html.parsed_ISR_2019_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2019_1<- title_ISR_2019_1[-c(1,22,23)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2019_2 <- "https://pubsonline.informs.org/toc/isre/30/2"
html_ISR_2019_2 <- read_html(url_ISR_2019_2)
html.parsed_ISR_2019_2<- htmlParse(html_ISR_2019_2)

title_ISR_2019_2 <- xpathSApply(html.parsed_ISR_2019_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2019_2<- title_ISR_2019_2[-c(1,19)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2019_3 <- "https://pubsonline.informs.org/toc/isre/30/3"
html_ISR_2019_3 <- read_html(url_ISR_2019_3)
html.parsed_ISR_2019_3<- htmlParse(html_ISR_2019_3)

title_ISR_2019_3 <- xpathSApply(html.parsed_ISR_2019_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2019_3<- title_ISR_2019_3[-c(1,24)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2019_4 <- "https://pubsonline.informs.org/toc/isre/30/4"
html_ISR_2019_4 <- read_html(url_ISR_2019_4)
html.parsed_ISR_2019_4<- htmlParse(html_ISR_2019_4)

title_ISR_2019_4 <- xpathSApply(html.parsed_ISR_2019_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2019_4<- title_ISR_2019_4[-c(1,18)]

# 2018
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2018_1 <- "https://pubsonline.informs.org/toc/isre/29/1"
html_ISR_2018_1 <- read_html(url_ISR_2018_1)
html.parsed_ISR_2018_1<- htmlParse(html_ISR_2018_1)

title_ISR_2018_1 <- xpathSApply(html.parsed_ISR_2018_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2018_1<- title_ISR_2018_1[-c(1,18)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2018_2 <- "https://pubsonline.informs.org/toc/isre/29/2"
html_ISR_2018_2 <- read_html(url_ISR_2018_2)
html.parsed_ISR_2018_2<- htmlParse(html_ISR_2018_2)

title_ISR_2018_2 <- xpathSApply(html.parsed_ISR_2018_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2018_2<- title_ISR_2018_2[-c(1,15)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2018_3 <- "https://pubsonline.informs.org/toc/isre/29/3"
html_ISR_2018_3 <- read_html(url_ISR_2018_3)
html.parsed_ISR_2018_3<- htmlParse(html_ISR_2018_3)

title_ISR_2018_3 <- xpathSApply(html.parsed_ISR_2018_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2018_3<- title_ISR_2018_3[-c(1,14)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2018_4 <- "https://pubsonline.informs.org/toc/isre/29/4"
html_ISR_2018_4 <- read_html(url_ISR_2018_4)
html.parsed_ISR_2018_4<- htmlParse(html_ISR_2018_4)

title_ISR_2018_4 <- xpathSApply(html.parsed_ISR_2018_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2018_4<- title_ISR_2018_4[-c(1,17,18)]

# 2017
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2017_1 <- "https://pubsonline.informs.org/toc/isre/28/1"
html_ISR_2017_1 <- read_html(url_ISR_2017_1)
html.parsed_ISR_2017_1<- htmlParse(html_ISR_2017_1)

title_ISR_2017_1 <- xpathSApply(html.parsed_ISR_2017_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2017_1<- title_ISR_2017_1[-c(1,14)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2017_2 <- "https://pubsonline.informs.org/toc/isre/28/2"
html_ISR_2017_2 <- read_html(url_ISR_2017_2)
html.parsed_ISR_2017_2<- htmlParse(html_ISR_2017_2)

title_ISR_2017_2 <- xpathSApply(html.parsed_ISR_2017_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2017_2<- title_ISR_2017_2[-c(1,14)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2017_3 <- "https://pubsonline.informs.org/toc/isre/28/3"
html_ISR_2017_3 <- read_html(url_ISR_2017_3)
html.parsed_ISR_2017_3<- htmlParse(html_ISR_2017_3)

title_ISR_2017_3 <- xpathSApply(html.parsed_ISR_2017_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2017_3<- title_ISR_2017_3[-c(1,14)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2017_4 <- "https://pubsonline.informs.org/toc/isre/28/4"
html_ISR_2017_4 <- read_html(url_ISR_2017_4)
html.parsed_ISR_2017_4<- htmlParse(html_ISR_2017_4)

title_ISR_2017_4 <- xpathSApply(html.parsed_ISR_2017_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2017_4<- title_ISR_2017_4[-c(1,2,3,4,16,17,18)]

# 2016
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2016_1 <- "https://pubsonline.informs.org/toc/isre/27/1"
html_ISR_2016_1 <- read_html(url_ISR_2016_1)
html.parsed_ISR_2016_1<- htmlParse(html_ISR_2016_1)

title_ISR_2016_1 <- xpathSApply(html.parsed_ISR_2016_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2016_1<- title_ISR_2016_1[-c(1,14,15)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2016_2 <- "https://pubsonline.informs.org/toc/isre/27/2"
html_ISR_2016_2 <- read_html(url_ISR_2016_2)
html.parsed_ISR_2016_2<- htmlParse(html_ISR_2016_2)

title_ISR_2016_2 <- xpathSApply(html.parsed_ISR_2016_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2016_2<- title_ISR_2016_2[-c(1,15,16)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2016_3 <- "https://pubsonline.informs.org/toc/isre/27/3"
html_ISR_2016_3 <- read_html(url_ISR_2016_3)
html.parsed_ISR_2016_3<- htmlParse(html_ISR_2016_3)

title_ISR_2016_3 <- xpathSApply(html.parsed_ISR_2016_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2016_3<- title_ISR_2016_3[-c(1,13,14)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2016_4 <- "https://pubsonline.informs.org/toc/isre/27/4"
html_ISR_2016_4 <- read_html(url_ISR_2016_4)
html.parsed_ISR_2016_4<- htmlParse(html_ISR_2016_4)

title_ISR_2016_4 <- xpathSApply(html.parsed_ISR_2016_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2016_4 <- title_ISR_2016_4[-c(1,2,18,19)]

# 2015
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2015_1 <- "https://pubsonline.informs.org/toc/isre/26/1"
html_ISR_2015_1 <- read_html(url_ISR_2015_1)
html.parsed_ISR_2015_1<- htmlParse(html_ISR_2015_1)

# title of informs
title_ISR_2015_1 <- xpathSApply(html.parsed_ISR_2015_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2015_1<- title_ISR_2015_1[-13]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2015_2 <- "https://pubsonline.informs.org/toc/isre/26/2"
html_ISR_2015_2 <- read_html(url_ISR_2015_2)
html.parsed_ISR_2015_2<- htmlParse(html_ISR_2015_2)

title_ISR_2015_2 <- xpathSApply(html.parsed_ISR_2015_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2015_2<- title_ISR_2015_2[-13]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2015_3 <- "https://pubsonline.informs.org/toc/isre/26/3"
html_ISR_2015_3 <- read_html(url_ISR_2015_3)
html.parsed_ISR_2015_3<- htmlParse(html_ISR_2015_3)

title_ISR_2015_3 <- xpathSApply(html.parsed_ISR_2015_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2015_3<- title_ISR_2015_3[-c(1,12)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2015_4 <- "https://pubsonline.informs.org/toc/isre/26/4"
html_ISR_2015_4 <- read_html(url_ISR_2015_4)
html.parsed_ISR_2015_4<- htmlParse(html_ISR_2015_4)

title_ISR_2015_4 <- xpathSApply(html.parsed_ISR_2015_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2015_4 <- title_ISR_2015_4[-c(1,2,16,17)]

# 2014
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2014_1 <- "https://pubsonline.informs.org/toc/isre/25/1"
html_ISR_2014_1 <- read_html(url_ISR_2014_1)
html.parsed_ISR_2014_1<- htmlParse(html_ISR_2014_1)

title_ISR_2014_1 <- xpathSApply(html.parsed_ISR_2014_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2014_1<- title_ISR_2014_1[-11]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2014_2 <- "https://pubsonline.informs.org/toc/isre/25/2"
html_ISR_2014_2 <- read_html(url_ISR_2014_2)
html.parsed_ISR_2014_2<- htmlParse(html_ISR_2014_2)

title_ISR_2014_2 <- xpathSApply(html.parsed_ISR_2014_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2014_2<- title_ISR_2014_2[-13]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2014_3 <- "https://pubsonline.informs.org/toc/isre/25/3"
html_ISR_2014_3 <- read_html(url_ISR_2014_3)
html.parsed_ISR_2014_3<- htmlParse(html_ISR_2014_3)

title_ISR_2014_3 <- xpathSApply(html.parsed_ISR_2014_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2014_3 <- title_ISR_2014_3[-13]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2014_4 <- "https://pubsonline.informs.org/toc/isre/25/4"
html_ISR_2014_4 <- read_html(url_ISR_2014_4)
html.parsed_ISR_2014_4<- htmlParse(html_ISR_2014_4)

title_ISR_2014_4 <- xpathSApply(html.parsed_ISR_2014_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2014_4 <- title_ISR_2014_4[-c(1,17)]

# 2013
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2013_1 <- "https://pubsonline.informs.org/toc/isre/24/1"
html_ISR_2013_1 <- read_html(url_ISR_2013_1)
html.parsed_ISR_2013_1<- htmlParse(html_ISR_2013_1)

title_ISR_2013_1 <- xpathSApply(html.parsed_ISR_2013_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2013_1<- title_ISR_2013_1[-c(1,13)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2013_2 <- "https://pubsonline.informs.org/toc/isre/24/2"
html_ISR_2013_2 <- read_html(url_ISR_2013_2)
html.parsed_ISR_2013_2<- htmlParse(html_ISR_2013_2)

title_ISR_2013_2 <- xpathSApply(html.parsed_ISR_2013_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2013_2<- title_ISR_2013_2[-c(16,17)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2013_3 <- "https://pubsonline.informs.org/toc/isre/24/3"
html_ISR_2013_3 <- read_html(url_ISR_2013_3)
html.parsed_ISR_2013_3<- htmlParse(html_ISR_2013_3)

title_ISR_2013_3 <- xpathSApply(html.parsed_ISR_2013_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2013_3 <- title_ISR_2013_3[-20]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2013_4 <- "https://pubsonline.informs.org/toc/isre/24/4"
html_ISR_2013_4 <- read_html(url_ISR_2013_4)
html.parsed_ISR_2013_4 <- htmlParse(html_ISR_2013_4)

title_ISR_2013_4 <- xpathSApply(html.parsed_ISR_2013_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2013_4 <- title_ISR_2013_4[-c(1,18)]

# 2012
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2012_1 <- "https://pubsonline.informs.org/toc/isre/23/1"
html_ISR_2012_1 <- read_html(url_ISR_2012_1)
html.parsed_ISR_2012_1<- htmlParse(html_ISR_2012_1)

title_ISR_2012_1 <- xpathSApply(html.parsed_ISR_2012_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2012_1<- title_ISR_2012_1[-17]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2012_2 <- "https://pubsonline.informs.org/toc/isre/23/2"
html_ISR_2012_2 <- read_html(url_ISR_2012_2)
html.parsed_ISR_2012_2<- htmlParse(html_ISR_2012_2)

title_ISR_2012_2 <- xpathSApply(html.parsed_ISR_2012_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2012_2<- title_ISR_2012_2[-18]

# Volume 3 - part 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2012_3_part_1 <- "https://pubsonline.informs.org/toc/isre/23/3-part-1"
html_ISR_2012_3_part_1 <- read_html(url_ISR_2012_3_part_1)
html.parsed_ISR_2012_3_part_1<- htmlParse(html_ISR_2012_3_part_1)

title_ISR_2012_3_part_1<- xpathSApply(html.parsed_ISR_2012_3_part_1,
                                      "//h5[@class='issue-item__title']/a",
                                      xmlValue)
title_ISR_2012_3_part_1<- title_ISR_2012_3_part_1[-13]

# Volume 3 - part 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2012_3_part_2 <- "https://pubsonline.informs.org/toc/isre/23/3-part-2"
html_ISR_2012_3_part_2 <- read_html(url_ISR_2012_3_part_2)
html.parsed_ISR_2012_3_part_2<- htmlParse(html_ISR_2012_3_part_2)

title_ISR_2012_3_part_2<- xpathSApply(html.parsed_ISR_2012_3_part_2,
                                      "//h5[@class='issue-item__title']/a",
                                      xmlValue)
title_ISR_2012_3_part_2<- title_ISR_2012_3_part_2[-15]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2012_4 <- "https://pubsonline.informs.org/toc/isre/23/4"
html_ISR_2012_4 <- read_html(url_ISR_2012_4)
html.parsed_ISR_2012_4<- htmlParse(html_ISR_2012_4)

title_ISR_2012_4 <- xpathSApply(html.parsed_ISR_2012_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2012_4<- title_ISR_2012_4[-c(1,17)]

# 2011
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2011_1 <- "https://pubsonline.informs.org/toc/isre/22/1"
html_ISR_2011_1 <- read_html(url_ISR_2011_1)
html.parsed_ISR_2011_1<- htmlParse(html_ISR_2011_1)

title_ISR_2011_1 <- xpathSApply(html.parsed_ISR_2011_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2011_1<- title_ISR_2011_1[-c(1,13)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2011_2 <- "https://pubsonline.informs.org/toc/isre/22/2"
html_ISR_2011_2 <- read_html(url_ISR_2011_2)
html.parsed_ISR_2011_2<- htmlParse(html_ISR_2011_2)

title_ISR_2011_2 <- xpathSApply(html.parsed_ISR_2011_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2011_2<- title_ISR_2011_2[-c(1,13)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2011_3 <- "https://pubsonline.informs.org/toc/isre/22/3"
html_ISR_2011_3 <- read_html(url_ISR_2011_3)
html.parsed_ISR_2011_3<- htmlParse(html_ISR_2011_3)

title_ISR_2011_3 <- xpathSApply(html.parsed_ISR_2011_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2011_3<- title_ISR_2011_3[-15]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2011_4 <- "https://pubsonline.informs.org/toc/isre/22/4"
html_ISR_2011_4 <- read_html(url_ISR_2011_4)
html.parsed_ISR_2011_4<- htmlParse(html_ISR_2011_4)

title_ISR_2011_4 <- xpathSApply(html.parsed_ISR_2011_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2011_4<- title_ISR_2011_4[-c(14,15)]

# 2010
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2010_1 <- "https://pubsonline.informs.org/toc/isre/21/1"
html_ISR_2010_1 <- read_html(url_ISR_2010_1)
html.parsed_ISR_2010_1<- htmlParse(html_ISR_2010_1)

title_ISR_2010_1 <- xpathSApply(html.parsed_ISR_2010_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2010_1<- title_ISR_2010_1[-12]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2010_2 <- "https://pubsonline.informs.org/toc/isre/21/2"
html_ISR_2010_2 <- read_html(url_ISR_2010_2)
html.parsed_ISR_2010_2<- htmlParse(html_ISR_2010_2)

title_ISR_2010_2 <- xpathSApply(html.parsed_ISR_2010_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2010_2<- title_ISR_2010_2[-11]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2010_3 <- "https://pubsonline.informs.org/toc/isre/21/3"
html_ISR_2010_3 <- read_html(url_ISR_2010_3)
html.parsed_ISR_2010_3<- htmlParse(html_ISR_2010_3)

title_ISR_2010_3 <- xpathSApply(html.parsed_ISR_2010_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2010_3<- title_ISR_2010_3[-12]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2010_4 <- "https://pubsonline.informs.org/toc/isre/21/4"
html_ISR_2010_4 <- read_html(url_ISR_2010_4)
html.parsed_ISR_2010_4<- htmlParse(html_ISR_2010_4)

title_ISR_2010_4 <- xpathSApply(html.parsed_ISR_2010_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2010_4<- title_ISR_2010_4[-c(1,2,25,26,27)]

# 2009
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2009_1 <- "https://pubsonline.informs.org/toc/isre/20/1"
html_ISR_2009_1 <- read_html(url_ISR_2009_1)
html.parsed_ISR_2009_1<- htmlParse(html_ISR_2009_1)

title_ISR_2009_1 <- xpathSApply(html.parsed_ISR_2009_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2009_1<- title_ISR_2009_1[-c(1,10)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2009_2 <- "https://pubsonline.informs.org/toc/isre/20/2"
html_ISR_2009_2 <- read_html(url_ISR_2009_2)
html.parsed_ISR_2009_2<- htmlParse(html_ISR_2009_2)

title_ISR_2009_2 <- xpathSApply(html.parsed_ISR_2009_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2009_2 <- title_ISR_2009_2[-9]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2009_3 <- "https://pubsonline.informs.org/toc/isre/20/3"
html_ISR_2009_3 <- read_html(url_ISR_2009_3)
html.parsed_ISR_2009_3 <- htmlParse(html_ISR_2009_3)

title_ISR_2009_3 <- xpathSApply(html.parsed_ISR_2009_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2009_3 <- title_ISR_2009_3[-9]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2009_4 <- "https://pubsonline.informs.org/toc/isre/20/4"
html_ISR_2009_4 <- read_html(url_ISR_2009_4)
html.parsed_ISR_2009_4 <- htmlParse(html_ISR_2009_4)

title_ISR_2009_4 <- xpathSApply(html.parsed_ISR_2009_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2009_4 <- title_ISR_2009_4[-c(1,8,9)]

# 2008
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2008_1 <- "https://pubsonline.informs.org/toc/isre/19/1"
html_ISR_2008_1 <- read_html(url_ISR_2008_1)
html.parsed_ISR_2008_1<- htmlParse(html_ISR_2008_1)

title_ISR_2008_1 <- xpathSApply(html.parsed_ISR_2008_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2008_1<- title_ISR_2008_1[-c(1,8)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2008_2 <- "https://pubsonline.informs.org/toc/isre/19/2"
html_ISR_2008_2 <- read_html(url_ISR_2008_2)
html.parsed_ISR_2008_2<- htmlParse(html_ISR_2008_2)

title_ISR_2008_2 <- xpathSApply(html.parsed_ISR_2008_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2008_2<- title_ISR_2008_2[-c(1,8)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2008_3 <- "https://pubsonline.informs.org/toc/isre/19/3"
html_ISR_2008_3 <- read_html(url_ISR_2008_3)
html.parsed_ISR_2008_3<- htmlParse(html_ISR_2008_3)

title_ISR_2008_3 <- xpathSApply(html.parsed_ISR_2008_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2008_3<- title_ISR_2008_3[-10]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2008_4 <- "https://pubsonline.informs.org/toc/isre/19/4"
html_ISR_2008_4 <- read_html(url_ISR_2008_4)
html.parsed_ISR_2008_4<- htmlParse(html_ISR_2008_4)

title_ISR_2008_4 <- xpathSApply(html.parsed_ISR_2008_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2008_4 <- title_ISR_2008_4[-c(1,8,9)]

# 2007
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2007_1 <- "https://pubsonline.informs.org/toc/isre/18/1"
html_ISR_2007_1 <- read_html(url_ISR_2007_1)
html.parsed_ISR_2007_1<- htmlParse(html_ISR_2007_1)

title_ISR_2007_1 <- xpathSApply(html.parsed_ISR_2007_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2007_1<- title_ISR_2007_1[-c(1,8)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2007_2 <- "https://pubsonline.informs.org/toc/isre/18/2"
html_ISR_2007_2 <- read_html(url_ISR_2007_2)
html.parsed_ISR_2007_2<- htmlParse(html_ISR_2007_2)

title_ISR_2007_2 <- xpathSApply(html.parsed_ISR_2007_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2007_2<- title_ISR_2007_2[-c(1,9)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2007_3 <- "https://pubsonline.informs.org/toc/isre/18/3"
html_ISR_2007_3 <- read_html(url_ISR_2007_3)
html.parsed_ISR_2007_3 <- htmlParse(html_ISR_2007_3)

title_ISR_2007_3 <- xpathSApply(html.parsed_ISR_2007_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2007_3<- title_ISR_2007_3[-8]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2007_4 <- "https://pubsonline.informs.org/toc/isre/18/4"
html_ISR_2007_4 <- read_html(url_ISR_2007_4)
html.parsed_ISR_2007_4 <- htmlParse(html_ISR_2007_4)

title_ISR_2007_4 <- xpathSApply(html.parsed_ISR_2007_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2007_4 <- title_ISR_2007_4[-c(1,8)]

# 2006
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2006_1 <- "https://pubsonline.informs.org/toc/isre/17/1"
html_ISR_2006_1 <- read_html(url_ISR_2006_1)
html.parsed_ISR_2006_1<- htmlParse(html_ISR_2006_1)

title_ISR_2006_1 <- xpathSApply(html.parsed_ISR_2006_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2006_1<- title_ISR_2006_1[-1]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2006_2 <- "https://pubsonline.informs.org/toc/isre/17/2"
html_ISR_2006_2 <- read_html(url_ISR_2006_2)
html.parsed_ISR_2006_2<- htmlParse(html_ISR_2006_2)

title_ISR_2006_2 <- xpathSApply(html.parsed_ISR_2006_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2006_2<- title_ISR_2006_2[-c(1,7)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2006_3 <- "https://pubsonline.informs.org/toc/isre/17/3"
html_ISR_2006_3 <- read_html(url_ISR_2006_3)
html.parsed_ISR_2006_3<- htmlParse(html_ISR_2006_3)

title_ISR_2006_3 <- xpathSApply(html.parsed_ISR_2006_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2006_3 <- title_ISR_2006_3[-1]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2006_4 <- "https://pubsonline.informs.org/toc/isre/17/4"
html_ISR_2006_4 <- read_html(url_ISR_2006_4)
html.parsed_ISR_2006_4 <- htmlParse(html_ISR_2006_4)

title_ISR_2006_4 <- xpathSApply(html.parsed_ISR_2006_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2006_4 <- title_ISR_2006_4[-c(8,9)]

# 2005
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2005_1 <- "https://pubsonline.informs.org/toc/isre/16/1"
html_ISR_2005_1 <- read_html(url_ISR_2005_1)
html.parsed_ISR_2005_1<- htmlParse(html_ISR_2005_1)

title_ISR_2005_1 <- xpathSApply(html.parsed_ISR_2005_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2005_1<- title_ISR_2005_1[-c(1,8)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2005_2 <- "https://pubsonline.informs.org/toc/isre/16/2"
html_ISR_2005_2 <- read_html(url_ISR_2005_2)
html.parsed_ISR_2005_2<- htmlParse(html_ISR_2005_2)

title_ISR_2005_2 <- xpathSApply(html.parsed_ISR_2005_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2005_2<- title_ISR_2005_2[-c(1,8)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2005_3 <- "https://pubsonline.informs.org/toc/isre/16/3"
html_ISR_2005_3 <- read_html(url_ISR_2005_3)
html.parsed_ISR_2005_3<- htmlParse(html_ISR_2005_3)

title_ISR_2005_3 <- xpathSApply(html.parsed_ISR_2005_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2005_3<- title_ISR_2005_3[-c(1,7)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2005_4 <- "https://pubsonline.informs.org/toc/isre/16/4"
html_ISR_2005_4 <- read_html(url_ISR_2005_4)
html.parsed_ISR_2005_4<- htmlParse(html_ISR_2005_4)

title_ISR_2005_4 <- xpathSApply(html.parsed_ISR_2005_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2005_4 <- title_ISR_2005_4[-c(1,7,8)]

# 2004
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2004_1 <- "https://pubsonline.informs.org/toc/isre/15/1"
html_ISR_2004_1 <- read_html(url_ISR_2004_1)
html.parsed_ISR_2004_1<- htmlParse(html_ISR_2004_1)

title_ISR_2004_1 <- xpathSApply(html.parsed_ISR_2004_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2004_1<- title_ISR_2004_1[-c(1,7)]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2004_2 <- "https://pubsonline.informs.org/toc/isre/15/2"
html_ISR_2004_2 <- read_html(url_ISR_2004_2)
html.parsed_ISR_2004_2 <- htmlParse(html_ISR_2004_2)

title_ISR_2004_2 <- xpathSApply(html.parsed_ISR_2004_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2004_2<- title_ISR_2004_2[-c(1,7)]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2004_3 <- "https://pubsonline.informs.org/toc/isre/15/3"
html_ISR_2004_3 <- read_html(url_ISR_2004_3)
html.parsed_ISR_2004_3 <- htmlParse(html_ISR_2004_3)

title_ISR_2004_3 <- xpathSApply(html.parsed_ISR_2004_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2004_3<- title_ISR_2004_3[-c(1,7)]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2004_4 <- "https://pubsonline.informs.org/toc/isre/15/4"
html_ISR_2004_4 <- read_html(url_ISR_2004_4)
html.parsed_ISR_2004_4 <- htmlParse(html_ISR_2004_4)

title_ISR_2004_4 <- xpathSApply(html.parsed_ISR_2004_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2004_4<- title_ISR_2004_4[-c(1,8)]

# 2003
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2003_1 <- "https://pubsonline.informs.org/toc/isre/14/1"
html_ISR_2003_1 <- read_html(url_ISR_2003_1)
html.parsed_ISR_2003_1<- htmlParse(html_ISR_2003_1)

title_ISR_2003_1 <- xpathSApply(html.parsed_ISR_2003_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2003_1<- title_ISR_2003_1[-7]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2003_2 <- "https://pubsonline.informs.org/toc/isre/14/2"
html_ISR_2003_2 <- read_html(url_ISR_2003_2)
html.parsed_ISR_2003_2<- htmlParse(html_ISR_2003_2)

title_ISR_2003_2 <- xpathSApply(html.parsed_ISR_2003_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2003_2 <- title_ISR_2003_2[-5]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2003_3 <- "https://pubsonline.informs.org/toc/isre/14/3"
html_ISR_2003_3 <- read_html(url_ISR_2003_3)
html.parsed_ISR_2003_3<- htmlParse(html_ISR_2003_3)

title_ISR_2003_3 <- xpathSApply(html.parsed_ISR_2003_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2003_3 <- title_ISR_2003_3[-5]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2003_4 <- "https://pubsonline.informs.org/toc/isre/14/4"
html_ISR_2003_4 <- read_html(url_ISR_2003_4)
html.parsed_ISR_2003_4 <- htmlParse(html_ISR_2003_4)

title_ISR_2003_4 <- xpathSApply(html.parsed_ISR_2003_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2003_4 <- title_ISR_2003_4[-c(5,6)]

# 2002
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2002_1 <- "https://pubsonline.informs.org/toc/isre/13/1"
html_ISR_2002_1 <- read_html(url_ISR_2002_1)
html.parsed_ISR_2002_1<- htmlParse(html_ISR_2002_1)

title_ISR_2002_1 <- xpathSApply(html.parsed_ISR_2002_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2002_1 <- title_ISR_2002_1[-1]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2002_2 <- "https://pubsonline.informs.org/toc/isre/13/2"
html_ISR_2002_2 <- read_html(url_ISR_2002_2)
html.parsed_ISR_2002_2<- htmlParse(html_ISR_2002_2)

title_ISR_2002_2 <- xpathSApply(html.parsed_ISR_2002_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2002_2 <- title_ISR_2002_2[-8]

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2002_3 <- "https://pubsonline.informs.org/toc/isre/13/3"
html_ISR_2002_3 <- read_html(url_ISR_2002_3)
html.parsed_ISR_2002_3<- htmlParse(html_ISR_2002_3)

title_ISR_2002_3 <- xpathSApply(html.parsed_ISR_2002_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2002_4 <- "https://pubsonline.informs.org/toc/isre/13/4"
html_ISR_2002_4 <- read_html(url_ISR_2002_4)
html.parsed_ISR_2002_4<- htmlParse(html_ISR_2002_4)

title_ISR_2002_4 <- xpathSApply(html.parsed_ISR_2002_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2002_4 <- title_ISR_2002_4[-1]

# 2001
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2001_1 <- "https://pubsonline.informs.org/toc/isre/12/1"
html_ISR_2001_1 <- read_html(url_ISR_2001_1)
html.parsed_ISR_2001_1<- htmlParse(html_ISR_2001_1)

title_ISR_2001_1 <- xpathSApply(html.parsed_ISR_2001_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2001_1 <- title_ISR_2001_1[-1]

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2001_2 <- "https://pubsonline.informs.org/toc/isre/12/2"
html_ISR_2001_2 <- read_html(url_ISR_2001_2)
html.parsed_ISR_2001_2 <- htmlParse(html_ISR_2001_2)

title_ISR_2001_2 <- xpathSApply(html.parsed_ISR_2001_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2001_3 <- "https://pubsonline.informs.org/toc/isre/12/3"
html_ISR_2001_3 <- read_html(url_ISR_2001_3)
html.parsed_ISR_2001_3<- htmlParse(html_ISR_2001_3)

title_ISR_2001_3 <- xpathSApply(html.parsed_ISR_2001_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2001_3 <- title_ISR_2001_3[-1]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2001_4 <- "https://pubsonline.informs.org/toc/isre/12/4"
html_ISR_2001_4 <- read_html(url_ISR_2001_4)
html.parsed_ISR_2001_4<- htmlParse(html_ISR_2001_4)

title_ISR_2001_4 <- xpathSApply(html.parsed_ISR_2001_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2001_4 <- title_ISR_2001_4[-1]

# 2000
# Volume 1: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2000_1 <- "https://pubsonline.informs.org/toc/isre/11/1"
html_ISR_2000_1 <- read_html(url_ISR_2000_1)
html.parsed_ISR_2000_1<- htmlParse(html_ISR_2000_1)

title_ISR_2000_1 <- xpathSApply(html.parsed_ISR_2000_1,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)

# Volume 2: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2000_2 <- "https://pubsonline.informs.org/toc/isre/11/2"
html_ISR_2000_2 <- read_html(url_ISR_2000_2)
html.parsed_ISR_2000_2<- htmlParse(html_ISR_2000_2)

title_ISR_2000_2 <- xpathSApply(html.parsed_ISR_2000_2,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)

# Volume 3: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2000_3 <- "https://pubsonline.informs.org/toc/isre/11/3"
html_ISR_2000_3 <- read_html(url_ISR_2000_3)
html.parsed_ISR_2000_3<- htmlParse(html_ISR_2000_3)

title_ISR_2000_3 <- xpathSApply(html.parsed_ISR_2000_3,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2000_3 <- title_ISR_2000_3[-1]

# Volume 4: Parsing & Nodeset 추출: ISR 논문 제목 추출
url_ISR_2000_4 <- "https://pubsonline.informs.org/toc/isre/11/4"
html_ISR_2000_4 <- read_html(url_ISR_2000_4)
html.parsed_ISR_2000_4<- htmlParse(html_ISR_2000_4)

title_ISR_2000_4 <- xpathSApply(html.parsed_ISR_2000_4,
                                "//h5[@class='issue-item__title']/a",
                                xmlValue)
title_ISR_2000_4 <- title_ISR_2000_4[-c(1,8)]

# Dataframe 생성
df_ISR_2021 <- data.frame(title = title_ISR_2021, published_year = 2021)
df_ISR_2020 <- data.frame(title = c(title_ISR_2020_1,title_ISR_2020_2,
                                    title_ISR_2020_3,title_ISR_2020_4), 
                          published_year = 2020)
df_ISR_2019 <- data.frame(title = c(title_ISR_2019_1,title_ISR_2019_2,
                                    title_ISR_2019_3,title_ISR_2019_4), 
                          published_year = 2019)
df_ISR_2018 <- data.frame(title = c(title_ISR_2018_1,title_ISR_2018_2,
                                    title_ISR_2018_3,title_ISR_2018_4), 
                          published_year = 2018)
df_ISR_2017 <- data.frame(title = c(title_ISR_2017_1,title_ISR_2017_2,
                                    title_ISR_2017_3,title_ISR_2017_4), 
                          published_year = 2017)
df_ISR_2016 <- data.frame(title = c(title_ISR_2016_1,title_ISR_2016_2,
                                    title_ISR_2016_3,title_ISR_2016_4), 
                          published_year = 2016)
df_ISR_2015 <- data.frame(title = c(title_ISR_2015_1,title_ISR_2015_2,
                                    title_ISR_2015_3,title_ISR_2015_4), 
                          published_year = 2015)
df_ISR_2014 <- data.frame(title = c(title_ISR_2014_1,title_ISR_2014_2,
                                    title_ISR_2014_3,title_ISR_2014_4), 
                          published_year = 2014)
df_ISR_2013<- data.frame(title = c(title_ISR_2013_1,title_ISR_2013_2,
                                   title_ISR_2013_3,
                                   title_ISR_2013_4), 
                         published_year = 2013)
df_ISR_2012<- data.frame(title = c(title_ISR_2012_1,title_ISR_2012_2,
                                   title_ISR_2012_3_part_1,
                                   title_ISR_2012_3_part_2,
                                   title_ISR_2012_4), 
                         published_year = 2012)
df_ISR_2011<- data.frame(title = c(title_ISR_2011_1,title_ISR_2011_2,
                                   title_ISR_2011_3,title_ISR_2011_4), 
                         published_year = 2011)
df_ISR_2010<- data.frame(title = c(title_ISR_2010_1,title_ISR_2010_2,
                                   title_ISR_2010_3,title_ISR_2010_4), 
                         published_year = 2010)
df_ISR_2009<- data.frame(title = c(title_ISR_2009_1,title_ISR_2009_2,
                                   title_ISR_2009_3,title_ISR_2009_4), 
                         published_year = 2009)
df_ISR_2008<- data.frame(title = c(title_ISR_2008_1,title_ISR_2008_2,
                                   title_ISR_2008_3,title_ISR_2008_4), 
                         published_year = 2008)
df_ISR_2007<- data.frame(title = c(title_ISR_2007_1,title_ISR_2007_2,
                                   title_ISR_2007_3,title_ISR_2007_4), 
                         published_year = 2007)
df_ISR_2006<- data.frame(title = c(title_ISR_2006_1,title_ISR_2006_2,
                                   title_ISR_2006_3,title_ISR_2006_4), 
                         published_year = 2006)
df_ISR_2005<- data.frame(title = c(title_ISR_2005_1,title_ISR_2005_2,
                                   title_ISR_2005_3,title_ISR_2005_4), 
                         published_year = 2005)
df_ISR_2004<- data.frame(title = c(title_ISR_2004_1,title_ISR_2004_2,
                                   title_ISR_2004_3,title_ISR_2004_4), 
                         published_year = 2004)
df_ISR_2003<- data.frame(title = c(title_ISR_2003_1,title_ISR_2003_2,
                                   title_ISR_2003_3,title_ISR_2003_4), 
                         published_year = 2003)
df_ISR_2002<- data.frame(title = c(title_ISR_2002_1,title_ISR_2002_2,
                                   title_ISR_2002_3,title_ISR_2002_4), 
                         published_year = 2002)
df_ISR_2001<- data.frame(title = c(title_ISR_2001_1,title_ISR_2001_2,
                                   title_ISR_2001_3,title_ISR_2001_4), 
                         published_year = 2001)
df_ISR_2000<- data.frame(title = c(title_ISR_2000_1,title_ISR_2000_2,
                                   title_ISR_2000_3,title_ISR_2000_4), 
                         published_year = 2000)

df_ISR <- rbind(df_ISR_2000, df_ISR_2001, df_ISR_2002, df_ISR_2003, df_ISR_2004, df_ISR_2005,
                df_ISR_2006, df_ISR_2007, df_ISR_2008, df_ISR_2009, df_ISR_2010, df_ISR_2011,
                df_ISR_2012, df_ISR_2013, df_ISR_2014, df_ISR_2015, df_ISR_2016, df_ISR_2017,
                df_ISR_2018, df_ISR_2019, df_ISR_2020, df_ISR_2021)

#####################
#####################
## JMIS
# 2000 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2000 <- data.frame() # 2000년도 정보를 넣기 위한 빈 데이터프레임 생성

for (i in c(87,82,92,111)){ # 2000년도에 해당하는 url 주소 i에 할당하여 파싱 & 노드셋 추출
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2000 <- rbind(df_JMIS_2000, new) # 추출한 값 데이터프레임에 추가
  print(df_JMIS_2000)
}

df_JMIS_2000$published_year <- 2000 # 연도 정보 데이터프레임에 추가

# 2001 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2001 <- data.frame()

for (i in c(50, 58, 28, 23)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2001 <- rbind(df_JMIS_2001, new)
  print(df_JMIS_2001)
}

df_JMIS_2001$published_year <- 2001

# 2002 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2002 <- data.frame()

for (i in c(49, 13, 60, 83)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2002 <- rbind(df_JMIS_2002, new)
  print(df_JMIS_2002)
}

df_JMIS_2002$published_year <- 2002

# 2003 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2003 <- data.frame()

for (i in c(73, 116, 31, 39)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2003 <- rbind(df_JMIS_2003, new)
  print(df_JMIS_2003)
}

df_JMIS_2003$published_year <- 2003

# 2004 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2004 <- data.frame()

for (i in c(76, 102, 46, 93)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2004 <- rbind(df_JMIS_2004, new)
  print(df_JMIS_2004)
}

df_JMIS_2004$published_year <- 2004

# 2005 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2005 <- data.frame()

for (i in c(62, 86, 29, 15)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2005 <- rbind(df_JMIS_2005, new)
  print(df_JMIS_2005)
}

df_JMIS_2005$published_year <- 2005

# 2006 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2006 <- data.frame()

for (i in c(77, 61, 47, 112)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2006 <- rbind(df_JMIS_2006, new)
  print(df_JMIS_2006)
}

df_JMIS_2006$published_year <- 2006

# 2007 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2007 <- data.frame()

for (i in c(68, 108, 26, 89)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2007 <- rbind(df_JMIS_2007, new)
  print(df_JMIS_2007)
}

df_JMIS_2007$published_year <- 2007

# 2008 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2008 <- data.frame()

for (i in c(32, 1, 37, 45)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2008 <- rbind(df_JMIS_2008, new)
  print(df_JMIS_2008)
}

df_JMIS_2008$published_year <- 2008

# 2009 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2009 <- data.frame()

for (i in c(106, 71, 33, 40)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2009 <- rbind(df_JMIS_2009, new)
  print(df_JMIS_2009)
}

df_JMIS_2009$published_year <- 2009

# 2010 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2010 <- data.frame()

for (i in c(48, 18, 80, 90)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2010 <- rbind(df_JMIS_2010, new)
  print(df_JMIS_2010)
}

df_JMIS_2010$published_year <- 2010

# 2011 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2011 <- data.frame()

for (i in c(20, 22, 43, 65)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2011 <- rbind(df_JMIS_2011, new)
  print(df_JMIS_2011)
}

df_JMIS_2011$published_year <- 2011

# 2012 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2012 <- data.frame()

for (i in c(7, 66, 24, 27)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2012 <- rbind(df_JMIS_2012, new)
  print(df_JMIS_2012)
}

df_JMIS_2012$published_year <- 2012

# 2013 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2013 <- data.frame()

for (i in c(64, 100, 41, 30)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2013 <- rbind(df_JMIS_2013, new)
  print(df_JMIS_2013)
}

df_JMIS_2013$published_year <- 2013

# 2014 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2014 <- data.frame()

for (i in c(120, 122, 123, 124)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2014 <- rbind(df_JMIS_2014, new)
  print(df_JMIS_2014)
}

df_JMIS_2014$published_year <- 2014

# 2015 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2015 <- data.frame()

for (i in c(125, 126, 127, 128, 129)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2015 <- rbind(df_JMIS_2015, new)
  print(df_JMIS_2015)
}

df_JMIS_2015$published_year <- 2015

# 2016 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2016 <- data.frame()

for (i in c(130, 131, 132, 133)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2016 <- rbind(df_JMIS_2016, new)
  print(df_JMIS_2016)
}

df_JMIS_2016$published_year <- 2016

# 2017 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2017 <- data.frame()

for (i in c(134, 135, 136, 137)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2017 <- rbind(df_JMIS_2017, new)
  print(df_JMIS_2017)
}

df_JMIS_2017$published_year <- 2017

# 2018 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2018 <- data.frame()

for (i in c(138, 139, 140, 141)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2018 <- rbind(df_JMIS_2018, new)
  print(df_JMIS_2018)
}

df_JMIS_2018$published_year <- 2018

# 2019 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2019 <- data.frame()

for (i in c(142, 143, 144, 145)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2019 <- rbind(df_JMIS_2019, new)
  print(df_JMIS_2019)
}

df_JMIS_2019$published_year <- 2019

# 2020 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2020 <- data.frame()

for (i in c(146, 147, 148, 149)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2020 <- rbind(df_JMIS_2020, new)
  print(df_JMIS_2020)
}

df_JMIS_2020$published_year <- 2020

# 2021 title
url_JMIS <- "https://www.jmis-web.org/issues/87"
df_JMIS_2021 <- data.frame()

for (i in c(150)){
  url <- paste0(str_sub(url_JMIS, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JMIS <- xpathSApply(html.parsed, "//div[@class='article ']/h4/a", 
                            xmlValue)
  new <- data.frame(title = title_JMIS)
  df_JMIS_2021 <- rbind(df_JMIS_2021, new)
  print(df_JMIS_2021)
}

df_JMIS_2021$published_year <- 2021


# Dataframe 생성하기
df_JMIS <- rbind(df_JMIS_2000,df_JMIS_2001, df_JMIS_2002, df_JMIS_2003, df_JMIS_2004,
                 df_JMIS_2005, df_JMIS_2006, df_JMIS_2007, df_JMIS_2008, df_JMIS_2009,
                 df_JMIS_2010, df_JMIS_2011, df_JMIS_2012, df_JMIS_2013, df_JMIS_2014,
                 df_JMIS_2015, df_JMIS_2016, df_JMIS_2017, df_JMIS_2018, df_JMIS_2019,
                 df_JMIS_2020, df_JMIS_2021)

#####################
#####################
### Other Journals
## JIS
# JIS의 논문 중 Volume 1에 해당하는 논문 정보 수집
url_JIS_1<-"https://meridian.allenpress.com/jis/issue/35/1"
df_JIS_1 <- data.frame()

for (i in c(14:35)){ # 14권에서 35권까지 추출
  url <- paste0(paste0(str_sub(url_JIS_1, end=-5),i),"/1") # Volume이 1인 것 추출
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JIS <- xpathSApply(html.parsed, "//div[@class='al-article-items']/h5[@class='customLink item-title']/a", 
                           xmlValue)
  new_1 <- data.frame(title = title_JIS)
  df_JIS_1 <- rbind(df_JIS_1, new_1)
  print(df_JIS_1)
}
df_JIS_1$published_year <- 1:nrow(df_JIS_1) # pubished year을 입력할 column 생성

df_JIS_1$published_year[1:5] <- 2000 ; df_JIS_1$published_year[6:10] <- 2001
df_JIS_1$published_year[11:19] <- 2002 ; df_JIS_1$published_year[20:28] <- 2003
df_JIS_1$published_year[29:38] <- 2004 ; df_JIS_1$published_year[39:48] <- 2005
df_JIS_1$published_year[49:62] <- 2006 ; df_JIS_1$published_year[63:73] <- 2007
df_JIS_1$published_year[74:84] <- 2008 ; df_JIS_1$published_year[85:94] <- 2009
df_JIS_1$published_year[95:101] <- 2010 ; df_JIS_1$published_year[102:113] <- 2011
df_JIS_1$published_year[114:125] <- 2012 ; df_JIS_1$published_year[126:144] <- 2013
df_JIS_1$published_year[145:160] <- 2014 ; df_JIS_1$published_year[161:170] <- 2015
df_JIS_1$published_year[171:181] <- 2016 ; df_JIS_1$published_year[182:189] <- 2017
df_JIS_1$published_year[190:197] <- 2018 ; df_JIS_1$published_year[198:207] <- 2019
df_JIS_1$published_year[208:219] <- 2020 ; df_JIS_1$published_year[220:231] <- 2021


# JIS의 논문 중 Volume 2에 해당하는 논문 정보 수집
url_JIS_2<-"https://meridian.allenpress.com/jis/issue/35/2"
df_JIS_2 <- data.frame()

for (i in c(14:35)){
  url <- paste0(paste0(str_sub(url_JIS_2, end=-5),i),"/2")
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JIS <- xpathSApply(html.parsed, "//div[@class='al-article-items']/h5[@class='customLink item-title']/a", 
                           xmlValue)
  new_2 <- data.frame(title = title_JIS)
  df_JIS_2 <- rbind(df_JIS_2, new_2)
  print(df_JIS_2)
}
df_JIS_2$published_year <- 1:nrow(df_JIS_2)

df_JIS_2$published_year[1:6] <- 2000 ; df_JIS_2$published_year[7:11] <- 2001
df_JIS_2$published_year[12:25] <- 2002 ; df_JIS_2$published_year[26:37] <- 2003
df_JIS_2$published_year[38:50] <- 2004 ; df_JIS_2$published_year[51:63] <- 2005
df_JIS_2$published_year[64:72] <- 2006 ; df_JIS_2$published_year[73:81] <- 2007
df_JIS_2$published_year[82:99] <- 2008 ; df_JIS_2$published_year[100:106] <- 2009
df_JIS_2$published_year[107:114] <- 2010 ; df_JIS_2$published_year[115:127] <- 2011
df_JIS_2$published_year[128:141] <- 2012 ; df_JIS_2$published_year[142:155] <- 2013
df_JIS_2$published_year[156:174] <- 2014 ; df_JIS_2$published_year[175:194] <- 2015
df_JIS_2$published_year[195:205] <- 2016 ; df_JIS_2$published_year[206:216] <- 2017
df_JIS_2$published_year[217:224] <- 2018 ; df_JIS_2$published_year[225:236] <- 2019
df_JIS_2$published_year[237:252] <- 2020

## Dataframe 생성하기
df_JIS <- rbind(df_JIS_1, df_JIS_2)
df_JIS<-df_JIS[order(df_JIS$published_year),] # 연도 별로 정렬하기
row.names(df_JIS) <- 1:nrow(df_JIS) # index값 바꾸기

#####################
#####################
## IJEIS
# 파싱
url_IJEIS <- "https://www.igi-global.com/journal-contents/international-journal-enterprise-information-systems/1086"
html_IJEIS<- read_html(url_IJEIS)
html.parsed_IJEIS <- htmlParse(html_IJEIS)

# Nodeset 추출: IJEIS 논문 제목
title_IJEIS <- xpathSApply(html.parsed_IJEIS,
                           "//div/a",
                           xmlGetAttr, "title")
title_IJEIS <- unlist(title_IJEIS)
title_IJEIS <- title_IJEIS[-c(1,2)]

# Dataframe 생성
df_IJEIS <- data.frame(title = title_IJEIS)
df_IJEIS$published_year <- c(1:nrow(df_IJEIS))

# IJEIS 발행 연도 추가
df_IJEIS$published_year[1:14] <- 2021 ; df_IJEIS$published_year[15:44] <- 2020 ;df_IJEIS$published_year[45:66] <- 2019
df_IJEIS$published_year[67:96] <- 2018 ; df_IJEIS$published_year[97:117] <- 2017; df_IJEIS$published_year[118:137] <- 2016
df_IJEIS$published_year[138:157] <- 2015 ; df_IJEIS$published_year[158:179] <- 2014 ; df_IJEIS$published_year[180:204] <- 2013
df_IJEIS$published_year[205:220] <- 2012 ; df_IJEIS$published_year[221:236] <- 2011 ;df_IJEIS$published_year[237:256] <- 2010
df_IJEIS$published_year[257:276] <- 2009 ; df_IJEIS$published_year[277:297] <- 2008 ; df_IJEIS$published_year[298:316] <- 2007
df_IJEIS$published_year[317:336] <- 2006 ; df_IJEIS$published_year[337:356] <- 2005

#####################
#####################
## JGIM
# 파싱
url_JGIM <- "https://www.igi-global.com/journals/open-access/table-of-contents/journal-global-information-management/1070?v=29"
html_JGIM <- read_html(url_JGIM)
html.parsed_JGIM <- htmlParse(html_JGIM)

# Nodeset 추출: JGIM 논문 제목 & Dataframe 생성
df_JGIM <- data.frame() # 빈 데이터프레임 준비
for (i in c(8:29)){ # Volume 8~29권
  url <- paste0(str_sub(url_JGIM, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JGIM <- xpathSApply(html.parsed, 
                            "//b[@class='margin-right-5']/a[@style='display:inline;']", 
                            xmlValue)
  new <- data.frame(title = title_JGIM)
  df_JGIM <- rbind(df_JGIM, new)
  print(df_JGIM)
}

# JGIM 발행 연도 추가
df_JGIM$published_year <- c(1:nrow(df_JGIM))
df_JGIM$published_year[1:16] <- 2000 ; df_JGIM$published_year[17:30] <- 2001 ; df_JGIM$published_year[31:50] <- 2002
df_JGIM$published_year[51:67] <- 2003 ; df_JGIM$published_year[68:85] <- 2004 ; df_JGIM$published_year[86:100] <- 2005
df_JGIM$published_year[101:114] <- 2006 ; df_JGIM$published_year[115:131] <- 2007 ; df_JGIM$published_year[132:148] <- 2008 
df_JGIM$published_year[149:163] <- 2009 ; df_JGIM$published_year[164:179] <- 2010 ; df_JGIM$published_year[180:194] <- 2011 
df_JGIM$published_year[195:208] <- 2012 ; df_JGIM$published_year[209:227] <- 2013 ; df_JGIM$published_year[228:242] <- 2014
df_JGIM$published_year[243:257] <- 2015 ; df_JGIM$published_year[258:273] <- 2016 ; df_JGIM$published_year[274:300] <- 2017
df_JGIM$published_year[301:338] <- 2018 ; df_JGIM$published_year[339:337] <- 2019 ; df_JGIM$published_year[338:416] <- 2020
df_JGIM$published_year[417:473] <- 2021

#####################
#####################
## JCIT
# 파싱
url_JCIT <- "https://www.igi-global.com/journals/open-access/table-of-contents/journal-cases-information-technology/1075?v=23"
html_JCIT <- read_html(url_JCIT)
html.parsed_JCIT <- htmlParse(html_JCIT)

# Nodeset 추출: JCIT 논문 제목 & Dataframe 생성
df_JCIT <- data.frame()
for (i in c(2:23)){ # Volume 2~23권 추출
  url <- paste0(str_sub(url_JCIT, end=-3), i)
  html <- read_html(url)
  html.parsed <- htmlParse(html)
  title_JCIT <- xpathSApply(html.parsed, 
                            "//b[@class='margin-right-5']/a[@style='display:inline;']", 
                            xmlValue)
  new <- data.frame(title = title_JCIT)
  df_JCIT <- rbind(df_JCIT, new)
  print(df_JCIT)
}

# JCIT 발행 연도 추가
df_JCIT$published_year <- c(1:nrow(df_JCIT))
df_JCIT$published_year[1:14] <- 2000 ; df_JCIT$published_year[15:36] <- 2001 ; df_JCIT$published_year[37:72] <- 2002 
df_JCIT$published_year[73:109] <- 2003 ; df_JCIT$published_year[110:145] <- 2004 ; df_JCIT$published_year[146:176] <- 2005
df_JCIT$published_year[177:200] <- 2006 ; df_JCIT$published_year[201:223] <- 2007 ; df_JCIT$published_year[224:244] <- 2008
df_JCIT$published_year[245:264] <- 2009 ; df_JCIT$published_year[265:284] <- 2010 ; df_JCIT$published_year[285:303] <- 2011
df_JCIT$published_year[304:323] <- 2012 ; df_JCIT$published_year[324:342] <- 2013 ; df_JCIT$published_year[343:364] <- 2014 
df_JCIT$published_year[365:385] <- 2015 ; df_JCIT$published_year[386:404] <- 2016 ; df_JCIT$published_year[405:425] <- 2017
df_JCIT$published_year[426:444] <- 2018 ; df_JCIT$published_year[445:466] <- 2019 ; df_JCIT$published_year[467:484] <- 2020
df_JCIT$published_year[485:497] <- 2021

#####################
#####################

# Dataframe 파일로 저장
write.csv(df_MISQ, file = "MISQ.csv")
write.csv(df_ISR, file = "ISR.csv")
write.csv(df_JMIS, file = "JMIS.csv")
write.csv(df_JIS, file = "JIS.csv")
write.csv(df_IJEIS, file = "IJEIS.csv")
write.csv(df_JGIM, file = "JGIM.csv")
write.csv(df_JCIT, file = "JCIT.csv")






