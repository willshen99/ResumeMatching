Does My Resume Match the Jobs I Apply?
================
Qiwei Shen
2021-08-01

# Overview

In this project, I will see what frequent keywords the selected job
postings contains, what keywords my resume contains, and whether they
match.

Analyze Jobs: Analyze what are some frequent terms in selected job
postings. Analyze Resume: Analyze what are some frequent terms in my
resume. Compare Jobs and Resume: Compare how my resume matches these job
postings.

## Load Libraries

There is a bug in wordcloud2 which prevents it from knitting more than 1
wordcloud, so we want to install wordcloud2 using devtools from github
using devtools::install\_github(“gaospecial/wordcloud2”). Just do this
once. if you don’t have devtools installed, install it first then
install wordcloud2 like this.

devtools::install\_github(“gaospecial/wordcloud2”)

``` r
library(tidyverse)
library(readxl)
library(tidytext)
library(wordcloud2)
library(janitor)
library(pdftools)
```

------------------------------------------------------------------------

# Part 1. Analyze Jobs

## 1. Import Job Description

1.  Create jobs dataframe by using read\_excel() to read in your
    MSBA30-50Jobs.xlsx (after you’ve loaded it with jobs), you can get
    started by using the MSBA20JobDescription.xlsx file if you want.
2.  I recommend piping it to clean\_names() to deal with column names
3.  Print the first 5 jobs to make sure your import works.

``` r
jobs <- read_xlsx('My20JobDescriptions.xlsx')%>%
  clean_names()

head(jobs,5)
```

    ## # A tibble: 5 x 4
    ##   title           firm          job_description                         location
    ##   <chr>           <chr>         <chr>                                   <lgl>   
    ## 1 Sales Analyst   Henkel        "At Henkel, you can make a difference ~ NA      
    ## 2 PDM Data and R~ Accenture     "Accenture is a global professional se~ NA      
    ## 3 Data Scientist  Volvo Financ~ "Position Description\r\n\r\nThe Data ~ NA      
    ## 4 Data Visualiza~ Wellnecity    "Wellnecity provides innovative data i~ NA      
    ## 5 Data Insights ~ Epic Games    "Love data and how data can inspire an~ NA

## 2. Job Description Term & Bigram Frequency Analysis

------------------------------------------------------------------------

### Create a table of term frequencies for job description

The following will make a term\_frequency table.

``` r
excludes <- c("you'll","including","experience","requirements","required","preferred","responsibilities","ability","knowledge") #vector of excluding words
term_frequency<-jobs%>%
  unnest_tokens(word,job_description)%>%
  anti_join(stop_words, by = c("word" = "word"))%>% #remove common words
  filter(!word %in% excludes )%>%
  filter(!str_detect(word,"^\\d"))%>% #remove digits
  count(word,sort = T)
term_frequency%>%
  top_n(20)
```

    ## Selecting by n

    ## # A tibble: 20 x 2
    ##    word               n
    ##    <chr>          <int>
    ##  1 data             192
    ##  2 business          88
    ##  3 team              60
    ##  4 skills            54
    ##  5 analytics         38
    ##  6 management        32
    ##  7 analysis          29
    ##  8 clients           29
    ##  9 information       25
    ## 10 teams             25
    ## 11 technical         24
    ## 12 science           23
    ## 13 development       22
    ## 14 solutions         22
    ## 15 degree            21
    ## 16 develop           20
    ## 17 financial         20
    ## 18 qualifications    20
    ## 19 support           20
    ## 20 systems           20

### Word Cloud of Job Description terms

``` r
term_frequency%>%
  wordcloud2()
```

![](4_job_descriptions_and_resume_Qiwei_Shen_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Words after DATA

Clearly “DATA” an important word so what words come after data?

``` r
data_term_frequency <- jobs%>%
  unnest_tokens(bigram, job_description, token = "ngrams", n = 2, n_min = 2)%>% #bigram is all the sequences of two adjacent elements from strings in 'tittle'
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(word1 == "data") %>%
  filter(!word2 %in% excludes )%>%
  anti_join(stop_words, by = c("word2" = "word"))%>%
  unite(bigram,word1,word2,sep=" ")%>%
  group_by(bigram)%>%
  summarise(n=n())%>%
  arrange(desc(n))

head(data_term_frequency,10)
```

    ## # A tibble: 10 x 2
    ##    bigram                 n
    ##    <chr>              <int>
    ##  1 data analysis          9
    ##  2 data analytics         9
    ##  3 data science           9
    ##  4 data scientist         6
    ##  5 data visualization     6
    ##  6 data sets              5
    ##  7 data analyst           4
    ##  8 data integrity         4
    ##  9 data management        4
    ## 10 data mining            4

### Create a word cloud of data + term combinations

``` r
data_term_frequency%>%
  wordcloud2()
```

![](4_job_descriptions_and_resume_Qiwei_Shen_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## 3. Technology Term Analysis

What technology term appears most in the job description?

``` r
technology_words <- c(
    "analytics", 
    "data",
    "analyze",
    "r", 
    "python", 
    "sql", 
    "excel", 
    "cloud",
    "aws",
    "azure",
    "ec2",
    "sas",
    "spss",
    "saas",
    "spark",
    "tensorflow",
    "sagemaker",
    "tableau",
    "hadoop",
    "pyspark",
    "h2o.ai",
    "spark", 
    "ai",
    "shiny",
    "dash",
    "pca",
    "k-means",
    "emr",
    "mapreduce",
    "nosql",
    "hive",
    "etl"
    )


technology_bigram <- c(
  "amazon web",
  "big data",
  "business analytics",
  "google cloud",
  "microsoft azure",
  "machine learning",
  "data science",
  "deep learning",
  "neural network",
  "neural networks",
  "neural nets",
  "random forests",
  "random forest",
  "elastic search",
  "map reduce",
  "artificial intelligence",
  "data visualization"
)
```

### Create Word & Bi-Gram Frequencies - Jobs

What word & bi-gram appear most frequently in job description?

``` r
tech_word_freq <- term_frequency%>%
  filter(word %in% technology_words)
bigram_frequency <- jobs%>%
  unnest_tokens(bigram,job_description,token="ngrams",n=2)%>%
  separate(bigram, c("word1", "word2"), sep = " ")%>%
  filter(!word1 %in% excludes )%>%
  filter(!word2 %in% excludes )%>%
  anti_join(stop_words, by = c("word1" = "word"))%>%
  anti_join(stop_words, by = c("word2" = "word"))%>%
  unite(bigram,word1,word2,sep=" ")%>%
  group_by(bigram)%>%
  summarise(n=n())%>%
  ungroup()%>%
  arrange(desc(n))
  
tech_bigram_freq<-bigram_frequency%>%
  filter(bigram %in% technology_bigram)

colnames(tech_bigram_freq)[1]='term'
colnames(tech_word_freq)[1]='term'
technology_term_frequency <- tech_bigram_freq%>%
  bind_rows(tech_word_freq)%>%
  arrange(-n)
technology_term_frequency
```

    ## # A tibble: 23 x 2
    ##    term                   n
    ##    <chr>              <int>
    ##  1 data                 192
    ##  2 analytics             38
    ##  3 sql                   14
    ##  4 python                12
    ##  5 analyze               11
    ##  6 data science           9
    ##  7 machine learning       9
    ##  8 excel                  9
    ##  9 tableau                8
    ## 10 data visualization     6
    ## # ... with 13 more rows

# Part 2. Analyze Resume

------------------------------------------------------------------------

## 1. Read Resume

``` r
resume <- pdf_text("QiweiShen_resume_202110.pdf")
```

## 2. What are most frequent words in my resume?

``` r
excludes=c(str_to_lower(month.name), #exclude month
           "china","shanghai","canada","apac","nc","school","university","fudan","label")
resume_word_freq<-resume%>%
  as_tibble(resume)%>% #convert into data frame
  unnest_tokens(word,value)%>% #parse into words
  anti_join(stop_words)%>%
  filter(!word %in% excludes )%>% 
  filter(!str_detect(word,"^\\d"))%>% #remove numbers
  count(word,sort=T)
```

    ## Joining, by = "word"

``` r
resume_word_freq
```

    ## # A tibble: 205 x 2
    ##    word           n
    ##    <chr>      <int>
    ##  1 data           7
    ##  2 financial      7
    ##  3 business       5
    ##  4 funds          3
    ##  5 industry       3
    ##  6 intern         3
    ##  7 investment     3
    ##  8 management     3
    ##  9 python         3
    ## 10 research       3
    ## # ... with 195 more rows

## 3. Create a word cloud of the remaining words in my resume

``` r
resume_word_freq%>%
  filter(n>2)%>%
  wordcloud2()
```

![](4_job_descriptions_and_resume_Qiwei_Shen_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## 4. What technology\_words and technology\_bigrams I use in my resume?

### technology\_bigrams used in resume

``` r
resume_bigram_freq<-as_tibble(resume)%>%
  unnest_tokens(word, value, token = "ngrams", n = 2, n_min = 2)%>%
  filter(word %in% technology_bigram )%>%
  group_by(word)%>%
  summarize(n=n()) %>%
  arrange(desc(n)) 
resume_bigram_freq
```

    ## # A tibble: 3 x 2
    ##   word                   n
    ##   <chr>              <int>
    ## 1 data visualization     2
    ## 2 business analytics     1
    ## 3 machine learning       1

### technology\_words used in resume

``` r
#tech word in resume
resume_tech_word_freq <- as_tibble(resume)%>%
  unnest_tokens(word, value, token = "words")%>%
  filter(word %in% technology_words )%>%
  group_by(word)%>%
  summarize(n=n()) %>%
  arrange(desc(n)) 
resume_tech_word_freq
```

    ## # A tibble: 8 x 2
    ##   word          n
    ##   <chr>     <int>
    ## 1 data          7
    ## 2 r             4
    ## 3 python        3
    ## 4 analytics     1
    ## 5 excel         1
    ## 6 shiny         1
    ## 7 sql           1
    ## 8 tableau       1

### What technology terms(words + bi-grams) used in my resume, and how frequently they are used?

``` r
#combine words and bigrams
resume_tech_freq <- resume_tech_word_freq%>%
  bind_rows(resume_bigram_freq)

resume_tech_freq%>%
  ggplot(aes(x=reorder(word,n),y=n))+
  geom_bar(stat="identity")+
  coord_flip()
```

![](4_job_descriptions_and_resume_Qiwei_Shen_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### make a wordcloud of combined technolgy term frequencies

``` r
resume_tech_freq%>%
  wordcloud2()
```

![](4_job_descriptions_and_resume_Qiwei_Shen_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

# Part 3. Compare job description and resume

## 1. What terms do my resume and jobs data have in common?

``` r
resume_word_freq%>%
  inner_join(term_frequency,by='word',suffix=c("_resume","_job"))%>%
  arrange(-n_resume)
```

    ## # A tibble: 120 x 3
    ##    word       n_resume n_job
    ##    <chr>         <int> <int>
    ##  1 data              7   192
    ##  2 financial         7    20
    ##  3 business          5    88
    ##  4 industry          3     8
    ##  5 investment        3     2
    ##  6 management        3    32
    ##  7 python            3    12
    ##  8 research          3    10
    ##  9 analyst           2    13
    ## 10 department        2     6
    ## # ... with 110 more rows

## 2. Based on the job’s terms, what terms are missing from my resume?

``` r
term_frequency%>%
  anti_join(resume_word_freq,by='word')%>%
  arrange(-n)
```

    ## # A tibble: 1,758 x 2
    ##    word               n
    ##    <chr>          <int>
    ##  1 information       25
    ##  2 teams             25
    ##  3 development       22
    ##  4 solutions         22
    ##  5 degree            21
    ##  6 develop           20
    ##  7 qualifications    20
    ##  8 support           20
    ##  9 systems           20
    ## 10 communication     19
    ## # ... with 1,748 more rows

## 3. What tech-skills does my resume and jobs have in common?

``` r
resume_tech_freq%>%
  inner_join(technology_term_frequency,by=c('word'='term'),suffix =c('_resume','_job') )%>%
  arrange(-n_resume)
```

    ## # A tibble: 9 x 3
    ##   word               n_resume n_job
    ##   <chr>                 <int> <int>
    ## 1 data                      7   192
    ## 2 python                    3    12
    ## 3 data visualization        2     6
    ## 4 analytics                 1    38
    ## 5 excel                     1     9
    ## 6 sql                       1    14
    ## 7 tableau                   1     8
    ## 8 business analytics        1     2
    ## 9 machine learning          1     9

## 4. Based on the job’s tech-skills what skills are missing from your resume?

``` r
technology_term_frequency%>%
  anti_join(resume_tech_freq,by=c('term'='word'))%>%
  arrange(-n)
```

    ## # A tibble: 14 x 2
    ##    term                        n
    ##    <chr>                   <int>
    ##  1 analyze                    11
    ##  2 data science                9
    ##  3 sas                         5
    ##  4 ai                          3
    ##  5 cloud                       3
    ##  6 hadoop                      3
    ##  7 hive                        3
    ##  8 spark                       3
    ##  9 deep learning               2
    ## 10 artificial intelligence     1
    ## 11 neural networks             1
    ## 12 etl                         1
    ## 13 mapreduce                   1
    ## 14 tensorflow                  1
