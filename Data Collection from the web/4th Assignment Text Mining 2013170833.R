library(dplyr)
library(stringr)
library(httr)
library(rvest)

url <- 'https://arxiv.org/search/?query="deep+learning"&searchtype=title&source=header&order=-announced_date_first&size=50&abstracts=show&start=0'
extract_amount <- read_html(url) %>% html_nodes('h1.title.is-clearfix') %>% html_text(T)
extract_amount <- lapply(strsplit(extract_amount, ' ', fixed = T),'[',4) %>% unlist %>% str_trim
num_paper <- gsub(',','', extract_amount)
num_paper <- strtoi(num_paper)

query_size <- 50
num_page <- as.integer(num_paper/query_size) +1

start <- proc.time()
title <- NULL
author <- NULL
subject <- NULL
abstract <- NULL
meta <- NULL

for( i in seq(0, num_paper, by = query_size)){
  
  tmp_url <- modify_url(url, query = list(start = i))
  tmp_list <- read_html(tmp_url) %>% html_nodes('p.list-title.level-left') %>% html_text(T)
  tmp_list <- lapply(strsplit(gsub('arXiv:', '', tmp_list), '\n', fixed = T),'[',1) %>% unlist
  tmp_list <- paste0('https://arxiv.org/abs/',tmp_list)
  
  for(j in 1:length(tmp_list)){ 
    
    tmp_paragraph <- read_html(tmp_list[j])
    
    tmp_title <- gsub('Title:', '',tmp_paragraph %>% html_nodes('h1.title.mathjax') %>% html_text(T))
    title <- c(title, tmp_title) 
    
    tmp_author <- tmp_paragraph %>% html_nodes('div.authors') %>% html_text
    tmp_author <- gsub('\\s+',' ',tmp_author)
    tmp_author <- gsub('Authors:','',tmp_author) %>% str_trim
    author <- c(author, tmp_author)  
    
    tmp_subject <- tmp_paragraph %>% html_nodes('td.tablecell.subjects') %>% html_text(T)
    subject <- c(subject, tmp_subject)
    
    tmp_abstract <- tmp_paragraph %>% html_nodes('blockquote.abstract.mathjax') %>% html_text(T)
    tmp_abstract <- sub('Abstract:','',tmp_abstract) %>% str_trim
    tmp_abstract <- gsub('\\s+',' ',tmp_abstract) 
    abstract <- c(abstract, tmp_abstract)
    
    tmp_meta <- tmp_paragraph %>% html_nodes('div.submission-history') %>% html_text
    tmp_meta <- lapply(strsplit(gsub('\\s+', ' ',tmp_meta), '[v1]', fixed = T),'[',2) %>% unlist %>% str_trim
    meta <- c(meta, tmp_meta)
    cat(j, "paper\n")
    Sys.sleep(1)
  }
  cat((i/query_size) + 1,'/', num_page, 'page\n')
  Sys.sleep(10)
}
final <- data.frame(title, author, subject, abstract, meta)
end <- proc.time()
end - start # Total Elapsed Time

# Export the result
write.csv(final, file = "4th Assignment Text Mining 2013170833.csv")