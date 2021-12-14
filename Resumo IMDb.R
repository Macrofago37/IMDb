library(tidyverse)
library(rvest)



Find_movie <- function(movie){
  #Arruma o nome do filme caso tenha espaço
  if(movie %>% grepl(" ", ., fixed=T)){
    movie <- movie %>% str_replace(" ", "+")
  }
  #Pesquisa no imdb o filme
  link_search <- c("https://www.imdb.com/find?q=", movie,"&s=tt&ttype=ft&ref_=fn_ft")
  
  #extrai o código do filme do objeto elemento td
  movie_id <- link_search %>% paste(collapse="") %>% read_html() %>% html_elements("td") %>% #no td está o nome do filme e o link para a página dele
  html_elements("a") %>% pluck(1) %>% toString() %>% substring(., regexpr("/title", .), regexpr(">", .)-2 )
  
  
  #cria o link do filme
  movie_link <<-  c("https://www.imdb.com", movie_id) %>% paste(collapse="") %>% read_html() 
  
  #Extrai nome do filme
  nome <- movie_link %>% html_elements("div") %>% pluck(grep("OriginalTitle__OriginalTitleText",.) %>% tail(1)) %>%
    html_text2() %>% substring(., regexpr(":",.)+2)
  
  #Extrai diretor do filme
  diretor <- movie_link %>% html_elements("li") %>% pluck(grep("ipc-metadata-list-item",.)[1]) %>%
    html_text2() %>% substring(., regexpr("\n",.)+1)
  
  #Extrai ano do filme
  
  ano <- movie_link %>% html_elements("li") %>% html_elements("span") %>% pluck(grep("TitleBlockMetaData",.)[1]) %>% 
    html_text2()
  
  #Runtime
  runtime <- movie_link %>% html_elements("li") %>% pluck(grep("ipc-inline-list__item",.)[3]) %>% html_text2() 
  
  #Extrai a nota do filme
  nota <- movie_link %>% html_elements("span") %>% pluck(grep("AggregateRating",.)[1]) %>% html_text2()
  
  #Resume as informações
  Resumo <- tibble(Nome=nome,
                   Diretor=diretor,
                   Ano=ano,
                   Duracao=runtime,
                   Nota=nota)
  return(Resumo)
  
}
