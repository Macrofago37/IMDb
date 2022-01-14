library(shiny)
library(tidyverse)
library(rvest)

Find_movie <- function(movie){
  #Arruma o nome do filme caso tenha espaço
  if(movie %>% grepl(" ", ., fixed=T)){
    movie <- movie %>% str_replace_all(" ", "+")
  }
  #Pesquisa no imdb o filme
  link_search <- c("https://www.imdb.com/find?q=", movie,"&s=tt&ttype=ft&ref_=fn_ft")
  
  #extrai o código do filme do objeto elemento td
  movie_id <- link_search %>% paste(collapse="") %>% read_html() %>% html_elements("td") %>% #no td está o nome do filme e o link para a página dele
    html_elements("a") %>% pluck(1) %>% toString() %>% substring(., regexpr("/title", .), regexpr(">", .)-2 )
  
  
  #cria o link do filme
  movie_link <-  c("https://www.imdb.com", movie_id) %>% paste(collapse="") %>% read_html() 
  
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
                   Nota=nota %>% as.numeric())
  Resumo
  
}

ui <- fluidPage(
 tags$head(
   tags$style(
     HTML(
       "@import url('https://fonts.googleapis.com/css2?family=Anton&family=Bebas+Neue&display=swap');
       h1{font-family: 'Anton', sans-serif;
       display: inline;
       color: #002B3B;
       font-size:100px;
       }
       h2{font-family: 'Bebas Neue', cursive;
       color:#E41E75;
       display: inline;
       font-size:40px;}"))),
     
 
  titlePanel(div(h1("IMDb"),h2("RESUMO"))),
  sidebarLayout(
    sidebarPanel(
  textInput(inputId = "Nome_filme", label= "Pesquisar filme", value= "Nome do filme..."),
  actionButton("goButton", "Pesquisar")),
  mainPanel(tableOutput("Find_movie"))
))


server <- function(input, output) {
  movie <- reactiveValues()
  
  observeEvent(input$goButton,{
    movie$Nome_filme <- input$Nome_filme})
    output$Find_movie <- renderTable({
      req(movie$Nome_filme)
      Find_movie(movie$Nome_filme)
    })
    }
      
 
   

    
shinyApp(ui = ui, server = server)


