#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(warn=-1)
require(shiny)
install.packages("dplyr") 
require(dplyr)
install.packages("tidytext") 
require(tidytext)
install.packages("janeaustenr") 
require(janeaustenr)
install.packages("tidyr") 
require(tidyr)
install.packages("ggplot2") 
require(ggplot2)
install.packages("tidyverse") 
require(tidyverse)
install.packages("stopwords") 
require(stopwords)
install.packages("wordcloud2") 
require(wordcloud2)
install.packages("tm") 
require(tm)
install.packages("rvest") 
require(rvest)
install.packages("stringr") 
require(stringr)
install.packages("data.table") 
require(data.table)
install.packages("textdata") 
require(textdata)
install.packages("shinydashboard") 
require(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Phone Analysis"),
                    dashboardSidebar(
                      sidebarMenu(
                        #Menu premier onglet : analyse des avis des telephones pre-enregistres
                        menuItem("Sentiment", icon = icon("dashboard"), startExpanded = TRUE,
                                 menuSubItem('Sentiment reviews by phone',tabName='Sentiment'),
                                 selectInput(inputId = "SelectProduct", label = "Sélectionnez le produit", selected = 1,
                                             choices = c('Alcatel OneTouch Idol' = 1, 'Iphone 6S' = 2,'Iphone 7' = 3,'LG G4' = 4,'Samsung S9' = 5,'Galaxy Note 9' = 6,'Motorola G6' = 7,'Huawei Mate SE' = 8)),
                                 actionButton('Action_sent', 'Launch', icon = icon('hand-point-right')),
                                 selected = TRUE
                        ),
                        #Menu deuxieme onglet : recommandation pour un produit
                        menuItem("Recommendation", icon = icon('greater-than-equal'), startExpanded = TRUE,
                                 
                                 menuSubItem('Ouput recommendation',tabName='Recommendation'),
                                 selectInput(inputId = "processor",
                                             label = "Processor (number of cores)",
                                             choices = c("Dual" = 1, "Quad" = 2, "Hexa" = 3, "Octa" = 4),
                                             selected = 1),
                                 
                                 selectInput(inputId = "memory",
                                             label = "Memory (GB)",
                                             choices = c("1" = 1, "2" = 2, "3" = 3, "4" = 4),
                                             selected = 1),
                                 
                                 selectInput(inputId = "internal_storage",
                                             label = "Internal storage (GB)",
                                             choices = c("8" = 1, "16" = 2, "32" = 3, "64" = 4, "128" = 5, "256" = 6),
                                             selected = 1),
                                 
                                 sliderInput(inputId = "screen",
                                             label = "Choose a size for your screen (inches)",
                                             value = 4.5, min = 4, max = 6, step = 0.5),
                                 
                                 selectInput(inputId = "front_camera",
                                             label = "Front camera (Megapixels)",
                                             choices = c("5" = 1, "12" = 2, "16" = 3),
                                             selected = 1),
                                 
                                 selectInput(inputId = "back_camera",
                                             label = "Back camera (Megapixels)",
                                             choices = c("2" = 1, "5" = 2, "7" = 3, "8" = 4, "16" = 5),
                                             selected = 1),
                                 actionButton('Action_reco', 'Launch', icon = icon('hand-point-right')),
                                 selected = FALSE
                        ),
                        #Menu troisieme onglet : urlde la page principale amazon d'un nouveau telephone
                        menuItem("Try a new phone!", icon = icon("mobile alt"), startExpanded = T,
                                 menuSubItem('Sentiment reviews - New phone',tabName='New_phone'),
                                 textInput("new_url", label='Url of your phone',value = "url"),
                                 textInput("FiveS", label='Number of 5 stars reviews',value = "1"),
                                 textInput("OneS", label='Number of 1 star reviews',value = "1"),
                                 actionButton('Action_new', 'Launch', icon = icon('hand-point-right')),
                                 selected = FALSE
                        )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(
                          #Corps du premier onglet : analyse des avis des telephones pre-enregistres
                          tabName = "Sentiment",
                          fluidRow(
                            box(title = "Critics rating :",verbatimTextOutput(outputId = "PP"), background = 'aqua', width = 5),
                            box(title = 'General sentiment', plotOutput("AS"),background = 'orange'),
                            box(title = "Most common words in negative reviews",wordcloud2Output("WCN"), background = 'red',width = 5),
                            box(title = "Most common words in positive reviews",wordcloud2Output("WCP"), background = 'olive',width = 5)
                          )
                        ),
                        tabItem(
                          #Corps du deuxieme onglet : recommandation pour un produit
                          tabName = "Recommendation",
                          fluidRow(
                            valueBoxOutput("process"),
                            valueBoxOutput("memory"),
                            valueBoxOutput("storage"),
                            valueBoxOutput("bcamera"),
                            valueBoxOutput("fcamera"),
                            valueBoxOutput("screen")),
                          fluidRow(
                            column(
                              12,
                              align = 'center',
                              box(tableOutput(outputId = "resultat"),width = 14, title = 'Best phone(s) regarding the selected options', background = 'olive')
                            )
                          )
                        ),
                        tabItem(
                          #Corps du troisieme onglet : urlde la page principale amazon d'un nouveau telephone
                          tabName = "New_phone",
                          fluidRow(
                            box(title = "Critics rating :",verbatimTextOutput(outputId = "NPP"), background = 'aqua', width = 5),
                            box(plotOutput("NAS"), background = 'orange'),
                            box(title = "Most common words in negative reviews",wordcloud2Output("NWCN"), background = 'red',width = 5),
                            box(title = "Most common words in positive reviews",wordcloud2Output("NWCP"), background = 'olive',width = 5)
                            
                          )
                          
                        )
                      )
                    )
)


#Partie server
server <- function(input, output){
  ######################################################Fonctions#################################################
  ###############################
  #Scrap tous les avis d'une page
  ###############################
  
  recup_page_sent <- function(url){
    page <- xml2::read_html(url)
    text <- html_text(page)
    
    #Trouve les indices de debut et de fin des avis
    locad_avis <- as.data.table(str_locate_all(pattern ='Verified Purchase', text)[[1]])
    locaf_avis <- as.data.table(str_locate_all(pattern ='Helpful', text)[[1]])
    
    #Creation du vecteur qui contiendra les avis
    avis <- ''
    
    #Scrap les avis de la page
    for (i in 1:dim(locad_avis)[1]){
      avis <- paste(avis,strsplit(str_sub(text, locad_avis$end[i],locaf_avis$start[i]),"\n")[[1]][1])
    }
    return(avis)
  }
  
  #################################
  #Scrap tous les avis d'un produit
  #################################
  recup_produit_sent <- function(url,nb_avis,sentiment='positive'){
    if (is.na(str_locate(pattern = 'amazon.com', string = url)[1])){
      return('No_new_url')
    }
    
    #Recuperation des infos produits
    product <- str_sub(url, str_locate(pattern='amazon.com/',url)[2]+1, str_locate(pattern='/dp',url)[1]-1)
    ref <- str_sub(url, str_locate(pattern='dp/',url)[2]+1, str_locate(pattern='/ref=',url)[1]-1)
    
    #Indique quel type d'avis va etre recueillis pour adapter l'url
    if (sentiment=='positive'){
      url <- paste('https://www.amazon.com/',product,'/product-reviews/',ref,'/ref=cm_cr_arp_d_viewopt_sr?ie=UTF8&reviewerType=all_reviews&filterByStar=five_star&pageNumber=1',sep='')
    }
    else {
      url <- paste('https://www.amazon.com/',product,'/product-reviews/',ref,'/ref=cm_cr_arp_d_viewopt_sr?ie=UTF8&reviewerType=all_reviews&filterByStar=one_star&pageNumber=1',sep='')
    }
    
    #Combien de pages à scraper selon le nombre d'avis
    nb_avis <- as.integer(nb_avis)
    if (nb_avis%%2 == 0){
      nb_pages <- as.integer(nb_avis/10)
    } else {
      nb_pages <- as.integer(nb_avis/10)+1
    }
    
    #Scrap des pages
    withProgress(message = paste('Scrapping',sentiment,'reviews'), value = 0, {
      total_avis <- ''
      n <- nb_pages
      prog <- 0
      for (i in sample(x = seq(1,nb_pages,by = 1), size = nb_pages)){
        prog <- prog + 1
        incProgress(1/n, detail = paste("Scrapping page", prog))
        if (i %% 3){
          Sys.sleep(3)
        }
        urlp <- str_replace_all(url,'pageNumber=1',paste('pageNumber=',as.character(i),sep=''))
        Sys.sleep(2)
        total_avis <- paste(total_avis,recup_page_sent(urlp))
      }
      
    })
    
    #Nettoyage des donnees
    total_avis <- str_remove_all(total_avis,'NA')
    total_avis <- tolower(total_avis)
    total_avis <- str_replace_all(total_avis,'egood','good')
    total_avis <- str_replace_all(total_avis,'egreat','great')
    total_avis <- str_replace_all(total_avis,'eexcelente','excelente')
    total_avis <- str_replace_all(total_avis,'eexcelent','excelent')
    total_avis <- str_replace_all(total_avis,'eexcellent','excelent')
    total_avis <- str_replace_all(total_avis,'ebuen','buen')
    total_avis <- str_replace_all(total_avis,'eworks','work')
    total_avis <- str_replace_all(total_avis,'ethe','the')
    total_avis <- str_replace_all(total_avis,'ethis','this')
    total_avis <- str_replace_all(total_avis,'eit','it')
    total_avis <- str_replace_all(total_avis,'ei','i')
    total_avis <- removeWords(total_avis,stopwords(kind = 'en'))
    total_avis <- gsub('[0-9]+','',total_avis) 
    total_avis <- removePunctuation(total_avis)
    total_avis <- gsub("[^\x01-\x7F]", "", total_avis)
    return(total_avis)
  }
  
  ########################
  #Creation d'un WordCloud
  ########################
  CreateWordCloud <- function(texte){
    avis <- tibble(txt = texte)
    bigrams <- avis %>%
      unnest_tokens(bigram, txt, token = "ngrams", n = 2)
    bigram_counts <- bigrams %>%count(bigram,sort=TRUE)
    return(wordcloud2(bigram_counts[1:30,],shuffle = FALSE))
  }
  
  ###################################
  #Creation de l'analyse de sentiment
  ###################################
  
  CreateAnalysisSentiment <- function(avis){
    avis <- tibble(txt = avis)
    
    AFINN <-  get_sentiments('afinn') 
    
    bigrams2 <- avis %>%
      unnest_tokens(bigram, txt, token = "ngrams", n = 2)
    
    bigrams_separated <- bigrams2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    bigrams_separated
    
    bigrams_separated %>%
      count(word1, word2, sort = TRUE)
    
    words <- bigrams_separated %>%
      inner_join(AFINN, by = c(word2 = "word")) %>%
      count(word2, value, sort = TRUE)
    
    words %>%
      mutate(contribution = n * value) %>%
      arrange(desc(abs(contribution))) %>%
      head(20) %>%
      mutate(word2 = reorder(word2, contribution)) %>%
      ggplot(aes(word2, n * value, fill = n * value > 0)) +
      geom_col(show.legend = FALSE) +
      xlab("Sentiments") +
      ylab("Occurrences") +
      coord_flip()
  }
  
  #######################
  #Indice de contentement
  #######################
  CriticsRate <- function(texte){
    
    avis2 <- tibble(txt = texte)
    
    AFINN <-  get_sentiments('afinn') 
    
    bigrams2 <- avis2 %>%
      unnest_tokens(bigram, txt, token = "ngrams", n = 2)
    
    bigrams_separated <- bigrams2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_separated %>%
      count(word1, word2, sort = TRUE)
    
    words <- bigrams_separated %>%
      inner_join(AFINN, by = c(word2 = "word")) %>%
      count(word2, value, sort = TRUE)
    
    words %>%
      mutate(contribution = n * value) %>%
      arrange(desc(abs(contribution))) %>%
      head(30) %>%
      mutate(word2 = reorder(word2, contribution))
    
    aa=words %>%
      mutate(contribution = n * value) 
    bb=round((sum(aa$contribution[which(aa$contribution>0)]) / sum(abs((aa$contribution))))*100)
    cat(bb,"%")}
  
  
  ################################################Reactives###############################################
  avis <- reactive({
    read.table('C:/3A/Données massives sous R/Projet/Avis.csv',sep=';',header = TRUE)
  })
  
  avis_new_pos <- reactive({
    input$Action_new
    isolate(recup_produit_sent(url = input$new_url,nb_avis = input$FiveS,sentiment = 'positive'))
    
  })
  
  avis_new_neg <- reactive({
    input$Action_new
    isolate(recup_produit_sent(url = input$new_url,nb_avis = input$OneS, sentiment ='negative'))
  })
  
  tab_recommandation <- reactive({
    read.table("C:/3A/Données massives sous R/Projet/tab_recommandation.csv", sep = " ", header = T)
  })
  
  ###############################################Plots#####################################################
  
  ###########################################Premier onglet################################################
  
  #########################
  #Word Cloud Avis positifs
  #########################
  output$WCP <- renderWordcloud2({
    avis_tot <- avis()
    input$Action_sent
    isolate(CreateWordCloud(as.character(avis_tot$Avis_pos[as.integer(input$SelectProduct)])))
    
  })
  
  #########################
  #Word Cloud Avis négatifs
  #########################
  output$WCN <- renderWordcloud2({
    avis <- avis()
    input$Action_sent
    isolate(CreateWordCloud(as.character(avis$Avis_neg[as.integer(input$SelectProduct)])))
    
  })
  
  ###################
  #Analyse sentiments
  ###################
  output$AS <- renderPlot({
    avis2 <- avis()
    input$Action_sent
    isolate(CreateAnalysisSentiment(paste(avis2$Avis_pos[as.integer(input$SelectProduct)],avis2$Avis_neg[as.integer(input$SelectProduct)])))
  })
  
  ###############
  #Critics rating
  ###############
  output$PP <- renderPrint({
    
    avis2 <- avis()
    input$Action_sent
    isolate(CriticsRate(paste(avis2$Avis_pos[as.integer(input$SelectProduct)],avis2$Avis_neg[as.integer(input$SelectProduct)])))
    
  })
  
  ###########################################Deuxieme onglet################################################
  
  ###############
  #Recommendation
  ###############
  output$resultat <- renderTable({
    choice_processor <- c("Dual", "Quad", "Hexa", "Octa")
    choice_memory <- c("1", "2", "3", "4")
    choice_internal_storage <- c("8", "16", "32", "64", "128", "256")
    choice_front_camera <- c("5", "12", "16")
    choice_back_camera <- c("2", "5", "7", "8", "16")
    tab_recommandation <- tab_recommandation()
    compteur = c(rep(0, nrow(tab_recommandation)))
    input$Action_reco
    isolate(
      for (i in 1:nrow(tab_recommandation)) {
        if(str_detect(tab_recommandation$Processor[i], pattern = choice_processor[as.integer(input$processor)]) == T){
          compteur[i] = compteur[i] + 1
        }
        if(str_detect(tab_recommandation$Memory[i], pattern = choice_memory[as.integer(input$memory)]) == T){
          compteur[i] = compteur[i] + 1
        }
        if(str_detect(tab_recommandation$Internal_storage[i], pattern = choice_internal_storage[as.integer(input$internal_storage)]) == T){
          compteur[i] = compteur[i] + 1
        }
        if(str_detect(tab_recommandation$Screen[i], pattern = as.character(input$screen)) == T){
          compteur[i] = compteur[i] + 1
        }
        if(str_detect(tab_recommandation$Front_camera[i], pattern = choice_front_camera[as.integer(input$front_camera)]) == T){
          compteur[i] = compteur[i] + 1
        }
        if(str_detect(tab_recommandation$Back_camera[i], pattern = choice_back_camera[as.integer(input$back_camera)]) == T){
          compteur[i] = compteur[i] + 1
        }
      })
    
    produit_recommand <- which(compteur==max(compteur))
    return(tab_recommandation[produit_recommand,])
    
  })
  
  ##############
  #Box processor
  ##############
  output$process <- renderValueBox({
    choice_processor <- c("Dual", "Quad", "Hexa", "Octa")
    input$Action_reco
    isolate(
      valueBox(
        choice_processor[as.integer(input$processor)],"Selected processor",  icon = icon("microchip"),
        color = "purple"
      ))
  })
  
  ###########
  #Box memory
  ###########
  output$memory <- renderValueBox({ 
    choice_memory <- c("1", "2", "3", "4")
    input$Action_reco
    isolate( 
      valueBox(
        paste(choice_memory[as.integer(input$memory)], ' GB'),"Selected memory",  icon = icon("database"),
        color = "green"
      )) 
  })  
  
  ############
  #Box storage
  ############
  output$storage <- renderValueBox({
    choice_internal_storage <- c("8", "16", "32", "64", "128", "256")
    input$Action_reco
    isolate(
      valueBox(
        paste(choice_internal_storage[as.integer(input$internal_storage)],' GB'),"Selected internal storage",  icon = icon("database"),
        color = "red"
      ))
  })
  
  ################
  #Box fron_camera
  ################
  output$fcamera <- renderValueBox({
    choice_front_camera <- c("5", "12", "16")
    input$Action_reco
    isolate(
      valueBox(
        paste(choice_front_camera[as.integer(input$front_camera)], ' Megapixels'),"Selected front camera",  icon = icon("camera"),
        color = "blue"
      ))
  })
  
  ################
  #Box back_camera
  ################
  output$bcamera <- renderValueBox({
    choice_back_camera <- c("2", "5", "7", "8", "16")
    input$Action_reco
    isolate(
      valueBox(
        paste(choice_back_camera[as.integer(input$back_camera)], ' Megapixels'),"Selected back camera",  icon = icon("camera"),
        color = "yellow"
      ))
  })
  
  ###########
  #Box screen
  ###########
  output$screen <- renderValueBox({
    input$Action_reco
    isolate(
      valueBox(
        paste(input$screen, ' inches'),"Selected screen",  icon = icon("mobile-alt"),
        color = "fuchsia"
      ))
  })
  
  ###########################################Troisieme onglet################################################
  
  #############################
  #Word Cloud New Avis positifs
  #############################
  output$NWCP <- renderWordcloud2({
    input$Action_new
    isolate(avis <- as.character(avis_new_pos()))
    if (avis=='No_new_url'){
      wordcloud2(data.table(word = 'No_new_url',n = 1),shuffle = FALSE)
    }
    else {
      CreateWordCloud(avis)}
    
  })
  
  #############################
  #Word Cloud New Avis négatifs
  #############################
  output$NWCN <- renderWordcloud2({
    input$Action_new
    isolate(avis <- as.character(avis_new_neg()))
    if (avis=='No_new_url'){
      wordcloud2(data.table(word = 'No_new_url',n = 1),shuffle = FALSE)
    }
    else {
      CreateWordCloud(avis)}
    
  })
  
  #######################
  #Analyse sentiments new
  #######################
  output$NAS <- renderPlot({
    input$Action_new
    isolate(CreateAnalysisSentiment(paste(avis_new_neg(),avis_new_pos())))
    
  })
  
  #################################
  #Critics rating for the new phone
  #################################
  output$NPP <- renderPrint({
    input$Action_new
    isolate(CriticsRate(paste(avis_new_neg(),avis_new_pos())))
    
  })
}

shinyApp(ui, server)
