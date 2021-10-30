library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext)
library(textdata)
library(ggplot2)
library(tibble)
library(purrr)
library(devtools)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(shiny)
library(datasets)
library(rvest)     
library(gganimate)
library(RColorBrewer)
library(kableExtra)
library(plotly)
library(data.table)
library(highcharter)
library(DT)
library(tableHTML)

init_teams <- c("Arsenal",
                "Aston Villa",
                "Burnley",
                "Chelsea",
                "Crystal Palace",
                "Everton",
                "Hull",
                "Leicester",
                "Liverpool",
                "Man City",
                "Man United",
                "Newcastle",
                "QPR",
                "Southampton",
                "Stoke",
                "Sunderland",    
                "Swansea",
                "Tottenham",
                "West Brom",
                "West Ham")

na_converter <- function(dt, value = 0) {
  
  for (j in names(dt)) set(dt, which(is.na(dt[[j]])), j, value)
  
  dt
  
}



ui <- shinyUI(fluidPage(
  titlePanel("European Football Analytics"),
  tabsetPanel(
    tabPanel('Welcome', 
             div(class = 'bg', 
                 div(class = 'first_line', strong('European Football Analytics')),
                 br(),
                 div(id = 'row_images', 
                     img(class = 'flags', src = "England.png"), 
                     img(class = 'flags', src = "Scotland.png"),
                     img(class = 'flags', src = "Germany.png"),
                     img(class = 'flags', src = "Italy.png"),
                     img(class = 'flags', src = "Spain.png"),
                     img(class = 'flags', src = "France.png"),
                     img(class = 'flags', src = "Netherlands.png"),
                     img(class = 'flags', src = "Belgium.png"),
                     img(class = 'flags', src = "Portugal.png"),
                     img(class = 'flags', src = "Turkey.png"),
                     img(class = 'flags', src = "Greece.png")
                 ),
                 br(),
                 div(class = 'third_line', 
                     'Explore the tabs at the top to begin'),
                 div(class= 'third_line',
                     'Developed by Anushree Avasthi and Aditya Agrawal')
             ),
             img(class = 'background', 
                 src = 'img2.jpg')
             
    ),
    tabPanel("Sentiment Analysis",
             sidebarPanel(
               tags$b("Here, you can compare sentiments surrounding football teams by inputting indicative hashtags. This will provide most common unique words, positive/negative texts and word clouds based on tweets using the hashtags you provide."),
               ## Getting hashtags from users as inputs
               br(),
               br(),
               textInput("hash1", "Insert First Hashtag (Ex: #manutd)", "#"),
               tags$b("This may take a few seconds, currently loading:"),
               verbatimTextOutput("value1"),
               br(),
               textInput("hash2", "Insert Second Hashtag (Ex: #liverpool)", "#"),
               tags$b("This may take a few seconds, currently loading:"),
               verbatimTextOutput("value2")
             ),
             
             mainPanel(
               tags$b("Most common unique words and their counts for Hashtag 1"),
               plotOutput("plotHashCount1"),
               br(),
               tags$b("Most common unique words and their counts for Hashtag 2"),
               plotOutput("plotHashCount2"),
               br(),
               tags$b("Positive and Negative reviews for Hashtag 1"),
               plotOutput("plotBingVisually1"),
               br(),
               tags$b("Positive and Negative reviews for Hashtag 2"),
               plotOutput("plotBingVisually2"),
               br(),
               tags$b("Word cloud for hashtag 1"),
               plotOutput("plotHash1"),
               br(),
               tags$b("Word cloud for hashtag 2"),
               plotOutput("plotHash2"),
               br(),
               #tags$b("Bing results Tibble for both the hashtags"),
               #tableOutput("BingAnalysisTibble"),
               br()
             )
             
    ),
    tabPanel( "Standings-Premier League",
              mainPanel(
                tags$b("This page may take a few seconds to load.."),
                br(),
                br(),
                br(),
                tags$b("Click on PLAY to visualize the points progression every game week of the title challengers (Premier League 2021/22)"),
                
                plotlyOutput("plotPremierLeaguePlayTop"),
                br(),
                tags$b("Find the color of your favorite team and observe their weekwise progression with respect to all premier league teams this season"),
                plotlyOutput("plotPremierLeaguePlay"),
                br(),
                tags$b("Every Premier League team and their point-growth per weeek (2021/22)"),
                plotOutput("plotPremierLeague"),
                br(),
                tags$b("Premier League 2021-2022 Standings"),
                tableOutput("PLStanding"),
                br()
              )
    ),
  
  #tab Application top of app
  tabPanel(
    'Sports Betting',
    tags$head(includeCSS('www/football.css')),
    tags$head(tags$script(src="floating_sidebar.js")),
    sidebarLayout(
      
      #sidebar starts here
      sidebarPanel(
        
        selectInput('country', 
                    'Select Country', 
                    choices = c('England', 
                                'Scotland', 
                                'Germany', 
                                'Italy', 
                                'Spain',
                                'France', 
                                'Netherlands', 
                                'Belgium', 
                                'Portugal', 
                                'Turkey',
                                'Greece'),
                    selected = 'England'),
        
        selectInput('division', 
                    'Select Division', 
                    choices = c('Premier League', 
                                'Championship', 
                                'League 1', 
                                'League 2', 
                                'Conference'),
                    selected = 'Premier League'),
        
        # selectInput('season',
        #             'Select Season',
        #             choices = rev(paste(1993:2017, shift(1993:2017, type='lead'), 
        #                                 sep = '-'))[-1], 
        #             selected = c('2016-2017')),
        
        sliderInput('date_range',
                    'Select the Date Range', 
                    value = c(as.IDate('2020-08-01'), as.IDate('2021-06-30')),
                    min = as.IDate('2020-08-01'), 
                    max = as.IDate('2022-06-30')),
        
        width = 3
      ),
      
      #main panel starts here
      mainPanel(
        
        tabsetPanel(
          tabPanel('About',
                   tags$h3("Which Sports Betting platform should you bet on today?"),
                   br(),
                   br(),
                   tags$b("Apart from helping you decide which website you should use to bet on your favorite fixture this week, on this tab, you can select from footballing leagues in 11 countries (Ex: England), and various vertical divisions (Ex: Premier league, Championship). The table will provide you with basic information about all fixtures in that season such as Goals Scored by both teams, Shots on target, Yellow/Red cards received, and who refereed the fixture. But more importantly, it will prove you with all sports betting-related data compiled from various platforms such as Bet365, BetVictor and more. Once you know what bet you want to place, this table will provide you with the information about what website provides you with the best odds, and the chance to win the most money. Gambling always has a chance of losing money and you should bet responsibly!"),
          ),
        #tab league table in Application tab
          tabPanel('Fixture wise data (20-21)',
                   br(),
                   fixedRow(
                     tableHTML_output('league_table', height = '800px')
                   )),
        tabPanel('Fixture wise data (21-22)',
                 br(),
                 tags$b("Please correct the date range on the slider to get the correct results. By default results are shown from the start of the 2021-2022 period."),
                 fixedRow(
                   tableHTML_output('league_table21', height = '800px')
                 ))
       
        ),    
        width = 9)
    )
  ),
  
  tabPanel('About',
           
           fixedRow(        
             column(
               img(src = 'stadium-pic.jpg', id = 'ribbon-about', height = 230),
               width = 12
             )
           ),
           br(),
           
           fixedRow(
             
             column(
               HTML('<ul class="nav nav-pills nav-justified">
                 <li class="active">
                 <a href="#about-tab-1" data-toggle="tab" data-value="Author">Author</a>
                 </li>
                 <li>
                 <a href="#about-tab-2" data-toggle="tab" data-value="Data/Charts">About the dashboard</a>
                 </li>
                 <li>
                 <a href="#about-tab-3" data-toggle="tab" data-value="Contact">Contact</a>
                 </li>
              </ul>'),
               width = 12
             )
           ),
           
           HTML('<div class="container-fluid">
              <div class="tab-content">
                 <div class="tab-pane active" data-value="Author" id="about-tab-1">'),
           
           br(),
           br(),
           br(),
           
           column(width = 2),
           column(h2('Author'), div(id = 'about', 
                                         'You can connect with the authors on linkedin :',
                                         a(id = 'footy', 'Aditya Agrawal', 
                                           href = 'https://www.linkedin.com/in/adityaagrawal314'),
                                         'and',
                                         a(id = 'footy', 'Anushree Avasthi', 
                                           href = 'https://in.linkedin.com/in/anushree-avasthi'),
                                         '.' ),
                  width = 8),
           column(width = 2),
           
           HTML('     </div>
                <div class="tab-pane" data-value="Data/Charts" id="about-tab-2">'),
           
           br(),
           br(),
           br(),
           
           column(width = 2),
           column(h2('Data'), 
                  div(id = 'about', 'The data in this application has been sourced from multiple applications. For sentiment analysis we have used Twitter APIs, Premier League data is sourced from', 
                      a(id = 'footy', 'weltfussball', 
                        href = 'https://www.weltfussball.de/'),
                      'and data for the sports betting tab has been collected from',
                      a(id = 'footy', 'football-data.co.uk', 
                        href = 'http://www.football-data.co.uk/data'),
                      '.'),
                  br(),
                  h2('Methodology'), 
                  div(id='about','We have used various methodologies across tabs to develop this dashboard.'),
                  h3('Sentiment Analysis'),
                  div(id = 'about', 'We used Twitter APIs to retrieve tweets by hashtags and performed sentiment analysis on them. We employed the use of bing lexicon to classify words and gauge the sentiment (by calculating the total score) of the tweet. Based on the results we created word clouds for most commonly used words and positive/negative words used with a partiular hashtag by occurence.'),
                  br(),
                  h3('Standings- Premier League'),
                  div(id = 'about', 'The English Premier League is one of the most followed football leagues. To depict how teams are performing in the current season, we used web scrapping to scrap the last modified data from this years Premier League and used plotly graphs to visualize the same. Users can come to this tab to see how their teams have performed over the season.'),
                  br(),
                  h3('Sports-betting'),
                  div(id = 'about', 'Here we allow the users to select from footballing leagues in 11 countries (Ex: England), and various vertical divisions (Ex: Premier league, Championship). The table will provide users with basic information about all fixtures in that season such as Goals Scored by both teams, Shots on target, Yellow/Red cards received, and who refereed the fixture. But more importantly, it will provide users with all sports betting-related data compiled from various platforms such as Bet365, BetVictor and more. We have rigorously worked on compiling this data for sports betting related activities.'),
                  br(),
                  width = 8),
           column(width = 2),
           
           HTML('     </div>
                <div class="tab-pane" data-value="Contacts" id="about-tab-3">'),
           
           br(),
           br(),
           br(),
           
           column(width = 2),
           column(h2('Contact'), 
                  div(id = 'about', 'If you would like to contact the authors about the application feel
                           free to drop an email at:',
                      a(id = 'email', 'aaa107@duke.edu', 
                        href = 'mailto:aaa107@duke.edu'),
                      'or you can mail',
                      a(id = 'email', 'ama131@duke.edu', 
                        href = 'mailto:ama131@duke.edu'),'.'),
                  width = 8 ),
           column(width = 2),
           
           HTML('     </div>
              </div>
           </div>')
  )
  )
)
);
server <- function(input, output, session) {
    output$value1 <- renderText({ input$hash1 })
    output$value2 <- renderText({ input$hash2 })
    
    ## Authenticating into the twitter api
    app_name= 'aaa107_ama131'
    consumer_key= 'NjSBG2CwK8n4u85U8cyZVlv8Y'
    consumer_secret= 'iQP20TbFqfcy0x6xRR8IUNkQG7oa6MZY4F74QZcVcqSXJwpQCY'
    access_token= '1044601287071809536-h5vLHK8AhJgbfsQpgwwRatMsfUtnLS'
    access_secret= '5HnjbgWInUPANATnd6YfITK4cmvpHoxk4TU69zrxUJ9xM'
    
    #Authentication
    create_token(app= app_name,
                 consumer_key=consumer_key,
                 consumer_secret= consumer_secret,
                 access_token= access_token,
                 access_secret= access_secret)
    
    ## Adding relevant hashtags based on user inputs
    ## Relevant inputs from users are input$hash1 and input$hash2
    
    #Set of common code for bing and afinn
    
    
  ## For Afinn
   
    ########## Common code ends #########
    
    #Using bing method
    output$BingAnalysis <- renderPrint({
        sentiment_bing = function(twt){
            #basic cleaning on the tweet
            twt_tbl = tibble(text=twt) %>% 
                mutate(
                    #Remove http manually
                    stripped_text = gsub("http\\S+","", text)
                ) %>%
                unnest_tokens(word, stripped_text) %>%
                anti_join(stop_words) %>% #Remove stop words
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=TRUE) %>%
                ungroup() %>%
                ## Score column 
                mutate(
                    score = case_when(
                        sentiment == 'negative'~n*(-1),
                        sentiment == 'positive'~n*1)
                )
            ## Total score
            sent.score = case_when(
                nrow(twt_tbl)==0~0, # if there are no score is 0
                nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negative
            )
            
            ## Keep track of tweets with no bing words
            zero.type = case_when(
                nrow(twt_tbl)==0~"Type 1",
                nrow(twt_tbl)>0~"Type 2",
            )
            list(score=sent.score, type= zero.type, twt_tbl= twt_tbl)
        }
        
        country1 <- search_tweets(input$hash1, n=100, include_rts= FALSE)
        
        lapply(country1$text, function(x){sentiment_bing(x)})
       
    })

    output$BingAnalysis2 <- renderPrint({
        sentiment_bing = function(twt){
            #basic cleaning on the tweet
            twt_tbl = tibble(text=twt) %>% 
                mutate(
                    #Remove http manually
                    stripped_text = gsub("http\\S+","", text)
                ) %>%
                unnest_tokens(word, stripped_text) %>%
                anti_join(stop_words) %>% #Remove stop words
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=TRUE) %>%
                ungroup() %>%
                ## Score column 
                mutate(
                    score = case_when(
                        sentiment == 'negative'~n*(-1),
                        sentiment == 'positive'~n*1)
                )
            ## Total score
            sent.score = case_when(
                nrow(twt_tbl)==0~0, # if there are no score is 0
                nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negative
            )
            
            ## Keep track of tweets with no bing words
            zero.type = case_when(
                nrow(twt_tbl)==0~"Type 1",
                nrow(twt_tbl)>0~"Type 2",
            )
            list(score=sent.score, type= zero.type, twt_tbl= twt_tbl)
        }
        country2 <- search_tweets(input$hash2, n=100, include_rts= FALSE)
        
        lapply(country2$text, function(x){sentiment_bing(x)})
        
    })
    
    output$BingAnalysisTibble <- renderTable({
        sentiment_bing = function(twt){
            #basic cleaning on the tweet
            twt_tbl = tibble(text=twt) %>% 
                mutate(
                    #Remove http manually
                    stripped_text = gsub("http\\S+","", text)
                ) %>%
                unnest_tokens(word, stripped_text) %>%
                anti_join(stop_words) %>% #Remove stop words
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=TRUE) %>%
                ungroup() %>%
                ## Score column 
                mutate(
                    score = case_when(
                        sentiment == 'negative'~n*(-1),
                        sentiment == 'positive'~n*1)
                )
            ## Total score
            sent.score = case_when(
                nrow(twt_tbl)==0~0, # if there are no score is 0
                nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negative
            )
            
            ## Keep track of tweets with no bing words
            zero.type = case_when(
                nrow(twt_tbl)==0~"Type 1",
                nrow(twt_tbl)>0~"Type 2",
            )
            list(score=sent.score, type= zero.type, twt_tbl= twt_tbl)
        }
        
        country1 <- search_tweets(input$hash1, n=100, include_rts= FALSE)
        
        country1_sent = lapply(country1$text, function(x){sentiment_bing(x)})
        
        country2 <- search_tweets(input$hash2, n=100, include_rts= FALSE)
        
        country2_sent <- lapply(country2$text, function(x){sentiment_bing(x)})
        
        country_sentiment = bind_rows(
            tibble(
                country=input$hash1,
                score=unlist({map(country1_sent,'score')}),
                type=unlist({map(country1_sent,'type')})
            ),
            tibble(
                country=input$hash2,
                score= unlist(map(country2_sent,'score')),
                type= unlist(map(country2_sent, 'type'))
            )
        )
        
        country_sentiment
        
    })
    
    output$plotHash1 <- renderPlot({
      country1 <- search_tweets(input$hash1, n=100, include_rts= FALSE)
      text <- country1$text
      docs <- Corpus(VectorSource(text))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      docs <- tm_map(docs, toSpace, "https")
      docs <- tm_map(docs, toSpace, "tco")
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your stopwords as a character vector
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      dtm <- TermDocumentMatrix(docs) 
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      gsub("https\\S*", "", docs$text) 
      gsub("@\\S*", "", docs$text) 
      gsub("amp", "", docs$text) 
      gsub("[\r\n]", "", docs$text)
      gsub("[[:punct:]]", "", docs$text)
      
      set.seed(1234)
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))})
    
    output$plotHash2 <- renderPlot({
      country1 <- search_tweets(input$hash2, n=100, include_rts= FALSE)
      text <- country1$text
      docs <- Corpus(VectorSource(text))
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      docs <- tm_map(docs, toSpace, "https")
      docs <- tm_map(docs, toSpace, "tco")
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove numbers
      docs <- tm_map(docs, removeNumbers)
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # Remove your own stop word
      # specify your stopwords as a character vector
      docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      dtm <- TermDocumentMatrix(docs) 
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      gsub("https\\S*", "", docs$text) 
      gsub("@\\S*", "", docs$text) 
      gsub("amp", "", docs$text) 
      gsub("[\r\n]", "", docs$text)
      gsub("[[:punct:]]", "", docs$text)
    
      set.seed(1234)
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2")) })
    
    output$plotHashCount1 <- renderPlot({
      country1 <- search_tweets(input$hash1, n=100, include_rts= FALSE)
      tweets.Country1 = country1 %>% select(screen_name,text)
      tweets.Country1$stripped_text1 <- gsub("http\\S+","",tweets.Country1$text)
      
      #unnest_tokens() to convert to lower case
      #remove punctuations and add id for each tweet
      tweets.Country1_stem <- tweets.Country1 %>% 
        select(stripped_text1) %>%
        unnest_tokens(word, stripped_text1)
      cleaned_tweets.Country1 <- tweets.Country1_stem %>% 
        anti_join(stop_words)
      
      cleaned_tweets.Country1 %>% 
        count(word, sort=TRUE) %>%
        top_n(10) %>%
        mutate(word= reorder(word,n)) %>%
        ggplot(aes(x=word, y=n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()+
        theme_classic()+
        labs(x="Count",
             y="Hashtag 1",
             title="")
    })
    
    output$plotHashCount2 <- renderPlot({
      country1 <- search_tweets(input$hash2, n=100, include_rts= FALSE)
      tweets.Country1 = country1 %>% select(screen_name,text)
      tweets.Country1$stripped_text1 <- gsub("http\\S+","",tweets.Country1$text)
      
      #unnest_tokens() to convert to lower case
      #remove punctuations and add id for each tweet
      tweets.Country1_stem <- tweets.Country1 %>% 
        select(stripped_text1) %>%
        unnest_tokens(word, stripped_text1)
      cleaned_tweets.Country1 <- tweets.Country1_stem %>% 
        anti_join(stop_words)
      
      cleaned_tweets.Country1 %>% 
        count(word, sort=TRUE) %>%
        top_n(10) %>%
        mutate(word= reorder(word,n)) %>%
        ggplot(aes(x=word, y=n))+
        geom_col()+
        xlab(NULL)+
        coord_flip()+
        theme_classic()+
        labs(x="Count",
             y="Hashtag 2",
             title="")
    })
    
    output$plotBingVisually1 <- renderPlot({
      country1 <- search_tweets(input$hash1, n=100, include_rts= FALSE)
      tweets.Country1 = country1 %>% select(screen_name,text)
      tweets.Country1$stripped_text1 <- gsub("http\\S+","",tweets.Country1$text)
      
      #unnest_tokens() to convert to lower case
      #remove punctuations and add id for each tweet
      tweets.Country1_stem <- tweets.Country1 %>% 
        select(stripped_text1) %>%
        unnest_tokens(word, stripped_text1)
      cleaned_tweets.Country1 <- tweets.Country1_stem %>% 
        anti_join(stop_words)
      bing_country1 = cleaned_tweets.Country1 %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort=TRUE) %>%
        ungroup()
      bing_country1 %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word= reorder(word,n)) %>%
        ggplot(aes(word,n, fill=sentiment)) +
        geom_col(show.legends= FALSE) +
        facet_wrap(~sentiment, scales="free_y")+
        labs(title= "",
             y= "Contribution to sentiment",
             x= NULL)+
        coord_flip() + theme_bw()
    })
    
    output$plotBingVisually2 <- renderPlot({
      country1 <- search_tweets(input$hash2, n=100, include_rts= FALSE)
      tweets.Country1 = country1 %>% select(screen_name,text)
      tweets.Country1$stripped_text1 <- gsub("http\\S+","",tweets.Country1$text)
      
      #unnest_tokens() to convert to lower case
      #remove punctuations and add id for each tweet
      tweets.Country1_stem <- tweets.Country1 %>% 
        select(stripped_text1) %>%
        unnest_tokens(word, stripped_text1)
      cleaned_tweets.Country1 <- tweets.Country1_stem %>% 
        anti_join(stop_words)
      bing_country1 = cleaned_tweets.Country1 %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort=TRUE) %>%
        ungroup()
      bing_country1 %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word= reorder(word,n)) %>%
        ggplot(aes(word,n, fill=sentiment)) +
        geom_col(show.legends= FALSE) +
        facet_wrap(~sentiment, scales="free_y")+
        labs(title= "",
             y= "Contribution to sentiment",
             x= NULL)+
        coord_flip() + theme_bw()
    })
    
    
  #Code for Premier League Standing web scrapping
    output$PLStanding <- renderTable({ 
      baseUrl <- "https://www.weltfussball.de/"
      path <- "spielplan/eng-premier-league-2021-2022-spieltag/"
      fileName <- 1
      url <- paste0(baseUrl, path, fileName)
      url
      tables <- read_html(url)
      xpath = "/html/body/div[3]/div[2]/div[4]/div[2]/div[1]/div/div[7]/div/table[1]"
      nodes <- html_nodes(tables, xpath = xpath)
      #html_table(nodes)
      # Create emtpy lists
      url <- list()
      pages <- list()
      nodes <- list()
      final <- list()
      start <- Sys.time()
      # For loop.
      # It will connect one by one to 38 different url links predefined 
      # by the line starting with url[[i]]
      # Collect the information with read_html(), html_nodes() and html_table()
      # Finally each table will be converted to a data frame
      for(i in 1:9){
        url[[i]] <- paste0(baseUrl, path, i)
        pages[[i]] <- read_html(url[[i]])
        nodes[[i]] <- html_nodes(pages[[i]], xpath = xpath)
        final[[i]] <- data.frame(html_table(nodes[[i]]))
      }
      # By coding start and end times of the whole process 
      # I can keep an eye on how fast my code is.
      end <- Sys.time()
      end-start
      ## Time difference of 22.62705 secs
      final[[9]]
      uk18 <-  do.call("rbind", final)
      uk18 <- uk18  %>% select(3:10)
      new_names <- c("team", "week", "won", "drawn", "lost", "goals", 
                     "difference", "points")
      colnames(uk18) <- new_names
      uk18 <- uk18 %>% separate(goals, c("scored", "against"), sep="\\:")
      head(uk18)
      
      uk18_filt <- uk18 %>% 
        filter(week == 9) %>%
        arrange(desc(points))
      knitr::kable(uk18_filt)
      
      uk18_filt
      })
    
    output$plotPremierLeague <- renderPlot({
      baseUrl <- "https://www.weltfussball.de/"
      path <- "spielplan/eng-premier-league-2021-2022-spieltag/"
      fileName <- 1
      url <- paste0(baseUrl, path, fileName)
      url
      tables <- read_html(url)
      xpath = "/html/body/div[3]/div[2]/div[4]/div[2]/div[1]/div/div[7]/div/table[1]"
      nodes <- html_nodes(tables, xpath = xpath)
      #html_table(nodes)
      # Create emtpy lists
      url <- list()
      pages <- list()
      nodes <- list()
      final <- list()
      start <- Sys.time()
      # For loop.
      # It will connect one by one to 38 different url links predefined 
      # by the line starting with url[[i]]
      # Collect the information with read_html(), html_nodes() and html_table()
      # Finally each table will be converted to a data frame
      for(i in 1:9){
        url[[i]] <- paste0(baseUrl, path, i)
        pages[[i]] <- read_html(url[[i]])
        nodes[[i]] <- html_nodes(pages[[i]], xpath = xpath)
        final[[i]] <- data.frame(html_table(nodes[[i]]))
      }
      # By coding start and end times of the whole process 
      # I can keep an eye on how fast my code is.
      end <- Sys.time()
      end-start
      ## Time difference of 22.62705 secs
      final[[9]]
      uk18 <-  do.call("rbind", final)
      uk18 <- uk18  %>% select(3:10)
      new_names <- c("team", "week", "won", "drawn", "lost", "goals", 
                     "difference", "points")
      colnames(uk18) <- new_names
      uk18 <- uk18 %>% separate(goals, c("scored", "against"), sep="\\:")
      head(uk18)
      
      uk18_filt <- uk18 %>% 
        filter(week == 9) %>%
        arrange(desc(points))
      knitr::kable(uk18_filt)
      finallevels <- as.character(uk18_filt$team)
      uk18$team <- factor(uk18$team, levels = finallevels)
      # We need a color palette with 20 colors
      colorCount <- length(unique(uk18$team))
      # colorRampPalette creatas a getPalette() function
      # This can modify an existing palette to include as many colors we want
      getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
      getPalette(colorCount)
      ##  [1] "#E41A1C" "#9B445D" "#526E9F" "#3C8A9B" "#469F6C" "#54A453" "#747B78"
      ##  [8] "#94539E" "#BD6066" "#E97422" "#FF990A" "#FFCF20" "#FAF632" "#D4AE2D"
      ## [15] "#AF6729" "#BF6357" "#E17597" "#E884B9" "#C08EA9" "#999999"
  
      # Plot season timeline
      uk <- ggplot(uk18, aes(x=week, y=points, col=team)) + 
        geom_smooth(se=TRUE) + 
        theme(text = element_text(size=15)) + 
        scale_color_manual(values = getPalette(colorCount))
      uk
 
    })
    
    output$plotPremierLeaguePlay <- renderPlotly({
      baseUrl <- "https://www.weltfussball.de/"
      path <- "spielplan/eng-premier-league-2021-2022-spieltag/"
      fileName <- 1
      url <- paste0(baseUrl, path, fileName)
      url
      tables <- read_html(url)
      xpath = "/html/body/div[3]/div[2]/div[4]/div[2]/div[1]/div/div[7]/div/table[1]"
      nodes <- html_nodes(tables, xpath = xpath)
      #html_table(nodes)
      # Create emtpy lists
      url <- list()
      pages <- list()
      nodes <- list()
      final <- list()
      start <- Sys.time()
      # For loop.
      # It will connect one by one to 38 different url links predefined 
      # by the line starting with url[[i]]
      # Collect the information with read_html(), html_nodes() and html_table()
      # Finally each table will be converted to a data frame
      for(i in 1:9){
        url[[i]] <- paste0(baseUrl, path, i)
        pages[[i]] <- read_html(url[[i]])
        nodes[[i]] <- html_nodes(pages[[i]], xpath = xpath)
        final[[i]] <- data.frame(html_table(nodes[[i]]))
      }
      # By coding start and end times of the whole process 
      # I can keep an eye on how fast my code is.
      end <- Sys.time()
      end-start
      ## Time difference of 22.62705 secs
      final[[9]]
      uk18 <-  do.call("rbind", final)
      uk18 <- uk18  %>% select(3:10)
      new_names <- c("team", "week", "won", "drawn", "lost", "goals", 
                     "difference", "points")
      colnames(uk18) <- new_names
      uk18 <- uk18 %>% separate(goals, c("scored", "against"), sep="\\:")
      head(uk18)
      
      uk18_filt <- uk18 %>% 
        filter(week == 9) %>%
        arrange(desc(points))
      knitr::kable(uk18_filt)
      finallevels <- as.character(uk18_filt$team)
      uk18$team <- factor(uk18$team, levels = finallevels)
      # We need a color palette with 20 colors
      colorCount <- length(unique(uk18$team))
      # colorRampPalette creatas a getPalette() function
      # This can modify an existing palette to include as many colors we want
      getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
      getPalette(colorCount)
      ##  [1] "#E41A1C" "#9B445D" "#526E9F" "#3C8A9B" "#469F6C" "#54A453" "#747B78"
      ##  [8] "#94539E" "#BD6066" "#E97422" "#FF990A" "#FFCF20" "#FAF632" "#D4AE2D"
      ## [15] "#AF6729" "#BF6357" "#E17597" "#E884B9" "#C08EA9" "#999999"
      
      # Plot season timeline
      
      uk18 %>%
        plot_ly(
          x = ~week, 
          y = ~points, 
          size = ~difference, 
          color = ~team, 
          frame = ~week, 
          text = ~team, 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers'
        )
    
    
      
    })
    
    output$plotPremierLeaguePlayTop <- renderPlotly({
      baseUrl <- "https://www.weltfussball.de/"
      path <- "spielplan/eng-premier-league-2021-2022-spieltag/"
      fileName <- 1
      url <- paste0(baseUrl, path, fileName)
      url
      tables <- read_html(url)
      xpath = "/html/body/div[3]/div[2]/div[4]/div[2]/div[1]/div/div[7]/div/table[1]"
      nodes <- html_nodes(tables, xpath = xpath)
      #html_table(nodes)
      # Create emtpy lists
      url <- list()
      pages <- list()
      nodes <- list()
      final <- list()
      start <- Sys.time()
      # For loop.
      # It will connect one by one to 38 different url links predefined 
      # by the line starting with url[[i]]
      # Collect the information with read_html(), html_nodes() and html_table()
      # Finally each table will be converted to a data frame
      for(i in 1:9){
        url[[i]] <- paste0(baseUrl, path, i)
        pages[[i]] <- read_html(url[[i]])
        nodes[[i]] <- html_nodes(pages[[i]], xpath = xpath)
        final[[i]] <- data.frame(html_table(nodes[[i]]))
      }
      # By coding start and end times of the whole process 
      # I can keep an eye on how fast my code is.
      end <- Sys.time()
      end-start
      ## Time difference of 22.62705 secs
      final[[9]]
      uk18 <-  do.call("rbind", final)
      uk18 <- uk18  %>% select(3:10)
      new_names <- c("team", "week", "won", "drawn", "lost", "goals", 
                     "difference", "points")
      colnames(uk18) <- new_names
      uk18 <- uk18 %>% separate(goals, c("scored", "against"), sep="\\:")
      head(uk18)
      
      uk18_filt <- uk18 %>% 
        filter(week == 9) %>%
        arrange(desc(points))
      knitr::kable(uk18_filt)
      finallevels <- as.character(uk18_filt$team)
      uk18$team <- factor(uk18$team, levels = finallevels)
      # We need a color palette with 20 colors
      colorCount <- length(unique(uk18$team))
      # colorRampPalette creatas a getPalette() function
      # This can modify an existing palette to include as many colors we want
      getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
      getPalette(colorCount)
      ##  [1] "#E41A1C" "#9B445D" "#526E9F" "#3C8A9B" "#469F6C" "#54A453" "#747B78"
      ##  [8] "#94539E" "#BD6066" "#E97422" "#FF990A" "#FFCF20" "#FAF632" "#D4AE2D"
      ## [15] "#AF6729" "#BF6357" "#E17597" "#E884B9" "#C08EA9" "#999999"
      
      uk18 %>% filter(team =="Manchester United" | team=="Chelsea FC" | team=="Liverpool FC" | team=="Manchester City") %>%
        plot_ly(
          x = ~week, 
          y = ~points, 
          size = ~difference, 
          color = ~team, 
          frame = ~week, 
          text = ~team, 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers'
        )
      
      })
    
    
    #Cumulative football statistics
    #declare reactive values------------------------------------------------------
    #declare reactive values------------------------------------------------------
    values <- reactiveValues()
    
    #an observe function that updates sidebar selectInputs------------------------
    observe({
      
      #update division when selecting country
      updateSelectInput(
        session, 
        'division', 
        choices = switch(input$country,
                         Greece      = 'Superleague',
                         England     = c('Premier League',
                                         'Championship', 
                                         'League 1', 
                                         'League 2', 
                                         'Conference'),
                         Scotland    = c('Premier  League', 
                                         'Division 1', 
                                         'Division 2', 
                                         'Division 3'),
                         Germany     = c('Bundesliga 1', 
                                         'Bundesliga 2'),
                         Italy       = c('Serie A', 
                                         'Serie B'),
                         Spain       = c('Primera Division', 
                                         'Segunda Division'),
                         France      = c('Ligue 1', 
                                         'Ligue 2'),
                         Netherlands = 'Eredivisie',
                         Belgium     = 'Jupiler League',
                         Portugal    = 'Liga 1',
                         Turkey      = 'Ligi 1')
      )
      
      #update years when selecting country
      updateSelectInput(
        session, 
        'season', 
        choices = switch(input$country,
                         Greece      = rev(paste(1994:2017, 
                                                 shift(1994:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         England     = rev(paste(1993:2017,
                                                 shift(1993:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         Scotland    = rev(paste(1994:2017,
                                                 shift(1994:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         Germany     = rev(paste(1993:2017,
                                                 shift(1993:2017, type = 'lead'),
                                                 sep='-'))[-1],
                         Italy       = rev(paste(1993:2017,
                                                 shift(1993:2017, type = 'lead'),
                                                 sep='-'))[-1],
                         Spain       = rev(paste(1993:2017,
                                                 shift(1993:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         France      = rev(paste(1993:2017,
                                                 shift(1993:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         Netherlands = rev(paste(1993:2017,
                                                 shift(1993:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         Belgium     = rev(paste(1995:2017,
                                                 shift(1995:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         Portugal    = rev(paste(1994:2017,
                                                 shift(1994:2017, type = 'lead'),
                                                 sep = '-'))[-1],
                         Turkey      = rev(paste(1994:2017,
                                                 shift(1994:2017, type = 'lead'),
                                                 sep = '-'))[-1]
        )
      )
      
      #pass country to global reactive values
      values$country <- input$country
      
    })
    
    #reactive function that downloads the dataset from---------------------------- 
    #football-data.co.uk----------------------------------------------------------
    dataInput <- reactive({
      
      #traslate country to code
      country_code <- switch(isolate(values$country),
                             Greece      = 'G',
                             England     = 'E',
                             Scotland    = 'SC',
                             Germany     = 'D',
                             Italy       = 'I',
                             Spain       = 'SP',
                             France      = 'F',
                             Netherlands = 'N',
                             Belgium     = 'B',
                             Portugal    = 'P',
                             Turkey      = 'T'
      )
      
      #translate division to code
      division_code <- switch(input$division,
                              `Premier League`    = '0',
                              Championship        = '1',
                              `League 1`          = '2',
                              `League 2`          = '3',
                              Conference          = 'C',
                              `Premier  League`   = '0',
                              `Division 1`        = '1',
                              `Division 2`        = '2',
                              `Division 3`        = '3',
                              `Bundesliga 1`      = '1',
                              `Bundesliga 2`      = '2',
                              `Serie A`           = '1',
                              `Serie B`           = '2',
                              `Primera Division`  = '1',
                              `Segunda Division`  = '2',
                              `Ligue 1`           = '1',
                              `Ligue 2`           = '2',
                              Eredivisie          = '1',
                              `Jupiler League`    = '1',
                              `Liga 1`            = '1',
                              `Ligi 1`            = '1',
                              Superleague         = '1'
      )
      
      #create season code
      # season_code <- paste0(substr(input$season, 3, 4), 
      #                       substr(input$season, 8, 9))
      
      #create url and read csv
      footy_set <- read.csv(paste0('http://www.football-data.co.uk/mmz4281/', 
                                   '20-21', 
                                   '/', 
                                   country_code, 
                                   division_code, 
                                   '.csv'))
      
      #some files contain empty lines which cause problems with date conversions
      #remove them
      footy_set <- footy_set[!footy_set$Date == '', ]
      
      #remove empty columns 
      footy_set <- footy_set[, grep('^X', 
                                    names(footy_set), 
                                    invert = TRUE, 
                                    value = TRUE)]
      
      #convert to data.table for super fast manipulations
      setDT(footy_set)
      
      footy_set[, Date := as.IDate(Date, format='%d/%m/%y')]
      
      #update all in-tab teams (second graphs in each tab)
      updateSelectInput(session, 
                        'team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      
      updateSelectInput(session,
                        'result_team', 
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      
      updateSelectInput(session,
                        'OU_team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      updateSelectInput(session, 
                        'cards_team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      updateSelectInput(session,
                        'corner_team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      
      #save min and max Date for on-going seasons. In August min and max year are the same
      # min_y <- footy_set[, year(min(Date))]
      # max_y <- min_y + 1
      # updateSliderInput(session, 
      #                   'date_range',
      #                   value=c(as.IDate(paste0(footy_set[, min_y],
      #                                           '-08-01')),
      #                           as.IDate(paste0(footy_set[, max_y],
      #                                           '-06-30'))),
      #                   min=as.IDate(paste0(footy_set[, min_y],
      #                                       '-08-01')),
      #                   max=as.IDate(paste0(footy_set[, max_y],
      #                                       '-06-30')))
      
      footy_set
      
    })
   
    output$league_table <- renderTable({
      footy_table <- dataInput()
      
      footy_table <- footy_table[between(Date, 
                                         input$date_range[1],
                                         input$date_range[2]), ]
      
      values$data <- footy_table
      
      footy_table
    }) 
    
    #Input for the present season
    dataInput1 <- reactive({
      
      #traslate country to code
      country_code <- switch(isolate(values$country),
                             Greece      = 'G',
                             England     = 'E',
                             Scotland    = 'SC',
                             Germany     = 'D',
                             Italy       = 'I',
                             Spain       = 'SP',
                             France      = 'F',
                             Netherlands = 'N',
                             Belgium     = 'B',
                             Portugal    = 'P',
                             Turkey      = 'T'
      )
      
      #translate division to code
      division_code <- switch(input$division,
                              `Premier League`    = '0',
                              Championship        = '1',
                              `League 1`          = '2',
                              `League 2`          = '3',
                              Conference          = 'C',
                              `Premier  League`   = '0',
                              `Division 1`        = '1',
                              `Division 2`        = '2',
                              `Division 3`        = '3',
                              `Bundesliga 1`      = '1',
                              `Bundesliga 2`      = '2',
                              `Serie A`           = '1',
                              `Serie B`           = '2',
                              `Primera Division`  = '1',
                              `Segunda Division`  = '2',
                              `Ligue 1`           = '1',
                              `Ligue 2`           = '2',
                              Eredivisie          = '1',
                              `Jupiler League`    = '1',
                              `Liga 1`            = '1',
                              `Ligi 1`            = '1',
                              Superleague         = '1'
      )
      
      #create season code
      # season_code <- paste0(substr(input$season, 3, 4), 
      #                       substr(input$season, 8, 9))
      
      #create url and read csv
      footy_set <- read.csv(paste0('http://www.football-data.co.uk/mmz4281/', 
                                   '21-22', 
                                   '/', 
                                   country_code, 
                                   division_code, 
                                   '.csv'))
      
      #some files contain empty lines which cause problems with date conversions
      #remove them
      footy_set <- footy_set[!footy_set$Date == '', ]
      
      #remove empty columns 
      footy_set <- footy_set[, grep('^X', 
                                    names(footy_set), 
                                    invert = TRUE, 
                                    value = TRUE)]
      
      #convert to data.table for super fast manipulations
      setDT(footy_set)
      
      footy_set[, Date := as.IDate(Date, format='%d/%m/%y')]
      
      #update all in-tab teams (second graphs in each tab)
      updateSelectInput(session, 
                        'team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      
      updateSelectInput(session,
                        'result_team', 
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      
      updateSelectInput(session,
                        'OU_team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      updateSelectInput(session, 
                        'cards_team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      updateSelectInput(session,
                        'corner_team',
                        choices = sort(unique(c(unique(footy_set$HomeTeam),
                                                unique(footy_set$AwayTeam)))))
      
      #save min and max Date for on-going seasons. In August min and max year are the same
      # min_y <- footy_set[, year(min(Date))]
      # max_y <- min_y + 1
      # updateSliderInput(session, 
      #                   'date_range',
      #                   value=c(as.IDate(paste0(footy_set[, min_y],
      #                                           '-08-01')),
      #                           as.IDate(paste0(footy_set[, max_y],
      #                                           '-06-30'))),
      #                   min=as.IDate(paste0(footy_set[, min_y],
      #                                       '-08-01')),
      #                   max=as.IDate(paste0(footy_set[, max_y],
      #                                       '-06-30')))
      
      footy_set
      
    })
    
    output$league_table21 <- renderTable({
      footy_table <- dataInput1()
      
      footy_table <- footy_table[between(Date, 
                                         input$date_range[1],
                                         input$date_range[2]), ]
      
      values$data <- footy_table
      
      footy_table
    }) 
  
}
shinyApp(ui, server)
