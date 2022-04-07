
#Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(tidyverse)

# Loading Dataset
setwd(getwd())
matches <- as.data.frame(read.csv('matches.csv'))
deliveries <- as.data.frame(read.csv('deliveries.csv'))
df <- deliveries %>% inner_join(matches, by = c('match_id' = 'id'))
season_filter <- sort(as.character(unique(df$season)), decreasing = T)

# Cleaning Dataset
names(matches)[1] = "match_id"
IPL = dplyr::inner_join(matches,deliveries)

# Creating layout of Shiny App

# UI function - app's appearance
ui <- dashboardPage(
    dashboardHeader(title = 'IPL Analysis'  # creates a header for dashboard page
    ),
    dashboardSidebar( tags$style(HTML('.main-sidebar{width: 220px;}')),  # contains menu items for quick navigation
                      sidebarMenu( # to define list of menu items
                          menuItem('About', tabName = 'about', icon = icon('pen')),
                          menuItem('IPL - Overview', tabName = 'summary', icon = icon('chart-bar')),
                          menuItem('Season Statistics', tabName = 'Season', icon = icon('chart-bar')),
                          menuItem('Team Wins & Points - Histograms ', tabName = 'Teamwins', icon = icon('chart-bar')),
                          menuItem("Source code", icon = icon("code"), href = "https://github.com/pavanalakshmi/AMOD_5250/blob/main/app.R")
                      )
    ),
    dashboardBody(  # main body of dashboard page
        tabItems( # defines each tab objects
            # About -------------------------------------------------------------------
            tabItem(tabName = 'about',
                    h2('Indian Premier League', align = 'center'),
                    tags$p('The Indian Premier League (IPL) is a professional Twenty20 cricket league in India contested during
              March or April and May of every year by eight teams representing eight different cities in India.
              The league was founded by the Board of Control for Cricket in India (BCCI) in 2008. 
              The IPL has an exclusive window in ICC Future Tours Programme. The IPL is the most-attended cricket 
              league in the world and in 2014 ranked sixth by average attendance among all sports leagues.', 
                           style = 'font-size: 120%;margin-left:2.5em;'),
                    h3('Teams of the League', align = 'center'),
                    
                    fluidRow(
                        shinydashboard::box(width = 12, background = 'black',
                                            valueBox(tags$p('Chennai Super Kings (CSK)', style = 'font-size: 20%;text-align: center;'), 
                                                     div(img(src ="csk.jpg", height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Mumbai Indians (MI)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'mi.jpg', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Sunrises Hyderabad (SRH)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'srh.png', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Delhi Capitals (DC)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'DCA.png', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Kolkata Knight Riders (KKR)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'KKR.png', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Kings XI  Punjab (KXIP)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'KXLP.png', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Royal Challengers Bangalore (RCB)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'RCB.jpg', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Rajasthan Royals (RR)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'RR.jpg', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Rising Pune Supergiant (RPS)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'RPS.png', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Gujarat Lions (GL)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'GL.jpg', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Deccan Chargers (DC)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'DC.png', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('Delhi Daredevils (DD)', style = 'font-size: 30%;text-align: center;'), 
                                                     div(img(src = 'DD.jpg', height = '120', width = '160'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black')
                                            
                        )
                    )
            ),
            
           
            # Overview -----------------------------------------------------------------
            
            tabItem(tabName = 'summary',
                    
                    shinydashboard::box(title = 'Overview',
                                        width = 12,
                                        solidHeader = T, 
                                        collapsible = F,
                                        background  = 'black',
                                        fluidRow(
                                            infoBoxOutput('matches'),tags$style('#matches {width:250px;}'),
                                            infoBoxOutput('teams'),tags$style('#teams {width:250px;}'),
                                            infoBoxOutput('runs'),tags$style('#runs {width:250px;}'),
                                            infoBoxOutput('wickets'),tags$style('#wickets {width:250px;}'),
                                            infoBoxOutput('seasons'),tags$style('#seasons {width:250px;}'),
                                            infoBoxOutput('mom'),tags$style('#mom {width:250px;}'),
                                            infoBoxOutput('sixes'),tags$style('#sixes {width:250px;}'),
                                            infoBoxOutput('fours'),tags$style('#fours {width:250px;}')
                                        )),
                    tags$p('Expand to see details', style = 'font-size: 120%;margin-left:2.5em;'),
                    
                    shinydashboard::box(title = 'Season Winners',
                                        width = 12,
                                        background  = 'black',
                                        solidHeader = T, 
                                        collapsible = T,
                                        collapsed = T,
                                        fluidRow(
                                            valueBox(tags$p('2008', style = 'font-size: 40%;text-align: center;'), div(img(src = 'RR.jpg', height = '80', width = '100'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2009', style = 'font-size: 40%;text-align: center;'), div(img(src = 'DC.png', height = '80', width = '100'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2010', style = 'font-size: 40%;text-align: center;'), div(img(src = 'csk.jpg', height = '80', width = '100'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2011', style = 'font-size: 40%;text-align: center;'), div(img(src = 'csk.jpg', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2012', style = 'font-size: 40%;text-align: center;'), div(img(src = 'KKR.png', height = '80', width = '100'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2013', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.jpg', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2014', style = 'font-size: 40%;text-align: center;'), div(img(src = 'KKR.png', height = '80', width = '100'),style='text-align: center;'), 
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2015', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.jpg', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2016', style = 'font-size: 40%;text-align: center;'), div(img(src = 'srh.png', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2017', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.jpg', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2018', style = 'font-size: 40%;text-align: center;'), div(img(src = 'csk.jpg', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black'),
                                            valueBox(tags$p('2019', style = 'font-size: 40%;text-align: center;'), div(img(src = 'mi.jpg', height = '80', width = '100'),style='text-align: center;'),  
                                                     icon = NULL, width = 2, color = 'black')
                                        )
                    )
            ),
            
             # Season Statistics -----------------------------------------------------------------
            
            tabItem(tabName = 'Season',
                    
                    selectInput("season_year","Select Season",choices=unique(sort(matches$season,
                                                                                  decreasing=TRUE)), selected = 2019),
                    submitButton("Go"),
                    tags$h3("Players table"),
                    div(style = "border:1px black solid;width:50%",tableOutput("player_table"))
                    
                    
            ),
            
            
            # Player performance ------------------------------------------------------
            
            tabItem(tabName = 'Teamwins',           
                    tags$h3("Team Wins & Points"),
                    div(style = "float:left;width:36%;",plotOutput("wins_bar_plot")),
                    div(style = "float:right;width:64%;",plotOutput("points_bar_plot"))
                    
            )
        )
    )
)

# server ------------------------------------------------------------------

# Defines server function

server <- function(input, output) { 
    
    matches_year = reactive({ matches %>% filter(season == input$season_year) })
    playoff = reactive({ nth(sort(matches_year()$match_id,decreasing = TRUE),4) })
    matches_played = reactive({ matches_year() %>% filter(match_id < playoff()) }) 
    t1 = reactive({ matches_played() %>% group_by(team1) %>% summarise(count = n()) })
    t2 = reactive({ matches_played() %>% group_by(team2) %>% summarise(count = n()) })
    wl = reactive({ matches_played() %>% filter(winner != "") %>% group_by(winner) %>% 
            summarise(no_of_wins = n()) })
    
    wl1=reactive({ matches_played() %>% group_by(winner) %>% summarise(no_of_wins=n()) })
    tied = reactive({ matches_played() %>% filter(winner == "") %>% select(team1,team2) })
    playertable = reactive({data.frame(Teams = t1()$team1,Played=t1()$count+t2()$count,
                                       Wins = wl()$no_of_wins,Points = wl()$no_of_wins*2)})
    
    # creating the output variable to store playertable
    output$player_table = renderTable({ playertable() })
    
    # creating barcharts for Team Wins & Points tab
    output$wins_bar_plot = renderPlot({ ggplot(wl1()[2:9,],aes(winner,no_of_wins,fill=winner))+geom_bar(stat = "identity")+ theme_classic()+xlab("Teams")+ ylab("Number Of Wins")+theme(axis.text.x=element_text(color="white"),legend.position = "none",axis.title=element_text(size=14),plot.background=element_rect(colour="white"))+geom_text(aes(x=winner,(no_of_wins+0.6),label = no_of_wins,size=7)) })
    
    output$points_bar_plot = renderPlot({ ggplot(playertable(),aes(Teams,Points,fill=Teams))+
            geom_bar(stat = "identity",size=3)+theme_classic()+theme(axis.text.x=element_text(
                color = "white"),legend.text = element_text(size = 14),axis.title = element_text(size=14))+
            geom_text(aes(Teams,(Points+1),label=Points,size=7)) })
    
    
    # Overview Outputs --------------------------------------------------------
    
    output$matches <- renderInfoBox({
        infoBox('# Matches', df %>% summarise(matches = n_distinct(match_id)),
                icon = icon('handshake'), color = 'olive', fill = T, width = 1)
    })
    output$seasons <- renderInfoBox({
        infoBox('# Season', df %>% summarise(matches = n_distinct(season)),
                icon = icon('trophy'), color = 'olive', fill = T, width = 1)
    })
    output$runs <- renderInfoBox({
        infoBox('# Runs', df %>% summarise(runs = sum(total_runs)), 
                icon = icon('walking'), color = 'olive', fill = T, width = 1)
    })
    output$teams <- renderInfoBox({
        infoBox('# Teams', df %>% summarise(teams = n_distinct(batting_team)), 
                icon = icon('users'), color = 'olive', fill = T, width = 1)
    })
    output$wickets <- renderInfoBox({
        infoBox('# Wickets', df %>% filter(dismissal_kind %in% 
                                               c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped'))%>%  summarise(wickets = n()), 
                icon = icon('hand-pointer'), color = 'olive', fill = T, width = 1)
    })
    output$fours <- renderInfoBox({
        infoBox('# Fours', filter(df, batsman_runs == 4) %>% summarise(fours = n()), 
                icon = icon('dice-four'), color = 'olive', fill = T, width = 1)
    })
    output$sixes <- renderInfoBox({
        infoBox('# Sixes', filter(df, batsman_runs == 6) %>% summarise(fours = n()), 
                icon = icon('dice-six'), color = 'olive', fill = T, width = 1)
    })
    output$mom <- renderInfoBox({
        infoBox('Most MOM', tags$p((df %>% group_by(player_of_match) %>% summarise(num = n_distinct(match_id)) %>% 
                                        arrange(desc(num)) %>% head(1)), style = 'font-size: 90%;'),
                icon = icon('user-plus'), color = 'olive', fill = T, width = 1)
    })
    
    
    # Season analysis --------------------------------------------------------
    
    output$top_10_batsman <- renderPlot({
        filter(df %>% group_by(batsman, season) %>% summarise(runs = sum(batsman_runs)), 
               season == input$season_filter) %>% arrange(desc(runs)) %>% head(10) %>%
            ggplot(aes(x = reorder(batsman,-runs), y = runs)) + geom_bar(stat="identity", width=0.6, fill="orange1") +
            geom_text(aes(label = as.numeric(runs), vjust = 2)) +
            labs(x = 'Batsman', y = 'Total Runs Scored') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$most_100s <- renderPlot({
        filter(df %>% group_by(batsman, season, match_id) %>% summarise(runs = sum(batsman_runs)),
               runs >= 100 & season == input$season_filter) %>% group_by(batsman) %>% summarise(hundreds = n()) %>% 
            arrange(desc(hundreds)) %>% 
            ggplot(aes(x = reorder(batsman,-hundreds), y = hundreds)) + geom_bar(stat="identity", width=0.6, fill="orange1") +
            geom_text(aes(label = as.numeric(hundreds), vjust = 2)) +
            labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$most_50s <- renderPlot({
        filter(df %>% group_by(batsman, season, match_id) %>% summarise(runs = sum(batsman_runs)),
               runs >= 50 & season == input$season_filter) %>% group_by(batsman) %>% summarise(fifties = n()) %>% 
            arrange(desc(fifties)) %>% head(10) %>%
            ggplot(aes(x = reorder(batsman,-fifties), y = fifties)) + geom_bar(stat="identity", width=0.6, fill="orange1") +
            geom_text(aes(label = as.numeric(fifties), vjust = 2)) +
            labs(x = 'Batsman', y = 'No. of 50s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$top_10_6s <- renderPlot({
        filter(as.data.frame(filter(df, batsman_runs == 6) %>% group_by(batsman, season) %>% summarise(sixes = n())), 
               season == input$season_filter) %>% arrange(desc(sixes)) %>% head(10) %>%
            ggplot(aes(x = reorder(batsman,-sixes), y = sixes)) + geom_bar(stat="identity", width=0.6, fill="orange1") +
            geom_text(aes(label = as.numeric(sixes), vjust = 2)) +
            labs(x = 'Batsman', y = 'No. of 6s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$top_10_4s <- renderPlot({
        filter(as.data.frame(filter(df, batsman_runs == 4) %>% group_by(batsman, season) %>% summarise(fours = n())), 
               season == input$season_filter) %>% arrange(desc(fours)) %>% head(10) %>%
            ggplot(aes(x = reorder(batsman,-fours), y = fours)) + geom_bar(stat="identity", width=0.6, fill="orange1") +
            geom_text(aes(label = as.numeric(fours), vjust = 2)) +
            labs(x = 'Batsman', y = 'No. of 4s') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$top_10_bowlers <- renderPlot({
        filter(df %>% filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 'lbw', 'hit wicket', 'stumped')) %>%
                   group_by(bowler, season) %>% summarise(wickets = n()), 
               season == input$season_filter) %>% arrange(desc(wickets)) %>% head(10) %>% 
            ggplot(aes(x = reorder(bowler, -wickets), y = wickets)) + geom_bar(stat = 'identity', width = 0.6 , fill = 'orange1') +
            geom_text(aes(label = as.numeric(wickets), vjust = 2)) +
            labs(x = 'Bowler', y = 'Total Wickets Taken') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$maiden <- renderPlot({
        data.frame(filter(filter(df, season== input$season_filter)%>% group_by(match_id,inning, over,bowler) %>% 
                              summarise(runs = sum(batsman_runs)-sum(bye_runs)), runs ==0) %>% group_by(bowler) %>% summarise(maiden =n())) %>%
            inner_join( data.frame(filter(df, season== input$season_filter)%>% group_by(match_id,inning, over,ball,bowler) %>% 
                                       summarise(runs_given = sum(batsman_runs)-sum(bye_runs)-sum(legbye_runs)) %>% group_by(bowler) %>% 
                                       summarise(runs_given= sum(runs_given), overs = n_distinct(match_id,over))) %>% mutate(econ = round(runs_given/overs,2)),
                        by = "bowler") %>% arrange(desc(maiden), econ) %>% head(10) %>% 
            ggplot(aes(x = reorder(bowler, -maiden), y = maiden)) + geom_bar(stat = 'identity', width = 0.6 , fill = 'orange1') +
            geom_text(aes(label = as.numeric(maiden), vjust = 2)) +
            labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$dot_balls <- renderPlot({
        filter(df, season== input$season_filter & total_runs == 0)%>% group_by(bowler) %>% summarise(dot = n())  %>% 
            arrange(desc(dot)) %>% head(10) %>% 
            ggplot(aes(x = reorder(bowler, -dot), y = dot)) + 
            geom_bar(stat = 'identity', width = 0.6 , fill = 'orange1') +
            geom_text(aes(label = as.numeric(dot), vjust = 2)) +
            labs(x = 'Bowler', y = '# of dot balls') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
    output$wickets_4 <- renderPlot({
        filter(filter(df, season== input$season_filter) %>% 
                   filter(dismissal_kind %in% c('bowled', 'caught', 'caught and bowled', 
                                                'lbw','hit wicket', 'stumped')) %>% group_by(bowler, match_id) %>% summarise(wickets = n()), 
               wickets == 4) %>% group_by(bowler) %>% summarise(Four_wickets = n()) %>% 
            ggplot(aes(x = reorder(bowler, -Four_wickets), y = Four_wickets)) + 
            geom_bar(stat = 'identity', width = 0.6 , fill = 'orange1') +
            geom_text(aes(label = as.numeric(Four_wickets), vjust = 2)) +
            labs(x = 'Bowler', y = '# of times 4 Wickets') + theme(axis.text.x = element_text(angle = 25, hjust = 1))
    })
    
}

# shinyApp calls UI and server functions

shinyApp(ui = ui, server = server)
