# devtools::install_github("abresler/nbastatR")
library("nbastatR")
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

assign_nba_players()
library(shiny)
library(dplyr)
library(plotly)
library(fpp3)
library(shinydashboard)
library(DT)
library(tsbox)
library(fpp2)






# UI
ui <- fluidPage(
    dashboardPage(skin = "red",
                  dashboardHeader(title = "NBA Player Stats"),
                  dashboardSidebar(
                      sidebarMenu(
                          menuItem("Get Started", tabName = "start", icon = icon("basketball-ball")),
                          menuItem("Important Notes", tabName = "notes", icon = icon("clipboard")),
                          menuItem("Choose your Player", tabName = "player", icon = icon("walking")),
                          menuItem("Table", tabName = "table", icon = icon("table")),
                          menuItem("Plot", tabName = "plot", icon = icon("chart-line")),
                          menuItem("Let's Make Predictions!", tabName = "prediction_intro", icon = icon("calculator")),
                          menuItem("Player Predictions", tabName = "predictions", icon = icon("running")),
                          menuItem("Simple Models", tabName = "simple", icon = icon("chart-area")),
                          menuItem("Exponential Smoothing Models", tabName = "exp",icon = icon("chart-area")),
                          menuItem("ARIMA Models", tabName = "arima", icon = icon("chart-area")),
                          menuItem("Differencing", tabName = "diff", icon = icon("chart-area")),
                          menuItem("STL Decomp", tabName = "STL", icon = icon("chart-area")),
                          menuItem("Performance Metrics", tabName = "perf", icon = icon("poll"))
                      )
                  ),
                  dashboardBody(
                      tabItems(
                          tabItem(tabName = "start",
                                  h1("Welcome to the NBA Player Stats App!"),
                                  h2(),
                                  h2("Instructions"),
                                  h4("1. Go to the first tab labeled 'Choose your Player'
                           to specify the player's stats you'd like to see."),
                                  h4("2. On this tab, select your player by typing your player's name."),
                                  h4("3. Next, go to the 'Table' tab to view your player's career stats
                           by year in a table and a summary of those stats."),
                                  h4("4. Wait for about 5 seconds for your table to load. When it loads,
                           you can scroll through his stats, see a summary of his stats, and explore his career."),
                                  h4("5. Then, go the 'Plot' tab to see a stat of your choosing plotted over
                           your player's career."),
                                  h4("6. Choose the stat you wish to plot and click the button to load
                           the plot."),
                                  h4("7. If you decide to change the player you selected, be sure to, go back
                        to the 'Choose your Player' tab and hit the
                           button again to load the table for your new player's stats."),
                                  h4("8. Once you are done looking at your player's career,
                                     then we can make some predictions for next year! Go to the next tab
                                     and load the active player list. Find an active player you'd
                                     like to make predictions for and go to the next tab."),
                                  h4("9. Type in your active player to predict his points per game for the next year."),
                                  h4("10. Go to the next tab to see your predictions using the simple models first.
                                     Be sure to give it around 5 seconds for the forecast to load."),
                                  h4("11. Next check out the Exponetial Smoothing tab to see your predictions
                                     using exponential smoothing models."),
                                  h4("12. Then, go to the ARIMA tab to see your predictions using different
                                     ARIMA models."),
                                  h4("12. Finally, go to the Performance Metrics tab to see which model predicted
                                     your stat for your player the best."),
                                  h1(),
                                  h5("You can hop around the app as you please. If you want to change anything,
                                     remember to hit the buttons again to load your new player or stat."),
                                  h1(),
                                  h1("HAVE FUN EXPLORING!")
                          ),
                          tabItem(tabName = "notes",
                                  h2("Important Notes"),
                                  h3("1."),
                                  h4("Some stats were not recorded until a certain time in the NBA, so if
                        you choose a player that played before a stat was being recorded, he
                        will not have a record of that stat."),
                                  h4("For example, Bill Russell played from 1956-1969. Blocks were not
                           recorded until 1973, so you will not be able to plot blocks or see
                           his number of blocks in his career."),
                                  h4("Here is a list of stats and the season when they were first recorded"),
                                  h5("Rebounds: 1950-1951"),
                                  h5("Minutes: 1951-1952"),
                                  h5("Games Started: 1970-1971"),
                                  h5("Steals: 1973-1974"),
                                  h5("Blocks: 1973-1974"),
                                  h5("Offensive Rebounds: 1973-1974"),
                                  h5("Defensive Rebounds: 1973-1974"),
                                  h5("Turnovers: 1977-1978"),
                                  h5("3 Point Field Goals: 1979-1980"),
                                  h1(),
                                  h4("This dataset tracks per game statistics. However, not every stat
                                     can be recorded on a per game level."),
                                  h4("Here are the list of stats that are player totals, not per game"),
                                  h5("Games Played"),
                                  h5("Games Started"),
                                  h5("Field Goal Percentage"),
                                  h5("3 Point Field Goal Percentage"),
                                  h5("Free Throw Percentage"),
                                  h5("2 Point Field Goal Percentage"),
                                  h1(),
                                  h3("2."),
                                  h4("One final note, the 2021-2022 season is currently being played.
                           This app plots a player's stat per game. This app does update throughout the year,
                           so you can check to see how your player's stats have changed after
                           they play a game!")),
                          tabItem(tabName = "player",
                                  h1("Choose your Player"),
                                  h4("Choose an NBA Player to see his career stats by typing his name."),
                                  h4("Be sure to have correct spelling and capitalization.
                        Ex: LeBron James or D.J. Augustin"),
                                  h1(),
                                  textInput("player", "Type an NBA player"),
                                  actionButton("download", "Click to get your player's stats"),
                                  h4("Once you have clicked the button to load your player's stats,
                           head over to the 'Table' tab to see your player's stats in a table
                           (Give it about 5 seconds to load)."),
                                  h4("If you get an error on the next page, you most likely
                                     spelled your player's name wrong. Be sure to have correct spelling
                                     and to add any periods if necessary.")
                          ),
                          tabItem(tabName = "table",
                                  h2("Year by Year Stats Per Game for Your Player"),
                                  dataTableOutput("table"),
                                  h1(),
                                  verbatimTextOutput("summary"),
                                  helpText("slugSeason = Season Year"),
                                  helpText("numberPlayerSeason = Years of Experience in the NBA, 0 = Rookie"),
                                  helpText("isRookie -> Rookie = TRUE, Not Rookie = FALSE"),
                                  helpText("slugTeam = NBA Team the player played for that season"),
                                  helpText("idTeam = for coding purposes"),
                                  helpText("gp = Games Played"),
                                  helpText("gs = Games Started"),
                                  helpText("fgm = Field Goals Made Per Game"),
                                  helpText("fga = Field Goal Attempts Per Game"),
                                  helpText("pctFG = Field Goal Percentage"),
                                  helpText("fg3m = 3 Point Field Goals Made Per Game"),
                                  helpText("fg3a = 3 Point Field Goals Attempted Per Game"),
                                  helpText("pctFG3 = 3 Point Field Goal Percentage"),
                                  helpText("pctFT = Free Throw Percentage"),
                                  helpText("fg2m = 2 Point Field Goals Made Per Game"),
                                  helpText("fg2a = 2 point Field Goals Attempted Per Game"),
                                  helpText("pctFG2 = 2 Point Field Goal Percentage"),
                                  helpText("minutes = Minutes Per Game"),
                                  helpText("ftm = Free Throws Made Per Game"),
                                  helpText("fta = Free Throws Attempted Per Game"),
                                  helpText("oreb = Offensive Rebounds Per Game"),
                                  helpText("dreb = Defensive Rebounds Per Game"),
                                  helpText("treb = Total Rebounds Per Game"),
                                  helpText("ast = Assists Per Game"),
                                  helpText("stl = Steals Per Game"),
                                  helpText("blk = Blocks Per Game"),
                                  helpText("tov = Turnovers Per Game"),
                                  helpText("pf = Personal Fouls Per Game"),
                                  helpText("pts = Points Per Game"),
                                  helpText("Game = for coding purposes")
                          ),

                          tabItem(tabName = "plot",
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput("stat", "Choose the stat you want to plot over your player's career",
                                                      choices = c("gp", "gs", "fgm", "fga", "pctFG", "fg3m", "fg3a",
                                                                  "pctFG3","fg2m", "fg2a", "pctFG2", "minutes", "ftm",
                                                                  "fta",  "pctFT","oreb", "dreb", "treb", "ast",
                                                                  "stl", "blk", "tov","pf", "pts")),
                                          helpText("gp = Games Played"),
                                          helpText("gs = Games Started"),
                                          helpText("fgm = Field Goals Made"),
                                          helpText("fga = Field Goal Attempts"),
                                          helpText("pctFG = Field Goal Percentage"),
                                          helpText("fg3m = 3 Point Field Goals Made"),
                                          helpText("fg3a = 3 Point Field Goals Attempted"),
                                          helpText("pctFG3 = 3 Point Field Goal Percentage"),
                                          helpText("fg2m = 2 Point Field Goals Made"),
                                          helpText("fg2a = 2 point Field Goals Attempted"),
                                          helpText("pctFG2 = 2 Point Field Goal Percentage"),
                                          helpText("minutes = Total Minutes Played in the Season"),
                                          helpText("ftm = Free Throws Made"),
                                          helpText("fta = Free Throws Attempted"),
                                          helpText("pctFT = Free Throw Percentage"),
                                          helpText("oreb = Offensive Rebounds"),
                                          helpText("dreb = Defensive Rebounds"),
                                          helpText("treb = Total Rebounds"),
                                          helpText("ast = Assists"),
                                          helpText("stl = Steals"),
                                          helpText("blk = Blocks"),
                                          helpText("tov = Turnovers"),
                                          helpText("pf = Personal Fouls"),
                                          helpText("pts = Points")
                                      ),
                                      mainPanel(
                                          actionButton("statplot",
                                                       "Click to plot player's stat per game over career"),
                                          plotlyOutput("plot"),
                                          h4("If you wish to change the stat plotted,
                                          be sure to hit the button again!"),
                                          h4("If you wish to change the player,
                                          be sure to go back to the 'Choose your Player' tab,
                                          change your player, and hit the button again!"),

                                      )
                                  )),
                          tabItem(tabName = "prediction_intro",
                                  h1("Now, let's make predictions for next season!"),
                                  h3("Here is a table of active players that we can make predictions on."),
                                  h3("Check to see if the player you chose is active by utilizing the search feature."),
                                  h4("If your player is in this table, he is active, and you are ready to procede to the next tab."),
                                  h4("If your player is not in this table, scroll through this table and find someone you'd like to make predictions for.
                                     Then, go to the next tab and choose that active player."),
                                  actionButton("active", "Click to load active players"),
                                  dataTableOutput("table2"),
                                  helpText("isRookie -> Rookie = TRUE, Not Rookie = FALSE"),
                                  helpText("namePlayer = Name of Player"),
                                  helpText("countSeasons = Number of complete seasons this player has been in the NBA"),
                                  helpText("Game = for coding purposes")
                          ),

                          tabItem(tabName = "predictions",
                                  h1("Choose Your Active Player"),
                                  h4("Choose an active NBA player to make predictions for next year."),
                                  h4("Be sure to have correct spelling and capitalization.
                        Ex: LeBron James or D.J. Augustin"),
                                  h1(),
                                  textInput("player2", "Type an active NBA player"),
                                  actionButton("download2", "Click to get make your player's predictions"),
                                  h4("Once you have clicked the button, go to the next tab to see your predictions!"),
                                  h1(),
                                  h3("Important Note!"),
                                  h4("This data does not have seasonal patterns because the stats
                                     recorded only during the NBA season, not the whole year. There are no seasons,
                                     in an NBA season."),
                                  h4("Therefore, I cannot make the following models: Seasonal Naive, Holt/Winters,
                                     and seasonally adjusted ARIMA models."),
                                  h4("I will replace these models with models that do not require seasonality including:
                                     first and second order differencing and STL Decomposition.")

                          ),

                          tabItem(tabName = "simple",
                                  h1("Making Projections Using Simple Models"),
                                  h4("Remember! There will be a small gap between
                                             this year's season and the forecasted season
                                             because this season is currently being played."),
                                  h1(),
                                  h3("Naive Model"),
                                  plotOutput("naivemodel"),
                                  textOutput("naivefc"),
                                  h1(),
                                  h3("Drift Model"),
                                  plotOutput("driftmodel"),
                                  textOutput("driftfc"),
                                  h1(),
                                  h3("Mean Model"),
                                  plotOutput("meanmodel"),
                                  textOutput("meanfc")
                          ),
                          tabItem(tabName = "exp",
                                  h1("Making Projections Using Exponential Smoothing"),
                                  h1(),
                                  h3("Damped Holts Model"),
                                  plotOutput("holts"),
                                  textOutput("holtsfc"),
                                  h1(),
                                  h4("Note: This model will only predict for players who have been in the NBA
                                     for at least 5 years. There needs to be enough data to estimate this model.")

                          ),
                          tabItem(tabName = "arima",
                                  h1("Making Projections Using ARIMA"),
                                  h1(),
                                  h3("Auto ARIMA"),
                                  plotOutput("autoarima"),
                                  textOutput("arimafc")
                          ),
                          tabItem(tabName = "diff",
                                  h1("Making Projections Using Differencing"),
                                  h1(),
                                  h3("First Order Differencing"),
                                  plotOutput("firstdiff"),
                                  textOutput("diff1fc"),
                                  h1(),
                                  h3("Second Order Differencing"),
                                  plotOutput("seconddiff"),
                                  textOutput("diff2fc"),
                          ),
                          tabItem(tabName = "STL",
                                  h1("Making Predictions Using STL Decomposition"),
                                  h1(),
                                  h3("STL Decomp Model"),
                                  plotOutput("stl"),
                                  h1(),
                                  textOutput("stlfc")
                          ),

                          tabItem(tabName = "perf",
                                  h1("Performance Metrics on the Predictive Models"),
                                  h2("Which one is the best? Look for the lowest RMSE"),
                                  h4("Naive Model"),
                                  tableOutput("perf1"),
                                  h4("Drift Model"),
                                  tableOutput("perf2"),
                                  h4("Mean Model"),
                                  tableOutput("perf3"),
                                  h4("Holts Model"),
                                  tableOutput("perf4"),
                                  h4("Auto ARIMA Model"),
                                  tableOutput("perf5"),
                                  h4("First Order Differencing Model"),
                                  tableOutput("perf6"),
                                  h4("Second Order Differencing Model"),
                                  tableOutput("perf7"),
                                  h4("STL Decomposition Model"),
                                  tableOutput("perf8")
                          )
                      ))
    )
)







# Server
server <- function(input, output) {
    playerTSData <- eventReactive(input$download,{
        player_totals <- players_careers(players = c(input$player), modes = c("PerGame"),
                                         assign_to_environment = F);
        # Make a tsibble
        player_totals[[5]][[1]]$Game <- 1:nrow(player_totals[[5]][[1]])
        player_totals[[5]][[1]] %>% as_tsibble(index = Game) -> playerTS
        return(playerTS)
    })

    output$table <- renderDT({playerTSData() %>% datatable(options = list(scrollX = TRUE))
    })

    playerPlot <- eventReactive(input$statplot,{
        (playerTSData() %>%
             autoplot(get(input$stat)) + labs(title = paste(input$player, input$stat, "Per Game Over His Career"),
                                              x = "Year in the League", y = paste(input$stat))) %>%
            ggplotly()

    })

    output$plot <- renderPlotly({
        playerPlot()
    })

    output$summary <- renderPrint({
        dataset <- playerTSData()
        summary(dataset[,6:NCOL(dataset)])
    })

    Active_Players <- eventReactive(input$active,{
        Active_players <- df_dict_nba_players[which(df_dict_nba_players$isActive == "TRUE"),];
        Active_players <- Active_players[,c("isRookie", "namePlayer","countSeasons")]
        Active_players$Game <- 1:nrow(Active_players)
        Active_players %>% as_tsibble(index = Game) -> ActiveTS
        return(ActiveTS)
    })

    output$table2 <- renderDT({Active_Players() %>% datatable(options = list(scrollX = TRUE))
    })

    playerTSData2 <- eventReactive(input$download2,{
        player_totals2 <- players_careers(players = c(input$player2), modes = c("PerGame"),
                                          assign_to_environment = F);
        # Make a tsibble
        player_totals2[[5]][[1]]$Game <- 1:nrow(player_totals2[[5]][[1]])
        player_totals2[[5]][[1]] %>% as_tsibble(index = Game) -> playerTS2
        return(playerTS2)
    })

    NaiveModel <- reactive({
        playerTSData2() %>%
            model(NAIVE(pts))  %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2,"Points Per Game Forecasted Using the Naive Model"),
                x = "Year in the League")
    })

    output$naivemodel <- renderPlot({
        NaiveModel()
    })


    DriftModel <- reactive({
        playerTSData2() %>%
            model(RW(pts ~ drift())) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the Drift Model"),
                x = "Year in the League")
    })

    output$driftmodel <- renderPlot({
        DriftModel()
    })

    MeanModel <- reactive({
        playerTSData2() %>%
            model(MEAN(pts)) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the Mean Model"),
                x = "Year in the League")
    })

    output$meanmodel <- renderPlot({
        MeanModel()
    })

    Holts_method <- reactive({
        playerTSData2() %>%
            model(ETS(pts ~ error("A") +
                          trend("Ad", phi = 0.9) + season("N"))) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the Damped Holts Model"),
                x = "Year in the League")
    })

    output$holts <- renderPlot({
        Holts_method()
    })

    autoarima_method <- reactive({
        playerTSData2() %>%
            model(ARIMA(pts, stepwise=FALSE)) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the Auto ARIMA Model"),
                x = "Year in the League")
    })

    output$autoarima <- renderPlot({
        autoarima_method()
    })

    first_diff <- reactive({
        playerTSData2() %>%
            model(ARIMA(pts ~ pdq(0,1,0))) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the First Order Differencing Model"),
                x = "Year in the League")
    })

    output$firstdiff <- renderPlot({
        first_diff()
    })

    second_diff <- reactive({
        playerTSData2() %>%
            model(ARIMA(pts ~ pdq(0,2,0))) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the Second Order Differencing Model"),
                x = "Year in the League")
    })

    output$seconddiff <- renderPlot({
        second_diff()
    })

    STL_method <- reactive({
        playerTSData2() %>%
            model(decomposition_model(
                STL(pts ~ trend(window = 7), robust = TRUE),
                NAIVE(season_adjust)
            )) %>%
            forecast(h = 2) %>%
            autoplot(playerTSData2()) + labs(
                title = paste(input$player2, "Points Per Game Forecasted Using the STL Decomp Model"),
                x = "Year in the League")
    })

    output$stl <- renderPlot({
        STL_method()
    })

    performance1 <- reactive({
        playerTSData2() %>%
            model(Naive = NAIVE(pts)) %>%
            accuracy()
    })

    output$perf1 <- renderTable({
        performance1()
    })

    performance2 <- reactive({
        playerTSData2() %>%
            model(Drift = RW(pts ~ drift())) %>%
            accuracy()
    })

    output$perf2 <- renderTable({
        performance2()
    })

    performance3 <- reactive({
        playerTSData2() %>%
            model(Mean = MEAN(pts)) %>%
            accuracy()
    })

    output$perf3 <- renderTable({
        performance3()
    })

    performance4 <- reactive({
        playerTSData2() %>%
            model(Holts = ETS(pts ~ error("A") +
                          trend("A") + season("N"))) %>%
            accuracy()
    })

    output$perf4 <- renderTable({
        performance4()
    })

    performance5 <- reactive({
        playerTSData2() %>%
            model(AutoARIMA = ARIMA(pts, stepwise=FALSE)) %>%
            accuracy()
    })

    output$perf5 <- renderTable({
        performance5()
    })

    performance6 <- reactive({
        playerTSData2() %>%
            model(First_Order_Differencing = ARIMA(pts ~ pdq(0,1,0))) %>%
            accuracy()
    })

    output$perf6 <- renderTable({
        performance6()
    })

    performance7 <- reactive({
        playerTSData2() %>%
            model(Second_Order_Differencing = ARIMA(pts ~ pdq(0,2,0))) %>%
            accuracy()
    })

    output$perf7 <- renderTable({
        performance7()
    })

    performance8 <- reactive({
        playerTSData2() %>%
            model(STL_Decomp = decomposition_model(
                STL(pts ~ trend(window = 7), robust = TRUE),
                NAIVE(season_adjust)
            )) %>%
            accuracy()
    })

    output$perf8 <- renderTable({
        performance8()
    })

    fc1 <- reactive({
        playerTSData2() %>%
            model(stlf = decomposition_model(
                STL(pts ~ trend(window = 7), robust = TRUE),
                NAIVE(season_adjust)
            )) %>% forecast(h = 2) -> nextyear1
        return(nextyear1$.mean[1])
    })

    output$stlfc <- renderText({
        paste("The STL Decomp model predicts", input$player2, "to average", fc1(), " points per game next season")
    })






    fc2 <- reactive({
        playerTSData2() %>%
            model(NAIVE(pts)) %>% forecast(h = 2) -> nextyear2
        return(nextyear2$.mean[1])
    })

    output$naivefc <- renderText({
        paste("The Naive model predicts", input$player2, "to average", fc2(), " points per game next season")
    })




    fc3 <- reactive({
        playerTSData2() %>%
            model(RW(pts ~ drift())) %>% forecast(h = 2) -> nextyear3
        return(nextyear3$.mean[1])
    })

    output$driftfc <- renderText({
        paste("The Drift model predicts", input$player2, "to average", fc3(), " points per game next season")
    })






    fc4 <- reactive({
        playerTSData2() %>%
            model(MEAN(pts)) %>% forecast(h = 2) -> nextyear4
        return(nextyear4$.mean[1])
    })

    output$meanfc <- renderText({
        paste("The Mean model predicts", input$player2, "to average", fc4(), " points per game next season")
    })






    fc5 <- reactive({
        playerTSData2() %>%
            model(ETS(pts ~ error("A") +
                          trend("Ad", phi = 0.9) + season("N"))) %>% forecast(h = 2) -> nextyear5
        return(nextyear5$.mean[1])
    })

    output$holtsfc <- renderText({
        paste("The Damped Holts model predicts", input$player2, "to average", fc5(), " points per game next season")
    })






    fc6 <- reactive({
        playerTSData2() %>%
            model(ARIMA(pts, stepwise=FALSE)) %>% forecast(h = 2) -> nextyear6
        return(nextyear6$.mean[1])
    })

    output$arimafc <- renderText({
        paste("The Auto ARIMA model predicts", input$player2, "to average", fc6(), " points per game next season")
    })






    fc7 <- reactive({
        playerTSData2() %>%
            model(ARIMA(pts ~ pdq(0,1,0))) %>% forecast(h = 2) -> nextyear7
        return(nextyear7$.mean[1])
    })

    output$diff1fc <- renderText({
        paste("The First Order Differencing model predicts", input$player2, "to average", fc7(), " points per game next season")
    })






    fc8 <- reactive({
        playerTSData2() %>%
            model(ARIMA(pts ~ pdq(0,2,0))) %>% forecast(h = 2) -> nextyear8
        return(nextyear8$.mean[1])
    })

    output$diff2fc <- renderText({
        paste("The Second Order Differencing model predicts", input$player2, "to average", fc8(), " points per game next season")
    })

}







# Run the application
shinyApp(ui = ui, server = server)
