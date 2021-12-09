library(shiny)
library(dplyr)
library(DT)

capture_rates <- c(.83, .93)

capture_strategy <- c('KO', 'Capture')

chain_odds <- c(3855, 3640, 3449, 3277, 3121, 2979,2849, 2731, 2621, 2521,
                2427, 2341, 2259, 2185, 2114, 2048, 1986, 1927, 1872, 1820,
                1771, 1724, 1680, 1638, 1598, 1560, 1524, 1489, 1456, 
                1310, 1285, 1260, 1236, 1213, 1192, 993, 799, 400, 200, 99)


#' Calculate the average time to achieve a chain state
#' 
#' @param N The desired chain length to be achieved
#' @param encounter_time The average length in seconds of the Pokemon encounter
#' @param chain_prob The continuation probability of the chain
#' 
#' @return Time in hours to achieve chain
total_encounter_time <- function(N, encounter_time, chain_prob){
    encounter_time <- encounter_time/60 #convert to mins
    
    probs <- chain_prob^c(1:N) #prob you could get to each chain
    fails <- probs[c(1:(N-1))]  #failure state probs
    success <- probs[N] #prob of getting your chain
    normed_fails <- fails/sum(fails)*(1-success) #prob failed attempt ends in outcome
    normed_outcomes <- c(normed_fails, success) #probs for each state
    
    #Expected chain attempts to reach chain goal
    ttl_attempts <- 1/(chain_prob^N)
    failed_attempts <- 1 - ttl_attempts
    
    #Add up how much time those failed attempts would take
    time_distribution <- normed_outcomes*ttl_attempts*c(1:N)*encounter_time
    
    #Return total time of fails and successes in hours
    sum(time_distribution)/60
}


#' Vectorize the encounter time calculations
vttime <- Vectorize(total_encounter_time)

#' Calculate times for each strategy
calculate_times <- function(df, reroll_time_secs){
    df2 <- 
        df %>%
        mutate(
            ttl_chain_time = vttime(N=streak_stop, encounter_time=encounter_time, chain_prob = chain_rate)
        )
    #Calculate chance of shiny on a roll of 4 at given odds level
    #Use pmf of binomial and get non-zero probability
    df2$shiny_chance <- 1-dbinom(0, size=4, prob=1/chain_odds[df2$streak_stop])
    
    df2$expected_rolls <- 1/df2$shiny_chance #Mean of geometric distribution is simply 1/p
    df2$ttl_roll_time <- (df2$expected_rolls*(reroll_time_secs))/60/60 #convert to hours
    df2$ttl_time <- df2$ttl_roll_time + df2$ttl_chain_time
    
    #Sort by total time
    df2 <- 
        df2 %>%
        arrange(ttl_time)
    
    return(df2)
}


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("PokeRadar (BDSP) Shiny Hunting Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("avg_catch_time",
                        "Average time to catch target Pokemon (seconds):",
                        min = 1,
                        max = 600,
                        value = 180),
            sliderInput("avg_KO_time",
                        "Average time to faint target Pokemon (seconds):",
                        min = 1,
                        max = 300,
                        value = 30),
            sliderInput("avg_reroll_time",
                        "Average time to reset PokeRadar (seconds)",
                        min = 5,
                        max = 300,
                        value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput('answer'),
            DTOutput('df2')
        )
    )
)

# Define server logic 
server <- function(input, output) {

   # times <- reactive(c(input$avg_KO_time, input$avg_catch_time))
    
    
    resultstable <- reactive({
        df <- expand.grid(c(1:40), c(.83,.93))
        df$v3 <- c(rep(input$avg_KO_time, 40), rep(input$avg_catch_time, 40) )
      
        colnames(df) <- c( 'streak_stop','chain_rate', 'encounter_time')
        
        df$encounter_type <- c(rep('faint', 40), rep('catch', 40)) 
        
        #The probability you can get a streak of a given rate
        df$prob_of_streak <- df$chain_rate^df$streak_stop   
        
        df_temp <- calculate_times(df, input$avg_reroll_time)
        return(df_temp)
    })
    
    output$df2 <-renderDT(resultstable() %>% 
                              select(streak_stop, encounter_type,ttl_chain_time, ttl_roll_time, ttl_time) %>%
                              mutate(
                                  ttl_chain_time = round(ttl_chain_time, 1),
                                  ttl_roll_time = round(ttl_roll_time, 1),
                                  ttl_time = round(ttl_time, 1)
                              )
                          )
    
    
    
    output$answer <- renderText({
        paste0("The optimal strategy is to target chains of ",
               resultstable()$streak_stop[1],
               ' by ' , resultstable()$encounter_type[1] ,'ing Pokemon',
               ' and then resetting your PokeRadar until you get a Shiny. ',
               'Your resetting and encounter portion of your hunt will take about ',
               round(resultstable()$ttl_time[1], 1) ,
               ' hours.  Happy hunting!')
    })

    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
