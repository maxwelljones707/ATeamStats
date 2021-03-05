################################################################################

#############################A-TEAM STATS#######################################
##Authors: Max Jones and Julian Wilson
##Date: 3/4/2021
################################################################################
library(shiny)
library(tidyverse)
library(moderndive)
library(readxl)
library(janitor)
library(ggthemr)
library(scales)
library(googlesheets4)
library(shinyWidgets)


options(httr_oob_default=TRUE) 
gs4_auth() 


big <- read_sheet("https://docs.google.com/spreadsheets/d/1nij5_MGT6T6xiyKLQPZVpAcSZhRmB07zZ1pnZ77rdQw/edit#gid=0")  ##Spreadsheet URL


ggthemr("pale")




ui <- fluidPage(
    
    setBackgroundColor(
        color = c("#00FFFF"),
        gradient = "linear",
        direction = "bottom"
    ),
    
    titlePanel("Match Data"), 
    sidebarLayout(
        sidebarPanel(
            #actionButton("update", "Update data"), 
            selectInput("big", "Select match; select 'Match_Number' to see season long data", c("Match_Number", unique(as.character(big$Match_Number)))),  
            #checkboxInput("check", "Only toss up", value = FALSE), 
            checkboxInput("r1", "Exclude round 1 (Toss-up)", value = FALSE),
            checkboxInput("r2", "Exclude round 2 (Team-directed)", value = FALSE),
            checkboxInput("r3", "Exclude round 3 (Toss-up)", value = FALSE),
            selectInput("category", "Select a category to examine", c("Category", unique(as.character(big$Category))))
            
        ),
        
        mainPanel(
            plotOutput("category"),
            textOutput("score"),
            textOutput("score2"), 
            
            plotOutput("frequency")
            
            
        )
    )
)


# Define server logic 
server <- function(input, output) 
{
    
    output$category <- renderPlot(
        {
            big1 <- big
            if(input$big != "Match_Number")
            {
                big1 <- big1 %>% filter(Match_Number == input$big)
            }
            if(input$r1 == TRUE)
            {
                big1 <- big1 %>% filter(Round_Number != 1)
            }
            if(input$r2 == TRUE)
            {
                big1 <- big1 %>% filter(Round_Number != 2)
            }
            if(input$r3 == TRUE)
            {
                big1 <- big1 %>% filter(Round_Number != 3)
            }
            
            
            
            ggplot(data = big1, aes(x = Category, fill = Team_Answered)) + 
                geom_bar() + 
                labs(y = "Number of questions", x = "Category", title = "Frequency of Categories") +
                theme(axis.text.x = element_text(size = 12, hjust = 0.5), axis.text.y = element_text(size = 12, hjust = 1), axis.title = element_text(size = 16, hjust = 0.5), title = element_text(size = 18, hjust = 0.5)) +   
                coord_flip() + 
                scale_fill_discrete(name = "Team Answered") + 
                scale_y_continuous(breaks = pretty_breaks())
        }
    )
    
    
    
    

    output$frequency <- renderPlot(
        {
            if(input$category != "Category")
            {
                big %>%  filter(Category == input$category) %>% 
                    ggplot(aes(x = as.factor(Match_Number), fill = Team_Answered)) + 
                    geom_bar() +
                    labs(x = "Match", y = "Number of questions", title = "Frequency of Specific Category Over Time") +
                    theme(axis.text.x = element_text(size = 12, hjust = 0.5), axis.text.y = element_text(size = 12, hjust = 1), axis.title = element_text(size = 16, hjust = 0.5), title = element_text(size = 18, hjust = 0.5)) + 
                    scale_fill_discrete(name = "Team Answered")  + 
                    scale_x_discrete() + scale_y_discrete()
                
            }
        }
    ) 
    
    output$score <- renderText(
        {
            big2 <- big
            if(input$r1 == TRUE)
            {
                big2 <- big2 %>% filter(Round_Number != 1)
                paste("1")
            }
            if(input$r2 == TRUE)
            {
                big2 <- big2 %>% filter(Round_Number != 2)
                paste("2")
            }
            if(input$r3 == TRUE)
            {
                big2 <- big2 %>% filter(Round_Number != 3)
                paste("3")
            }
            if(input$big == "Match_Number") {
                paste("Our total points are: ", sum(big2$Team_Answered == "Us")  * 10 - (sum(big2$Neg == "Us") * 5), ". Our average points per game is: ", round((sum(big2$Team_Answered == "Us")  * 10 - sum(big2$Neg == "Us") * 5)  /  max(big2$Match_Number)))}
            else if(input$big != "Match_Number") {
                big2 <- big2 %>%  filter(big2$Match_Number == input$big)
                paste("Our score is: ", sum(big2$Team_Answered == "Us")  * 10 - (sum(big2$Neg == "Us") * 5))
            }
        }
        
    )
    
    output$score2 <- renderText(
        {
            big3 <- big
            if(input$r1 == TRUE)
            {
                big3 <- big3 %>% filter(Round_Number != 1)
            }
            if(input$r2 == TRUE)
            {
                big3 <- big3 %>% filter(Round_Number != 2)
            }
            if(input$r3 == TRUE)
            {
                big3 <- big3 %>% filter(Round_Number != 3)
            }
            
            if(input$big == "Match_Number") {
                paste("Opponents' total points are: ", sum(big3$Team_Answered == "Them")  * 10 - (sum(big3$Neg == "Them") * 5), ". Average opponents' points per game is: ", round((sum(big3$Team_Answered == "Them")  * 10 - sum(big3$Neg == "Them") * 5)  /  max(big3$Match_Number)))
                
            }
            else if(input$big != "Match_Number") {
                big3 <- big3 %>%  filter(big3$Match_Number == input$big)
                paste("Opponent's score is: ", sum(big3$Team_Answered == "Them")  * 10 - (sum(big3$Neg == "Them") * 5))
            }
        }
    )
    
}    


# Run the application 
shinyApp(ui = ui, server = server)


