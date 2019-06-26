#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(grid)
library(ggrepel)
data_path <- '/Users/larsenb/Documents/ImpactFactors/'

## Function to read data
import_JIF <- function(incsv){
  if_data <- read.csv(incsv,skip = 1,stringsAsFactors = F,na.strings = "Not Available")
  if_data$Year <- as.numeric(if_data$Year)
  if_data <- if_data %>% drop_na()
  if_data_title <- gsub(
    x=colnames(
      read.csv(incsv,
               check.names = F)
    )[1],
    pattern = "Journal Profile: ",
    replacement = "")
  if_data$Journal <- str_to_title(if_data_title)
  return(if_data)
}

## Read in data
journal_csvs<-list.files(path = data_path,pattern = "*.csv")
ImpactTable <- import_JIF(sprintf("%s%s",data_path,journal_csvs[1]))
for (f in journal_csvs[2:length(journal_csvs)]) {
  this_journal <- import_JIF(sprintf("%s%s",data_path,f))
  ImpactTable <- rbind(ImpactTable,this_journal)
}
# Trim PNAS name
ImpactTable$Journal[startsWith(x = ImpactTable$Journal, prefix = "Proceedings")] = "PNAS"

ui <- fluidPage(
  # Application title
  titlePanel("Journal Impact Factors"),
  
  inputPanel(
    pickerInput("Journal","Journal",sort(unique(ImpactTable$Journal)),multiple = T,options = list(`actions-box` = TRUE)),
    selectInput("Year","Rank Year",unique(ImpactTable$Year))
  ),
  
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("outplot"), plotOutput("yearplot"))
      )
    )
  
)

server <- function(input, output) {
  
  output$outplot <- renderPlot({

    thisTable <- ImpactTable %>%
      filter(Journal %in% input$Journal)

    g<-ggplot(data = thisTable, aes(x = Year, y = Journal.Impact.Factor,Group = Journal, color = Journal)) +
      geom_line()+
      geom_point()+
      theme(plot.margin = unit(c(1,3,1,1), "lines")) +
      geom_text_repel(data = thisTable %>% filter(Year == 2018),aes(label=Journal)) +
      theme(legend.position = "none")+
      xlab("Year") + ylab("Impact Factor") + ggtitle("Impact by Year")
    gt <- ggplotGrob(g)
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    grid.draw(gt)

  })
  
  output$yearplot <- renderPlot({
    yearTable <- ImpactTable %>%
      filter(Year == input$Year)%>%
      filter(Journal %in% input$Journal)
    
    ggplot(data = yearTable, aes(x = reorder(Journal,Journal.Impact.Factor), y = Journal.Impact.Factor,Group = Journal)) + 
      geom_bar(stat = "identity",aes(fill = Journal)) + coord_flip() + 
      geom_text(aes(label=Journal.Impact.Factor),hjust=1) +
      theme(legend.position = "none")+
      ylab("Impact Factor") + xlab("Journal") + ggtitle(sprintf("Ranking for year %s",input$Year))
  })
  
}

shinyApp(ui = ui, server = server)

