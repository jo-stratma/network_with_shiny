### Shiny App tutorial from 
# https://shiny.rstudio.com/tutorial/

#### This script will allow to present data from a correlation matrix in a 
# Network, in which the nodes can be manually selected, added and removed
# instead of presenting all the data at ones in a network. 
# Presenting all data from large data sets might not give a clear picture of 
# potentially interesting connections, hence with this script, specific nodes can 
# be selected for a network. 
# This script is supposed to be an example, and can be modified according to the data 
# layout of your own data in terms of column name, column numbers and so on ...   

R
library(shiny)
library(visNetwork)
library(data.table)
library(tidyverse)
library(dplyr)
library(igraph)
library(DT)
library(shinythemes)

# create a test data set 
dat <- data.frame(
  tf=c("A","B", "C", "D","E", "F", "G","H", "I", "J"),
  target=sample(c('k','l','m'), size=10, replace=T), 
  correlation=runif(n=10, min= 0, max=1),
  p_value=runif(n=10, min= 0, max=.06))


# build the shiny app 
#####
ui <- fluidPage( theme = shinytheme("united"),
titlePanel("Test Network"),
  sidebarLayout( 
    sidebarPanel(width=2,  
    selectInput('tf', "Transcription Factor:", choice=dat$tf, multiple=T),
    br(),
    selectInput('target', "Target:", choice=dat$target,  multiple=T)  
  ),
    mainPanel(
      tabsetPanel( type="tabs",
      tabPanel("Transcription Factors", dataTableOutput("tab1")),
      tabPanel("TF-Target Pair", dataTableOutput("tab2")),
      tabPanel("Network Plot", fillPage(visNetworkOutput("plot", height=800))),
      tabPanel("Network Data", dataTableOutput("tab7"))
            ) 
    )
  )
)     

server <- function(input, output) {

tfs <- reactive({dat[dat$tf %in% input$tf,]}) #subset main data by tfactors of choice
output$tab1 <- renderDataTable({tfs()}) # shows plot 

dt <- reactive({tfs()[tfs()$target %in% input$target,]}) #subset the subsetted data by targets of choice
output$tab2 <- renderDataTable({dt()}) # shows plot

# create a list of disinct factors found across all TFs and Targets 
tfactors  <- reactive({dt() %>% distinct(tf) %>% rename(label=tf)}) # extract the distinct transcription factors from the subsetted dataframe
targets  <- reactive({dt() %>% distinct(target) %>% rename(label=target)}) # extract the distinct targets from the subsetted dataframe 
combo <- reactive({full_join(tfactors(), targets(), by = "label")}) #join the two distinct datasets into one  

# create the nodes
id <- reactive({c(combo()$label)})  # store the distinct values in the variable "id"
label <- reactive({c(combo()$label)})  # store the distinct values in the variable "Label"
nodes <- reactive({data.frame(id(),label()) %>% rename(id=id.., label=label..)}) # create a data frame called Nodes 

# create the edges 
from <- reactive({c(dt()$tf)})
to <- reactive({c(dt()$target)})
title <- reactive({c('regulator')})
edges <- reactive({data.frame(from(),to(),title()) %>% rename(from=from.., to=to.., title=title..)})


output$tab7 <- renderDataTable({edges()}) # shows plot 

# create network plot  ==
output$plot <- renderVisNetwork({
visNetwork(nodes(), edges()) %>%  
visEdges(arrows = 'to') %>% 
visNodes(color = list(background='palegreen', border='darkgreen', highlight='yellow'), shadow = list(enabled = TRUE, size = 10)) %>%
visIgraphLayout(type = "full", layout = "layout_nicely") %>% 
visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T)
})

}
  
shinyApp(ui, server)
