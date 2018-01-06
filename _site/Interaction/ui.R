library(readr)
library(shiny)
library(ggplot2)

shinyUI(navbarPage("QBS Fall 2017 - Jessie Chiu",
                   tabPanel("Interaction Visualized",
                            sidebarPanel(
                              width = 3,
                              selectInput("select", label = "Select Interaction:",
                                          choices = c("Without Interaction", "With Interaction"), selected = "With Interaction"),
                              helpText("Note: 'With Interaction' includes the interaction between MENA and SIGI values.")
                            ),

                            mainPanel(
                              imageOutput("plot")
                            )

                   ),

                   tabPanel(title=HTML("</a></li><li><a href='http://homepage.ntu.edu.tw/~b04701204/site/index.html' target='_blank'>Back"))

)
)





