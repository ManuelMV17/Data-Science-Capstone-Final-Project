suppressWarnings(library(shiny))
suppressWarnings(library(markdown))
shinyUI(navbarPage("Data Science Capstone: Final Project",
                   tabPanel("Predict the Next Word",
                            br(),
                            # Sidebar
                            sidebarLayout(
                                sidebarPanel(
                                    textInput("inputString", "Text input",value = ""),
                                    br(),
                                    helpText("It might take a few seconds to load the data. Please wait."),
                                    br(),
                                    helpText("Use only English words."),
                                    br()
                                ),
                                mainPanel(
                                    h2("Predicted word:"),
                                    verbatimTextOutput("prediction"),
                                    strong("Sentence Input:"),
                                    tags$style(type='text/css', '#text1 {background-color: rgba(0,255,255,0.40); color: black;}'), 
                                    textOutput('text1'),
                                    br(),
                                    strong("Note:"),
                                    tags$style(type='text/css', '#text2 {background-color: rgba(0,255,255,0.40); color: black;}'),
                                    textOutput('text2')
                                )
                            )
                            
                   )
)
)