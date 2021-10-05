
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("shinyMobile")
#install.packages("plotly")

library(shiny)
library(dplyr)
library(tidyr)
library(shinyMobile)
library(wordcloud2)
library(plotly)

hearings <- readRDS("hearings.Rdata")
termsdf <- readRDS("termsForProfs.Rdata")
profstats <- readRDS("profstats.Rdata")


#user interface
ui <- f7Page(
  tags$style(HTML("
    html{max-width: 600px;
          margin: 0 auto;}
    .title{padding-top: 10px;}
    a {margin-left: 16px;
        font-size: 10px;}
    div.block-title{font-size: 12px;}
  ")),
  title = "Professions Trangressions",
  f7TabLayout(
    navbar = f7Navbar(
      title = "Professions Transgressions",
      subtitle = "Explore health professionals' misconduct hearings",
      hairline = FALSE,
      shadow = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      id = "tabs",
      f7Tab(
        f7Link(label = "Hearing information from the HCPTS", href = 'https://www.hcpts-uk.org/hearings/search/Search/'),
        tabName = "Words used",
        active = TRUE,
        f7Select(
          inputId = "profession",
          label = "Show me words relating to:",
          choices = colnames(termsdf)[1:14],
          selected = "Paramedic"
        ),
        f7Radio(
          inputId = "outputChoice",
          label = "When I choose a word show me:",
          choices = c("Related hearings", "Word stats"),
          selected = "Related hearings"
        ),
        f7Block(
          "Select a word:",
          wordcloud2Output(outputId = "cloud", width = "100%"),
          #Javascript for selecting word: https://stackoverflow.com/questions/44502965/is-it-possible-to-create-a-click-event-in-shiny-with-wordcloud2
          tags$script(HTML(
            "$(document).on('click', '#canvas', function() {",
            'word = document.getElementById("wcSpan").innerHTML;',
            "Shiny.onInputChange('selected_word', word);",
            "});"
          ))
        ),
        f7Popup(
          id = "wordStats",
          title = "Word frequency",
          plotlyOutput(outputId = "wordplot")
        ),
        f7Popup(
          id = "hearings",
          title = "Hearings",
          htmlOutput(outputId = "hearingText")
        )
      ),
      f7Tab(
        tabName = "Allegation stats",
        plotlyOutput(outputId = "profstats")
      )
    )
  )
)



server <- function(input, output) {
  
  output$cloud <- renderWordcloud2({
    mutate(termsdf, word = terms, freq = get(input$profession)) %>% subset(dodgy == 1) %>% select(word, freq) -> selectedTerms
    wordcloud2(data = selectedTerms)
  })
  
  
  
  output$wordplot <- renderPlotly({
    
    selectedWord <- sub(":[0-9]*", "", input$selected_word)
    
    subset(termsdf, terms == selectedWord) %>% select(1:14) %>%  gather() -> profsTermFreq
    
    plot_ly(y = profsTermFreq$key, x = profsTermFreq$value, type = "bar") %>% 
      layout(xaxis = list(title = "Term frequency"), margin = list(l = 200), width = 370, dragmode = FALSE, title = paste0("Frequency of \"", selectedWord, "\" by profession"), margin = c(2,2,2,2)) %>% 
      config(displayModeBar = F)
  })
  
  #https://cran.r-project.org/web/packages/shinyMobile/shinyMobile.pdf
  
  
  observeEvent(input$selected_word, {
    if(input$outputChoice == "Word stats"){
      updateF7Popup(id = "wordStats")
    } else {
      updateF7Popup(id = "hearings")
    }
  }) 
  

  
  output$hearingText <- renderUI({
    selectedWord <- sub(":[0-9]*", "", input$selected_word)
    hearingtext <- ""
    for(i in 1:nrow(hearings)){
      if (grepl(selectedWord, hearings[i,"allegation"])){
        hearingtext <- HTML(paste(hearingtext,"<br><br><b>Profession:</b>", hearings[i, "profession"], "<br><b>Outcome:</b>", hearings[i,"outcome"],"<br><b>Allegation:</b>", hearings[i,"allegation"]))
      }
    }
    hearingtext
  })
  
  
  output$profstats <- renderPlotly({
    subset(profstats, year == 2019) %>% mutate(ratePer1000 = Cases/Registrants*1000) %>% arrange(ratePer1000) %>%  select(Profession, ratePer1000) -> plotCaseStats
    plot_ly(y = plotCaseStats$ratePer1000, x = plotCaseStats$Profession, type = "bar", height = 550) %>% 
      layout(yaxis = list(title = "Cases per 1000 professionals"), 
             xaxis = list(title = "", categoryorder = "array", categoryarray = rev(plotCaseStats$Profession)),
             margin = list(b = 210), 
             dragmode = FALSE, title = "Allegation rate by profession (2019)", margin = c(2,2,2,2)) %>% 
      config(displayModeBar = F)
  })
  
  
  
}


shinyApp(ui = ui, server = server)


