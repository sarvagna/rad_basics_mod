########################################################
## Generate word cloud - Jeevan
########################################################
wc_book_list <- c("A Mid Summer Night's Dream" = "summer",
                  "The Merchant of Venice" = "merchant",
                  "Romeo and Juliet" = "romeo")

output$ui_word_cloud <- renderUI({
  tagList(
    wellPanel(
      selectInput("selection", "Choose a book:",
                  choices = wc_book_list),
      actionButton("wc_plot", "Plot", width = "100%")
    ),
    wellPanel(
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    help_modal("Word cloud help", "wc_help",
               help_file = inclMD(file.path(getOption("radiant.path.basics"),"app/tools/help/clt.md")))
  )
})

wc_plot_height <- function() 700
wc_plot_width <- function() 700

## output is called from the main radiant ui.R
output$word_cloud <- renderUI({
  register_print_output("summary_wc_calc", ".summary_wc_calc")

  register_plot_output("plot_word_cloud", ".plot_word_cloud",
                       height_fun = "wc_plot_height",
                       width_fun = "wc_plot_width")

  wc_output_panel<- tagList(
  tabPanel("Summary", verbatimTextOutput("summary_wc_calc")),
  tabPanel("Plot",
           plot_downloader("wc_download", height = wc_plot_height()),
           plotOutput("plot_word_cloud", width = "100%", height = "100%"))
  )

  stat_tab_panel(menu = "Basics > Word cloud",
                 tool = "Word cloud generator",
                 data = NULL,
                 tool_ui = "ui_word_cloud",
                 output_panels = wc_output_panel)

})

.summary_wc_calc <- reactive({
  str(terms())
})

terms<- eventReactive(input$wc_plot, {
  if(is.null(input$selection)) return("Please choose a book")
  withProgress({
    setProgress(message = "Processing corpus...")
    getTermMatrix(input$selection)
  })
})

getTermMatrix <- function(book) {
  text <- readLines(sprintf("./%s.txt", book),
                    encoding="UTF-8")

  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))

  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))

  m = as.matrix(myDTM)

  sort(rowSums(m), decreasing = TRUE)
}

# output$plot_word_cloud <- renderPlot({
#   v <- terms()
#   wordcloud(names(v), v, scale=c(4,0.5),
#                 min.freq = input$freq, max.words=input$max,
#                 colors=brewer.pal(8, "Dark2"))
# })

.plot_word_cloud <- function(terms = terms()) {

  if (not_pressed(input$wc_plot)) return("** Press the Plot button to get wordcloud **")

  req(input$freq)
  req(input$max)

  #plot(terms,type="values",shiny=T)
  wordcloud(names(terms), terms, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
}
