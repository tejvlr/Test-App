library(shiny)
library(ggplot2)

# Note frequencies (simplified mapping)
note_freqs <- c(A = 440, B = 494, C = 523, D = 587, E = 659, F = 698, G = 784)

ui <- fluidPage(
  titlePanel("Sound Wave Visualizer"),
  
  # Input for user to enter text
  textInput("text_input", "Enter a word or phrase:"),
  
  # Output plot space for the waveform
  plotOutput("waveform_plot")
)


server <- function(input, output) {
  
  output$waveform_plot <- renderPlot({
    
    # Handle errors gracefully
    text_input <- input$text_input
    if (is.null(text_input) || nchar(text_input) == 0) return(NULL)  

    frequencies <- sapply(tolower(strsplit(text_input, "")[[1]]), function(x) {
      if (x %in% names(note_freqs)) {
        note_freqs[x]
      } else {
        440 # Default to A440 if the note isn't found
      }
    })

  
    if (length(frequencies) == 0) return(NULL)


    time <- seq(0, 1, length.out = 1000)
    
    waveforms <- sapply(frequencies, function(freq) sin(2 * pi * freq * time))

    combined_waveform <- rowSums(waveforms)

    # Create the plot using ggplot2
    ggplot(data.frame(time = time, waveform = combined_waveform), aes(x = time, y = waveform)) +
      geom_line() +
      labs(title = "Combined Sound Wave", x = "Time", y = "Amplitude") + 
      ylim(min(combined_waveform), max(combined_waveform))  # Important for showing the entire waveform.
    
  })
}

shinyApp(ui = ui, server = server)
