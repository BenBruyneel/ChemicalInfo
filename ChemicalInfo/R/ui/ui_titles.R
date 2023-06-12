
titleText <- "Mass Spectrometry / Chemistry Info"
contactPreText <- "Questions/Comments: "
contactName <- "Ben Bruyneel"
contactEmail <- "benbruyneel@gmail.com"
contactText <- paste(c(contactPreText,
                       "<a href = mailto:",
                       contactEmail,">",
                       contactName,"</a>"), collapse ="")

ui_titles <- tagList(
  fluidRow(
    column(9,
           titlePanel(titleText)
    ),
    column(3,
           style = "text-align: right; padding-top: 15px;",
           HTML(contactText)
    )
  )
)
