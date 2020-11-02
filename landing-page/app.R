library(shiny)

ui <- shinyUI(
  navbarPage(
    title = "Introduction to R",
    fluid = TRUE,
    theme = "style.css",
    collapsible = TRUE,
    tabPanel(
      "Home",
      tags$head(tags$script(
        HTML(
          '
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '
        )
      )),
      fluidRow(
        h1("Learn R from the comfort of your console"),
        column(
          width = 4,
          HTML(
            "<center>
            <span style='color:#80A8D7'>
            <i class='fas fa-window-restore fa-4x'></i>
            </span>"
          ),
          br(),
          br(),
          "We have created a six module course
           that introduces topics commonly used
           when conducting data analyses using R."
        ),
        column(
          width = 4,
          HTML(
            "<center>
             <span style='color:#80A8D7'>
             <i class='fab fa-r-project fa-4x'></i>
             </span>"
          ),
          br(),
          br(),
          "These modules were created as part
          of a research study examining best
          practices for learning R - after
          clicking on the link below, you
          can optionally opt into participating
          in this study"
        ),
        column(
          width = 4,
          HTML(
            "<center>
            <span style='color:#80A8D7'>
            <i class='fas fa-hand-holding-usd fa-4x'></i>
            </span>"
          ),
          br(),
          br(),
          "The first 150 participants who
           enroll in the study and complete the
          course will receive a $5 amazon gift card."
        ),
        
      ),
      br(),
      br(),
      includeHTML("home.html")
    ),
    tabPanel(
      "Consent",
      includeHTML("consent.html"),
      conditionalPanel(
        condition = "output.random == 'A'",
        actionButton(
          inputId = "yes",
          label = "Yes, I agree to this study",
          icon = icon("check"),
          onclick = "window.open('http://google.com')"
        ),
        actionButton(
          inputId = "no",
          label = "No, I do not agree to this study",
          onclick = "fakeClick('Home')"
        ),
        conditionalPanel(
          condition = "output.random == 'B'",
          actionButton(
            inputId = "yes",
            label = "Yes, I agree to this study",
            icon = icon("check"),
            onclick = "window.open('http://yahoo.com')"
          ),
          actionButton(
            inputId = "no",
            label = "No, I do not agree to this study",
            onclick = "fakeClick('Home')"
          )
        ),
        hr()
      )
      
    )
  )
)


server <- function(input, output) {
  output$random <- reactive({
    ifelse(runif(1) > 0.5, "A", "B")
  })
  outputOptions(output, "random", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
