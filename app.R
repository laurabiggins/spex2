# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

#pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#options("golem.app.prod" = TRUE)
#options(shiny.reactlog = TRUE)

#spex::run_app() # add parameters here (if any)

library(shiny)
library(magrittr)

#pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

#metadata0 <- readRDS("inst/extdata/metaFem_edited.rds")
metadata <- qs::qread("data/metadata.qs")
dataset  <- readRDS("inst/extdata/femExpression2.rds")
of_interest <- readRDS("inst/extdata/of_interest2.rds")
sample_names <- "sample_name"
measure_names <- rownames(dataset)
bab_light_blue <- "#00aeef"
bab_dark_blue <- "#1d305f"

ui <- tagList(
  
  #bootstrapDep,

  fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    theme = bslib::bs_theme(
      bg = bab_dark_blue,
      fg = "white",
      primary = bab_light_blue,
      secondary = bab_light_blue
    ),
    titlePanel(
      tags$img(
        src = "BI_logo_grey.png", 
        #src = "bioinformatics_logo_square_small.png", 
        style="margin-top: -10px; padding-right:10px; padding-bottom:10px", 
        width = "80", 
        height = "85",
        align = "right"
      ),
      windowTitle="spex"
    ),
    br(),
    #titlePanel("Dataset title"),
    tabsetPanel(
      id = "main_panels",
      tabPanel(
        "info",
        br(),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              inputId = "choose_dataset",
              label = "Choose dataset",
              choices = c("to be populated", 2, 3),
            ),
            p("Explore your chosen dataset by using the tabs above."),
            p("Sample names and experimental conditions are shown in the metadata section."),
            p("The data tab shows the whole dataset, which can be downloaded if required."),
            p("A range of plots can be viewed and downloaded to explore different aspects of the dataset.")
          ),
          mainPanel(
            br(),
            titlePanel(h1("dataset name", align = "center")),
            br(),
            h5("Information about the dataset and publication link"),
            br(),br(),br(),
            br(),br(),br(),br(),br(),br(),br(),
            h6("For more information about work carried out at the Babraham Institute
                 visit the", a(href= "https://www.babraham.ac.uk/", "website")),
            br(),br(),br(),br()
          )
        )  
      ),  
      tabPanel(
        "metadata",
        br(),
        fluidRow(
          column(
            width = 6,
            wellPanel(
              h3("Dataset summary", align = "center", style="margin: 10px;"),
              textOutput("meta_info1"),
              textOutput("meta_info2"),
              h6("Number of categories in each variable:"),
              tableOutput("meta_info3"),
              checkboxInput("show_meta_summary", "show more information on variables"),
              conditionalPanel(
                condition = "input.show_meta_summary == 1",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      "selected_condition",
                      "select condition",
                      choices = names(metadata$meta_sum)
                    ),
                  ),
                  column(width = 6, tableOutput("meta_summary"))
                )
              ),
              checkboxInput("show_meta", "show all metadata"),
              conditionalPanel(
                condition = "input.show_meta == 1",
                DT::dataTableOutput("meta_table")
              )#,
              #actionButton("browser", "browser")
            )
          ),
          column(
            width = 6,
            wellPanel(
              h3("Sets of interest", align = "center", style="margin: 10px;"),
              textOutput("set_info1"),
              h6("Number in each set:"),
              tableOutput("set_info2"),
              checkboxInput("show_sets", "show items in set"),
              conditionalPanel(
                condition = "input.show_sets == 1",
                fluidRow(
                  column(
                    width = 6,
                    selectInput(
                      "selected_set",
                      "select set",
                      choices = names(of_interest)
                    )
                  ),
                  column(width = 6, tableOutput("set_summary"))
                )
              )
            )
          )  
        ),
        br(),
        br()
      ),
      tabPanel(
        "data",
        br(),
        wellPanel(DT::dataTableOutput("data_table"))
      ),
      tabPanel(
        "plot",
        br(),
        navlistPanel(
          "plot type",
          tabPanel("histogram", mod_histogramUI("hist", metadata$meta_sum)),
          tabPanel("scatterplot", mod_scatterplot_ui("scatter", metadata$meta_sum, of_interest)),
          tabPanel("heatmap", mod_heatmap_ui("heatmap", metadata$meta_sum)),
          tabPanel("violinplot", mod_violinplot_ui("violinplot", metadata$meta_sum)),
          widths = c(3,9)
        )
      ),
      tabPanel(
        "filter",
        br(),
        mod_name_filter_ui("name_filter", measure_names)
      )
    ),
    br(),
    fluidRow(
      column(
        width = 3,
        tags$img(src = "bioinformatics_logo_small_grey.png", width = "200", height = "71")
      ),
      column(
        width = 6,
        offset = 3,
        br(),
        br(),
        p("Any problems please email laura.biggins@babraham.ac.uk", style = "font-size:12px", align = "right")
      )  
    ),
    br()
  )
)


server <- function(input, output, session ) {
  
  data_values <- dataset
  
  measures_of_interest <- reactiveVal(of_interest)
  
  # Data tab - the main dataset
  output$data_table <- DT::renderDataTable(
    dt_setup(data_values, n_rows = 20, dom_opt = "ftlip", show_rownames = TRUE)
  )
  
  output$meta_info1 <- renderText({
    paste0("The dataset contains ", nrow(metadata$meta_summary[[sample_names]]), " samples.")
  })
  
  output$meta_info2 <- renderText({
    paste0("Variables are: ", 
           paste0(names(metadata$meta_summary)[!names(metadata$meta_summary) %in% metadata$sample_name], collapse = ", "),
           "."
    )
  })
  
  output$meta_info3 <- renderTable({
    tibble::enframe(sapply(metadata$meta_summary, nrow))
  }, colnames = FALSE)
  
  output$set_info1 <- renderText({
    n_sets <- length(measures_of_interest())
    if(n_sets == 1) text <- " set available"
    else text <- " sets available"
    paste0(n_sets, text, ". To add more, use the filter tab.")
  })  
  
  output$set_info2 <- renderTable({
    tibble::enframe(sapply(measures_of_interest(), nrow))
  }, colnames = FALSE)
  
  output$set_summary <- renderTable({
    req(input$selected_set)
    req(measures_of_interest())
    measures_of_interest()[[input$selected_set]]
  })
  
  output$meta_table <- DT::renderDataTable(dt_setup(metadata$meta_all, n_rows = 20))
  
  output$meta_summary <- renderTable({
    req(input$selected_condition)
    req(metadata$meta_summary)
    metadata$meta_summary[[input$selected_condition]]
  })
  
  
  mod_histogramServer("hist", data_values, metadata$meta_all, sample_name_col = sample_names)
  
  mod_heatmap_server("heatmap", data_values, metadata$meta_summary, metadata$meta_all, 
                     sample_name_col = sample_names, of_interest = of_interest)
  
  mod_scatterplot_server(
    "scatter", 
    data_values, 
    metadata$meta_summary, 
    metadata$meta_all, 
    sample_name_col = sample_names, 
    sets_of_interest = measures_of_interest
  )
  
  mod_violinplot_server("violinplot", data_values, metadata$meta_summary, metadata$meta_all, 
                        sample_name_col = sample_names)
  
  filter_results <- mod_name_filter_server("name_filter", measure_names, of_interest)
  
  observeEvent(filter_results(), {
    
    measures_of_interest(filter_results())
    print("filter results updated")
    updateSelectInput(inputId = "selected_set", choices = names(measures_of_interest()))
  })
  
  observeEvent(input$browser, browser())
  
}


shinyApp(ui = ui, server = server)
