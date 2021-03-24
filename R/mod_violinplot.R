#' violinplot  UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_violinplot_ui <- function(id, meta_sum){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 4,
          selectInput(
            inputId = ns("select_condition"),
            label = "select variable",
            choices = sort(names(meta_sum)),
          ),
          br(),
          checkboxInput(ns("add_boxplot"), "add boxplot"),
          br(),
          downloadButton(ns("download_png"), "png"),
          downloadButton(ns("download_pdf"), "pdf")#,
         # actionButton(ns("browser"), "browser")
        ),
        mainPanel(
          width = 8,
          shinycssloaders::withSpinner(
            plotOutput(ns("plot"), width = "100%", height = 500), 
            image = "bioinf1.gif", 
            image.width = 100
          )
        )
      )  
    ),
    tags$script(
      "var myWidth = 0;
      $(document).on('shiny:connected', function(event) {
        myWidth = $(window).width();
        Shiny.onInputChange('violinplot-shiny_width', myWidth);
      });
      $(window).resize(function(event) {
         myWidth = $(window).width();
         Shiny.onInputChange('violinplot-shiny_width', myWidth);
      });"
    )
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
mod_violinplot_server <- function(id, dataset, meta_sum, metadata, sample_name_col, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {
    
    tibble_dataset <- reactive(get_tibble_dataset(dataset, sample_name_col))
    
    selected_data <- reactive({
      data_with_group_info(metadata, tibble_dataset(), input$select_condition, sample_name_col)
    })
    
    violin_obj <- reactive({
      violinplot(selected_data(), input$select_condition, boxplot = input$add_boxplot)
    })
    
    output$plot <- renderPlot({
      #violinplot(selected_data(), input$select_condition, boxplot = input$add_boxplot)
      violin_obj()
    }) %>% bindCache(input$select_condition, input$add_boxplot)
    
    output$download_png <- downloadHandler(
      filename = function() {
        paste0("violin.png")
      },
      content = function(file) {
        ggplot2::ggsave(
          file, 
          violin_obj(), 
          device = "png", 
          width = input$shiny_width*0.75/4,
          units = "mm"
        )
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("violin.pdf")
      },
      content = function(file) {
        ggplot2::ggsave(
          file, 
          violin_obj(), 
          device = "pdf", 
          width = input$shiny_width*0.75/4,
          units = "mm"
        )
      }
    )
    
    
    observeEvent(input$browser, browser())
  })
}


#' violinplot
#'
#' @param selected_data 
#' @param condition 
#'
#' @return
violinplot <- function(dataset, condition, boxplot = FALSE) {

  p <- dataset %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[condition]], y = value)) +
    ggplot2::geom_violin(fill = "#70b5aa")
  
  if(boxplot){
    p <- p + ggplot2::geom_boxplot(width=0.1, fill = "#92d1c7")
  }
  p
}


#' get_tibble_dataset
#' 
#' Convert the main matrix dataset into a tibble
#'
#' @param matrix_data main dataset
#' @param sample_name_col column name that contains all the sample names (should
#' be set in the initial golem options, then passed through as a function argument
#' to the module)
#'
#' @return tibble
get_tibble_dataset <- function(matrix_data, sample_name_col) {

  tib_data <- tibble::as_tibble(matrix_data, rownames = "row_attribute")
  tidyr::pivot_longer(tib_data, !row_attribute, names_to = sample_name_col)
}

#' select_by_group
#' 
#' This function adds the relevant metadata to the main dataset.
#' 
#' It removes NAs and coerces the condition to a factor. This is done locally here 
#' as we need it for the violinplot but may not want to change the main metadata object.
#'
#' @param metadata the metadata tibble
#' @param tibble_dataset the main dataset in tibble format
#' @param condition the condition that we want to pull the groups from
#' @param sample_name_col the name of the column that contains all the sample names
#'
#' @return 
data_with_group_info <- function(metadata, tibble_dataset, condition, sample_name_col){
  
  metadata %>%
    dplyr::select(c(condition, sample_name_col)) %>%
    tidyr::drop_na(condition) %>%
    dplyr::mutate_at(condition, factor) %>%
    dplyr::inner_join(tibble_dataset) 
} 
# this could be simplified by just merging the tibbles and not bothering with the filtering,
# but if the dataset is massive, it might be good to filter it.
