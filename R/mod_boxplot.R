# This is temporary - it's not perfect boxplot code but I had this from the scatterplot
# code and didn't want to get rid of it all
#' boxplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_boxplot_ui <- function(id, individual_samples, meta_sum){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          width = 4,
          tabsetPanel(
            id = ns("plot_samples"),
            tabPanel(
              title = "condition",
              br(),
              selectInput(
                inputId = ns("select_condition"),
                label = "",
                choices = names(meta_sum)
              )
            )
          )
        ),
        mainPanel(
          width = 8,
          plotOutput(ns("plot"), width = "100%")#, height = "100%")
        )
      )  
    )
  )
}
    
#' scatterplot Server Function
#'
#' @noRd 
mod_boxplot_server <- function(id, dataset, meta_sum, metadata, sample_name_col, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {
    
    tibble_dataset <- reactive(get_tibble_dataset(dataset, sample_name_col))
    
    selected_data <- reactive({
      data_with_group_info(metadata, tibble_dataset(), input$select_condition, sample_name_col)
    })
    
    output$plot <- renderPlot(boxplot(selected_data(), input$select_condition))

    observeEvent(input$browser, browser())
  })
}


#' boxplot
#'
#' @param selected_data 
#' @param condition 
#'
#' @return
boxplot <- function(dataset, condition) {

  dataset %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[condition]], y = value)) +
    ggplot2::geom_boxplot()
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
#' as we need it for the boxplot but may not want to change the main metadata object.
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
