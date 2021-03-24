#' scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scatterplot_ui <- function(id, meta_sum, measures_of_interest){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      sidebarLayout(
        position = "right",
        sidebarPanel(
          style = "padding: 10px",
          width = 4,
          selectInput(
            inputId = ns("select_condition"),
            label = "select variable",
            choices = sort(names(meta_sum))
          ),
          selectInput(
            ns("x_axis"), 
            label = "x axis", 
            choices = ""
          ),
          selectInput(
            ns("y_axis"), 
            label = "y axis", 
            choices = "" 
          ),
          #actionButton(ns("browser"), "browser"),
          #br(),
          br(),
          downloadButton(ns("download_png"), "png"),
          downloadButton(ns("download_pdf"), "pdf")
        ),
        mainPanel(
          width = 8,
          shinycssloaders::withSpinner(
            plotOutput(ns("plot"), width = "100%", height = 500), 
            image = "bioinf1.gif", 
            image.width = 100, image.height = 40
          )
        )
      ),
      br(),
      checkboxInput(inputId = "highlight_panel", label = "show highlight options"),
      conditionalPanel(
        condition = "input.highlight_panel == 1",
        wellPanel(
          fluidRow(
            column(
              width = 6, 
              selectInput(
                ns("set_to_highlight"),
                "choose set",
                choices = names(measures_of_interest)
              )
            ),
            column(
              width = 6, 
              checkboxInput(ns("highlight_genes"), label = "highlight set"),
              checkboxInput(inputId = ns("label_highlights"), label = "show labels")
            )
          )
        )
      )  
    )
  )
}

#' scatterplot server function
#' 
#'
#' @noRd 
mod_scatterplot_server <- function(id, dataset, meta_sum, metadata, sample_name_col, sets_of_interest, prefix = "", session) {
  
  moduleServer(id, function(input, output, session) {
    
    # Made this a reactive so that it's not called on initialisation
    tibble_dataset <- reactive(get_tibble_dataset(dataset, sample_name_col))
    
    x_y_choices <- reactive(get_choices(input$select_condition, meta_sum)) %>% bindCache(input$select_condition)

    label_highlighted <- reactiveVal(FALSE)
    
    shinyjs::disable("label_highlights")
    
    rv <- reactiveValues()
    
    observeEvent(sets_of_interest(), {
      updateSelectInput(session, "set_to_highlight", choices = names(sets_of_interest()))
    })
    
    observeEvent(input$highlight_genes, {
      if(input$highlight_genes) {
        shinyjs::enable("label_highlights")
      } else {
        shinyjs::disable("label_highlights")
        updateCheckboxInput(session, "label_highlights", "show labels", value = FALSE)
      }  
    })
    
    observeEvent(input$label_highlights, label_highlighted(input$label_highlights))

    # There was an issue with the updating of the drop downs - there were some times 
    # when the variable type didn't match the x and y vars, because it hadn't had time to.
    # This seems to fix it.
    observe({
      if(input$x_axis %in% metadata[[input$select_condition]] &
        input$y_axis %in% metadata[[input$select_condition]]){
          rv$xvar <- input$x_axis
          rv$yvar <- input$y_axis
          rv$condition <- input$select_condition
      }
    })
    
    observeEvent(input$select_condition, {
      req(x_y_choices())
      req(length(x_y_choices()) >= 2)
      updateSelectInput(
        inputId = "x_axis",
        choices = x_y_choices(),
        session = session
      )
      updateSelectInput(
        inputId = "y_axis",
        choices = x_y_choices(),
        session = session,
        selected = x_y_choices()[2]
      )
    })

    selected_no_colour <- reactive({

      select_by_group(
        metadata,
        tibble_dataset(),
        condition = rv$condition,
        sample_name_col = sample_name_col,
        x_var = rv$xvar,
        y_var = rv$yvar
      )
    })
    
    selected_data <- reactive({
      req(selected_no_colour())
      if(input$highlight_genes & isTruthy(input$set_to_highlight)){
        genes <- get_set_to_highlight(sets_of_interest(), input$set_to_highlight)
        return(add_colours(selected_no_colour(), genes))
      } else {
        return(add_colours(selected_no_colour(), FALSE))
      }    
    })

    scatter_plot_object <- reactive({
      req(selected_data())
      scatter(selected_data(), input$x_axis, input$y_axis, label_highlighted())
    })
    
    output$plot <- renderPlot({
      req(scatter_plot_object())
      scatter_plot_object()      
    }) %>% bindCache(input$select_condition, input$x_axis, input$y_axis, input$set_to_highlight,
                     input$highlight_genes, label_highlighted())

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("scatter.png")
      },
      content = function(file) {
        ggplot2::ggsave(file, scatter_plot_object(), device = "png")
      }
    )
    
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("scatter.pdf")
      },
      content = function(file) {
        ggplot2::ggsave(file, scatter_plot_object(), device = "pdf")
      }
    )
    
    observeEvent(input$browser, browser())
  })
}

#' scatter plot function
#'
#' @param dataset dataset in tibble format that should contain columns with the 
#' same names as x_var and y_var
#' @param x_var variable to plot on the x axis
#' @param y_var variable to plot on the x axis
#'
#' @return ggplot object
#' @noRd
scatter <- function(dataset, x_var, y_var, label_subset) {

  req(x_var %in% colnames(dataset))
  req(y_var %in% colnames(dataset))
  
  dataset <- dplyr::arrange(dataset, custom_colour)
  
  p <- dataset %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])
    ) +
    ggplot2::geom_point(colour = dataset[["custom_colour"]]) +
    ggplot2::geom_abline(slope = 1, colour = "#3cc1f2") +
    ggplot2::theme(legend.position = "none") 
  
  if(label_subset){
    p <- p + ggplot2::geom_text(
      data = subset(dataset, custom_colour == "red"),
      ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], label = row_attribute),
      nudge_x = 1
    )
  }
  p
}

#' get_choices
#' 
#' Check which categories are available for a chosen condition.
#'
#' @param selected_condition 
#' @param meta_sum 
#'
#' @return
#' @export
#'
#' @noRd
get_choices <- function(selected_condition, meta_sum){
  
  assertthat::see_if(
    assertthat::has_name(
      meta_sum[[selected_condition]],
      selected_condition
    ),  
    msg = paste0("couldn't find the column ", selected_condition, 
                 " in meta_sum[[input$`scatter-select_condition`]]")
  )
  opts <- meta_sum[[selected_condition]][[selected_condition]]
  assertthat::see_if(
    length(opts) >= 2, 
    msg = paste0("number of factors in selected option for scatterplot is only ", 
                 length(opts), 
                 " so will not work well in a scatterplot."
    )
  )  
  opts
}

#' Get the set of names to highlight on the plot 
#'
#' Extracts a vector of names from a list, the list item can have multiple columns
#' e.g. separate classes within a set of genes, but only the first column will be
#' returned and any information in other columns will be ignored.
#'
#' @param sets list object containing the sets of interest
#' @param selected_set the selected set
#'
#' @return vector of names (genes/proteins etc)
#' @export
#'
#' @examples
get_set_to_highlight <- function(sets, selected_set){
  # check it's not null
  sets[[selected_set]][[1]]
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
#' select by group or sample
#' 
#' If samples are selected by group, so that there are multiple samples per group, 
#' the mean value for each measure within the group is calculated
#'
#' @param metadata the metadata tibble
#' @param tibble_dataset the main dataset in tibble format
#' @param condition the condition that we want to pull the groups from
#' @param sample_name_col the name of the column that contains all the sample names
#' @param x_var group 1 
#' @param y_var group 2
#'
#' @return
#' @export
#'
#' @examples
select_by_group <- function(metadata, tibble_dataset, condition, sample_name_col, x_var, y_var){
  
  selected_samples <- metadata %>%
    dplyr::filter(.data[[condition]] %in% c(x_var, y_var)) %>%
    dplyr::select(c(condition, sample_name_col))  
  
  n_samples <- dplyr::count(selected_samples)
  
  selected_data <- dplyr::inner_join(selected_samples, tibble_dataset)
  
  if(n_samples < 2 | length(unique(selected_data[[sample_name_col]])) < 2) {
    print("only found 1 selected variable to plot on scatter")
    print(paste0("n_samples = ", n_samples, "selected_data[[sample_name_col]] = ", selected_data[[sample_name_col]]))
    #browser()
  }
  # whether to group and summarise
  if(n_samples > 2) {
    selected_data <- selected_data %>%
      dplyr::group_by(.data[[condition]], row_attribute) %>%
      dplyr::summarise(mean_val = mean(value)) %>%
      dplyr::ungroup()
    
    return(tidyr::pivot_wider(selected_data, names_from = condition, values_from = mean_val))
  }
  # returned if n_samples == 2
  tidyr::pivot_wider(selected_data, names_from = condition, values_from = value)
}

#' Add colour column to tibble
#' 
#' This is used for the scatterplot point colours
#'
#' @param dataset tibble containing values and names
#' @param names_to_highlight vector of names that should be in the dataset
#'
#' @return tibble with an additional column containing colours
#' @export
#'
#' @examples
add_colours <- function(data_no_colour, names_to_highlight){
  
  if(isTruthy(names_to_highlight)){
    data_colour <- dplyr::mutate(
      data_no_colour,
      custom_colour = dplyr::if_else(
        row_attribute %in% names_to_highlight,
        "red",
        "grey"
      )
    ) 
  } else {
    data_colour <- dplyr::mutate(data_no_colour, custom_colour = "black")
  }
  data_colour
}

