#' filter_panel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_name_filter_ui <- function(id, measure_names){
  
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 7,
        tabsetPanel(
          #widths = c(3,9),
          #well = FALSE,
          tabPanel(
            "free text",
            wellPanel(
              h5("1. Enter set of names to search for in the dataset"),
              # style = "padding: 10px",
              textInput(
                ns("pasted_names"),
                label = NULL,
                width = "500px",
                placeholder = "Enter names here e.g. gene1, gene2"
              ),
              fluidRow(
                column(
                  width = 7,
                  style = "border: 10px",
                  radioButtons(
                    ns("name_delimiter"),
                    label = "separator",
                    choices = c("space/tab" = "space", "comma" = "comma"),
                    inline = TRUE
                  )
                ),
                column(
                  width = 3,
                  actionButton(ns("search_names"), "search")
                )
              ),
              textOutput(ns("search_summary"))
            )
          ),
          tabPanel(
            "dropdown",
            wellPanel(
              h5("1. Search names in dataset"),
              shinyWidgets::pickerInput(
                inputId = ns("measure_selector"),
                label = NULL,
                choices = measure_names,
                multiple = TRUE,
                options = shinyWidgets::pickerOptions(
                  actionsBox = TRUE,
                  liveSearch = TRUE, 
                  selectedTextFormat = "count > 10"
                )
              ),
              actionButton(ns("confirm"), "Confirm selection"),
              textOutput(ns("dropdown_msg"))
            )
          )
        )
      ),
      column(
        width = 5,
        br(),
        wellPanel(
          h5("2. Add the new set to the sets of interest"),
          fluidRow(
            column(
              width = 6,
              textInput(
                ns("set_name"), 
                label = NULL,
                width = "400px", 
                placeholder = "name of set"
              )
            ),
            column(
              width = 6,
              actionButton(ns("add_names"), label = "Add")
            )
          ),
          textOutput(ns("add_set_msg")),
          p("To view set information, go to the metadata tab"),
          br(),
          br()#,
          #actionButton(ns("browser"), "browser")
        )
      )
    ),
    br(),
    br(),
    br(), 
    br(),
    br(),
    br(),
    br(),
    br()
  )
}


mod_name_filter_server <- function(id, measure_names, of_interest){
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns 
     
    shinyjs::disable("add_names")
    shinyjs::disable("set_name")
    
    observeEvent(input$browser, browser())
    
    sets_of_interest <- reactiveVal(of_interest)
    
    rv <- reactiveValues()
    
    set_msg <- reactiveVal()
    
    matched_names <- reactive({
      match_names(rv$entered_names, measure_names)
    })
    
    observeEvent(input$confirm, {
      rv$entered_names <- input$measure_selector
    })
    
    observeEvent(matched_names(), {
      
      if(isTruthy(matched_names())){
        shinyjs::enable("add_names")
        shinyjs::enable("set_name")
      } else {
        shinyjs::disable("add_names")
        shinyjs::disable("set_name")
      }
      
    })
    
    search_msg <- reactive({
      req(rv$entered_names)
      req(matched_names())
      paste0(
        length(rv$entered_names), 
        " names entered, ", 
        length(matched_names()),
        " of these were found in the dataset"
      )
    })
    
    output$dropdown_msg <- renderText({
      paste0(length(matched_names()), " names in set.")
    })
    
    output$search_summary <- renderText(search_msg())
    
    output$add_set_msg <- renderText(set_msg())

        
    observeEvent(input$search_names, {
    
      shinyFeedback::hideFeedback("pasted_names")
      shinyjs::disable("add_names")
      shinyjs::disable("set_name")
      
      rv$entered_names <- split_names(input$pasted_names, input$name_delimiter)
      
      # some guesses for what may be wrong
      any_names <- isTruthy(input$pasted_names)
      delim_invalid <- (!isTruthy(matched_names()) & length(rv$entered_names) == 1)
      no_matches <- (!isTruthy(matched_names())) & length(rv$entered_names) > 1
      
      if(!any_names){ 
        print("not any names")
        shinyFeedback::feedbackWarning("pasted_names", !any_names, "Please enter some names")
      } else if (delim_invalid) {
        shinyFeedback::feedbackWarning(
          "pasted_names",
          delim_invalid,
          "No names matched the dataset, do you need to change the delimiter option below?"
        )
      } else if (no_matches) {
        shinyFeedback::feedbackWarning(
          "pasted_names",
          no_matches,
          "No names matched the dataset, make sure the separator option is set correctly
          below. </br>
          If this doesn't work, try selecting from the available names in the dataset 
          by using the dropdown tab." 
        )
      } else if (isTruthy(search_msg())){
        shinyjs::enable("add_names")
        shinyjs::enable("set_name")
      }
    })

    observeEvent(input$add_names, {
      
      print("from add names")
      no_names <- !(isTruthy(matched_names()))
      
      shinyFeedback::feedbackWarning(
        "add_names",
        no_names,
        "Select some names before adding a set"
      )
      
      req(matched_names())
      sets <- sets_of_interest()
      
      if(input$set_name %in% names(sets)){
        showModal(modal_confirm)
      } else {
        this_set_name <- get_set_name(input$set_name, length(sets_of_interest()))
        new_sets <- add_set(sets_of_interest(), this_set_name,  matched_names())
        sets_of_interest(new_sets)
        set_msg(paste0("Added ", this_set_name, "."))
        
        #sets_of_interest(new_sets())
        #set_msg(paste0("Added ", this_set_name(), "."))
      }  
    })

    modal_confirm <- modalDialog(
      "You already have a set with this name. Are you sure you want to overwrite it?",
      title = "Overwriting set",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Overwrite", class = "btn btn-danger")
      )
    )
    
    observeEvent(input$ok, {
      showNotification("Overwriting set")
      removeModal()
      # I don't like this code duplication but I haven't got a nice way to encapsulate it.
      this_set_name <- get_set_name(input$set_name, length(sets_of_interest()))
      new_sets <- add_set(sets_of_interest(), this_set_name,  matched_names())
      sets_of_interest(new_sets)
      set_msg(paste0("Added ", this_set_name, "."))
    })
    
    observeEvent(input$cancel, {
      print("running cancel option")
      removeModal()
    })
    
    reactive(sets_of_interest())
  })
}
    
    
#' Title
#'
#' @param name_string 
#' @param delimiter one of "space" or "comma"
#'
#' @return
#' @export
#'
#' @examples
split_names <- function(name_string, delimiter){
  
  delim <- switch(delimiter,
    space = " ",
    comma = ","
  )
  stringr::str_split(name_string, pattern = delim)[[1]]
  #stringr::str_trim(name_string)
}    
    
#' Title
#'
#' @param selected_names names to filter on
#' @param all_names  all the rownames from the main dataset
#'
#' @return vector of names that matched
#' @export
#'
#' @examples
match_names <- function(selected_names, all_names){
  
  #convert to upper or lower case
  selected_names[toupper(selected_names) %in% toupper(all_names)]
}    
    

#' get_set_name
#' 
#' Checks whether the name is empty and if so, 
#'
#' @param set_name 
#' @param n_sets 
#'
#' @return
#' @export
#'
#' @examples
get_set_name <- function(set_name, n_sets){
  
  if(nchar(set_name) >= 1){
    return(set_name)
  } else {
    return(paste0("set_", n_sets + 1))
  }
}    
    
add_set <- function(sets, new_set_name = "another_set", new_set){
  print("new set name = ")
  print(new_set_name)
  sets[[new_set_name]] = tibble::tibble(!!new_set_name:=new_set)
  sets
}    

   