#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above (if you are running this script in the RStudio IDE)
# (and if this file has been named "app.R")
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# import packages
library(shiny)
library(Matrix)
library(arules)
library(httr)
library(jsonlite)
library(opusminer)

# import data, where each row is a concatenated list of unique program titles watched by each unique user in a given time period
# with each program title separated by underscores ("_")
# Why underscores? Because this way we avoid problems with program titles that have commas in them
# like "Love, Inevitably" or "#MeToo, Now What?")
data <- read_transactions("streaming_transactions.csv", format = "transactions", sep = "_")

# generate the association rules
# note: we're not going to allow users to change this part. It would be too messy / confusing for most people.
# it could also easily crash the app if users were allowed to choose too low a level of support
streamingrules <- apriori(data, parameter = list(support =
                                                   0.010, confidence = 0.30, minlen = 2, maxlen = 2))

# create a list of all unique program titles on the left-hand side (to be used for program selection box)
list_of_lhs_programs <- as(unique(unlist(as(lhs(streamingrules), "list"))), "list")

# create a list of all unique programs on the right-hand side (to be used to find the correct show slug)
list_of_rhs_programs <- as(unique(unlist(as(rhs(streamingrules), "list"))), "list")


# import a dataframe containing program titles and their show slugs
program_title_and_slug_df <- read.csv("VPPA_program_titles_and_show_slugs.csv")

# puts the list of left-hand side programs in alphabetical order
list_of_lhs_programs <- list_of_lhs_programs[order(unlist(list_of_lhs_programs), decreasing = FALSE)]


# custom "get_slugs" function that retrieves the show slug for a given program title,
# replaces any unmatched title-slug combos with Bob Ross :)
get_slugs <- function(my_list, program_title_and_slug_df) {
  # Get slugs for each program title in the list
  slug_list <- program_title_and_slug_df$slug[match(my_list, program_title_and_slug_df$program_title)]
  
  # Check for unmatched program titles
  unmatched_titles <- setdiff(my_list, program_title_and_slug_df$program_title)
  
  if (length(unmatched_titles) > 0) {
    # Create a vector with "best-joy-painting" slug for unmatched titles
    unmatched_slugs <- rep("best-joy-painting", length(unmatched_titles))
    
    # Determine the correct positions in slug_list for unmatched titles
    unmatched_positions <- match(unmatched_titles, my_list)
    
    # Insert the unmatched slugs in the correct positions in slug_list
    slug_list <- append(slug_list, unmatched_slugs, unmatched_positions)
  }
  
  return(slug_list)
}




# define a function that takes a Media Manager API parsed response object as an argument
# and returns the mezzanine image URL (or background image URL if there is no mezzanine image) as a string
# this function works for MM API calls for SHOWS by slug or show id ONLY, and NOT asset searches
# if no image is found, returns Bob Ross :)
get_image_url <- function(response) {
  for (image in response[["data"]][[1]][["attributes"]][["images"]]) {
    if (image[["profile"]] == "show-mezzanine16x9") {
      selected_image <- image[["image"]]
      return(selected_image)
    } else if (image[["profile"]] == "background") {
      selected_image <- image[["image"]]
    }
  }
  if (exists("selected_image")) {
    return(selected_image)
  } else {
    return("Bob-Ross-happy-accidents.jpg")
  }
}

# define a function that takes the parsed Media Manager API response object as an argument
# and returns the show long description
# if no long description is found, returns Bob Ross. :)
get_description <- function(response) {
  if (length(response[["data"]]) > 0) {
    description <- response[["data"]][[1]][["attributes"]][["description_long"]]
    if (!is.null(description)) {
      return(description)
    } else {
      return("Oops! We couldn't find the program you were looking for, as it may no longer be available to stream. May we suggest The Joy of Painting with Bob Ross instead?")
    }
  } else {
    return("Oops! We couldn't find the program you were looking for, as it may no longer be available to stream. May we suggest The Joy of Painting with Bob Ross instead?")
  }
}

# custom function to output recommendations 2+ as one big HTML output
# aka with a show image, hyperlink & description for each show in the list of recommended shows
get_program_info <- function(program_titles) {
  if (length(program_titles) < 2) {
    return("<p>There are no more recommended shows for the program you've chosen. Please make another selection.</p>")
  } else {
    output <- ""
    for (title in program_titles[2:min(length(program_titles), 6)]) {
      slug <- get_slugs(title, program_title_and_slug_df)
      api_url <- paste0("https://media.services.pbs.org/api/v1/shows/?slug=", slug)
      response <- GET(api_url, authenticate("username", "password"))
      parsed_response <- content(response, as = "parsed")
      image_url <- get_image_url(parsed_response)
      show_url <- paste0("https://video.pbsutah.org/show/", slug, "/?utm_source=additional_recs&utm_campaign=recommendation_app&utm_medium=link")
      description <- get_description(parsed_response)
      
      html_output <- paste0(
        '<h3><strong>', title, '</strong></h3>',
        '<a href="', show_url, '" target="_blank"><img src="', image_url, '" width = "672" height = "378"></a>',
        "<br></br><p>", description, "</p><br></br>"
      )
      
      output <- paste(output, html_output, sep = "")
    }
    return(output)
  }
}


# Globally set the user agent for all future API calls
set_config(add_headers(`User-Agent` = "A request from a PBS program recommendation app."))





# Define the UI
ui <- fluidPage(
  
  
  # Grid layout 
  fluidRow(
    column(12,
           tags$style(
           HTML(".header {max-width: 100%;
                          height: auto;}
                          .mobile_header {max-width: 100%;
                          height: auto;
                          display: none;}
                              
                          @media(max-width: 500px){
                          .header {
                            display: none;}
                          .mobile_header{
                            display: block;}"        # can get rid of this if CSS is wrong
           )),
           HTML('<center><img src="what_to_watch_banner_wide.jpg", height = "135", width = "1920", class = "header"></center>'),
           HTML('<center><img src="what_to_watch_banner.png", height = "135", width = "638", class = "mobile_header"></center>'),
           br(),
           br(),
  fluidRow(
    column(3,
           
           # HTML text goes here. Follows normal HTML rules, but as an R function instead of using <></> brackets
           tags$style(HTML("h4.sidebar {text-align: left;}")),
           h4("I'm looking to watch something similar to...", align = "left", class="sidebar"),
           
           # Builds the selection box where users select the program for which they want recommendations
           selectInput(
             inputId = 'program',
             label = NULL,
             choices = list_of_lhs_programs,
             # selected = "All Creatures Great and Small",
             multiple = FALSE, # only one input allowed
             selectize = FALSE), # only use if using selectInput above (not selectizeInput)
           # options = list(created = FALSE)), # not allowed to create new inputs that do not exist in dataset
           helpText("Select a program you enjoy from the list of options above."),
           br(),
           span(class = "brsmall"),
    ),
    
    # This section tells the UI to output whatever the server is doing below (in grid format)
    column(9,
          tags$style(HTML(".button {border-radius: 12px;
                                  background-color: #2638c4;
                                  color: white;
                                  border: none;
                                  padding: 14px;}
                          img {max-width: 100%;
                              height: auto;}
                          h4 {max-width: 672px;
                              overflow-wrap: anywhere;}
                          p {max-width: 672px;
                            overflow-wrap: anywhere;}"             # can get rid of this if CSS is wrong
                            )),
           h4(textOutput("program_name"), align = "center"),
           span(class = "brsmall"),
           h2(strong(textOutput("first_rule"), align = "center")),
           htmlOutput("show_image"), align = "center",
           br(),
          div(div(textOutput("long_description"),
                  style = "max-width: 672px;",
                  style = "overflow-wrap: anywhere;",
                  style = "text-align: center;",
                  style = "display: inline-block; text-align:left;")),
           br(),
           h4(htmlOutput("hyperlink_button"), align = "center"),
           br(),
           br(),
          
          fluidRow(
          column(12,
                 tags$style(HTML("h4 {text-align: center;}
                            h3 {text-align: center;}
                            div.container {text-align: center;}
                            a {text-align: center;}"      # get rid of this if CSS is incorrect
                 )),
                 h4("You might also like:"),
                 span(class = "brsmall"),
                 uiOutput("more_rules_html"),
                 br(),
                 br())
          )
    )
    )
  )
)
)




# Define server logic that actually does the things
server <- function(input, output) {
  
  # this line adds a title above the rules output with the program title chosen by the user
  output$program_name <- renderText({
    paste("If you liked ", input$program, ", you should watch...", sep = "")
  })
  
  # subset rules based on the user's chosen program title, with lift > 1
  rules_subset <- reactive({
    subset(streamingrules, subset = lhs %oin% input$program & lift > 1.0)
  })
 
  # turn right-hand-side rules into a list
  rhs_list <- reactive({
    as(rhs(sort(rules_subset(), by = "lift")), "list")
  })
  
  # sort the first rule by lift, in descending order, and format the result as text
  first_rule_string <- reactive({
    toString(as(rhs(sort(rules_subset(), by = "lift")), "list")[1])
  })
  
  # outputs the first recommendation as text with an exclamation point (!)
  output$first_rule <- renderText({
    paste(first_rule_string(), "!", sep = "")
  })
  
  # matches the first rule string with its show slug from the dataframe we imported at the beginning
  first_rule_string_slug <- reactive({
    program_title_and_slug_df[program_title_and_slug_df$program_title == first_rule_string(), "slug"]
  })
  
  # builds the API url based on the user's first recommended program, turned into a slug
  API_url <- reactive({
    paste("https://media.services.pbs.org/api/v1/shows/?slug=", first_rule_string_slug(), sep = "")
  })
  
  # performs the GET request
  GET_response <- reactive({
    GET(url = API_url(), authenticate("username", "password"))
  })
  
  # parse response
  response_parsed <- reactive({
    content(GET_response(), as = "parsed")
  })
  
  # finds the show mezzanine (or background) image URL from the parsed JSON response, using the function defined earlier
  show_image_url <- reactive({
    get_image_url(response_parsed())
  })
  
  # creates the show url from the slug generated a while back (now with utm codes)
  show_url <- reactive({
    paste("https://video.pbsutah.org/show/", first_rule_string_slug(), "/?utm_source=first_rec&utm_campaign=recommendation_app&utm_medium=link", sep = "")
  })
  
  # renders the show mezzanine image from the URL
  output$show_image <- renderText({
    c('<a href="', show_url(), '" target="_blank"><img src="', show_image_url(), '" alt="', first_rule_string(), '" width = "672" height = "378"></a>')
  })
  
  # creates the hyperlink to the show on video.pbsutah.org (that is also a button)
  output$hyperlink_button <- renderText({
    c('<strong><a href="', show_url(), '" target="_blank"><button class = "button" type = "submit">Watch Now</button></a></strong>')
  })
  
  # pulls the show long description using the function defined earlier, creates a text output
  output$long_description <- renderText({
    get_description(response_parsed())
  })
  
  # output rules 2:6 as one big fat HTML output
  output$more_rules_html <- renderText({
    get_program_info(rhs_list())
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

