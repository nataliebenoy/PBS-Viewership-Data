#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# import packages
library(shiny)
library(waiter)
library(Matrix)
library(arules)
library(httr)
library(jsonlite)
library(data.table)
library(qs)
library(purrr)
library(glue)

# read in arules object
streamingrules_filtered <- qs::qread("data/streamingrules_qs.rds")

# create a vector of all unique program titles on the left-hand side (to be used for program selection box)
list_of_lhs_programs <- as(as(unique(lhs(streamingrules_filtered)), "list"), "character")

# create a vector of all unique programs on the right-hand side (to be used to find the correct show slug)
list_of_rhs_programs <- as(as(unique(rhs(streamingrules_filtered)), "list"), "character")


# import a dataframe containing program titles and their show slugs
program_title_and_slug_df <- fread("data/VPPA_program_titles_and_show_slugs_08_2024.csv",
                                      header = TRUE,
                                      col.names = c("program_title", "slug"),
                                      colClasses = "character",
                                      nrows = 1500)
                                     # comment.char = "")

# puts the vector of left-hand side programs in alphabetical order
list_of_lhs_programs <- sort(list_of_lhs_programs, decreasing = FALSE)


# custom "get_slugs" function that retrieves the show slug for a given program title,
# replaces any unmatched title-slug combos with Bob Ross :)
get_slugs <- function(my_list, program_title_and_slug_df) {
  # Create a named vector for faster lookup
  slug_lookup <- setNames(program_title_and_slug_df$slug, program_title_and_slug_df$program_title)
  
  # Get slugs for each program title in the list, replace NAs with "best-joy-painting"
  slug_list <- slug_lookup[my_list]
  slug_list[is.na(slug_list)] <- "best-joy-painting"
  
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

### test - theoretically faster implementation of the above function ### has some issues, so we'll keep both versions of this function
get_image_url_2 <- function(response) {
  # Extract the images list from the response
  images <- response[["data"]][[1]][["attributes"]][["images"]]
  
  # Find the mezzanine image URL
  mezzanine_image <- sapply(images, function(image) {
    if (image[["profile"]] == "show-mezzanine16x9") {
      return(image[["image"]])
    }
    NULL
  })
  
  # Remove NULL values
  mezzanine_image <- mezzanine_image[!sapply(mezzanine_image, is.null)]
  if (length(mezzanine_image) > 0) {
    return(mezzanine_image[1])
  }
  
  # Find the background image URL
  background_image <- sapply(images, function(image) {
    if (image[["profile"]] == "background") {
      return(image[["image"]])
    }
    NULL
  })
  
  # Remove NULL values
  background_image <- background_image[!sapply(background_image, is.null)]
  if (length(background_image) > 0) {
    return(background_image[1])
  }
  
  # If no image is found, return Bob Ross
  return("Bob-Ross-happy-accidents.jpg")
}


# define a function that takes the parsed Media Manager API response object as an argument
# and returns the show long description
# if no long description is found, returns Bob Ross. :)
get_description <- function(response) {
  default_message <- "Oops! We couldn't find the program you were looking for, as it may no longer be available to stream. May we suggest The Joy of Painting with Bob Ross instead?"
  
  # Check if response contains data and if the long description is present
  if (length(response[["data"]]) > 0 && !is.null(response[["data"]][[1]][["attributes"]][["description_long"]])) {
    return(response[["data"]][[1]][["attributes"]][["description_long"]])
  } else {
    return(default_message)
  }
}

# custom function to output recommendations 2+ as one big HTML output
# aka with a show image, hyperlink & description for each show in the list of recommended shows
get_program_info <- function(program_titles) {
  if (length(program_titles) < 2) {
    return("<p>There are no more recommended shows for the program you've chosen. Please make another selection.</p>")
  } else {
    titles_to_process <- program_titles[2:min(length(program_titles), 6)]
    
    get_program_details <- function(title) {
      slug <- get_slugs(title, program_title_and_slug_df)
      api_url <- glue("https://media.services.pbs.org/api/v1/shows/?slug={slug}")
      response <- GET(api_url, authenticate("username", "password"))
      parsed_response <- content(response, as = "parsed")
      
      image_url <- get_image_url_2(parsed_response)
      show_url <- glue("https://video.pbsutah.org/show/{slug}/?utm_source=additional_recs&utm_campaign=recommendation_app&utm_medium=referral")
      description <- get_description(parsed_response)
      
      glue(
        '<h3><strong>{title}</strong></h3>',
        '<a href="{show_url}" target="_blank"><img src="{image_url}" width="672" height="378"></a>',
        "<br></br><p>{description}</p><br></br>"
      )
    }
    
    output <- map_chr(titles_to_process, get_program_details)
    return(paste(output, collapse = ""))
  }
}

# Globally set the user agent for all future API calls
set_config(add_headers(`User-Agent` = "A request from a PBS program recommendation app. Contact nbenoy@pbsutah.org for questions and/or complaints about the number of API calls :) "))




# Define the UI
ui <- fluidPage(
  
  # loading animation
  # waiter
  useWaiter(), 
  waiterShowOnLoad(html = spin_ring(),
                   color = "#bcbcbc"),
  
  # Grid layout 
  fluidRow(
    column(12,
           tags$style(
           HTML(".header {max-width: 100%;
                          height: auto;}
                          .mobile_header {max-width: 100%;
                          height: auto;
                          display: none;}
                          
                          .waiter-overlay-content{
                            position: fixed;
                            top: 250px; /*250 pixels from the top*/
                            left: 50%;
                          }
                              
                          @media(max-width: 500px){
                          .header {
                            display: none;}
                          .mobile_header{
                            display: block;}"
                
                # can get rid of this if CSS is wrong
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
                 htmlOutput("more_rules_html"),
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
  output$program_name <- bindCache(renderText({
    paste("If you liked ", input$program, ", you should watch...", sep = "")
  }), input$program)
  
  # subset rules based on the user's chosen program title
  rules_subset <- bindCache(reactive({
    subset(streamingrules_filtered, subset = lhs %oin% input$program)
  }), input$program)
 
  # turn right-hand-side rules into a list
  rhs_list <- bindCache(reactive({
    as(as(rhs(sort(rules_subset(), by = "lift")), "list"), "character")
  }), rules_subset())
  
  # sort the first rule by lift, in descending order, and format the result as text
  first_rule_string <- bindCache(reactive({
    rhs_list()[1]
  }), rhs_list())
  
  # outputs the first recommendation as text with an exclamation point (!)
  output$first_rule <- bindCache(renderText({
    paste(first_rule_string(), "!", sep = "")
  }), first_rule_string())
  
  # matches the first rule string with its show slug from the dataframe we imported at the beginning
  first_rule_string_slug <- bindCache(reactive({
    program_title_and_slug_df[program_title_and_slug_df$program_title == first_rule_string(), "slug"]
  }), first_rule_string())
  
  # build API url, perform GET request, parse response
  response_parsed <- bindCache(reactive({
    content(GET(url = paste("https://media.services.pbs.org/api/v1/shows/?slug=", first_rule_string_slug(), sep = ""),
                authenticate("username", "password")),
            as = "parsed")
  }), first_rule_string_slug())
  
  # creates the show url from the slug generated a while back (now with utm codes)
  show_url <- bindCache(reactive({
    paste("https://video.pbsutah.org/show/", first_rule_string_slug(), "/?utm_source=first_rec&utm_campaign=recommendation_app&utm_medium=referral", sep = "")
  }), first_rule_string_slug())
  
  # renders the show mezzanine image from the URL
  output$show_image <- bindCache(renderText({
    c('<a href="', show_url(), '" target="_blank"><img src="', get_image_url(response_parsed()), '" alt="', first_rule_string(), '" width = "672" height = "378"></a>')
  }), show_url(), response_parsed(), first_rule_string())
  
  # creates the hyperlink to the show on video.pbsutah.org (that is also a button)
  output$hyperlink_button <- bindCache(renderText({
    c('<strong><a href="', show_url(), '" target="_blank"><button class = "button" type = "submit">Watch Now</button></a></strong>')
  }), show_url())
  
  # pulls the show long description using the function defined earlier, creates a text output
  output$long_description <- bindCache(renderText({
    get_description(response_parsed())
  }), response_parsed())
  
  # output rules 2:6 as one big fat HTML output
  output$more_rules_html <- bindCache(renderText({
    get_program_info(rhs_list())
  }), rhs_list())
  
  # hide waiter
  waiter_hide()
  
}


# Run the application 
shinyApp(ui = ui, server = server)
