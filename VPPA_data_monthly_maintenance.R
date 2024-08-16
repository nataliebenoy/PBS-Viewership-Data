# Monthly maintenance for VPPA data, for use in the program recommendation engine

## This script will be used to:
### Pull in the latest master VPPA dataset each month,
### Determine which shows are still available to stream (using an absurd amount of MM API calls)
### Update & export the sparse matrix that powers the program recommendation app,
### Update & export the program titles and slugs dataset for each unique program title, for use in the program recommendation app

# import necessary packages
library(tidyverse)
library(httr)
library(jsonlite)
library(listviewer)
library(lava)


# Section 1: Import the latest cleaned master VPPA dataset

# import most recent master VPPA dataset going back to the dawn of time
data <- read_csv("streaming_09_2016_08_2024.csv", show_col_types = FALSE)

# About this dataset
#   column names & data types:
#     TP.Media.ID (num), First.Name (chr), Last.Name (chr), Email (chr), UID (chr), Membership.ID (chr), Title (chr),
#     Content.Channel (chr), Device (chr), Date.Watched (date), Time.Watched (num), Total.Run.Time.of.the.video (num),
#     CID (chr), Genre (chr), Percent.Watched (num)








# Section 2: Determine which shows are still available for streaming, using the MM API


# define a function to retrieve the show title from the MM API response object
# regardless of whether the show has seasons or not, return NA if it fails
get_title <- function(response) {
  if (!is.null(response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["season"]][["attributes"]])) {
    title <- response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["season"]][["attributes"]][["show"]][["attributes"]][["title"]]
  } else if (!is.null(response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["show"]][["attributes"]])) {
    title <- response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["show"]][["attributes"]][["title"]]
  } else if (!is.null(response[["data"]][["attributes"]][["parent_tree"]][["attributes"]])) {
    title <- response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["title"]]
  } else {
    title <- NA
  }
  
  return(title)
}

# define a function to retrieve the show slug regardless of whether the show has seasons or not, return NA if it fails
get_slug <- function(response) {
  if (!is.null(response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["season"]][["attributes"]])) {
    slug <- response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["season"]][["attributes"]][["show"]][["attributes"]][["slug"]]
  } else if (!is.null(response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["show"]][["attributes"]])) {
    slug <- response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["show"]][["attributes"]][["slug"]]
  } else if (!is.null(response[["data"]][["attributes"]][["parent_tree"]][["attributes"]])) {
    slug <- response[["data"]][["attributes"]][["parent_tree"]][["attributes"]][["slug"]]
  } else {
    slug <- NA
  }
  
  return(slug)
}

# Custom function to retrieve the Passport start and end dates for each asset in a list of asset TP Media IDs
# also the show title, asset type (full-length, preview, clip) and show slug
## includes both assets available to all Passport members, and assets available to only your station's Passport members
getAvailability <- function(assetTP.Media.IDs) {
  # Initialize an empty data frame to store the results
  result_df <- data.frame(TP.Media.ID = character(0),
                          Title = character(0),
                          Slug = character(0),
                          AssetType = character(0),
                          StartDate_allMembers = character(0),
                          EndDate_allMembers = character(0),
                          StartDate_stationMembers = character(0),
                          EndDate_stationMembers = character(0),
                          stringsAsFactors = FALSE)
  
  # Base URL of the API endpoint
  base_url <- "https://media.services.pbs.org/api/v1/assets/legacy/?tp_media_id="
  
  for (id in assetTP.Media.IDs) {
    # Construct the API URL for the current CID
    api_url <- paste0(base_url, id)
    
    # Perform the GET request
    response <- GET(url = api_url, config = authenticate("BpDPaXjOLKWzJ5HE", "qM8ADDfWQaVs3tGoPT05JvIEcmiRLCPq"))
    
    # Make sure the status code is 200
    if (status_code(response) == 200) {
      # Parse the JSON response
      response_json <- content(response, "parsed")
      
      # Extract asset attributes
      show_info <- response_json$data$attributes
      
      # Extract data or replace with "NA" if missing dates
      asset_type <- show_info$object_type
      start_date_all_members <- ifelse(!is.null(show_info$availabilities$all_members$start), show_info$availabilities$all_members$start, "NA")
      end_date_all_members <- ifelse(!is.null(show_info$availabilities$all_members$end), show_info$availabilities$all_members$end, "NA")
      start_date_station_members <- ifelse(!is.null(show_info$availabilities$station_members$start), show_info$availabilities$station_members$start, "NA")
      end_date_station_members <- ifelse(!is.null(show_info$availabilities$station_members$end), show_info$availabilities$station_members$end, "NA")
      
      # Add the information to the result data frame
      result_df <- rbind(result_df, data.frame(TP.Media.ID = id,
                                               Title = get_title(response_json),
                                               Slug = get_slug(response_json),
                                               AssetType = asset_type,
                                               StartDate_allMembers = start_date_all_members,
                                               EndDate_allMembers = end_date_all_members,
                                               StartDate_stationMembers = start_date_station_members,
                                               EndDate_stationMembers = end_date_station_members,
                                               stringsAsFactors = FALSE))
    }
  }
  
  return(result_df)
}

# Globally set the user agent for the absurd amount of API calls we are about to make
set_config(add_headers(`User-Agent` = "A request from PBS Utah as a nonsanctioned, backdoor method of querying the PBS assets database. Contact nbenoy@pbsutah.org for any complaints about request volume"))

# Define a throttled version of the above function with a delay of 0.10s so as to not exceed the API rate limits
getAvailability_delayed <- slowly(getAvailability, rate = rate_delay(0.10))


# get list of all unique TP.Media.IDs
TP.Media.ID.list <- as.list(unique(data$TP.Media.ID))

# run the throttled function
availability.df <- getAvailability_delayed(TP.Media.ID.list)

# take a look
tail(availability.df, 30)

# change NA character values to properly coded NAs
availability.df <- availability.df %>%
  mutate(
    StartDate_allMembers = ifelse(StartDate_allMembers == "NA", NA, StartDate_allMembers),
    StartDate_stationMembers = ifelse(StartDate_stationMembers == "NA", NA, StartDate_stationMembers),
    EndDate_allMembers = ifelse(EndDate_allMembers == "NA", NA, EndDate_allMembers),
    EndDate_stationMembers = ifelse(EndDate_stationMembers == "NA", NA, EndDate_stationMembers)
  )

# change start and end dates to "date" data type, keeping NAs
availability.df <- availability.df %>%
  mutate(
    StartDate_allMembers = as.Date(StartDate_allMembers, optional = TRUE),
    EndDate_allMembers = as.Date(EndDate_allMembers, optional = TRUE),
    StartDate_stationMembers = as.Date(StartDate_stationMembers, optional = TRUE),
    EndDate_stationMembers = as.Date(EndDate_stationMembers, optional = TRUE)
  )

# filter to rows with EndDate = NA or > 31 days from now.
availability.df <- availability.df %>%
  filter((EndDate_allMembers > (Sys.Date() + 31) | is.na(EndDate_allMembers)) |
           (EndDate_stationMembers > (Sys.Date() + 31) | is.na(EndDate_stationMembers)))

# filter to full-length assets only that have a proper start date for station members (retains acquisitions like Doc Martin)
availability.df <- availability.df %>%
  filter(AssetType == "full_length" & !is.na(StartDate_stationMembers))

# get list of unique show titles
shows.list <- as.list(unique(availability.df$Title))

# take a look
shows.list[1:50]

# testing -- are APT shows properly included?
"Doc Martin" %in% shows.list
"America's Test Kitchen" %in% shows.list

# then use this list to filter the VPPA data prior to grouping & concatenating


### also need to create the show_slugs df from this list, and export as CSV, for use in the recommendation app
# get rid of any duplicate combinations of program title, slug
slugdf <- availability.df %>%
  distinct(Title, Slug, .keep_all = FALSE)

# take a look at rows that have > 1 slug per program title
slugdf %>%
  group_by(Title) %>%
  summarize(
    n = n()
  ) %>%
  arrange(desc(n)) # it's Professor T and Before We Die (English and foreign language versions)

slugdf %>%
  filter(Title %in% c("Professor T", "Before We Die"))

# we'll choose to keep the UK versions here
slugdf <- slugdf %>%
  filter((Title %ni% c("Professor T", "Before We Die")) |
           (Title %in% c("Professor T", "Before We Die") & str_ends(Slug, "-uk")))

# rename columns to play nice with our R Shiny app
slugdf <- slugdf %>%
  rename(
    program_title = Title,
    slug = Slug
  )

# export dataset of slugs and program titles
write.csv(slugdf, file = "VPPA_program_titles_and_show_slugs_08_2024.csv", row.names = FALSE)




# Section 3: Re-create the sparse matrix used to power the program recommendation app

# change date.watched to date data type
data <- data %>%
  mutate(
    Date.Watched = as.Date(Date.Watched)
  )

# filter to only streams of shows that are still available for at least the next month (list we created above)
recent_data <- data %>%
  filter(Content.Channel %in% shows.list)

# filter by rows where Total.Run.Time.of.the.video is either > 120 (seconds) to remove promos
# or = 0 (keeping the rows of bad data that PBS needs to fix)
# filter by at least 80% percent watched (arbitrary cutoff)
recent_data <- recent_data %>%
  filter((Percent.Watched >= 0.80) & (Total.Run.Time.of.the.video > 240 | Total.Run.Time.of.the.video == 0))

# remove all columns except Email and Content.Channel
recent_data <- recent_data[c("Email", "Content.Channel")]

# group by email, concatenate Content.Channel as strings separated by underscores
# (helps to avoid problems with program names that have commas in them, like "Love, Inevitably")
data_underscore_sep <- recent_data %>%
  select(Email, Content.Channel) %>% unique() %>%
  group_by(Email) %>%
  summarize(text = str_c(Content.Channel, collapse='_'))

# remove email column, keep only column of concatenated shows watched
data_underscore_sep <- data_underscore_sep[2]

# write to new file
write.csv(data_underscore_sep, "streaming_sparse_08_2024.csv", row.names = FALSE, quote = FALSE)




# Section 4: Create the Association Rules object ahead of time to cut down on processing time in-app

# load relevant libraries
library(arules)
library(opusminer)

# read transactions file back in (dumb solution)
newdata <- read_transactions("C:\\Users\\u6010825\\OneDrive - University of Utah\\Documents\\R_Projects\\streaming_sparse_08_2024.csv", format = "transactions", sep = "_")

# generate the association rules in advance! what a novel idea!
streamingrules <- apriori(newdata, parameter = list(support =
                                                   0.005, confidence = 0.20, minlen = 2, maxlen = 2))

# filter to only rules with lift >= 3.5 (somewhat arbitrary cutoff)
streamingrules <- subset(streamingrules, subset = lift >= 3.5)

# define custom function to filter rules to only those with >= i and <= j unique programs on the right-hand-side
filter_rules <- function(rules_obj, i, j) {
  lhs_values <- unique(as(lhs(sort(rules_obj, by = "lift")), "list"))
  filtered_rules <- rules_obj[0]  # Create empty rules object
  
  for (lhs_value in lhs_values) {
    rhs_values <- unique(as(rhs(rules_obj[lhs(rules_obj) %in% lhs_value]), "list"))
    if (length(rhs_values) >= i) {
      rules_to_add <- rules_obj[lhs(rules_obj) %in% lhs_value]
      rules_to_add <- sort(rules_to_add, by = "lift")[1:min(j, length(rules_to_add))]  # Limit to j rules
      
      filtered_rules <- c(filtered_rules, rules_to_add)
    }
  }
  
  return(filtered_rules)
}


filtered_rules <- filter_rules(streamingrules, 3, 6)

# write our rules object to a file
saveRDS(filtered_rules, "streamingrules.rds")

# alternative: write our rules object to a file using qsave (faster read/write capabilities)
qsave(filtered_rules, "streamingrules_qs.rds")
