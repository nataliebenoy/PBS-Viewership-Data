# Monthly maintenance for VPPA data, for use in the program recommendation engine

## This script will be used to:
### Append new data to the master VPPA dataset each month,
### Update the master list for cross-referencing from the PBS assets database,
### Update & export the sparse matrix that powers the program recommendation app,
### Update & export the program titles and slugs dataset for each unique program title, for use in the program recommendation app

# import necessary packages
library(tidyverse)
library(httr)
library(jsonlite)
library(listviewer)
library(lava)


# Section 1: Import the latest month's cleaned master VPPA dataset

# import most recent master VPPA dataset going back to the dawn of time
data <- read_csv("your_VPPA_data_goes_here.csv")

# About this dataset
#   column names & data types:
#     TP.Media.ID (num), First.Name (chr), Last.Name (chr), Email (chr), UID (chr), Membership.ID (chr), Title (chr),
#     Content.Channel (chr), Device (chr), Date.Watched (date), Time.Watched (num), Total.Run.Time.of.the.video (num),
#     CID (chr), Genre (chr), Percent.Watched (num)


# TEST IN PROGRESS - filter VPPA dataset to only include program titles in the list of current (not expired)
# Passport titles from the PBS "current Passport library" document
currentshows <- readxl::read_excel("your_file_name_goes_here.xlsx")

# rename columns
currentshows <- currentshows %>%
  rename(
    Content.Channel = `SHOW TITLE`,
    Title = `ASSET TITLE`,
    Genre = GENRE,
    Start.Date = `START DATE`,
    End.Date = `END DATE`,
    TP.Media.ID = `TP MEDIA ID`,
    Geo.Availability = `GEO AVAILABILITY`
  )

# change start and end dates to "date" data type
currentshows <- currentshows %>%
  mutate(
    Start.Date = as.Date(Start.Date),
    End.Date = as.Date(End.Date)
  )

# filter to shows with end date > 2023-10-01 (or enter a date of your choice)
currentshows <- currentshows %>%
  filter(End.Date >= "2023-10-01")

# get list of show titles to filter VPPA data by
currentshows.list <- as.list(unique(currentshows$Content.Channel))









# Section 2: Update the master list of assets from the PBS assets database

# import master dataset from PBS assets database (current through 02/16/2023)
PBS_database <- read.csv("From_PBS_tp_media_id_all_assets.csv")

# use MM changelog endpoint to find all assets changed (created or updated, etc.) since 30 days ago (farthest back you can go)
changelog <- GET(url = "https://media.services.pbs.org/api/v1/changelog/?type=asset",
                 config = authenticate("username", "password"))

# check to see if it worked (status code 200)
status_code(changelog)

# parse response
changelog_parsed <- content(changelog, as = "parsed")

# custom function to retrieve the CIDs of all assets changed since [date] as a list, without duplicates
get_ids <- function(json_obj) {
  data_list <- json_obj$data
  id_list1 <- lapply(data_list, function(x) x$id)
  id_df <- data.frame(id = unlist(id_list1))
  id_list <- unique(id_df$id)
  return(id_list)
}

# get a list of unique CIDs for assets changed in the last 30 days
id_list <- get_ids(changelog_parsed)

# now that we have the asset CIDs, we can pull the legacy tp_media_id, video title, show title, and duration for each asset CID

# test to make sure at least 1 of the assets changed in the last 30 days is accessible / was not deleted (404 status code)
ID1 <- id_list[3]
assets_url <- paste0("https://media.services.pbs.org/api/v1/assets/", ID1)

assets <- GET(url = assets_url,
                 config = authenticate("username", "password"))

# check to see if it worked (status code 200) (code 403 for assets we don't have access to, 404 for deleted assets)
status_code(assets)

# parse response
assets_parsed <- content(assets, as = "parsed")

# look at content of the response
str(content(assets))

# take a closer look at the response object
listviewer::jsonedit(assets_parsed)


# define a function to retrieve the show title regardless of whether the show has seasons or not, return NA if it fails
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


# retrieve the TP Media ID, video title, show title, and duration for each unique CID
# in the list of CIDs of assets created or updated in the past 30 days
# function skips over any assets where the status code is not 200 (assets we don't have access to & deleted assets)
get_asset_data <- function(ids) {
  
  # Initialize data frame to store results
  asset_data <- data.frame(legacy_tp_media_id = numeric(),
                           cid = character(),
                           video_title = character(),
                           show_title = character(),
                           duration = integer(),
                           stringsAsFactors = FALSE)
  
  # Loop through each id and retrieve asset data
  for (id in ids) {
    
    # Construct API endpoint URL for this id
    url <- paste0("https://media.services.pbs.org/api/v1/assets/", id)
    
    # Perform GET request and parse JSON response
    response <- GET(url, config = authenticate("username", "password"))
    
    # Check status code of response
    if (status_code(response) != 200) {
      next # skip over this id if status_code is not 200
    }
    
    parsed_response <- content(response, as = "parsed")
    
    # Extract relevant data from parsed response
    legacy_tp_media_id <- parsed_response$data$attributes$legacy_tp_media_id
    cid <- id
    video_title <- parsed_response$data$attributes$title
    show_title <- get_title(parsed_response)
    duration <- parsed_response$data$attributes$duration
    
    # Add data to asset_data data frame
    asset_data <- rbind(asset_data, 
                        data.frame(legacy_tp_media_id = legacy_tp_media_id,
                                   cid = cid,
                                   video_title = video_title,
                                   show_title = show_title,
                                   duration = duration,
                                   stringsAsFactors = FALSE))
  }
  
  # Return final data frame
  return(asset_data)
}

# run the function on the list of CIDs
updated_assets <- get_asset_data(id_list)

# add updated assets to PBS_database
PBS_database <- rbind(PBS_database, updated_assets)








# Section 3: Re-create the sparse matrix that powers the program recommendation app

# change date.watched to date data type
data <- data %>%
  mutate(
    Date.Watched = as.Date(Date.Watched)
  )

# TEST IN PROGRESS - get list of KUED local shows (plus local acquisitions, like Doc Martin)
KUED.shows <- as.list(c("Art Elevated: The Governor's Mansion Artist Awards", "Call of the Canyon: Zion National Park",
                "Contact", "Contact in the Community", "Downwinders and the Radioactive West", "Free Speech Messages",
                "Governor's Monthly News Conference", "KUED Series", "KUED Specials", "Let's Talk", "Local Productions",
                "Modern Gardener", "National Parks - Beyond the Crowds", "National Parks - Troubled Edens", "PBS Utah Presents",
                "PBS Utah Town Hall", "RadioWest Films on PBS Utah", "Roots, Race & Culture", "The Gerda That Remains",
                "The Governor's Mansion", "The Hinckley Report", "The Utah Bucket List", "This Is Utah",
                "Tuacahn - Miracle in Padre Canyon", "Utah Conversations", "Utah Conversations with Ted Capener",
                "Utah Culture", "Utah Gymnastics: Red Rocks Retrospective", "Utah History", "Utah Insight", "Utah Issues",
                "Utah Movie Palaces", "Utah Now", "Utah Places", "Utah Vietnam War Stories", "VERVE",
                "We Shall Remain: A Native History of Utah", "Doc Martin"))

# filter to recent streams & shows in the list of current shows (from PBS Passport library, see above) or KUED shows (testing)
recent_data <- data %>%
  filter(Date.Watched >= "2020-01-01"
          & (Content.Channel %in% currentshows.list | Content.Channel %in% KUED.shows))



# filter by rows where Total.Run.Time.of.the.video is either > 60 (seconds) to remove promos
# or = 0 (keeping the rows of bad data that PBS needs to fix)
# filter by at least 40% percent watched (arbitrary cutoff)
recent_data <- recent_data %>%
  filter((Percent.Watched >= 0.40) & (Total.Run.Time.of.the.video > 60 | Total.Run.Time.of.the.video == 0))

# remove "Remembering Queen Elizabeth" because it's extremely problematic (aka no longer available), also "Masterpiece"
recent_data <- recent_data %>%
  filter(str_starts(Content.Channel, "Remembering Queen Elizabeth", negate = TRUE) & Content.Channel != "Masterpiece")

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
write.csv(data_underscore_sep, "your_sparse_data.csv", row.names = FALSE, quote = FALSE)








# Section 4: Update & export the program titles and slugs for each program title, for the program recommendation app

# Start by getting the Content.Channel and asset CID for the most recent observation of each unique Content.Channel

# sanity check for later: how many unique values of Content.Channel are in this dataset?
unique(data$Content.Channel)

# selects the Content.Channel and cid for the most recent instance of each Content.Channel (could also use TP.Media.ID)
program_cids <- data %>%
  filter(!is.na(CID)) %>%
  select(Content.Channel, Date.Watched, CID) %>%
  group_by(Content.Channel) %>%
  arrange(desc(Date.Watched)) %>%
  slice(1)

# ungroup the grouped data frame
program_cids <- ungroup(program_cids)

# select only the Content.Channel and cid columns
program_cids <- program_cids %>%
  select(Content.Channel, CID)




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



# define a function that retrieves the show slugs for a given list of program titles & asset cids
get_slugs_for_program_titles <- function(list_of_program_titles, program_cids) {
  
  # define a list of matching cids for the input program titles
  matching_cids <- unlist(as(program_cids[c(program_cids$Content.Channel %in% list_of_program_titles), "CID"], "list"))
  
  # Initialize a data frame to store the results
  results <- data.frame(
    program_title = character(),
    slug = character(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over the matching cids and retrieve the corresponding slugs
  for (cid in matching_cids) {
    # Build the asset URL
    asset_url <- paste("https://media.services.pbs.org/api/v1/assets/", cid, sep = "")
    
    # Perform the GET request, parse response and retrieve the slug
    response <- GET(url = asset_url, config = authenticate("username", "password"))
    response_parsed <- content(response, as = "parsed")
    slug <- get_slug(response_parsed)
    
    # Append the results to the data frame
    results <- rbind(results, data.frame(program_title = program_cids$Content.Channel[program_cids$CID == cid],
                                         slug = slug, stringsAsFactors = FALSE))
  }
  
  # Return the results
  return(results)
}


# Define a throttled version of the above function with a delay of 0.2s
get_slugs_for_program_titles_delayed <- slowly(get_slugs_for_program_titles, 
                            rate = rate_delay(0.2))


# use throttled function to generate the program slugs for all of the unique program titles in the master VPPA dataset
slugs <- get_slugs_for_program_titles_delayed(program_cids[["Content.Channel"]], program_cids)

# check out slugs dataframe
head(slugs, 30)

# see how many slugs are not NA
sum(!is.na(slugs$slug))


# Now we'll attempt to fix the missing slugs using the program titles, and applying slug logic
# some show slugs don't properly follow PBS's own logic (looking at you, Secrets of the Dead)

# make show title lowercase to prevent future problems, in a new column
slugs$slug2 <- tolower(slugs$program_title)

# try to fix the slugs according to slug logic using regex
slugs$slug2 <- slugs$slug2 %>%
  gsub("&", " ", .) %>%
  gsub("-", " ", .) %>%
  gsub("[[:punct:]]", "", .) %>%
  gsub("\\bthe\\s", "", .) %>%
  gsub("\\bthis\\s", "", .) %>%
  gsub("\\ba\\s", " ", .) %>%
  gsub("\\bthat\\s", " ", .) %>%
  gsub("\\bof\\s", " ", .) %>%
  gsub("\\bin\\s", " ", .) %>%
  gsub("\\ban\\s", " ", .) %>%
  gsub("\\bon\\s", " ", .) %>%
  gsub("\\bby\\s", " ", .) %>%
  gsub("\\bwith\\s", " ", .) %>%
  gsub("\\bat\\s", " ", .) %>%
  gsub("\\bbut\\s", " ", .) %>%
  gsub("\\bto\\s", " ", .) %>%
  gsub("\\bfrom\\s", " ", .) %>%
  gsub("\\bfor\\s", " ", .) %>%
  gsub(" +", "-", .) %>%
  gsub("^-", "", .) %>%
  gsub("-$", "", .)

# take a look at new column
head(slugs$slug2, 40)

# take a look at random sample of new column to see if slug rules worked correctly
set.seed(221)
sample_n(slugs, 20) %>%
  select(program_title, slug2)



# function to test if a MM API response object contains any data under response$data
check_show_slugs <- function(show_slugs) {
  base_url <- "https://media.services.pbs.org/api/v1/shows/?slug="
  
  # Map over show slugs and send GET requests
  responses <- map(show_slugs, function(slug) {
    url <- paste(base_url, slug, sep="")
    response <- GET(url, config = authenticate("username", "password"))
    return(response)
  })
  
  # Check if response[["data"]][[1]] exists and return TRUE or FALSE
  results <- map_lgl(responses, function(response) {
    json <- content(response, as="parsed")
    return(length(json[["data"]]) > 0)
  })
  
  # Combine slugs and results into a data frame
  df <- data.frame(slug = show_slugs, response = results, stringsAsFactors = FALSE)
  return(df)
}

# Define a throttled version of the above function with a delay of 0.2s
check_show_slugs_delayed <- slowly(check_show_slugs, rate = rate_delay(0.2))

# test it
test3 <- check_show_slugs_delayed(slugs$slug2)

head(test3, 10)
mean(test3$response)


# merge test3 with slugs on slug
all_slugs <- merge(slugs, test3, by.x = "slug2", by.y = "slug")

head(all_slugs)

# check how many slugs are not NA
sum(!is.na(all_slugs$slug))

# fix slugs that were previously NA but where response == TRUE
# meaning: the particular episode of a given show we searched for earlier has expired, but the show itself is still available
all_slugs <- all_slugs %>%
  mutate(
    slug = ifelse(is.na(slug) & response == TRUE, slug2, slug)
  )

# select only the program titles and slugs where slug != NA
all_slugs <- all_slugs %>%
  filter(!is.na(slug)) %>%
  select(program_title, slug)

# export dataset of slugs and program titles
write.csv(all_slugs, file = "VPPA_program_titles_and_show_slugs.csv", row.names = FALSE)



# recurring problem -- the Media Manager API only lets you pull shows that YOUR STATION HAS ACCESS TO!!!!
# sanity check -- are there any rows where slug is not NA but response == FALSE?
all_slugs %>%
  filter(is.na(slug) & response == TRUE)
