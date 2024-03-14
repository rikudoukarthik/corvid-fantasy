# eBird API functions ---------------------------------------------------------------

# admin units based on eBird 
get_admin_codes <- function(unit_code, hi_arch = TRUE) {
  
  if (!exists("all_units")) {
    
    list_countries <- ebirdsubregionlist("country", key = myebirdtoken)
    
    parent_code <- str_sub(unit_code, 1, 2)
    list_states <- ebirdsubregionlist("subnational1", parent_code, key = myebirdtoken)
    list_districts <- ebirdsubregionlist("subnational2", parent_code, key = myebirdtoken)
    
    all_units <- list_countries %>% 
      bind_rows(list_states) %>% 
      bind_rows(list_districts) 
    
    list("list_countries" = list_countries,
         "parent_code" = parent_code,
         "list_states" = list_states,
         "list_districts" = list_districts,
         "all_units" = all_units) %>% 
      list2env(envir = .GlobalEnv)
    
  }
  
  
  if (!unit_code %in% all_units$code) {
    return("Input admin unit code is not a valid code!")
  }
  
  # if country, we want only subnational1
  req_adm2 <- if (unit_code %in% list_countries$code) FALSE else TRUE
  
  if (!hi_arch) {
    
    return(unit_code)
    
  } else {
    
    req_units <- all_units %>% 
      filter(str_detect(code, unit_code)) %>% 
      {if (req_adm2) {
        .
      } else {
        anti_join(., list_districts, by = c("code", "name"))
      }} %>% 
      pull(code)
    
    return(req_units)
    
  }
  
}

get_admin_names <- function(region_input) {
  
  if (!exists("all_units")) {
    list_countries <- ebirdsubregionlist("country", key = myebirdtoken)
    
    parent_code <- str_sub(unit_code, 1, 2)
    list_states <- ebirdsubregionlist("subnational1", parent_code, key = myebirdtoken)
    list_districts <- ebirdsubregionlist("subnational2", parent_code, key = myebirdtoken)
    
    all_units <- list_countries %>% 
      bind_rows(list_states) %>% 
      bind_rows(list_districts) 
    
    list("list_countries" = list_countries,
         "parent_code" = parent_code,
         "list_states" = list_states,
         "list_districts" = list_districts,
         "all_units" = all_units) %>% 
      list2env(envir = .GlobalEnv)
  }
  
  region_names <- all_units %>% 
    filter(code %in% get_admin_codes(region_input, hi_arch = TRUE)) %>% 
    magrittr::set_colnames(c("REGION", "REGION.NAME"))
  
  return(region_names)
  
}



get_obs_count <- function(region, date = date_cur)
{
  date <- str_replace_all(date, "-", "/")
  h <- new_handle()
  address_API <- glue("https://api.ebird.org/v2/product/stats/{region}/{date}")
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(address_API, h)
  
  Sys.sleep(0.2)     
  return(req)
}

write_obs_tally <- function(region, date = date_cur)
{  
  req <- get_obs_count(region, date)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  else
  {
    print ("Observer count returned error")
  }
  
  regionStat <- cbind(region, parsed$numContributors, parsed$numChecklists, parsed$numSpecies)
  # print(nrow(regionStat))
  # print(regionStat)
  
  return(regionStat)
}


get_spec_list <- function(region, date = date_cur)
{
  date <- str_replace_all(date, "-", "/")
  h <- new_handle()
  address_API <- glue("https://api.ebird.org/v2/data/obs/{region}/historic/{date}")
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(address_API, h)
  
  Sys.sleep(0.2)     
  return(req)
}

write_spec_tally <- function(region, date = date_cur)
{  
  req <- get_spec_list(region, date)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  speciesList <- cbind(region, comName = parsed$comName)
  
  if(is.null(parsed$comName)) {
    speciesList <- cbind(speciesList, comName = NA_real_)
  }
  
  return(speciesList)
}


get_notable_spec <- function(region, back, maxResults = 5) {
  
  h <- new_handle()
  address_API <- glue("https://api.ebird.org/v2/data/obs/{region}/recent/notable")
  
  # myebirdtoken should be assigned in token.R
  handle_setheaders(h, "X-eBirdApiToken" = myebirdtoken)
  req <- curl_fetch_memory(address_API, h)
  
  Sys.sleep(0.2)     
  return(req)
  
}

write_notable_spec <- function(region) {  
  req <- get_notable_spec(region)
  
  if(req$status_code == 200)
  {
    response <- jsonlite::prettify(rawToChar(req$content))
    parsed <- jsonlite::fromJSON(response, flatten = FALSE)
  }
  speciesList <- cbind(region, parsed$comName, parsed$subId)
  
  if(is.null(parsed$comName)) {
    speciesList <- cbind(speciesList, comName = NA_real_, subId = NA_real_)
  }
  
  return(speciesList)
  
}


# adapted from PJ's code in ebird-datasets/BCI-metrics
get_media_summary <- function(date_start, date_end) {
  
  # load token for media
  source("token_media.R")
  
  h <- new_handle()
  
  region_info <- get_admin_names(cur_region) %>% filter(REGION == cur_region)
  
  address_url <- "https://media.ebird.org/api/v2/stats/media-count?"
  address_part1 <- glue("obsDtFrom={date_start}&obsDtTo={date_end}")
  address_part2 <- glue("&regionCode={region_info$REGION}")
  address_part3 <- "&unconfirmed=incl&birdOnly=true"
  address_API <- glue("{address_url}{address_part1}{address_part2}{address_part3}")
  
  handle_setheaders(h, "X-eBirdApiToken" = mediatoken)
  retries <- 0
  while (retries < 20) {
    req <- curl_fetch_memory(address_API, h)
    Sys.sleep(3)
    
    if (req$status_code == 200) {
      mediaSummary <- jsonlite::prettify(rawToChar(req$content)) %>% 
        fromJSON(flatten = FALSE) %>%
        as.data.frame() %>% 
        relocate(photo, audio, video)
      return(mediaSummary)
    } else {
      print("HTTP GET returned error. Retrying...")
      retries <- retries + 1
    }
    
  }
  
}

# higher-level API functions --------------------------------------------------------

# uses above functions 


# generate species list for given regions and dates

# # for subnat2
# get_admin_codes(input$region_code, hi_arch = TRUE) %>%
#   gen_spec_list(dates = dates_cur) %>%
#   filter(REGION != input$region_code) # if hi_arch == TRUE

gen_spec_list <- function(regions, dates) {
  
  parent_code <- str_sort(regions)[1]
  
  if (length(dates) == 1) {
    
    list_spec <- regions %>% 
      map(~ write_spec_tally(.x, dates)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "ENGLISH.NAME")) %>% 
      left_join(ebd_tax, by = "ENGLISH.NAME") %>% 
      arrange(REGION, SORT) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>% 
      dplyr::select(REGION, REGION.NAME, ENGLISH.NAME)
    
  } else if (length(dates) > 1) {
    
    regions_dates <- expand_grid(regions, dates) %>% 
      magrittr::set_colnames(c("REGION", "DATE")) %>% 
      arrange(DATE) %>% 
      group_by(DATE) %>% 
      mutate(DAY.NO = cur_group_id()) %>% 
      ungroup()
    
    list_spec <- map2(regions_dates$REGION, regions_dates$DATE, 
                      ~ write_spec_tally(.x, .y) %>% bind_cols(tibble(DATE = .y))) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "ENGLISH.NAME", "DATE")) %>% 
      left_join(regions_dates %>% distinct(DATE, DAY.NO), 
                by = "DATE") %>% 
      left_join(ebd_tax, by = "ENGLISH.NAME") %>% 
      arrange(DAY.NO, REGION, SORT) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>%
      dplyr::select(DAY.NO, REGION, REGION.NAME, ENGLISH.NAME) %>% # remove DATE for pivot
      mutate(PRESENT = 1) %>% 
      pivot_wider(names_from = "DAY.NO", names_glue = "DAY{DAY.NO}", values_from = "PRESENT") %>% 
      mutate(across(starts_with("DAY"), ~ replace_na(., replace = 0))) %>% 
      arrange(REGION, across(starts_with("DAY"), desc))
    
  }
  
  return(list_spec)
  
}


# generate participation summary for given regions and dates

# # for subnat2
# get_admin_codes(input$region_code, hi_arch = TRUE) %>%
#   gen_part_summ(dates = dates_cur) %>%
#   filter(REGION != input$region_code) # if hi_arch == TRUE

gen_part_summ <- function(regions, dates, list_spec = NULL) {
  
  parent_code <- str_sort(regions)[1]
  
  if (length(dates) == 1) {
    
    summary_part <- regions %>% 
      map(~ write_obs_tally(.x, dates)) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "OBSERVERS", "CHECKLISTS", "SPECIES")) %>% 
      mutate(across(c("OBSERVERS", "CHECKLISTS", "SPECIES"), ~ as.integer(.))) %>% 
      arrange(desc(OBSERVERS), desc(SPECIES)) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>% 
      relocate(REGION, REGION.NAME)
    
  } else if (length(dates) > 1) {
    
    regions_dates <- expand_grid(regions, dates) %>% 
      magrittr::set_colnames(c("REGION", "DATE")) %>% 
      arrange(DATE) %>% 
      group_by(DATE) %>% 
      mutate(DAY.NO = cur_group_id()) %>% 
      ungroup()
    
    summary_part <- map2(regions_dates$REGION, regions_dates$DATE, 
                         ~ write_obs_tally(.x, .y) %>% bind_cols(tibble(DATE = .y))) %>% 
      list_c() %>% 
      as.data.frame() %>% 
      magrittr::set_colnames(c("REGION", "OBSERVERS", "CHECKLISTS", "SPECIES", "DATE")) %>% 
      mutate(across(c("OBSERVERS", "CHECKLISTS", "SPECIES"), ~ as.integer(.))) %>% 
      left_join(regions_dates %>% distinct(DATE, DAY.NO), 
                by = "DATE") %>% 
      arrange(DAY.NO, desc(OBSERVERS), desc(SPECIES)) %>% 
      left_join(get_admin_names(parent_code), by = "REGION") %>%
      relocate(DATE, DAY.NO, REGION, REGION.NAME) %>% 
      pivot_longer(c(OBSERVERS, CHECKLISTS, SPECIES),
                   names_to = "TOTAL", values_to = "VALUE") %>% 
      dplyr::select(-DATE) %>% 
      pivot_wider(names_from = "DAY.NO", names_glue = "DAY{DAY.NO}", values_from = "VALUE") %>% 
      arrange(REGION)
    
    # adding all-day totals
    if (exists("list_spec") & !is.null(list_spec)) {
      
      # total species reported over all days
      tot_spec_alldays <- list_spec %>% 
        group_by(REGION) %>% 
        reframe(TOT.SPEC = n_distinct(ENGLISH.NAME))
      
      summary_part <- summary_part %>% 
        left_join(tot_spec_alldays %>% mutate(TOTAL = "SPECIES"), 
                  by = c("TOTAL", "REGION")) %>% 
        mutate(ALL.DAYS = case_when(
          TOTAL == "OBSERVERS" ~ "NA",
          TOTAL == "CHECKLISTS" ~ rowSums(across(starts_with("DAY"))) %>% as.character(),
          TOTAL == "SPECIES" ~ TOT.SPEC %>% as.character()
        ),
        TOT.SPEC = NULL)
      
    } else {
      
      warning(paste("Total species over all days can only be calculated from species lists.",
                    "Returning NA."))
      
      summary_part <- summary_part %>% 
        mutate(ALL.DAYS = case_when(
          TOTAL == "OBSERVERS" ~ "NA",
          TOTAL == "CHECKLISTS" ~ rowSums(across(starts_with("DAY"))) %>% as.character(),
          TOTAL == "SPECIES" ~ "NA"
        ))
      
    }
    
  }
  
  return(summary_part)
  
}

# other higher-level functions ------------------------------------------------------

gen_textual_summ <- function(species, observers = NULL, lists, event_code, event_day = NULL) {

  day_of <- if (is.null(event_day) | is.null(observers)) { 
    # even if day specified for multi-day, don't include
    "In "
  } else {
    glue("On Day {event_day} of ")
  }
  
  line1 <- "Congratulations to everyone! "
  
  line2a <- glue("{day_of}{str_replace(event_code, '_', ' ')}, ")
  line2b <- if (!is.null(observers)) glue("{observers} observers ") else glue("observers ")
  line2c <- glue("reported an list of {species} species ")
  line2d <- glue("from {lists} checklists.")
  
  summary_text <- glue("{line1}{line2a}{line2b}{line2c}{line2d}")
  
  return(summary_text)
  
}

gen_ebird_barchart <- function(data, obs_omit = FALSE) {
  
  require(patchwork)
  
  ebird_green <- "#36824B"
  
  ldb <- data %>% 
    {if (obs_omit == TRUE) {
      dplyr::select(., REGION.NAME, CHECKLISTS, SPECIES) %>% 
        arrange(desc(CHECKLISTS), desc(SPECIES))
    } else {
      dplyr::select(., REGION.NAME, OBSERVERS, CHECKLISTS, SPECIES) %>% 
        arrange(desc(OBSERVERS), desc(CHECKLISTS), desc(SPECIES))
    }} %>% 
    mutate(RANK = row_number(),
           REGION.NAME = fct_inorder(REGION.NAME)) %>% 
    {if (obs_omit == TRUE) {
      .
    } else {
      mutate(., OBS.NUDGE = 0.9*str_count(OBSERVERS))
    }} %>% 
    mutate(across(c(everything(), -starts_with("REGION")),
                  ~ as.integer(.))) %>% 
    mutate(CHECK.NUDGE = 0.9*str_count(CHECKLISTS),
           SPEC.NUDGE = 0.9*str_count(SPECIES))
  
  
  if (obs_omit == FALSE) {
    
    plot1 <- ldb %>% 
      ggplot(aes(y = fct_rev(REGION.NAME))) + 
      geom_col(aes(x = OBSERVERS), fill = ebird_green, colour = NA) + 
      geom_text(aes(x = OBSERVERS, label = OBSERVERS), colour = "black", hjust = -0.2) +
      scale_x_continuous(limits = c(0, 
                                    max(ldb$OBSERVERS) + ceiling(0.3*diff(range(ldb$OBSERVERS)))),
                         position = "top",
                         name = "eBirders") +
      theme_void() +
      theme(axis.text.y = element_text(hjust = 1, size = 9),
            axis.title.x.top = element_text(size = 14, hjust = 0.05,
                                            margin = margin(0, 0, 4, 0, "pt")))
    
  }
  
  plot2 <- ldb %>% 
    ggplot(aes(y = fct_rev(REGION.NAME))) + 
    geom_col(aes(x = CHECKLISTS), fill = ebird_green, colour = NA) + 
    geom_text(aes(x = CHECKLISTS, label = CHECKLISTS), colour = "black", hjust = -0.2) +
    scale_x_continuous(limits = c(0, 
                                  max(ldb$CHECKLISTS) + ceiling(0.3*diff(range(ldb$CHECKLISTS)))),
                       position = "top",
                       name = "Checklists") +
    theme_void() +
    {if (obs_omit == FALSE) {
      theme(axis.title.x.top = element_text(size = 14, hjust = 0.05,
                                            margin = margin(0, 0, 4, 0, "pt")))
    } else {
      theme(axis.text.y = element_text(hjust = 1, size = 9),
            axis.title.x.top = element_text(size = 14, hjust = 0.05,
                                            margin = margin(0, 0, 4, 0, "pt")))
    }}

  plot3 <- ldb %>% 
    mutate() %>% 
    ggplot(aes(y = fct_rev(REGION.NAME))) + 
    geom_col(aes(x = SPECIES), fill = ebird_green, colour = NA) + 
    geom_text(aes(x = SPECIES, label = SPECIES), colour = "black", hjust = -0.2) +
    scale_x_continuous(limits = c(0, 
                                max(ldb$SPECIES) + ceiling(0.3*diff(range(ldb$SPECIES)))),
                       position = "top",
                       name = "Species") +
    theme_void() +
    theme(axis.title.x.top = element_text(size = 14, hjust = 0.05,
                                          margin = margin(0, 0, 4, 0, "pt")))

  chart <- if (obs_omit == FALSE) {
    plot1 | plot2 | plot3
  } else {
    plot2 | plot3
  }
  
  return(chart)
  
}

ez_multiday_summ <- function(data) {
  
  temp <- data %>% 
    pivot_wider(names_from = "TOTAL", values_from = c(contains("DAY")), 
                names_glue = "{TOTAL}_{.value}") %>% 
    relocate(REGION, REGION.NAME, 
             starts_with("OBSERVERS"), starts_with("CHECKLISTS"), starts_with("SPECIES")) %>% 
    mutate(across(everything(),
                  ~ ifelse(. == "NA", NA_character_, .))) %>% 
    # removes full-NA columns like ALL.DAY for observers
    remove_empty(which = "cols") %>% 
    mutate(across(everything(),
                  ~ ifelse(is.na(.), "NA", .))) 
  
  ez <- temp %>% 
    # everything needs to be same class
    mutate(across(everything(), ~ as.character(.))) %>% 
    # adding second header row
    add_row((map(str_split(names(temp), "_"),
                 ~ pluck(., 2, .default = "")) %>% 
               unlist()) %>% 
              bind_cols() %>% 
              t() %>% 
              as_tibble() %>% 
              magrittr::set_colnames(names(temp)),
            .before = 1) %>% 
    # cleaning first header row
    magrittr::set_colnames(map(str_split(names(temp), "_"),
                               ~ pluck(., 1, .default = "")) %>% 
                             unlist())
  
  return(ez)
  
}