### standard Fuzzy Matching Function
fuzzy_matching <- function(x) {
  cur_wdrs <- dplyr::filter(wdrs_entire, year == unique(x$year))
  
  stringdist_left_join(
  x,
  cur_wdrs,
  by = c("NAME_SUBMITTER" = "NAME_WDRS"),
  max_dist = 3,
  distance_col = "distance"
  )
}


# fuzzy matching function that matches on both first name first and last name first name orders
fuzzymatch_name_flip <- function(x) {
  library(magrittr)
  
  # filter wdrs to records with the same DOB year as the input
  # to reduce the set of possible matches
  cur_wdrs <- dplyr::filter(wdrs_entire, year %in% x$year)
  
  # 'fuzzy' match, allowing for a string distance difference up to 3,
  # on name between input and WDRS records
  match_1 <- fuzzyjoin::stringdist_left_join(
    x,
    cur_wdrs,
    by = c("NAME_SUBMITTER" = "NAME_WDRS"),
    max_dist = 3,
    distance_col = "distance"
  )
  
  # 'fuzzy' match on the first name/last name flipped version of 
  # the name in the input, to account for switching of fields, and
  # using a slightly stricter string distance threshold of 2 or less
  match_2 <- fuzzyjoin::stringdist_inner_join(
    x,
    cur_wdrs,
    by = c("NAME_SUBMITTER_2" = "NAME_WDRS"),
    max_dist = 2,
    distance_col = "distance"
  )
  
  # bind together and return all possible matches
  rbind(match_1, match_2) %>%
    as.data.frame() %>%
    unique() %>%
    dplyr::select(-c(NAME_SUBMITTER_2))
  
}

