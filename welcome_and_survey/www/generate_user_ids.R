#' Generate Random Strings from random.org
#'
#' This function is minimally adapted from the CRAN package "random" which
#' provides access to random.org.
#'
#' @param n Number of strings
#' @param len Length of strings
#' @param digits Logical: TRUE and digits will be in string, FALSE and they won't.
#' @param upperalpha Logical: TRUE and capital letters will be in string, FALSE and they won't.
#' @param loweralpha Logical: TRUE and lowercase will be in string, FALSE and they won't.
#' @param unique Logical: TRUE and strings will be unique, FALSE and they won't.
#'
#' @return A data frame with column name "strings" containing n unique strings.
#' @export
#'
#' @examples
#' 
#' strings <- rand_str(upperalpha = FALSE)

rand_str <- function(n = 10, len = 6, digits = TRUE,
                          upperalpha = TRUE, loweralpha=TRUE,
                          unique = TRUE) { 
  if (n < 1 || n > 1e4)
    stop("Random string requests must be between 1 and 10,000 numbers")
  if (len < 1 || len > 20)
    stop("Random string length must be between 1 and 20")
  if (class(digits)!="logical" || class(upperalpha)!="logical" ||
      class(loweralpha)!="logical" || class(unique)!="logical") 
    stop("The 'digits', '(lower|upper)alpha' and 'unique' arguments has to be logical")
  if ( !digits && !upperalpha && !loweralpha)
    stop("The 'digits', 'loweralpha' and 'loweralpha' cannot all be false at the same time")
  urltxt <- paste("https://www.random.org/strings/",
                  "?num=", n,
                  "&len=", len,
                  "&digits=", ifelse(digits, "on", "off"),
                  "&upperalpha=", ifelse(upperalpha, "on", "off"),
                  "&loweralpha=", ifelse(loweralpha, "on", "off"),
                  "&unique=", ifelse(unique, "on", "off"),
                  "&format=plain",
                  "&rnd=new",
                  sep="")
  con <- url(urltxt, open="r")
  randStrings <- read.table(con)
  names(randStrings) <- "strings"
  on.exit(close(con))
  return(randStrings)
}

#' Set up the username files on Dropbox
#'
#' @param drop_path The path (folder) in dropbox account where the username file is located
#' @param num_usernames Number of usernames to generate between both groups. Must be divisible by two.
#'
#' @return
#' @export
#'
#' @examples
setup_usernames <- function(drop_path, num_usernames) {
  
  if (num_usernames %% 2 != 0) {
    stop("Number of usernames must be divisible by two.")
  }
  
  # Create random strings and vectors for each group
  strings <- rand_str(n = num_usernames, upperalpha = FALSE)
  group_a <- paste0(strings[1:(num_usernames/2),], "_GA")
  group_b <- paste0(strings[((num_usernames/2) + 1):num_usernames,], "_GB")
  
  all_usernames <- data.frame(combined_groups = c(group_a, group_b))
  
  saveRDS(all_usernames, "all_usernames.rds")
  rdrop2::drop_upload(file = 'all_usernames.rds',
                      path = drop_path)
  unlink("all_usernames.rds")
  
  # Save the vectors to rds, upload to dropbox, and delete local versions
  saveRDS(group_a, "group_a.rds")
  rdrop2::drop_upload(file = 'group_a.rds', 
                      path = drop_path)
  unlink("group_a.rds")
  
  
  saveRDS(group_b, "group_b.rds")
  rdrop2::drop_upload(file = 'group_b.rds', 
                      path = drop_path)
  unlink("group_b.rds")
  
}

#' Update username vector on Dropbox
#' 
#' This function is called on exit when supplying a new username for the
#' teaching r study. It takes in the vector most recently used to supply a
#' username, removes the entry that was used, and reuploads the file.
#'
#' @param user_vec The last used vector
#' @param file The username file on dropbox.
#' @param drop_path The path (folder) in dropbox account where the username file is located
#'
#' @return
#'
update_username_vector <- function(user_vec, file, drop_path) {
  
  # remove the used username
  new_vec <- user_vec[-1]
  # save the new object locally
  saveRDS(object = new_vec, 
          file = file)
  # upload the new .rds file to dropbox
  rdrop2::drop_upload(file = file,
                      path = drop_path)
  # delete local version
  unlink(file)
}


#' Get a unique username
#' 
#' This function takes in a group (either A or B) and a 
#'
#' @param group The experimental group. Character strings "A" or "B".
#' @param drop_path The path (folder) in dropbox account where the username file is located
#'
#' @return
#' @export
#'
#' @examples
get_username <- function(group, drop_path) {
  
  # get the filename containing usernames for each group
  filename <- paste0("group_", tolower(group), ".rds")
  
  # download the group-specific usernames, read it, and select the first.
  rdrop2::drop_download(paste0(drop_path, filename))
  vector <- readRDS(filename)
  username <- vector[1]
  
  #when exiting the function, update the username vectors on dropbox.
  on.exit(update_username_vector(user_vec = vector, 
                                 file = filename,
                                 drop_path = drop_path))
  
  return(username)
  
}

#' Add Usernames
#'
#' @param drop_path The path (folder) in dropbox account where the username file is located
#' @param num_usernames The number of usernames to add.
#'
#' @return
#' @export
#'
#' @examples
add_usernames <- function(drop_path, num_usernames) {
  
  if (num_usernames %% 2 != 0) {
    stop("Number of usernames must be divisible by two.")
  }
  
  # There is a better way to do this with functional programming, I'm sure, but
  # this works...
  
  all_filename <- paste0(drop_path, "all_usernames.rds")
  a_filename <- paste0(drop_path, "group_a.rds")
  b_filename <- paste0(drop_path, "group_b.rds")
  
  rdrop2::drop_download(all_filename)
  rdrop2::drop_download(a_filename)
  rdrop2::drop_download(b_filename)
  
  original_usernames <- readRDS("all_usernames.rds")
  original_group_a <- readRDS("group_a.rds")
  original_group_b <- readRDS("group_b.rds")
  
  new_usernames <- rand_str(n = num_usernames, upperalpha = FALSE) %>%
    dplyr::mutate(group = rep(c("_GA", "_GB"), (num_usernames/2))) %>%
    tidyr::unite(col = combined_groups,
                 sep = "")
  
  new_group_a <- new_usernames %>%
    dplyr::filter(stringr::str_detect(combined_groups, "_GA")) %>%
    dplyr::pull(combined_groups)
  
  new_group_b <- new_usernames %>%
    dplyr::filter(stringr::str_detect(combined_groups, "_GB")) %>%
    dplyr::pull(combined_groups)
  
  added_a <- c(original_group_a, new_group_a)
  
  added_b <- c(original_group_b, new_group_b)
  
  added_all_usernames <- rbind(original_usernames, new_usernames)
  
  files_usernames <- list(added_a,
                          added_b,
                          added_all_usernames)
  
  filenames_usernames <- c("group_a.rds",
                           "group_b.rds",
                           "all_usernames.rds")
  
  purrr::walk2(.x = files_usernames, 
               .y = filenames_usernames,
               ~saveRDS(object = .x, 
                        file = .y))
  
  purrr::walk(.x = filenames_usernames, 
              ~ rdrop2::drop_upload(file = .x,
                                    path = drop_path))
  
  purrr::walk(.x = filenames_usernames, ~ unlink(.x))
  
}



#' Remove usernames
#' 
#' @param drop_path 
#' @param num_usernames The number of usernames to remove.
#'
#' @return
#' @export
#'
#' @examples
remove_usernames <- function(drop_path, num_usernames) {
  
  if (num_usernames %% 2 != 0) {
    stop("Number of usernames must be divisible by two.")
  }
  
  # There is a better way to do this with functional programming, I'm sure, but
  # this works...
  
  all_filename <- paste0(drop_path, "all_usernames.rds")
  a_filename <- paste0(drop_path, "group_a.rds")
  b_filename <- paste0(drop_path, "group_b.rds")
  
  rdrop2::drop_download(all_filename)
  rdrop2::drop_download(a_filename)
  rdrop2::drop_download(b_filename)
  
  original_usernames <- readRDS("all_usernames.rds")
  original_group_a <- readRDS("group_a.rds")
  original_group_b <- readRDS("group_b.rds")
  
  new_usernames <- data.frame(combined_groups = original_usernames[1:(nrow(original_usernames) - num_usernames),])
  
  new_group_a <- original_group_a[1:(length(original_group_a) - (num_usernames/2))]
  
  new_group_b <- original_group_b[1:(length(original_group_b) - (num_usernames/2))]
  
  files_usernames <- list(new_group_a,
                          new_group_b,
                          new_usernames)
  
  filenames_usernames <- c("group_a.rds",
                           "group_b.rds",
                           "all_usernames.rds")
  
  purrr::walk2(.x = files_usernames, 
               .y = filenames_usernames,
               ~saveRDS(object = .x, 
                        file = .y))
  
  purrr::walk(.x = filenames_usernames, 
              ~ rdrop2::drop_upload(file = .x,
                                    path = drop_path))
  
  purrr::walk(.x = filenames_usernames, ~ unlink(.x))
  
}






