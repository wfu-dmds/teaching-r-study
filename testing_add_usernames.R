# Testing the add usernames


# Set up usernames with 10, download and save original ones and print out lenths
setup_usernames('testing/', num_usernames = 10)
rdrop2::drop_download('teaching-r-study/all_usernames.rds', overwrite = T)
rdrop2::drop_download('teaching-r-study/group_a.rds', overwrite = T)
rdrop2::drop_download('teaching-r-study/group_b.rds', overwrite = T)
original_usernames <- readRDS('all_usernames.rds')
original_group_a <- readRDS('group_a.rds')
original_group_b <- readRDS('group_b.rds')
unlink('all_usernames.rds')
unlink('group_a.rds')
unlink('group_b.rds')
nrow(original_usernames)
length(original_group_a)
length(original_group_b)


# Add new usernames and then download and save and print out lengths
add_usernames('teaching-r-study/', num_usernames = 10)
rdrop2::drop_download('teaching-r-study/all_usernames.rds', overwrite = T)
rdrop2::drop_download('teaching-r-study/group_a.rds', overwrite = T)
rdrop2::drop_download('teaching-r-study/group_b.rds', overwrite = T)
added_usernames <- readRDS('all_usernames.rds')
added_group_a <- readRDS('group_a.rds')
added_group_b <- readRDS('group_b.rds')
unlink('all_usernames.rds')
unlink('group_a.rds')
unlink('group_b.rds')
nrow(added_usernames)
length(added_group_a)
length(added_group_b)

# Remove the newly added usernames, download and save and print length
remove_usernames('teaching-r-study/', num_usernames = 10)
rdrop2::drop_download('teaching-r-study/all_usernames.rds', overwrite = T)
rdrop2::drop_download('teaching-r-study/group_a.rds', overwrite = T)
rdrop2::drop_download('teaching-r-study/group_b.rds', overwrite = T)
removed_usernames <- readRDS('all_usernames.rds')
removed_group_a <- readRDS('group_a.rds')
removed_group_b <- readRDS('group_b.rds')
unlink('all_usernames.rds')
unlink('group_a.rds')
unlink('group_b.rds')
nrow(removed_usernames)
length(removed_group_a)
length(removed_group_b)

# Use testthat to check that the original usernames and one which had added and
# removed are equal
testthat::test_that("Original usernames and removed usernames are equal", {
  testthat::local_edition(3)
  testthat::expect_equal(original_usernames, removed_usernames)
})