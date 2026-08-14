
# Test util functions
#------------------------------------------------------------------------------#

# Author: Steve Spreadborough
# Date: 2024-09-03

# get_env_var
testthat::test_that("utils - get_env_var", {

  test_var_val <- "SQLRtools_get_env_var_TEST"

  # create test var in renviron
  Sys.setenv(SQLRtools_TEST_VAR = test_var_val)

  # get the variable from Renviron, expecting warning that it's got it from
  # renviron
  testthat::expect_warning(
    renv_var <- get_env_var("SQLRtools_TEST_VAR")
  )

  # create var using keyring
  keyring::key_set_with_value(service = "SQLRtools_TEST_VAR",
                              password = test_var_val)

  # get the variable again, not expecting warning as should come from keyring
  kr_var <- get_env_var("SQLRtools_TEST_VAR")

  # delete the vars
  Sys.unsetenv("SQLRtools_TEST_VAR")
  keyring::key_delete(service = "SQLRtools_TEST_VAR")

  # expect error & vars to match
  testthat::expect_error(get_env_var("SQLRtools_TEST_VAR"))
  testthat::expect_match(renv_var, test_var_val)
  testthat::expect_match(kr_var, test_var_val)

})

# chr_between
testthat::test_that("utils - chr_between", {
  
  # identify BEGIN in code with comments

  string <- glue("
    -- this is some annotation 
    -- to replicate comments in SQL
    --begin here
    DROP TABLE my_table
    BEGIN
    Some coode here
    -- make it odd --"
  )
  
  chr_between_check <- chr_between(
    x = "begin", 
    y = "--", 
    z = "\n", 
    string = string
    )
  
  expect_equal(chr_between_check$within_yz, c(TRUE, FALSE))
  
  # identify BEGIN in code with comments using /*

  query <- glue(
    "-- title 
    
    /* some really long
    annoation that happens to include the word begin
    */
    
    "
  )
  
  chr_between_check_2 <- chr_between(
    x = "begin", 
    y = "/\\*", 
    z = "\\*/", 
    string = query
  )
  
  expect_equal(chr_between_check_2$within_yz, TRUE)
  
  # check identifies being after comments with /* */
  
  query <- glue(
    "-- title 
    
    begin
    
    /* some really long
    annoation that happens to include the word begin
    */
    
    "
  )
  
  chr_between_check_3 <- chr_between(
    x = "begin", 
    y = "/\\*", 
    z = "\\*/", 
    string = query
  )
  
  expect_equal(chr_between_check_3$within_yz, c(FALSE, TRUE))
  
})


# split_sql_statement
testthat::test_that("utils - split_sql_statement", {
  
  test_query <- glue(
    "-- some annotations
    -- some descriptions 
    -- includes; some list of info
    -- begin the script
    
    DROP TABLE IF EXISTS catalog.schema.my_table;
    
    CREATE TABLE catalog.schema.my_table 
    AS 
    SELECT *
    FROM my_temp;
    
    DROP TABLE my_temp"
  )

  
  split_q <- split_sql_statement(test_query)
  
  expect_true(length(split_q) == 3)
  
  no_colons <- sapply(split_q, function(x) {
    expect_false(grepl(";", x))
  })
  
  begin_query <- glue(
    "BEGIN
      INSERT INTO audit_log VALUES ('start'); 
      UPDATE customers SET status = 'ACTIVE';
    END"
  )
  
  expect_error(split_sql_statement(begin_query))
  
  begin_query2 <- glue(
    "/* some annotation
    and text with being 
    */
    
    BEGIN
      INSERT INTO audit_log VALUES ('start'); 
      UPDATE customers SET status = 'ACTIVE';
    END"
  )
  
  expect_error(split_sql_statement(begin_query2))
  
})
  


