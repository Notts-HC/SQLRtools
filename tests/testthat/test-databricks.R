
# Test sql_server class with databricks
#-------------------------------------------------------------------------------

# Author: Steve Spreadborough
# Date: 2026-07-17
#
# Description: check that the methods in sql_server class work as expected
# with a MS SQL server.

# create mysql server object
databricks_serv <- sql_server$new(
  driver = "Databricks ODBC Driver",
  port = SQLRtools::get_env_var("TEST_DATABRICKS_PORT"),
  host = SQLRtools::get_env_var("TEST_DATABRICKS_HOST"),
  httppath = SQLRtools::get_env_var("TEST_DATABRICKS_HTTPPATH"),
  pwd_var = "TEST_DATABRICKS_PAT",
  catalog = SQLRtools::get_env_var("TEST_DATABRICKS_CATALOG"),
  schema = SQLRtools::get_env_var("TEST_DATABRICKS_SCHEMA")
)

# set name of test table
test_table_name <- tolower("SQLRtools_temp_table")

# make sure table doesn't exist before running
databricks_serv$drop_table(test_table_name)

# create dummy data to be uploaded
n <- 50

test_data <- data.frame(
  Int_field = 1:n,
  char_field_1 = stri_rand_strings(n, sample(5:11, 5, replace = TRUE), '[a-zA-Z]'),
  char_field_2 = stri_rand_strings(n, sample(5:11, 5, replace = TRUE), '[a-zA-Z]'),
  date_field = sample(seq(as.Date('2018/01/01'), as.Date('2024/01/01'), by = "day"), n),
  date_time_field = sample(
    seq(as_datetime('2018-01-01 00:00:00'),
        as_datetime('2024-01-01 00:00:00'),
        by = "min"), n)
)

test_data <- as.data.frame(
  apply(test_data, 2, function(x) {x[sample(c(1:n), floor(n/10))] <- NA; x})
)

test_data <- test_data |> 
  mutate(
    Int_field = as.integer(trimws(Int_field)),
    date_field = as.Date(date_field), 
    date_time_field = as_datetime(date_time_field)
  )

test_data$char_field_1[5] <- "special'char"


# 1. uploading data ------------------------------------------------------------

testthat::test_that("databricks_server - upload method", {

  # upload table in one go
  upload_outcome <- databricks_serv$upload(
    data = test_data,
    table_name = test_table_name,
    batch_upload = 10,
    close_conn = FALSE
    )

  # table exists
  table_exists <- databricks_serv$table_exists(test_table_name,
                                          close_conn = FALSE)
  
  
  # rename table
  new_tbl_nm <- glue("{test_table_name}_renamed")
  suppressMessages(databricks_serv$drop_table(new_tbl_nm))
  databricks_serv$rename_table(test_table_name, new_tbl_nm)
  
  ori_after_rename_exists <- databricks_serv$table_exists(test_table_name)
  renamed_exists <- databricks_serv$table_exists(new_tbl_nm)
  
  # revert
  databricks_serv$rename_table(new_tbl_nm, test_table_name)
  
  rename_exists_after_revert <- databricks_serv$table_exists(new_tbl_nm)
  ori_exists_after_revert <- databricks_serv$table_exists(test_table_name)

  # n rows
  table_rows <- databricks_serv$get(
    glue("SELECT count(*) as n
         from {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}"),
  close_conn = FALSE) %>%
    pull(n) %>%
    as.integer()

  # field names
  table_fields <- databricks_serv$get(
  glue("SELECT *
       from {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}
       LIMIT 0;"),
  close_conn = FALSE)


  # tests
  testthat::expect_equal(upload_outcome, "success")
  testthat::expect_equal(table_exists, "yes")
  testthat::expect_equal(table_rows, 50L)
  testthat::expect_equal(colnames(table_fields), colnames(test_data))

  # test errors if try further batch upload without specifying append
  testthat::expect_error(
    databricks_serv$upload(data = test_data,
                      table_name = test_table_name,
                      close_conn = FALSE)
  )

  # append to the data using batch upload of 100 rows at a time
  batch_upload_outcome <- databricks_serv$upload(
    data = test_data,
    table_name = test_table_name,
    batch_upload = 50,
    append_data = TRUE,
    close_conn = FALSE
    )

  # n rows
  table_rows <- databricks_serv$get(
    glue("SELECT count(*) as n
         from {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}"),
    close_conn = FALSE)  |> 
    pull(n) |> 
    as.integer()

  # tests
  testthat::expect_equal(batch_upload_outcome, "success")
  testthat::expect_equal(table_rows, 100L)
})

# 2. Get data ------------------------------------------------------------------

testthat::test_that("databricks_server - get data", {

  # extract the data just upload
  # note that date_field is getting turned into char as read into R (meta data
  # check below will confirm if being held as a date in SQL)
  db_data <- databricks_serv$get(
    glue("SELECT * FROM {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}"),
    close_conn = FALSE
    ) |> 
    mutate(date_field = as.Date(date_field))

  # test matches the data uploaded
  testthat::expect_equal(
    rbind(test_data, test_data) |> 
      arrange(Int_field, char_field_1), 
    db_data |> 
      arrange(Int_field, char_field_1)
    )

})

# 3. Meta data -----------------------------------------------------------------

testthat::test_that("databricks_server - meta data", {

  # extract the data just upload
  db_tables <- databricks_serv$db_tables(close_conn = FALSE)

  # check has return the test table
  testthat::expect_true(test_table_name %in% db_tables$table_name)

  # get the fields of the table
  table_fields <- databricks_serv$object_fields(objects = test_table_name,
                                           close_conn = FALSE)

  # check returned the same field names
  testthat::expect_equal(sort(table_fields[[1]]),
                         sort(colnames(test_data)))
})

# 4. Replace the table ---------------------------------------------------------

testthat::test_that("mssql_server - replace_db_table", {
  
  n_rows_current_table <- databricks_serv$get(
    glue(
      "SELECT count(*) as n
      from {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}")
  ) %>%
    pull(n) %>%
    as.integer()
  
  # replace with original test data
  replace_table <- databricks_serv$replace_db_table(
    data = test_data,
    table_name = test_table_name
  )
  
  n_rows_replaced_table <- databricks_serv$get(
    glue(
      "SELECT count(*) as n
      from {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}")
  ) %>%
    pull(n) %>%
    as.integer()
  
  expect_equal(n_rows_current_table, 100)
  expect_match(replace_table, "success")
  expect_equal(n_rows_replaced_table, 50)
  
})

# 5. Drop table ----------------------------------------------------------------

testthat::test_that("databricks_server - drop table", {

  # drop table
  databricks_serv$drop_table(test_table_name, close_conn = FALSE)

  # check if it exists
  testthat::expect_equal("no", databricks_serv$table_exists(test_table_name))

})

# 6. drop connection -----------------------------------------------------------

testthat::test_that("databricks_server - close connection", {

  # drop table
  databricks_serv$close_connection()

  # check if it exists
  testthat::expect_false(DBI::dbIsValid(databricks_serv$conn))

  # reconnect & check temp table no longer exists
  testthat::expect_error(
    databricks_serv$get(
      glue(
        "SELECT * FROM {databricks_serv$catalog}.{databricks_serv$schema}.{test_table_name}")
      )
  )

  # close connection
  databricks_serv$close_connection()

})










