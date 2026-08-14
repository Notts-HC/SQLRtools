
# Test sql_server class with MySQL server
#------------------------------------------------------------------------------#

# Author: Steve Spreadborough
# Date: 2023-10-17
#
# Description: check that the methods in sql_server class work as expected
# with a MYSQL server.

# create mysql server object
mysql_serv <- sql_server$new(
  driver = "MySQL ODBC 8.0 Unicode Driver",
  server = get_env_var("HOST_NAME"),
  database = get_env_var("MYSQL_DB"),
  port = get_env_var("MYSQL_PORT"),
  uid_var = "MYSQL_USER",
  pwd_var = "MYSQL_PASSWORD"
  )

# set name of test table
test_table_name <- "SQLRtools_test_table"

# make sure table doesn't exist before running
mysql_serv$drop_table(test_table_name)
mysql_serv$drop_table(glue("{test_table_name}_renamed"))

# create dummy data to be uploaded
n <- 200

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

testthat::test_that("mysql_server - upload method", {

  # upload table in one go
  upload_outcome <- mysql_serv$upload(data = test_data,
                                      table_name = test_table_name,
                                      schema = get_env_var("MYSQL_DB"))

  # table exists
  table_exists <- mysql_serv$table_exists(test_table_name)
  
  # rename table
  new_tbl_nm <- glue("{test_table_name}_renamed")
  mysql_serv$rename_table(test_table_name, new_tbl_nm)
  
  ori_after_rename_exists <- mysql_serv$table_exists(test_table_name)
  renamed_exists <- mysql_serv$table_exists(new_tbl_nm)
  
  # revert
  mysql_serv$rename_table(new_tbl_nm, test_table_name)
  
  rename_exists_after_revert <- mysql_serv$table_exists(new_tbl_nm)
  ori_exists_after_revert <- mysql_serv$table_exists(test_table_name)
  

  # n rows
  table_rows <- mysql_serv$get(glue("SELECT count(*) as n
                                    from {test_table_name}")) %>%
    pull(n) %>%
    as.integer()

  # field names
  table_fields <- mysql_serv$get(glue("SELECT *
                                    from {test_table_name}
                                    LIMIT 0"))

  # tests
  testthat::expect_equal(upload_outcome, "success")
  testthat::expect_equal(table_exists, "yes")
  testthat::expect_equal(ori_after_rename_exists, "no")
  testthat::expect_equal(renamed_exists, "yes")
  testthat::expect_equal(rename_exists_after_revert, "no")
  testthat::expect_equal(ori_exists_after_revert, "yes")
  testthat::expect_equal(table_rows, 200L)
  testthat::expect_equal(colnames(table_fields), colnames(test_data))

  # test errors if try further batch upload without specifying append
  testthat::expect_error(
    mysql_serv$upload(data = test_data,
                      table_name = test_table_name,
                      schema_name = get_env_var("MYSQL_DB"),
                      batch_upload = 100)
    )

  # append to the data using batch upload of 100 rows at a time
  batch_upload_outcome <- mysql_serv$upload(data = test_data,
                                           table_name = test_table_name,
                                           schema = get_env_var("MYSQL_DB"),
                                           batch_upload = 10,
                                           append_data = TRUE)

  # n rows
  table_rows <- mysql_serv$get(glue("SELECT count(*) as n
                                    from {test_table_name}")) %>%
    pull(n) %>%
    as.integer()

  # tests
  testthat::expect_equal(batch_upload_outcome, "success")
  testthat::expect_equal(table_rows, 400L)
})

# 2. Getting data --------------------------------------------------------------

testthat::test_that("mysql_server - get method", {

  # extract the data just upload
  db_data <- mysql_serv$get(glue("SELECT * FROM {test_table_name}"))

  # test matches the data uploaded
  testthat::expect_equal(rbind(test_data, test_data), db_data)
  
  # multi statement query
  db_data_multi <- mysql_serv$run(
    glue(
      "
      DROP TABLE IF EXISTS {mysql_serv$database}.{test_table_name}_2;
      DROP TABLE IF EXISTS {mysql_serv$database}.{test_table_name}_3;
      
      CREATE TABLE {mysql_serv$database}.{test_table_name}_2 AS
      SELECT count(*) as n
      FROM {mysql_serv$database}.{test_table_name};
      
      CREATE TABLE {mysql_serv$database}.{test_table_name}_3 AS
      SELECT *
      FROM {mysql_serv$database}.{test_table_name}_2;
      
      DROP TABLE {mysql_serv$database}.{test_table_name}_2
      "),
    close_conn = FALSE) 
  
  db_data_multi_count <- mysql_serv$get(
    glue("SELECT * FROM {mysql_serv$database}.{test_table_name}_3"),
    close_conn = FALSE
  )
  
  expect_true(nrow(db_data_multi_count) == 1)
  expect_true(db_data_multi_count$n == nrow(db_data))
  
  mysql_serv$drop_table(glue("{test_table_name}_3"))
  
  # multi statement query get
  db_data_multi_get <- mysql_serv$get(
    glue(
      "
      DROP TABLE IF EXISTS {mysql_serv$database}.{test_table_name}_2;
      DROP TABLE IF EXISTS {mysql_serv$database}.{test_table_name}_3;
      
      CREATE TABLE {mysql_serv$database}.{test_table_name}_2 AS
      SELECT count(*) as n
      FROM {mysql_serv$database}.{test_table_name};
      
      CREATE TABLE {mysql_serv$database}.{test_table_name}_3 AS
      SELECT *
      FROM {mysql_serv$database}.{test_table_name}_2;
      
      DROP TABLE {mysql_serv$database}.{test_table_name}_2;
      
      SELECT * FROM {mysql_serv$database}.{test_table_name}_3
      "),
    close_conn = FALSE) 
  
  expect_equal(db_data_multi_get, db_data_multi_count)
  

})

# 3. Meta data -----------------------------------------------------------------

testthat::test_that("mysql_server - meta data", {

  # extract the data just upload
  db_tables <- mysql_serv$db_tables()

  # check has return the test table
  testthat::expect_true(test_table_name %in% db_tables$table_name)

  # get the fields of the table
  table_fields <- mysql_serv$object_fields(objects = test_table_name)

  # check returned the same field names
  testthat::expect_equal(sort(table_fields[[1]]),
                         sort(colnames(test_data)))

  # get meta data
  original_meta <- mysql_serv$meta_data(objects = test_table_name)[[1]]

  # checks
  testthat::expect_equal(sort(table_fields[[1]]), sort(original_meta$col_name))
  testthat::expect_equal(c("date", "datetime", "int", "text", "text" ),
                         sort(original_meta$data_type))
  testthat::expect_equal("yes", unique(original_meta$nullable))

  # get detailed meta data
  detailed_meta <- mysql_serv$meta_data(objects = test_table_name,
                                        details = TRUE)[[1]]

  # checks on detailed meta
  testthat::expect_equal(colnames(detailed_meta),
                         c("col_name", "data_type", "col_type", "nullable",
                           "num_precision", "datetime_precision", "index",
                           "prop_complete", "n_unique_vals", "prop_completed_vals_unique",
                           "date_last_non_null_value", "n_rows"))
  testthat::expect_true(grepl("(from field \\[date_field\\])", detailed_meta$date_last_non_null_value[1]))

  # get the fields of all tables
  all_table_fields <- mysql_serv$object_fields()

  # get all tables & views
  all_tables <- mysql_serv$db_tables()
  all_views <- mysql_serv$db_views()

  # check have the same number of tables
  testthat::expect_equal(length(all_table_fields),
                         nrow(all_tables) + nrow(all_views))

  # get all databases
  all_dbs <- mysql_serv$databases()
  testthat::expect_true(nrow(all_dbs) > 2)

  # expect error if trying and order fileds by data type for mysql
  testthat::expect_error(
    mysql_serv$order_object_fields(object = test_table_name),
    regexp = "function not required for My SQL databases"
  )

  # drop the date field and run meta again
  mysql_serv$run(glue("ALTER TABLE {test_table_name}
                       DROP COLUMN date_field;"),
                 close_conn = FALSE)

  # get meta data
  meta_data2 <- mysql_serv$meta_data(objects = test_table_name,
                                     detail = TRUE,
                                     close_conn = FALSE)[[1]]

  testthat::expect_true(grepl("(from field \\[date_time_field\\])", meta_data2$date_last_non_null_value[1]))

})


# 4. Replace the table ---------------------------------------------------------

testthat::test_that("mssql_server - replace_db_table", {
  
  n_rows_current_table <- mysql_serv$get(
    glue(
      "SELECT count(*) as n
      from {test_table_name}")
    ) %>%
    pull(n) %>%
    as.integer()
  
  # replace with original test data
  replace_table <- mysql_serv$replace_db_table(
    data = test_data,
    table_name = test_table_name
  )
  
  n_rows_replaced_table <- mysql_serv$get(
    glue(
      "SELECT count(*) as n
      from {test_table_name}")
  ) %>%
    pull(n) %>%
    as.integer()
  
  expect_equal(n_rows_current_table, 400)
  expect_match(replace_table, "success")
  expect_equal(n_rows_replaced_table, 200)
  
})

# 5. Dropping table ------------------------------------------------------------

testthat::test_that("mysql_server - drop table", {

  # drop table
  mysql_serv$drop_table(test_table_name)

  # check if it exists
  testthat::expect_equal("no", mysql_serv$table_exists(test_table_name))

})


# 6. drop connection -----------------------------------------------------------

testthat::test_that("mysql_serv - close connection", {

  # drop table
  mysql_serv$close_connection()

  # check if it exists
  testthat::expect_false(DBI::dbIsValid(mysql_serv$conn))

})











