
# Test sql_server class with MSSQL server
#-------------------------------------------------------------------------------

# Author: Steve Spreadborough
# Date: 2023-11-22
#
# Description: check that the methods in sql_server class work as expected
# with a MS SQL server.

# create mysql server object
mssql_serv <- sql_server$new(driver = "SQL Server",
                             server = get_env_var("MSSQL_SERVER"),
                             database = get_env_var("MSSQL_DATABASE"))
# set name of test table
test_table_name <- "#SQLRtools_test_table"

# make sure table doesn't exist before running
suppressWarnings(mssql_serv$drop_table(test_table_name))

# create dummy data to be uploaded
test_data <- data.frame(Int_field = 1:200,
                        char_field_1 = stri_rand_strings(200, sample(5:11, 5, replace = TRUE), '[a-zA-Z]'),
                        char_field_2 = stri_rand_strings(200, sample(5:11, 5, replace = TRUE), '[a-zA-Z]'),
                        date_field = sample(seq(as.Date('2018/01/01'), as.Date('2024/01/01'), by = "day"), 200),
                        date_time_field = sample(seq(as_datetime('2018-01-01 00:00:00'),
                                                     as_datetime('2024-01-01 00:00:00'),
                                                     by = "min"), 200))


# 1. uploading data ------------------------------------------------------------

testthat::test_that("mssql_server - upload method", {

  # upload table in one go
  upload_outcome <- mssql_serv$upload(data = test_data,
                                      table_name = test_table_name,
                                      close_conn = FALSE)

  # table exists
  table_exists <- mssql_serv$table_exists(test_table_name,
                                          close_conn = FALSE)

  # n rows
  table_rows <- mssql_serv$get(glue("SELECT count(*) as n
                                    from {test_table_name}"),
                               close_conn = FALSE) %>%
    pull(n) %>%
    as.integer()

  # field names
  table_fields <- mssql_serv$get(glue("SELECT TOP 0 *
                                    from {test_table_name}"),
                                 close_conn = FALSE)


  # tests
  testthat::expect_equal(upload_outcome, "success")
  testthat::expect_equal(table_exists, "yes")
  testthat::expect_equal(table_rows, 200L)
  testthat::expect_equal(colnames(table_fields), colnames(test_data))

  # test errors if try further batch upload without specifying append
  testthat::expect_error(
    mssql_serv$upload(data = test_data,
                      table_name = test_table_name,
                      schema_name = Sys.getenv("MYSQL_DB"),
                      batch_upload = 100,
                      close_conn = FALSE)
  )

  # append to the data using batch upload of 100 rows at a time
  batch_upload_outcome <- mssql_serv$upload(data = test_data,
                                            table_name = test_table_name,
                                            batch_upload = 50,
                                            append_data = TRUE,
                                            close_conn = FALSE)

  # n rows
  table_rows <- mssql_serv$get(glue("SELECT count(*) as n
                                    from {test_table_name}"),
                               close_conn = FALSE) %>%
    pull(n) %>%
    as.integer()

  # tests
  testthat::expect_equal(batch_upload_outcome, "success")
  testthat::expect_equal(table_rows, 400L)
})

# 2. Get data ------------------------------------------------------------------

testthat::test_that("mssql_server - get data", {

  # extract the data just upload
  # note that date_field is getting turned into char as read into R (meta data
  # check below will confirm if being held as a date in SQL)
  db_data <- mssql_serv$get(glue("SELECT * FROM {test_table_name}"),
                            close_conn = FALSE) %>%
    mutate(date_field = as.Date(date_field))

  # test matches the data uploaded
  testthat::expect_equal(rbind(test_data, test_data), db_data)

})

# 3. Run & order by fields methods ---------------------------------------------

testthat::test_that("mssql_server - run & order by fields", {

  # change the data type of column a to varchar(max)
  mssql_serv$run(glue("ALTER TABLE {test_table_name}
                       ALTER COLUMN char_field_1 varchar(max);"),
                    close_conn = FALSE)

  # now expect error extracting the data
  testthat::expect_error(
    suppressWarnings(mssql_serv$get(glue("SELECT * FROM {test_table_name}"),
                                    close_conn = FALSE))
  )

  # use method order_object_fields to avoid issue

  # get all the fields ordered by data type
  table_fields <- mssql_serv$order_object_fields(object = test_table_name,
                                                 close_conn = FALSE)

  # now extract all the data
  extract_ordered_feilds <- mssql_serv$get(glue("SELECT {table_fields}
                                                   FROM {test_table_name}"),
                                           close_conn = FALSE) %>%
    mutate(date_field = as.Date(date_field))

  # tests
  testthat::expect_equal(colnames(extract_ordered_feilds),
                         c("Int_field", "char_field_2", "date_field",
                           "date_time_field", "char_field_1"))
  testthat::expect_equal(rbind(test_data, test_data),
                         select(extract_ordered_feilds,
                                Int_field, char_field_1, char_field_2,
                                date_field, date_time_field))

})


# 3. Meta data -----------------------------------------------------------------

testthat::test_that("mssql_server - meta data", {

  # extract the data just upload
  db_tables <- mssql_serv$db_tables(close_conn = FALSE)

  # check has return the test table
  testthat::expect_true(mssql_serv$temp_table_name(test_table_name)
                        %in% db_tables$table_name)

  # get the fields of the table
  table_fields <- mssql_serv$object_fields(objects = test_table_name,
                                           close_conn = FALSE)

  # check returned the same field names
  testthat::expect_equal(sort(table_fields[[1]]),
                         sort(colnames(test_data)))

  # get meta data
  meta_data <- mssql_serv$meta_data(objects = test_table_name,
                                    detail = TRUE,
                                    close_conn = FALSE)[[1]]

  # checks
  testthat::expect_equal(sort(table_fields[[1]]), sort(meta_data$col_name))
  testthat::expect_equal(c("date", "datetime", "int", "varchar", "varchar" ),
                         sort(meta_data$data_type))
  testthat::expect_equal("yes", unique(meta_data$nullable))
  testthat::expect_equal(colnames(meta_data),
                         c("col_name", "data_type", "col_type", "nullable",
                           "num_precision", "datetime_precision", "index",
                           "prop_complete", "n_unique_vals", "prop_completed_vals_unique",
                           "date_last_non_null_value", "n_rows"))
  testthat::expect_true(grepl("(from field \\[date_field\\])", meta_data$date_last_non_null_value[1]))


  # filter the data by date in meta data
  date_to_filter <- max(test_data$date_field) - 150
  meta_data2 <- mssql_serv$meta_data(objects = test_table_name,
                                     detail = TRUE,
                                     row_limit = 1000,
                                     date_filter = date_to_filter,
                                     date_field = "date_field",
                                     close_conn = FALSE)[[1]]
  testthat::expect_equal(meta_data2$n_rows[1], nrow(filter(test_data, date_field >= date_to_filter))*2)
  testthat::expect_true(grepl("(from field \\[date_field\\])", meta_data2$date_last_non_null_value[1]))

  # drop the date field and run meta again
  mssql_serv$run(glue("ALTER TABLE {test_table_name}
                       DROP COLUMN date_field;"),
                 close_conn = FALSE)

  # drop the date field and run meta again
  mssql_serv$run(glue("ALTER TABLE {test_table_name}
                       DROP COLUMN date_time_field;"),
                 close_conn = FALSE)

  # get meta data
  meta_data3 <- mssql_serv$meta_data(objects = test_table_name,
                                     detail = TRUE,
                                     close_conn = FALSE)[[1]]

  testthat::expect_true(is.na(meta_data3$date_last_non_null_value[1]))
})

# 4. Drop table ----------------------------------------------------------------

testthat::test_that("mssql_server - drop table", {

  # drop table
  mssql_serv$drop_table(test_table_name, close_conn = FALSE)

  # check if it exists
  testthat::expect_equal("no", mssql_serv$table_exists(test_table_name))

})

# 5. drop connection -----------------------------------------------------------

testthat::test_that("mssql_server - close connection", {

  # drop table
  mssql_serv$close_connection()

  # check if it exists
  testthat::expect_false(DBI::dbIsValid(mssql_serv$conn))

  # reconnect & check temp table no longer exists
  testthat::expect_error(
    mssql_serv$get(glue("SELECT * FROM {test_table_name}"))
  )

  # close connection
  mssql_serv$close_connection()

})














