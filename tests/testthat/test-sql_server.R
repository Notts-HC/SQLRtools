test_that("expect R6 class object", {
  local_mocked_bindings(
    sql_server = function(driver,
                          server,
                          database,
                          port,
                          uid,
                          pwd) {
      driver = "MySQL ODBC 8.0 Unicode Driver"
      server = "HOST_NAME"
      database = "MYSQL_DB"
      port = "MYSQL_PORT"
      uid = "MYSQL_USER"
      pwd = "MYSQL_PASSWORD"
      return(R6Class("Test",
                    public = list(driver = driver,
                  server = server,
                  database = database,
                  port = port,
                  uid = uid,
                  pwd = pwd)
                  ))
    })
    
    mysql_serv_mock <- sql_server(driver = "MySQL ODBC 8.0 Unicode Driver",
                                 server = "HOST_NAME",
                                 database = "MYSQL_DB",
                                 port = "MYSQL_PORT",
                                 uid = "MYSQL_USER",
                                 pwd = "MYSQL_PASSWORD")
    
    mysql_serv_NULL <- sql_server(driver = NULL,
                             server = NULL,
                             database = NULL,
                             port = NULL,
                             uid = NULL,
                             pwd = NULL)

    expect_true(is.R6Class(mysql_serv_mock))
    expect_true(is.R6Class(mysql_serv_NULL))
    expect_error(mysql_serv_mock$new(), NA)
    expect_error(mysql_serv_NULL$new(), NA)
})


test_that("expect message when drop_table used as no table exists", {
  local_mocked_bindings(
    sql_server = function(driver,
                          server,
                          database,
                          port,
                          uid,
                          pwd,
                          table_name) {
      driver = "driver"
      server = "HOST_NAME"
      database = "MYSQL_DB"
      port = "MYSQL_PORT"
      uid = "MYSQL_USER"
      pwd = "MYSQL_PASSWORD"
      table_name = NULL
      return(R6Class("Test",
                     public = list(driver = driver,
                                   server = server,
                                   database = database,
                                   port = port,
                                   uid = uid,
                                   pwd = pwd,
                                   table_name = table_name)
      ))
    })
  
  # create vector
  test_table_name <- "SQLRtools_test_table"

  # create dummy data using dittodb
  test_data <- data.frame(Int_field = 1:200,
                          char_field_1 = stri_rand_strings(200, sample(5:11, 5, replace = TRUE), '[a-zA-Z]'),
                          char_field_2 = stri_rand_strings(200, sample(5:11, 5, replace = TRUE), '[a-zA-Z]'),
                          date_field = sample(seq(as.Date('2018/01/01'), as.Date('2024/01/01'), by = "day"), 200),
                          date_time_field = sample(seq(as_datetime('2018-01-01 00:00:00'),
                                                       as_datetime('2024-01-01 00:00:00'),
                                                       by = "min"), 200))

  mysql_serv_mock <- sql_server(driver = "driver",
                           server = "HOST_NAME",
                           database = "MYSQL_DB",
                           port = "MYSQL_PORT",
                           uid = "MYSQL_USER",
                           pwd = "MYSQL_PASSWORD",
                           table_name = NULL)
  
  expect_message(mysql_serv$drop_table(test_table_name), "table SQLRtools_test_table doesn't exist")
  
})


