# Test README example code
#-------------------------------------------------------------------------------

# Author: Steve Spreadborough
# Date: 2024-09-11
#
# Description: check example code in README works

testthat::test_that("README", {
  # set connection to MS SQL server
  ms_sql_server <- sql_server$new(
    driver = "SQL Server",
    server = get_env_var("MSSQL_SERVER"),
    database = get_env_var("MSSQL_DATABASE")
  )

  # set connect to MySQL server
  my_sql_server <- sql_server$new(
    driver = "MySQL ODBC 8.0 Unicode Driver",
    server = get_env_var("HOST_NAME"),
    database = get_env_var("MYSQL_DB"),
    port = get_env_var("MYSQL_PORT"),
    uid = get_env_var("MYSQL_USER"),
    pwd = get_env_var("MYSQL_PASSWORD")
  )


  # create a basic temp table
  my_data <- data.frame(
    a = c("a", "b", "c"),
    b = 1:3
  )

  # upload as a temporary table - note that generally close_conn should be TRUE
  # (which is the default setting), but it needs to be FALSE here so the
  # connection isn't shut after uploading the temporary table, as this would drop
  # the temporary table straight away.
  ms_sql_server$upload(
    data = my_data,
    table_name = "#SQLRtools_example",
    close_conn = FALSE
  )

  # get the data
  sql_data <- ms_sql_server$get("SELECT *
                                 FROM #SQLRtools_example")

  testthat::expect_equal(my_data, sql_data)


  # meta data

  # get databases in server
  ms_sql_dbs <- ms_sql_server$databases()

  # get list of tables in a given database
  my_sql_db_tables <- ms_sql_server$db_tables(database = ms_sql_dbs$name[20])

  # get list of views in a given database
  my_sql_db_views <- ms_sql_server$db_views(database = ms_sql_dbs$name[20])

  # get meta data of table in given tables
  my_sql_meta_data <- ms_sql_server$meta_data(
    database = ms_sql_dbs$name[20],
    objects = my_sql_db_views$view_name[1:5],
    details = FALSE
  )

  testthat::expect_equal(
    names(my_sql_meta_data),
    my_sql_db_views$view_name[1:5]
  )

  testthat::expect_equal(
    ms_sql_server$database,
    get_env_var("MSSQL_DATABASE")
  )


  # Avoid the "Invalid Descriptor Index" issue

  # create a basic temp table
  my_data <- data.frame(
    a = c("a", "b", "c"),
    b = 1:3
  )

  # upload as a temporary table
  ms_sql_server$upload(
    data = my_data,
    table_name = "#SQLRtools_example",
    close_conn = FALSE
  )

  # change the data type of column a to varchar(max)
  ms_sql_server$run("ALTER TABLE #SQLRtools_example
                   ALTER COLUMN a varchar(max);",
    close_conn = FALSE
  )

  # try extracting the data
  testthat::expect_error(
    suppressWarnings(
      ms_sql_server$get("SELECT * FROM #SQLRtools_example",
        close_conn = FALSE
      )
    )
  )

  # use method order_object_fields to avoid issue

  # get all the fields ordered by data type
  table_fields <- ms_sql_server$order_object_fields(
    object = "#SQLRtools_example",
    close_conn = FALSE
  )

  # now extract all the data
  tempdata <- ms_sql_server$get(glue::glue("SELECT {table_fields}
                                     FROM #SQLRtools_example",
    close_conn = TRUE
  ))

  testthat::expect_equal(my_data[c("b", "a")], tempdata)

  ms_sql_server$close_connection()
})
