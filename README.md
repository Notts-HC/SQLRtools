
# SQL R Tools

![](https://img.shields.io/badge/lifecycle-Stable-green.svg)

The aim of this package is to provide functionality to support working
with MSSQL, databricks and MYSQL databases from R.

The package currently contains one R6 class object that is used to
connect with a given SQL database and provides several methods for
interacting with the database, including:

- **close_connection**: close the connection to the server, note that
  other methods will do this by default.
- **get**: run SQL query on the server and return results to R
  environment.
- **run**: run SQL query on the server without returning results to R
  environment, e.g. if inserting data into another table/creating temp
  tables etc.
- **table_exists**: check if a table exists in a database.
- **upload**: upload data to a database, with options to batch upload.
- **drop_table**: drop table in a database.
- **databases**: list databases in a server.
- **db_tables**: list tables in a database.
- **db_views**: list views in a database.
- **temp_table_name**: get the full name of a temporary table.
- **object_fields**: get list of fields from a table/view.
- **order_object_fields**: lists the fields in a table/view and order by
  data type, with varchar(max) and geometry fields at the end
  (i.e. useful for avoiding “Invalid Descriptor Index” errors, see
  examples below)
- **meta_data**: gives details on given list of tables/views, including
  data types, indexes, completeness and proportion values that are
  unique.

## Installation

This package is not on CRAN and can be installed from GitHub using:

``` r

# install the package
pak::pkg_install("Notts-HC/SQLRtools")
```

## About

You are reading the doc about version: 0.0.4

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-07-20 15:33:37 BST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading SQLRtools
#> ── R CMD check results ─────────────────────────────── SQLRtools 1.0.0.9000 ────
#> Duration: 13m 13.9s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

``` r
covr::package_coverage()
#> SQLRtools Coverage: 85.97%
#> R/utils.R: 75.84%
#> R/sql_server.R: 88.65%
```

## Using the package

As above, the package contains an R6 class object that acts as the
connection to the server. This means that once the initial sql_server
class object is created there is no further need to provide connection
details to connect to the server.

The below gives examples of connecting to a MSSQL server and MYSQL
server:

``` r

library(SQLRtools)


# set connection to MS SQL server
ms_sql_server <- sql_server$new(driver = "SQL Server",
                                server = get_env_var("MSSQL_SERVER"),
                                database = get_env_var("MSSQL_DATABASE"))

# set connect to MySQL server
my_sql_server <- sql_server$new(driver = "MySQL ODBC 8.0 Unicode Driver",
                                server = get_env_var("HOST_NAME"),
                                database = get_env_var("MYSQL_DB"),
                                port = get_env_var("MYSQL_PORT"),
                                uid = get_env_var("MYSQL_USER"),
                                pwd = get_env_var("MYSQL_PASSWORD"))
```

The methods listed above can now be used with these connections to:

##### Upload & query data

``` r

# create a basic temp table
my_data <- data.frame(a = c("a", "b", "c"),
                      b = 1:3)

# upload as a temporary table - note that generally close_conn should be TRUE
# (which is the default setting), but it needs to be FALSE here so the 
# connection isn't shut after uploading the temporary table, as this would drop
# the temporary table straight away. 
ms_sql_server$upload(data = my_data,
                     table_name = "#SQLRtools_example",
                     close_conn = FALSE)

# get the data
sql_data <- ms_sql_server$get("SELECT * 
                               FROM #SQLRtools_example")
```

##### Explore databases & their objects

``` r

# get databases in server
ms_sql_dbs <- ms_sql_server$databases()

# get list of tables in a given database
my_sql_db_tables <- ms_sql_server$db_tables(database = ms_sql_dbs$name[20])

# get list of views in a given database
my_sql_db_views <- ms_sql_server$db_views(database = ms_sql_dbs$name[20])

# get meta data of table in given tables
my_sql_meta_data <- ms_sql_server$meta_data(database = ms_sql_dbs$name[20],
                                            objects = my_sql_db_views$view_name[1:5],
                                            details = FALSE)

names(my_sql_meta_data)[1]
View(my_sql_meta_data[1][[1]])
```
