
# SQLRtools 1.0.1

Minor update to handle multi-statement SQL queries when running on MySQL
and databricks. No other changes to functions or use. 

# SQLRtools 1.0.0

### Breaking changes

- **uid** & **pwd** arguments in `sql_server$new()` depreciated and replaced 
with `uid_var` and `pwd_var` respectively. These arguments take the name of the 
environmental variable that holds the related value, as opposed to the values 
themselves. This means that the R6 object created by `sql_server$new()` does
not then store these credentials itself once created. Code therefore needs 
changing from `sql_server$new(uid = SQLRtools::get_env_var("MY_PASSWORD")` to
`sql_server$new(uid = "MY_PASSWORD")`

### Other updates:

- Added connection for databricks to run both locally/on posit connect and 
in databricks. See README for connection details.
- Added `replace_db_table()` method. This will upload a table and replace and
existing table with the same name, but will do it in stages to avoid a situation
where an error results in the existing table being dropped and the new table not
being created (resulting in no table in the database). 
- Add `catalogs()` method. Equivalent of `databases()`, lists all the catalogs 
in databricks connection. 
- Added `schemas()` method. List all the schemas in a catalog in databricks. 
- Added `rename_table()` method. Renames a given table. 


# SQLRtools 0.0.4

Minor fix to connecting to database via DSN to include the database in the 
connection. This should fix the issue of method `table_exists` not working
correctly in Linux. 

# SQLRtools 0.0.3

Minor update adding in option to connect to server via a Data Source Name (DSN)
in the initiation of the `sql_server` class object. 

# SQLRtools 0.0.2

Minor update adding in option to remove encryption in the initiation of the
`sql_server` class object.

# SQLRtools 0.0.1

Initial complete package containing:

- R6 class object `sql_server` for connecting with SQL servers containing 
methods for interacting with the server, such as running queries, getting
meta data and uploading data.
- `get_env_var()` to easily implement keyring to store credentials and 
sensitive information. 

Note all methods in `sql_server` are stable other than the `meta_data()` 
method. 
