
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
