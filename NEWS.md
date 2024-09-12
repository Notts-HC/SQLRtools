
# SQLRtools 0.0.1

Initial complete package containing:

- R6 class object `sql_server` for connecting with SQL servers containing 
methods for interacting with the server, such as running queries, getting
meta data and uploading data.
- `get_env_var()` to easily implement keyring to store credentials and 
sensitive information. 

Note all methods in `sql_server` are stable other than the `meta_data()` 
method. 
