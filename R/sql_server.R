
#' @title R6 Class representing a SQL Server
#'
#' @description
#' Create R6 class object to connect to SQL server. Contains methods to
#' interact with the SQL server, such as getting data.
#'
#' `r lifecycle::badge("stable")`
#'
#' @details
#' Note: all methods are stable other than `meta_data` which is experimental.
#'
#'
#' @returns An R6 Class object for connecting with specified SQL server and
#' methods for interacting with the server, such as running queries, getting
#' meta data and uploading data.
#'
#' @import R6
#' @import dplyr
#' @import glue
#' @import odbc
#' @import DBI
#' @importFrom stats setNames
#' @importFrom janitor clean_names
#' @import lubridate
#' @import stringi
#' @import lifecycle
#'
#' @export

sql_server <- R6Class("sql_server", public = list(


  #' @field driver driver to be used, e.g. "SQL Server". Quoted string,
  #' no default.
  driver = NULL,

  #' @field server server of the database. Quoted string, no default.
  server = NULL,

  #' @field database name of the database. Quoted string, no default.
  database = NULL,

  #' @field port port of the database. Not required if SQL server on prem &
  #' using user credentials. Quoted string, default NULL.
  port = NULL,

  #' @field uid user name for database login. Not required if querying on prem
  #' SQL server as will use windows credentials. Do NOT save credentials in
  #' code. Quoted string, default NULL.
  uid = NULL,


  #' @field pwd user password for database login. NOT REQUIRED if querying on
  #' prem SQL server as will use windows credentials. Do NOT save credentials
  #' in code. Quoted string, default NULL.
  pwd = NULL,

  #' @field server_type type of connection, set when initialised.
  server_type = NULL,

  #' @field conn connection object, set when initialised.
  conn = NULL,


  #' @description
  #' Create new SQL server connection object.
  #'
  #' @param driver driver, e.g. "SQL Server".
  #' @param server server of the database.
  #' @param database name of the database.
  #' @param port port of the database.
  #' @param uid user name for database login.
  #' @param pwd user password for database login.
  #' @return A new 'SQL server connection' object.

  initialize = function(driver,
                        server,
                        database,
                        port = NULL,
                        uid = NULL,
                        pwd = NULL) {

    # set up params
    self$driver <- driver
    self$server <- server
    self$database <- database
    self$port <- port
    self$uid <- uid
    self$pwd <- pwd
    self$conn
    self$server_type <- case_when(tolower(self$driver) == "sql server" ~ "mssql",
                                  grepl("mysql", tolower(self$driver)) ~ "mysql",
                                  TRUE ~ "other")


    if (self$server_type == "other") {
      stop("class only works with MS SQL and MySQL servers, check driver input")
    }

  },

  #' @description
  #' Sets connection to the database using parameters for class. No further
  #' arguments needed.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param database database to connect to. Quoted string, defaults to
  #' database used to create class.

  connect = function(database = self$database) {

    # if self$conn is null or invalid, connect/re-connect
    conn_null <- is.null(self$conn)

    if (conn_null == "TRUE") {
      conn_valid <- FALSE
    } else {
        conn_valid <- DBI::dbIsValid(self$conn)
    }
    
    # if database is different to self$database, re-connect
    if (database != self$database) {
      conn_valid <- FALSE
    }

    if (isFALSE(conn_valid)) {

      # set connection by server type
      if (self$server_type == "mssql") {

        # set connection string
        conn_string <- paste("driver={", self$driver, "};",
                             "server=", self$server, ";",
                             "database=", database, ";",
                             "Encrypt=true;",
                             "trusted_connection=true", sep = "")

        # set connection
        self$conn <- odbc::dbConnect(odbc::odbc(),
                                     .connection_string = conn_string,
                                     timeout = 60)


      } else if (self$server_type == "mysql") {

        # set connection with credentials
        self$conn <- DBI::dbConnect(odbc::odbc(),
                                    Driver   = self$driver,
                                    Server   = self$server,
                                    UID      = self$uid,
                                    PWD      = self$pwd,
                                    Port     = self$port,
                                    database = database)
      }
    }
  },

  #' @description
  #' Close server connection
  #'
  #' @param close logical, TRUE or FALSE whether to close the connection.

  close_connection = function(close = TRUE) {

    if (isTRUE(close)) {
      if (DBI::dbIsValid(self$conn)) {
        dbDisconnect(self$conn)
      }
    }
  },

  #' @description
  #' Run SQL query and return results to R.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param query the query to be run, quoted string, no default.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  get = function(query, close_conn = TRUE) {

    self$connect()
    output <- DBI::dbGetQuery(self$conn, query)
    self$close_connection(close_conn)
    return(output)

  },

  #' @description
  #' Run SQL query on the server without returning results. I.e.
  #' use to run processes on the server (such as creating tables etc),
  #' as opposed to extracting data from it.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param query the query to be run, quoted string, no default.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  run = function(query, close_conn = TRUE) {

    self$connect()
    output <- DBI::dbSendStatement(self$conn, query, immediate  = TRUE)
    DBI::dbClearResult(output)
    self$close_connection(close_conn)

  },



  #' @description
  #' Check if specified table exists on the server.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param table_name name of table without syntax (i.e. remove any brackets
  #' or '`'). Quoted string, no default
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  table_exists = function(table_name, close_conn = TRUE) {

    # if database is tempdb, check the table name
    if (self$database == "tempdb") {
      table_name <- self$temp_table_name(table_name)
    }

    self$connect()
    if(DBI::dbExistsTable(self$conn, table_name)) {
      self$close_connection(close_conn)
      return("yes")
    } else {
      self$close_connection(close_conn)
      return("no")
    }
  },

  #' @description
  #' Upload a dataframe to specified database in SQL server. Checks the number
  #' of rows in the tabe after upload and gives indication if sucessful or not.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param data data frame to be uploaded. Unquoted string; no default.
  #' @param table_name The name of the table when uploaded. Quoted string; no
  #' default.
  #' @param schema_name The name of the table schema when uploaded. Quoted
  #' string; default NULL.
  #' @param variable_types Data types, e.g. "nvarchar(50)", "int", "tinyint" etc.
  #' Variable types need to be in the same order as the columns in the data
  #' frame. Quoted string; no default.
  #' @param append_data Passed to `dbWriteTable`, set to TRUE to append data to
  #' an existing table in the SQL server. logical; default FALSE.
  #' @param batch_upload Upload the data in batches. Set to NULL to upload in one
  #' go or an integer to indicate the size of batches to upload data to. Note,
  #' when uploading in batches, input for append_data will be used for the first
  #' batch and then set to TRUE for following batches to allow data to be appended
  #' to the same table. If uploading in batches, it is highly recommended to check
  #' the number of rows uploaded is as expected. Numeric, default NULL.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.
  #'
  upload = function(data,
                    table_name,
                    schema_name = NULL,
                    variable_types = NULL,
                    append_data = FALSE,
                    batch_upload = NULL,
                    close_conn = TRUE) {

    # If variable_types is not NULL, make sure same length as number cols in data
    if (!is.null(variable_types)) {

      # If not as many as there are columns stop, otherwise name them
      if (length(variable_types) != length(colnames(data))) {

        stop(glue("Number of Variable Types specified needs to be the same as ",
                  "the number of columns in the data"))

      } else {

        # Make variable_types a "named character vector"
        variable_types <- setNames(variable_types, c(colnames(data)))

      }
    }

    # check batch_upload is correct input
    if (!is.null(batch_upload)) {
      if (!is.numeric(batch_upload)) {
        stop("batch_upload needs to be NULL or an integer")
      }
    }

    # Check if table exists and append_data = TRUE
    if (self$table_exists(table_name, close_conn = close_conn) == "yes"
        & append_data == FALSE) {

      stop(glue("Table {table_name} already exists. To add data to this table ",
                 "set append_data to TRUE"))

    } else if (self$table_exists(table_name, close_conn = close_conn) == "no"
               & append_data == TRUE) {

      append_data <- FALSE
      warning(glue("Append set to TRUE but table doesn't exist. Still run ",
                   "but check outputs"))

    }

    # get n rows in table to start with
    if (self$table_exists(table_name, close_conn = close_conn) == "no") {
      start_n_rows <- 0
    } else {
      start_n_rows <- self$get(query = glue("SELECT count(*) AS n
                                             FROM {table_name}"),
                               close_conn = close_conn) %>%
        pull(n)
    }

    # set connection
    self$connect()

    # if batch upload & variable types are not set, set variables types from
    # entire data set now (this avoids DBI::dbWriteTable setting different
    # data types for each batch)
    if (is.null(variable_types) & !is.null(batch_upload) & append_data == FALSE) {
      variable_types <- odbc::dbDataType(self$conn, data)
    }

    # set table name formatting depending on server type
    if (self$server_type == "mssql") {
      tbl_name <- DBI::Id(schema = schema_name,
                          table = self$temp_table_name(table_name)) #check tt
    } else if (self$server_type == "mysql") {
      tbl_name <- table_name
    }

    # if not doing in batch, just upload
    if (is.null(batch_upload)) {

      # Upload to SQL server
      DBI::dbWriteTable(self$conn,
                        name = tbl_name,
                        value = data,
                        append = append_data,
                        field.types = variable_types)

      # otherwise run as batches
    } else {

      # group data
      data <- data %>%
        mutate(group = floor(row_number()/batch_upload))

      # set up progress bar
      progress <- 0
      pb <- txtProgressBar(min = progress,
                           max = max(unique(data$group)),
                           initial = 0,
                           style = 3)

      # loop through groups
      for (i in unique(data$group)) {

        data_to_upload <- data %>%
          filter(group == i) %>%
          select(-group)

        # unless it's the first loop, set append to true and variable types to NULL
        if (i != min(unique(data$group))) {
          append_data <- TRUE
          variable_types <- NULL
        }

        # Upload to SQL server
        DBI::dbWriteTable(self$conn,
                          name = tbl_name,
                          value = data_to_upload,
                          append = append_data,
                          field.types = variable_types)

        # update progress bar
        progress <- progress + 1
        setTxtProgressBar(pb, progress)
      }

      # close the progress bar
      close(pb)
    }

    # check number of rows in table
    table_n_rows <- self$get(glue("SELECT count(*) AS n
                                    FROM {table_name}"),
                             close_conn = close_conn) %>%
      pull(n)

    # Close ODBC connection
    self$close_connection(close_conn)

    # set output
    if (start_n_rows + nrow(data) == table_n_rows) {
      return("success")
    } else {
      return(glue("N rows don't match, number in table before upload ",
                  "({start_n_rows}) plus number rows in data ({nrow(data)}) ",
                  "not equal to n rows now in table in server ({table_n_rows})"))
    }
  },

  #' @description
  #' Drop table on server if exists
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param table_name name of table.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  drop_table = function(table_name, close_conn = TRUE) {

    if (self$table_exists(table_name,
                          close_conn = close_conn) == "yes") {

      self$connect()
      DBI::dbRemoveTable(self$conn, table_name)
      self$close_connection(close_conn)

      } else {
        message(glue("table {table_name} doesn't exist"))
        }
  },


  #' @description
  #' List all the databases on the server.
  #'
  #' `r lifecycle::badge("stable")`

  databases = function() {

    if (self$server_type == "mssql") {

      query <- "SELECT name FROM sys. databases"

    } else if (self$server_type == "mysql") {

      query <- "show databases"

    }

    # return
    return(self$get(query))

  },


  #' @description
  #' List tables in a given database on the server. Note that
  #' parameter 'database' can be used here to change to a different database
  #' on the same server.
  #'
  #' NOTE: this will return an empty output if you don't have permissions to
  #' access tables.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param database database to list table from. Quoted string, defaults to
  #' database used to create class.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  db_tables = function(database = self$database, close_conn = TRUE) {

    # connect to database set for meta data
    self$connect(database = database)

    if (self$server_type == "mssql") {

      # data
      tables <- self$get("SELECT table_catalog as [database]
                          , table_schema as [schema]
                          , table_name
                          FROM INFORMATION_SCHEMA.TABLES
                          WHERE TABLE_TYPE = 'BASE TABLE'",
                         close_conn = close_conn) %>%
        arrange(table_name)

    } else if (self$server_type == "mysql") {

      # data
      tables <- self$get(glue("SHOW FULL TABLES IN {database}
                               WHERE TABLE_TYPE LIKE 'BASE TABLE'"),
                         close_conn = close_conn) %>%
        mutate(database = database,
               schema = NA_character_) %>%
        select(-Table_type)

      colnames(tables)[1] <- "table_name"
      arrange(tables, table_name)
    }
    return(tables)
  },

  #' @description
  #' List the views in a given database on the server. Note that
  #' parameter 'database' can be used here to change to a difference database
  #' on the same server.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param database database to list views from. Quoted string, defaults to
  #' database used to create class.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  db_views = function(database = self$database,
                      close_conn = TRUE) {

    self$connect(database = database)

    if (self$server_type == "mssql") {

      # data
      views <- self$get("SELECT table_catalog as [database]
                         , table_schema as [schema]
                         , table_name as [view_name]
                         FROM INFORMATION_SCHEMA.TABLES
                         WHERE TABLE_TYPE = 'VIEW'",
                        close_conn = close_conn) %>%
        arrange(view_name)

    } else if (self$server_type == "mysql") {

      # data
      views <- self$get(glue("SHOW FULL TABLES IN {database}
                              WHERE TABLE_TYPE LIKE 'VIEW'"),
                        close_conn = close_conn) %>%
        mutate(database = database,
               schema = NA_character_) %>%
        select(-Table_type)
      colnames(views)[1] <- "view_name"
      views <- arrange(views, view_name)
    }

    # return
    return(views)
  },

  #' @description
  #' Get full name of a temp table.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param x name of the table. Quoted string, no default.

  temp_table_name = function(x) {

    if (substr(x, 1, 1) == "#") {

      temp_table <- self$db_tables(database = "tempdb",
                                   close_conn = FALSE) %>%
        filter(grepl(paste0(x, "_"), table_name) |
                 x == table_name) %>%
        pull(table_name)

      if (length(temp_table) == 1) {
        return(temp_table)

      } else {

        # if not found any matches, assume doesn't exist or is not a temp table
        # and therefore return original name
        return(x)
      }
    }

    # otherwise return intput
    return(x)
  },

  #' @description
  #' SQL returns error "Invalid Descriptor Index" when reading in data that
  #' contains fields with data types varbinary(max), varchar(max) or geometry
  #' if these fields are not at the end of the SELECT statement. This is
  #' annoying when reading in all fields. This function takes the names of all
  #' the fields in a table/view, orders them by data type and returns an output
  #' that can be used within a SQL query to avoid this issue.
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param database database with tables/views to fields from. Quoted
  #' string, defaults to database used to create class.
  #' @param object name of view/table, must not include the schema name;
  #' quoted string, no default.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.


  order_object_fields = function(database = self$database, object, close_conn = TRUE) {

    self$connect(database = database)

    # if object is temp table, get the full name
    if (substr(object, 1, 1) == "#") {
      object <- self$temp_table_name(object)
    }

    if (self$server_type == "mssql") {

      # get fields & data types
      data_types <- self$get(glue("SELECT column_name
                                  , data_type
                                  , character_maximum_length
                                  FROM INFORMATION_SCHEMA.COLUMNS
                                  WHERE TABLE_NAME = '{object}'"),
                             close_conn = close_conn) %>%
        mutate(row_id = row_number(),
               order = case_when(character_maximum_length == -1L ~ 99999L,
                                 TRUE ~ row_id)) %>%
        arrange(order)

      # return
      return(paste0(data_types$column_name, collapse = ", "))


    } else if (self$server_type == "mysql") {

      stop("function not required for My SQL databases")

    }
  },

  #' @description
  #' List of fields in an object (i.e. table/field).
  #'
  #' `r lifecycle::badge("stable")`
  #'
  #' @param database database with tables/views to fields from. Quoted
  #' string, defaults to database used to create class.
  #' @param objects vector with table(s) and/or view(s) to get fields from.
  #' Quoted string, default NULL includes all the tables & views in the database
  #' (that have permissions to).
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  object_fields = function(database = self$database,
                           objects = NULL,
                           close_conn = TRUE) {

    self$connect(database = database)

    # if not given any objects, use them all
    if (is.null(objects)) {
      objects <- c(self$db_views(database = database, close_conn)$view_name,
                   self$db_tables(database = database, close_conn)$table_name)
    }

    # create list
    field_list <- list()

    # get fields for each object
    for (obj in objects) {

      obj_name <- self$temp_table_name(obj)

      obj_field_query <- glue("SELECT column_name as col_name
                               FROM INFORMATION_SCHEMA.COLUMNS
                               WHERE TABLE_NAME = '{obj_name}'
                               AND TABLE_SCHEMA = '{database}'")

      # if MS SQL server, remove reference to TABLE_SCHEMA
      if (self$server_type %in% "mssql") {
        obj_field_query <- gsub("AND TABLE_SCHEMA",
                                " -- AND TABLE_SCHEMA",
                                obj_field_query)
      }

      field_list[[obj]] <- self$get(obj_field_query,
                                    close_conn = close_conn) %>%
        pull(col_name)
    }

    # return
    return(field_list)
  },


  #' @description
  #' Create meta data from specified objects Defaults to all the objects
  #' in the given databases, otherwise specify a list of specific objects to
  #' create data for.
  #'
  #' `r lifecycle::badge("experimental")`
  #'
  #' Important: this function will fail if there are views & tables in the
  #' database with the same name.
  #'
  #' @param database database with tables/views to create meta data for. Quoted
  #' string, defaults to database used to create class.
  #' @param objects vector with table(s) and/or view(s) to create meta data for.
  #' Quoted string, defaults to all the tables & views in the database (that have
  #' permissions to).
  #' @param field vector with field(s) to create meta data for. Quoted string,
  #' default NULL includes all fields.
  #' @param details indicate if want detailed output, adds fields showing
  #' % completion of data, number of unique values, proportion of completed
  #' values are unique, date of the last non NULL value (where date is taken
  #' from field provided by argument `date_field` or the first date/datetime
  #' field in the object when `date_field` is NULL) and number of rows.
  #' Note, depending on the size of the data, this can add significant
  #' processing time. Consider use of `row_limit`, `date_filter` and
  #' `date_field` to clauclate details on subset of data. Logical,
  #' default FALSE.
  #' @param row_limit limit the number of rows used to calculate the extra
  #' details from. Integer; default 500000L.
  #' @param date_filter set date to filter data by when when calculating the
  #' extra details, useful where tables contain a large amount of data. Function
  #' will apply as date_field >= date_filter. Quoted string, default NULL.
  #' @param date_field set date field to filter & order data by when when
  #' calculating the extra details, useful where tables contain a large amount
  #' of data. Function will apply as date_field >= date_filter. Order will have
  #' affect when number of rows returned is greater than set via row_limit, i.e.
  #' the most recent row_limit number of records will be used, according to the
  #' date_field field. Quoted string, default NULL.
  #' @param close_conn set whether to close the connection after query is run.
  #' Generally this should be TRUE, only leave open if specific reason to do so,
  #' such as using temporary tables. Logical, default TRUE.

  meta_data = function(database = self$database,
                       objects = NULL,
                       details = FALSE,
                       row_limit = NULL,
                       date_filter = NULL,
                       date_field = NULL,
                       close_conn = TRUE) {
    
    # set database in connection
    self$connect(database = database)

    # make sure row_limit is integer
    if (!is.null(row_limit)) {
      row_limit <- as.integer(row_limit)
    }

    # set data filter
    if (!is.null(date_filter) & !is.null(date_field)) {
      if (self$server_type == "mysql") {
        date_filter <- glue("WHERE `{date_field}` >= '{date_filter}'")
      } else {
        date_filter <- glue("WHERE [{date_field}] >= '{date_filter}'")
      }
    } else if (!is.null(date_filter) | !is.null(date_field)) {
      message("date_filter & date_field need setting for a date filter to be applied")
    } else {
      date_filter <- ""
    }

    # set order by
    if (is.null(row_limit)) {
      order_by <- ""
      top_n_rows <- ""
    } else if (!is.null(row_limit) & !is.null(date_field)) {
      if (self$server_type == "mysql") {
        order_by <- glue("ORDER BY `{date_field}` DESC LIMIT {row_limit}")
        top_n_rows <- ""
      } else if (self$server_type == "mssql") {
        order_by <- glue("ORDER BY [{date_field}] DESC")
        top_n_rows <- glue("TOP {row_limit}")
      }
    } else if (!is.null(row_limit) & is.null(date_field)) {
      message("row_limit not applied when date_field is null")
    }

    # get list of all objects
    db_objects <- rbind(self$db_views(database = database, close_conn) %>%
                          rename(obj_name = view_name) %>%
                          mutate(type = "view"),
                        self$db_tables(database = database, close_conn) %>%
                          rename(obj_name = table_name) %>%
                          mutate(type = "user_table"))

    # if object supplied, filter db_objects for them
    if (!is.null(objects)) {

      # for each object, check if temp table and get full name if so
      objects <- sapply(objects, function(x) {
        if (substr(x, 1, 1) == "#") {
          return(self$temp_table_name(x))
        } else {
            return(x)
          }
        }) %>%
        unname()

      objects <- filter(db_objects, obj_name %in% objects)
    } else {
      objects <- db_objects
    }

    # add in fields & syntax for SQL (important if table names have spaces)
    objects <- objects %>%
      mutate(obj_name_ = case_when(self$server_type == "mysql" ~ glue("`{obj_name}`"),
                                   self$server_type == "mssql" ~ glue("[{obj_name}]")),
             schema_ = case_when(!is.na(schema) ~ paste0(schema, "."),
                                 TRUE ~ ""),
             full_obj_name = paste0(database, ".",
                                    schema_,
                                    obj_name_),
             field_id = row_number()) %>%
      select(-schema_, -obj_name_)

    # create list
    field_list <- list()

    # for each object
    for (id in objects$field_id) {

      #id <- objects$field_id[1]
      obj_ref <- filter(objects, field_id == id)
      obj <- obj_ref$obj_name
      obj_full_name <- obj_ref$full_obj_name
      print(obj)

      # get fields
      fields <- self$object_fields(database = database,
                                   objects = obj,
                                   close_conn = close_conn)[[1]]

      # write query
      obj_md_query <- glue("SELECT COLUMN_NAME as col_name
                          , IS_NULLABLE as nullable
                          , DATA_TYPE as data_type
                          , NUMERIC_PRECISION as num_precision
                          , DATETIME_PRECISION as datetime_precision
                          , CHARACTER_MAXIMUM_LENGTH as max_len
                          FROM INFORMATION_SCHEMA.COLUMNS
                          WHERE TABLE_NAME = '{obj}'
                          AND TABLE_SCHEMA = '{database}'")

      # if MS SQL server, remove reference to TABLE_SCHEMA
      if (self$server_type %in% "mssql") {
        obj_md_query <- gsub("AND TABLE_SCHEMA",
                             " -- AND TABLE_SCHEMA",
                             obj_md_query) 
      }
      
      
      # set database in connection
      self$connect(database = database)

      # get basic meta from SQL
      obj_MD <- self$get(obj_md_query,
                         close_conn = close_conn) %>%
        mutate(max_len = case_when(!is.na(max_len) ~ paste0("(", max_len, ")"),
                                   TRUE ~ ""),
               col_type = glue("{data_type}{max_len}"),
               nullable = tolower(nullable)) %>%
        select(col_name, data_type, col_type, everything(), -max_len)

      # get indexes
      if (self$server_type == "mssql") {

        indexes <- self$get(glue("select *
                             from sys.indexes
                             where object_id = (select top 1 object_id
                                               from sys.objects
                                               where [name] = '{obj}' AND
                                               [type_desc] = '{obj_ref$type}')
                                   AND [name] IS NOT NULL"),
                            close_conn = close_conn) %>%
          janitor::clean_names() %>%
          select(name) %>%
          rename(col_name = name) %>%
          mutate(index = "yes")

      } else if (self$server_type == "mysql") {

        indexes <- self$get(glue("SHOW INDEX FROM {obj_full_name}"),
                            close_conn = close_conn) %>%
          janitor::clean_names() %>%
          select(column_name) %>%
          rename(col_name = column_name) %>%
          mutate(index = "yes")
      }

      # add index details
      obj_MD <- left_join(obj_MD, indexes, by = "col_name")

      # add further details
      if (details == "TRUE") {

        # record further info on fields
        obj_details <- data.frame()

        # get first date time field if date field not provided
        if (is.null(date_field)) {
          date_field_null_calc <- obj_MD %>%
            filter(data_type %in% c("date", "datetime")) %>%
            pull(col_name)
          if (length(date_field_null_calc) > 0) {
            date_field_null_calc <- date_field_null_calc[1]
          } else {
            date_field_null_calc <- NULL
          }
        } else {
          date_field_null_calc <- date_field
        }

        # create temp table from main dataset
        temp_table <- glue("meta_temp_{gsub('[[:punct:] ]+','', now())}")

        if (self$server_type == "mysql") {

          temp_table <- glue("{database}.{temp_table}")

          self$run(glue("CREATE TEMPORARY TABLE {temp_table}
                         SELECT {top_n_rows} *
                         FROM {obj_full_name}
                         {date_filter}
                         {order_by}"),
                   close_conn = FALSE)

        } else if (self$server_type == "mssql") {

          temp_table <- glue("#{temp_table}")
          self$connect()
          DBI::dbExecute(self$conn,
                               glue("SELECT {top_n_rows} *
                                     INTO {temp_table}
                                     FROM {obj_full_name}
                                     {date_filter}
                                     {order_by}"),
                         immediate = TRUE)
        }

        # number of rows
        n_rows <- self$get(glue("SELECT count(*) as n
                                 FROM {temp_table}"),
                           close_conn = FALSE) %>%
          pull(n)

        # set progress bar
        i <- 0
        pb <- txtProgressBar(min = i, max = length(fields), initial = 0)

        # loop through fields and get relevant details
        for (field in fields) {

          #field <- fields[1]

          tidy_field <- case_when(self$server_type == "mssql" ~ glue("[{field}]"),
                                  self$server_type == "mysql" ~ glue("`{field}`"))

          # percent complete
          proportion_complete <- self$get(glue("SELECT count(*) as n, completed
                                               from (
                                                   SELECT case when {tidy_field} is null then 'missing'
                                                   ELSE 'complete' end as completed
                                                   FROM {temp_table}
                                               ) as b
                                               GROUP BY completed
                                               "),
                                          close_conn = FALSE) %>%
            tidyr::complete(completed = c("missing", "complete")) %>%
            mutate(n = as.integer(n),
                   n = case_when(is.na(n) ~ 0,
                                 TRUE ~ n)) %>%
            tidyr::pivot_wider(values_from = "n", names_from = "completed") %>%
            mutate(proportion_complete = complete/(missing+complete)) %>%
            pull(proportion_complete)

          # number of unique values
          n_unique_vals <- self$get(glue("SELECT count(*) as n
                                    from (
                                        SELECT distinct {tidy_field}
                                        FROM {temp_table}
                                        WHERE {tidy_field} IS NOT NULL
                                    ) as b
                                    "),
                                    close_conn = FALSE) %>%
            pull(n)

          # calculate proportion of completed values are unique
          prop_completed_vals_unique <- n_unique_vals/(n_rows*proportion_complete)
          prop_completed_vals_unique <- round(prop_completed_vals_unique, 3)

          # tidy perc_complete
          prop_complete <- round(proportion_complete, 3)

          # if have a date field, get the date of the last non-null record
          if (!is.null(date_field_null_calc)) {
            date_of_last_non_null <- self$get(glue("SELECT max({date_field_null_calc}) as max_date
                                                   FROM {temp_table}
                                                   WHERE {tidy_field} IS NOT NULL"),
                                              close_conn = FALSE) %>%
              pull(max_date) %>%
              paste0(., " (from field [", date_field_null_calc, "])")
          } else {
            date_of_last_non_null <- NA_character_
          }

          # add to data frame
          obj_details <- rbind(obj_details,
                               data.frame(col_name = field,
                                          prop_complete = prop_complete,
                                          n_unique_vals = n_unique_vals,
                                          prop_completed_vals_unique = prop_completed_vals_unique,
                                          date_last_non_null_value = date_of_last_non_null,
                                          n_rows = n_rows))

          # update progress bar
          i <- i + 1
          setTxtProgressBar(pb, i)
        }

        # join to main meta data and add fields
        obj_MD <- obj_MD %>%
          left_join(obj_details, by = "col_name")

        # drop temporary table if exists
        if (self$server_type == "mysql") {
          if (self$table_exists(temp_table) == "yes") {
            self$drop_table(temp_table)
          }
        }
      }

      # add the object meta data to the meta data list
      field_list[[obj]] <- obj_MD
    }

    # return data
    return(field_list)
  }

))





