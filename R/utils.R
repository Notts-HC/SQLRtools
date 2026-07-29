#' Get environment variables
#'
#' Function to get relevant credentials, works locally and on connect.
#'
#' If running on Connect will, use Sys.getenv() to get variables.
#'
#' If locally, will get variable using keyring (i.e. windows credential
#' manager). If keyring fails (likely because not in use for given
#' variable), will then use Sys.getenv() & give a warning to start using
#' keyring.
#'
#' @param var_name name of the variable. Note that this is the "service" in
#' when using keyring to retrieve variable. Quoted string, no default.
#'
#' @returns string
#'
#' @export

get_env_var <- function(var_name) {

  # if running on server, use Sys.getenv()
  if (Sys.getenv()["USERNAME"][[1]] == "rstudio-connect") {

    return(Sys.getenv(var_name))

    # if running locally, try keyring first
  } else {
    
    # check if have keyring installed
    kr_inst <- try({
      kr_in <- find.package("keyring")
      TRUE },
      silent = TRUE)
    
    if (kr_inst != TRUE) {
      stop("install keyring to use this function: 'installpackages('keyring')")
    }

    # try keyring
    kr <- try({
      var <- keyring::key_get(var_name)
      TRUE },
      silent = TRUE)

    # if keyring worked, return
    if (kr[1] == TRUE) {
      return(var)

      # otherwise, try Sys.getenv
    } else {

      sys_env <- try({
        var <- Sys.getenv(var_name)
        TRUE},
        silent = TRUE)

      # if Sys.gentenv worked, give warning and return
      if (sys_env[1] == TRUE & var != "") {

        warning(glue(
          "variable {var_name} found in Renviron file but not windows credentials. ",
          "Update your process to use keyring and not .Renviron file by doing the ",
          "following: \n",
          " 1. Run: keyring::key_set(service = '{var_name}') \n",
          " 2. Enter the variable value in the text box \n",
          " 3. Delete the variable {var_name} from your .Renviron file \n"
        )
        )

        return(var)

        # otherwise, variable not found
      } else {
        stop(glue("variable {var_name} not found using keyring or Sys.getenv()"))
      }
    }
  }
}

#' Check identifier input
#' 
#' @param x identifier text. Quoted string, no default
#' @param return set whether to return the value. Logical, default FALSE. 

check_identifier <- function(x, return = FALSE) {
  
  if (!grepl("^[A-Za-z0-9_]+$", x)) {
    stop("Invalid identifier")
  }
  
  if (return == TRUE) {
    x
  }
}


#' Upload data to databricks
#' 
#' Uploads data frame to databricks.
#' 
#' IMPORTANT: this creates the table and then inserts the data using DBI. Note
#' that this is unlikely to be the most efficient way to upload data to 
#' databricks, this should ONLY be used for 'one off' uploads of small 
#' datasets, anything more regular should be ingested into databricks direclty. 
#' 
#' @param conn the connection to databricks, has to be a SQLRtools::sql_server
#' R6 object. Unquoted string, no default. 
#' @param catalog The name of the catalog the schema is in. Note that this
#' is only required when connecting to databricks. Quoted string, default NULL 
#' (uses value from conn)
#' @param schema The name of the schema. Note that this is only required when 
#' connecting to databricks. Quoted string, default NULL (uses value from conn)
#' @param table_name The name of the table when uploaded. Quoted string; no
#' default.
#' @param data data frame to be uploaded. Unquoted string; no default.
#' @param append Set to TRUE to append data to an existing table in the SQL 
#' server. logical; default FALSE.
#' @param batch_upload Upload the data in batches. Set to NULL to upload in one
#' go or an integer to indicate the size of batches to upload data to. Note,
#' when uploading in batches, input for append_data will be used for the first
#' batch and then set to TRUE for following batches to allow data to be appended
#' to the same table. If uploading in batches, it is highly recommended to check
#' the number of rows uploaded is as expected. Numeric, default NULL.
#' 
#' @importFrom tidyr pivot_longer
#' @importFrom sparklyr sdf_copy_to spark_write_table

upload_to_databricks <- function(
    conn, 
    catalog = NULL, 
    schema = NULL,
    table_name, 
    data, 
    append = FALSE,
    batch_upload = 50) {
  
  
  if (nrow(data) > 1000 & conn$databricks_loc != "databricks") {
    stop("over 1000 rows, this probably needs loading via databricks")
  }
  
  if (is.null(catalog)) {catalog <- conn$catalog}
  
  if (is.null(schema)) {schema <- conn$schema}
  
  check_identifier(catalog)
  check_identifier(schema)
  
  # table exists
  table_exists <- conn$table_exists(
    table_name = table_name, 
    catalog = catalog, 
    schema = schema
  )
  
  # error if not appending and table already exists
  if (append == FALSE & table_exists == "yes") {
    stop(
      glue("table already exists in catalog schema, but append set to false")
    )
  }
  
  # different process depending where running from
  
  # when running on databricks
  if (conn$databricks_loc == "databricks") {
    
    # function shouldnt be used for creating schemas?
    #conn$run(glue("CREATE SCHEMA IF NOT EXISTS {catalog}.{schema}"))
    
    # note - as char important, spark_write_table disagrees with glue 
    # objects
    target_table_name <- as.character(glue("{catalog}.{schema}.{table_name}"))
    
    if (append == FALSE) {
      write_mode <- "error"
    } else {
      write_mode <- "append"
    }
    
    # note: use sparklyr for this (SparkR not available in later version
    # of R)
    if (is.data.frame(data)) {
      
      if (is.null(catalog)) {
        stop("catalog needs to be set when writing table in databricks")
      }
      
      if (is.null(schema)) {
        stop("schema needs to be set when writing table in databricks")
      }
      
      conn$run(glue("USE CATALOG {catalog}"))
      conn$run(glue("USE SCHEMA {schema}"))
      
      conn$connect()
      
      tbl_spark_data <- sparklyr::sdf_copy_to(
        conn$conn, 
        data, 
        overwrite = TRUE # looks like a temp table is stored, so replace if so (above handles over writing actual table)
      )
      
    } else {
      
      tbl_spark_data <- data
      
    }
    
    if (!inherits(tbl_spark_data, "tbl_spark")) {
      stop("df must be a Spark-backed tbl_spark or a local data.frame")
    }
    
    # write table
    sparklyr::spark_write_table(
      x = tbl_spark_data, 
      name = as.character(table_name), 
      mode = write_mode
    )
    
    # when running locally
  } else if (conn$databricks_loc == "local") {
    
    # add row id
    data <- data |> 
      mutate(row_id = row_number()) |> 
      select(row_id, everything())
    
    
    # create table if need to
    if (table_exists == "no") {
      
      # code to create fields and data types
      df_fields <- as.data.frame(
        sapply(select(data, -row_id), function(x) {class(x)[1]})
      ) 
      colnames(df_fields)[1] <- "data_type"
      df_fields$field <- rownames(df_fields)
      
      table_fields <- df_fields |> 
        mutate(
          data_type = tolower(data_type),
          sql_data_type = case_when(
            data_type == "character" ~ "STRING", 
            data_type == "integer" ~ "INT", 
            data_type == "logical" ~ "BOOLEAN",
            data_type == "double" ~ "DOUBLE", 
            data_type == "numeric" ~ "DOUBLE",
            data_type == "date" ~ "DATE", 
            data_type == "factor" ~ "STRING", 
            data_type == "posixct" ~ "TIMESTAMP",
            data_type == "posixt" ~ "TIMESTAMP",
            TRUE ~ "unknown"
          ),
          val = paste0(field, " ", sql_data_type)
        )
      
      failed_data_type <- filter(table_fields, sql_data_type == "unknown")
      
      if (nrow(failed_data_type) > 0) {
        fails <- paste0(unique(failed_data_type$data_type, collapse = ", "))
        stop(glue("failed to convert data types for: {fails}"))
      }
      
      table_fields <- paste0(table_fields$val, collapse = ", \n")
      
      # create the table
      conn$run(
        glue(
          "CREATE TABLE {catalog}.{schema}.{table_name} (
                {table_fields}
                );"
        )
      )
    }
    
    # format data for insert
    value_data <- data |> 
      mutate(across(everything(), as.character)) |> 
      pivot_longer(-row_id) |> 
      mutate(
        value = case_when(
          is.na(value) ~ "NULL",
          value == "" ~ "NULL", 
          TRUE ~ as.character(DBI::dbQuoteString(conn$conn, value))
        ),
        value = gsub("''", "\\\\'", value)
      ) |> 
      group_by(row_id) |> 
      summarise(val = paste0(value, collapse = ", "),
                .groups = "drop") |> 
      mutate(
        val = paste0("(", val, ")"),
        row_id = as.integer(row_id), 
        batch = floor(row_id/batch_upload)
      ) |> 
      arrange(row_id)
    
    # set up progress bar
    progress <- 0
    pb <- txtProgressBar(min = progress,
                         max = max(unique(value_data$batch)) + 1,
                         initial = 0,
                         style = 3)
    
    # batch upload
    for (batch_n in unique(value_data$batch)) {
      
      #batch_n <- unique(value_data$batch)[1]
      
      batch_vals <- value_data |> 
        filter(batch == batch_n) |> 
        pull(val) %>%
        paste0(., collapse = ", \n")
      
      
      # upload
      conn$run(
        glue(
          "INSERT INTO {catalog}.{schema}.{table_name}
                VALUES
                {batch_vals}
                "
        )
      )
      
      # update progress bar
      progress <- progress + 1
      setTxtProgressBar(pb, progress)
      
    }
    
    # close the progress bar
    close(pb)
  }
  
}

