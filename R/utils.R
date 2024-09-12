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
#' @import keyring
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
