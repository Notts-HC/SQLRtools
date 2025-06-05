test_that("returns an expected password as if it were from keyring", {
  local_mocked_bindings(
    get_env_var = function(var_name) {
      service = "GLOBAL-SETTING"
      username = "keyring_words"
      return(list(var_name = service, username = username))
    }
  )
  
  # Call the function
  result <- SQLRtools::get_env_var(var_name = "GLOBAL-SETTING")
  
  # Check the result
  expect_equal(result$username, "keyring_words")
  expect_equal(result$var_name, "GLOBAL-SETTING")
  
})

test_that("returns message when not keyring is not set up", {
   
  testthat::expect_error(
    get_env_var(var_name = "Something"),
    "variable Something not found using keyring or Sys.getenv()"
  ) 

}) 


test_that("warning if string from Sys.getenv but username is not rstudio-connect", {

  withr::local_envvar(c("USERNAME" = "something-connect"))
  withr::local_envvar(c("GLOBAL_SETTING" = "system_words"))

  expect_warning(
    SQLRtools::get_env_var(var_name = "GLOBAL_SETTING"),
    "found in Renviron file but not windows credentials"
  )
})

test_that("returns string when username is rstudio-connect", {
  
  withr::local_envvar(c("USERNAME" = "rstudio-connect"))
  withr::local_envvar(c("GLOBAL_SETTING" = "system_words"))

  # Check the result
  expect_equal(SQLRtools::get_env_var(var_name = "GLOBAL_SETTING"), "system_words")
  
})


