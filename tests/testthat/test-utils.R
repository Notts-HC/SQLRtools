
# Test util functions
#------------------------------------------------------------------------------#

# Author: Steve Spreadborough
# Date: 2024-09-03

# 1. get_env_var() -------------------------------------------------------------

testthat::test_that("utils - get_env_var", {

  test_var_val <- "SQLRtools_get_env_var_TEST"

  # create test var in renviron
  Sys.setenv(SQLRtools_TEST_VAR = test_var_val)

  # get the variable from Renviron, expecting warning that it's got it from
  # renviron
  testthat::expect_warning(
    renv_var <- get_env_var("SQLRtools_TEST_VAR")
  )

  # create var using keyring
  keyring::key_set_with_value(service = "SQLRtools_TEST_VAR",
                              password = test_var_val)

  # get the variable again, not expecting warning as should come from keyring
  kr_var <- get_env_var("SQLRtools_TEST_VAR")

  # delete the vars
  Sys.unsetenv("SQLRtools_TEST_VAR")
  keyring::key_delete(service = "SQLRtools_TEST_VAR")

  # expect error & vars to match
  testthat::expect_error(get_env_var("SQLRtools_TEST_VAR"))
  testthat::expect_match(renv_var, test_var_val)
  testthat::expect_match(kr_var, test_var_val)

})


