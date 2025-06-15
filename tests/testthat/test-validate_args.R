test_that("validate_arg handles missing arguments correctly", {
  # Error if missing and required
  expect_error(
    validate_arg("test_arg", required = TRUE),
    "'test_arg' is required"
  )
  
  # No error if missing but NOT required
  expect_silent(
    validate_arg("test_arg", required = FALSE)
  )
})

test_that("validate_arg handles type checking correctly", {
  # Error if wrong type
  expect_error(
    validate_arg("test_arg", "string", type = "numeric"),
    "'test_arg' must be of type numeric"
  )
  
  # No error if correct type
  expect_silent(
    validate_arg("test_arg", 1, type = "numeric")
  )
  
  # Works with multiple allowed types
  expect_silent(
    validate_arg("test_arg", 1, type = c("numeric", "integer"))
  )
  
  expect_silent(
    validate_arg("test_arg", 1L, type = c("numeric", "integer"))
  )
})

test_that("validate_arg handles single additional check correctly", {
  # Error if fails single additional check
  expect_error(
    validate_arg("test_arg", 0, 
                additional_checks = function(x) if(x <= 0) stop("must be positive")),
    "'test_arg': must be positive"
  )
  
  # No error if passes single additional check
  expect_silent(
    validate_arg("test_arg", 1, 
                additional_checks = function(x) if(x <= 0) stop("must be positive"))
  )
})

test_that("validate_arg handles list of additional checks correctly", {
  # Error if fails any check in list
  expect_error(
    validate_arg("test_arg", 0, 
                additional_checks = list(
                  function(x) if(x <= 0) stop("must be positive"),
                  function(x) if(x >= 10) stop("must be less than 10")
                )),
    "'test_arg': must be positive"
  )
  
  expect_error(
    validate_arg("test_arg", 11, 
                additional_checks = list(
                  function(x) if(x <= 0) stop("must be positive"),
                  function(x) if(x >= 10) stop("must be less than 10")
                )),
    "'test_arg': must be less than 10"
  )
  
  # No error if passes all checks
  expect_silent(
    validate_arg("test_arg", 5, 
                additional_checks = list(
                  function(x) if(x <= 0) stop("must be positive"),
                  function(x) if(x >= 10) stop("must be less than 10")
                ))
  )
})

test_that("validate_arg handles complex validation scenarios", {
  # Test with multiple conditions
  expect_error(
    validate_arg("test_arg", "abc", 
                type = "character",
                additional_checks = list(
                  function(x) if(nchar(x) < 4) stop("must be at least 4 characters"),
                  function(x) if(!grepl("^[A-Z]", x)) stop("must start with uppercase")
                )),
    "'test_arg': must be at least 4 characters"
  )
  
  expect_error(
    validate_arg("test_arg", "abcd", 
                type = "character",
                additional_checks = list(
                  function(x) if(nchar(x) < 4) stop("must be at least 4 characters"),
                  function(x) if(!grepl("^[A-Z]", x)) stop("must start with uppercase")
                )),
    "'test_arg': must start with uppercase"
  )
  
  # No error if passes all conditions
  expect_silent(
    validate_arg("test_arg", "Abcd", 
                type = "character",
                additional_checks = list(
                  function(x) if(nchar(x) < 4) stop("must be at least 4 characters"),
                  function(x) if(!grepl("^[A-Z]", x)) stop("must start with uppercase")
                ))
  )
})

# Tests for validate_args
test_that("validate_args handles input validation correctly", {
  # Error if args is not a named list
  expect_error(
    validate_args(list(1, 2)),
    "args must be a named list"
  )
  
  expect_error(
    validate_args(list(a = 1, 2)),
    "args must be a named list"
  )
})

test_that("validate_args applies validation to all arguments", {
  # Error if any argument fails validation
  expect_error(
    validate_args(
      args = list(
        arg1 = "string",
        arg2 = "string"
      ),
      type = "numeric"
    ),
    "'arg1' must be of type numeric"
  )
  
  # No error if all arguments pass validation
  expect_silent(
    validate_args(
      args = list(
        arg1 = 1,
        arg2 = 2
      ),
      type = "numeric"
    )
  )
})

test_that("validate_args handles additional checks correctly", {
  # Error if any argument fails additional checks
  expect_error(
    validate_args(
      args = list(
        arg1 = 0,
        arg2 = 1
      ),
      type = "numeric",
      additional_checks = function(x) if(x <= 0) stop("must be positive")
    ),
    "'arg1': must be positive"
  )
  
  # No error if all arguments pass additional checks
  expect_silent(
    validate_args(
      args = list(
        arg1 = 1,
        arg2 = 2
      ),
      type = "numeric",
      additional_checks = function(x) if(x <= 0) stop("must be positive")
    )
  )
})

test_that("validate_args handles complex validation scenarios", {
  # Test with multiple conditions
  expect_error(
    validate_args(
      args = list(
        arg1 = "abc",
        arg2 = "def"
      ),
      type = "character",
      additional_checks = list(
        function(x) if(nchar(x) < 4) stop("must be at least 4 characters"),
        function(x) if(!grepl("^[A-Z]", x)) stop("must start with uppercase")
      )
    ),
    "'arg1': must be at least 4 characters"
  )
  
  # No error if all arguments pass all conditions
  expect_silent(
    validate_args(
      args = list(
        arg1 = "Abcd",
        arg2 = "Efgh"
      ),
      type = "character",
      additional_checks = list(
        function(x) if(nchar(x) < 4) stop("must be at least 4 characters"),
        function(x) if(!grepl("^[A-Z]", x)) stop("must start with uppercase")
      )
    )
  )
}) 