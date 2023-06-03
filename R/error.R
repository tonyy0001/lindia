# This file contains exception checking and error handling functions
#
# exception handling function for malformed input in lindia
#
handle_exception <- function(input, function_name) {
  
  type = class(input)
  
  # exception handling: input not lm object
  if (type != "lm") {
    function_name |> 
      paste("doesn't know how to handle non-lm object") |> 
      stop()
  }
}
