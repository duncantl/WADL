checkValues =
function(value, options, varName = deparse(value), repeating = FALSE)
{
  if(repeating)
    value = strsplit(value, ",")[[1]]

  match.arg(value, options, repeating)
#  if(any(is.na(pmatch(value, options))))
#    stop('unrecognized value(s) for ', varName)
}

convertJSON =
function(text)
{
   fromJSON(text, asText = TRUE)
}
