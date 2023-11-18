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


matchTemplateFunction =
function(args, funcNames, append = character(), all = FALSE)
{
  cb = sprintf("{%s}", args)
  m = sapply(cb, function(x) grepl(x, funcNames, fixed = TRUE))
  ok = apply(m, 1, all)
  if(!any(ok))
    stop("cannot find a matching function name")
  fnNames = funcNames[ok]

  if(length(append))
     fnNames = grep(sprintf("}/%s$", append), fnNames, value = TRUE)

  if(all)
    fnNames
  else
    fnNames[ which.min(nchar(fnNames)) ]
}


