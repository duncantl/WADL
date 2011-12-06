
# Find the resources that have the same method in the wadl

getWADLAliases =
function(wadl)
{
  wadl = as(wadl, "WADL")
  defs = xpathSApply(wadl@ref, '//x:method[@id]', xmlGetAttr, "id", namespaces = "x")
  refs = gsub("^#", "", xpathSApply(wadl@ref, '//x:method[@href]', xmlGetAttr, "href", namespaces = "x"))
  refNames = xpathSApply(wadl@ref, '//x:method[@href]/parent::x:resource[@path]', xmlGetAttr, "path", namespaces = "x")
  names(refs) = refNames
  list(defs = defs, refs = refs)
}

if(FALSE) {
w = wadl("WADL/GeneQuestions.wadl")  
a = getWADLAliases(w)  
table(sapply(split(names(a$refs), a$refs), length))
}

GenericParameterDocumentation =
list(.convert = "a logical value or a function that controls how the result of the method is returned. If this is a function, the character string or raw vector is passed to this function and it converts it appropriately. If this is a logical value and \\code{TRUE}, then we attempt to convert the result based on its Content-Type returned by the Web server.  If this is \\code{FALSE}, the value from the Web server is returned as is.",
     .url = "the URL for the Web request. This defaults to the correct value, but can be specified by the caller if the method is available at a different URL, e.g. locally or in a mirror server.")  

makeWADLDocs =
  # Do all of the different real methods, creating the aliases for those that point to the same thing.
function(wadl, aliases = getWADLAliases(wadl), ..., makeDoc = makeWADLDoc,
          .genericParams = GenericParameterDocumentation)
{
  wadl = as(wadl, "WADL")
  als = data.frame(id = aliases$refs, alias = names(aliases$refs))
  ans = by(als, als$id,
             function(info) {
               node = getNodeSet(wadl@ref, sprintf("//x:method[@id = '%s']", info[1, "id"]), "x")[[1]]
               makeDoc(node, info$alias, ..., .genericParams = .genericParams)
             })

  structure(ans, class = "WADLDocumentationCollection")
}


makeWADLDoc =
function(node, aliases = character(), author = "", seeAlso = character(), keywords = character(), ...,
          .genericParams = GenericParameterDocumentation)
{
  # Need to convert HTML to text.
  structure(
    list(name = xmlGetAttr(node, 'id'),
       alias = aliases,
       title = xpathSApply(node, "./x:doc[@title='summary']", xmlValue, namespaces = "x"),
       description = xpathSApply(node, "./x:doc[@title='description']", xmlValue, namespaces = "x"),
       arguments = getParametersDoc(node, .genericParams),
       value = getValueDoc(node),
       author = author,
       seealso = seeAlso,
       keyword = keywords
      ), class = "WADLDocumentation")
}


getValueDoc =
function(node)
{
   d = getNodeSet(node, "./x:response/x:doc", "x")
   if(length(d))
      xmlValue(d[[1]])
   else {
      d = getNodeSet(node, "./x:response/x:representation", "x")
      paste(sapply(d, xmlGetAttr, "mediaType"), collapse = "\n")
   }
}

getParametersDoc  =
function(node, .genericParams = GenericParameterDocumentation, ...)
{
  parmNodes = getNodeSet(node, "./x:request/x:param", "x")
  ans = structure(lapply(parmNodes, getParamDoc), names = sapply(parmNodes, xmlGetAttr, "name"))
  if(length(.genericParams))
      ans[names(.genericParams)] = .genericParams
  ans
}

getParamDoc =
function(node)
{
  paste(xpathSApply(node, "./x:doc[@title = 'help' or @title = 'MultiValued']", xmlValue, namespaces = "x"),
          collapse = "\n")
}

escapeRdCharacters =
function(x)
{
  gsub("%", "\\\\%", x)
}

writeDoc =
  function(obj, to = stdout(), ...)
    UseMethod("writeDoc")

writeDoc.WADLDocumentation =
function(obj, to = stdout())
{
  if(is.character(to)) {
    to = file(to, "w")
    on.exit(close(to))
  }
  
  if(!isOpen(to)) {
    open(to, "w")
    on.exit(close(to))
  }

  obj$alias = paste(sprintf("\\alias{%s}", obj$alias), collapse = "\n")
  obj$keyword = paste(sprintf("\\keyword{%s}", obj$keyword), collapse = "\n")  
  obj$arguments = paste(sprintf("\n\\item{%s}{%s}", names(obj$arguments), obj$arguments), collapse = "")
  
  for(i in names(obj))  {
    if(length(obj[[i]])) {
       if(i == "alias" || i == "keyword")
          cat(obj[[i]], sep = "\n", file = to)
       else
          cat("\\", i, "{", escapeRdCharacters(obj[[i]]), "}\n", sep = "", file = to)
    }
   }
}


writeDoc.WADLDocumentationCollection =
function(obj, to = ".")
{
  rds = sprintf("%s/%s.Rd", to, sapply(obj, function(x) x$name))
  mapply(writeDoc, obj, rds)
  invisible(rds)
}
