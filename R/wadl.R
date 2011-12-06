# http://code.w3.org/unicorn

# http://wadl.dev.java.net/2009/02

  # The WADL name space
 # xmlns="http://research.sun.com/wadl/2006/10"
WADLNamespace = c(wadl = "http://wadl.dev.java.net/2009/02")

  # A class for representing a WADL document based on the XMLInternalDocument.
setOldClass(c("XMLInternalDocument", "XMLAbstractDocument", "oldClass"))
setClass("WADL", representation(ref = "XMLInternalDocument", namespace = "character"),
            prototype = prototype(namespace = as.character(NA)))

 # Convert a WADL to the XMLInternalDocument
setAs("WADL", "XMLInternalDocument",
       function(from)
        from@ref)

setAs("character", "WADL",
        function(from) {
          new("WADL", ref = xmlParse(from))
        })

setAs("XMLInternalDocument", "WADL",
        function(from) {
          ans = new("WADL", ref = from)
          computeNamespace(ans)         
          ans
        })

computeNamespace =
function(wadl)
{  
  ns = xmlNamespaceDefinitions(ans@ref, simplify = TRUE)
  
  i = grep("research.sun.com/wadl", ns, fixed = TRUE)
  if(length(i))
    ans@namespace = ns[i]
  ans
}


  # Constructor for creating a WADL object from a file/URL name.
  # We might make this generic and allow an already parsed XMLInternalDocument
wadl = function(file, nampesace = NA) {
  doc = xmlParse(file)
  new("WADL", ref = doc)
}

  # Get the names of the methods in
setMethod("names", "WADL",
           function(x) {
              base = xmlGetAttr(getNodeSet(x@ref, "//x:resources[@base]", "x")[[1]], "base")
              m = xpathSApply(x@ref, "//x:resource", xmlGetAttr, "path", namespaces = "x")
              sprintf("%s%s", base, m)
           })

getMethodName =
  #
  # expects the method node, but this can be the actual definition or a reference to it via a href attribute
  #
function(node)
{
  if(xmlName(node) == "resource")
    node = node[["method"]]
  
  if(! is.null( href <- xmlGetAttr(node, "href")))
     node = getNodeSet(node, sprintf("//x:method[@id = '%s']", gsub("^#", "", href)), "x")[[1]]

  xmlGetAttr(node, "name", NA)
}


wadlMethods =
function(doc, baseURL = xpathSApply(doc@ref, "//x:resources[@base]", xmlGetAttr, "base", namespaces = "x"))
{
  doc = as(doc, "WADL")
  resNodes = getNodeSet(doc@ref, "//x:resource[not(./x:resource)]", "x")
  
  structure(lapply(resNodes, getWADLMethod),
             names = sapply(resNodes, getResourceURL))
}

getResourceURL =
  #  Look up the ancestors to get the resources and the inidividual resource parents and put the
  # path elements and base together to get the name of this resource.
function(node)
{
  els = xpathSApply(node, "./ancestor-or-self::x:resource[@path]", xmlGetAttr, "path", namespaces = "x")
  els = c(xpathSApply(node, "./ancestor::x:resources[@base]", xmlGetAttr, "base", namespaces = "x"), els)

  gsub("[^:]//", "/", paste(els, collapse = "/"))
}

getWADLMethod =
  # given the resource node
function(node, methodNode = node[["method"]])
{
  if(! is.null( href <- xmlGetAttr(methodNode, "href")))
     methodNode = getNodeSet(node, sprintf("//x:method[@id = '%s']", gsub("^#", "", href)), "x")[[1]]

  params = getNodeSet(methodNode, ".//x:request/x:param", "x")
  ids = sapply(params, xmlGetAttr, "name")
  vars = c("name", "type", "required", "default", "repeating")
  tmp= lapply(vars,
               function(var)
                  sapply(params, xmlGetAttr, var, NA))
  ans = structure(as.data.frame(tmp), names = vars)
  ans$required = as.logical(ans$required)
  ans$repeating = as.logical(ans$repeating)

  ans$options = lapply(params, getParamOptions)
  ans
}

getParamOptions =
function(paramNode)
{
  xpathSApply(paramNode, "./x:option", xmlGetAttr, "value", namespaces = "x")
}


readWADL =
  # Not used at present.
function(file, ...)
{
  w = as(file, "WADL")
  rsrc = getNodeSet(w@ref, "//x:resource", "x")
}

makeGsubFun =
function(patterns)
{
  force(patterns)
  
  function(x)
     gsub(patterns[1], patterns[2], x)
}

makeFunctions =
function(wadl, methods = wadlMethods(wadl), eval = FALSE, rewriteURL = function(x) x,
          makeFun = makeFunction, ...)
{
  if(!missing(rewriteURL) && is.character(rewriteURL) && length(rewriteURL) == 2)
      rewriteURL = makeGsubFun(rewriteURL)

  wadl = as(wadl, "WADL")

  ans = mapply(function(id, x)
                   makeFun(x, id, name = basename(id), ...),
               rewriteURL(names(methods)), methods)

  if(!missing(eval)) {
     if(is.logical(eval)) {
        if(!eval)
           return(ans)
        else
           eval = globalenv()
      }
    
     invisible(lapply(ans, function(x) eval(parse(text = paste(x, collapse = "\n")), eval))) # 
  } else
    invisible(ans)
      
}

sQuote =
function(x)
  sprintf("'%s'", x)

makeSignature =
function(paramIds, defaults, extraArgs, name, url)
{
  #XXX Have to handle where we don't have a default.
  # c(sprintf("%s = '%s'", paramIds, defaults), extraArgs)
  i = !is.na(defaults) & defaults != ""
  defaults[i] = sQuote(defaults[i])
  c(structure(sprintf("%s%s%s", paramIds, c("", " = ")[i + 1L], defaults), names = paramIds), extraArgs)
}

makeFunction =
function(params, url, options = params$options, name = "foo", converter = "NULL", action = "getForm",
          OptionsCharacterThreshold = getOption("WADLOptionsCharacterThreshold", 5000L),
          hooks = list(makeSignature = makeSignature, preCall = NULL, postCall = NULL))
{

   ids = sprintf("`%s`", params$name)

   dflt = as.character(params$default)
   dflt[is.na(params$required) | !params$required] = "NULL"

   i = sapply(options, function(x) sum(nchar(x)) > OptionsCharacterThreshold)
   optVars = character()   
   if(any(i)) {
      vars = lapply(params$name[i], function(id) sprintf("`%s.%s.options`", name, id))
      optVars = mapply(function(id, vals)
                        sprintf("%s = \n  %s", id, getOptionsVector(vals)),
                       vars, options[i])
      options[i] = vars
   }


   extraArgs = c("...", .url = sprintf(".url = '%s'", url), .convert = sprintf('.convert = %s', converter))

   dflt = gsub("'", "\\\\'", dflt)
   sig = c("function(", paste(sig <- hooks$makeSignature(ids, dflt, extraArgs, name, url), collapse = ", "), ")")

   missings = character()
   if(any(is.na(params$required) | !params$required)) {
     var = params$name[is.na(params$required) | !params$required]
     missings = sprintf("if(!missing(`%s`))\n       params[['%s']] = `%s`", var, var, var)
   }

   if(any(i <- (sapply(options, length) > 0))) {
      check = unlist(mapply(checkValueCode, as.character(params$name[i]), options[i], params$required[i], params$repeating[i]))
   } else
     check = character()

   var = params$name[params$required]
   setParams = c("params = list(",
                   paste(sprintf("              '%s' = `%s`", var, var), collapse = ",\n    "),
                 "            )")
   
   body = c(
            check,
            setParams,
             "",
             missings,
             "",
             if(length(hooks$preCall)) hooks$preCall(params, url, name),
             sprintf("ans = %s(.url, .params = params, ...)", action),
             if(length(hooks$postCall)) hooks$postCall(params, url, name),     
             "if(!is.null(.convert))",
             "   .convert(ans)",
             "else if(!is.null(type <- attr(ans, 'Content-Type')) && type == 'text/json')",
             "   convertJSON(ans)",
             "else",
             "   ans"
            )

  c(paste(c(sprintf("%s = ", name), sig, "{", sprintf("    %s", body), "}"), collapse = "\n"), optVars)
}

getOptionsVector =
function(values)
{
  con = textConnection('foo', 'w', local = TRUE)
  old.width = getOption("width")
  options(width = 100)
  on.exit({close(con); options(width = old.width)})
  dput(values, con)
  vals = paste(textConnectionValue(con), collapse = "\n                      ")
}
  

checkValueCode =
function(var, values, required, repeating)
{
  vals = if(is.symbol(values))
           values
         else
           getOptionsVector(values)
  
  req = if(!required)
          sprintf("if(!missing(`%s`))", var)
        else
          ""
    
  sprintf("%s `%s` <- checkValues(`%s`, %s, '%s', %s)",
             req, var, var, vals, var, as.character(repeating))
}



XSDTypeMap = c("string" = "character")

xsdTypeToRType =
function(type)
{
  type = gsub("^xsd:", "", type)
  XSDTypeMap[type]
}
