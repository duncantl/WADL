doc = xmlParse("inst/sampleWADLs/ops.wadl")
rs = getNodeSet(doc, "//x:resources",  c("x" = "http://wadl.dev.java.net/2009/02"))

bases = sapply(rs, xmlGetAttr, "base")

xmlApply(rs[[1]], processResource)

