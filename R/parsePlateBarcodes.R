
parsePlateBarcodes <- function(plates) {
  platenr = substr(plates,1L,3L)

  # Each plate contains the same set of taerget siRNAs. In all wells of one
  # plate the same query siRNA is pipetted. The query siRNA on the
  # \Sexpr{length(platenr)} plates are grouped in sample (double gene knock
  # down) and negative control to measure the main effects (single knock down
  # effects) of the target genes.

  queryGroup = rep("sample",length(plates))
  queryGroup[grep("N",plates,fixed = TRUE)] = "negControl"

  # The remainder of the plate barcodes contain the targetDesign (CI or CII).

  r = substr(plates,4L,10000L)
  #  print(head(r))

  S = which(queryGroup == "sample")
#  N = 161:168
  split_qn = strsplit(r,split="[QN]")
  
  targetDesign = vapply(split_qn,function(x) { x[1] }, FUN.VALUE = character(1))
  targetDesign[targetDesign == "CI"] = 1L
  targetDesign[targetDesign == "CII"] = 2L
  targetDesign = as.integer(targetDesign)

  # The remainder of the plate barcodes contain the query gene.

  r = vapply(split_qn,function(x) { x[2] }, FUN.VALUE = character(1))
  #  print(head(r))
  
  queryGene = rep("NegControl",length(plates))
  queryGene[S] = substr(r[S],1,2)

  # The remainder of the plate barcodes contain the query design.

  r[S] = substr(r[S],3,100)
  #  print(head(r))
  split_r = strsplit(r[S],split="[R]")

  queryDesign = vapply(split_r,function(x) { x[1] }, FUN.VALUE = character(1))
  queryDesign[queryDesign == "I"] = 1L
  queryDesign[queryDesign == "II"] = 2L
  queryDesign = as.integer(queryDesign)

  # The remainder of the plate barcodes contain the biological replicate.
  replicate = vapply(strsplit(r,split="[R]"),function(x) { x[2] }, FUN.VALUE = character(1))
  replicate[replicate == "I"] = 1L
  replicate[replicate == "II"] = 2L
  replicate = as.integer(replicate)

  # The plate annotation is summarized in a table.
  PlateAnnotation = data.frame(plate = plates,
                               targetDesign = targetDesign,
                               queryGroup = queryGroup,
                               queryGene = queryGene,
                               queryDesign = queryDesign, 
                               replicate = replicate,
                               stringsAsFactors=FALSE)

  PlateAnnotation
}

