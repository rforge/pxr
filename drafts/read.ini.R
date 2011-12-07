read.pxini <- function(filename, shpPath = 'shp',  encoding = "latin1", ...){
  filename = paste(paste(shpPath,  '/',  sep = ''),
    paste(filename,  '.ini',  sep = ''), sep = '')
  a <- scan(filename, what = "character", sep = "\n",
            quiet = TRUE, fileEncoding = encoding)
  line <- a[grepl("NumMaps=",a, ignore.case=TRUE)]
  nummaps <- as.numeric(unlist(strsplit(line, "NumMaps="))[[2]])
  linesoffields <-  unlist(lapply(1:nummaps, function(x){
    grep(paste("\\[FIELDS",x,"\\]",sep=""),
         a, ignore.case=TRUE)
  }))
  linesoffields <- c(linesoffields, length(a))
  keyfields <- unlist(lapply(1:nummaps, function(i) {
    afield <- a[ linesoffields[i]: linesoffields[i+1]]
    line <- afield[grepl("KeyField=",afield, ignore.case=TRUE)]
    unlist(strsplit(line, "KeyField="))[[2]]
  }))
  filesshp <- unlist(lapply(1:nummaps, function(i) {
    line <- a[grepl(paste(i,"=",sep=""), a,ignore.case=TRUE)]
    unlist(strsplit(line, "Maps\\\\"))[[2]]
  }))
  filesshp <- paste(shpPath,  filesshp,  sep = '/')
  list(nummaps=nummaps, filesshp=filesshp, keyfields=keyfields)
}


## filename <- "spain_ccaapro_img_ind"

## read.pxini(filename=filename)
