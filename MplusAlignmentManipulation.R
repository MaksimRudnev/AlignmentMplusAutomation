runAlignment <- function(
  model = "Moral BY prostit homosex abortion divorce;", 
  group = "country",
  dat = wvs.s, 
  sim.samples = c(100, 500, 1000), # can be NULL to avoid simulations
  sim.reps = 500,
  Mplus_com = "Mplus",
  path = getwd(),
  summaries = FALSE) {
  oldwd <- getwd()
  setwd(path)
  
  message("Create input for free alignment.\n")
  
  var.list <- strsplit(model, ";|\n") [[1]]
  var.list <-   var.list[!var.list==""]
  var.list <-   unlist(strsplit(var.list, "(?i)(by)", perl=TRUE))
  var.list <-   unlist(strsplit(var.list[seq(2, length(var.list), by=2)], " "))
  var.list <- paste(unique(unlist(var.list)), collapse=" ")
  var.list <- strsplit(var.list, " ")[[1]]
  var.list <-   var.list[!var.list==""]
  
  # var.list <- paste0("; ", model, " ;")
  # var.list<- gsub("\n", ";", var.list)
  # var.list <- paste(sapply(var.list, function(i) sub(".*BY *(.*?) *;.*", "\\1", i)), collapse=" ")
  # var.list <- strsplit(gsub(" BY | +|;", " ", var.list), " ")[[1]]
  # var.list <- var.list[!var.list ==""]
  
  d <- dat[c(group, var.list)]
  d <- as.data.frame(unclass(d))
  
  if(!is.numeric(d[group])) {
  #d[,group] <- gsub(" ", "_", as.character( d[,group] )  )
    warning("The group variable must be numeric!")
  
  }

  #require(MplusAutomation)
  #inp <- capture.output(prepareMplusData(d,  "mplus_temp.tab"))
  
  write.table(d, "mplus_temp.tab", quote=F, sep="\t", row.names=F, col.names=F, na=".")
  
  
  list.of.groups = unique(as.matrix(d[,1]))

  
  inp <- c("DATA:","\n",
           "   FILE = 'mplus_temp.tab';", "\n",
           " VARIABLE:", "\n",
           "   NAMES =", group, " ", paste(var.list, collapse=" "), ";\n",
           "   MISSING=.;", "\n",
           
           "classes = c(", length(list.of.groups), ");\n",
           "knownclass = c(", #group, " = ", 
           
          # if(is.numeric(dat[,group])) {
             paste0(group, " = ", list.of.groups, " \n", collapse="") 
          # } else {
          #   paste0(group, " = '", list.of.groups, "' \n", collapse="")
          # }
          ,
           
           ");\n",
           
            "ANALYSIS:\n",
            "  type = mixture;\n",
            "  estimator = ml;\n",
            "  alignment =", kind = "", ";\n", 
           
            "MODEL:\n",
            "  %OVERALL%\n",
            model, "\n",
            "OUTPUT: align tech8 SVALUES;", "\n",
            "SAVEDATA: ", "\n",
            "  RANKING = ranking.dat; "
           
  )
  
  
  
  inp["kind"]<-"FREE"
  cat(inp, file = "free.inp", sep="")
  message("Run free in Mplus.")
  trash <- system(paste(Mplus_com, "free.inp"))
  
  
  outFree <- paste(readLines("free.out"), collapse = "\n") 
  if(grepl("TO AVOID MISSPECIFICATION USE THE GROUP WITH VALUE", outFree)) {
  refGroup <- sub(".*TO AVOID MISSPECIFICATION USE THE GROUP WITH VALUE *(.*?) *AS THE BASELINE GROUP.*", "\\1", outFree)
  } else {
     
      free.tab.means <- sub(".*FACTOR MEAN COMPARISON AT THE 5% SIGNIFICANCE LEVEL IN DESCENDING ORDER *(.*?) *QUALITY OF NUMERICAL RESULTS.*", "\\1", outFree)
      refGroup <- as.character(read.table(text=sub(".*\n *(.*?) *\n\n\n\n\n.*", "\\1", free.tab.means))[3])
      
    
  }
  
  inp["kind"]<-paste0("FIXED(", refGroup, ")")
  cat(inp, file = "fixed.inp", sep="")
  message("Run fixed in Mplus.")
  trash <- system(paste(Mplus_com, "fixed.inp"))
  
  # Creating simulations
  if(!is.null(sim.samples)) {
  
  outFixed <- paste(readLines("fixed.out"), collapse = "\n") 
  
 
  stValues <- sub(".*MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES *(.*?) *\n\n\n\n.*", "\\1", outFixed)
  stValues <- gsub("%C#", "%g#", stValues)
  
  stValues <- gsub("c#", "g#", stValues)
  
  corrupt.code <- sub(".*%OVERALL% *(.*?) *%g#1%.*", "\\1", stValues)
  correction <-strsplit(corrupt.code, "\n")[[1]]
  correction <- correction[grep(" BY ",  correction)]
  correction <- gsub(";", "*1;", correction)
  
  stValues <- paste(paste(correction, collapse="\n"), "\n", substr(stValues, regexpr("%g#1%", stValues), nchar(stValues)))
  
  for(x in sim.samples) { 
    code <- c("  MONTECARLO:
          NAMES = ", paste(var.list, collapse = " "), ";\n",
              "ngroups = ", ngroups, ";\n", 
              "NOBSERVATIONS =", ngroups, "(", x, ");\n", 
              "NREPS =", sim.reps, ";\n\n",
              "ANALYSIS:
            TYPE = MIXTURE;
          ESTIMATOR = ml;
          alignment = fixed;\n",
              
              "MODEL POPULATION:
              %OVERALL%\n",
              paste(stValues, collapse="\n"),
              "\nMODEL:
              %OVERALL%\n",
              paste(stValues, collapse="\n")
    )
    cat(code, sep="", file = paste0("sim", x , ".inp"))
  }
  
  for (x in sim.samples) {
    message("Run simulation", x, "in Mplus.\n")
    trash <- system(paste(Mplus_com, paste0("sim", x, ".inp")))
    
  }
  }
  
  # Return summaries
  
  if(summaries) {
    
    if(!is.null(sim.samples)) {
    list(extractAlignment("fixed.out"),
         extractAlignment("free.out"),
         extractAlignmentSim(sapply(sim.samples, paste0("sim", x, ".inp")))
         ) 
    } else {
    list(extractAlignment("fixed.out"),
         extractAlignment("free.out"))
    }
    
  } else {
    message("Done running models. Refer to the free.out, fixed.out, ranking.dat and some sim###.out files.\nConsider  using `extractAlignment()` and `extractAlignmentSim()` to extract important parts.")
  }
  
  setwd(oldwd)
  
}






extractAlignment <- function(file = "alignment.out", silent = FALSE) {
  

  # Basic extraction function  
  extractBetween <- function(begin, end, string) {
    mapply(function(a, b) substr(string, a, b),
           gregexpr(begin, string)[[1]]+nchar(begin),
           gregexpr(end, string)[[1]]-1
    )  
  }
  
  
  
  # Read file
  b.string<-  paste(readLines(file), collapse="\n")
  
  
  # Extract pairwise comparisons ########
  
  align.outp <- extractBetween("ALIGNMENT OUTPUT", "Average Invariance index", b.string)
  align.outp <-strsplit(align.outp, "Loadings\n")[[1]]
  align.outp <- sub("\n\nINVARIANCE ANALYSIS\n\n Intercepts/Thresholds\n ", " ", align.outp)
  
  al.pw.i <- strsplit(align.outp[1], "Intercept for ")[[1]]
  al.pw.l <- unlist(strsplit(align.outp[2:length(align.outp)], "Loadings for "))
  
  # this is reqired to name the fit contribution
  loading.names.by.factor <- {
    a <- strsplit(align.outp[2:length(align.outp)], "Loadings for ")
    lapply(a, function(y) {
      m <- sapply(y, function(b) substr( b, 1, regexpr("\n", b)-1))
      m <- m[!m==""]
      unname(m)
    })
  }
  
  
  al.pw.i <-al.pw.i[!al.pw.i %in% c(" ",NA,"")]
  al.pw.l <-al.pw.l[!al.pw.l %in% c(" ",NA,"")]
  
  
  al.pw <- c(paste("Intercepts", al.pw.i), paste("Loadings", al.pw.l))
  al.pw.names <- sapply(1:length(al.pw), function(b) substr( al.pw[b], 1, regexpr("\n", al.pw[b])-1))
  
  al.pw.list <-strsplit(al.pw, " Approximate Measurement Invariance Holds For Groups:")
  names(al.pw.list)<- al.pw.names
  
  align.outp <- lapply(al.pw.list, function(x) { 
    
    pairwise.tab <- read.table(text=x[1], stringsAsFactors = FALSE, skip=2)
    colnames(pairwise.tab) <- c("Group1", "Group2", "Est_in_G1", "Est_in_G2", "Difference", "SE", "P_value" )
    
    invariant.groups <- substr(x[2], 2, regexpr("Weighted Average Value Across Invariant Groups:",x[2])-1)
    invariant.groups <- unlist(strsplit(readLines(textConnection(invariant.groups)), " "))
    invariant.groups <- invariant.groups[!invariant.groups==""]
    
    all.groups <- unique(unlist(pairwise.tab[,1:2]))
    non.invariant.groups <- all.groups[!all.groups %in% invariant.groups]
    
    
    AlignedPar = as.numeric(sub(".*Weighted Average Value Across Invariant Groups: *(.*?) *\n.*", "\\1", x[2]))
    R2 = as.numeric(sub(".*R-square/Explained variance/Invariance index: *(.*?) *\n.*", "\\1", x[2]))
    
    inv.comparison <- substr(x[2], regexpr("Invariant Group Values, Difference to Average and Significance", x[2]), nchar(x[2]))
    inv.comparison <- read.table(text = inv.comparison, skip=2)
    colnames(inv.comparison) <- c("Group","Value", "Difference","SE","P.value")
    
    
    list(
      "Pairwise comparison" = pairwise.tab,
      "Aligned parameter" = AlignedPar,
      "R2" = R2,
      "Comparison of aligned" = inv.comparison,
      "Invariant groups" = invariant.groups,
      "Non-invariant groups" = non.invariant.groups
    )
    
  })
  output <- list()
  output[["alignment.output"]] <- align.outp
  
  # Summaries
  
  
  # invariant.groups.pars.tab
  
  all.groups <- c(align.outp[[1]]$`Invariant groups`, align.outp[[1]]$`Non-invariant groups`)
  non.invariant.groups.pars.tab <- sapply(align.outp, function(x) { 
    
    temp<-rep("", length(all.groups))
    temp[!all.groups %in% x[["Invariant groups"]]]<-"X"
    temp
    })
  rownames(non.invariant.groups.pars.tab)<-all.groups
  
  output[["non.invariant.pars"]] <- non.invariant.groups.pars.tab
 # 
 #  knitr::kable(non.invariant.groups.pars.tab, format = "html") %>%
 #  kableExtra::kable_styling(., bootstrap_options=c("striped", "bordered"), position = "left", font_size = 12)

  
 # Extract summaries: R2, aligned parameters, list of invariant and non-invariant groups #####
 
 summ <- 
 t(sapply(align.outp, function(x)  c(AlignedParameter = x[["Aligned parameter"]],
                                   R2 = x[["R2"]], 
                                   invariant.gr = paste(x[["Invariant groups"]], collapse=" "), 
                                   non.invar.gr = paste(x[["Non-invariant groups"]], collapse = " "))))
 
 

  output[["summary"]] <- summ
  
  
  # Fit contribution ######
  if(grepl("TECHNICAL 8 OUTPUT", b.string))  {
    
   tech8 <-  substr(b.string, regexpr("TECHNICAL 8 OUTPUT", b.string), nchar(b.string))
   tech8 <-  sub("TECHNICAL 8 OUTPUT\n\n\n", "", tech8)
   tech8 <-  substr(tech8, 1, regexpr("(\n\n\n)", tech8))
   tech8.align <- strsplit(tech8, "ALIGNMENT RESULTS FOR ")[[1]][-1]
   
   
  fit.contrib <- lapply(1:length(tech8.align), function(x) {
  
   f.contrib.l <-  sub(".* Fit Function Loadings Contribution By Variable *(.*?) *Fit Function Loadings Contribution By Group.*", "\\1", tech8.align[[x]])
   f.contrib.i <-  sub(".* Fit Function Intercepts Contribution By Variable *(.*?) *Fit Function Intercepts Contribution By Group.*", "\\1", tech8.align[[x]])
     
   contrib <- c(unlist(read.table(text=f.contrib.i)), unlist(read.table(text=f.contrib.l)))
  
   names(contrib) <- c( paste("Intercepts", loading.names.by.factor[[x]]), paste("Loadings", loading.names.by.factor[[x]]) )
   contrib
   })
  
  
  f.names <- sapply(tech8.align, function(x) substr(x, 1, regexpr("\n", x)-1))
  
  
  output$summary <- merge(summ, data.frame(Fit.contribution = unlist(fit.contrib),
                        Factor = rep(unname(f.names), lengths(fit.contrib)), stringsAsFactors = FALSE), by = "row.names")
  output$summary <- output$summary[order(output$summary$Factor),]
}
  
  
  # Extract mean comparison ######
  
  mean.comparison <- extractBetween("FACTOR MEAN COMPARISON AT THE 5% SIGNIFICANCE LEVEL IN DESCENDING ORDER", "\n\n\n\n\n", b.string)
  mean.comparison<-mean.comparison[!mean.comparison==""]
  mean.comparison<- strsplit(mean.comparison,"(Results for Factor)")[[1]][-1]
  
  mean.comparison<- gsub("\n\n$", "", mean.comparison)
  
  names(mean.comparison) <- sapply(mean.comparison, function(x)  substr(x, 2, regexpr("\n", x)-1))
  
  mean.comp <- lapply(mean.comparison, function(x) {
    read.fwf(file=textConnection(x), skip=4, widths = c(7, 10, 10, 12, 1000),
             col.names = c("Ranking", "Latent class", "Group value", "Factor mean", "Groups With Significantly Smaller Factor Mean")) 
    
  })
  
  output[["mean.comparison"]] <- mean.comp
  
  
  
  
  # Extract ranking table #####
  if(grepl("Factor Mean Ranking Tables", b.string)) {
    
    rankingFile <- sub(".*Factor Mean Ranking Tables *(.*?) *Save format.*", "\\1", b.string)
    rankingFile <- gsub("Save file|\n| ", "", rankingFile)
    ranking.tabs <- paste(readLines(paste0(substr(file, 1, regexpr("/.*$", file)), rankingFile)), collapse="\n")
    ranking.tabs <- strsplit(ranking.tabs, "Ranking table for ")[[1]][-1]
    names(ranking.tabs)<- sapply(ranking.tabs, function(x) substr(x, 1, regexpr("\n", x)-1))
    ranking.tab <- lapply(ranking.tabs, function(x) {
      rt <- read.csv(text=x, skip=2, row.names = 1)
      colnames(rt)<- rownames(rt)
      rt[,-ncol(rt)]
    })
    
    
    output[["ranking.table"]] <-ranking.tab
  }
  
  
  if(!silent) print(output$summary, row.names=FALSE)
  invisible(output)
}

                                 
                                 
                                 
                                 
extractAlignmentSim <- function(sim.outputs = c("sim500.out", "sim100.out", "sim1000.out"), silent = FALSE) {
  
  
otp<- lapply(sim.outputs, function(x) {
    f <- paste(readLines(x), collapse="\n")
    cor.tabs <- sub(".*CORRELATIONS AND MEAN SQUARE ERROR OF POPULATION AND ESTIMATE VALUES *(.*?) *QUALITY OF NUMERICAL RESULTS.*", "\\1", f)
    cor.tabs <- strsplit(cor.tabs, "CORRELATION AND MEAN SQUARE ERROR OF THE AVERAGE ESTIMATES")[[1]]
    cor.tabs1 <- strsplit(cor.tabs[1], "\n")[[1]][-c(1:4)]
    cor.tabs1 <- cor.tabs1[!cor.tabs1==""]

   dt1 <-sapply(seq(1, length(cor.tabs1), by=3), function(f.id) unlist(read.table(text=cor.tabs1[f.id+1:2])[-1]))
   colnames(dt1) = gsub(" ", "", cor.tabs1[seq(1, length(cor.tabs1), by=3)])
   row.names(dt1)<-c("Correlations Mean Average", "Correlations Variance Average", "Correlations Mean SD", "Correlations Variance SD",
                     "MSE Mean Average", "MSE Variance Average", "MSE Mean SD", "MSE Variance SD")
   
   
   
   
   
   cor.tabs2 <- strsplit(cor.tabs[2], "\n")[[1]]
   cor.tabs2 <- cor.tabs2[!cor.tabs2==""]
   dt2 <- read.table(text=gsub(".*(Mean|Variance) *", "", cor.tabs2))
   dt2 <- sapply(seq(1, nrow(dt2), by=2), function(y) unlist(dt2[y:(y+1),]) )
   colnames(dt2) <- gsub(" *", "", gsub("(Mean|Variance).* *", "",   cor.tabs2)) [seq(1, length(cor.tabs2), by=2)]
   rownames(dt2) <- c("Correlation of average means with true", "Correlation of average variances with true",
                      "MSE of average means with true","MSE of average variances with true")
   
   list('Correlations and mean square error of population and estimate values' = dt1,
        'Correlation and mean square error of the average estimates' = dt2)

  
})

names(otp)<- sim.outputs
                
if(!silent) { 

for(i in colnames(otp[[3]][[2]])  ) {
  cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", i," ", rep("⎯", getOption("width", 80)-nchar(i)-2),  "\n", sep="") 
  print(sapply(otp, function(x) x[[1]][,i] ), digits = 2)
  print(sapply(otp, function(x) x[[2]][,i] ), digits = 2)
 }
}

invisible(otp)
   
}