runAlignment <- function(
  model = "Moral BY prostit homosex abortion divorce;", 
  group = "country",
  dat = wvs.s, 
  categorical = NULL,
  sim.samples = c(100, 500, 1000), # can be NULL to avoid simulations
  sim.reps = 500,
  Mplus_com = "Mplus",
  path = getwd(),
  summaries = FALSE
) {
  
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
  for(i in colnames(d)) d[,i] <- unclass(d[,i])
  rm(i)
  
  if(!is.numeric(d[,group])) {
    #d[,group] <- gsub(" ", "_", as.character( d[,group] )  )
    message("The group variable must be numeric!")
    
  }
  
  #require(MplusAutomation)
  #inp <- capture.output(prepareMplusData(d,  "mplus_temp.tab"))
  
  write.table(d, "mplus_temp.tab", quote=F, sep="\t", row.names=F, col.names=F, na=".")
  
  #var.list <- gsub("\\.", "_", var.list)
  
  list.of.groups = unique(as.matrix(d[,1]))
  ngroups = length(list.of.groups)
  
  inp <- c("DATA:","\n",
           "   file = 'mplus_temp.tab';", "\n",
           " VARIABLE:", "\n",
           "   names =", gsub("\\.", "_", group), " ", paste(gsub("\\.", "_", var.list), collapse=" "), ";\n",
           "   missing = .;", "\n",
           ifelse(any(is.null(categorical)),
                  "\n",
                  paste("   categorical = ", paste(categorical, collapse = " "), ";\n")
           ),
           
           "   classes = c(", ngroups, ");\n",
           "   knownclass = c(", paste0(gsub("\\.", "_", group), " = ", list.of.groups, " \n    ", collapse=""),
           
           ");\n\n",
           
           "ANALYSIS:\n",
           "  type = mixture;\n",
           "  estimator = ml;\n",
           "  alignment =", kind = "", ";\n", 
           ifelse(any(is.null(categorical)),
                  "\n",  
                  "  algorithm = integration;\n\n"),
           
           "MODEL:\n",
           "  %OVERALL%\n",
           model, 
           "\n\n",
           
           "OUTPUT: align tech8 SVALUES;", 
           "\n\n",
           
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
    
    if(!any(is.null(categorical))) {
      g1 <- sub(".*%g#1% *(.*?) *%g#2%.*", "\\1", stValues)
      g1 <- strsplit(g1, "\n")[[1]]
      g1 <- g1[grep("\\[", g1)]
      g1 <- g1[grep("\\$", g1)]
      g1 <- sapply(g1 , function(x)   sub(" *\\[ *(.*?) *\\$.*", "\\1", x))
      gen.cat <- paste0(names(table(g1)), " (", table(g1), ")")
    }
    
    
    
    
    for(x in sim.samples) { 
      code <- c("MONTECARLO:",
                " NAMES = ", paste(gsub("\\.", "_", var.list), collapse = " "), ";\n",
                " ngroups = ", ngroups, ";\n", 
                " NOBSERVATIONS =", ngroups, "(", x, ");\n", 
                " NREPS =", sim.reps, ";\n\n",
                ifelse(any(is.null(categorical)),
                       "\n",  
                       paste(
                         " CATEGORICAL =", paste(categorical, collapse = " "), ";\n", 
                         " GENERATE = ", paste(gen.cat, collapse = " "),
                         ";\n\n"  )),
                
                
                
                "ANALYSIS:",
                " TYPE = MIXTURE;",
                " ESTIMATOR = ml;",
                " alignment = fixed;\n",
                ifelse(any(is.null(categorical)),
                       "\n",  
                       " algorithm = integration;\n\n"),
                
                "MODEL POPULATION:",
                " %OVERALL%\n",
                paste(stValues, collapse="\n"),
                "\nMODEL:",
                " %OVERALL%\n",
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
      
      otpt <- list(fixed= extractAlignment("fixed.out", silent = TRUE),
                   free = extractAlignment("free.out", silent = TRUE),
                   simulations = extractAlignmentSim(sapply(sim.samples, function(x) paste0("sim", x, ".out")), silent = TRUE)
      )
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Free alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="")
      print(otpt$free$summary)
      
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Fixed alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="") 
      print(otpt$fixed$summary)
      
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of simulations", rep("⎯", getOption("width", 80)-20),  "\n", sep="") 
      print(otpt$simulations)
      
      
      
    } else {
      otpt <- list(fixed = extractAlignment("fixed.out", silent = TRUE),
                   free =  extractAlignment("free.out", silent = TRUE))
      
      
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Free alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="")
      print(otpt$free$summary)
      
      cat("\n", "⎯⎯⎯⎯⎯⎯⎯⎯⎯ ", "Results of Fixed alignemnt", rep("⎯", getOption("width", 80)-20),  "\n", sep="") 
      print(otpt$fixed$summary)
      
      
    }
    
  } else {
    message("Done running models. Refer to the free.out, fixed.out, ranking.dat and some sim###.out files.\nConsider  using `extractAlignment()` and `extractAlignmentSim()` to extract important parts.")
  }
  
  setwd(oldwd)
  if(summaries) invisible(otpt)
}
