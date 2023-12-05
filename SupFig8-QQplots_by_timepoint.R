library(tidyverse)
library(readr)

trait_code <- data.frame(name = c('plant.height', 'plant.width','convex.hull', 'PSA'),
                         keys = c('PBPH', 'PBPW','CHV', 'PSA'))

saving_dir <- "C:/Timepoints/QQplots"

#------------ SAP -------------------------------
setwd("C:/Timepoints/SAP")

for (j in c('plant.height', 'plant.width','convex.hull', 'PSA')){

  print(paste0("Working on: ", j))
  pvalues <- read.delim(paste0("GAPIT.Association.GWAS_Results.FarmCPU.SAP.", j, ".5.csv"), sep = ",")[,1:3]
  names(pvalues) <- c("SNP", "Chromosome", "Position")
  
  for (i in dir(pattern = paste0("GAPIT.Association.GWAS_Results.FarmCPU.SAP.", j))){
    
    trait <- trait_code[trait_code$name == j, 2]
    week <- sum(as.numeric(substring(i, nchar(i)-4, nchar(i)-4)), 4)
    
    data <- read.table(paste0(i), head = TRUE, sep = ",")
    data <- data[,c(1,4)]
    
    names(data)[2] <- paste0(trait, "_", week)
    
    pvalues <- left_join(pvalues, data, by ="SNP")
  
    }
  
  current_dir <- getwd()
  setwd(saving_dir)
  
  CMplot(pvalues, plot.type="q",
       col=c("dodgerblue1", "olivedrab3", "darkgoldenrod1", "blue"), #threshold=1e-6,
       ylab.pos=2, #signal.pch=c(19,6,4), 
       signal.cex=1.2, signal.col="red",
       file.name = paste0(trait, "_SAP"),
       conf.int = TRUE, box = F, multraits = TRUE,
       file="jpg", dpi = 300, file.output = T, verbose = TRUE, width = 5, height = 5)

  setwd(current_dir)
  rm(pvalues, data, week, trait)
}

#-----------------------  PSP -------------------------------
setwd("C:/Timepoints/PSP")

for (j in c('PBPH', 'PBPW', 'PSA','CHV')){
  
  print(paste0("Working on: ", j))
  pvalues <- read.delim(paste0("GAPIT.Association.GWAS_Results.FarmCPU.", j, "_6.csv"), sep = ",")[,1:3]
  names(pvalues) <- c("SNP", "Chromosome", "Position")
  
  for (i in dir(pattern = paste0("GAPIT.Association.GWAS_Results.FarmCPU.", j))){
    
    trait <- j
    week <- sum(as.numeric(substring(i, nchar(i)-4, nchar(i)-4)), 4)
    
    data <- read.table(paste0(i), head = TRUE, sep = ",")
    data <- data[,c(1,4)]
    
    names(data)[2] <- paste0(trait, "_", week)
    
    pvalues <- left_join(pvalues, data, by ="SNP")
    
  }
  
  current_dir <- getwd()
  setwd(saving_dir)
  
  CMplot(pvalues, plot.type="q",
         col=c("dodgerblue1", "olivedrab3", "darkgoldenrod1", "blue"), #threshold=1e-6,
         ylab.pos=2, #signal.pch=c(19,6,4), 
         signal.cex=1.2, signal.col="red",
         file.name = paste0(trait, "_", week, "_PSP"),
         conf.int = TRUE, box = F, multraits = TRUE,
         file="jpg", dpi = 300, file.output = T, verbose = TRUE, width = 5, height = 5)
  
  setwd(current_dir)
  rm(pvalues, data, week, trait)
}

#-----------------------  PSP+SAP -------------------------------
setwd("C:/Timepoints/PSP-SAP")

for (j in c('PBPH', 'PBPW', 'PSA','CHV')){
  
  print(paste0("Working on: ", j))
  pvalues <- read.delim(paste0("GAPIT.Association.GWAS_Results.FarmCPU.", j, "_6.csv"), sep = ",")[,1:3]
  names(pvalues) <- c("SNP", "Chromosome", "Position")
  
  for (i in dir(pattern = paste0("GAPIT.Association.GWAS_Results.FarmCPU.", j))){
    
    trait <- j
    week <- sum(as.numeric(substring(i, nchar(i)-4, nchar(i)-4)), 4)
    
    data <- read.table(paste0(i), head = TRUE, sep = ",")
    data <- data[,c(1,4)]
    
    names(data)[2] <- paste0(trait, "_", week)
    
    pvalues <- left_join(pvalues, data, by ="SNP")
    
  }
  
  current_dir <- getwd()
  setwd(saving_dir)
  
  CMplot(pvalues, plot.type="q",
         col=c("dodgerblue1", "olivedrab3", "darkgoldenrod1", "blue"), #threshold=1e-6,
         ylab.pos=2, #signal.pch=c(19,6,4), 
         signal.cex=1.2, signal.col="red",
         file.name = paste0(trait, "_", week, "_PSP+SAP"),
         conf.int = TRUE, box = F, multraits = TRUE,
         file="jpg", dpi = 300, file.output = T, verbose = TRUE, width = 5, height = 5)
  
  setwd(current_dir)
  rm(pvalues, data, week, trait)
}
