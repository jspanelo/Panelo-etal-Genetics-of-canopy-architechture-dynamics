library(tidyverse)
library(readr)
library(CMplot)

# Directories
setwd("C:/Parameters/GWAS_Results")

saving_dir <- "C:/Parameters/QQplots"

# Global variables
analysis <- c("Dw1", "Dw1+Dw3", "Dw3", "No Covariate")
param <- c('Intercept', 'Slope')
descript <- c('PBPH', 'PBPW','CHV', 'PSA')
pop <- c('Comb', 'PSP', 'SAP')

#----------- Loop for QQ plots ---------------

# Change working directory to each Populations
for (p in pop) {
  setwd(paste0("./", p))

  # Change working directory to each analysis (Covariates)
  for (i in analysis){
    
    setwd(paste0("./", i)) # change wd
    f_results <- grep("Results", dir(), value = T) # make a vector of "results" files in the wd 
    
    # Look at both parameters
    for (j in param) {
      
      # filter the vector to get file names conintaingn the parameter we need
      results_params <- grep(j, f_results, value = T) 
      
      # call the first file in the list of file names for the given parameter. This is only to get SNP, Chr and Pos info
      pval <- read.table(results_params[1], head = TRUE, sep = ",")[,1:3]
      names(pval) <- c("SNP", "Chromosome", "Position") # rename the dataframe (just in case)
      
      # Loop into each descriptor
      for (k in descript) {
        
        # filter the filenames that contain the descriptor we need  
        file_name <- grep(k, results_params, value = T)
        results_params_trait <- read.table(file_name, head = TRUE, sep = ",")[,c(1,4)] # open the file, and get SNP and p-value
        
        names(results_params_trait)[2] <- paste(k, j, i) # rename the pvalue column to 
        
        pval <- left_join(pval, results_params_trait)
      }
      
      current_dir <- getwd()
      setwd(saving_dir)
      
      CMplot(pval, plot.type="q",
             col=c("dodgerblue1", "olivedrab3", "darkgoldenrod1", "blue"), #threshold=1e-6,
             ylab.pos=2, #signal.pch=c(19,6,4),
             signal.cex=1.2, signal.col="red",
             conf.int = TRUE, box = F, multraits=TRUE, 
             file.name = paste0(i, "_", j, "_", p),
             file="jpg", dpi = 300, file.output = T, verbose = TRUE, width = 5, height = 5)  
      
      setwd(current_dir)
      rm(pval, results_params_trait, current_dir)
      
    }
    
    rm(f_results)
    setwd("../")
  
  }

  setwd("../")
}
