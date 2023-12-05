library(tidyverse)
setwd("C:/")

#####################################################
#####################################################
#############                    ####################
#############     Dataframes     ####################
#############                    ####################
#####################################################
#####################################################

# Significant_SNPS_timepoint.csv includes SNP, Chromosome, Position, p-value, Trait (Descriptor), Week (WAP), and Population
Associations <- read.delim(file = "./Significant_SNPS_timepoint.csv", sep = ",")

Associations$Week <- as.numeric(Associations$Week)
Associations$Trait <- factor(Associations$Trait, levels = c("PBPH", "PBPW", "CHV", "PSA"))

new_labels <- c("1" = "Chr 1", "2" = "Chr 2", "3" = "Chr 3", "4" = "Chr 4", "5" = "Chr 5",
                "6" = "Chr 6", "7" = "Chr 7", "8" = "Chr 8", "9" = "Chr 9", "10" = "Chr 10")

Associations$Population <- as.factor(Associations$Population)
 
levels(Associations$Population)[1] <- "PSP+SAP"

SAP.f <- Associations %>% filter(Population == "SAP") %>% group_by(Population, SNP, Chr, Chrom, Pos, Trait) %>% summarise(n = n()) %>% filter(n > 1)
PSP.f <- Associations %>% filter(Population == "PSP") %>% group_by(Population, SNP, Chr, Chrom, Pos, Trait) %>% summarise(n = n()) %>% filter(n > 1)
Combined.f <- Associations %>% filter(Population == "PSP+SAP") %>% group_by(Population, SNP, Chr, Chrom, Pos, Trait) %>% summarise(n = n()) %>% filter(n > 1)

# Significant_SNPS_labels.csv presents information on the SNPs discovered among different weeks.
# Includes: SNP, Chromosome, Position, Population where the SNP was discovered

for_labels <- read.delim(file = "Significant_SNPS_labels.csv", sep = ',')

#################################################
#################################################
#############                ####################
#############     Figure     ####################
#############                ####################
#################################################
#################################################

Associations %>%  
  ggplot(mapping = aes(x = Pos/1000000, y = Week)) + 

  geom_vline(data = Combined.f, aes(xintercept = Pos/1000000), lty = 3, linewidth = 2, colour = '#009E73') + # cont
  geom_vline(data = PSP.f, aes(xintercept = Pos/1000000), lty = 2, linewidth = 2, colour = '#E69F00') + # dahsed
  geom_vline(data = SAP.f, aes(xintercept = Pos/1000000), lty = 2, linewidth = 2, colour = '#56B4E9') + # dot
  
  geom_label(data = for_labels[for_labels$Population == 'SAP',], 
             aes(x = X/1000000, y = Week, label = Label, color = Population), size = 13, fill = '#56B4E9', color = 'white') +
  geom_label(data = for_labels[for_labels$Population == 'PSP',], 
             aes(x = X/1000000, y = Week, label = Label, color = Population), size = 13, fill = '#E69F00', color = 'white') +
  geom_label(data = for_labels[for_labels$Population == 'Combined',], 
             aes(x = X/1000000, y = Week, label = Label, color = Population), size = 13, fill = '#009E73', color = 'white') +
  
  geom_point(aes(alpha = 0.6, #fill = Population, 
                 color = Population, shape = Trait, 
                 size = 12,
                 #size = -log10(`P.value`)
                 )) + 
  
  scale_shape_manual(values = c(15, 18, 19, 17)) +
  #scale_shape_manual(values = c(0, 5, 1, 2)) +
  
  scale_color_manual(values = c('#009E73', '#E69F00', '#56B4E9')) +
  scale_size(range = c(10, 20)) +
  
  facet_wrap(.~ Chr, ncol = 5, scales = 'free_x', labeller = labeller(Chr = new_labels)) +

  guides(alpha = 'none', fill = 'none', size = 'none',
         shape = guide_legend(override.aes = list(size = 10)),
         color = guide_legend(override.aes = list(size = 10)),
         scale_shape_manual(values = c(15, 18, 19, 17))) +
         #scale_shape_manual(values = c(0, 5, 1, 2))) +

  theme_bw() +
  xlab('Position [Mb]') +
  ylab("Weeks after planting") +
  labs(title = "Significant associations by timepoint",
       color = 'Population', 
       #size = '-log(p-value)', 
       shape = 'Canopy Descriptor') +
  
  theme(legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.box.just = 'left',
        legend.justification = 'left',
        legend.title = element_text(size = 38L),
        legend.text = element_text(size = 38L),
        axis.title.y = element_text(size = 42L),
        axis.title.x = element_text(size = 38L),
        axis.text.y = element_text(size = 38L),
        axis.text.x = element_text(size = 38L),
        strip.text.x = element_text(size = 38L),
        plot.title = element_text(size = 52L),
        plot.margin = unit(c(1,1,1,1), "cm"))

# Export as 3500 x 2000 pixels 
