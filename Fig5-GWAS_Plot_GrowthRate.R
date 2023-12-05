library(tidyverse)
setwd("C:/")

#####################################################
#####################################################
#############                    ####################
#############     Dataframes     ####################
#############                    ####################
#####################################################
#####################################################

# Significant_SNPS_GrowthRate.csv includes SNP, Chromosome, Position, p-value, Trait (Descriptor), Analysis (No covariates, Dw1, Dw3, Dw1+Dw3), and Population
Associations <- read.delim(file = "Significant_SNPS_GrowthRate.csv", sep = ",")

Associations$Analysis <- as.factor(Associations$Analysis)

Associations$Parameter <- factor(Associations$Parameter, levels = c("GrowthRate"))
Associations$Descriptor <- factor(Associations$Descriptor, levels = c("PBPH", "PBPW", "CHV", "PSA"))

new_labels <- c("1" = "Chr 1", "2" = "Chr 2", "3" = "Chr 3", "4" = "Chr 4", "5" = "Chr 5",
                "6" = "Chr 6", "7" = "Chr 7", "8" = "Chr 8", "9" = "Chr 9", "10" = "Chr 10")

SAP.f <- Associations %>% filter(Population == "SAP") %>% group_by(Population, SNP, Chr, Pos, Descriptor, Parameter, By_trait) %>% summarise(n = n()) %>% filter(n > 1)
SAP.f <- SAP.f %>% filter(!SNP %in% "S6_39628988")

PSP.f <- Associations %>% filter(Population == "PSP" & Analysis == "No Covariate")# %>% group_by(Population, SNP, Chr, Pos, Descriptor, Parameter) %>% summarise(n = n()) #%>% filter(n > 1)

Combined.f <- Associations %>% filter(Population == "PSP+SAP") %>% group_by(Population, SNP, Chr, Pos, Descriptor, Parameter, By_trait) %>% summarise(n = n()) %>% filter(n > 1)
Combined.f <- Combined.f %>% filter(!SNP %in% c("S7_58390034", "S7_58584926")) 
# 
for_labels <- rbind(SAP.f, Combined.f, PSP.f)
Associations %>% filter(SNP %in% for_labels$SNP) %>% group_by(By_trait) %>% summarise(n = n()) %>% filter(n>1) %>% select(By_trait) -> list
lst <- c(list$By_trait, PSP.f$By_trait) %>% sort()
for_labels <- Associations %>% filter(By_trait %in% lst) %>% group_by(Population, Descriptor, SNP, Chr, Pos) %>% summarise(n = n()) %>% select(!n)

{

#################################################
#################################################
#############                ####################
#############     Figure     ####################
#############                ####################
#################################################
#################################################

s <- 13
s <- 3

Associations$Week <- Associations$Analysis
Associations$Week <- as.factor(Associations$Week)
levels(Associations$Week) <- c(1, 0, 2, 3)
Associations$Week <- as.character(Associations$Week)
Associations$Week <- as.numeric(Associations$Week)
}
Associations %>%    
  ggplot(mapping = aes(x = Pos/1000000, y = Week)) + 
  
  geom_point(aes(alpha = 0.6, 
                 #fill = Population, 
                 color = Population, 
                 shape = Descriptor, 
                 size = 12)) + 
  
  scale_y_discrete(limits = c("Dw1 x Dw3", "Dw1", "Dw3", "No Covariate")) +
  
  geom_vline(data = Combined.f, aes(xintercept = Pos/1000000), lty = 2, linewidth = 2, colour = '#009E73') + # cont
  geom_vline(data = PSP.f, aes(xintercept = Pos/1000000), lty = 2, linewidth = 2, colour = '#E69F00') + # dahsed
  geom_vline(data = SAP.f, aes(xintercept = Pos/1000000), lty = 2, linewidth = 2, colour = '#56B4E9') + # dot
  
  geom_label(data = for_labels[for_labels$Population == 'SAP',], 
             aes(x = X/1000000, y = Week, label = Label, color = Population), size = s, fill = '#56B4E9', color = 'white') +
  geom_label(data = for_labels[for_labels$Population == 'PSP',], 
            aes(x = X/1000000, y = Week, label = Label, color = Population), size = s, fill = '#E69F00', color = 'white') +
  geom_label(data = for_labels[for_labels$Population == 'PSP+SAP',], 
            aes(x = X/1000000, y = Week, label = Label, color = Population), size = s, fill = '#009E73', color = 'white') +
  
  scale_shape_manual(values = c(15, 18, 19, 17)) +
  #scale_shape_manual(values = c(0, 5, 1, 2)) +
  
  scale_color_manual(values = c('#009E73', '#E69F00', '#56B4E9')) +
  scale_size(range = c(10, 20)) +
  
  facet_wrap(.~ Chr, ncol = 5, scales = 'free_x', labeller = labeller(Chr = new_labels)) +
  
  guides(alpha = 'none', fill = 'none', size = 'none',
         shape = guide_legend(override.aes = list(size = 10)),
         color = guide_legend(override.aes = list(size = 10)),
         scale_shape_manual(values = c(15, 18, 19, 17))) +

  theme_bw() +
  xlab('Position [Mb]') +
  ylab("Analysis") +
  labs(title = "Significant associations for Growth Rate",
       color = 'Population', 
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

# Export as 3500 x 1800 pixels 

# Export as 3500 x 2000 pixels 

snps <- read.delim(file = "clipboard")
