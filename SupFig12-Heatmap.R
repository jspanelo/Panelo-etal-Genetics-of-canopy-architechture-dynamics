library(readr)
library(tidyverse)

# Data from Table S5
Sig_SNP <- read.delim(file = "SupplementaryTable5.txt")[,1:7]
Sig_SNP <- Sig_SNP[Sig_SNP$Analysis == "Single Time Point",]

# Rename Variables
names(Sig_SNP)[5] <- "WAP"

# Filter data
Sig_SNP <- Sig_SNP %>%
  mutate_at(vars(-WAP, -p.value, -Chromosome), as.factor) %>% 
  unite(Key, c(Population, Descriptor, SNP), sep = " ", remove = F)

# Re state levels
Sig_SNP$Population <- factor(Sig_SNP$Population, levels = c("PSP+SAP", "PSP", "SAP"))
Sig_SNP$Descriptor <- factor(Sig_SNP$Descriptor, levels = c("PBPH", "PBPW", "CHV", "PSA"))

# Define SNPs to plot
Sig_SNP %>% 
  group_by(Key) %>% summarise(n = n()) %>% 
  filter(n > 1) %>% 
  select(Key) -> markers_to_plot

# Add blank spaces as for titls
Sig_SNP_with_blank <- Sig_SNP %>%
  filter(Key %in% markers_to_plot$Key) %>%
  unite(Key, c(Descriptor, SNP), sep = " ", remove = F) %>% 
  add_row(Population = "SAP", Key = "SAP") %>%
  add_row(Population = "PSP", Key = "PSP") %>%
  add_row(Population = "PSP+SAP", Key = "PSP + SAP") %>%
  add_row(Population = "SAP", Key = " ") %>%
  add_row(Population = "PSP", Key = "  ") %>%
  add_row(Population = "PSP+SAP", Key = "   ") %>%
  mutate(Population = factor(Population, levels = c("PSP+SAP", "PSP", "SAP"))) %>% 
  arrange(Population, desc(Descriptor), desc(Chromosome), SNP, WAP) %>%
  mutate(`LOD-score` = -log10(p.value)) %>%
  mutate(Key = factor(Key, levels = unique(Key)))

# Create a transparet color
transparent_red <- rgb(0, 0, 0, alpha = 0.000000001)

# Plot
ggplot(data = Sig_SNP_with_blank, aes(x = WAP, y = Key, fill = `LOD-score`)) +
  geom_tile(color = 'white') +
  scale_fill_viridis_c() +
  scale_color_manual("white") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(vjust = 0.85, size = 18),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 18)) +
  
  theme(panel.grid = element_blank()) +
  scale_y_discrete(position = "right") +
  ylab(NULL) +
  xlab("Weeks after planting") +
  
  # modify valus accordingly to the number of SNPs
  geom_rect(aes(xmin = 6.5, xmax = 10.5, ymin = 0.5, ymax = 11.5), 
            fill = transparent_red, color = "black", size = 1) + 
  geom_rect(aes(xmin = 6.5, xmax = 10.5, ymin = 13.5, ymax = 16.5), 
            fill = transparent_red, color = "black", size = 1) +
  geom_rect(aes(xmin = 6.5, xmax = 10.5, ymin = 18.5, ymax = 25.5), 
            fill = transparent_red, color = "black", size = 1)
