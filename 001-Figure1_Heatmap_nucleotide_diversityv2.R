# Load necessary libraries
library(pheatmap)    # for heatmap
library(RColorBrewer) # for color palette
library(data.table)   # for data handling
library(dplyr)
library(gggenes)
library(ggplot2)
library(tidyr)
library(cowplot)

setwd("C:/Users/diash/Dropbox/HUMAN_PHP_Campa_etal2023/Data/Figura_1_diversity")

# Load data ---------------------------------------------------------------


# Load your data
PiA <- read.csv("PSPH_PI_AncExon.csv", sep = ",", header = F, stringsAsFactors = F)
colnames(PiA)= c('chr','Pos1','Pos2','#snpAnc','pi','SNAnc','Type')
head(PiA)

PiM <- read.csv("PSPH_PI_ModExon.csv", sep = ",", header = F, stringsAsFactors = F)
colnames(PiM)= c('chr','Pos1','Pos2','#snpAnc','pi','SNAnc','Type')
head(PiM)

PiR <- read.csv("PSPH_PI_AncMod_columnasSinRestaExon.csv", sep = ",", header = F, stringsAsFactors = F)
colnames(PiR)= c('chr','Pos1','Pos2','#snpAnc','pi','SNAnc','Type')
head(PiR)

PSPH_gene <- read.csv("Gene_subPSPH_Exo.csv", sep = ",", header = F, stringsAsFactors = F)
PSPH_EI <- read.csv("Gene_PSPH_Exo.csv", sep = ",", header = F, stringsAsFactors = F)
colnames(PSPH_gene)= c('molecule','gene','start','end','strand','subgene','from','to','orientation','direction')
colnames(PSPH_EI)= c('molecule','gene','start','end','strand','orientation','direction')
head(PSPH_gene)
head(PSPH_EI)


q

############# P4
P4 <- ggplot(subset(PSPH_EI, molecule == "Chromosome 7" & gene == "PSPH"),
             aes(xmin = start, xmax = end, y = strand)) +
  geom_gene_arrow(color = "gray80") +
  geom_gene_label(aes(label = gene)) +
  geom_subgene_arrow(
    data = subset(PSPH_gene, molecule == "Chromosome 7" & gene == "Exon"),
    aes(xsubmin = from, xsubmax = to, fill = subgene)) +
#  scale_fill_viridis_d(option = "A", begin = 0.4)+
  scale_fill_grey(start = 0,end = 0.65)+
  #scale_fill_manual(values = c("#1a1a1a", "#1a1a1a","#1a1a1a","#1a1a1a", "#1a1a1a","#1a1a1a", "#1a1a1a","#1a1a1a"))+
 #scale_fill_manual(values = c("#3f007d", "#54278f","#6a51a3","#807dba", "#8c6bb1","#88419d", "#810f7c","#4d004b"))+
  geom_subgene_label(
    data = subset(PSPH_gene, molecule == "Chromosome 7" & gene == "Exon"),
    aes(xsubmin = from, xsubmax = to, label = subgene),
    min.size = 0, color = "gray90", fontface = "bold") + theme_void() + theme(legend.position = "none") +
  #scale_x_continuous(name="Nucleotide position\nPSPH gene") #+
  
  scale_x_continuous(name="PSPH gene") +
  #scale_x_continuous(name="Chr7 Position(Mb)")
  theme(axis.text.x=element_blank())

# graphics.off()
# windows()
# P4



# Figure 001 assembly   ---------------------------------------------------
  
  
  fplot2 <- ggdraw() + 
    draw_image("Alignment_version3alt.png" ,x = -0.02, y = -.22, width =1.05)+
    draw_plot(hmp1,x = 0, y = 0.53, width =0.95, height = 0.46)+
    draw_plot(P4,x = 0.029, y = 0.35, width =0.918,height = 0.48)+
    draw_plot_label(label = "A", x = 0.01, y = 0.93, size = 15)+
    draw_plot_label(label = "B", x = 0.01, y = 0.53, size = 15)+
    geom_text(aes(x = 0.038, y = 0.84, label = "Modern"), size =4.2)+
    geom_text(aes(x = 0.038, y = 0.75, label = "Ancient \n"), size =4.2)+
    geom_text(aes(x = 0.035, y = 0.7, label ="Delta*pi"), parse = T,size =5)+
    geom_text(aes(x = 0.58, y = 0.91, label ="R27\n*"),size =3.5,  color = "red", fontface = "bold")+
    geom_text(aes(x = 0.485, y = 0.91, label ="Q83\n*"),size =3.5,  color = "red", fontface = "bold")+
    geom_text(aes(x = 0.045, y = 0.345, label = "Human\nChimp\nYeast\n"), size = 4)+
    geom_text(aes(x = 0.034, y = 0.16, label = "Conservation"), size = 3.5)+
    geom_text(aes(x = 0.18, y = 0.5, label ="27"),size =3.5,  color = "red", fontface = "bold")+
    geom_text(aes(x = 0.665, y = 0.5, label ="83"),size =3.5,  color = "red", fontface = "bold")+
    geom_text(aes(x = 0.235, y = 0.5, label ="32"),size =3.5,  color = "cyan3" , fontface = "bold")+
    geom_text(aes(x = 0.267, y = 0.5, label ="35"),size =3.5,  color = "cyan3", fontface = "bold")
   
  
  # graphics.off()
  # windows()
  # fplot2
  
  # Save to PDF
  pdf("../Alt_Figures2025/Fig001-nucleotide_diversity_heatmap_v3.pdf", width = 12, height = 5)
  fplot2
  # Close the PDF device
  dev.off()
  
