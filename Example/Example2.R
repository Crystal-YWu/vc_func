#Minimal workflow for ingle oocyte
library(tidyverse)
library(abf2load)
library(vcfunc)

#This is all you need to change -----------------------------------------------

csv_file <- "vcfunc example/sample data file index.csv"
data_folder <- "vcfunc example/sample data/"

#Select oocyte to analyse
index <- read_csv(csv_file)
N = c(1:20)
selected <- select.samples(dataindex = index,
                           oocyteNo = N,
                           gene = "A2",
                           buffer = "50N")

#End of manual inteference-----------------------------------------------------

nselected <- nrow(selected)

#We keep abf_raw as a backup so we can easily restore
sample_list <- abf2.load_in_folder(folder = data_folder,
                               filename_list = unlist(selected[, "FileName"]))
sample_title <- unlist(selected[, "FileName"])

#Find sampling intervals
intv_list <- FindAllSamplingInterval(sample_list, backward_seach = TRUE)
#Plot channels with sampling intervals
p_chan1 <- PlotAll_ChannelWithIntv(sample_list, intv_list, 1, title_list = sample_title)
p_chan2 <- PlotAll_ChannelWithIntv(sample_list, intv_list, 2, title_list = sample_title)

#Calculate mean voltage and current
df_summary <- AllSamples_IVSummary(sample_list, intv_list)
#Show the table
View(df_summary)
#Plot I-V curve
p_summary <- qplot(x = df_summary$Voltage, y = df_summary$Current) + geom_smooth()
