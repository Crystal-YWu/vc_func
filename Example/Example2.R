#Minimal workflow for ingle oocyte
library(tidyverse)
library(abf2load)
library(vcfunc)

#storage for all buffer data
all_buffer = list()
#storage for channel plot
p_chan1 = list()
p_chan2 = list()

###############################################################################
#This is all you need to change -----------------------------------------------

csv_file <- "vcfunc example/sample data file index.csv"
data_folder <- "vcfunc example/sample data/"

#Select oocyte to analyse
index <- read_csv(csv_file)
buffer = c("B", "1N", "5N", "10N", "25N", "50N", "100N")
oocyteN = c(1:20)

#Control plotting
enable_plot = FALSE

#End of manual inteference-----------------------------------------------------
###############################################################################

for (i in buffer) {
  selected <- select_samples(
    dataindex = index,
    oocyteNo = oocyteN,
    gene = "A2",
    buffer = i
  )

  cat("Proessing buffer", i, "\n")

  nselected <- nrow(selected)
  #We keep abf_raw as a backup so we can easily restore
  sample_list <- abf2.load_in_folder(folder = data_folder,
                                     filename_list = unlist(selected[, "FileName"]))
  sample_title <- unlist(selected[, "FileName"])

  #Find sampling intervals
  intv_list <-
    FindAllSamplingInterval(sample_list, backward_seach = TRUE)
  #Plot channels with sampling intervals
  if (enable_plot) {
    p_chan1[[i]] <- PlotAll_ChannelWithIntv(sample_list, intv_list, 1, title_list = sample_title)
    print(p_chan1[[i]])
    p_chan2[[i]] <- PlotAll_ChannelWithIntv(sample_list, intv_list, 2, title_list = sample_title)
    print(p_chan2[[i]])
  }

  #Calculate mean voltage and current
  df_summary <- AllSamples_IVSummary(sample_list, intv_list)

  all_buffer[[i]] <- df_summary
}

p_buffer <- PlotAll_IVSummary(all_buffer)
example_summary <- bind_rows(all_buffer, .id = "Buffer")
View(example_summary)
print(p_buffer)
