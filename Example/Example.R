library(tidyverse)
library(abf2load)
library(vcfunc)

index <- read_csv("vcfunc example/sample data file index.csv")

N = c(1:20)
selected <- select.samples(dataindex = index,
                           oocyteNo = N,
                           gene = "A2",
                           buffer = "50N")
nselected <- nrow(selected)

#We keep abf_raw as a backup so we can easily restore
abf_raw <- abf2.load_in_folder(folder = "vcfunc example/sample data/",
                                filename_list = unlist(selected[, "FileName"]))

abf_list <- abf_raw
abf_title <- unlist(selected[, "FileName"])


#Plot all loaded abf files
#Could be slow depending on how large the list is, note that title_list is optional
PlotAll_IVChannel(abf_list, title_list = abf_title)

#Alternatively, plot individual channel side by side
PlotAll_Channel(abf_list, 1, title_list = abf_title)

PlotAll_Channel(abf_list, 2, title_list = abf_title)

#Remove unwanted episodes
abf_list[[2]] <- AbfRemoveEpisode(abf_list[[2]], 11)

#Restore to original state
abf_list[[2]] <- abf_raw[[2]]

#Find sampling intervals, one-step
intv_list <- FindAllSamplingInterval(abf_list)
#Check the result
PlotAll_IVChannelWithIntv(abf_list, intv_list, title_list = abf_title)
#Alternatively, plot individual channel side by side
PlotAll_ChannelWithIntv(abf_list, intv_list, 1)
PlotAll_ChannelWithIntv(abf_list, intv_list, 2)

#It seems that the auto selected sampling interval of Sample 3 is not optimal
#Perform a backward search for a better interal
intv_list[[3]] <- FindSamplingInterval(abf_list[[3]], backward_seach = TRUE)
#Check new intervals
PlotAll_ChannelWithIntv(abf_list, intv_list, 1)
PlotAll_ChannelWithIntv(abf_list, intv_list, 2)

#Alternatively, for data consistency, you can perform backward_search for all data
intv_list <- FindAllSamplingInterval(abf_list, backward_seach = TRUE)
#Check new intervals
PlotAll_ChannelWithIntv(abf_list, intv_list, 1)
PlotAll_ChannelWithIntv(abf_list, intv_list, 2)

#You can also do this for individual abf data
intv1 <- FindSamplingInterval(abf_list[[1]])
#Sampling interval is fixed size, however, you can dynamically (and automatically) expand
#by simply give a maximum interval expansion rate
intv2 <- FindSamplingInterval(abf_list[[1]], max_interval_expansion_rate = 12)
#compare the results
Plot_IVChannelWithIntv(abf_list[[1]], intv1)
Plot_IVChannelWithIntv(abf_list[[1]], intv2)

#Manually change interval for 2nd abf
sample_id <- 2
intv_start <- 6200
intv_length <- 150
#You can call a function to do this
intv_list <- ChangeInterval(intv_list, sample_id, intv_start, intv_length)
#Or do it manually
intv_list[[sample_id]][1] <- intv_start
intv_list[[sample_id]][2] <- intv_start + intv_length -1

#Processing data
#Get a data.frame of mean current
df_mean_current <- AllSamples_CurrentMeans(abf_list, intv_list)
#Get a data.frame of mean voltage
df_mean_voltage <- AllSamples_VoltageMeans(abf_list, intv_list)
#Or preferably, just get a summary one-step
df_summary <- AllSamples_IVSummary(abf_list, intv_list)
View(df_summary)
#result quick peek
qplot(x = df_summary$Voltage, y = df_summary$Current) + geom_smooth()
