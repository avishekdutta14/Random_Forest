library(dplyr)

#reading the unique tally file (output of paprica)

df =  read.csv("BP_training_20.bacteria.unique_tally.csv", header=TRUE,row.names = 1)
df[is.na(df)] = 0
df_percent<- df / rowSums(df) * 100
write.csv(df_percent, "unique_percentage.csv")
df2 =  read.csv ("unique_percentage.csv", header = TRUE)
colnames(df2)[1] <- "SampleID"

#Reading the metadata file
metadata =  read.csv("sample_data_with_sulfide_09012020.csv", header=TRUE)

sulfide = metadata %>%  select(SampleID, Sulfide)

#merging them based on SampleID
RF_input = merge(sulfide,df2,by="SampleID")

#sorting gives an even random sampling in random forest

RF_input_sort = RF_input %>% arrange(desc(Sulfide))

#writing the formatted file for RF_input

write.csv(RF_input_sort,"RF_input.csv", row.names = FALSE)
