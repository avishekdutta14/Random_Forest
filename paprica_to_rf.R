library(dplyr)

#reading the unique tally file (output of paprica)

df =  read.csv("BP_training_20.bacteria.unique_tally.csv", header=TRUE,row.names = 1)
df[is.na(df)] = 0
df_percent<- df / rowSums(df) * 100
write.csv(df_percent, "unique_percentage.csv")
df2 =  read.csv ("unique_percentage.csv", header = TRUE)
colnames(df2)[1] <- "SampleID"

#Reading the metadata file-the metadata file should have a column named SampleID 
#having the sample names matching the SampleID in paprica output. Also the response/
#dependent variables should be present in the metadata file.
metadata =  read.csv("sample_metadata.csv", header=TRUE)

#Here Sulfide is the dependent variable, you can choose independent variable of your choice
sulfide = metadata %>%  select(SampleID, Sulfide)

#merging based on SampleID
RF_input = merge(sulfide,df2,by="SampleID")

#sorting gives an even random sampling in random forest

RF_input_sort = RF_input %>% arrange(desc(Sulfide))

#writing the formatted file for RF_input

write.csv(RF_input_sort,"RF_input.csv", row.names = FALSE)
