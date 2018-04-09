######################
##### USER INPUT #####
######################

#Your directory
directory <- "/Usr/blah"

#####################################
##### READ IN LIBRARIES, INPUTS #####
#####################################

#Set the working directory (insert your directory here)
setwd(directory)

#Import necessary libraries
library(ShortRead)

#Read in the reference libraries (targets=T, controls=C) and combine
reflibT = read.csv("./reflib/RefLibT.csv")
reflibC = read.csv("./reflib/RefLibC.csv")
reflibCT = rbind(reflibT,reflibC)

######################################
##### FORMAT REFERENCE LIBRARIES #####
######################################

#Remove duplicated rows in reflibs
reflibCTR <- reflibCT[!duplicated(reflibCT$Phage),]

#Append 51bp of conserved H3 sequence
reflibCTR$H3 <- paste0(reflibCTR$H3,"GACTACTGGGGTCAAGGAACCCTGGTCAAGATCGGAAGAGCACACGTCTGA")

#Cut to 35bp total starting from beginning
reflibCTR$H3 <- substr(reflibCTR$H3,1,35)

##################################
##### FIND SEQUENCING COUNTS #####
##################################

#Delete (if needed) and then make output directory
unlink("output",recursive=TRUE)
dir.create("output")

#Read file names
raw <- list.files("./input")

#Loop for reading in Fastq files
for (f in 1:length(raw)){
  	reads=readFastq(file.path("./input",raw[f]))

	#Parse out the sequences themselves
	sequences=sread(reads)

	#Create a subset containing only bases 1-35 in the sequences
	dict=DNAStringSet(substr(sequences,1,35))
	
	#Tabulate the number of frequency of the same sequence and output as tbls
	tbls <- as.data.frame(table(dict)) 
	
	#Remove the clutter before continuing
	rm(dict)
	rm(sequences)
	rm(reads)
	gc()
	
	#Count rows in reference library
	n <- nrow(reflibCTR)

	#Set up outputs as data frames
	output <- as.data.frame(matrix(0,ncol=2,nrow=n))

	#For targets, find the exact match from ref library and output the frequency of that match
	for (i in 1:n) {
  	output[i,] <- c(as.character(reflibCTR[i,1]),tbls[grepl(reflibCTR[i,2], tbls$dict),"Freq"]) 
	
	#Output progress to the console
	percent_sample <- format((f/length(raw))*100, digits=2)
	percent_clone <- format(((i/n)*100), digits=2)
	cat(paste0("sample(",percent_sample,"%)","  ","clone(",percent_clone,"%)","\n"))
	}

	#Export the counts file
	name <- paste(raw[f],".csv", sep="")
	write.csv(output,paste("./output/",name))

}

############################
##### CONSOLIDATE DATA #####
############################

#List same-group files
outfiles <- list.files("./output/")

#Create a data frame for receiving counts
df1 <- as.data.frame(matrix(0,ncol=length(outfiles),nrow=n))

#Name the rows and columns
row.names(df1) <- reflibCTR$Phage
colnames(df1) <- outfiles


#Append target data
for (o in 1:length(outfiles)) {
	temp <- read.csv(paste("./output/",outfiles[o],sep=""))
	df1[,o] <- temp$V2
}
	
#Change lines that have words into zeros for targets
for (s in 1:length(outfiles)) {
df1[,s] = as.numeric(as.character(df1[,s]))
df1[,s][is.na(df1[,s])] = 0
}

#Export the final file as a CSV for targets and controls
write.csv(df1, "counts.csv")
