raw_data = read.csv('anes_timeseries_cdf_rawdata.txt')

# Take year 2016 data
clean_data = raw_data[raw_data$VCF0004 == 2016,]

# Take data with interview result (for 2016, VCF0013 and VCF0014 are identical) 
clean_data=clean_data[clean_data$VCF0013==1,]

# Remove all NA
all_na = c()
for(i in names(clean_data)){
  # print(i)
  if(all(is.na(clean_data[i]))){
    all_na = c(all_na, i)
  }
}
clean_data = clean_data[ , !(names(clean_data) %in% all_na)]

# Check rest NA
for(i in names(clean_data)){
  if(anyNA(clean_data[i])){
    print(i)
  }
}

# Remove meaningless data
redundant = c()
for(i in names(clean_data)){
  if(length(table(clean_data[i]))==1){
    redundant = c(redundant, i)
  }
}
clean_data = clean_data[ , !(names(clean_data) %in% redundant)]

# Cor
sub_clean_data = clean_data[ , !(names(clean_data) %in% c('VCF0070a', 'VCF0070b', 'VCF0900c', 'VCF0901b'))]
correlation = c()
for(i in names(sub_clean_data)){
  for(j in names(sub_clean_data)){
    if(i!=j){
      # print(j)
      if(abs(cor(sub_clean_data[i], sub_clean_data[j])) == 1 ){
        print(paste(i, j, cor(sub_clean_data[i], sub_clean_data[j])))
      }
    }
  }
}

clean_data = clean_data[ , !(names(clean_data) %in% c('VCF0006, VCF0006a', 'VCF0010y', 'VCF0011y'))]

# Output
write.csv(clean_data, file="clean_data.csv")
