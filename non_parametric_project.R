library(readxl)
data <- read.csv('data__.csv')
# Data Cleaning and filling missinng valus :- 

# total dimention of data 
dim(data)

# we have 999 rows and 32 columns 

# now define a function to check missing values and if more then 25 % are missing
# we will drop that coumn 
coumn_check <- function(data){
  out <- NULL
  n = dim(data)[2]
  for( i in 1:n){
    len <- 999-sum(is.na(data[,i])) - sum(which(data[,i] ==''))
    persent = len/999*100
    if (persent < 75){
      print(paste('remove',i,'th coumn'))
      out = append(out, i)
      
    }
    
  }
  return(out)
  
}
colm <- coumn_check(data) # here we get missing walues columns that we should remove 
df <- data[,-colm]

# now we will replace missing values with medain if the catogry is numberical and if catogircal
# we will replace by max output in that coulmn


fill_missing_values <- function(data) {
  for (column_name in colnames(data)) {
    # Identify empty strings and convert them to NA
    data[[column_name]][data[[column_name]] == ""] <- NA
    
    # Check if the column is numeric
    if (is.numeric(data[[column_name]])) {
      # Replace NA (missing) values with the median
      median_value <- median(data[[column_name]], na.rm = TRUE)
      data[[column_name]][is.na(data[[column_name]])] <- median_value
    } else {
      # Replace NA (missing) values with the mode for categorical columns
      mode_value <- names(sort(table(data[[column_name]]), decreasing = TRUE))[1]
      data[[column_name]][is.na(data[[column_name]])] <- mode_value
    }
  }
  return(data)
}

df_filled <- fill_missing_values(df)
df_filled



write.csv(x = df_filled,file = 'data_non_perametic.csv')

# repeat rows

length(df_filled[,1]) -length(unique(df_filled[,1]))
# remove duplicate
df_filled_uni <- data[!duplicated(df_filled[[1]]), ]
length(df_filled_uni[,1])

write.csv(x = df_filled_uni,file = 'data_non_perametic.csv')
