library(tidyverse)
# setwd("C:/Users/Anta/Downloads/Obtain_Data")
# csv<- list.files(
#   pattern = "\\.csv",
#   full.names = T) %>% 
#   lapply(read.csv) %>% 
#   bind_rows()
# 
# colnames(csv) = c("ID", "Price", "Date", "PostCode", "Type1", "Type2", "Type3", "House Num", "Flat", "Street Name", "Locality", "Town", "District", "County", "Type4", "Type5")
# fire=tail(csv)




#-----------------------------------------
#Cleaning Data For House Price

pop2020 = read_csv("C:/Users/Anta/Downloads/Data Science/Obtain_Data/pp-2020.csv", show_col_types = FALSE)
pop2021 = read_csv("C:/Users/Anta/Downloads/Data Science/Obtain_Data/pp-2021.csv")
pop2022 = read_csv("C:/Users/Anta/Downloads/Data Science/Obtain_Data/pp-2022.csv", show_col_types = FALSE)
pop2023 = read_csv("C:/Users/Anta/Downloads/Data Science/Obtain_Data/pp-2023.csv")


# Renaming the columns
colnames(pop2020) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
                     "Locality", "Town" , "District", "County", "Type1", "Type2" )
colnames(pop2021) <-colnames(pop2020)
colnames(pop2022)<-colnames(pop2020)
colnames(pop2023)<-colnames(pop2020)
# 
# colnames(pop2021) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
#                      "Locality", "Town" , "District", "County", "Type1", "Type2")
# colnames(pop2022) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
#                      "Locality", "Town" , "District", "County" , "Type1", "Type2")
# colnames(pop2023) = c("ID" , "Price", "Year", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street Name",
#                      "Locality", "Town" , "District", "County" , "Type1", "Type2")

# Merging the datasets
HousePrices = pop2023 %>%
  add_row(pop2022)%>%
  add_row(pop2021) %>% 
  add_row(pop2020)


fire1=tail(HousePrices)
fire=head(HousePrices)

# Write the combined data to a new CSV file
write.csv(HousePrices, "merge-pop-data.csv")

data=read.csv("C:/Users/Anta/Downloads/Data Science/clean data/merge-pop-data.csv")
view(data)





# Filtering Greater Manchester and Merseyside data
FilteredHousePrices = filter(data, County == 'GREATER MANCHESTER' | County == 'MERSEYSIDE')

FilteredHousePrices = FilteredHousePrices %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1, 4))) %>%
  mutate(Year = str_trim(substring(Year, 1, 4))) %>% 
  select(PostCode, shortPostcode, Year, PAON, Price) %>% 
  na.omit()

# exporting filteredhouseprices data set to  csv
write.csv(FilteredHousePrices, "CleanedHousePrices.csv")


#Town
HousePricesclean <- read_csv("C:/Users/Anta/Downloads/Data Science/clean data/CleanedHousePrices.csv")



population = read_csv("C:/Users/Anta/Downloads/Data Science/Obtain_Data/Population2011_1656567141570.csv", show_col_types = FALSE)
# Clean the population data
population_data <- population %>%
  mutate(shortPostcode = str_trim(substring(Postcode, 1, 4))) %>% 
  group_by(shortPostcode) %>% 
  summarise(Population = sum(as.numeric(gsub(",", "", Population))))




FilteredTown = filter(data, County == 'GREATER MANCHESTER' | County == 'MERSEYSIDE')

# Join the house prices data with population data
FilteredTown1 = FilteredTown %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>%
  mutate(Year = str_trim(substring(Year, 1,4))) %>% 
  left_join(population_data,by="shortPostcode") %>% 
  select(PostCode, shortPostcode, Year, Town, District, County) %>% 
  group_by(shortPostcode) %>%
  filter(row_number()==1) %>%
  arrange(County) %>% 
  na.omit()
write.csv(FilteredTown1, "CleanedData_Towns.csv")



#Ten houses with the highest prices
Filtered_high_houseprice <- data %>%
  select(County, House.Num, Price,Street.Name,Year) %>%
  arrange(desc(Price)) %>%
  head(10)
print(Filtered_high_houseprice)
# exporting filteredhouseprices data set to  csv
write.csv(Filtered_high_houseprice, "CleanedData/Cleaned_Top_HousePrices.csv")





