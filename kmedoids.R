library(readxl)
library(dplyr)
library(cluster)

#Edited ----------------------------------------------------------------------------------------------------------------
set.seed(100)
# Read the data
malaria = read_excel("Malaria rate.xlsx")

# Remove "Entity" and "Year" columns
malaria2 = select(malaria, -Entity, -Year)

# Normalize the data using min-max method
normalized_malaria = 
  as.data.frame(apply(malaria2, 2, function(x) (x - min(x)) / (max(x) - min(x))))

# Perform clustering
k = 3
clust_medoids = pam(normalized_malaria, k)

Entity = malaria$Entity
Year = malaria$Year
data = cbind(Entity, Year, normalized_malaria)

print(clust_medoids)

malaria$Entity[400]
malaria$Entity[59]
malaria$Entity[1822]

#1 Cambodia
#2 Angola
#3 Nigeria

#Unedited --------------------------------------------------------------------------------------------------------------
set.seed(100)
# Read the data
malaria_e = read_excel("Malaria rate unedited.xlsx")

# Remove "Entity" and "Year" columns
malaria1 = select(malaria_e, -Entity, -Year)

# Normalize the data using min-max method
normalized_malaria1 = as.data.frame(apply(malaria1, 2, function(x) (x - min(x)) / (max(x) - min(x))))

# Perform clustering
k = 3
clust_medoids1 = pam(normalized_malaria1, k)

Entity = malaria_e$Entity
Year = malaria_e$Year
data1 = cbind(Entity, Year, normalized_malaria1)

print(clust_medoids1)

malaria_e$Entity[6660]
malaria_e$Entity[5693]
malaria_e$Entity[4185]

#1 World Bank High Income
#2 Sub-Saharan Africa (WB)
#3 Nigeria

