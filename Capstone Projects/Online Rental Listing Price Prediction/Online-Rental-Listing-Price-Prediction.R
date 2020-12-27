## ------------------------------------------------------------------------------------------------------------
# Online Rental Listing Price Prediction
## ------------------------------------------------------------------------------------------------------------
# Load libraries

options(digits = 5) #output digits
options(scipen = 999) # avoid exponential notations

library(tidyverse) #for tables, ggplot, etc
library(caret) #for ML
library(rpart) #for decision tree
library(lubridate) #for dates
library(naniar) #for missing values
library(gridExtra) #for plots
library(plyr) #for rounding digits (round_any)
library(Hmisc) #for correlation (Spearman) matrix
library(corrplot) #for correlation plots
library(rcompanion) #for Cramer's V correlation matrix


#Or use the following if packages are not installed earlier

# if(!require(tidyverse)) install.packages("tidyverse", 
#                                          repos = "http://cran.us.r-project.org")
# if(!require(caret)) install.packages("caret",
#                                      repos = "http://cran.us.r-project.org")
# if(!require(rpart)) install.packages("rpart",
#                                      repos = "http://cran.us.r-project.org")
# if(!require(lubridate)) install.packages("lubridate",
#                                          repos = "http://cran.us.r-project.org")
# if(!require(naniar)) install.packages("naniar",
#                                          repos = "http://cran.us.r-project.org")
# if(!require(gridExtra)) install.packages("gridExtra", 
#                                          repos = "http://cran.us.r-project.org")
# if(!require(plyr)) install.packages("plyr",
#                                          repos = "http://cran.us.r-project.org")
# if(!require(Hmisc)) install.packages("Hmisc",
#                                          repos = "http://cran.us.r-project.org")
# if(!require(corrplot)) install.packages("corrplot",
#                                          repos = "http://cran.us.r-project.org")
# if(!require(rcompanion)) install.packages("rcompanion",
#                                          repos = "http://cran.us.r-project.org")




## ------------------------------------------------------------------------------------------------------------
# Load Dataset
df_modfit <- read_csv("train.csv")

## Assumed that the downloaded file is saved in your working directory
## Use getwd() to check the same

## Data Understanding and Cleaning ----------------------------------------------------------------------------

# View dataset

head(df_modfit)


# Basic Summary

## data types and variables
df_modfit %>%
  mutate_if(., is.character, str_trunc, width = 20) %>%
  str(.)



# Summary stats

summary(df_modfit) 

## ------------------------------------------------------------------------------------------------------------
# Remove unneeded columns

df_modfit <- df_modfit %>%
  select(-c(name, description,thumbnail_url))


## ------------------------------------------------------------------------------------------------------------
# Check null value counts

## Initialize dataframe
missing_vals <- data.frame()

## null value count
for (col in colnames(df_modfit)){
  missing_vals <- missing_vals %>%
    rbind(data.frame(column = col, null_value = sum(is.na(df_modfit[, col]))))
}

## Updata dataframe
missing_vals %>%
  arrange(desc(null_value))


## ------------------------------------------------------------------------------------------------------------
# Visualize

naniar::vis_miss(df_modfit, warn_large_data = FALSE, sort_miss = TRUE)+
                   theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust = 1))

## ------------------------------------------------------------------------------------------------------------
# Derive variables from date type columns

## Find date type columns
date_list <- df_modfit %>%
  select_if(., is.Date) %>%
  colnames()

date_list


## ------------------------------------------------------------------------------------------------------------
## Create variable review_time_diff and update host_since
df_modfit <- df_modfit %>%
  mutate(review_time_diff = as.double(difftime(last_review, first_review, units = "weeks")),
         hosting_since = as.double(difftime(date(now()), host_since, units = "weeks")))

## Remove unneeded columns
df_modfit <- df_modfit %>%
  select(-all_of(date_list))



## ------------------------------------------------------------------------------------------------------------
# Derive Variables from Amenities

## split amenities in the dataset
x <- df_modfit$amenities %>%
  str_replace_all(., pattern = "\\{|\\}|\"|\"", replacement = "") %>%
  str_split(., pattern = ",")

df_modfit <- df_modfit %>%
  mutate(amenities = str_replace_all(amenities, pattern = "\\{|\\}|\"|\"",
                                     replacement = "") %>%
           str_split(., pattern = ","))

## Create amenities list, remove unneeded string symbols, eg: (s), -, etc
amenities_list <- Reduce(union, x) %>%
  str_trim() %>%
  str_replace_all(., pattern = "\\s*/\\s*|\\s|-|\\.", replacement = "_") %>%
  str_replace_all(., pattern = "\\(s\\)|\\:|'|\'", replacement = "")

df_modfit <- df_modfit %>%
  mutate(amenities =   str_trim(amenities) %>%
           str_replace_all(., pattern = "\\s*/\\s*|\\s|-|\\.", 
                           replacement = "_") %>%
           str_replace_all(., pattern = "\\(s\\)|\\:|'|\'", 
                           replacement = "")) 

head(amenities_list)
length(amenities_list)

## Remove zero-length input at index 80
print(amenities_list[80])
amenities_list <- amenities_list[-80]


## Generate separate columns for amenities

for (element in amenities_list){
  df_modfit[element] <- ifelse(str_detect(df_modfit$amenities, pattern = element), 1, 0)
}

## remove unneeded columns
df_modfit <- df_modfit %>%
  select(-amenities)

# check value counts of amenities

amenity_count <- data.frame(amenity = character(), 
                            present = double(), 
                            percent = double())

for (element in amenities_list){
  amenity_count <- amenity_count %>%
    rbind(., data.frame(amenity = element,
                        present = table(df_modfit[element])["1"],
                        percent = prop.table(table(df_modfit[element]))["1"]))
}

rownames(amenity_count) <- 1:length(amenities_list)

## view
amenity_count

## ------------------------------------------------------------------------------------------------------------
# Replace unneeded characters from host_response_rate

## Remove % sign and convert to numeric
df_modfit <- df_modfit %>%
  mutate(host_response_rate = 
           as.numeric(str_replace_all(host_response_rate, 
                                      pattern = "%", 
                                      replacement = "")))

# Remove uneeded characters from zipcode

df_modfit <- df_modfit %>%
  mutate(zipcode = 
           as.character(as.numeric(zipcode)))
  


## ------------------------------------------------------------------------------------------------------------
# Convert Logical to Numeric

## Find columns with "logical" data type
logical_list <- df_modfit %>%
  select_if(., is.logical) %>%
  colnames(.)

## Convert columns to numeric
df_modfit[, logical_list] <- sapply(df_modfit[, logical_list], as.numeric)


## OR use dynamic variables
# 
# for (element in logical_list){
# df_modfit <- df_modfit %>%
#   mutate(!!element := as.numeric(!!as.name(element)))
# }


## ------------------------------------------------------------------------------------------------------------


## Update missing_vals tables (since we derived a few)
missing_vals <- data.frame()

for (col in colnames(df_modfit)){
  missing_vals <- missing_vals %>%
    rbind(data.frame(column = col, null_value = sum(is.na(df_modfit[, col]))))
}

## Get list of columns with missing values
## we use the table created earlier
missing_list <- missing_vals %>%
  filter(null_value != 0) %>%
  arrange(desc(null_value)) %>%
  pull(column)

missing_list


## ------------------------------------------------------------------------------------------------------------
## Null value interactions
naniar::gg_miss_upset(df_modfit, nsets = naniar::n_var_miss(df_modfit), nintersects = NA)


## ------------------------------------------------------------------------------------------------------------
# host_response_Rate

## histogram_plot
df_modfit %>%
  ggplot(aes(x = host_response_rate)) +
  geom_histogram(color = "black", fill = "grey", size = 1) +
  theme_bw() 


# Replace NAs with mean value

df_modfit$host_response_rate[is.na(df_modfit$host_response_rate)] <-
  round(mean(df_modfit$host_response_rate, na.rm = TRUE))

any(is.na(df_modfit$host_response_rate))

# Checking New distribution

df_modfit %>%
  ggplot(aes(x = host_response_rate)) +
  geom_histogram(color = "black", fill = "grey", size = 1) +
  theme_bw() 


## ------------------------------------------------------------------------------------------------------------
# review_scores_rating and review_time_diff

## Null value interactions
naniar::gg_miss_upset(df_modfit, nsets = 2, nintersects = NA)


## histogram plot

p1 <- df_modfit %>%
  ggplot(aes(x = review_scores_rating))  +
  geom_histogram(color = "black", fill = "grey", size = 1) +
  theme_bw() 

p2 <- df_modfit %>%
  ggplot(aes(x = review_time_diff))  +
  geom_histogram(color = "black", fill = "grey", size = 1) +
  theme_bw() 

gridExtra::grid.arrange(p1, p2)


# Replace NAs 

## since concluded that reviews are not received
## with min value for review_scores_rating
## and max value for review_time_diff

df_modfit$review_scores_rating[is.na(df_modfit$review_scores_rating)] <- 0

df_modfit$review_time_diff[is.na(df_modfit$review_time_diff)] <-
  max(df_modfit$review_time_diff, na.rm = TRUE)

any(is.na(df_modfit$review_scores_rating))
any(is.na(df_modfit$review_time_diff))


## ------------------------------------------------------------------------------------------------------------
# Treat neighbhourhood 

## Check for overlaps
naniar::gg_miss_upset(df_modfit, nsets = 3)


## Categories in neighbourhood

head(sort(table(df_modfit$neighbourhood), decreasing = TRUE))
print(c("Unique categories including NA", length(unique(df_modfit$neighbourhood))))

## We replace null values with "Unknown" category
df_modfit$neighbourhood[is.na(df_modfit$neighbourhood)] <-
  "Unknown"

any(is.na(df_modfit$neighbourhood))


## ------------------------------------------------------------------------------------------------------------
# Treat zipcode

## Check for overlaps
naniar::gg_miss_upset(df_modfit, nsets = 3, nintersects = NA)

### No significant overlaps

## Categories in zipcode

head(sort(table(df_modfit$zipcode), decreasing = TRUE))
print(c("Unique categories including NA", length(unique(df_modfit$zipcode))))

## We replace null values with "Unknown" category
df_modfit$zipcode[is.na(df_modfit$zipcode)] <-
  "Unknown"

any(is.na(df_modfit$zipcode))


##------------------------------------------------------------------------------------------------------------
# Treat bathrooms

## Check for overlaps
naniar::gg_miss_upset(df_modfit, nsets = 3, nintersects = NA)


## Check distribution

df_modfit %>%
  ggplot(aes(x = bathrooms))  +
  geom_bar(color = "black", fill = "grey",  size = 1) +
  theme_bw() 

## Replace with mean
df_modfit$bathrooms[is.na(df_modfit$bathrooms)] <-
  round_any(mean(df_modfit$bathrooms, na.rm = TRUE), 0.5)
  
### like with host_response_Rate, bathrooms are not absent 
### in the property, only data is unavailable
### hence replaced with mean


## ------------------------------------------------------------------------------------------------------------
# Treat hosting_since, host_identity_verified and host_has_profile_pic

## Check for overlaps
naniar::gg_miss_upset(df_modfit, nsets = 4, nintersects = NA)

### We see significant overlaps

## Check distributions
p1 <- df_modfit %>%
  ggplot(aes(x = hosting_since)) +
  geom_histogram(color = "black", fill = "grey", size = 1) +
  theme_bw()

p2 <- df_modfit %>%
  ggplot(aes(x = host_identity_verified)) +
  geom_bar(color = "black", fill = "grey", size = 1) +
  theme_bw()

p3 <- df_modfit %>%
  ggplot(aes(x = host_has_profile_pic)) +
  geom_bar(color = "black", fill = "grey", size = 1) +
  theme_bw()

grid.arrange(p1,p2,p3, nrow = 2)

## Replace values, we use minimum values for all
## after concluding the entity is absent

df_modfit$hosting_since[is.na(df_modfit$hosting_since)] <- 
  min(df_modfit$hosting_since, na.rm = TRUE)

df_modfit$host_identity_verified[is.na(df_modfit$host_identity_verified)] <-
  min(df_modfit$host_identity_verified, na.rm = TRUE)

df_modfit$host_has_profile_pic[is.na(df_modfit$host_has_profile_pic)] <-
  min(df_modfit$host_has_profile_pic, na.rm = TRUE)

## check NAs to confirm
any(is.na(df_modfit$hosting_since))
any(is.na(df_modfit$host_identity_verified))
any(is.na(df_modfit$host_has_profile_pic))


## ------------------------------------------------------------------------------------------------------------
# Treat beds and bedrooms

## Check for overlaps
naniar::gg_miss_upset(df_modfit, nsets = 2, nintersects = NA)

## No significant overlaps. We treat them seperately

## Check values
df_modfit %>%
  ggplot(aes(x = beds)) +
  geom_bar(color = "black", fill = "grey", size = 1) +
  theme_bw()


## Replace using mean value,
## since data not recorded

df_modfit$beds[is.na(df_modfit$beds)] <-
  round(mean(df_modfit$beds, na.rm = TRUE))

##check NAs to confirm
any(is.na(df_modfit$beds))


## Check values
df_modfit %>%
  ggplot(aes(x = bedrooms)) +
  geom_bar(color = "black", fill = "grey", size = 1) +
  theme_bw()

## Replace using mean value,
## since data not recorded

df_modfit$bedrooms[is.na(df_modfit$bedrooms)] <-
  round(mean(df_modfit$bedrooms, na.rm = TRUE))

##check NAs to confirm
any(is.na(df_modfit$bedrooms))

##check for all dataset

any(is.na(df_modfit))


## ------------------------------------------------------------------------------------------------------------
# Free up space

rm(missing_vals, p1, p2, p3, x,
   col, date_list, logical_list, missing_list,
   element, i, k)


## Exploratory Data Analysis -----------------------------------------------------------------------------------
# Create factor list except amenities and id
factor_list <- c("property_type", 
                 "room_type", 
                 "accommodates", 
                 "bathrooms",
                 "bed_type",
                 "cancellation_policy",
                 "cleaning_fee",
                 "city",
                 "host_has_profile_pic",
                 "host_identity_verified",
                 "instant_bookable",
                 "neighbourhood",
                 "zipcode",
                 "bedrooms",
                 "beds")


## ------------------------------------------------------------------------------------------------------------
## unique_count_length

unique_counts <- vector(mode = "numeric")

for(col in factor_list){
  unique_counts <- append(unique_counts,
                          length(unique(df_modfit[[col]])))
}

data.frame(col = factor_list,
           unique_count_length = unique_counts)


# Value counts

## table and proportion table
### exclude neighbourhood, zipcode (for illustration)
### due to large number of values

for(col in factor_list){
  data.frame(table(df_modfit[[col]])) %>%
    dplyr::rename(!!col := Var1)%>%
    arrange(desc(Freq)) %>%
    cbind(., data.frame(prop.table(table(df_modfit[[col]]))) %>%
            arrange(desc(Freq)) %>%
            dplyr::rename(proportion = Freq) %>%
            select(proportion)) 
}



## ------------------------------------------------------------------------------------------------------------
# List for different type of variables

cat3_var <- c("property_type", "room_type", "bed_type", 
              "city", "neighbourhood","zipcode")

cat2_var <- c("cleaning_fee", "host_has_profile_pic", 
              "host_identity_verified", "instant_bookable",
              all_of(amenities_list))

cont_var <- df_modfit %>%
  select(-c(all_of(factor_list), all_of(amenities_list),
            "id")) %>%
  colnames(.)

ordcat_var <- c("accommodates", "bathrooms", "bedrooms", 
                "beds", "cancellation_policy")



## ------------------------------------------------------------------------------------------------------------
# Correlation amongst continuous and ordinal categorical variables

## Correlation matrix
cont_ordcat_corr <- df_modfit %>%
  select(c(all_of(cont_var), all_of(ordcat_var))) %>%
  
  ##label encode ordcat vars
  mutate_at(.,
            vars(all_of(ordcat_var)),
            function(x){as.numeric(as.factor(x))}) %>%
  as.matrix(.) %>%
  rcorr(., type = "spearman")

## View matrix

cont_ordcat_corr$r  %>%
  data.frame()

## View p values for reference.
### Note: they wont be very accurate, 
### because of ties in the dataset, 
### but will give a good reference

cont_ordcat_corr$P %>%
  data.frame(.) 

## Correlation plot

cont_ordcat_corr$r %>%
  corrplot(., type = "upper", order = "hclust")
  



## ------------------------------------------------------------------------------------------------------------
# Correlation among nominal categorical variables

nomcatcorr <- function(variables, df){
  
  ## create dataframe
  df_cor <- data.frame(id = 1:length(variables))
  
  ## convert variables to factor for better computation speed
  df <- df %>%
    mutate_at(., vars(all_of(variables)), as.factor)
  
  ## update dataframe
  for(x in variables){
    l <- vector(mode = "numeric")
    for(y in variables){
      l <- append(l, cramerV(df[[x]], df[[y]]) %>%
                    round(., digits = 4))
    }
    df_cor[x] <- l
  }
  rownames(df_cor) <- all_of(variables)
  df_cor <- df_cor %>%
    select(-c("id"))

  ##return dataframe
  return(df_cor)
}

nomcat_corr_var <- df_modfit %>%
  select(c(all_of(cat3_var), all_of(cat2_var))) %>%
  colnames(.)


nomcat_corr <- nomcatcorr(variables = all_of(nomcat_corr_var), df = df_modfit)

## View correlations

nomcat_corr

## Correlation plot

nomcat_corr %>%
  as.matrix(.) %>%
  corrplot(., type = "upper", is.corr = F, number.font = 100)


## ------------------------------------------------------------------------------------------------------------
# Correlation between nominal and continuous variable (only target variable)
# Variables with 3+ categories

## create function
targetcat3_corr <- function(target, var_list, df){
  l1 <- vector()
  for (col in var_list){
    l1 <- append(l1, round(summary(aov(df[[target]] ~
                                        df[[col]]))[[1]]$'Pr(>F)'[1], 
                          digits = 5))
  }
  df_corr <- data.frame(pval = l1)
  rownames(df_corr) <- var_list
  
  return(df_corr)
}


## get correlation (p values)
target_nomcat3_corr <- targetcat3_corr("log_price", all_of(cat3_var), 
                                    df_modfit)

##view
target_nomcat3_corr 


## ------------------------------------------------------------------------------------------------------------
# Correlation between nominal and continuous variable (only target variable)
# Variables with 2 categories

## create function
targetcat2_corr <- function(target, var_list, df){
  l1 <- vector()
  l2 <- vector()
  for (col in var_list){
    l1 <- append(l1, round(cor.test(x = df[["log_price"]], 
                                    y = df[[col]])[[4]], 
                           digits = 4))
    l2 <- append(l2, round(cor.test(x = df[["log_price"]], 
                                    y = df[[col]])[[3]], 
                           digits = 5))
  }
  df_corr <- data.frame(names = var_list, correlation = l1, pval = l2)

  
  return(df_corr)
}


## get correlation (p values)
target_nomcat2_corr <- targetcat2_corr("log_price", 
                                       df_modfit %>% 
                                          select(all_of(cat2_var)) %>% 
                                          colnames(), 
                                       df_modfit)


view(target_nomcat2_corr)

## min max correlation vals
print(c(max(target_nomcat2_corr["correlation"]),
        min(target_nomcat2_corr["correlation"])))


## ------------------------------------------------------------------------------------------------------------
# Correlation between nominal and continuous variable (only target variable)
# Also checking within group

# One-hot encode variables
temp <- df_modfit %>%
  mutate_at(.,
            vars(all_of(ordcat_var), 
                 all_of(cat3_var)),
            as.factor)

dmy <- dummyVars("~.", data = temp)
df_1h <- data.frame(predict(dmy, newdata = temp))




## get correlation (p values) for all vars
target_nomcat2_corr <- targetcat2_corr("log_price", 
                                       df_1h %>% 
                                         select(-all_of(cont_var)) %>% 
                                         colnames(), 
                                       df_1h)

## view correlations
view(target_nomcat2_corr)

## ------------------------------------------------------------------------------------------------------------
# Continuous Variables EDA Graphs

## function for histogram

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

df_modfit %>%
  select(all_of(cont_var)) %>%
  pairs(., lower.panel = NULL, diag.panel = panel.hist)



# Target variable and Categorical Variables plot1
## use factor_list

p <- list()
i = 1
for (col in factor_list){
  p[[i]] <- df_modfit %>%
    select("log_price", col) %>%
    mutate_at(., vars(col), as.factor) %>%
    ggplot(aes(y = log_price, x = (!!as.name(col)))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust = 1))
  i = i+1
    
}

do.call(grid.arrange, p)


# Target variable and Categorical variables plot 2

p <- list()
i = 1
for (col in amenities_list){
  p[[i]] <- df_modfit %>%
    select("log_price", col) %>%
    mutate_at(., vars(col), as.factor) %>%
    ggplot(aes(y = log_price, x = (!!as.name(col)))) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, 
                                     vjust = 0.5, 
                                     hjust = 1)) +
    theme(axis.title = element_text(size = 15)) +
    theme(axis.text = element_text(size = 15))
  i = i+1
  
}

do.call(grid.arrange, c(p, ncol = 15))


## ------------------------------------------------------------------------------------------------------------
# Free up space
rm(amenity_count, cont_ordcat_corr, 
   df_1h, dmy, nomcat_corr, p, 
   target_nomcat2_corr, target_nomcat3_corr,
   temp, factor_list, i, unique_counts,
   panel.hist, targetcat2_corr, targetcat3_corr)


## Data Modeling ----------------------------------------------------------------------------------------------
# Data Preparation

## convert categorical variables (not one hot encoded) to factor
df_modfit <- df_modfit %>%
  mutate_at(.,
            vars(all_of(ordcat_var), 
                 all_of(cat3_var)),
            as.factor)


## remove unneeded variables
df_modfit <- df_modfit %>%
  select(-c("id"))



## ------------------------------------------------------------------------------------------------------------
# Feature Importance
### took 1.5-2 hrs to run

## Set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

rf <- train(log_price ~., 
            method = "rf",
            data = df_modfit,
            trControl = control,
            ntree = 10,
            tuneGrid = data.frame(mtry = seq(25,150,25)))


## ------------------------------------------------------------------------------------------------------------
# For variables with more than two categories, we check mean value

## Location categorical variables: neighbour, city, zipcode
print(c("neighbour", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^neighbour")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))

print(c("city", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^city")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))

print(c("zipcode", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^zipcode")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))



## Room accommodation categorical variables: accommodates, beds, bedrooms, bathrooms
print(c("accommodates", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^accommodates")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))

print(c("beds", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^beds")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))

print(c("bedrooms", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^bedrooms")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))

print(c("bathrooms", varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  filter(str_detect(name, "^bathrooms")) %>%
  .$Overall %>%
  mean(.) %>%
    round(., digits = 4)))



## amenity list
varImp(rf)$importance %>%
  mutate(name = rownames(.)) %>%
  mutate(name = str_replace_all(name, pattern = "`", replacement = "")) %>%
  filter(name %in% amenities_list) %>%
  arrange(desc(Overall))


## ------------------------------------------------------------------------------------------------------------
# Modify dataset

## Derive variables
df_modfit2 <- df_modfit %>%
  as.data.frame(.) %>%
  mutate(.,INTERNET = 
           as.numeric(df_modfit$Internet == 1 | 
                        df_modfit$Wireless_Internet == 1),
         TRANSLATION_MISSING = 
           as.numeric(df_modfit$translation_missing_en_hosting_amenity_49 == 1|
                        df_modfit$translation_missing_en_hosting_amenity_50 ==1),
         WASHER_DRYER = 
           as.numeric(df_modfit$Washer == 1|
                        df_modfit$Dryer == 1 |
                        df_modfit$Washer_Dryer == 1),
         PETS_ALLOWED = 
           as.numeric(df_modfit$Pets_allowed ==1 |
                        df_modfit$Pets_live_on_this_property == 1|
                        df_modfit$Dog == 1|
                        df_modfit$Cat ==1 |
                        df_modfit$Other == 1 |
                        df_modfit$Other_pet == 1),
         BEDROOM_FACILITIES = 
           as.numeric(df_modfit$Hot_water == 1|
                        df_modfit$Bed_linens == 1|
                        df_modfit$Extra_pillows_and_blankets == 1),
         COOKING_FACILITIES = 
           as.numeric(df_modfit$Cooking_basics == 1 |
                        df_modfit$Refrigerator == 1 |
                        df_modfit$Dishes_and_silverware == 1|
                        df_modfit$Microwave == 1 |
                        df_modfit$Oven == 1 |
                        df_modfit$Stove == 1 |
                        df_modfit$Dishwasher == 1),
         ACCESS_FRIENDLY = 
           as.numeric(df_modfit$Step_free_access == 1 |
                        df_modfit$Wide_clearance_to_bed == 1|
                        df_modfit$Accessible_height_bed == 1 |
                        df_modfit$Wide_doorway == 1 |
                        df_modfit$Accessible_height_toilet == 1 |
                        df_modfit$Wide_entryway == 1 |
                        df_modfit$Wide_hallway_clearance == 1 |
                        df_modfit$Flat == 1 |
                        df_modfit$Well_lit_path_to_entrance == 1),
         CHILDREN_FACILITIES = 
           as.numeric(df_modfit$Childrens_dinnerware == 1 |
                        df_modfit$Childrens_books_and_toys == 1),
         HIGH_CHAIR =
           as.numeric(df_modfit$High_chair == 1 |
                        df_modfit$Pack_n_Play_travel_crib == 1),
         PATHWAY = 
           as.numeric(df_modfit$Flat_smooth_pathway_to_front_door == 1|
                        df_modfit$Path_to_entrance_lit_at_night == 1 |
                        df_modfit$Firm_matress == 1 |
                        df_modfit$Firm_mattress == 1),
         WASH_FACILITIES =
           as.numeric(df_modfit$Hand_soap == 1 |
                        df_modfit$Body_soap == 1 |
                        df_modfit$Hand_or_paper_towel == 1 |
                        df_modfit$Toilet_paper == 1 |
                        df_modfit$Bath_towel == 1)) 




## Remove unneeded columns
df_modfit2 <- df_modfit2 %>%
  select(-c(beds, accommodates, bathrooms, 
            review_scores_rating,
            neighbourhood, zipcode,
            latitude,
            c(Wireless_Internet, Internet),
            c(translation_missing_en_hosting_amenity_50,
              translation_missing_en_hosting_amenity_49),
            c(Washer, Dryer, Washer_Dryer),
            c(Pets_allowed, Pets_live_on_this_property,
              Dog, Cat, Other, Other_pet),
            c(Hot_water, Bed_linens, Extra_pillows_and_blankets),
            Lockbox,
            c(Coffee_maker, Refrigerator, Dishes_and_silverware, Microwave,
            Oven, Stove, Dishwasher, Cooking_basics),
            c(Step_free_access, Wide_clearance_to_bed, Accessible_height_bed,
              Wide_doorway, Accessible_height_toilet, Wide_entryway,
              Wide_hallway_clearance, Flat, Well_lit_path_to_entrance),
            Luggage_dropoff_allowed,
            BBQ_grill,
            c(Childrens_dinnerware, Childrens_books_and_toys),
            c(High_chair, Pack_n_Play_travel_crib),
            c(Firm_matress, Firm_mattress, Path_to_entrance_lit_at_night, 
              Flat_smooth_pathway_to_front_door),
            c(Bath_towel, Hand_soap, Body_soap, Hand_or_paper_towel, Toilet_paper)
            ))

## Change colnames to suit model preferences (rpart)
colnames(df_modfit2) <- make.names(colnames(df_modfit2))



## ------------------------------------------------------------------------------------------------------------
# Split dataset into training and validation sets

## Set seed
set.seed(1, sample.kind = "Rounding")

## Get test indices
test_index <- createDataPartition(y = df_modfit2$log_price,
                                  p = 0.2,
                                  times = 1,
                                  list = FALSE)

## Create training and testing datasets
df_training <- df_modfit2 %>% slice(-test_index)
temp <- df_modfit2 %>% slice(test_index)

## Make sure all categories in test set are present in train set
df_validation <- temp %>%
  semi_join(df_training, by = "property_type") 

## Add removed rows back to training set
removed <- anti_join(temp, df_validation)
df_training <- rbind(df_training, removed)


# Split dataset for intermediate test set

## Set seed
set.seed(1, sample.kind = "Rounding")

## Get test indices
test_index <- createDataPartition(y = df_training$log_price,
                                  p = 0.2,
                                  times = 1,
                                  list = FALSE)

## Create training and testing datasets
df_train <- df_training %>% slice(-test_index)
temp <- df_training %>% slice(test_index)

## Make sure all categories in test set are present in train set
df_test <- temp %>%
  semi_join(df_train, by = "property_type")

## Add removed rows back to training set
removed <- anti_join(temp, df_test)
df_train <- rbind(df_train, removed)



## ------------------------------------------------------------------------------------------------------------
# R-squared function definition

R2 <- function(preds, actual){
  rss <- sum((preds - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  rsq <- 1 - rss/tss
  
  return(rsq)
}


## ------------------------------------------------------------------------------------------------------------
# Using mean to predict prices

## fit model
naive_model <- function(df, target_var){
  return(mean(df[[target_var]]))
}

## Predict 
y_hat_naive <- naive_model(df_train, "log_price")




## Create results table
results <- data.frame(method = "Only Average", 
                      RMSE = RMSE(y_hat_naive, df_test$log_price),
                      R2 = R2(y_hat_naive, df_test$log_price) %>%
                        round(., digits = 4))

## view results
results


## ------------------------------------------------------------------------------------------------------------
# Random Forest iteration 1
### took about 1.5-2 hrs to run

##set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
rf1 <- train(log_price ~., 
            method = "rf",
            data = df_train,
            trControl = control,
            ntree = 50)

## Evaluate model
rf1


## ------------------------------------------------------------------------------------------------------------
# Random Forest iteration 2
### took about 1.5-2 hrs to run

## set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
rf2 <- train(log_price ~., 
            method = "rf",
            data = df_train,
            trControl = control,
            ntree = 50,
            tuneGrid = data.frame(mtry = seq(75,90,3)))

## Evaluate model
rf2

#Check variable importance

view(varImp(rf2)$importance)

## Mean value
mean(varImp(rf2)$importance$Overall)

## Plot graph

as.data.frame(varImp(rf2)$importance) %>%
  mutate(name = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  ggplot(aes(x = 1:length(rownames(varImp(rf2)$importance)), y = Overall)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  theme_bw() +
  xlab("variables") +
  scale_y_continuous(breaks = seq(0,100,2)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))

## Filter features with little importance

unimportant_vars_rf <- varImp(rf2)$importance %>%
  mutate(name = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  filter(Overall <=1) %>%
  mutate(name = str_replace_all(name, pattern = "`", replacement = "")) %>%
  filter(!str_detect(string = name, 
                     pattern =
                       "^city|^bedrooms|^property_type|^bed_type|^room_type|^cancellation_policy")) %>%
  .$name

## View top important variables and count
df_train %>%
  select(-all_of(unimportant_vars_rf), -c("log_price")) %>%
  colnames(.) %>%
  length() %>%
  print()

df_train %>%
  select(-all_of(unimportant_vars_rf), -c("log_price")) %>%
  colnames(.) %>%
  head() %>%
  print()


## ------------------------------------------------------------------------------------------------------------
# Random Forest Iteration 3
### took about 1.5-2 hrs to run

## set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
rf3 <- train(log_price ~., 
            method = "rf",
            data = df_train %>%
              select(-all_of(unimportant_vars_rf)),
            trControl = control,
            ntree = 50,
            tuneGrid = data.frame(mtry = seq(35,50, 3)))

## Evaluate model
rf3

##plot

plot(rf3)


# Predict for intermediate test set

## get y_hat
y_hat_rf <- predict(rf3, newdata = df_test)

## update results
results <- results %>%
  rbind(data.frame(method = "Random Forest", 
                      RMSE = RMSE(y_hat_rf, df_test$log_price),
                      R2 = R2(y_hat_rf, df_test$log_price)))

results


## ------------------------------------------------------------------------------------------------------------
# Decision Tree Iteration 1

## Set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
decisiontree1 <- train(log_price ~.,
                      data = df_train,
                      method = "rpart",
                      trControl = control)

## Evaluate model
decisiontree1


## ------------------------------------------------------------------------------------------------------------
# Decision Tree Iteration 2

## Set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
decisiontree2 <- train(log_price ~.,
                      data = df_train,
                      method = "rpart",
                      trControl = control,
                      control = rpart.control(minsplit = 2,
                                              minbucket = 1),
                      tuneGrid = data.frame(cp = seq(0.001, 0.035, 0.002)))

## Evaluate model
decisiontree2



# plot tree

plot(decisiontree2$finalModel)
text(decisiontree2$finalModel, cex = 1.5)


# Check Variable Importance

as.data.frame(varImp(decisiontree2)$importance) %>%
  mutate(name = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  ggplot(aes(x = 1:length(rownames(varImp(decisiontree2)$importance)), y = Overall)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +
  theme_bw() +
  xlab("variables") +
  scale_y_continuous(breaks = seq(0,100,2)) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text = element_text(size = 18))



unimportant_vars_decisiontree <- varImp(decisiontree2)$importance %>%
  mutate(name = rownames(.)) %>%
  arrange(desc(Overall)) %>%
  filter(Overall <=1) %>%
  mutate(name = str_replace_all(name, pattern = "`", replacement = "")) %>%
  filter(!str_detect(string = name, 
                     pattern =
                       "^city|^bedrooms|^property_type|^bed_type|^room_type|^cancellation_policy")) %>%
  .$name

df_train %>%
  select(-all_of(unimportant_vars_decisiontree), -c("log_price")) %>%
  colnames(.) %>%
  length(.) %>%
  print(.)

df_train %>%
  select(-all_of(unimportant_vars_decisiontree), -c("log_price")) %>%
  colnames(.) %>%
  head(.) %>%
  print(.)


## ------------------------------------------------------------------------------------------------------------
# Decision Tree Iteration 3

## Set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
decisiontree3 <- train(log_price ~.,
                      data = df_train %>%
                        select(-all_of(unimportant_vars_decisiontree)),
                      method = "rpart",
                      trControl = control,
                      control = rpart.control(minsplit = 2,
                                              minbucket = 1),
                      tuneGrid = data.frame(cp = seq(0.001, 0.035, 0.002)))

## Evaluate model
decisiontree3

# plot results

plot(decisiontree3)

# plot tree

plot(decisiontree3$finalModel)
text(decisiontree3$finalModel, cex = 1.5)

# Predict for intermediate test set

## get y_hat
y_hat_rpart <- predict(decisiontree3, newdata = df_test)

## update results
results <- results %>%
  rbind(data.frame(method = "Decision tree", 
                      RMSE = RMSE(y_hat_rpart, df_test$log_price),
                      R2 = R2(y_hat_rpart, df_test$log_price)))

results


## ------------------------------------------------------------------------------------------------------------
# normalize for linear regression

## for train set
preproc1 <- preProcess(df_train[, which(colnames(df_train) %in% cont_var)[-1]], method = c("range"))

df_train_norm <- predict(preproc1, df_train[, which(colnames(df_train) %in% cont_var)[-1]])

df_train_norm <- df_train_norm %>% cbind(df_train[, -which(colnames(df_train) %in% cont_var)[-1]])

## for intermediate test set
preproc1 <- preProcess(df_test[, which(colnames(df_test) %in% cont_var)[-1]], method = c("range"))

df_test_norm <- predict(preproc1, df_test[, which(colnames(df_test) %in% cont_var)[-1]])

df_test_norm <- df_test_norm %>% cbind(df_test[, -which(colnames(df_test) %in% cont_var)[-1]])


## ------------------------------------------------------------------------------------------------------------
# Linear Regression Iteration 1
## only two iterations here since no tuning

## Set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
linearfit1 <- train(log_price ~.,
                      data = df_train_norm,
                      method = "lm",
                      trControl = control)


## Evaluate model
linearfit1

#get coef table

## extract coefficients
varsig <- coef(summary(linearfit1))

## getting names of significant variables
mod1_varsig <- data.frame(pval = varsig[, 4] %>%
             round(., digits = 4))

## View insignificant vars
invisible(mod1_varsig %>%
 mutate(name = rownames(.)) %>%
  arrange(pval) %>%
  filter(pval >0.05 ) %>%
  mutate(name = str_replace_all(name, pattern = "`", replacement = "")) %>%
  filter(!str_detect(string = name, 
                     pattern =
                       "^city|^bedrooms|^property_type|^bed_type|^room_type|^cancellation_policy")))

## Get var names
insignificantvars <- mod1_varsig %>%
 mutate(name = rownames(.)) %>%
  arrange(pval) %>%
  filter(pval >0.05 ) %>%
  mutate(name = str_replace_all(name, pattern = "`", replacement = "")) %>%
  filter(!str_detect(string = name, 
                     pattern =
                       "^city|^bedrooms|^property_type|^bed_type|^room_type|^cancellation_policy")) %>%
  .$name



## ------------------------------------------------------------------------------------------------------------
# Linear Regression Iteration 2

## Set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model
linearfit2 <- train(log_price ~.,
                      data = df_train_norm %>%
                      select(-all_of(insignificantvars)),
                      method = "lm",
                      trControl = control)

## Evaluate model
linearfit2

# Predict for intermediate test set

## get y_hat
y_hat_lm <- predict(linearfit2, newdata = df_test_norm)

## update results
results <- results %>%
  rbind(data.frame(method = "Linear Regression", 
                      RMSE = RMSE(y_hat_lm, df_test$log_price),
                      R2 = R2(y_hat_lm, df_test$log_price)))

results


## ------------------------------------------------------------------------------------------------------------
# Models used to from ensemble

linearfit2
decisiontree2

# Predict for intermediate test set: average used

## get y_hat
y_hat_ensemble <- (y_hat_rpart + y_hat_lm)/2

## update results
results <- results %>%
  rbind(data.frame(method = "Ensemble Model: Decision Tree + Linear Regression", 
                      RMSE = RMSE(y_hat_ensemble, df_test$log_price),
                      R2 = R2(y_hat_ensemble, df_test$log_price)))

results
### best results shown by random forest. using it to validate

## Final Results -----------------------------------------------------------------------------------------------
# Validation



## set seed
set.seed(1, sample.kind = "Rounding")

## cv folds
control <- trainControl(method = "cv", number = 3, p = 0.8)

## fit model on entire training set
rf3 <- train(log_price ~., 
            method = "rf",
            data = df_training %>%
              select(-all_of(unimportant_vars_rf)),
            trControl = control,
            ntree = 50,
            tuneGrid = data.frame(mtry = 41))

## Evaluate model
rf3

# Final model metrics

## y_hat for Random Forest
y_hat_rf <- predict(rf3, newdata = df_validation)

## Final metrics table

results_val <- data.frame(method = "Random Forest",
                         RMSE = RMSE(y_hat_rf, df_validation$log_price),
                         R2 = R2(y_hat_rf, df_validation$log_price) )



## update with naive model for comparison
y_hat_naive_val <- naive_model(df_training, "log_price")

results_val <- results_val %>%
  rbind(data.frame(method = "Only average",
                   RMSE =RMSE(y_hat_naive_val, df_validation$log_price),
                   R2 = R2(y_hat_naive_val, df_validation$log_price) %>%
                     round(.,digits = 4)))

## Final results table
results_val







