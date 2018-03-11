### Data Mining using dplyr
### Author: Deepak Agarwal

## Health care data 
## Data Source - https://data.medicare.gov/data/physician-compare

# load the library
library(dplyr)

# read the data

data <- read.csv("Physician_Compare_National_Downloadable_File.csv")



# check first few rows of data

head(data)

# check unique ids for NPI column

data %>% distinct(NPI) %>% tally()

# check unique ids for PAC.ID column

data %>% distinct(PAC.ID) %>% tally()

# check unique ids for both NPI, PAC.ID column combined

data %>% distinct(NPI,PAC.ID) %>% tally()


# Print the gender wise data (number of males,females)

data %>% group_by(Gender) %>% summarise(n=n())


# subset the data on the basis of unique PAC.ID to find male-female ratio

data_distinct <- data %>% distinct(PAC.ID,.keep_all = T)

# display the ratio (rounded upto four decimals) of males to females  

data_distinct %>% 
  group_by(Gender) %>%
      summarise(n=n()) %>% 
            mutate(ratio=format(round(n/n[Gender=='F'],4),nsmall=4)) %>% 
                                                    filter(ratio==max(ratio)) 



# find the credential with highest male-female ratio

credential_female_ratio <- data_distinct %>% 
                             group_by(Credential,Gender) %>% 
                                            summarise(n=n()) %>% 
                                                 mutate(ratio=format(round((1/(n/n[Gender=='F'])),4),nsmall=4)) %>% 
                                                                                filter(Gender=='M',ratio==max(ratio)) %>% 
                                                                                                    select(Credential,ratio)

# display the credential

credential_female_ratio[which.max(credential_female_ratio$ratio),] 



# load the performance data for the physicians

performance_data <- read.csv("Physician_Compare_2015_Individual_EP_Public_Reporting___Performance_Scores.csv")

# display the data

head(performance_data)


# display the standard deviation where the average performance measure rate is greater than 10

performance_data %>% 
  group_by(PAC.ID) %>% 
    summarise(avg_perf_measure=mean(Measure.Performance.Rate)) %>% 
                                      filter(avg_perf_measure>=10) %>% 
                                          summarise(sd=format(sd(avg_perf_measure),nsmall = 10))



# find the practitioners who have at least 10 performance measures

practitioner_atleast10_measures <- performance_data %>% 
                                          group_by(PAC.ID) %>% 
                                              summarise(perf_measures=n()) %>% 
                                                        filter(perf_measures>=10)

head(practitioner_atleast10_measures)

# subset performance data for practitioners who have at least 10 performance measures

performance_data_atleast10measures <- performance_data %>% 
                                        filter(PAC.ID %in% practitioner_atleast10_measures$PAC.ID) %>% 
                                                                    select(PAC.ID,Measure.Performance.Rate)


# subset physician data for PAC.ID and credential

data_credentials <- data %>% select(PAC.ID,Credential)


# join the performance data and credential data to get credentials for practitioners who have at least 10 performance measures

data_credentials_practitioner_atleast_10measure <- left_join(performance_data_atleast10measures,data_credentials,by="PAC.ID")


# calculate the absolute difference between average performance rate of practitioners with credentials 'MD' and 'NP'

data_credentials_practitioner_atleast_10measure %>% 
                                group_by(Credential) %>% 
                                  summarise(avg_perf_measure=mean(Measure.Performance.Rate)) %>% 
                                                           filter(Credential %in% c("MD","NP")) %>% 
                                                             mutate(diff=format(abs(avg_perf_measure[1]-avg_perf_measure[2]),
                                                                                 nsmall = 4))



# find if the average difference between performance measure of 'MD' and 'NP' is significant or not

practitioner_10measure_md <- data_credentials_practitioner_atleast_10measure %>% 
                                                          filter(Credential=="MD") %>% 
                                                                    select(Measure.Performance.Rate)

practitioner_10measure_np <- data_credentials_practitioner_atleast_10measure %>% 
                                                        filter(Credential=="NP")  %>% 
                                                            select(Measure.Performance.Rate)

# perform two sample t-test

ttest <- t.test(practitioner_10measure_md,practitioner_10measure_np,var.equal = T)

# dislay the results

ttest

# display the actual p-value

format(ttest$p.value,scientific = F)


## find the average performance measure of data practitioners whose graduation year is between 1973-2003 both inclusive

# subset the practitoner data for the said years

data_practitioner_1973_2003 <- subset(data_distinct, Graduation.year>=1973 & Graduation.year<=2003)


# join subsetted practitoner data with the performance data using PAC.ID

performance_data_atleast10measures_1973_2003 <- inner_join(performance_data_atleast10measures,
                                                           data_practitioner_1973_2003,by="PAC.ID")

# find the average performance measure year-wise

performance_data_atleast10measures_1973_2003 %>% 
                          group_by(Graduation.year) %>% 
                                summarise(avg_perf_measure=mean(Measure.Performance.Rate))


## find the linear relationship between performance measure and graduation year for 1973-2003

# get the target and predictor variables

y <- performance_data_atleast10measures_1973_2003$Measure.Performance.Rate
x <- performance_data_atleast10measures_1973_2003$Graduation.year

# fit the simple linear regresion model

fit <- lm(y~x)

# display the summary of model

summary(fit)

# find the p-value of the linear regression model

linregpval <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  fstat <- summary(modelobject)$fstatistic
  pval <- pf(fstat[1],fstat[2],fstat[3],lower.tail=F)
  attributes(pval) <- NULL
  return(pval)
}

# display the p-value

format(linregpval(fit),scientific = F)


