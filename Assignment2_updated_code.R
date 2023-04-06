#Assignment-2

#Loading the libraries

rm(list=ls())
library("data.table")
library("ggplot2")
library(psych)

#########Question1#############################


#use setwd while running the codes, to set working directory to access the csv files

# Part a
earthquakes = fread(file = "earthquakes.csv",
                    na.strings = c("NA", ""), 
                    sep = "auto",
                    stringsAsFactors = FALSE,
                    data.table = TRUE
)
earthquakes

earthquakes_subset <- earthquakes[State %in% c("Alaska", "California") & Year %in% 2002:2011]
earthquakes_subset


# Part b

earthquakes_stats <- earthquakes_subset[, .(
  mean = mean(Magnitude, na.rm = TRUE),
  median = median(Magnitude, na.rm = TRUE),
  std = sd(Magnitude, na.rm = TRUE),
  min = min(Magnitude, na.rm = TRUE),
  max = max(Magnitude, na.rm = TRUE),
  percentile_25th = quantile(Magnitude, 0.25, na.rm = TRUE),
  percentile_75th = quantile(Magnitude, 0.75, na.rm = TRUE)
), by = .(Year, State)][order(Year)]


earthquakes_stats[is.na(std), std := 0]
earthquakes_stats



# Part c

# create a vector of years from 2002 to 2011
years <- 2002:2011

# create an empty list to store the subsets
subset_list <- list()

# use for loop to apply subset function to each year in the vector
for (y in years) {
  subset_list[[y-2001]] <- earthquakes_stats[Year == y]
}
subset_list

# Part d

# reshape the data table to wide format
earthquakes_table <- dcast(earthquakes_stats, Year ~ State, value.var = c("mean", "median", "std", "min", "max", "percentile_25th", "percentile_75th"))


# Part e

Alaska <- earthquakes_stats[State == "Alaska"]
California <- earthquakes_stats[State == "California"]

# Create separate plots for each state

plot1<-ggplot(Alaska,aes(x = Year, y = mean)) +
  geom_line() +
  scale_y_continuous(name = "Average Magnitude") +
  ggtitle("Alaska")+theme_classic()

plot2<-ggplot(California,aes(x = Year, y = mean)) +
  geom_line() +
  scale_y_continuous(name = "Average Magnitude") +
  ggtitle("California")+theme_classic()

# Display the plots side by side
gridExtra::grid.arrange(plot1, plot2, ncol = 2)

# Part f
Alaska <- earthquakes[State == "Alaska"]
California <- earthquakes[State == "California"]


t_test_result <- t.test(California$Magnitude, Alaska$Magnitude, alternative = "greater", var.equal = FALSE)

t_test_result

#Question 2

#Please use setwd to set your working directory before running the below chunk of code
study_gpa = fread(file = "study_gpa.csv",
                  na.strings = c("NA", ""), 
                  sep = "auto",
                  stringsAsFactors = FALSE,
                  data.table = TRUE
)
study_gpa


# Part a
hours_of_study <- study_gpa[, AveTime]


ggplot(data = study_gpa, aes(x =hours_of_study)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "skyblue", color = "black") + 
  labs(x = "Average Study time(hrs)", y = "Density") + 
  theme_bw() +
  
  # Add a density plot
  geom_density(aes(y = ..density..), color = "blue") +
  
  # Add a normal curve
  stat_function(fun = dnorm, args = list(mean = mean(hours_of_study), sd = sd(hours_of_study)), aes(color = "Normal"), size = 1) + 
  scale_color_manual(values = c("red", "blue")) 

# Part b
describe(hours_of_study)

# Perform normality tests
shapiro.test(hours_of_study) # Shapiro-Wilk test
library(nortest) 
ad.test(hours_of_study) # Anderson-Darling test
ks.test(hours_of_study, "pnorm") # Kolmogorov-Smirnov test

# Plot histogram and Q-Q plot
#Run the below 4 lines of code together
par(mfrow=c(1,2))
hist(hours_of_study, main="Histogram")
qqnorm(hours_of_study)
qqline(hours_of_study)


# Part c

# Subset Section 2 of study_gpa
study_gpa_2 <- study_gpa[Section == 2]

# Compute correlation matrix for AveTime, Units, and GPA
cor_matrix <- cor(study_gpa_2[, .(AveTime, Units, GPA)])

# Perform hypothesis tests for correlations between AveTime/Units, AveTime/GPA, and Units/GPA
cor_test_1 <- cor.test(study_gpa_2$AveTime, study_gpa_2$Units)
cor_test_2 <- cor.test(study_gpa_2$AveTime, study_gpa_2$GPA)
cor_test_3 <- cor.test(study_gpa_2$Units, study_gpa_2$GPA)

# Print results
print(cor_matrix)
print(cor_test_1)
print(cor_test_2)
print(cor_test_3)


#Question 3

# Part a
#Please use setwd to set your working directory before running the below chunk of code
vite = fread(file = "vite.csv",
             na.strings = c("NA", ""), 
             sep = "auto",
             stringsAsFactors = FALSE,
             data.table = TRUE
)
vite

# Part b
vite_pivot <- dcast(vite, ID + Treatment ~ Visit, value.var = "Plaque")
vite_pivot

# Part c
# subset the data by Treatment == 1
treat <- vite_pivot[Treatment == 1]

# perform paired t-test 
t_test_result <- t.test(treat[[5]], treat[[3]], alternative = "less", paired = TRUE, var.equal = FALSE)

t_test_result

# Part d
treat <- vite_pivot[Treatment == 1]
treat[, difference := treat[[5]]-treat[[3]]]

control_group<-vite_pivot[Treatment==0]
control_group[, difference := control_group[[5]]- control_group[[3]]]

t_test_result <- t.test(treat$difference,control_group$difference , alternative = "less", var.equal = FALSE)
t_test_result

# Part e
# Please, refer to Part d

# Part f
treat<- vite[Treatment==1]
control_group<- vite[Treatment==0]

# Test for alcohol
alcohol_test<-t.test(treat$Alcohol, control_group$Alcohol,alternative = "two.sided", var.equal = FALSE )
alcohol_test

# Test for smoke
smoke_test<-t.test(treat$Smoke, control_group$Smoke,alternative = "two.sided", var.equal = FALSE )
smoke_test
