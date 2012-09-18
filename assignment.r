*1. Load the biofam data set that comes with the TraMineR library.*
> library (TraMineR)
> data(biofam)

*2. Print the variable names.*
> names (biofam)

*3. Create an age variable by subtracting the birth year from the year of the survey and add it to the biofam data frame.*
> biofam$age<-2000-biofam$birthyr

*4. What is the minimum, maximum, median and mean age in the sample?*
> min (biofam$age)
> max (biofam$age)
> mean (biofam$age)
> median (biofam$age)

*5. What is the minimum, maximum, median and mean age of the women?*
> mean(biofam$age[biofam$sex=="woman"])
> min(biofam$age[biofam$sex=="woman"])
> max(biofam$age[biofam$sex=="woman"])
> median(biofam$age[biofam$sex=="woman"])

 *6. Add a cohort factor to the biofam data frame grouping the birth years into the following categories: 1900-1929, 1930-1939, 1940-1949, 1950-1959.*
> biofam$AgeGroup <- cut (biofam$birthyr, c("1900","1930","1940","1950", "1960"), lables=c("1990-1929", "1930-1939", "1940-1949", "1950-1959"), right=FALSE)

*7. Generate an histogram of the distribution of birthyear using the above birth year classes. (Look at the help of the hist function for how to do that.)*
> hist (biofam$birthyr, breaks=c (1900,1930,1940,1950,1960), col='cyan')

*8. Produce a frequency table of the cohort factor.*
> table (biofam$AgeGroup)

 *9. Cross tabulate the cohort with the state at 25 years old.*
> levels (biofam$a25)<-c ("Parent", "Left", "Married", "Left+Marr", "Child", "Left+Child", "Left+Marr+Child", "Divorced")
> ct1<-table (biofam$AgeGroup, biofam$a25)
> ct1

 * 10. Fit a logistic regression for the probability to be married with a child and having
left home at 25 years old in terms of the language of the questionnaire and the sex.
Comment the results.*
> lg.gr<-glm (biofam$a25==6~biofam$plingu02+biofam$sex, family=binomial, data=biofam)
> summary (lg.gr)
> exp (lg.gr$coefficients)
> lg.gr.coeff<- as.data.frame (summary(lg.gr)$coefficients)
> lg.gr.coeff<- cbind (lg.gr.coeff,'Exp Estim.'=exp (lg.gr.coeff[, "Estimate"]))
> lg.gr.coeff
 
*11. Fit the same logistic regression, but for the youngest cohort only.*
> lg.gr<-glm (biofam$a25==6~biofam$plingu02+biofam$sex, family=binomial, data=biofam [biofam$AgeGroup=="1950-1959",])
> summary (lg.gr)
> exp (lg.gr$coefficients)
> lg.gr.coeff<- as.data.frame (summary(lg.gr)$coefficients)
> lg.gr.coeff<- cbind (lg.gr.coeff,'Exp Estim.'=exp (lg.gr.coeff[, "Estimate"]))
> lg.gr.coeff
