# STAT311 - Homework 5
# Name: NAME

# Question 1.a
# Construction a function called ci.for.proportion which calculates confidence 
# intervals for proportions. Your function should take three arguments, 
# ci.for.proportion(phat, n, conf) phat is the sample proportion, n is the sample 
# size, and conf is the level of confidence, expressed as a real number 
# between 0 and 1.

# Your function should return a vector of two items, representing the lower 
# and upper bound of a confidence interval for the specified level of confidence.
# IE: c(lowerbound, upperbound)

ci.for.proportion <- function(phat, n, conf) {
  # Calculate z critical value based on confidence level
  z <- qnorm(1 - (1 - conf) / 2)
  
  # Calculate standard error
  se <- sqrt(phat * (1 - phat) / n)
  
  # Calculate margin of error
  margin_of_error <- z * se
  
  # Calculate lower and upper bounds
  lower_bound <- phat - margin_of_error
  upper_bound <- phat + margin_of_error
  
  return(c(lower_bound, upper_bound))
}



# Question 2

# The state of Florida is in the midst of a homeowners insurance crisis, with 
# insurance costs skyrocketing and pricing many homeowners out of the market. 
# More and more homeowners are resorting to self-insurance, saving money that 
# would be spent on insurance in the hopes that the savings can cover any issues 
# that arise that would normally fall under an insurance policy. (Some people 
# might call this `not having insurance', but sure, let's 
# go with `self-insurance'.)

# A survey of 487 Florida homeowners found that 82 of those surveyed 
# reported relying on self-insurance.

# Question 2.a
# Based on the sample proportion found, what is the estimated standard error 
# of the distribution of phat? 
# Save your answer in the variable q2.a

# Calculate sample proportion
phat <- 82 / 487

# Calculate estimated standard error
q2.a <- sqrt(phat * (1 - phat) / 487)

# Question 2.b
# Calculate a 90% confidence interval for the true proportion of Florida 
# homeowners who rely on self-insurance. (Your answer should be formatted 
# similarly to the function output, if done by hand). 
# Save your answer in the variable q2.b

q2.b <- ci.for.proportion(phat, 487, 0.90)

# Question 2.c
# If the true proportion of Florida homeowners relying on self-insurance were 
# p=.177, what is the true standard error of the distribution of phat? 
# Save your answer in the variable q2.c

p_true <- 0.177
q2.c <- sqrt(p_true * (1 - p_true) / 487)

# Question 2.d
# If the true proportion of Florida homeowners relying on self-insurance were 
# p=.177, what is the probability that the interval you found in q2.b 
# contains the true proportion? 
# Save your answer in the variable q2.d (Solution is hidden, consider carefully)

# Since the true proportion p=0.177 is outside our calculated confidence interval q2.b,
# the probability that the interval contains the true proportion is 0
q2.d <- 0

# Question 2.e
# If the true proportion of Florida homeowners relying on self-insurance were 
# p=.202, what is the true standard error of the distribution of phat? 
# Save your answer in the variable q2.e

p_true2 <- 0.202
q2.e <- sqrt(p_true2 * (1 - p_true2) / 487)

# Question 2.f
# If the true proportion of Florida homeowners relying on self-insurance were 
# p=.202, what is the probability a 95% interval constructed from the sample 
# used in q2.b would contains the true proportion? 
# Save your answer in the variable q2.f (Solution is hidden, consider carefully)

# The interval is constructed from the sample data we already have
# If p=0.202 is inside the 95% CI constructed from our sample, probability is 1
# If p=0.202 is outside the 95% CI constructed from our sample, probability is 0
# Let's calculate the 95% CI and check
ci_95 <- ci.for.proportion(phat, 487, 0.95)
q2.f <- ifelse(ci_95[1] <= 0.202 && ci_95[2] >= 0.202, 1, 0)

# Question 2.g
# If the true proportion of Florida homeowners relying on self-insurance were 
# p=.202, what is the probability a 95% interval constructed from a new sample 
# of some unknown large size would contains the true proportion? 
# Save your answer in the variable q2.g (Solution is hidden, consider carefully)

# For any properly constructed 95% confidence interval from a new sample,
# the probability that it contains the true proportion is 0.95 by definition
q2.g <- 0.95


# Question 3
# A 2022 survey of high school students found that a reported 14.1% of students 
# used e-cigarettes. Researchers attempting to show that the proportion of 
# students using e-cigarettes has decreased in 2023. They take a sample of
# 189 high school students and find that 19 of the high school students 
# used e-cigarettes.
# Researchers use this sample to conduct a hypothesis test to determine 
# if there is evidence that less students are using e-cigarettes than the 
# previous year. They plan to test their hypothesis at the α = 5% level.
# Your answers should be exact, utilizing the binomial distribution.


# Question 3.a
# Based on the proposed hypothesis test of the researcher, what is the 
# assumed value of the population parameter p. 
# Save your answer in the variable q3.a

# The null hypothesis assumes the population proportion is the same as 2022
q3.a <- 0.141

# Question 3.b
# How many standard deviations (or standard errors) away from the mean is the 
# observed sample proportion? 
# Save your answer in the variable q3.b

# Calculate observed proportion
phat3 <- 19 / 189

# Calculate standard error under null hypothesis
se3 <- sqrt(q3.a * (1 - q3.a) / 189)

# Calculate z-score
q3.b <- (phat3 - q3.a) / se3

# Question 3.c
# What is the probability under the null hypothesis of observing an estimate 
# of the parameter as or more extreme than the observed phat? 
# (This is the p-value). 
# Save your answer in the variable q3.c

# Since researchers want to show a decrease, this is a one-sided test
# Using exact binomial probability for observed value or more extreme
q3.c <- pbinom(19, 189, 0.141)

# Question 3.d
# What is the maximum number of students reporting using e-cigarettes that 
# would lead the researchers to reject the null hypothesis? 
# (The sample size remains unchanged). 
# Save your answer in the variable q3.d

# Find the largest k such that P(X ≤ k) ≤ 0.05 where X ~ Bin(189, 0.141)
for (k in 0:189) {
  if (pbinom(k, 189, 0.141) > 0.05) {
    q3.d <- k
    break
  }
}

# Question 3.e
# What is the exact probability of committing a type 1 (type I) error if the 
# null hypothesis is true? 
# Save your answer in the variable q3.e

# Type I error rate is equal to the significance level when using the exact rejection region
q3.e <- pbinom(q3.d, 189, 0.141)

# A much larger study of high school students found that 10.0% of students 
# used e-cigarettes. Assume this is the true value of the parameter.

# Question 3.f
# Based on the true value of the parameter, what is the probability of 
# committing a type 1 (type I) error? 
# Save your answer in the variable q3.f (Solution is hidden, consider carefully)

# If the true parameter is 10%, then the null hypothesis (p=14.1%) is false
# If the null hypothesis is false, we cannot commit a Type I error
# Type I error is rejecting a true null hypothesis
q3.f <- 0

# Question 3.g
# Based on the true value of the parameter, what is the probability of 
# observing a level of e-cigarette use that would causes the researchers 
# to reject the null hypothesis? (This is the power of the test). 
# Save your answer in the variable q3.g

# Power is the probability of rejecting H0 when H1 is true
# We reject H0 when the number of e-cigarette users is ≤ q3.d
q3.g <- pbinom(q3.d, 189, 0.10)