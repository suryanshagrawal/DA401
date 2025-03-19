# clear all objects including hidden objects

rm(list = ls(all.names = TRUE)) 

# Or remove individual objects

rm(lerner.df1)

# Install packages

install.packages("pacman")

pacman::p_load(zoo, dplyr, readxl, readr, stringr, data.table, stargazer)

# Import data

lerner.df <- read.csv("/Users/suryanshagrawal/Library/CloudStorage/GoogleDrive-agrawa_s1@denison.edu/My Drive/Classes/Fall 2024/ECON 440/Final Project/BAC.Learner.csv")

# Convert the format of date from "string, character" to "Date" format

lerner.df$date = as.Date(strptime(lerner.df$t, "%m/%d/%Y")) 


# Create variables

lerner.df <- lerner.df %>% 
  mutate(OE = TNIE - PE, 
         Cost = IE + PE + OE, 
         W1 = IE/Deposit,
         W2 = PE/Asset,
         W3 = OE/Asset,
         e_a = Equity/Asset,
         loan_a = Loan_A/100,
         npl_loan = NPL_Loan/100,
         roa = ROA/100,
         w1 = log(W1), 
         w2 = log(W2),
         w3 = log(W3),
         c = log(Cost),
         a = log(Asset),
         w1_sq = w1^2,
         w2_sq = w2^2,
         w3_sq = w3^2,
         a_sq = a^2,
         w1_a = w1*a,
         w2_a = w2*a,
         w3_a = w3*a,
         w12 = w1*w2,
         w13 = w1*w3,
         w23 = w2*w3,
         roa_mean = rollapply(roa, width = 8, FUN = mean, fill = NA, align = "right"),
         roa_sd = rollapply(roa, width = 8, FUN = sd, fill = NA, align = "right"),
         z = (e_a + roa_mean)/roa_sd) 


# Select the date range

start_date <- as.Date("0004-03-31")
end_date <- as.Date("0023-12-31")
lerner.df1 <- lerner.df[lerner.df$date >= start_date & lerner.df$date <= end_date, ]

# Translog cost function

model1 = lm(c ~ w1 + w2 + w3 + a + w1_sq + w2_sq + w3_sq + a_sq + 
              w1_a + w2_a + w3_a + w12 + w13 + w23, data = lerner.df1)
summary(model1)

stargazer(model1, type = "html", out = "fullmodel.doc")

# Extract coefficients from the model
coefficients <- coef(model1)

# Automate calculation of elas and Lerner index
lerner.df1 <- lerner.df1 %>% 
  mutate(
    elas = coefficients["a"] + 2 * coefficients["a_sq"] * a +
      coefficients["w1_a"] * w1 + coefficients["w2_a"] * w2 +
      coefficients["w3_a"] * w3,
    Lerner = abs(1 - elas * (Cost / Revenue))
  )

# Compute the Lerner index

#model1
#lerner.df1 <- lerner.df1 %>% 
 # mutate(elas = 2.030261  + 2*-0.149318*a + -0.130482*w1 + -0.181550 *w2 + -0.255470 *w3,
  #       Lerner = 1 - elas*(Cost/Revenue))

# Regression 1: Z & Lerner (i) with the Lerner index as the only independent variable

Model1 = lm(z~Lerner, data = lerner.df1)
summary(Model1)

# Regression 2: Z & Lerner (ii) with control variables added

Model2 = lm(z~Lerner + e_a + loan_a+ npl_loan + GDP + UR + FFR + CPI, data = lerner.df1)
summary(Model2)

# Regression 3: Z & Lerner & learner^2 (iii) with control variables added

lerner.df1$Lerner_sq = lerner.df1$Lerner * lerner.df1$Lerner

Model3 = lm(z~Lerner + Lerner_sq + e_a + loan_a + npl_loan + GDP + UR + FFR + CPI, data = lerner.df1)
summary(Model3)

# Create a time series data set of Z-score and Lerner index from the dataframe.

lerner.ts <- ts(lerner.df1[, c("z", "Lerner")], start = 2004, frequency = 4)

# Plot the results

plot(lerner.ts, main = "Z-score & Lerner index - Bank of America (2004 to 2023)")

#
stargazer(Model1, Model2, Model3, type = "html", out = "modeloutput2.doc")

# Export cleaned data
write.csv(lerner.df1, "lerner.full.data.csv")
          
########################## END ##########################

