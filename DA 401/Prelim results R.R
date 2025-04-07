# clear all objects including hidden objects

rm(list = ls(all.names = TRUE)) 

# Or remove individual objects

rm(lerner.df1)

# Install packages

install.packages("pacman")
pacman::p_load(zoo, dplyr, readxl, readr, stringr, data.table, stargazer)

# Import data
lerner.df <- read_excel("G:/My Drive/DA 401/Prelim Analysis.xlsm", sheet = "main")

# Convert the format of date from "string, character" to "Date" format

lerner.df$date = as.Date(strptime(lerner.df$Date, "%m/%d/%Y")) 

model1 <- lm(CET1 ~ PX_TO_BOOK_RATIO + VOLATILITY_30D + TOT_LOAN_TO_TOT_DPST + 
               BS_TOT_ASSET + RETURN_COM_EQY + RETURN_ON_ASSET + 
               NPLS_TO_TOTAL_LOANS + GROWTH_IN_TOT_LOAN + 
               NON_INT_INC + DVD_PAYOUT_RATIO, data = lerner.df)

summary(model1)

model2 <- lm(CET1 ~ PX_TO_BOOK_RATIO + VOLATILITY_30D + TOT_LOAN_TO_TOT_DPST + 
               BS_TOT_ASSET + RETURN_COM_EQY + RETURN_ON_ASSET + 
               NPLS_TO_TOTAL_LOANS + GROWTH_IN_TOT_LOAN + 
               NON_INT_INC + DVD_PAYOUT_RATIO + 
               
               # Quadratic terms
               I(PX_TO_BOOK_RATIO^2) + I(VOLATILITY_30D^2) + 
               I(TOT_LOAN_TO_TOT_DPST^2) + I(BS_TOT_ASSET^2) + 
               I(RETURN_COM_EQY^2) + I(RETURN_ON_ASSET^2) + 
               I(NPLS_TO_TOTAL_LOANS^2) + I(GROWTH_IN_TOT_LOAN^2) + 
               I(NON_INT_INC^2) + I(DVD_PAYOUT_RATIO^2) + 
               
               # Interaction terms
               PX_TO_BOOK_RATIO:VOLATILITY_30D + 
               PX_TO_BOOK_RATIO:TOT_LOAN_TO_TOT_DPST + 
               PX_TO_BOOK_RATIO:BS_TOT_ASSET +
               VOLATILITY_30D:TOT_LOAN_TO_TOT_DPST +
               VOLATILITY_30D:BS_TOT_ASSET +
               TOT_LOAN_TO_TOT_DPST:BS_TOT_ASSET, 
             
             data = lerner.df)
summary(model2)

stargazer(model1, model2, type = "html", out = "mot.doc")


library(ggplot2)

# Assuming your data is stored in 'lerner.df'
ggplot(lerner.df, aes(x = RETURN_ON_ASSET, y = CET1, color = Equity)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(group = Equity), se = FALSE) + 
  labs(title = "Return on Assets vs CET1 by Bank", x = "Return on Assets", y = "CET1") +
  theme_bw()


ggplot(lerner.df, aes(x = TOT_LOAN_TO_TOT_DPST, y = CET1)) + 
  geom_point(color = "green") + 
  geom_smooth(method = "lm", formula = y ~ x, color = "orange") + 
  labs(title = "Loan-to-Deposit Ratio vs CET1", x = "Loan-to-Deposit Ratio", y = "CET1") +
  theme_minimal()


