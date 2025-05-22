df <- read.csv("Churn.csv", stringsAsFactors =  FALSE) #կարդում ենք դատասեթը
dim(df) #ցույց է տալիս դատասեթի չափը
str(df) #ցույց է տալիս փոփոխականների type-ը
colSums(is.na(df)) #missing values

# Replace NAs in TotalCharges with its mean
df$TotalCharges[is.na(df$TotalCharges)] <- mean(df$TotalCharges, na.rm = TRUE)
sum(is.na(df$TotalCharges))

prop.table(table(df$Churn)) * 100 #churn-ի տոկոսը դատասեթում
#նայենք Թվային փոփոխականների նկարագրերը
summary(df[, c("tenure", "MonthlyCharges", "TotalCharges")])



#Descriptive Statistics
prop.table(table(df$SeniorCitizen)) * 100
prop.table(table(df$Dependents)) * 100
prop.table(table(df$Partner)) * 100 
prop.table(table(df$PaperlessBilling)) * 100 
prop.table(table(df$Churn)) * 100
prop.table(table(df$gender)) * 100
prop.table(table(df$OnlineSecurity)) * 100

#barplots
barplot(prop.table(table(df$PhoneService, df$Churn), 1)[, "Yes"] * 100,
        main = "Churn Rate by Phone Service",
        ylab = "Churn Percentage (%)",
        col = 'blue')
# OnlineSecurity vs Churn
barplot(prop.table(table(df$OnlineSecurity, df$Churn), 1)[, "Yes"] * 100,
        main = "Churn Rate by Online Security",
        ylab = "Churn Percentage (%)",
        col = 'blue')
#OnlineBackup vs Churn
barplot(prop.table(table(df$OnlineBackup, df$Churn), 1)[,"Yes"] * 100,
        main = "Churn Rate by Online backup",
        ylab = "Churn Percentage (%)",
        col = 'blue')
#Contract type vs churn
barplot(prop.table(table(df$Contract, df$Churn), 1)[,"Yes"] * 100,
        main = "Churn Rate by Contact type",
        ylab = "Churn Percentage (%)",
        col = 'blue')
#Payment Method vs Churn
barplot(prop.table(table(df$PaymentMethod, df$Churn), 1)[, "Yes"] * 100,
        main = "Churn Rate by Payment method",
        ylab = "Churn Percentage (%)",
        col = 'blue')

#Թրենդային նկարագրություն՝ ըստ Tenure-ի
# Ստեղծել բինավորված Tenure խմբեր
df$TenureGroup <- cut(df$tenure,
                      breaks = c(0, 12, 24, 36, 48, 60, 72),
                      labels = c("0-12", "13-24", "25-36", "37-48", "49-60", "61-72"))

# Հաշվել Churn տոկոսները ըստ այդ  խմբերի
t_churn <- table(df$TenureGroup, df$Churn)
t_p <- prop.table(t_churn, 1) * 100

# Գծել barplot
barplot(t_p[, "Yes"],
        main = "Churn Rate by Tenure Group",
        ylab = "Churn Percentage (%)",
        xlab = "Tenure Group (Months)",
        col = "lightblue")

#Թրենդային վերլուծություն՝ ըստ MonthlyCharges-ի
# Ստեղծել բինավորված MonthlyCharges խմբեր
df$ChargeGroup <- cut(df$MonthlyCharges,
                      breaks = c(0, 30, 50, 70, 90, 110, Inf),
                      labels = c("0-30", "31-50", "51-70", "71-90", "91-110", "110+"))

# Հաշվել Churn տոկոսները ըստ խմբերի
charge_churn <- table(df$ChargeGroup, df$Churn)
charge_p <- prop.table(charge_churn, 1) * 100

# Գծել barplot
barplot(charge_p[, "Yes"],
        main = "Churn Rate by Monthly Charges",
        ylab = "Churn Percentage (%)",
        xlab = "Monthly Charges (USD)",
        col = "blue")

#Inferential Statistics
#Confidence interval for churn rate
#Հաշվում ենք churn proportion-ը
churn_prop <- mean(df$Churn == "Yes")
n <- 7043

#բինոմական բաշխման համար margin of error ենք հաշվում
mor <- qnorm(0.975) * sqrt((churn_prop * (1 - churn_prop)) / n)  
#Սա լինելու է մեր միջակայքը 
l <- churn_prop - mor
u <- churn_prop + mor

# Տպում ենք վստահության միջայակայքը՝ տոկոսներով
print(c("Lower bound:" = round(l * 100, 1), "Upper bound:" = round(u * 100, 1)))

#Hypothesis test
n <- 7043
x_ <- 0.2653699 #MLE գնահատական
p0 <- 0.25
alpha <- 0.05

TS <- (x_ - p0) / sqrt(p0 * (1 - p0) / n)
z_alpha <- qnorm(1 - alpha)

print(c("TS =" = round(TS, 3)))
print(c("Z_alpha =" = round(z_alpha, 3)))

if (TS > z_alpha) {
  print("Մերժում ենք H0")
} else {
  print("Չենք մերժում H0")
}

