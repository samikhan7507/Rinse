# Generates realistic, deliberately messy demo datasets. Run: Rscript data/generate_samples.R
# The data is synthetic (no personal information) but shaped like real-world problems.
set.seed(2026)
out <- file.path("data", "samples"); dir.create(out, showWarnings = FALSE, recursive = TRUE)

blank <- function(x, p, token = NA) { i <- sample(length(x), round(length(x) * p)); x[i] <- token; x }
ws    <- function(x, p) { i <- sample(length(x), round(length(x) * p)); x[i] <- paste0(" ", x[i], "  "); x }
casey <- function(x, p) { i <- sample(length(x), round(length(x) * p)); x[i] <- ifelse(runif(length(i)) < .5, toupper(x[i]), tolower(x[i])); x }
dups  <- function(d, k) rbind(d, d[sample(nrow(d), k), ])
save  <- function(d, f) utils::write.csv(d[sample(nrow(d)), ], file.path(out, f), row.names = FALSE, na = "")

# 1. Jaipur house prices (regression, target: price_lakh)
n <- 600
loc <- sample(c("Vaishali Nagar", "Malviya Nagar", "Mansarovar", "C-Scheme", "Jagatpura", "Tonk Road", "Sodala"), n, TRUE, prob = c(.2, .18, .22, .08, .14, .1, .08))
base <- c("Vaishali Nagar" = 5200, "Malviya Nagar" = 5800, "Mansarovar" = 4300, "C-Scheme" = 9500, "Jagatpura" = 3900, "Tonk Road" = 4600, "Sodala" = 5000)
area <- round(runif(n, 450, 3200)); bhk <- pmin(5, pmax(1, round(area / 650 + rnorm(n, 0, .5))))
age <- sample(0:25, n, TRUE); price <- round((area * base[loc] * (1 - age * .008) + bhk * 90000 + rnorm(n, 0, 250000)) / 1e5, 1)
h <- data.frame(property_id = sprintf("JP%05d", 1:n), locality = loc, area_sqft = format(area, big.mark = ","), bedrooms = bhk,
                age_years = age, furnishing = sample(c("Furnished", "Semi-Furnished", "Unfurnished"), n, TRUE),
                parking = sample(c("Yes", "No"), n, TRUE, prob = c(.7, .3)), city = "Jaipur",
                price_lakh = paste0("₹", price), stringsAsFactors = FALSE)
h$locality <- ws(casey(h$locality, .12), .08); h$age_years <- blank(h$age_years, .09); h$furnishing <- blank(h$furnishing, .06, "N/A")
h$bedrooms <- blank(h$bedrooms, .04); h$area_sqft[sample(n, 4)] <- "999,999"
save(dups(h, 18), "jaipur_house_prices.csv")

# 2. Campus placement (classification, target: placed) - imbalanced
n <- 500; cgpa <- round(pmin(10, pmax(4.5, rnorm(n, 7.2, 1))), 2); proj <- rpois(n, 2.2); intern <- rbinom(n, 3, .35)
pl <- rbinom(n, 1, plogis(-9 + cgpa * .95 + proj * .35 + intern * .6) * .75)
d <- data.frame(roll_no = sprintf("21CSE%04d", 1:n), branch = sample(c("CSE", "IT", "ECE", "AI&DS"), n, TRUE, prob = c(.45, .2, .2, .15)),
                gender = sample(c("Male", "Female"), n, TRUE, prob = c(.62, .38)), cgpa = cgpa, backlogs = rpois(n, .4),
                projects = proj, internships = intern, github_commits = round(rexp(n, 1 / 120)), coding_platform_rating = round(rnorm(n, 1400, 250)),
                communication_score = round(pmin(10, pmax(1, rnorm(n, 6.5, 1.6))), 1), placed = ifelse(pl == 1, "Yes", "No"), stringsAsFactors = FALSE)
d$student_email <- sprintf("%s%d@gmail.com", sample(c("rahul", "priya", "aman", "sneha", "vikram", "anita", "rohit", "kavya"), n, TRUE), 1:n)
d$mobile <- paste0(sample(6:9, n, TRUE), sprintf("%09d", sample(0:999999999, n)))
d$gender <- casey(d$gender, .15); d$cgpa <- blank(d$cgpa, .07); d$communication_score <- blank(d$communication_score, .1, "?")
d$coding_platform_rating <- blank(d$coding_platform_rating, .25); d$github_commits[sample(n, 5)] <- 20000
save(dups(d, 10), "campus_placement.csv")

# 3. Loan default (classification, target: defaulted) - strongly imbalanced
n <- 700; inc <- round(rlnorm(n, 12.3, .5)); amt <- round(inc * runif(n, .2, 2.5)); sc <- round(pmin(900, pmax(300, rnorm(n, 690, 85))))
dflt <- rbinom(n, 1, plogis(-5.2 - (sc - 690) / 40 + amt / inc * 1.9))
l <- data.frame(loan_id = 100000 + 1:n, age = sample(21:65, n, TRUE), monthly_income = paste0("Rs. ", format(round(inc / 12), big.mark = ",")),
                loan_amount = amt, credit_score = sc, employment = sample(c("Salaried", "Self-employed", "Business", "Student"), n, TRUE, prob = c(.55, .2, .2, .05)),
                purpose = sample(c("Home", "Education", "Vehicle", "Personal", "Medical"), n, TRUE), country = "India",
                defaulted = ifelse(dflt == 1, "yes", "no"), stringsAsFactors = FALSE)
l$collection_notice_sent <- ifelse(dflt == 1, ifelse(runif(n) < .99, "Yes", "No"), ifelse(runif(n) < .01, "Yes", "No"))
l$credit_score <- blank(l$credit_score, .12, "null"); l$employment <- ws(casey(l$employment, .1), .1); l$age <- blank(l$age, .05)
save(dups(l, 14), "loan_default.csv")

# 4. Telecom churn (classification, target: churn)
n <- 800; ten <- sample(1:72, n, TRUE); mc <- round(runif(n, 199, 1499)); ct <- sample(c("Month-to-month", "One year", "Two year"), n, TRUE, prob = c(.55, .25, .2))
ch <- rbinom(n, 1, plogis(-.3 - ten * .035 + (ct == "Month-to-month") * 1.1 + (mc - 700) / 900))
t <- data.frame(customer_id = sprintf("CUST-%05d", 1:n), tenure_months = ten, monthly_charges = mc, total_charges = round(mc * ten * runif(n, .9, 1.05)),
                contract = ct, payment_method = sample(c("UPI", "Credit card", "Net banking", "Cash"), n, TRUE), support_calls = rpois(n, 1.3),
                internet_service = sample(c("Fiber", "DSL", "None"), n, TRUE), churn = ifelse(ch == 1, "Yes", "No"), stringsAsFactors = FALSE)
t$total_charges <- blank(t$total_charges, .06, " "); t$contract <- casey(t$contract, .08); t$support_calls[sample(n, 6)] <- 60
t$internet_service <- blank(t$internet_service, .05, "-")
save(dups(t, 12), "telecom_churn.csv")

# 5. Heart disease screening (classification, target: heart_disease)
n <- 450; age <- sample(30:78, n, TRUE); chol <- round(rnorm(n, 235, 48)); bp <- round(rnorm(n, 132, 18)); sex <- sample(c("M", "F"), n, TRUE, prob = c(.65, .35))
hd <- rbinom(n, 1, plogis(-15 + age * .09 + chol * .012 + bp * .022 + (sex == "M") * 1.0))
s <- data.frame(age = age, sex = sex, chest_pain = sample(c("typical", "atypical", "non-anginal", "asymptomatic"), n, TRUE), resting_bp = bp,
                cholesterol = chol, fasting_sugar = sample(c("<120", ">120"), n, TRUE, prob = c(.85, .15)), max_heart_rate = round(rnorm(n, 148, 22)),
                exercise_angina = sample(c("yes", "no"), n, TRUE, prob = c(.33, .67)), hospital = "City Care", heart_disease = ifelse(hd == 1, "present", "absent"), stringsAsFactors = FALSE)
s$cholesterol <- blank(s$cholesterol, .1, "?"); s$resting_bp <- blank(s$resting_bp, .04); s$chest_pain <- ws(s$chest_pain, .07)
s$cholesterol[sample(n, 3)] <- "0"; s$sex <- casey(s$sex, .1)
save(dups(s, 8), "heart_screening.csv")

# 6. UPI transactions (classification, target: is_fraud) - dates in mixed formats, rare fraud
n <- 900; amt <- round(rlnorm(n, 6.2, 1.1)); hr <- sample(0:23, n, TRUE, prob = c(rep(.02, 6), rep(.04, 4), rep(.06, 8), rep(.05, 4), rep(.03, 2)))
fr <- rbinom(n, 1, plogis(-4.6 + (amt > 8000) * 3 + (hr < 5) * 2.6))
dt <- as.Date("2026-01-01") + sample(0:300, n, TRUE)
u <- data.frame(txn_id = sprintf("TXN%07d", sample(1e6:9e6, n)), txn_date = ifelse(runif(n) < .6, format(dt, "%Y-%m-%d"), format(dt, "%d/%m/%Y")),
                hour = hr, amount_inr = paste0("₹", format(amt, big.mark = ",")), merchant_type = sample(c("Grocery", "Food", "Travel", "Electronics", "Bills", "Gaming"), n, TRUE),
                device = sample(c("Android", "iOS", "Web"), n, TRUE, prob = c(.7, .2, .1)), city = sample(c("Jaipur", "Delhi", "Mumbai", "Pune", "Kota"), n, TRUE),
                is_fraud = ifelse(fr == 1, "Yes", "No"), stringsAsFactors = FALSE)
u$device <- blank(u$device, .08, "N/A"); u$city <- casey(ws(u$city, .06), .1); u$hour <- blank(u$hour, .03)
save(dups(u, 9), "upi_transactions.csv")
message("Sample datasets written to ", out)

# Extra formats so every Unit III import path can be demonstrated.
h <- utils::read.csv(file.path(out, "jaipur_house_prices.csv"), colClasses = "character", encoding = "UTF-8")
writexl::write_xlsx(list(Listings = h, Notes = data.frame(note = "Sheet 1 holds the data. Pick it when asked.")), file.path(out, "jaipur_house_prices.xlsx"))
cp <- utils::read.csv(file.path(out, "campus_placement.csv"), colClasses = "character", encoding = "UTF-8")
con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "college_records.sqlite"))
DBI::dbWriteTable(con, "placements", cp, overwrite = TRUE); DBI::dbWriteTable(con, "telecom", utils::read.csv(file.path(out, "telecom_churn.csv"), colClasses = "character", encoding = "UTF-8"), overwrite = TRUE)
DBI::dbDisconnect(con)
message("Excel and SQLite samples written.")

# New loan applicants for the Predict and Drift demos: no target column, and the population has shifted
# (lower credit scores and incomes), so the drift check should flag credit_score and monthly_income.
m <- 300; inc2 <- round(rlnorm(m, 11.9, .5)); amt2 <- round(inc2 * runif(m, .3, 2.8)); sc2 <- round(pmin(900, pmax(300, rnorm(m, 610, 90))))
nw <- data.frame(loan_id = 200000 + 1:m, age = sample(21:60, m, TRUE), monthly_income = paste0("Rs. ", format(round(inc2 / 12), big.mark = ",")), loan_amount = amt2,
                 credit_score = sc2, employment = sample(c("Salaried", "Self-employed", "Business", "Student"), m, TRUE, prob = c(.4, .3, .2, .1)),
                 purpose = sample(c("Home", "Education", "Vehicle", "Personal", "Medical"), m, TRUE), country = "India", collection_notice_sent = "No", stringsAsFactors = FALSE)
utils::write.csv(nw, file.path(out, "loan_new_applicants.csv"), row.names = FALSE)
message("New-applicants file written.")
