# Rinse demo guide

Use this to test the project and to present it in class or in an interview.

## The six test datasets

All are in `data/samples/` and in the app under **Try a sample**. They are synthetic (no real people) but built with the problems real data has. Numbers below are what Rinse produces with default settings and a fixed seed.

| Dataset | Task | Target | Rows | Score before → after | Best model | What to point out |
|---|---|---|---|---|---|---|
| Jaipur house prices | Regression | `price_lakh` | 618 → 600 | 69 → 99 | Linear regression, R² 0.94 | `₹87.5` and `2,022` stored as text; mixed-case localities; a fake `999,999` sqft outlier |
| Campus placement | Classification | `placed` | 510 → 500 | 66 → 97 | Logistic regression, F1 0.67 | **Student emails and mobile numbers** are detected and dropped; `?` placeholders; 25% missing coding rating |
| Loan default risk | Classification | `defaulted` | 714 → 700 | 68 → 98 | Logistic regression, F1 0.71, AUC 0.82 | `collection_notice_sent` **leaks the answer** and is removed; `Rs. 20,506` income text; 79% of loans do not default |
| Telecom churn | Classification | `churn` | 812 → 800 | 82 → 100 | Logistic regression, F1 0.70, AUC 0.81 | Blank total charges; mixed-case contract type |
| Heart screening | Classification | `heart_disease` | 458 → 450 | 79 → 95 | Logistic regression, AUC 0.88 | `?` cholesterol; impossible cholesterol of `0`; 94% negative cases |
| UPI fraud check | Classification | `is_fraud` | 909 → 900 | 75 → 95 | Logistic regression, AUC 0.87 | Dates in two formats. **97% accuracy but only 0.49 macro F1**: the classic imbalance trap |

Extra files:

- `jaipur_house_prices.xlsx` tests Excel import and the sheet picker (choose **Listings**).
- `college_records.sqlite` tests database import and the table picker.
- `loan_new_applicants.csv` is for the **Predict** and **Drift check** pages. It has no target column, and its credit scores and incomes are lower than the training data, so Drift flags both.

Also try one real public file: the Titanic file from Kaggle (target `Survived`), UCI Heart Disease, or Telco Customer Churn.

## Seven-minute presentation script

1. **The problem (30 s).** "Models are only as good as their data, but most people clean data without measuring it, and they never check for leakage or personal data."
2. **Sign in (20 s).** Show email sign-in (Google and phone appear only when they are configured). Mention that every attempt is recorded.
3. **Load and check (60 s).** Choose *Loan default risk*. Show the score of 68. Point at **Possible target leakage: collection_notice_sent**. "A model would look perfect and fail in real life."
4. **Clean (45 s).** Click **Clean my data**. Show 68 rising to 98 and the log, including "Dropped: almost identical to the target".
5. **Prepare (30 s).** "I split first and scale using only training rows, so nothing leaks from the test set."
6. **Models (90 s).** Train all four. Show the comparison, ROC curves and confusion matrix. Click **Run the raw vs clean comparison**. Explain why the raw file scores 1.000: the raw model uses the leaked column. "That is exactly the trap Rinse removes."
7. **Insights (30 s).** Show the two borrower groups in real units and the unusual records.
8. **Predict and drift (60 s).** Go to Predict, upload `loan_new_applicants.csv`, download predictions. Then Drift check with the same file: credit score and income flagged as significant drift.
9. **Admin (60 s).** Sign in as admin. Show **Online now**, **Sign-ins** (including a failed attempt), the **Database** table browser, and export a table to Excel. Show the app on a phone.

## Likely questions and short answers

**What is the score and how is it calculated?** It starts at 100. Each problem subtracts points, capped per category (Completeness 30, Consistency 20, Uniqueness 15, Usefulness 15, Distribution 10, Target 10). Every deduction is listed with its reason and fix.

**What is data leakage and how do you detect it?** A feature that contains the answer, for example a "collection notice sent" flag for a loan default model. Rinse measures how strongly each feature is associated with the target (correlation, correlation ratio or Cramér's V). Above 0.95 it is flagged and dropped.

**Why train on the raw file again?** To measure the effect of cleaning with the same model and split. When the raw file contains leakage, its score is inflated, and Rinse says so.

**Why macro F1 instead of accuracy?** On imbalanced data accuracy hides failure. In the fraud data a model that always says "not fraud" gets 97% accuracy and catches nothing.

**How does Predict clean new data?** The cleaning step stores what it learned on the training data (fill values, label spellings, outlier limits, dropped columns) and the preparation step stores encoding levels and scaling statistics. New rows go through exactly those.

**What is drift?** When new data no longer looks like the data a model learned from. Rinse uses the Population Stability Index: below 0.1 stable, 0.1 to 0.25 moderate, above 0.25 significant.

**How are passwords and sign-ins protected?** Argon2 hashing, lockout after 5 failures, idle sign-out, parameterised SQL, and every sign-in recorded with device and IP.

**Which syllabus topics does it cover?** Unit II: vectors, factors, lists, data frames, built-in functions. Unit III: import from CSV, Excel, JSON, SQLite and web; missing data, filtering, `dplyr`; export. It goes beyond the syllabus into modelling and deployment.

**What would you add next?** Email-based password reset (needs a mail server), PostgreSQL for many users, and gradient-boosted models.

## Honest talking points

- Sample data is synthetic. Say so, then show a real Kaggle file.
- The Phone tab is hidden unless SMS is configured. With `RINSE_DEMO_SMS=1` it appears and the code shows on screen, because no SMS service is connected.
- Google sign-in needs your own Google keys.
- Models are classical and fast. The aim is a trustworthy comparison, not a leaderboard score.
