# Online Shoppers Purchase Prediction (R)

> Six classifiers compared in R to predict whether a browsing session ends in a purchase.

## Overview

- Data Mining and Visualization group case study (AoL, 2024/2025): optimize marketing strategy by predicting purchase intention from website sessions.
- Covers data cleaning, exploratory visualization and supervised modelling, all written in R.

## Data / Scope

- UCI Online Shoppers Purchase Intention dataset: 12,330 sessions, 17 features plus the `Revenue` label.
- Only about 15% of sessions end in a purchase, so accuracy was read against the 84.5% no-information rate.

## Workflow

- Cleaned the data: column names, factor types, duplicate check and IQR outlier check (outliers kept as real behaviour).
- Visualized revenue by visitor type, weekend, month and special day; correlation heatmap; exit versus bounce rate.
- Scaled numeric features and split 70/30 with `set.seed(123)` using caret.
- Trained Random Forest (100 trees), Logistic Regression, Decision Tree, linear SVM, Naive Bayes and KNN (k tuned from 1 to 20).
- Compared all models with confusion matrices, accuracy confidence intervals, sensitivity, specificity and balanced accuracy.

## Results

- Random Forest was best: 90.4% accuracy (95% CI 89.4% to 91.3%).
- Decision Tree 89.9%, Logistic Regression 88.6%, SVM 88.5%, KNN 88.3% and Naive Bayes 80.0%.
- Specificity was low for most models (about 38% to 58%), so the models catch non-buyers better than buyers; balanced accuracy is the fairer comparison.
- `PageValues` was by far the strongest predictor in the Random Forest importance plot, and the decision tree split first on it.

## Tech Stack

R, tidyverse, ggplot2, caret, randomForest, rpart, e1071, class (KNN)

## Repository Structure

```
code/       R analysis script (cleaning, EDA, six models)
data/       online_shoppers_intention.csv (UCI)
docs/       Written report (cover page removed)
figures/    Key charts from the report
```

## How to Run

```r
install.packages(c('tidyverse','janitor','car','gridExtra','reshape2','corrplot','caret','randomForest','rpart','rpart.plot','e1071','caTools','knitr'))
source('code/online_shoppers_analysis.R')   # run from the repo root so the data path resolves
```

The script reads `online_shoppers_intention.csv`; point it to `data/online_shoppers_intention.csv` if you run from the repo root.

## Team

Group project by Aaron Jeremy Alexander, Ann Abigail Halim, Britney Angeline Soeseno, Joycelin, Nadya Angelie Lislie and Vania Wyanet Tumbelaka (Data Mining and Visualization, BINUS University).

## Notes

- Student ID numbers were removed from the report PDF by dropping its cover page.
- The accuracy figures above are taken from the confusion matrices and summary table in the report.

## Author

Nadya Angelie Lislie, BINUS University (Computer Science and Statistics). [LinkedIn](https://www.linkedin.com/in/nadya-angelie-lislie/) | [GitHub](https://github.com/Nadya-AngelieLislie)

## Continuous Integration

GitHub Actions (`.github/workflows/ci.yml`) runs `ruff` and `pytest` repository-health checks on every push; run them locally with `make lint test`.
