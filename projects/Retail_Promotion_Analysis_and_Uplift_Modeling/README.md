🛒 Retail Promotion Analysis & Uplift Modeling
Advanced analytics case study combining EDA, segmentation, ML forecasting, and uplift modeling for smarter promotion decisions.
📌 Overview

This project explores the effectiveness of retail promotions through:

Descriptive analytics

Customer & product segmentation

Baseline machine learning forecasting

Uplift modeling (Causal ML)

Optimization of promotion targeting

The objective is to determine which promotions work, for which customer segments, and under what conditions, enabling personalized and cost-efficient campaign strategies.

📊 1. Analytical Steps
🔎 Descriptive Analysis

Sales trends & seasonality

Customer behavior profiling

Product-level KPI comparison

Promotion uplift vs. non-promo baseline

Price elasticity & sensitivity indicators

🧩 2. Segmentation

K-Means clustering

RFM-based customer grouping

High-response vs. low-response customer cohorts

Behavioral clusters for tailored targeting

🤖 3. Baseline Machine Learning Models
Models Used:

Random Forest

Gradient Boosting

XGBoost Regression

Cross-validated performance metrics (MAPE, RMSE)

Purpose:

Establish an unbiased base sales prediction

Compare baseline vs. promotion-affected outcomes

Identify products/customers sensitive to promotions

🎯 4. Uplift Modeling (Causal ML)

The core of this project — measuring incremental impact of promotions.

Techniques:

XGBoost Classifier with treatment flag

HistGradientBoosting Classifier

Uplift buckets (Q1–Q4)

Qini curve / Uplift curve evaluation

Treatment effect ranking

Outcomes:

Identified customer groups with positive uplift

Prevented discount leakage

Improved promotion ROI

Enabled precise targeting instead of mass discounting

📈 Key KPIs

Incremental Sales

Incremental Conversion Rate

Promotion ROI

Base vs. Promo Lift

Uplift Score (Treatment Effect)

Customer Sensitivity Score

Cost Efficiency

🧠 Final Insights

Clear segmentation of customers by uplift response

Smarter, targeted promotion strategy

Reduced unnecessary promotional cost

Demonstrated data-driven commercial impact

Full ML workflow delivered: EDA → Segmentation → Baseline → Uplift → Recommendation

🛠 Tech Stack

Python (pandas, numpy, scikit-learn, xgboost)

Matplotlib / Seaborn

Jupyter Notebook

Power BI (optional)

Causal ML / Uplift Modeling
Data & Business Analytics | CRM | Predictive Modeling | Uplift Models
