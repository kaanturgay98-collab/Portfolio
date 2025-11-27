Campari BI Analysis — Advanced Performance Measurement

Power BI • Data Modeling • KPI Engineering • Benchmarking • Social Media Analytics

This project delivers a complete Business Intelligence analysis for Campari Group, based on Sell-Out market data and Social Media Mentions.
The analysis includes: data cleaning, data modeling (star-schema), KPI design, Sales dashboards, Benchmarking, Promotion Effectiveness, and Social Media Impact.
All visualizations were developed for Marketing & Sales decision-making using Power BI.

(Content extracted and summarized from the full written report.) 

Group15_written_report

🔍 Project Overview

The objective is to provide Campari with actionable insights across 3 countries (Italy, Germany, UK), using real-time sell-out data and social media activity.
The BI solution includes five dashboards:

General Sales Overview — total sales, market trends, brand drivers

Benchmarking & Competitor Analysis — market share, pricing strategy, brand equity

Promotion Performance — promo uplift, promo effectiveness index, elasticity

Social Media Mentions — sentiment, reach, platform impact

Integrated KPI Framework — evaluation of all KPIs (measurability, precision, timeliness)

🧹 Data Processing
Datasets Used

Sell-Out Data — category, manufacturer, channel, country, pricing, volume/value

Social Media Mentions — platform, sentiment, reach, brand mentions

Key Cleaning Steps

Fixed time inconsistencies (week 52 month 1 issue)

Reconstructed No Promo values using statistical thresholds

Imputed missing Category.Global and Manufacturer.Global

Merged manufacturers using brand-to-company mapping

Combined datasets for all countries

Built unified DimCalendar for cross-fact analysis

🧩 Data Modeling (Star Schema)

Fact Tables

FactSales

FactMentions

Dimension Tables

DimProduct, DimCategory, DimManufacturer

DimCountry, DimChannel, DimSource

DimCalendar

This model supports flexible slicing by time, country, category, product, and platform.

📈 Key KPIs Engineered
Sales KPIs

Total Sales

Total Volume

Market Share

Growth Rate

Cumulative Contribution

Brand Equity

Promotions KPIs

Promo Sales Volume

Promo Sales Contribution

Promo Effectiveness Index

Promo Pressure

Mean Uplift

Price Discount

Elasticity

Incremental Sales Value

Social Media KPIs

Total Mentions

Sentiment %

Reach by Platform

Mentions–Sales Correlation

📊 Dashboard Highlights
1. General View

Country-level sales trends

Brand performance ranking

Seasonal analysis

Market share evolution

2. Benchmarking

Top-10 manufacturers comparison

Brand equity by category

Opportunity exploitation (QoQ growth vs. competitors)

Category-level market share analysis

3. Promotion Performance

Channel contribution (Hypermarkets, Supermarkets, LSPs)

Promo uplift vs. discount bubble charts

Promo vs. Baseline vs. Incremental breakdown

4. Social Media Mentions

Earned vs. owned mentions

Sentiment distribution

Platform comparison (Instagram, Facebook, X)

Mentions & incremental sales correlation

📝 Summary of Strategic Insights
🇬🇧 United Kingdom

Biggest opportunity in Whisky, no dominant leader

Marketing efforts most effective early in the year

Strong potential for premium positioning

🇩🇪 Germany

Low concentration market → high growth potential

Predictable promo response

Biggest opportunity in Sparkling Wines (Cinzano SW)

🇮🇹 Italy

Campari strongest presence (leader in Aperitifs)

Lowest promo effectiveness

Need to move promotions away from Hypermarkets

⭐ Social Media

X = strongest reach efficiency

Instagram = best branding channel

Integrated communication strategy recommended

📚 Source

Full written report available here:
Group15_written_report.pdf 
