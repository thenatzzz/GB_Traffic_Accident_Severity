# Great Britain Traffic Accident Severity Classification

### Final Project, Statistical Modelling and Prediction, Simon Fraser University

#### Data
- Great Britain Road Safety Data
- ref: https://data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data
- Highly unbalanced dataset with 3 classes: Fatal, Serious, Slight (of 0.5%/14%/84.5%) 

#### Tools:
- R

#### Models:
- GLM, XGBoost, SVM, Random Forest, Neural Networks

#### Analysis Steps:
1. Data Preprocessing + Exploratory Data Analysis
2. Model Exploration
3. Parameter Tuning
4. Prediction and Analysis

#### Processing
- Merge 3 files of attendant, vehicle, and environment details based on ID
- Drop features that has more than 90% percent of same values
- Drop one of pair features that have correlation more than 95% percent
- Drop column with text that need nlp technique to extract information
- Process datetime column to hour, day, and month
- etc.

#### Parameter Tuning
- Out-of-bag error for Random Forest
- Repeated 10-fold Cross Validation for all

#### Evaluation
- Precision, Recall, F1
- Misclassification rate
