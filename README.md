# Data Science Employee Attrition: Project Overview 
* Create model on Hr_employee Data to classify Attrition.
* Logistic Regression,Feature Selection,Random forest ,Boruta . 

## Code and Resources Used 
**Tool:** R Studio  
**Libraries:** corrplot,caret,

## Data Cleaning and EDA
*	Checking Null Values.
*	Converting Data into their specific Data types.
*	Checking Zeros.
*	Seperating Catogerical And Numeric Data.
*	Checking Multicolinearity.
*	Checking Outiler
*	Updating Outiler with mean/median/mode.
*	Removing Feature with High colinearity
## Model Building 

First, I transformed the categorical variables into dummy variables. I also split the data into train and tests sets with a test size of 30%. 
I trie different models Like Logistic Regression,Some feature+Logistic Regression,Random Forest,and Boruta on Random Forest.
I tried three different models:

## Model performance
The Random Forest model far outperformed the other approaches on the test and validation sets. 
*	**Logistic Regression model (M1)**: Accuracy = 0.88 ,Specificity=0.90
*	**Logistic Regression model (M2)**: Accuracy = 0.907 ,Specificity=0.92
*	**Logistic Regression model (M3)**: Accuracy = 0.912 ,Specificity=0.92
*	**Random Forest model (M1)**: Accurecy = 0.957 ,Specificity=0.95
*	**Random Forest Model(M2)**: Accuracy = 0.96 , Specificity=0.96




