# Earhart
## Calculate Multiple R-squared of a Data Set with Missing Data

Earhart is an 8 step process, plus the output:  
1. Create the desired model based on the specified list of DVs and IVs;  
2. Create an Amelia object from the model specified in Step 1. Meld the imputations to get regression coefficients;  
3. Extract the imputations from the Amelia obejct;  
4. Use the extracted values from Step 3 to manually calcualte the mean across imputations (this is done by the Amelia::meld in Step 2, but it stays under the hood);
5. Calcualte y-hat values using the means from Step 4 and the regression coefficients from Step 2;
6. Calcualte sum-of-squares of the y-hat values from Step 5 and the observed/imputed y values;
7. Calcualte the multiple R-squared effect size using the values from Step 6;
8. Use a specified R-squared adjustment to correct for model size and observations *(UNDER CONSTRUCTION)*  

END: Output the Multiple R-squared effect size.
