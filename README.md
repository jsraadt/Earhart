# Earhart
Calculate Multiple R-squared of a Data Set with Missing Data
Earhart is an 8 step process, plus the output.
Step 1: Create the desired model based o nthe specified list of DVs and IVs
Step 2: Create an Amelia object from the model specified in Step 1. Meld the imputations to get regression coefficients
Step 3: Extract the imputed from the Amelia obejct 
Step 4: Use the extracted values from Step 3 to manually calcualte mean across imputations (this is done by the meld process in Step 2, but it stay under the hood).
Step 5: Calcualte y-hat values usign the means from step 4 and the regressio ncoefficients from step 2
Step 6: Calcualte sum-of-squares of the y-hat values from step 5 and the observed/imputed y values
Step 7: Calcualte the multipel R-squared effect size using the values from step 6
Step 8: Use a specified R-squared adjustment to correct for model size and observations (UNDER CONSTRUCTION)
END: Output the Multiple R-squared effect size.
