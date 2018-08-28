## Title
*Educational Note: Paradoxical Collider Effect in the Analysis of Non-Communicable Disease Epidemiological Data: a reproducible illustration and web application*   

**Authors**  
Miguel Angel Luque-Fernandez, Michael Schomaker, Daniel Redondo-Sanchez,
Maria Jose Sanchez Perez, Anand Vaidya and Mireille E. Schnitzer  

## Abstract

A collider for a certain pair of variables (e.g. outcome: A and exposure: Y) is a third variable that is their common effect (W). Controlling for, or conditioning the analysis on (i.e., stratification or regression) a collider, can introduce a spurious association between its causes. This is potentially explaining why the medical literature is full of paradoxical findings, where established risk factors become protective effects when conditioning on a collider. In DAG terminology, a collider is the variable in the middle of an inverted fork (i.e., variable W in A -> W <- Y). Based on a motivating example from non-communicable disease epidemiology, we generated a dataset with 1,000 observations and run Monte Carlo simulations to contextualize and explain the effect of conditioning on a collider. We estimated the effect of 24-hour dietary sodium intake in grams on systolic blood pressure in mmHg controlling for the effect of age in years acting as a confounder and 24-hour urinary protein excretion in mg acting as a collider. We illustrated how adding a collider to a regression model introduces selection bias. Thus to prevent paradoxical associations, epidemiologists estimating causal effects should not condition on a collider. We provide R-code in easy-to-read boxes throughout the manuscript and a GitHub repository: https://github.com/migariane/ColliderApp for reproducibility and an educational web application allowing real-time interaction to visualize the paradoxical effect of conditioning on a collider http://watzilei.com/shiny/collider/.  

**Keywords**:  
epidemiological methods, causality, noncommunicable disease epidemiology  

This repository provide free open access for replicability and educational purposes to the data and code used in the educational note:  

1. app.R 
2. code_boxes_article.R  

Please **cite** this repository as follows:    

Miguel Angel Luque-Fernandez, Michael Schomaker, Daniel Redondo-Sanchez,
Maria Jose Sanchez Perez, Anand Vaidya and Mireille E. Schnitzer (2018). *Educational Note: Paradoxical Collider Effect in the Analysis of Non-Communicable Disease Epidemiological Data: a reproducible illustration and web application.* GitHub Repository: https://github.com/migariane/ColliderApp  


**Figure**. Time of cancer death in minutes of the day in acute oncology and cancer palliative wards in Hong Kong, 2008 to 2016.  

![Figure Link](https://github.com/migariane/ColliderApp/blob/master/Figure.tiff) 
 
**Acknowledgment**: 
Miguel Angel Luque Fernandez is supported by the Spanish National Institute of Health, Carlos III Miguel Servet I Investigator Award (CP17/00206). Maria Jose Sanchez Perez is supported by the Andalusian Department of Health. Research, Development and Innovation Office project grant PI-0152/2017. Anand Vaidya was supported by the National Institutes of Health (grants DK107407 and DK115392) and by the Doris Duke Charitable Foundation (award 2015085). Mireille E. Schnitzer is supported by a New Investigator Salary Award from the Canadian Institutes of Health Research.  

