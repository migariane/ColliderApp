## Title
*Educational Note: Paradoxical Collider Effect in the Analysis of Non-Communicable Disease Epidemiological Data: a reproducible illustration and web application*   

**Authors**  
Miguel Angel Luque-Fernandez, Michael Schomaker, Daniel Redondo-Sanchez,
Maria Jose Sanchez Perez, Anand Vaidya and Mireille E. Schnitzer  

## Abstract

Classical epidemiology has focused on the control of confounding but it is only recently that epidemiologists have started to focus on the bias produced by colliders. A collider for a certain pair of variables (e.g., an outcome Y and an exposure A) is a third variable (C) that is caused by both. In DAGs terminology, a collider is the variable in the middle of an inverted fork (i.e., the variable C in A -> C <- Y). Controlling for, or conditioning an analysis on a collider (i.e., through stratification or regression) can introduce a spurious association between its causes. This potentially explains many paradoxical findings in the medical literature, where established risk factors for a particular outcome appear protective. We used an example from non-communicable disease epidemiology to contextualize and explain the effect of conditioning on a collider. We generated a dataset with 1,000 observations and ran Monte-Carlo simulations to estimate the effect of 24-hour dietary sodium intake on systolic blood pressure, controlling for age, which acts as a confounder, and 24-hour urinary protein excretion, which acts as a collider. We illustrate how adding a collider to a regression model introduces bias. Thus, to prevent paradoxical associations, epidemiologists estimating causal effects should be wary of conditioning on colliders. We provide R-code in easy-to-read boxes throughout the manuscript and a GitHub repository (https://github.com/migariane/ColliderApp) for the reader to reproduce our example. We also provide an educational web application allowing real-time interaction to visualize the paradoxical effect of conditioning on a collider http://watzilei.com/shiny/collider/.  

**Keywords**:  
epidemiological methods, causality, noncommunicable disease epidemiology  

This repository provide free open access for replicability and educational purposes to the data and code used in the educational note:  

1. app.R 
2. code_boxes_article.R  

Please **cite** this repository as follows:    

Miguel Angel Luque-Fernandez, Michael Schomaker, Daniel Redondo-Sanchez,
Maria Jose Sanchez Perez, Anand Vaidya and Mireille E. Schnitzer (2018). *Educational Note: Paradoxical Collider Effect in the Analysis of Non-Communicable Disease Epidemiological Data: a reproducible illustration and web application.* GitHub Repository: https://github.com/migariane/ColliderApp  


**Figure**. Collider web application 2018.  

![Figure Link](https://github.com/migariane/ColliderApp/blob/master/Figure.png) 
 
**Acknowledgment**: 
Miguel Angel Luque Fernandez is supported by the Spanish National Institute of Health, Carlos III Miguel Servet I Investigator Award (CP17/00206). Maria Jose Sanchez Perez is supported by the Andalusian Department of Health. Research, Development and Innovation Office project grant PI-0152/2017. Anand Vaidya was supported by the National Institutes of Health (grants DK107407 and DK115392) and by the Doris Duke Charitable Foundation (award 2015085). Mireille E. Schnitzer is supported by a New Investigator Salary Award from the Canadian Institutes of Health Research.  

