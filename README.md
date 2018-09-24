# LCTMtools

Latent Class Trajectory Modelling Tools: an R Package  

Maintainer: Hannah Lennon  
Contact: hannah.lennon.manchester@gmail.com  
Last Updated: 24th September 2018


To install the R package, in the R console use the command 
```{r}
devtools::install_github("hlennon/LCTMtools")
```  


All statistical (R and SAS) codes used to implement Latent Class Trajectory Modelling and the tools described in the manuscript "A framework to construct and interpret latent class trajectory modelling", are available here and can be downloaded from www.github.com/hlennon/LCTMtools.  

An example (simulated) dataset 'bmi' and 'bmi_long' (long format version) is provided to describe the steps throughout.

*Reference*
Lennon H, Kelly S, Sperrin M, et al Framework to construct and interpret latent class trajectory modelling BMJ Open 2018;8:e020683. doi: 10.1136/bmjopen-2017-020683

Available at
https://bmjopen.bmj.com/content/8/7/e020683




## Example

```{r eval=TRUE}
library(LCTMtools)
set.seed(002010800)
data(bmi_long, package = "LCTMtools" )


# Use the hlme function from the 'lcmm' R package to fit a 2 class latent class trajectory model
model2classes <- hlme(fixed = BMI ~ Age + I(Age^2), 
                      mixture= ~ Age, 
                      random = ~ Age, 
                      ng = 2, 
                      nwg = TRUE,  
                      subject = "ID", 
                      data = bmi_long[1:500, ] )


# Compute model adequacy measures
LCTMtoolkit(model2classes)


# Compare with a 3 class model
model3classes <- hlme(fixed = BMI ~ Age + I(Age^2), 
                      mixture= ~ Age, 
                      random = ~ Age, 
                      ng = 3, 
                      nwg = TRUE,  
                      subject = "ID", 
                      data = bmi_long[1:500, ] )

LCTMtoolkit(model3classes)

LCTMcompare(model2classes, model3classes)
```  
