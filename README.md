# LCTMtools

Latent Class Trajectory Modelling Tools: an R Package  

Maintainer: This R package is no longer maintained. 
Last Updated: August 2019


To install the R package, in the R console use the command 
```{r}
install.packages("devtools")
devtools::install_github("hlennon/LCTMtools")
```  


All statistical (R and SAS) codes used to implement Latent Class Trajectory Modelling and the tools described in the manuscript "A framework to construct and interpret Latent Class Trajectory Modelling", are available here and can be downloaded from www.github.com/hlennon/LCTMtools.  

An example (simulated) dataset 'bmi' and 'bmi_long' (long format version) is provided to describe the steps throughout.



### Reference  
Lennon H, Kelly S, Sperrin M, et al., Framework to construct and interpret Latent Class Trajectory Modelling, BMJ Open 2018;8:e020683.   

Available at
https://bmjopen.bmj.com/content/8/7/e020683

Supplementary material contains extra details:
https://bmjopen.bmj.com/content/bmjopen/8/7/e020683/DC1/embed/inline-supplementary-material-1.pdf?download=true



## Help Files
There are two help manuals available above:    
+ 1) The standard R manual detailing the input and outputs of each of the functions, called LCTMtools.pdf  
https://github.com/hlennon/LCTMtools/blob/master/vignettes/LCTMtools.pdf   
+ 2) A vignette with a guided example, named LCTMtools-vignette 
http://rpubs.com/hlennon/LCTMtoolsvignette   or https://github.com/hlennon/LCTMtools/blob/master/vignettes/LCTMtoolsvignette.pdf 


## Brief Example

```{r eval=TRUE}
library(LCTMtools)
data(bmi_long, package = "LCTMtools" )


# Use the hlme function from the 'lcmm' R package to fit a 2 class latent class trajectory model
set.seed(100)
library(lcmm)
model2classes <- lcmm::hlme(fixed = bmi ~ age + I(age^2), 
                      mixture= ~ age, 
                      random = ~ age, 
                      ng = 2, 
                      nwg = TRUE,  
                      subject = "id", 
                      data = data.frame(bmi_long[1:500, ]) )


# Compute model adequacy measures
LCTMtoolkit(model2classes)


# Compare with a 3 class model
set.seed(100)
model3classes <- lcmm::hlme(fixed = bmi ~ age + I(age^2), 
                      mixture= ~ age, 
                      random = ~ age, 
                      ng = 3, 
                      nwg = TRUE,  
                      subject = "id", 
                      data = data.frame(bmi_long[1:500, ]) )


LCTMtoolkit(model3classes)

LCTMcompare(model2classes, model3classes)
```  


### Citation
Please cite as 

Hannah Lennon. {LCTMtools}: Latent Class Trajectory Models tools R Functions. R package version 0.1.2.


Lennon H, Kelly S, Sperrin M, et al
    Framework to construct and interpret Latent Class Trajectory Modelling
    BMJ Open 2018;8:e020683. doi: 10.1136/bmjopen-2017-020683


### Thanks
A special thank you to Charlotte Watson for testing.  

### Contributing
Please note this R package is no longer maintained. The R package is open. Fork requests for contributions are encouraged.

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.
