# Introduction



This is the repository for 

Peter Phillips and Zhentao Shi, "[Boosting: Why You Can Use the HP Filter,](https://onlinelibrary.wiley.com/doi/10.1111/iere.12495)" *International Economic Review*, 62(2), 521-570



It offers 
* the function `BoostedHP` in `R`, `MATLAB`, `Python` and `Julia`.
* Replication code for simulations and empirical applications in the paper.



**To R users**: a dedicated R package [BoostedHP](https://github.com/chenyang45/BoostedHP) is developed with [vignette](https://github.com/chenyang45/BoostedHP/blob/master/vignettes/vignette.pdf).




### Contributors
Zhentao Shi, Yang Chen and Ziwei Mei


### Updates
* 11/18/2020: Replication files are added into the folder `replications`.
* 8/27/2020: [python code](https://github.com/zhentaoshi/Boosted_HP_filter/tree/master/python) and [julia code](https://github.com/zhentaoshi/Boosted_HP_filter/tree/master/Julia) are added by Ziwei Mei. The bHP-ADF result are slightly different from that of the [R code](https://github.com/zhentaoshi/Boosted_HP_filter/tree/master/R) because the ADF tests cited in these languages. In the scripts, the testing examples immediately follow the main function.
* 8/21/2020: [Matlab code](https://github.com/zhentaoshi/Boosted_HP_filter/tree/master/matlab) is added into the MATLAB folder. It produces exactly the same results as the [R code](https://github.com/zhentaoshi/Boosted_HP_filter/tree/master/R). We offer two scripts under these folders:
  * `BoostedHP` contains the function to implement the automated boosted HP filter.
    The inputs and outputs are detailed in the beginning of the function.
  * `testing_example` is a script to demonstrate the usage of the filtering procedure.
* 8/18/2020: The [R code](https://github.com/zhentaoshi/Boosted_HP_filter/tree/master/R) is superseded by a new R package [BoostedHP](https://github.com/chenyang45/BoostedHP). 
  See the [vignette](https://github.com/chenyang45/BoostedHP/blob/master/vignettes/vignette.pdf) for its usage. 
