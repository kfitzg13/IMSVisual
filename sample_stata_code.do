
* Block 2
* Read cross-sectional data
import delimited "https://raw.githubusercontent.com/kfitzg13/IMSVisual/main/cross_sectional_data.csv", clear

* Order data by ID
sort id

* create a new variable so our reference category is rrms
encode mstype, gen(mstype_num)
encode sex, gen(sex_num)
* Fit a mixed-effects model
mixed gcipl age i.sex_num ib0.mstype_num || id:, reml



* Block 3
* Read longitudinal data
import delimited "https://raw.githubusercontent.com/kfitzg13/IMSVisual/main/participants_data_long.csv", clear

* Inspect the data
list in 1/15
sort id time
list in 1/15

* Fit a mixed-effects model
encode mstype, gen(mstype_num)
encode sex, gen(sex_num)

mixed gcipl age i.sex_num ib0.mstype_num time ib0.mstype_num#c.time || id: || eye: time, reml

* Custom hypothesis testing for interaction effect                   
* Display confidence intervals
lincom _b[time] + _b[1.mstype_num#c.time], level(95)
                                          
