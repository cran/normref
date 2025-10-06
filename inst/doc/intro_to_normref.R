## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadlibrary, echo=TRUE, include=TRUE-------------------------------------
library(normref)
library(gamlss.dist) # also needed for vignette

## ----getdata, echo=TRUE, results='hide'---------------------------------------
get(data("ids_data"))

## ----shapedata, echo=TRUE, include=TRUE---------------------------------------
mydata_BB <- shape_data(data = ids_data,
                     age_name = "age",
                     score_name = "y14",
                     family = "BB",
                     max_score = 34)

## ----selectmodel_BCPE_y14, echo=TRUE, include=TRUE----------------------------
mydata_BCPE <- shape_data(data = ids_data,
                     age_name = "age",
                     score_name = "y14",
                     family = "BCPE")


## ----selectmodel_BB_y14, echo=TRUE, include=TRUE------------------------------
mod_BB_y14 <- fb_select(data = mydata_BB,
                       age_name = "age",
                       score_name = "shaped_score",
                       family = "BB",
                       selcrit = "BIC")

## ----selectmodel_BCPENO_y14, echo=TRUE, include=TRUE--------------------------
mod_BCPE_y14 <- fb_select(data = mydata_BCPE,
                         age_name = "age",
                         score_name = "shaped_score",
                         family = "BCPE",
                         selcrit = "BIC") 

mod_NO_y14 <- fb_select(data = ids_data,
                      age_name = "age",
                      score_name = "y14",
                      family = "NO",
                      selcrit = "BIC")

## ----showfit, echo=TRUE, include=TRUE-----------------------------------------
mod_BB_y14$selcrit
mod_BCPE_y14$selcrit
mod_NO_y14$selcrit

## ----show_wormplot, echo=TRUE, include=TRUE, fig.width=6, fig.height=4--------
gamlss::wp(mod_BCPE_y14, ylim.all = 2)
gamlss::wp(mod_BB_y14, ylim.all = 2) 
gamlss::wp(mod_NO_y14, ylim.all = 2) 

## ----show_wormplot_age, echo=TRUE, include=TRUE, fig.width=6, fig.height=4, warning=FALSE, message = FALSE----
gamlss::wp(mod_BCPE_y14, xvar = age, ylim.worm = 2, n.inter = 4)

## ----plot_model_BCPE, echo=TRUE, include=TRUE, fig.width=6, fig.height=4------
gamlss::centiles(mod_BCPE_y14, cent = c(5,25,50,75,95))

## ----normtable_BCPE, echo=TRUE, include=TRUE----------------------------------
temp_file <- tempfile(fileext = ".xlsx")
norm_mod_BCPE_y14  <- normtable_create(model = mod_BCPE_y14,
                                      data = mydata_BCPE,
                                      age_name = "age",
                                      score_name = "shaped_score", 
                                      normtype = "IQ", 
                                      excel_name = temp_file,
                                      excel = TRUE)
head(norm_mod_BCPE_y14[["norm_matrix"]][, c(1, 11)],
     n = 15)

## ----plot_normtable_y14, echo=TRUE, include=TRUE, fig.width=6, fig.height=4----
plot_normtable(normtable = norm_mod_BCPE_y14)

## ----create_normtable_y14, echo=TRUE, results='hide'--------------------------
get(data("ids_rel_data"))
temp_file <- tempfile(fileext = ".xlsx")
norm_mod_BCPE_y14  <- normtable_create(model = mod_BCPE_y14,
                                      data = mydata_BCPE,
                                      age_name = "age",
                                      score_name = "shaped_score",
                                      datarel = ids_rel_data,
                                      normtype = "IQ",
                                      excel_name = temp_file,
                                      excel = TRUE)

## ----modelnorms_y7, echo=TRUE, include=TRUE-----------------------------------
mydata_BCPE_y7 <- shape_data(data = ids_data,
                     age_name = "age",
                     score_name = "y7",
                     family = "BCPE")
mod_BCPE_y7 <- fb_select(data = mydata_BCPE_y7,
                        age_name = "age",
                        score_name = "shaped_score",
                        family = "BCPE",
                        selcrit = "BIC")
norm_mod_BCPE_y7 <- normtable_create(model = mod_BCPE_y7,
                                    data = mydata_BCPE_y7,
                                    age_name = "age",
                                    score_name = "shaped_score")

## ----modelnormcompositescore, echo=TRUE, include=TRUE-------------------------
composite <- list(norm_mod_BCPE_y7,
                  norm_mod_BCPE_y14)

composite_data <- composite_shape(normtables = composite)

modNO_comp <- fb_select(
  data = composite_data,
  age_name = "age",
  score_name = "z_sum",
  family = "NO",
  selcrit = "BIC"
)

norm_modNO_comp <- normtable_create(modNO_comp,
                                   composite_data,
                                   age_name = "age",
                                   score_name = "z_sum",
                                   cont_cor = TRUE)

## ----newsamplenorm, echo=TRUE, include=TRUE-----------------------------------
newdata <- ids_data[1:5,c("age","y14")]
norm_mod_BCPE_newdata <- normtable_create(model = mod_BCPE_y14,
                                         data = newdata,
                                         age_name = "age",
                                         score_name = "y14", 
                                         new_data = TRUE,
                                         datarel = ids_rel_data)
norm_mod_BCPE_newdata[["norm_sample"]]

## ----getdata_ids_kn_data, echo=TRUE, results='hide'---------------------------
get(data("ids_kn_data"))

## ----fit truncated model, echo=TRUE, include=TRUE-----------------------------

gamlss.tr::gen.trun(par=c(0,34), family="NO", name="tr34", type="both")

mydata_NOtr34 <- shape_data(data = ids_kn_data,
                     age_name = "age_years",
                     score_name = "rawscore",
                     family = "NOtr34")
mod_NOtr34_KN <- fb_select(data = mydata_NOtr34,
                        age_name = "age_years",
                        score_name = "shaped_score",
                        family = "NOtr34")

## ----item_variables_ids_kn_data, echo=TRUE, include=TRUE----------------------
item_variables_kn <- which(substr(colnames(ids_kn_data),1,2) == "KN")

## ----selectwindowstep_ids_kn_data, echo=TRUE, include=TRUE--------------------
rel_int <- different_rel(data = ids_kn_data, 
                         item_variables = item_variables_kn, 
                         step_window = c(0.5, 1 , 2, 5, 10, 20), 
                         age_name = "age_years",
                         min_agegroup = 5,
                         max_agegroup = 20,
                         step_agegroup = c(0.5,1,1.5,2))

## ----plot_selectwindow_ids_kn_data, echo=TRUE, include=TRUE, fig.width=6, fig.height=4----
plot_drel(rel_int, ncol = 2)

## ----estrel_ids_kn_data, echo=TRUE, include=TRUE------------------------------
rel_est <- reliability_window(data = ids_kn_data,
                   age_name = "age_years",
                   item_variables = item_variables_kn,
                   window_width = 2)

## ----calculate confidence intervals, echo=TRUE, include=TRUE------------------
norm_mod_BCPEtr34_KN  <- normtable_create(model = mod_NOtr34_KN,
                                     data = mydata_NOtr34,
                                     age_name = "age_years",
                                     score_name = "shaped_score",
                                     datarel = rel_est,
                                     normtype = "IQ")

