#------------------------------------------------------------------------------
#   Test on real data.
#   Copyright (C) 2014  Bertram Ieong
#------------------------------------------------------------------------------


### SETUP ###
# set directory to where the shared library is stored
setwd("~/ParallelForest/tests/")
# source("test_realdata_01.r")



# load the shared libraries compiled in Fortran
dyn.load("../src/ParallelForest.so")
is.loaded("ParallelForest")

source("../R/forest.r")
source("../R/tree.r")
source("../R/grow.forest.r")
source("../R/predict.forest.r")


trainraw = read.csv("../NOCOMMIT/census-income-mld/census-income.data")

cn = c(
	"age",
	"class_of_worker",
	"detailed_industry_recode",
	"detailed_occupation_recode",
	"education",
	"wage_per_hour",
	"enroll_in_edu_inst_last_wk",
	"marital_stat",
	"major_industry_code",
	"major_occupation_code",
	"race",
	"hispanic_origin",
	"sex",
	"member_of_a_labor_union",
	"reason_for_unemployment",
	"full_or_part_time_employment_stat",
	"capital_gains",
	"capital_losses",
	"dividends_from_stocks",
	"tax_filer_stat",
	"region_of_previous_residence",
	"state_of_previous_residence",
	"detailed_household_and_family_stat",
	"detailed_household_summary_in_household",
	"X",
	"migration_code-change_in_msa",
	"migration_code-change_in_reg",
	"migration_code-move_within_reg",
	"live_in_this_house_1_year_ago",
	"migration_prev_res_in_sunbelt",
	"num_persons_worked_for_employer",
	"family_members_under_18",
	"country_of_birth_father",
	"country_of_birth_mother",
	"country_of_birth_self",
	"citizenship",
	"own_business_or_self_employed",
	"fill_inc_questionnaire_for_veterans_admin",
	"veterans_benefits",
	"weeks_worked_in_year",
	"year",
	"Y"
	)

colnames(trainraw) = cn


# only keep the continuous and ordinal variables, and the response
train = trainraw[,c("age","wage_per_hour","capital_gains","capital_losses",
	"dividends_from_stocks","num_persons_worked_for_employer",
	"weeks_worked_in_year","Y")]
train$Y = as.integer(train$Y)
train$Y[train$Y==1] = 0
train$Y[train$Y==2] = 1

# fit forest
fforest = grow.forest(Y~., data=train, min_node_obs=10000, max_depth=10,
    numsamps=100000, numvars=5, numboots=5)

