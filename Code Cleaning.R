#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("AAIN2JDE.csv")
dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID,
         agegr10,
         sex,
         marstat,
         jsr_02,
         ttlincg2,
         esc1_01,
         tmg_03,
         fam_03,
         oda_01m,
         dbh_01,
         sfc_05,
         stj_05,
         wet_110,
         
  ) %>% 
  mutate_at(vars(agegr10:wet_110), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(agegr10:stj_05),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agegr10,
         job_satisfaction = jsr_02,
         income = ttlincg2,
         marry = marstat,
         school = esc1_01,
         competition = sfc_05,
         satisfaction_balance = tmg_03,
         satisfaction_familytime = fam_03,
         outdoor_activity = oda_01m,
         discrimination = dbh_01,
         match_JobandEducation = stj_05,
         weeks_employed = wet_110
  ) 

#### Clean up ####

gss <- gss %>% 
  mutate_at(vars(age:match_JobandEducation), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated"|.=="Don't know", "NA", .))) 

gss <- gss %>% 
  mutate_at(vars(job_satisfaction), .funs = funs(case_when(
    .=="Very satisfied"~1,
    .=="Satisfied"~1,
    .=="Neither satisfied nor dissatisfied"~0,
    .=="Dissatisfied"~0,  
    .=="Very Dissatisfied"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(satisfaction_balance), .funs = funs(case_when(
    .=="Very satisfied"~1,
    .=="Satisfied"~1,
    .=="Neither satisfied nor dissatisfied"~0,
    .=="Dissatisfied"~0,  
    .=="Very Dissatisfied"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(marry), .funs = funs(case_when(
    .=="Married"~1,
    .=="Living common_law"~0,
    .=="Widowed"~0,   
    .=="Separated"~0, 
    .=="Divorced"~0,
    .=="Single, never married"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(school), .funs = funs(case_when(
    .=="Yes"~1,
    .=="No"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(outdoor_activity), .funs = funs(case_when(
    .=="Yes"~0,
    .=="No"~1,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(match_JobandEducation), .funs = funs(case_when(
    .=="Completely"~1,
    .=="Mostly"~1,
    .=="Somewhat"~1,
    .=="Mostly not"~0,
    .=="Not at all"~0,
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(competition), .funs = funs(case_when(
    .=="Always"~1,
    .=="Often"~1,
    .=="Sometimes"~1,
    .=="Rarely"~0,
    .=="Never"~0,
    .=="Do not have any colleagues"~0,    
    .=="NA"~as.numeric(NA)
  )))

gss <- gss %>% 
  mutate_at(vars(income), .funs = funs(case_when(
    .=="Less than $25,000"~ "Less than 50,000",
    .=="$25,000 to $49,999"~"Less than 50,000",
    .=="$50,000 to $74,999"~ "Between 50,000 to 100,000",
    .=="$75,000 to $99,999"~ "Between 50,000 to 100,000",
    .=="$100,000 to $124,999"~"More than 100,000",
    .=="$125,000 or more"~"More than 100,000"
  )))


gss <- na.omit(gss)

write_csv(gss, "gss.csv")
