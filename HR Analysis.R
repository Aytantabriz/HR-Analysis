data <- read.csv("C:/Users/Admin/Desktop/competition/train_LZdllcl.csv")%>% mutate_if(is.character,as.factor)
names(data)
data$is_promoted <- data$is_promoted %>% as.factor()
data %>% skim()

#-----------------------------------------work with NA ----------------------------------------------
string_2_factor_names <- data %>% select_if(is.character) %>% names()
unique_numeric_values_tbl <- data %>% select_if(is.numeric) %>% map_df(~ unique(.) %>% length()) %>%
  gather() %>% arrange(value) %>% mutate(key = as_factor(key))
num_2_factor_names <- unique_numeric_values_tbl %>% filter(value < 7) %>% arrange(desc(value)) %>%
  pull(key) %>% as.character()
rec_obj <- recipe(~ ., data = data)%>% step_meanimpute(all_numeric()) %>%
  step_modeimpute(all_nominal()) %>% prep(stringsAsFactors = FALSE)
data_all <- bake(rec_obj, data) 
data_all %>% skim()

#------------------------------------------H2O model ------------------------------------------------
library(h2o)
h2o.init()
h2o_data <- as.h2o(data_all)

h2o_data <- h2o.splitFrame(h2o_data,ratios = c(0.7,0.15),seed=123)
train<-h2o_data[[1]]
validation<-h2o_data[[2]]
test<-h2o_data[[3]]

outcome <- 'is_promoted'
features <- setdiff(colnames(data),outcome)

aml <- h2o.automl(y = outcome,
                  x = features,
                  training_frame = train,
                  validation_frame = validation,
                  leaderboard_frame = test,
                  seed=123,
                  max_runtime_secs = 500)
aml@leader
aml@leaderboard %>% as_tibble() %>% head(.,10)

model_path <- h2o.saveModel(object=aml@leader, path=getwd(), force=TRUE)

#------------------------------------------ Prediction ----------------------------------------------
new <- read.csv("C:/Users/Admin/Desktop/competition/test_2umaH9m.csv")
newdata <- as.h2o(new)
h2oModelLoaded <- h2o.loadModel(model_path)
prediction <- as.data.frame(predict(h2oModelLoaded, newdata = newdata))
prediction
write_xlsx(prediction,'prediction.xlsx')

