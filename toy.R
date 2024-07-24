
#setup toy example
m = matrix(
  c(1,1,2,1,
    2,1,3,1,
    1,2,2,3,
    2,3,3,3,
    1,3,2,NA,
    2,2,3,2,
    3,1,4,NA,
    3,3,4,NA,
    3,2,4,NA),ncol=4,byrow = T)

colnames(m) = c("Year","Task","Next Year","Next Task")

#produce a data frame similar to what we awnt to do eventually
map = data.frame(m)

#combine the columns into a factor label
labels = map %>%
  mutate(label1=paste0(Task," (Yr ",Year,")"),
         label2=paste0(Next.Task," (Yr ",Next.Year,")")) 

#the tasks are defined by the left column
tasks =  factor(labels$label1)

for (year in 3:1){
  year_updates = labels %>% filter(Year==year)
  to_recode = year_updates$label2
  recode_as = year_updates$label1
  names(to_recode) = recode_as
  tasks = fct_recode(tasks,!!!to_recode)
  #for example, to_recode
  #    1 (Yr 1)    2 (Yr 1)    3 (Yr 1) 
  #  "1 (Yr 2)"  "3 (Yr 2)" "NA (Yr 2)" 
  # means replace Task 1 (Yr 2) with Task 1 (Yr 1) and Task 3 (Yr 2) with Task 2 (Yr 1)
}
#now we want to prepare our relabeling function

labels$label=tasks
labels$easy_read =tasks
levels(labels$easy_read) = c("A","B","C","D")

labels %>% select(Year,Task,label)

#label2 includes some values not in label 1

fct_recode(tasks,!!!translator)
"Illustration
1->1>->1; A
2->3->3; B
3->NA; D
->2->2; C
"