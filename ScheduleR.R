
# load libraries needed to do the task
library(taskscheduleR)

# creating a variable to find the R script that we want to run
# extdata is in the taskscheduleR package folder which is where I stored the script "Dublin_Bike_API_Call.R"
myscript <- system.file("extdata", "Dublin_Bike_API_Call.R", package = "taskscheduleR")

# creating a variable to define when to start our process - this starts the process in 2 minutes from now
runon <- format(Sys.time() + 120, "%H:%M")

# creating the task
taskscheduler_create(taskname = "dublinbikecall", rscript = myscript, 
                     schedule = "HOURLY", starttime = runon)

# creating a variable to see all tasks in the task manager
# x <-taskscheduleR::taskscheduler_ls()

# viewing the df containing all tasks
# View(x)

# command to delete the script you created, if necessary
# taskscheduler_delete("dublinbikecall")

