
# info for model run report - move to RMD
author <<- "Kendra Kaiser"
todays_date <<- "02/05/2023"

#TODO: move knit results to shiny so users can click - "download report"
# knit Model Results PDF
#detach(package:plyr) #plyr interferes with a grouping function needed for plotting
#params_list = list(fig_dir_mo_rmd = fig_dir_mo_rmd, set_author = author, 
#                  todays_date=todays_date, data_dir = data_dir, 
#                 git_dir = git_dir, input_data = input_data, run_date=run_date)

# update the Rmd to work with figures in the shiny folder and create a "make pdf" button
# knit PDF - if it doesn't work you can open the 'ModelOutputv2.Rmd' and press 'knit'
#rmarkdown::render(file.path(git_dir, 'ModelOutputv2.Rmd'), params = params_list, 
#output_file = file.path(git_dir, paste0("ModelOutput-", end_date, ".pdf")))