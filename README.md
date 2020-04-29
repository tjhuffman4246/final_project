# Pitch Sequencing in Major League Baseball

Written as the final project for Harvard's Gov 1005, a data science class taught by Preceptor David Kane, this Shiny app examines trends in pitch sequencing in the MLB and explores its possible effects on plate appearance outcomes. The `data` folder contains a CSV file of all pitches in the MLB for each year since 2015 in addition to a CSV of player names and MLB identification numbers; `shiny_initial` contains the Shiny app for this project.

### `about.Rmd`

Contains inital draft of Shiny app's "About" page, the knitted HTML output of which is contained in `about.html`.

### `explore.Rmd`

Initial code used to clean the data, which is replicated in other Rmarkdown files. Also contains code used to produce output for various project milestones. Knitted HTML output in `explore.html`.

### `gather.Rmd`

Code to scrape Baseball Savant, download pitch data, and combine into a single dataset. Knitted HTML output in `gather.html`.

### `graphics.Rmd`

Uses downloading/cleaning code from `explore.Rmd` to access data, producing plots used in the Shiny app's homepage.

### `model.Rmd`, `multinom_reg.Rmd`

Relatively similar documents, the use of which is limited by scale of data. The latter produced the predictions used in the "Model" section of the Shiny app, and it took much of its code from the former, which was initially created to create a linear regression model to predict wOBA and then altered to use a random forest to classify pitches. The inaccuracy of the first model led to the switch to classification, and the second was too computationally expensive to use. The code for both remains here in GitHub, however.