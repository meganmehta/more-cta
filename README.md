# CS 424 Project 2 - Project 2 - Don't Sleep on the Subway

This code repository contains an app.R file along with a set of .tsv files and one .csv. The app.R file contains all of the code needed to run this project. All the data from the .tsv files comes from the [CTA-Ridership L-Station Entries, Daily Totals](https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f) dataset, while the .csv file contains 
data from the [CTA - System Information - List of 'L' Stops](https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme) dataset. This data is available from the [Chicago Data Portal](https://data.cityofchicago.org/). 

To run this project locally, make sure to clone this repo on your computer in whatever location you choose. Here are some simple steps below: 

```
cd 
git clone https://github.com/meganmehta/more-cta.git
```
Along with cloning this repo, make sure to download [R](https://www.r-project.org/ ) (v 4.1.2) and [R Studio](https://www.rstudio.com/products/rstudio/download/ )(v 2021.09).

Once you have R and R Studio downloaded, open this project file in R Studio. You can do this by setting the working directory: Session -> Set Working Directory -> Choose Directory
![Session -> Set Working Directory -> Choose Directory](https://github.com/meganmehta/cta_rides/blob/main/documentation1.jpg)

After the working directory is set, open the app.r file within R Studio. This file contians all of the code in this project. 

Before running this application, make sure that all necessary packages/libraries are downloaded. To check what libraries are currently added, type the command
`installed.packages()` in the R Studio Console. If some packages are missing, use `install.packages("package-name-here")` to install the remaining ones necessary. 
All of the needed libraries are listed at the top of the app.R file. The ggplot and leaflet packages are the most used ones in this project.

After that, you should be good to run the project. Click the 'Run App' button on the right. 
![](https://github.com/meganmehta/cta_rides/blob/main/documentation2.jpg). 

An additional window will pop up with the visualization and you're all set. 

[Here](https://meganmehta.shinyapps.io/424_project_2/) is the link to my deployed version! If you'd like to read more about the project details, check out
the documentation [here](https://mmehta25.people.uic.edu/project2.html).
