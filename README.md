# OCC-OCAT_Shiny
My scripts for an R-Shiny app for processing OCAT data to generate statistics and graphs.
The Data does need to already be formatted properly, according to the scripts in https://github.com/GuestJim/OCC-OCAT.

Links to ShinyApps.io applets:
* https://guestjim.shinyapps.io/OCC-OCAT_Dying_Light_2/
	* full applet, using data from Dying Light 2 Review (http://www.overclockersclub.com/reviews/dying_light_2_review)
* https://guestjim.shinyapps.io/OCC-OCAT_Dying_Light_2_Tables/
	* tables only, using data from Dying Light 2 Performance Analysis (http://www.overclockersclub.com/reviews/dying_light_2_performance)

The Shiny package for R allows the creation of web applets so the processing can be interactive.
In this case that means the ability to:
* select what statistics are calculated in the tables
* add statistics to the table
	* custom percentiles
	* custom ECDF frame rates
* enable/disable configurations in tables
	* GPU
	* API
	* Quality
	* Location
* change datatype for tables
* control rounding in tables
* enable/disable faceting groups in graphs (except Course graph)
* change datatype for graphs
* change scales for graphs
* new Consecutive Difference (Percentage) graph
	* Y-scale is difference as a percentage of the measurement, as stuttering impact changes with performance
* get full-size graphs of single facets
	* double-click on a facet or select via lists and "Update Single-Plot" button
* zoom in on certain graphs
	* Course
	* QQ
* stats for brushed/highlighted sections of certain graphs
	* Course
	* Frequency
	* QQ
	* Consecutive Difference
	* Consecutive Difference (Percentage)
* experiment with theoretical line on QQ graph

The purpose is presently not to enable the downloading of data, statistics, or graphs. Downloading of tables can be toggled on via.

At present the scripts can be set to work with a provided CSV, compressed CSV, or RData file, as created with ***OCAT to RData.r***, or to allow a file to be uploaded and processed.
For clarity and simplicity, I believe the static-data version is best for sharing.
However, the ***app.r*** script is configured for upload presently, as I am not including data in this repository (at the moment, at least).

Since originally making them, I have learned about Lag graphs, which plot the data against the data but shifted by some amount, nromally 1.
(The X axis is n + 1 and Y is n, for example.)
My Consecutive Difference graphs then are possibly a variant of lag graphs, as they are the data on one axis and the difference between n and n + 1 on the other.
Additional information: [NIST Lag Plot in Handbook](https://www.itl.nist.gov/div898/handbook/eda/section3/lagplot.htm)