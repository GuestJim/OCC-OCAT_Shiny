# OCC-OCAT_Shiny
My scripts for an R-Shiny app for processing OCAT data to generate statistics and graphs.
The Data does need to already be formatted properly, according to the scripts in https://github.com/GuestJim/OCC-OCAT.

The Shiny package for R allows the creation of web applets so the processing can be interactive.
In this case that means the ability to:
* select what statistics are calculated in the tables
* add statistics to the table
	* custom percentiles
	* custom ECDF frame rates
* enable/disable configurations in tables
* change datatype for tables
* control rounding in tables
* enable/disable facets in graphs
* change datatype for graphs
* change scales for graphs
* zoom in on certain graphs
	* Course
	* QQ
* get statistics for the zoomed in portions
* select specific facet within complex graph
	* double-click on a facet or select via lists and "Update Single-Plot" button
* experiment with theoretical line on QQ graph

The purpose is presently not to enable the downloading of data, statistics, or graphs.

At present the scripts can be set to work with a provided CSV (or compressed CSV) or to allow a file to be uploaded and processed.
For clarity and simplicity, I believe the static-data version is best for sharing.
However, the ***app.r*** script is configured for upload presently, as I am not including data in this repository (at the moment, at least).