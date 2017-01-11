# Spinalcord_MappR v1.0
2017-01-10
Author: Nicolas Stifani nstifani@gmail.com

This is a script for R processing data of spinal cord mapping

1.	Installation
−	Download R for your OS from https://cran.r-project.org/
−	Install R
−	For Mac only
o	Xquartz is required. Download it from  https://www.xquartz.org/
o	Install X Quartz
−	Download Spinalcord_MappR.R from https://github.com/nstifani/Spinalcord_MappR

2.	Processing with Spinalcord_MappR.R
It will prompt to select a folder containing the CSV X and Y Coordinates for each detected cell
It will then prompt to select the Registration Coordinates CSV file generated previously in ImageJ SC Registration function
It can prompt to select the Marker Information Created by Manual Cell Detection (Spinalcord_Mapper for ImageJ)



To do List
- Add weighted density

- Get inspired by FACS data analysis

- Perform DBSCAN Clustering
http://scikit-learn.org/stable/modules/clustering.html
http://www.statmethods.net/advstats/cluster.html
http://scikit-learn.org/stable/auto_examples/cluster/plot_dbscan.html#sphx-glr-auto-examples-cluster-plot-
