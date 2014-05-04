HMIS Data Analyst (Oracle)
==========================
- **Version:**  2
- **Date:**  10 April 2014
- **RDBMS:**  Oracle
- **Administrators:**  Jason Rodriguez (Vanderbilt University); Dave Totten (Georgia Department of Community Affairs); Brian Vogler (Online Consultants)
- **Contact:**  jason.m.rodriguez@vanderbilt.edu; dave.totten@dca.ga.gov; brian@vogler.com

Description
------------
- HMIS Data Analyst is an open-source library of applications designed to provide an intuitive platform for analysis of Homeless Management Information System (<a href=http://en.wikipedia.org/wiki/Homeless_Management_Information_Systems>HMIS</a>) data. The ultimate goal of this project is to help <a href=http://www.endhomelessness.org/>end homelessness</a> by creating transparency, advancing knowledge, and facilitating research related to America's homeless service provider systems.

<div align="center"><a href=http://72.242.201.147:3838/Data%20Quality%20Oracle/>Check out our first app!</a></div>

General Usage Notes
--------------------
- The HMIS Data Analyst applications are written using the open-source <a href=http://cran.us.r-project.org/>R software environment</a> (version >= 3.0.0). In particular, they utilize the <a href=http://shiny.rstudio.com/>Shiny package</a> (GitHub <a href=https://github.com/rstudio/shiny>here</a>) for R. The technical goal is to present a simple web-based user interface that translates R statistical analyses into useful information that ordinary people can easily digest.
- For now, this project's primary source of data is de-identified client-level administrative data collected by homeless service providers throughout the entire state of Georgia. At any given time, we have access to 5 years' worth of data.
- This GitHub version of HMIS Data Analyst currently requires a keyfile to connect to the RDBMS. The keyfile is not public and must be stored locally on the contributor's machine. This is necessary for safeguarding the privacy of our homeless clients.
- Data Analyst applications are browser-based--but they may not work properly on Internet Explorer. Chrome is recommended.
- The primary languages used in writing these applications are R and SQL. To a small extent, they also utilize HTML and JavaScript. A contributor with a working knowledge of JavaScript would have the potential to vastly improve the functionality and appearance of Data Analyst.
- Every time a new version of a Data Analyst application is completed, the administrators will copy it onto a private Linux server and publish it to the web. 

Installation Notes for Contributors
-------------------------------------------
- The HMIS Data Analyst GitHub project requires local installations of R version 3.1.0, various R packages, a JDBC driver, and a keyfile. Windows users can receive a setup file that installs all these components at once. Please contact <a href mailto:dave.totten@dca.ga.gov>Dave</a> or <a href mailto:jason.m.rodriguez@vanderbilt.edu>Jason</a> for further guidance.

First Oracle, Then MySQL
-------------------------
- Georgia's HMIS administrators are in the process of transitioning to a new open-source version of HMIS (OpenHMIS). At the moment, the Data Analyst project is being developed for the existing Oracle RDBMS. Eventually, the entire project will be converted to communicate with a MySQL RDBMS. To enable current development and ease this future conversion, two duplicate repositories have been created on the hmis-analyst GitHub.
