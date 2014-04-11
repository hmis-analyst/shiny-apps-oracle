HMIS Data Analyst
==========================
- **Version:**  2.1.1
- **Date:**  10 April 2014
- **RDBMS:**  Oracle
- **Administrators:**  Jason Rodriguez (GA Dept of Community Affairs); Brian Vogler (); Tony Zhang (Pathways Community Network Institute)
- **Contact:**  jsn.rgz@gmail.com; brian@vogler.com; tony.zhang@pcni.org

Description
------------
- HMIS Data Analyst is an open-source library of applications designed to provide an intuitive platform for analysis of Homeless Management Information System (<a href=http://en.wikipedia.org/wiki/Homeless_Management_Information_Systems>HMIS</a>) data. The ultimate goal of this project is to help end homelessness by creating transparency, improving knowledge, and facilitating research related to America's homeless service 
provider systems.
- These applications make use of the open-source <a href=http://cran.us.r-project.org/>R software environment</a> (version >= 3.0.0). In particular, they utilize the <a href=http://shiny.rstudio.com/>Shiny</a> package for R (see <a href=https://github.com/rstudio/shiny>GitHub</a>). The technical goal is to present a simple user interface that translates R statistical analyses into useful information for the interested layperson.

General Usage Notes
--------------------
- This open-source version of HMIS Data Analyst currently requires a keyfile to connect to the RDBMS. The keyfile is not public and must be stored locally on the developer's machine. Please contact one of the administrators for a copy.
- HMIS Data Analyst applications are browser-based. They will not work properly on Internet Explorer. Chrome is recommended.
- Each new version of a Data Analyst application is moved onto a private server and will eventually be published to the web. 
- The primary languages used in writing these applications are R and SQL. To a small extent, they also utilize HTML and JavaScript. In terms of both functionality and appearance, JavaScript offers the most potential for Data Analyst improvement.
- The JDBC API is used to communicate with the Oracle RDBMS.

Installation Notes for Contributors
-------------------------------------------
- The HMIS Data Analyst open-source project requires a local installation of R version >= 3.0.0.
- Applications will have varying R package dependencies (see source code). All dependencies must be installed before applications are sourced.

MySQL versus Oracle
--------------------
- The database administrators are in the process of transitioning to a new open-source version of HMIS (OpenHMIS). At the moment, the Data Analyst project is being developed for the existing Oracle RDBMS. Eventually, the entire project will be converted to communicate with a MySQL RDBMS. To enable current development and ease this future conversion, two duplicate repositories have been created on GitHub.
