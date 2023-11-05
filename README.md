# Network Analysis
 Survey on the relationships among the degree programs at the University of Padova.

## Introduction
In this project we extract data from degree program syllabus web page of the University od Padova (https://www.didattica.unipd.it) through web scraping technique.
We then convert text data into network structured data and perform a community detection for directed graph.

## Structure
The project is structured as follows:
- `Analysis.R`: contanins the code for replicating the analysis.
- `school_net_lib.R`: contains all the functions defined to perfrom the analysis from the data retrivial to the community detection.
- In the `figures` folder there are the plots of the network made using the software Gephi.
- `Scuola_di_Ingegneria.csv` and `Scuola_di_Scienze.csv` are the data retrived from the web scraping.

## Next Steps
- [ ] Create a Shiny app to visualize the network interactively.
