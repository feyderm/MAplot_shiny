# MAplot_shiny
 Shiny app to create interactive MA plot for data exploration  

 MA data and stats generated by:  
   1) TopHat2 alignment  
   2) feature count with HTSeq  
   3) feature statistics with EdgeR (performed below)

 Note that this file should be in the same directory as the MAplot_shiny_app
 directory containing the the app.R file.  Also the meta-data 
 (MAplot_shiny_meta_data.csv) and sample files (sampleX.txt) should be in the 
 same directory as this file.  That is:
 
 ~/edgeR_MAplot_shiny_template.R  
 ~/MAplot_shiny_meta_data.csv  
 ~/sample1.txt  
 ~/sample2.txt  
 ~/sample3.txt  
 ~/sample4.txt  
    
  ~/MAplot_shiny_app/app.R

 Note that MA plot can be resized to default view by removing all values 
 for the x- and y-axis.  Also note, non-integer numbers can be manually 
 entered for finer resolution.
