##### installing shiny and SDMTools packages ========================

if (length(setdiff("shiny", rownames(installed.packages()))) == 1) {
    install.packages("shiny",repos = "https://cloud.r-project.org/")
}

if (length(setdiff("SDMTools", rownames(installed.packages()))) == 1) {
    install.packages("D:/MOTIVE_Ecosystem/Github/Projects/SDMTools_1.1-221.2.tar.gz", repos=NULL, type="source")  
}