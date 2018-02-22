## Title of the data set


##CREATORS:

    Random Citizen <random@example.com> PI, University of Null
    
    Post Doc <post@examplec.com> CO-PI, Null State University
    
    
CONTRIBUTORS: List of people who have contributes. (Name, email, role, institution)


PROJECT DATES: Date range for when the project occurred.


PROJECT DESCRIPTION: 
    A description of the project associated with the data. What was the goal of the project?
    Briefly describe methods.


DATA DESCRIPTION:
    The data are derived from surveys sent to a variety of library workers using Qualitrix. 
    The survey results are not provided due to privacy concerns. In the interest of openness,
    we provide the sanitized data in the form of words used by the respondants. The script
    `sanitize.R` was used to sanitize the data and is provided along with the sanitized data
    in `sanitized_survey.R`. See provided data dictionary for more detail about the data.
    
REQUIRED SOFTWARE:
    R version 3.4.3 or more recent


USAGE:
    
    To rerun the the analyses, run the `survey_analysis.R` script. From the R console
    
    ```R
    source('~/r_projects/linked_data_survey/sanitize.R')
    ```


FILE MANIFEST or DATA STRUCTURE
    For data sets with just a few files, list the files and briefly describe them.
    
    critter1.bam: Sequence data file from sample critter1. 
    critter2.bam: Sequence data file from sample critter2.
    
    For data sets with lots of files, it is better to arrange your files in a directory
    structure and describe the directory structure.


CITATION: Tell users how to cite your data.

    If this data is used in a publication, please cite the following:
    
    Random Citizen and Post Doc "Cool Genomic Data", 2016. DOI: doi:10.1000/182
    
    AND/OR provide a reference to an appropriate publication.
    
    LICENSING: How your data is licensed. Data should typically be very open suchas
    
    Creative Commons Attribution 4.0 International
    
    CC0 Public Domain (no rights reserved)

