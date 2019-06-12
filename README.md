# Entity-Recognition-on-Twitter-data
NLP : Identifying common characteristics of Twitter followers according to the News Channel followed.

Entity Recognition on Twitter Handles' description.

Data collection through Twitter's API.
Comparison of several methods:
- existing NLP packages
- dictionary based matching (hand-created)
- Machine Learning approaches (CRF & Random Forest) (after labeling)

Deliverables: R & Python Scripts + RMarkdown for presenting the results

## Organization
The final report is available as an RMarkdown, and pdf file.

The scripts written for the analysis can be found on this repository.


The R scripts can be found in Entity Extraction_ Rcode_Dictionarylookup_Machinelearning :
 - Extraction of data, preprocessing and dictionary matching : Concept extraction project_Dictionary lookup.R 
 - Building and evaluation of the CRF model : Concept extraction_Machine learning_CRF.R 

 
 The Python scripts can be found in Entity Extraction_Pythoncode_Spacy_Machine learning :
 - Attemp at using the Spacy package for automated NER : NLP_spacy_NER.ipynb
 - Building and evaluation of the memory-based and random forest algorithm : Machine Learning_Random Forest NER.ipynb

 
 Intermediary data files written by the multiple scripts can be found in Files saved.
