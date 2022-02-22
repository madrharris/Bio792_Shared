#!/usr/local/bin/python3
import pandas
		##use pandas.read_csv to read the file


doc = pandas.read_csv('Bloom_etal_2018_Reduced_Dataset.csv', usecols = ["taxa", "Reg"])
		## pull file, read through it, then identify columns "taxa" & "Reg" to grab. turn into dataframe
		## files has been moved to same location as script
		
print(doc)



###		adding up all log sizes
doc_add = pandas.read_csv('Bloom_etal_2018_Reduced_Dataset.csv', usecols = ["logbodysize"]) #   <-- what file to get it from and what column. Redoing the above step
print(doc_add) ##   <-- check to see if it works


col_add = "logbodysize" 		#	<--- grab this column
col_sum = doc_add[col_add].sum()			# 	<-- sum it all up	
print("The total sum value of all body sizes is: ", col_sum)		#	<-- print value



##Create a documented python script that will open up the file “Bloom_etal_2018_Reduced_Dataset”.  
##Read through the file and print out the taxon name and their diadromous status. 
## Add up all of the log body sizes and print out the total log body size for all the individuals in the file.  






