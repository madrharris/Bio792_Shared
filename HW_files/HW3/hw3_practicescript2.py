


### Create a documented python script that will do the following two things. 
##For each task, first write the pseudocode, comment out the pseudocode and beneath the pseudocode write the script.

##a. Create a list of numbers (any numbers you like). 
## Then loop through the items in the list adding 1 to every number and print those numbers.

## b. Create a dictionary of animals and their sizes (make up whatever you want). 
##Print out the keys of the dictionary.
##Make a list of all the animals and then write an if else statement to print out the animal name and the word “big” if the weight is over 20 grams and the word “small” if the weight is less than 20 grams.


#!/usr/local/bin/python3


## A. 
list1 = list(range(0,51))	## create the first list, with values 0-50
list2 = []					## empty list for the new numbers

for n in list1:
	list2 += [n+1]			## adds 1 to each value
	
print(list2)
print(list1)				## check to see if it worked


print("Part B")				## page break



Animalweight_grams = {
	'Elephant':'7000'
	'Horse':'3000'
	'Mouse':'10'
	'Cat':'40'
	'Dog':'60'
	'Fish':'10'
	'plant':'15'
}





