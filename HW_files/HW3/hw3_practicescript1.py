#!/usr/local/bin/python3

##		 start with the local. or #!/usr/bin/env python3

##		run as excecutable: chmod u+x python_code_here.py



## 	Create a documented python script that would

#	a. take this number 112345678911234566 and count the number of 2s in the string and print out the number.

#	b. take a sentence from user input, turn it all to lowercase letters and remove the spaces and count the length and print out the length. 
# You choose the sentence.


number = (1,1,2,3,4,5,6,7,8,9,1,1,2,3,4,5,6,6)
numberof2s = number.count(2)
print('the number of 2s is ', numberof2s)

print(																)		## line break
print("Part B") ##		<-- line break

statement = input("Type statement here: ")		# takes the input typed in

lower = statement.lower()			## lowercase, then replacing spaces with no spaces, then get the length of the string (number of characters)
space = lower.replace(" ","")
length = len(space)
print('Your input: ',statement,' --> ',space,'\n','Number of characters: ',length)		## print the original statement, then the new edited one, adding a page break, then print number of characters

###		Eq: Help me! I'm Stuck in a Computer!