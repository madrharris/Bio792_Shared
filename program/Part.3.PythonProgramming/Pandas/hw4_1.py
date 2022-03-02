#!/usr/local/bin/python3

## import packages & datasets
import os
import numpy as np
import pandas as pd


## make the working directory
PATH = '/Users/mine/Desktop/Bio792_Shared/program/Part.3.PythonProgramming/Pandas'

assert os.path.exists(os.path.join(PATH,'states_covid.csv')), 'states_covid.csv does not exist' 
assert os.path.exists(os.path.join(PATH,'Bloom_etal_2018_Reduced_Dataset.csv')), 'Bloom_etal_2018_Reduced_Dataset.csv does not exist'


###		read states_covid with date as a "date" dtype, with only columns: hospitalization, ICU, and ventilators
##			dtype date --> data type for data or columns
