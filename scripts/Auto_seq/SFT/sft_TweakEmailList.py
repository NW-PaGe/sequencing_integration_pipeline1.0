# -*- coding: utf-8 -*-
"""
Created on Wed Dec  8 11:20:32 2021

@author: PJG1303
"""
#%% Import Libraries

import os
import pickle
import keyring

#%% Ensure we are working in the correct directory
#### For easy user-interacting, this part allows a user to run the script from any location...
#### .... AS LONG AS the WAgisaid_objects.p file exists in the same directory/folder as this file.

# Define sequencing filepath location
sequencing_path = keyring.get_password('sequencing', 'fpath')

# Re-define our objects variable using the version in the newly defined working directory
objects = pickle.load( open( f'{sequencing_path}/Data_Objects/SFT/sft_objects.p', "rb" ) )

#%% Redefine the email list

# Define our email list
email_list = objects['email_recipient_list']

# Redefine email list variable
redefined_email_list = [
                        'your.name@your.email.address'
                        ]

# Replace our objects value using newly redefined email list
objects['email_recipient_list'] = redefined_email_list

#%% Save our new list to the pickle file, overwriting previous version

# Save
pickle.dump( objects, open( f'{sequencing_path}/Data_Objects/SFT/sft_objects.p', "wb" ) )
