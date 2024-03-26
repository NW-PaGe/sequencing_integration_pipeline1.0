# -*- coding: utf-8 -*-
"""
Created on Mon Sep 13 15:49:08 2021

@author: PJG1303
"""

###############################################################################################################
# Load all libraries
###############################################################################################################

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver import ActionChains

import time
import os
import shutil
import numpy as np
import pandas as pd
import regex as re
import datetime
import pickle
import keyring
import smtplib
from email.message import EmailMessage
from email.mime.text import MIMEText

# Import customized functions .py file from the working directory we navigated to
from sft_functions import *

###############################################################################################################
# Navigate to the proper working directory
#### For easy user-interacting, this part allows a user to run the script from any location...
#### .... AS LONG AS the sft_objects.p file exists in the same directory/folder as this file.
###############################################################################################################

# Define sequencing filepath location
sequencing_path = keyring.get_password('sequencing', 'fpath')

# Reload pre-defined objects for this script.  Mostly xpaths.
objects = pickle.load( open( f'{sequencing_path}/Data_Objects/SFT/sft_objects.p', "rb" ) )

###############################################################################################################
# Define all variables of interest to be used later on
###############################################################################################################

# Define todays date
todays_date_formatted = str(datetime.datetime.now().date())
todays_date_unformatted = todays_date_formatted.replace('-','')

# Define path to log file
path_to_log = f'{sequencing_path}/Submissions/sft_automated_pull_log.csv'

###############################################################################################################
#  Basic error handling to make sure that the main script ran successfully
###############################################################################################################

# If the script didn't run correctly, this will error out
if (objects['last_completed_date'] == todays_date_formatted):
    print('Main script ran succesfully.  Proceeding as normal.')
    
else:
    raise ValueError('Morning did not run successfully')

###############################################################################################################
# Read in / analyze today's log file for new downloads
###############################################################################################################

# Read in the logfile
logfile = pd.read_csv(path_to_log)

# Query our dataframe to see what we downloaded today.  
### NOTE: Used copy() to avoid warning.  Not essential, but cleans up output.
todays_downloads = logfile.copy().query('Date == {0}'.format(todays_date_unformatted))

# Count anything we downloaded
files_downloaded = len(todays_downloads)


if files_downloaded > 0:
    
    # Redefine the Logfile's column to be more digestible
    todays_downloads['Folder'] = todays_downloads['DestinationName']
    
    # Similarly redefine Logfile column "FileName" to be simpler
    todays_downloads['File'] = todays_downloads['FileName']
    
    # Parse out the time that it was downloaded
    todays_downloads['TimeOfDownload'] = todays_downloads['ArchiveFileName'].str[9:13]
    todays_downloads['TimeOfDownload'] = todays_downloads['TimeOfDownload'].str[:2] + ':' +  todays_downloads['TimeOfDownload'].str[-2:]
    
    # order our output
    todays_downloads.sort_values('Folder',inplace=True)
    
    # Select columns we want to use and set the date as the index
    todays_downloads = todays_downloads[['Date','Folder','File','TimeOfDownload']]
    todays_downloads.reset_index(drop=True, inplace=True)
    
    # Convert to html for email.
    html_table_foremail = todays_downloads.to_html(classes='table table-striped')
    
    
###############################################################################################################
# Look for new lab submitters
###############################################################################################################

# Find out what was downloaded today vs. what was downloaded before    
todays_downloads = logfile.copy().query('Date == {0}'.format(todays_date_unformatted))
previous_downloads = logfile.copy().query('Date < {0}'.format(todays_date_unformatted))

# Using set differences, find out any first-submission-drops based on our log file.
new_submitters = list( set(todays_downloads['DestinationName'].tolist()) - set(previous_downloads['DestinationName'].tolist()))

# Initialize empty list, will go into email as empty if no new submission folders located.
html_new_submitters = ''

# Looping through each new submitter, format the correct header2 to potentially use in the email.
for i in np.arange(0,len(new_submitters)):
    html_new_submitters += '<h2 style="text-align: left;"><span style="color: #DE3163;">Please review first submission:  {}</span></h2>'.format(new_submitters[i])

###############################################################################################################
# Send email.  Seperated if-statement for clarity purposes
###############################################################################################################

# Define subject and recipients
email_subject = 'Sequencing SFT Pull: Daily Report Automated Email'
email_recipients = objects['email_recipient_list']

# Define the email body.  Old header color - #3366ff
##########################

if files_downloaded > 0:
    
    email_body = '''
    <h1 style="text-align: left;"><span style="color: #6495ED;">SFT Pull Report: {date_today}</span></h1>
    {header2}
    <p>Hello,</p>
    <p>See the table below for all files pulled from the SFT today.  Times are represented in a 24-hour format.</p>
    {table}
    <p>Have a great day!</p>
    <p>-DIQA Epis</p>
     '''.format(header2 = html_new_submitters, date_today=todays_date_formatted , table=html_table_foremail)
     
else:            
    email_body = '''
    <h1 style="text-align: center;"><span style="color: #3366ff;">SFT Pull Report: {date_today}</span></h1>
    <p>Hello,</p>
    <p>There were no files found in the SFT today.</p>
    <p>Have a great day!</p>
    <p>-DIQA Epis</p>
     '''.format(date_today=todays_date_formatted)
        
# SEND THE EMAIL!!!

msg = MIMEText(email_body, 'html')
msg['Subject'] = email_subject
msg['From'] = ''
msg['To'] = ", ".join(email_recipients)

# Send the message via our own SMTP server.
s = smtplib.SMTP('')
s.send_message(msg)
s.quit()
