# -*- coding: utf-8 -*-
"""
Created on Tue Aug 31 17:58:35 2021

@author: PJG1303
"""
###############################################################################################################
###############################################################################################################

# For selenium, a web-navigation tool
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver import ActionChains

# Generic packages
import time
import os
import shutil
import numpy as np
import pandas as pd
import regex as re
import datetime
import pickle
import keyring

# Packages related to email sending
import win32com.client as win32

###############################################################################################################
###############################################################################################################

####################################################################################
# Deleting File/Folder
####################################################################################

def delete_element_by_id(driver,element_id):
    
    '''
    (Python)

    SPECIFIC TO SFT WEBSITE.  MAY NEED ADJUSTMENT IF SITE CHANGES STRUCTURE
    
    Deletes an element based on the web element's id.
    Used for deleting file after download/transfer

    --------------------------------------------------------------------------------------
    
    Keyword arguments:
    driver -- selenium webdriver 
    element_id -- the web element id.  Found in html structure.

    --------------------------------------------------------------------------------------
    
    Example:
    html_element_id = 'somefile' # typically found using CTRL+SHIFT+C (web inspection mode) and manually finding ID
    delete_element_by_id(html_element_id)
    
    --------------------------------------------------------------------------------------

    Dependencies:
    import time
    from selenium import webdriver
    from selenium.webdriver import ActionChains
    '''

    # Find the proper web element
    element = driver.find_element_by_id(element_id)
    time.sleep(1)
    
    # Allow for right-clicking
    action = ActionChains(driver)
    
    # Click on file in question
    action.context_click(element).perform()
    time.sleep(1)
    
    # Delete file
    rightclick_delete = driver.find_element_by_id('allFiles_context::delete')
    rightclick_delete.click()
    time.sleep(1)

    # Confirm deletion
    confirm_delete = driver.find_element_by_id('delete_actions::delete')
    confirm_delete.click()
    time.sleep(1)
    
    print('Deletion Complete:  {}'.format(str(element_id)))
       
###############################################################################################################
###############################################################################################################
    
####################################################################################
# Downloading File
####################################################################################

def download_element_by_id(driver,element_id):
    
    '''
    (Python)

    SPECIFIC TO SFT WEBSITE.  MAY NEED ADJUSTMENT IF SITE CHANGES STRUCTURE
    
    Downloads an element based on the web element's id.
    Used for downloading relevant files from SFT

    --------------------------------------------------------------------------------------
    
    Keyword arguments:
    driver -- selenium webdriver 
    element_id -- the web element id.  Found in html structure.

    --------------------------------------------------------------------------------------
    
    Example:
    html_element_id = 'somefile' # typically found using CTRL+SHIFT+C (web inspection mode) and manually finding ID
    download_element_by_id(html_element_id)
    
    --------------------------------------------------------------------------------------

    Dependencies:
    import time
    from selenium import webdriver
    from selenium.webdriver import ActionChains
    
    # RELIES ON the webdriver defined under the variable "driver"
    '''
    # Find the proper web element.  First sleep allowing files to finish rendering on page. 
    time.sleep(5)
    element = driver.find_element_by_id(element_id)
    time.sleep(3)
    
    # Allow for right-clicking
    action = ActionChains(driver)
    
    # Click on file in question
    action.context_click(element).perform()
    time.sleep(1)
    
    # Delete file
    rightclick_download = driver.find_element_by_id('allFiles_context::download')
    rightclick_download.click()
    time.sleep(5)
    
    print('Download Initiated:  {}'.format(str(element_id)))   
    
###############################################################################################################
###############################################################################################################

####################################################################################
# Waiting for download
####################################################################################

def wait_for_download(filename, download_directory):

    '''
    (Python)

    Waits for a file to be downloaded

    --------------------------------------------------------------------------------------
    Keyword arguments:
    filename -- Name of the file that was downloaded
    download_directory -- Full path to wherever things get downloaded to

    --------------------------------------------------------------------------------------

    Example:
    file = 'monkey_picture.png'
    dir_downloads = 'C:/Users/Downloads/'

    wait_for_download( file, dir_downloads )
    --------------------------------------------------------------------------------------

    Dependencies:
    import os
    import time
    '''

    # Convert filename to full file path with forward slashes.
    full_downloads_path_forwardslash = download_directory + filename
    
    # Convert filename to full file path with last location specified as "...folder\file.extension"
    full_downloads_path_backslash = download_directory[:-1] + '\\' + filename
    
    # Initialize conditions before while loop
    conditions_met = 0
    iteration = 1
    
    # Initiate while loop.  Waits for:
    #### 1.  the file to be finished downloading -> both full file paths recognized via os.path.exists() function
    #### OR
    #### 2.  timeout occurs -> operation runs for 5 mins (300 seconds) without any action. 
    
    while (conditions_met != 2) & (iteration < 300):
        
        # Check to see if paths exist.  Returns boolean TRUE or FALSE
        cond1 = os.path.exists(full_downloads_path_forwardslash)
        cond2 = os.path.exists(full_downloads_path_backslash)
        
        # See how many conditions were met. (adding booleans produces INT value)
        conditions_met = cond1 + cond2
        
        # Update iteration count
        iteration += 1
        
        # Rest
        time.sleep(1)
        
    # If timeout occured, you will see an iteration of 300 or greater.  Return output.
    if iteration > 299:
        raise TimeoutError('File never downloaded.  Timed out after 5 mins')
        
    # Otherwise print message indicating completion.    
    else:
        print('Action Complete:  {}'.format(str(filename)))
            
###############################################################################################################
###############################################################################################################

####################################################################################
# Moving a file
####################################################################################

def move_file(filename, source_directory, destination_directory):

    '''
    (Python)

    Moves a file from one place to another, making sure that 

    --------------------------------------------------------------------------------------
    Keyword arguments:
    filename -- Name of the file that was downloaded
    source_directory -- Full path to source.  Ends in /
    destination_directory -- Full path to where you want the file to go.  End with /

    --------------------------------------------------------------------------------------

    Example:
    file = 'monkey_picture.png'
    dir_downloads = 'C:/Users/Downloads/'
    dir_dest = 'C:/Some/Location/on/Yourdrive/'

    move_file( file, dir_downloads, dir_dest )
    --------------------------------------------------------------------------------------

    Dependencies:
    import shutil
    import 
    import time
    '''

    # Specify full path of the source
    full_source_path = source_directory + filename

    # Specify full path of the destination
    full_destination_path = destination_directory + filename
    
    # Initialize copy #
    copy_num = 1
    
    # If the file already exists, follow windows formatting for adding version extension to name
    #### ex: file (1).txt or file (2).txt
    while os.path.exists(full_destination_path):
        
        version_extension = ' ({})'.format(str(copy_num))
        filename_split = str(filename).split('.')
        new_filename = str(''.join(filename_split[:-1])) + version_extension + str(filename_split[-1])
        full_destination_path = destination_directory + new_filename
        copy_num += 1
        time.sleep(2)

        
    # Move the file
    shutil.move( full_source_path , full_destination_path)
    
    # Print output
    print('Moved File:  {}'.format(str(filename)))
    
###############################################################################################################
###############################################################################################################

####################################################################################
# Copying a file to archive with unique name
####################################################################################

def copy_file_to_archive(filename, source_directory, destination_directory, today_date):

    '''
    (Python)

    Copies file to an archive with datetime prefix for emergency backup purposes

    --------------------------------------------------------------------------------------
    
    Keyword arguments:
    filename -- Name of the file that was downloaded
    source_directory -- Full path to source.  Ends in /
    destination_directory -- Full path to where you want the file to go.  End with /
    today_date -- todays date as string

    --------------------------------------------------------------------------------------

    Example:
    file = 'monkey_picture.png'
    dir_downloads = 'C:/Users/Downloads/'
    dir_dest = 'C:/Some/Location/on/Yourdrive/'

    copy_file_to_archive( file, dir_downloads, dir_dest, '20210830' )
    --------------------------------------------------------------------------------------

    Dependencies:
    import pandas as pd
    import numpy as np
    import os
    import shutil
    import time
    '''

    # Get full path of the source dir
    full_source_path = source_directory + filename

    # Eventually file with get saved with prefix "%Y%m%d_%h%m_"
    current_time = str(datetime.datetime.now().time()).replace(':','')[:6]
    current_date = today_date
    current_datetime = current_date + '_' + current_time + '_'
    
    # Specify the full destination output with prefix
    full_destination_path = destination_directory + current_datetime + filename
    
    # Specify archive file name
    output_file = current_datetime + filename
    
    # Copy the file!
    shutil.copyfile( full_source_path , full_destination_path)
    
    # print output
    print('Copy File to Archive:  {}'.format(str(output_file)))
    
    return output_file
    
###############################################################################################################
###############################################################################################################

####################################################################################
# Log the action
####################################################################################

def log_action( date, filename, folder_dest, path_dest, archive_filename, archive_full_filepath):
    
    '''
    (Python)

    Creates new row in log-file for SFT pull script

    --------------------------------------------------------------------------------------
    
    Keyword arguments:
    date -- current date formatted as YYYYmmdd (ex: 20210130)
    filename -- name of file that was downloaded (ex: metadata_someplace.csv)
    folder_dest -- folder name that the file was saved to
    path_dest -- full path that the file was sent to
    archive_filename -- file name saved to the archive location with timestamp prefix
    archive_full_filepath -- full path that the file was saved to 

    --------------------------------------------------------------------------------------

    Example:
    date_now = '20210831'
    file = 'file.txt'
    destination = 'labsubmitter'
    path_dest = 'C:/path/to/labsubmitter/'
    file_archival = '20210831_1401_file.txt'
    path_archival = 'C:/path/to/archival/location'
    
    log_action( date_now, file, destination, path_dest, file_archival, path_archival )
    --------------------------------------------------------------------------------------

    Dependencies:
    import pandas as pd
    import pickle
    '''
    
    # Define sequencing filepath location
    sequencing_path = keyring.get_password('sequencing', 'fpath')    
    
    # Read in the .csv log file
    df = pd.read_csv( f'{sequencing_path}/Submissions/sft_automated_pull_log.csv' )

    # Read in the columns
    df_columns = df.columns

    # Get a list of the entries we will put into log
    newrow = [date, filename, folder_dest, path_dest, archive_filename, archive_full_filepath]

    # Convert to row-like pandas dataframe
    pd_newrow = pd.DataFrame(newrow).transpose()

    # Name the columns of the newly created dataframe using existing
    pd_newrow.columns = df_columns

    # Append the new row to the dataframe
    df = df.append(pd_newrow,ignore_index=True)
    
    # Overwrite to the newly appended log file
    df.to_csv( f'{sequencing_path}/Submissions/sft_automated_pull_log.csv' , index=False )

###############################################################################################################
###############################################################################################################
    
def send_email(subject, recipients, html_content):
    
    '''
    (Python)

    Sends an email using outlook

    --------------------------------------------------------------------------------------
    
    Keyword arguments:
    subject:  str, required. Will be displayed in subject line.
    recipients: list, required.  List of strings detailing email addresses to send email to.
    html_content:  str, required.  String of HTML content that makes up body of email.

    --------------------------------------------------------------------------------------

    Example:
    subject = 'test email'
    recipient_list = ['youremailhere@gmail.com']
    content = '<p>Hello World!.</p>'
    
    send_email( subject , recipient_list , content )

    # Check your inbox!!
    
    --------------------------------------------------------------------------------------

    Dependencies:
    import win32com.client as win32
    '''
    
    # Connect to outlook
    outlook = win32.Dispatch('outlook.application')
    
    # Create the item
    mail = outlook.CreateItem(0)
    
    # Define the recipient(s)
    mail.To = ';'.join(recipients)

    # Define the subject
    mail.Subject = subject
        
    # Define the body
    mail.HTMLBody = html_content
    
    # Send email
    mail.Send()
    
    # Print success
    print('Email Successfully Sent!')
    