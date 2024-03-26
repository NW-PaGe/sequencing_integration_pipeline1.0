# -*- coding: utf-8 -*-
"""
Created on Tue Aug 31 18:13:25 2021

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

import win32com.client as win32

###############################################################################################################
# Define all functions
###############################################################################################################

# Deleting File/Folder

def delete_element_by_id(driver, element_id):
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

# Downloading File

def download_element_by_id(driver, element_id):
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

# Waiting for download

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

# Moving a file

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
    shutil.move(full_source_path, full_destination_path)

    # Print output
    print('Moved File:  {}'.format(str(filename)))

# Copying a file to archive with unique name

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
    current_time = str(datetime.datetime.now().time()).replace(':', '')[:6]
    current_date = today_date
    current_datetime = current_date + '_' + current_time + '_'

    # Specify the full destination output with prefix
    full_destination_path = destination_directory + current_datetime + filename

    # Specify archive file name
    output_file = current_datetime + filename

    # Copy the file!
    shutil.copyfile(full_source_path, full_destination_path)

    # print output
    print('Copy File to Archive:  {}'.format(str(output_file)))

    return output_file

# Log the action

def log_action(date, filename, folder_dest, path_dest, archive_filename, archive_full_filepath):
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

    # Read in the .csv log file
    df = pd.read_csv('//Sequence Data and Reporting/Submissions/sft_automated_pull_log.csv')

    # Read in the columns
    df_columns = df.columns

    # Get a list of the entries we will put into log
    newrow = [date, filename, folder_dest, path_dest, archive_filename, archive_full_filepath]

    # Convert to row-like pandas dataframe
    pd_newrow = pd.DataFrame(newrow).transpose()

    # Name the columns of the newly created dataframe using existing
    pd_newrow.columns = df_columns

    # Append the new row to the dataframe
    df = df.append(pd_newrow, ignore_index=True)

    # Overwrite to the newly appended log file
    df.to_csv('//Sequence Data and Reporting/Submissions/sft_automated_pull_log.csv', index=False)

# Send the email

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


###############################################################################################################
# Pull all files from the sft
###############################################################################################################

####################################################################################
# Define all variables of interest to be used later on
####################################################################################

# Read in required data_objects and define objects.
py_creds = pickle.load( open( f'C:/Users/{os.getlogin()}/Projects/Sequencing/Data_Objects/py_creds.p', "rb" ) )

# Reload the objects file from the right location
objects = pickle.load( open( '//Sequence Data and Reporting/Data_Objects/SFT/sft_objects.p', "rb" ) )

# Load in our expected directories crosswalk -> links SFT dirs to net drive locs
expected_dirs = pd.read_csv('//Sequence Data and Reporting/Data_Objects/SFT/sft_expected_directories.csv')

# Define credentials to sft website
username = py_creds['sft_user']
password = py_creds['sft_pw']

# Define todays date
todays_date = str(datetime.datetime.now().date()).replace('-','')

####################################################################################
# Get selenium up and running
####################################################################################

# Define URL path to website we want.
base_page = 'https://sft.wa.gov/'

# Initiate Chrome
driver = webdriver.Chrome()

# Allow for right-clicking
action = ActionChains(driver)

# Go to website
driver.get(base_page)

time.sleep(15)

####################################################################################
# Login to the SFT website
####################################################################################


# Find username field and enter username
username_field = driver.find_element_by_name('user')
username_field.send_keys(username)

# Find password field and enter password
password_field = driver.find_element_by_name('password')
password_field.send_keys(password)

# Login and wait
driver.find_element_by_id('loginButon').click()
time.sleep(15)

####################################################################################
# Iteritively open every single directory
####################################################################################

# initialize the number of unopned directories to some non-zero value
num_unopened_directories = 1

# open anything unopened until everything is opened
while num_unopened_directories > 0:

    # Get a snapshot of all open directories
    DirectoryTree_RawText_CurrentView = driver.find_element_by_id('allFiles_Tree::').get_attribute('outerHTML')

    # Find all the ids of the directories
    ids = np.array(re.findall('id="(allFiles_Tree::[^"]+)"',DirectoryTree_RawText_CurrentView))

    # Find out whether or not they have already been clicked (aria-expanded="true")
    is_expanded = np.char.lower(np.array(re.findall('aria-expanded="([^"]+)"',DirectoryTree_RawText_CurrentView)[1:]))

    # Find out what isn't expanded.  Get all id's that meet this criteria.
    condition_needs_expanding = (is_expanded=='false')
    remaining_ids_to_click = ids[condition_needs_expanding]

    # For every element that is still open...
    for j in np.arange(0,len(remaining_ids_to_click)):

        # get the id to click
        cur_id_toclick = remaining_ids_to_click[j]

        # click on that id (of the folder) to open it
        driver.find_element_by_id(cur_id_toclick).click()

        # wait a sec
        time.sleep(1)

    # Get another view of the open/unopened directories
    DirectoryTree_RawText_PostView = driver.find_element_by_id('allFiles_Tree::').get_attribute('outerHTML')

    # find out using regular expressions if anything is still unopen
    num_unopened_directories = len(re.findall('aria-expanded="(false)"',DirectoryTree_RawText_PostView))

    # wait 2 seconds
    time.sleep(2)

####################################################################################
# Define a key we will use to guide the downloads / map folders to the net drive.
# NECISSARY STEP FOR COMPLEX FILE STRUCTURES
####################################################################################

# Get a snapshot of all open directories
DirectoryTree_RawText_CurrentView = driver.find_element_by_id('allFiles_Tree::').get_attribute('outerHTML')

# Find all the ids of the directories we could navigate to
ids = pd.Series(re.findall('id="(allFiles_Tree::[^"]+)"',DirectoryTree_RawText_CurrentView))

# Count the number of colans in each id.  Later used to sort on
NumColans_inIDstring = ids.str.count(':')

# Get the base directory
dir1 = ids.str.split('::',expand=True)[1].str.split(':',expand=True)[0]

# Get the second directory
dir2 = ids.str.split('::',expand=True)[1].str.split(':',expand=True)[1]

# Get a nice pandas dataframe containing all relevant information to folder paths
pandas_navigation_key_unfiltered = pd.DataFrame(np.array([ids,NumColans_inIDstring,dir1,dir2]).transpose(),
                                     columns=['id','OrderByMe','dir1','dir2'])

# Filter out sentinal labs that we don't want to click through
pandas_navigation_key_filtered = pandas_navigation_key_unfiltered.query('(dir1 != "SENTINEL_LABS")|(dir2 == "NW_Genomics")')

# FINALLY, merge with net drive mapping information.
pandas_naviagtion_key_final = pandas_navigation_key_filtered.merge(expected_dirs, left_on='dir1', right_on='DirName', how='left')\
                                                   .drop('DirName',axis=1)\
                                                   .drop('dir2',axis=1)\
                                                   .dropna(subset=['net_Drive_Mapping'])

# Order & reset index
pandas_naviagtion_key_final.sort_values(['dir1','OrderByMe'], ascending=[True, False], inplace=True)
pandas_naviagtion_key_final.reset_index(inplace=True,drop=True)


####################################################################################
# Walk through each file downloading, moving, archiving, logging, and deleting files (in that order)
####################################################################################

# for each folder/subfolder we will click into...
for i in np.arange(0,len(pandas_naviagtion_key_final)):

    # Define the net drive location we will map to, folder we are looking at.  Contained in the key we just made
    path_to_destination_dir = pandas_naviagtion_key_final.loc[i,'net_Drive_Mapping']
    destination_name = pandas_naviagtion_key_final.loc[i,'dir1']

    # Get information on the web element location we will click on
    id_of_folder_to_click = pandas_naviagtion_key_final.loc[i,'id']
    xpath_folder_to_click = '//*[@id="'+str(id_of_folder_to_click)+'"]/a/span'

    # Click on the relevant folder
    ##### First click by id.  Exposes the underlying /a/span/ element that we actually want.
    ##### Then click on the element that shows us the folder contents
    driver.find_element_by_id(id_of_folder_to_click).click()
    time.sleep(1)
    driver.find_element_by_xpath(xpath_folder_to_click).click()
    time.sleep(1)

    # Get the HTML information for the filelist
    filelist = driver.find_element_by_id('filelist-grid').get_attribute('outerHTML')
    time.sleep(1)

    # Use regex to get list of all web element ID's of the files/folders
    content_ids = re.findall('div index="\d{1,3}".*?id="([^"]+)"', filelist)

    # IF the folder contains anything (has 1+ id to work with)
    if len(content_ids) > 0:

        # Convert to pandas series to make easy to work with
        content_ids = pd.Series(content_ids)

        # Directories begin with a ':' character.  Define condition that exposes these.
        condition_isDirectory = (content_ids.str[0] == ':')

        # Define the directories as a list
        directory_ids = list(content_ids[condition_isDirectory])

        # Define the files as a list.  Note that the '~' character represents a NOT logical expression.
        file_ids = list(content_ids[~condition_isDirectory])


        # For all of the files...
        for j in np.arange(0,len(file_ids)):

            # Grab the iteration's file ID
            file_name = file_ids[j]

            # Wait
            time.sleep(2)

            # 1. Download a file
            download_element_by_id(driver,file_name)
            wait_for_download(file_name, f'C:/Users/{os.getlogin()}/Downloads/')

            # 2. Copy to archive
            copy_filename = copy_file_to_archive(file_name, f'C:/Users/{os.getlogin()}/Downloads/', '//Sequence Data and Reporting/Submissions/ARCHIVE_copies/', todays_date)
            wait_for_download(copy_filename, '//Sequence Data and Reporting/Submissions/ARCHIVE_copies/')

            # 3.  Move to folder
            move_file( file_name, f'C:/Users/{os.getlogin()}/Downloads/', path_to_destination_dir )
            wait_for_download(file_name, path_to_destination_dir)

            # 4.  Delete file
            delete_element_by_id(driver,file_name)

            # 5.  Log to archive
            log_action( todays_date, file_name, destination_name, path_to_destination_dir, copy_filename, '//Sequence Data and Reporting/Submissions/ARCHIVE_copies/'+copy_filename)
            
            print('\n\n')
            
        # For all the directories...
        for k in np.arange(0,len(directory_ids)):
            
            # Get directory web element id
            dir_name = directory_ids[k]
            
            # Delete directory
            delete_element_by_id(driver,dir_name)
            
            print('\n\n')
            
        # Rest between iterations    
        time.sleep(1)

####################################################################################
# Close out
####################################################################################

driver.close()

####################################################################################
# Log in our objects file that the last completed date was today
####################################################################################

objects = pickle.load( open( '//Sequence Data and Reporting/Data_Objects/SFT/sft_objects.p', "rb" ) )
objects['last_completed_date'] = str(datetime.datetime.now().date())
pickle.dump( objects, open( '//Sequence Data and Reporting/Data_Objects/SFT/sft_objects.p', "wb" ) )

###############################################################################################################
# Send email
###############################################################################################################

# Define todays date
todays_date_formatted = str(datetime.datetime.now().date())
todays_date_unformatted = todays_date_formatted.replace('-', '')

# Define path to log file
path_to_log = '//Sequence Data and Reporting/Submissions/sft_automated_pull_log.csv'

####################################################################################
#  Basic error handling to make sure that the main script ran successfully
####################################################################################

# If the script didn't run correctly, this will error out
if (objects['last_completed_date'] == todays_date_formatted):
    print('Main script ran succesfully.  Proceeding as normal.')

else:
    raise ValueError('Morning did not run successfully')

####################################################################################
# Read in / analyze today's log file for new downloads
####################################################################################

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
    todays_downloads['TimeOfDownload'] = todays_downloads['TimeOfDownload'].str[:2] + ':' + todays_downloads[
                                                                                                'TimeOfDownload'].str[
                                                                                            -2:]

    # order our output
    todays_downloads.sort_values('Folder', inplace=True)

    # Select columns we want to use and set the date as the index
    todays_downloads = todays_downloads[['Date', 'Folder', 'File', 'TimeOfDownload']]
    todays_downloads.reset_index(drop=True, inplace=True)

    # Convert to html for email.
    html_table_foremail = todays_downloads.to_html(classes='table table-striped')

####################################################################################
# Look for new lab submitters
####################################################################################

# Find out what was downloaded today vs. what was downloaded before
todays_downloads = logfile.copy().query('Date == {0}'.format(todays_date_unformatted))
previous_downloads = logfile.copy().query('Date < {0}'.format(todays_date_unformatted))

# Using set differences, find out any first-submission-drops based on our log file.
new_submitters = list(
    set(todays_downloads['DestinationName'].tolist()) - set(previous_downloads['DestinationName'].tolist()))

# Initialize empty list, will go into email as empty if no new submission folders located.
html_new_submitters = ''

# Looping through each new submitter, format the correct header2 to potentially use in the email.
for i in np.arange(0, len(new_submitters)):
    html_new_submitters += '<h2 style="text-align: left;"><span style="color: #DE3163;">Please review first submission:  {}</span></h2>'.format(
        new_submitters[i])

####################################################################################
# Send email
####################################################################################

# Define subject and recipients
email_subject = 'Sequencing SFT Pull: Daily Report'
email_recipients = ['']

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
     '''.format(header2=html_new_submitters, date_today=todays_date_formatted, table=html_table_foremail)

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
msg['From'] = 'your.name@your.email.address'
msg['To'] = ", ".join(email_recipients)

# Send the message via our own SMTP server.
s = smtplib.SMTP('your.mail.server')
s.send_message(msg)
s.quit()
