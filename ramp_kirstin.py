#!/usr/bin/env python
# coding: utf-8

# # Parsing qualtrics metadata to create a data dictionary
# **Author:** K L Purves
# **Date:** 30 April 2020

# ### Set up
#
# #### #autosave

# Note that I enabled the nbconvert hide code extension, added a html export template that respects this extension metadata, and updated my config file (path retreieved with the following command:  jupyter notebook --generate-config) to read this template when converting to html and python with code found here: https://gist.github.com/jbwhit/881bdeeaae3e4128947c
#
# This allowed me to use api token calls and have this sensitive information safely hidden / unretrievable in autosaved files for github sync.

# #### #install and import packages
'''import and install packages'''
# make sure that relevant packages are installed Run this in your terminal.
# toggle cell is reliant of toggle_cell in notebook directory

#pip install requests os json pprint imp

import os
import requests
#from toggle_cell import toggle_code as hide_solution # use this if you want to toggle whether code shows or not.
import json
import pprint
from traitlets.config import Config
import nbformat as nbf
from nbconvert.exporters import HTMLExporter
import pandas as pd
import numpy as np
import gspread
import google.auth
from google.oauth2 import service_account
import re
from multi_rake import Rake
import string

c = Config()

# #### #Setup tokens and path for api etc.
#
# DO NOT SAVE THE content of these variables in any version uploaded to github

# %load "/Users/kirstin/Dropbox/SGDP/RAMP/data/RawStorage/fileinfo.py"

# RAMP file info

api_token = "kqBI3SOA54Ik6r1QFnYhq6W0oYDPyhxp9q5wwPtX"
center = "kcliop"
survey_id = "SV_0DrSSOISyMOqN5r"

survey_path = "/Users/kirstin/Dropbox/SGDP/RAMP/data/RawStorage/RAMP_Baseline.csv"

# google credentials to link with google sheets.

googjson = "/Users/kirstin/Dropbox/SGDP/RAMP/data/GoogleApi/creds.json"

# data wave
wave = 1
'''fetch the meta data for questionnaire from qualtrics'''
# center = "eu"
# survey_id = "SV_4PyAHbAxpdbyacl"

baseUrl = "https://{0}.qualtrics.com/API/v3/surveys/{1}".format(
    center, survey_id)

headers = {
    "x-api-token": api_token,
}

metadata = requests.get(baseUrl, headers=headers)

pprint.pprint(metadata.text)

metadata.headers['content-type']

# #### #create a python nested dictionary from the json object

ramp_dict = metadata.json()

# #### #print this to screen using pretified print option to be navigate and explore

pprint.pprint(ramp_dict)

# ##### #Establish first level keys

for key, value in ramp_dict.items():
    print(key)

# ##### #Print values for the result key
# This will give us keys for the next dictionary nest

for value in ramp_dict['result']:
    print(value)

# From this, we can identify a number of useful meta dictionaries. Questions and exportColumnMap. Use these to create pandas dataframes that contain all of our variable data in dictionary format.

# ### #Create a data frame with question metadata
#
# This will involve un-nesting the dictionaries within this data frame by making keys tuples that correspond to multi-index values. then use the pandas DataFrame.from_dict function to compile.

# #### #Deconstruct dictionary objects for nested properties
#
# Question type and choices have more nests than other levels. Deconstruct these to append later. Extract:
#
# From question type:
#     * type (e.g. matrix, multiple choice)
#     * Selector (e.g. Single answer, multiple answer, multiple answer column, Likert)
#     * Sub selector (e.g. other ==. text)
#
#
# From validation:
#     * Does force response (True/False)
#     *  Does request response (True/False)
#     * Type(e.g. Valid number)
#     * settings(e.g. maxDecimals, maximum)
#
# From choices:
#
#

questionNames = [
    'qid', "questionType", "questionSelector", "questionSubSelector",
    "validationForce", "validationRequest", "validationType",
    "validationSettings"
]
questiondf = pd.DataFrame(columns=questionNames)

# empty dataframe for choices variable.

choicenames = [
    "qid", "choices", "recode", "description", "text", "subQuestion"
]

choicesdf = pd.DataFrame(columns=choicenames)

# ###### #Deconstruct deepest nests (question types and choices)
#
# When there are questions with multiple categories each with individual response options (i.e. subQuestions) we use the subquestion metadata to populate the choices columns.
#
# Where side by side matrices, make sure we have questions for each of the column responses with appropriate text.

## Deconstruct question related set and choice related sets into dataframes

for qid in ramp_dict['result']['questions'].keys():

    #Question and question validation deconstruct
    questionRow = {}

    # append question data for each question id
    questionRow['qid'] = qid

    questionRow["questionType"] = ramp_dict['result']['questions'][qid][
        'questionType']['type']
    questionRow["questionSelector"] = ramp_dict['result']['questions'][qid][
        'questionType']['selector']
    questionRow["questionSubSelector"] = ramp_dict['result']['questions'][qid][
        'questionType']['subSelector']

    #Validation deconstruct
    #collect dictionary keys for validation

    valkeys = ramp_dict['result']['questions'][qid]['validation'].keys()

    if 'doesForceResponse' in valkeys:
        questionRow["validationForce"] = ramp_dict['result']['questions'][qid][
            'validation']['doesForceResponse']
    else:
        questionRow["validationForce"] = float("NaN")

    if 'doesRequestResponse' in valkeys:
        questionRow["validationRequest"] = ramp_dict['result']['questions'][
            qid]['validation']['doesRequestResponse']
    else:
        questionRow["validationRequest"] = float("NaN")

    if 'type' in valkeys:
        questionRow["validationType"] = ramp_dict['result']['questions'][qid][
            'validation']['type']
    else:
        questionRow["validationType"] = float("NaN")

    if 'settings' in valkeys:
        questionRow["validationType"] = ramp_dict['result']['questions'][qid][
            'validation']['settings']
    else:
        questionRow["validationType"] = float("NaN")

    # append new row to the dataframe using dictionary reference to connect columns

    questiondf = questiondf.append(questionRow, ignore_index=True)

    #Choice deconstruct

    #if there are choices, collect dict keys and then fill in dataframe per row per choice item accordingly.
    # if there are sub questions, use this metadata:
    ## if there are subquestions AND

    # create a dictionary to hold the column value pairing per row.

    choicerow = {}

    ## SBS are the only data metadata eith columns, so this first section unravels these

    # if qid == "QID124931342":
    #     breakpoint()

    if 'subQuestions' in ramp_dict['result']['questions'][qid].keys():

        if 'columns' in ramp_dict['result']['questions'][qid].keys():

            for subchoice_no in ramp_dict['result']['questions'][qid][
                    'subQuestions'].keys():

                for column in ramp_dict['result']['questions'][qid][
                        'columns'].keys():

                    for choice_cols in ramp_dict['result']['questions'][qid][
                            'columns'][column]["choices"].keys():

                        choicekeys = ramp_dict['result']['questions'][qid][
                            'subQuestions'][subchoice_no].keys()

                        choicerow['qid'] = qid
                        choicerow['choices'] = choice_cols

                        choicerow['recode'] = ramp_dict['result']['questions'][
                            qid]['columns'][column]["choices"][choice_cols][
                                'recode']

                        choicerow['description'] = ramp_dict['result'][
                            'questions'][qid]['columns'][column]['choices'][
                                choice_cols]['description']

                        choicerow['text'] = ramp_dict['result']['questions'][
                            qid]['subQuestions'][subchoice_no]['description']

                        choicerow['subQuestion'] = column

                        # append dictionary to dataframe.

                        choicesdf = choicesdf.append(choicerow,
                                                     ignore_index=True)

        ## deal with unravelling matrix question types

        elif ramp_dict['result']['questions'][qid]['questionType'][
                'type'] == "Matrix":

            for subchoice_no in ramp_dict['result']['questions'][qid][
                    'subQuestions'].keys():

                for choice in ramp_dict['result']['questions'][qid][
                        'choices'].keys():

                    choicekeys = ramp_dict['result']['questions'][qid][
                        'subQuestions'][subchoice_no].keys()

                    choicerow['qid'] = qid
                    choicerow['choices'] = choice

                    choicerow['recode'] = ramp_dict['result']['questions'][
                        qid]['choices'][choice]['recode']

                    choicerow['description'] = ramp_dict['result'][
                        'questions'][qid]['choices'][choice]['choiceText']

                    choicerow['text'] = ramp_dict['result']['questions'][qid][
                        'subQuestions'][subchoice_no]['choiceText']

                    choicerow['subQuestion'] = subchoice_no

                    # append dictionary to dataframe.

                    choicesdf = choicesdf.append(choicerow, ignore_index=True)

        ## deal with unravelling Side By Side question types

        elif ramp_dict['result']['questions'][qid]['questionType'][
                'type'] == "SBS":

            for subchoice_no in ramp_dict['result']['questions'][qid][
                    'subQuestions'].keys():

                for choice in ramp_dict['result']['questions'][qid][
                        'choices'].keys():

                    choicekeys = ramp_dict['result']['questions'][qid][
                        'subQuestions'][subchoice_no].keys()

                    choicerow['qid'] = qid
                    choicerow['choices'] = choice

                    choicerow['recode'] = ramp_dict['result']['questions'][
                        qid]['choices'][choice]['recode']

                    choicerow['description'] = ramp_dict['result'][
                        'questions'][qid]['choices'][choice]['choiceText']

                    choicerow['text'] = ramp_dict['result']['questions'][qid][
                        'subQuestions'][subchoice_no]['choiceText']

                    choicerow['subQuestion'] = subchoice_no

                    # append dictionary to dataframe.

                    choicesdf = choicesdf.append(choicerow, ignore_index=True)

        else:

            for subchoice_no in ramp_dict['result']['questions'][qid][
                    'subQuestions'].keys():

                choicekeys = ramp_dict['result']['questions'][qid][
                    'subQuestions'][subchoice_no].keys()

                choicerow['qid'] = qid
                choicerow['choices'] = subchoice_no

                choicerow['recode'] = ramp_dict['result']['questions'][qid][
                    'subQuestions'][subchoice_no]['recode']

                choicerow['description'] = ramp_dict['result']['questions'][
                    qid]['subQuestions'][subchoice_no]['description']

                choicerow['text'] = ramp_dict['result']['questions'][qid][
                    'subQuestions'][subchoice_no]['choiceText']

                choicerow['subQuestion'] = "yes"

                # append dictionary to dataframe.

                choicesdf = choicesdf.append(choicerow, ignore_index=True)

    elif 'choices' in ramp_dict['result']['questions'][qid].keys():

        for choice_no in ramp_dict['result']['questions'][qid]['choices'].keys(
        ):

            choicekeys = ramp_dict['result']['questions'][qid]['choices'][
                choice_no].keys()

            choicerow['qid'] = qid
            choicerow['choices'] = choice_no
            choicerow['subQuestion'] = "no"

            if 'recode' in choicekeys:
                choicerow['recode'] = ramp_dict['result']['questions'][qid][
                    'choices'][choice_no]['recode']
            else:
                choicerow['recode'] = float("NaN")

            if 'description' in choicekeys:
                choicerow['description'] = ramp_dict['result']['questions'][
                    qid]['choices'][choice_no]['description']
            else:
                choicerow['description'] = "None"

            if 'choiceText' in choicekeys:
                choicerow['text'] = ramp_dict['result']['questions'][qid][
                    'questionText']
            else:
                choicerow['text'] = float("NaN")

            # append dictionary to dataframe.

            choicesdf = choicesdf.append(choicerow, ignore_index=True)

    else:
        choicerow['qid'] = qid
        choicerow['choices'] = float("NaN")
        choicerow['recode'] = float("NaN")
        choicerow['description'] = float("NaN")
        choicerow['text'] = float("NaN")

        # append dictionary to dataframe.

        choicesdf = choicesdf.append(choicerow, ignore_index=True)

choicesdf.loc[choicesdf.qid == "QID124931342"]

choicesdf.columns

# #### # Create remaining data dictionary components
#
# Create pandas dataframed with the remaining metadata to flesh out our data dictionary.
#
# I will do an easy extract of the questions variables to get question text, labels and question names. I will concatenate these with the type and choice data already acquired to make the full dictionary.
#
# This requires resetting the index and ensuring it is named qid for later merging.

# dataframe with question dictionary from pandas dataframe from dict function.

QuestionMD = pd.DataFrame.from_dict(ramp_dict['result']['questions'],
                                    orient='index')

# reset index to be a column and name it qid.

QuestionMD = QuestionMD.rename_axis('qid').reset_index()

# check your work
print('check data head', '\n', QuestionMD.head(), '\n\n\n')
print('check data column names', '\n', list(QuestionMD.columns))

# #### Adjust question text for SBS items, where you need question text for each column
#
# get the question text items to add to the main dataframe.

sbsNames = ['qid', "column", "questiontext"]
sbsdf = pd.DataFrame(columns=sbsNames)

sbs_text = {}

for qid in QuestionMD['qid']:

    for num in QuestionMD['questionType'].loc[QuestionMD['qid'] ==
                                              qid].to_dict().keys():

        if 'SBS' in QuestionMD['questionType'].loc[QuestionMD['qid'] ==
                                                   qid].to_dict()[num]['type']:

            for col in QuestionMD['columns'].loc[QuestionMD['qid'] ==
                                                 qid].to_dict()[num].keys():

                sbs_text['qid'] = qid
                sbs_text['col'] = col
                sbs_text['qtext'] = QuestionMD['columns'].loc[
                    QuestionMD['qid'] ==
                    qid].to_dict()[num][col]['questionText']

                sbsdf = sbsdf.append(sbs_text, ignore_index=True)

                print(sbs_text)

# ##### #drop questions
#
# From the question section, drop the type, validation and choices that we now cover. Also subquestion as this is deconstructed.
#
# Keep the choice text column for when there are side by side matrix options.

QuestionMD = QuestionMD.drop([
    'questionType', 'validation', 'choices', 'subQuestions', 'columns',
    'questionLabel'
],
                             axis=1)

print('check data column names', '\n', list(QuestionMD.columns))

# ### #Create a data frame with block metadata
#
# This will involve un-nesting the dictionaries within the block part of results metadata to access block names for each question id

# #### #initialise dataframe
#

blockNames = ['qid', "block"]
blockdf = pd.DataFrame(columns=blockNames)

## Deconstruct block into qid and description

for blockID in ramp_dict['result']['blocks'].keys():

    # Create dictionary to hold and append qid and description values.
    blockRow = {}

    for q in range(len(ramp_dict['result']['blocks'][blockID]['elements'])):

        if ramp_dict['result']['blocks'][blockID]['elements'][q][
                'type'] == 'Question':
            blockRow['qid'] = ramp_dict['result']['blocks'][blockID][
                'elements'][q]['questionId']
            blockRow['block'] = ramp_dict['result']['blocks'][blockID][
                'description']

        else:
            next

        blockdf = blockdf.append(blockRow, ignore_index=True)

blockdf.head(10)
ramp_dict['result'].keys()
# ## join all datasets together to create the data dictionary
#
# Perform full join so that we retain the multiple rows per qid where there are multiple choices.
#
# clean the data frame.
#
#

DataDictionary = pd.merge(blockdf, QuestionMD, on='qid')
DataDictionary = pd.merge(DataDictionary, choicesdf, on='qid')
DataDictionary = pd.DataFrame(pd.merge(DataDictionary, questiondf, on='qid'))

# Add a column to identify data wave

DataDictionary['Wave'] = wave

# ### RAMP / COPING only
#
# change wave to 0 for retrospective ratings.

DataDictionary['Wave'] = DataDictionary['questionText'].apply(
    lambda x: np.where(
        "before the pandemic" in x or "Before the pandemic" in x or
        "prior to the pandemic" in x, 0, wave))

# change the wave for health anxiety questions where QID occurs after the pandemic questions

## RAMP ONLY - NEED to do this for COPING identifying the right H.Anx point
qidlist = [
    "QID124935551", "QID124935552", "QID124935553", "QID124935554",
    "QID124935555", "QID124935556", "QID124935557", "QID124935559",
    "QID124935560", "QID124935561", "QID124935562", "QID124935563",
    "QID124935564", "QID124935565"
]

for row in range(DataDictionary.shape[0]):
    if DataDictionary['qid'][row] in qidlist:
        DataDictionary['Wave'][row] = 0

#
#
# ### #Clean the dictionary
#
# Drop descriptive blocks and unneccessary columns.

# #### if the question type is a SBS, adjust the questionText to reflect each column
#
# use the sbs_text dataframe created earlier for this step.

for row in range(DataDictionary.shape[0]):

    if DataDictionary['qid'][row] in list(sbsdf['qid']):
        qid = DataDictionary['qid'][row]
        col = DataDictionary['subQuestion'][row]
        newtext = str(sbsdf['qtext'].loc[(sbsdf['qid'] == qid)
                                         & (sbsdf['col'] == col)])
        newtext = re.sub(r'[0-9]+', '', newtext)
        newtext = re.sub(r'Name: qtext, dtype: object', '', newtext)
        newtext = newtext.strip()

        DataDictionary['questionText'][row] = newtext
    else:
        next

DataDictionary.head()

DataDictionary[DataDictionary['qid'] == "QID124964827"]

# ### #Create qid variable names that replicate data download format
#
# Use question type and sub question type information to create qualtrics replicated variable ids to link with the dataset.
#
# Rules:
#
# * any NON FORM text entry variable type ends in '_TEXT'
# * any item with a subquestion ends in '_choice number'
# * any text entry WITHIN a FORM ends in '_choice number'
# * any side by side data type ends in '#columnno_choiceno'
# * any slider question type ends in '_choice number'
#
#
# These rules should recreate downloaded data headers.

# rename qid function


def rename_qid(qid, qType, choice, col):

    if qType == 'SBS':
        newqid = "#".join((qid, col))
        newqid = "_".join((newqid, choice))

    if qType == 'sub':
        newqid = "_".join((qid, choice))

    if qType == 'text':
        newqid = "_".join((qid, "TEXT"))

    return newqid


# ### #Rename qid using function for each question type

for row in range(0, DataDictionary.shape[0]):

    # side by side nested questions

    if DataDictionary['questionType'][row] == "SBS":
        DataDictionary['qid'][row] = rename_qid(
            DataDictionary['qid'][row], "SBS", DataDictionary['choices'][row],
            DataDictionary['subQuestion'][row])

    # text (non-form) questions

    elif DataDictionary['questionType'][
            row] == "TE" and DataDictionary['questionSelector'][row] != "FORM":

        DataDictionary['qid'][row] = rename_qid(
            DataDictionary['qid'][row], "text", DataDictionary['choices'][row],
            DataDictionary['subQuestion'][row])

    # text FORM questions

    elif DataDictionary['questionType'][row] == "TE" and DataDictionary[
            'questionSelector'][row] == "FORM":

        DataDictionary['qid'][row] = rename_qid(
            DataDictionary['qid'][row], "sub", DataDictionary['choices'][row],
            DataDictionary['subQuestion'][row])

        # Matrix question types

    elif DataDictionary['questionType'][row] == "Matrix":

        DataDictionary['qid'][row] = rename_qid(
            DataDictionary['qid'][row], "sub",
            DataDictionary['subQuestion'][row],
            DataDictionary['subQuestion'][row])

    # multiple choice sub question types

    elif DataDictionary['subQuestion'][row] == "yes":

        breakpoint()
        DataDictionary['qid'][row] = rename_qid(
            DataDictionary['qid'][row], "sub", DataDictionary['choices'][row],
            DataDictionary['subQuestion'][row])

# ## # cleaning cells
#
# Clean the dictionary, dropping descriptive blocks, renaming columns and cells.

# ##### #truncate text to google sheet 50000 limit and strip html formatting
#
# This will truncate text cells for things like information sheet and consent.
# Strip html tags

# function from https://medium.com/@jorlugaqui/how-to-strip-html-tags-from-a-string-in-python-7cb81a2bbf44


def remove_html_tags(text):
    """Remove html tags from a string"""
    import re
    clean = re.compile('<.*?>')
    return re.sub(clean, '', text)


# truncate every cell to < 50000 characters
DataDictionary = pd.DataFrame(
    DataDictionary.apply(
        lambda x: x.astype(str).str.slice(start=0, stop=50000), axis=0))

## Remove the html tags from text descriptor fields

DataDictionary['questionText'] = pd.DataFrame(
    DataDictionary['questionText'].apply(lambda x: remove_html_tags(x)))

DataDictionary['description'] = pd.DataFrame(
    DataDictionary['description'].apply(lambda x: remove_html_tags(x)))

DataDictionary['text'] = pd.DataFrame(
    DataDictionary['text'].apply(lambda x: remove_html_tags(x)))

# ##### #Add useful names for question types and selectors
#
# use qualtrics reference points from here https://api.qualtrics.com/docs/working-with-surveys-using-the-conversational-apis
#
# I will break these down into whether they result in categorical, continuous or text type questions
'''"MC", "Multiple choice"
TE", "Text entry"
"DB", "Descriptive block"
"SBS", "Side by side",
"TB", "Text box",
"DD", "Drop-down",
"MC", "Multiple choice",
'''

## replace abbreviations in question type to indicate whether they are cat/contin / text etc

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "MC", "Categorical",
    DataDictionary['questionType'])

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "DB", "Descriptive block",
    DataDictionary['questionType'])

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "TE", "Text",
    DataDictionary['questionType'])

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "Slider", "Continuous",
    DataDictionary['questionType'])

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "Matrix", "Categorical",
    DataDictionary['questionType'])

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "TB", "Text",
    DataDictionary['questionType'])

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "DD", "Drop-down",
    DataDictionary['questionType'])

## replace abbreviations in question selector

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "SAVR", "Single answer",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "MAVR", "Multiple answer",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "MACOL", "Multiple answer",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "SACOL", "Single answer",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "SAHR", "Single answer",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "MAHR", "Multiple answer",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "DB", "Descriptive box",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "TE", "Text entry",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "HSLIDER", "Horizontal slider",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "SBS", "Side by side",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "TB", "Text box",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "DD", "Drop-down",
    DataDictionary['questionSelector'])

DataDictionary['questionSelector'] = np.where(
    DataDictionary['questionSelector'] == "SL", "Text single line",
    DataDictionary['questionSelector'])

# ### #Create easy variable names
#
#     THIS BLOCK IS RAMP SPECIFIC AT THE MOMENT. WILL WORK, BUT NOT AS EFFECTIVELY FOR OTHER DATASETS.
#     WILL MAKE A BROADER EASY VARIABLE NAME FUNCTION AT SOME POINT IN THE FUTURE.
#
# Will create easy variable names using the block, text and description information.
#
# Mostly will truncate these, dropping short words (like 'I", "and" etc).
#

# ## trying to use RAKE to parse text and pick keywords

# #### RAKE function to create second part of easy variable names

# Create a function that uses rake and returns the first key with a max of 7 words

# If the question is less than 6 words, just use this


def rake_first(text, stoplist):

    import re
    import string

    remove = string.punctuation
    pattern = r"[{}]".format(remove)  # create the pattern to match punctuation

    rake = Rake(max_words=5)
    key_words = rake.apply(text, text_for_stopwords=stoplist)

    n_words = len(str.split(text))  # get nmber of words

    if n_words < 7:  # if fewer than 7 words, return the words, strppied of punctuation

        text = re.sub(pattern, "", text)

        return (text)

    if len(key_words) > 1:

        return (key_words[0][0] + " " + key_words[1][0])

    elif len(key_words) > 0:
        return (key_words[0][0])

    else:
        return ('NaN')


# #### #grab relevant fields into new variable name column

# initiate empty column
DataDictionary['Easy.variable.name'] = 'NaN'

## first concatenate the variable blocks and the description into one field, dependent on question type.
# add a quick edit to SBS questionText field here

for row in range(0, DataDictionary.shape[0]):

    # side by side nested questions
    if DataDictionary['questionType'][row] == "SBS":

        DataDictionary['Easy.variable.name'][row] = DataDictionary['text'][
            row] + " " + DataDictionary['questionText'][row]

    # other categorical
    elif DataDictionary['questionType'][row] == "Categorical":
        DataDictionary['Easy.variable.name'][row] = DataDictionary['text'][row]

    # text (non-form) questions

    elif DataDictionary['questionType'][row] == "Text" and DataDictionary[
            'questionSelector'][row] != "FORM":

        DataDictionary['Easy.variable.name'][row] = DataDictionary[
            'questionText'][row]

    # text FORM questions

    elif DataDictionary['questionType'][row] == "Text" and DataDictionary[
            'questionSelector'][row] == "FORM":

        DataDictionary['Easy.variable.name'][row] = DataDictionary[
            'description'][row]

    # multiple choice sub question types

    elif DataDictionary['subQuestion'][row] == "yes":

        DataDictionary['Easy.variable.name'][row] = DataDictionary['text'][row]
    else:
        DataDictionary['Easy.variable.name'][row] = DataDictionary[
            'questionText'][row]

# change SBS type here so that we dont miss the correct easy variable name adaptations.

DataDictionary['questionType'] = np.where(
    DataDictionary['questionType'] == "SBS", "Categorical",
    DataDictionary['questionType'])

studySpecDrop = ['RAMP_', "COVID_", 'Baseline_', 'FollowUp_', 'Measures_']

# #### #create stop text list based on the new column content.
#
# This should in theory avoid using any very repetitive word content. I drop key words (like pandemic) from this text.
#

stoptext = re.sub(
    "pandemic", "",
    ' '.join(DataDictionary['Easy.variable.name'][35:1850].tolist()))

# ### #clean the variables and create varnames using block info and keywords
#

# create a function to drop words from the presecified lists


def clean_varnames(to_drop_list, text):
    """shorten easy variable names"""
    import re
    clean = re.compile('|'.join(to_drop_list))  # drop additional listed words
    return re.sub(clean, '_', text)


# clean underscores


def clean_underscore(s):
    from string import punctuation
    punc = set(punctuation)

    if s.startswith('_'):
        s = s[1:]

    reps = 1
    s = ''.join([
        '' if i > reps - 1 and e == s[i - reps] and i in punc else e
        for i, e in enumerate(s)
    ])

    return (s)


dropPhrase = [
    "How different are these feelings to how you felt ",
    "Had you felt that way ",
    "How different are these feelings to how you felt ",
    "Had you felt that way ",
    "Had you",
    "Have you ",
    "By professional we mean: any doctor, nurse or person with specialist training . Please include disorders even if you did not need treatment for them or if you did not agree with the diagnosis.",
    "By professional we mean any doctor nurse or person with specialist training  Please include disorders even if you did not need treatment for them or if you did not agree with the diagnosis",
    "that you are currently living in ?",
    "Please answer about the place you are currently living",
    "Please answer this question about the place that you are currently living",
    "Please include living areas and bedrooms Do not count bathrooms and hallways",
    "If you ",
    "in need of additional isolation or protective measure during the pandemic as identified by the government If yes please identify which group you belong to from the list below",
    "for you",
    "is it ",
    "attend work or work",
    "Since the pandemic have you",
    "Have you",
    "Had you",
    "To answer this section please click firmly on the bar for each question and drag it to the point on the scale line that best fits your experience",
    "Please respond about cigarettes or vaping",
    "I tend to take a",
    "in my life",
    "I have a ",
    "I tend to ",
    "I do not ",
    "I really ",
    "what is going to happen",
    "I usually ",
    "a lot of ",
    #"like I had",
    #     "like I was",
    "have these",
    "felt that way",
    "How different are these to your",
    "I find it ",
    "felt",
    "about the RAMP study",
    "Welcome to RAMP.",
    "Before we start can we check if you are already",
    "in the RAMP study and the longterm anonymised storage of my questionnaire data I understand that although this involves follow up surveys I can choose to withdraw at any time",
    "If you would ",
    "please select yes here",
    "would like to ",
    "in the future ",
    "do you have ",
    "individuals live ",
    "Are you at ",
    "due to this pandemic ",
    "Is there an ",
    " to you",
    "or due to the ",
    "in the RAMP study and the longtoterm anonymised storage of my questionnaire data I understand that although this involves follow up surveys I can choose to withdraw at any time",
    "eg if 2mg please enter 2",
    "eg if twice daily please enter 2",
    "have you ",
    "is the ",
    "Finding it ",
    " as if you are ‘acting out’ a dream",
    "in your ",
    "has there been any ",
    "For pregnant women has your ",
    "I think about what will happen ",
    "Whether or not ",
    " if you do get it",
    "Felt as if a ",
    " of a family member friend or acquaintance",
    "Felt as if you are ",
    " other than yourself",
    " or power",
    "how often you performed these activities ",
    "I feel I ",
    " because I am afraid I might need them later",
    " gas and water taps and light switches after turning them off",
    "I need ",
    " to be",
    "I feel that there are ",
    "I get ",
    "when I know it has been ",
    "Over the past two weeks how often been bothered by feeling ",
    " certain",
    " over and over again",
    " in doing things",
    "Thoughts that you would be ",
    " or hurting yourself in some way",
    "Many people have thoughts that ",
    "Have you felt that way?",
    "so slowly that other people could have noticed.",
    "the opposite; being so ",
    " that you have been moving around a lot more than usual",
    " on things, such as reading the newspaper or watching TV",
    "(more than twice in the same time period during the day) ",
    " than usual (overeating or eating when you were not hungry)",
    "you felt ",
    "(undereating or not eating even when you were hungry) ",
    " than usual ",
    "because ",
    ", whether or not you meant to end your life",
    "Was this ",
    "How much were you bothered by this problem?",
    " of images of a stressful experience from the past",
    "a stressful experience from the past",
    " you of a stressful experience from the past",
    "you of a ",
    " from the past",
    "because they ",
    " or situations because they reminded you of a stressful experience from the past",
    "this does not include activities or situations that are currently restricted or advised against",
    "Feeling distant or ",
    "Feeling very ",
    " something ",
    " from other people",
    "^(.*?_)(.*_.*)(_.*)$"
]

## Make sure the _ is always last
"""this whole block is messy, but works. Dont currently have the time / capacity to clean it"""

## change dashes to "to" for age brackets
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("-", "to", x))

## drop brackets then punctuation

## drop everything in brackets
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("[\(].*?[\)]", "", x))

## dropping all punctuation
remove = string.punctuation
pattern = r"[{}]".format(remove)  # create the pattern to match punctuation

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub(pattern, "", x))

## replace marital/relationship with just relationship
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("marital/relationship", "relationship", x))

# strip whitespace
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: x.strip())

## another underscore sweep
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("^(.*?_)(.*_.*)(_.*)$", "", x))

## remove newlines
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("([\r\n]+)", "", x))

## another underscore sweep
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("^(.*?_)(.*_.*)(_.*)$", "", x))

## another underscore sweep
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("(_.*)$", "", x))

# strip remaining whitespace
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: x.strip())

# drop any text in the drop phrase list above using our clean varnames function
# this will lose anything manually identified as unnecessary including _

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: clean_varnames(dropPhrase, x))

## clean leading and trailing underscores

## another underscore sweep - leading
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("^(_)", "", x))

# strip whitespace
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: x.strip())

## another underscore sweep - leading
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("^(_)", "", x))

# strip whitespace
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: x.strip())

## another underscore sweep - trailing
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("(_)$", "", x))

# strip whitespace
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: x.strip())

## another underscore sweep - trailing
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("(_)$", "", x))

# strip whitespace
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: x.strip())

## another underscore sweep - trailing
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("(_)", "", x))

## adjust the relationship with people living with you question
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub(
        "live with others how would you describe your relationships with them",
        "relationship with household members", x))

## adjust individual questions
DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("need r job", "need for job", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("Felt like I had a lot to look forward to",
                         "a lot to look forward to", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("changer", "change", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("damager", "damage", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub("pandemicI", "pandemic", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("objects people or animals that", "things", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub(
        "difficult to touch an object touched by strangers or certain people",
        "difficulty touching object touched by others", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: re.sub(
        "anxious blue bored frustrated or lonely", "negative emotion", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("related to the pandemic", "related pandemic", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("having angry outbursts", "angry", x))

DataDictionary['Easy.variable.name'] = DataDictionary[
    'Easy.variable.name'].apply(
        lambda x: re.sub("Feeling irritable or", "irritable", x))

# manually change the self harm items where there is no way of indicating which itemr efers to what in the metadata

## RAMP QIDs

DataDictionary['Easy.variable.name'].loc[
    DataDictionary['qid'] ==
    "QID124942607"] = "life not worth living two weeks"
DataDictionary['Easy.variable.name'].loc[
    DataDictionary['qid'] ==
    "QID124942608"] = "life not worth living before pandemic"
DataDictionary['Easy.variable.name'].loc[
    DataDictionary['qid'] ==
    "QID124942610"] = "contemplated self harm two weeks"
DataDictionary['Easy.variable.name'].loc[
    DataDictionary['qid'] ==
    "QID124942611"] = "contemplated self harm before pandemic"
DataDictionary['Easy.variable.name'].loc[
    DataDictionary['qid'] ==
    "QID124942613"] = "deliberately harmed self pre pandemic"

## COPING QIDs

#DataDictionary['Easy.variable.name'].loc[DataDictionary['qid'] == "QID124990660"] = "life not worth living two weeks"
#DataDictionary['Easy.variable.name'].loc[DataDictionary['qid'] == "QID124990661"] = "life not worth living before pandemic"
#DataDictionary['Easy.variable.name'].loc[DataDictionary['qid'] == "QID124990663"] = "contemplated self harm two weeks"
#DataDictionary['Easy.variable.name'].loc[DataDictionary['qid'] == "QID124990664"] = "contemplated self harm before pandemic"
#DataDictionary['Easy.variable.name'].loc[DataDictionary['qid'] == "QID124990666"] = "deliberately harmed self before pandemic"

# #### #Apply the rake function to our new variable
#
# this should limit it to key words.

DataDictionary['Easy.variable.nameTEMP'] = DataDictionary[
    'Easy.variable.name'].apply(lambda x: rake_first(x, stoptext))

# #### clean up the text
#
# #strip spaces, replace them with underscores
# #remove anything within parentheses
# #drop any words we definitely dont want
#

DataDictionary[
    'Easy.variable.nameTEMP'] = DataDictionary['Easy.variable.nameTEMP'].apply(
        lambda x: re.sub(" ", "_", x))  # replace spaces with underscores

# #### add block names

## drop all but the last bit of the block name to keep the important part of the scale name

for row in range(0, DataDictionary.shape[0]):

    DataDictionary['Easy.variable.nameTEMP'][
        row] = DataDictionary['block'][row].split(
            "_")[-1] + "." + DataDictionary['Easy.variable.nameTEMP'][row]

# #### add wave details

# append the wave information to the back of the variable names
DataDictionary['Easy.variable.nameTEMP'] = DataDictionary[
    'Easy.variable.nameTEMP'] + "." + DataDictionary['Wave']

# #### #Drop descriptive blocks and captcha

# create a new datarame without the descriptive blocks and captcha

DataDictionary = pd.DataFrame(
    DataDictionary[(DataDictionary['questionType'] != 'Descriptive block')
                   & (DataDictionary['questionType'] != 'Captcha')])

# ## Final clean up
#
# ### Drop surplus columns and rename for easier use
#
# Drop columns that arent informative and name ther columns so that they are easier to work with.

dict_newnames = {
    'qid': 'QuestionID',
    'block': 'surveyBlock',
    'questionText': 'questionText',
    'questionName': 'qualtricsName',
    'choices': 'originalLevel',
    'recode': 'recodeLevel',
    'description': 'valueLabel',
    'text': 'itemLabel',
    'subQuestion': 'subQuestion',
    'questionType': 'questionType',
    'questionSelector': 'questionSelector',
    'questionSubSelector': 'questionSubSelector',
    'validationForce': 'validationForced',
    'validationRequest': 'validationRequested',
    'validationType': 'validationType',
    'validationSettings': 'validationSettings',
    'Wave': 'Wave',
    'Easy.variable.name': 'Easy.variable.nameOLD',
    'Easy.variable.nameTEMP': 'easyVariableName'
}

DataDictionary.columns = DataDictionary.columns.map(dict_newnames)

DataDictionary.columns

# ### Drop surplus columns
#
# and then reorder them

DataDictionary = DataDictionary.drop([
    'subQuestion', 'Easy.variable.nameOLD', 'questionSelector',
    'questionSubSelector', 'validationRequested', 'validationSettings'
],
                                     axis=1)

colorder = [
    'QuestionID', 'surveyBlock', 'easyVariableName', 'itemLabel', 'Wave',
    'questionText', 'originalLevel', 'recodeLevel', 'valueLabel',
    'validationForced', 'validationType', 'questionType', 'qualtricsName'
]

DataDictionary = DataDictionary[colorder]

# ### drop identical rows
#
# drop any rows where every cell is the same.

#DataDictionary = DataDictionary.sort_values(["surveyBlock","QuestionID"])

DataDictionary.drop_duplicates(keep='first', inplace=True)

# ## Google sheets update

# ### Create an api link to data dictionary google sheet and update
#
# Did this via the google developer console under the ramp project. Created json and shared with client email.
#
# Note that needed to pip install oauth2client (v1.5.2), PyOpenSSL, gspread (great package for intgerating with sheets)

# #### #set up credentials and link with the google sheet dictionary
#
# Important note: all necessary credientials are saved in the file that gets loaded in once at the top of this script and must not be saved on github.If keys are ever saved on github they need to be reenerated immediatly.

# Establish the project credentials.

credentials = service_account.Credentials.from_service_account_file(googjson)

# with scopes
scoped_credentials = credentials.with_scopes(
    ['https://www.googleapis.com/auth/cloud-platform'])

# made the link with the acreddited service account
gc = gspread.service_account()

# Open the first sheet of the data dictionary
sheet = gc.open("RAMP.DataDictionary").sheet1

# check the link has been established.

print(sheet)

# ### #Update the dictionary spreadsheet with the qualtrics vairables.
# First establish the range to be updated. Then update.

# ##### #set variables for flexible updating

# Establish update range.

lastrow = len(DataDictionary)
lastcol = chr(ord('A') + DataDictionary.shape[1])

print('last row:', lastrow)
print('last letter:', lastcol)

# create range variable.

updateRange = "A1:{0}{1}".format(lastcol, lastrow)

print(updateRange)

# ##### #Get the alphabet position for the last column to use to create the empty list dataframe for refreshing google page.


def char_position(letter):
    return ord(str.lower(letter)) - 97


lastcol_index = char_position(lastcol)

# ###### #clear the old dictionary using ranges.

# clear the older sheet.

cols = [''] * lastcol_index
whole_grid = [cols] * lastrow

sheet.update(updateRange, whole_grid)

# ### Update data dictionary with newly created dictionary

# Update cells
sheet.update([DataDictionary.columns.values.tolist()] +
             DataDictionary.values.tolist())

# format heading bold

sheet.format('A1:R1', {'textFormat': {'bold': True}})
