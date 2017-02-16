# esadocs-bulk

A shiny-based bulk PDF uploader to complement [ESAdocs Search](https://github.com/jacob-ogre/esadocs)
and [Upload](https://github.com/jacob-ogre/esadocs-upload).

## Challenge

We initialized the ESAdocs database with docs scraped from the web, acquired by
Freedom of Information Act requests, and gathered haphazardly. But new documents
are created every day and untold numbers of documents exist but are not in our
dataset. [ESAdocs Upload](https://github.com/jacob-ogre/esadocs-upload) allows
users to enter metadata and upload a single document. This works well when there 
aren't too many docs to add to the database, but people may be hesitant to
put in the effort if they have tens or more of relevant documents.

## Solution

The solution is to provide a basic multi-file uploader app as well as some
complementary processing scripts to control file passing among servers and
optical character recognition. The app is based on [Shiny](http://shiny.rstudio.com/), 
and the scripts make the following assumptions:

  - there are two servers: one that hosts the app and one where PDF ...

### Setup

Setup requires storing several system variables in `~/.Renviron`, including:

    ESADOC_KEY="anicelongstringwith&djn8sdfnj"
    
    OCR_SERVER="user@<IP_ADDRESS>"
    DOC_SERVER="user@<IP_ADDRESS>"
    
    BULK_PATH="/home/user/Data/bulk_ESAdocs"
    OCR_PATH="/home/user/Data/bulk_ESAdocs_OCR"
    BAK_PATH="/home/user/Data/ESAdocs_ES_bak"
    STG_PATH="/home/user/Data/stage_ESAdocs"
    DOC_PATH="/home/user/Data/ESAdocs"

The app is quite basic: before the uploader is even available, the person's 
name and email must be entered and the type of Endangered Species Act-related
document must be selected. After those tasks pass basic checks server-side,
the upload (multiple = TRUE) file input is available. As currently written the
app accepts only PDFs and only up to 30MB per file. (May need to cap the total
size of the upload in the future...) 

In addition to the basic checks described above, the user must enter a unique 
"key" before any files are moved out of a tmp directory on the server. We share 
this key - a 27-character string - in a file that is tightly secured in a totally
separate platform. The key can be quickly changed server-side 
