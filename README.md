# esadocs-bulk

A shiny-based bulk PDF uploader to complement ESAdocs Search and Upload.

## Usage

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
