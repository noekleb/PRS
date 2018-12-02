/******************************************************************************

  Program:      pdf_pre.i
  
  Written By:   Gordon Campbell
  Written On:   July 6, 2005
  
  Description:  Preprocessor defintions for PDFinclude
  
******************************************************************************/

&GLOBAL-DEFINE PDFDIR 

&IF OPSYS = "UNIX" &THEN
  &GLOBAL-DEFINE zlib          /lib/libz.so.1
&ELSE
  &GLOBAL-DEFINE zlib          zlib1.dll
&ENDIF

&GLOBAL-DEFINE MD5LIB          c:\gord\PDFinclude2\dll\md5.exe
&GLOBAL-DEFINE pdfencryptlib   c:\gord\pdfinclude2\dll\procryptlib.dll

/* end of pdf_pre.i */
