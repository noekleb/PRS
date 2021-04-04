/******************************************************************************

  Program:      PDFinfo-test.p
  
  Written By:   Gordon Campbell
  Written On:   January 2004
  
  Description:  This procedure tests PDFinfo.p 
  
******************************************************************************/

{ pdfinfo.i }

RUN PDFinfo.p ("c:\Program Files\Progress\ODBC\odbcref.pdf",
               OUTPUT TABLE TT_Info,
               OUTPUT TABLE TT_Font).

FOR EACH TT_Info:
  disp TT_Info.info_name  FORMAT "x(15)" LABEL "Name"
       TT_Info.info_value FORMAT "X(55)" LABEL "Value".
END.

FOR EACH TT_Font:
  disp TT_Font.font_name format "x(60)".
END.

/* ----------- The following is a list of test files ----------- */
/* Copy a filename into the first input parameter

To Test Acrobat Reader and Acrobat SDK files ...

"c:\program files\adobe\acrobat 5.0\help\ENU\acrobat.pdf"
"c:\program files\adobe\acrobat 5.0\help\ENU\docbox.pdf"
"c:\program files\adobe\acrobat 5.0\help\ENU\minireader.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\releasenotes.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Core_API\CoreAPIOverview.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Extended_API_for_Plugins\ADMReferenceGuide.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Extended_API_for_Plugins\ADMReferenceGuide.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Getting_Started\DeveloperFAQ.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Getting_Started\DevelopmentOverview.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Getting_Started\DocumentationRoadmap.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Getting_Started\GettingStarted.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Getting_Started\PluginTutorial.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\Getting_Started\SamplesGuide.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\InterApplication_Communication\IACReference.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\InterApplication_Communication\IACOverview.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\InterApplication_Communication\VBJavascript.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\PDF_Creation_APIs\DistillerAPIReference.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\PDF_Creation_APIs\DistillerParameters.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\PDF_Creation_APIs\pdfmarkReference.pdf"
"c:\program files\adobe\acrobat 5.0 SDK\documentation\PDF_Creation_APIs\PDFWriterAPIReference.pdf"

To Test PDFlib files ...

"c:\program files\PDFlib\PDFlib 5.0.2p1\doc\grid.pdf"
"c:\program files\PDFlib\PDFlib 5.0.2p1\doc\PDFlib-license.pdf"
"c:\program files\PDFlib\PDFlib 5.0.2p1\doc\PDFlib-manual-COM-dotnet.pdf"
"c:\program files\PDFlib\PDFlib 5.0.2p1\doc\PDFlib-purchase-order.pdf"
"c:\program files\PDFlib\PDFlib 5.0.2p1\resource\boilerplate.pdf"
"c:\program files\PDFlib\PDFlib 5.0.2p1\resource\reference.pdf"
"c:\program files\PDFlib\PDFlib 5.0.2p1\resource\stationery.pdf"

To Test PDFinclude files ....

"c:\gord\PDFinclude\samples\super\itemlist.pdf"
"c:\gord\PDFinclude\samples\super\bookmark.pdf"
"c:\gord\PDFinclude\samples\super\calendar.pdf"
"c:\gord\PDFinclude\samples\super\table.pdf"

Miscellaneous tests ... 

"c:\gord\text2pdf\text2pdfdocumentation.pdf"
"c:\gord\text2pdf\text2pdf datasheet.pdf"
"c:\program files\Adobe\Photoshop 7.0\Samples\Travel Poster.pdf"
"c:\Documents and Settings\Gcampbell\Desktop\Temperature.pdf"
"c:\wika\newsletter\newsletter.pdf"
"c:\gord\Kanada\IT\TP_ServerSpecifikation.pdf"  -- ENCRYPTED
"c:\gord\Kanada\ModelCodes_Temp\MC_OPTIONS_SI440F.pdf"
"c:\Program Files\Progress\ODBC\odbcref.pdf"

*/
