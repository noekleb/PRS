&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
/* Procedure Description
"Basic Window Template

Use this template to create a new window. Alter this default template or create new ones to accomodate your needs for different default sizes and/or attributes."
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*********************************************************************
* Copyright (C) 2001 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOK                 AS LOG    NO-UNDO.
DEF VAR ix                  AS INT    NO-UNDO.
                            
DEF VAR hToolbar            AS HANDLE NO-UNDO.
DEF VAR hBrowse             AS HANDLE NO-UNDO.
                            
DEF VAR cEntityId           AS CHAR   NO-UNDO.
DEF VAR cContext            AS CHAR   NO-UNDO.
DEF VAR cDocDescription     AS CHAR   NO-UNDO.
DEF VAR hFillInDescription  AS HANDLE NO-UNDO.

DEF VAR iXpos               AS INT    NO-UNDO INIT ?.
DEF VAR iYpos               AS INT    NO-UNDO INIT ?.
                            
DEF VAR hParent             AS HANDLE NO-UNDO.
                            
DEF VAR bEnglish            AS LOG    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectToolBar rectWinToolbar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetDocContext C-Win 
FUNCTION SetDocContext RETURNS LOGICAL
  ( INPUT icEntityId       AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icDocDescription AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setXYpos C-Win 
FUNCTION setXYpos RETURNS LOGICAL
  ( INPUT iiXpos AS INT,
    INPUT iiYpos AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 90 BY 5.24.

DEFINE RECTANGLE rectToolBar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY .95.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectBrowse AT ROW 2.43 COL 2
     rectToolBar AT ROW 1.14 COL 1.8
     rectWinToolbar AT ROW 1.14 COL 83.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 93 BY 6.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window Template
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dokumenter"
         HEIGHT             = 6.91
         WIDTH              = 93
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/cntrlhry.ico":U) THEN
    MESSAGE "Unable to load icon: ico/cntrlhry.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 6.95
       FRAME DEFAULT-FRAME:WIDTH            = 93.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dokumenter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dokumenter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  IF THIS-PROCEDURE:PERSISTENT THEN
    PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    IF CAN-DO(hParent:INTERNAL-ENTRIES,"getDocListXYpos") THEN
      RUN getDocListXYpos IN hParent (OUTPUT iXpos,OUTPUT iYpos).
    IF CAN-DO(hParent:INTERNAL-ENTRIES,"getDocListContext") THEN
      RUN getDocListContext IN hParent (OUTPUT cEntityId,OUTPUT cContext,OUTPUT cDocDescription).
    RUN InitializeObject.
    SetDocContext (cEntityId,cContext,cDocDescription).
    THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  END.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN OpenDocs.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iDokId     AS INT    NO-UNDO.
DEF VAR hDeleteBtn AS HANDLE NO-UNDO.
DEF VAR cDocIdList AS CHAR   NO-UNDO.

hDeleteBtn = DYNAMIC-FUNCTION("getEventWidget",hToolbar,"delete","").
IF NOT VALID-HANDLE(hDeleteBtn) OR NOT hDeleteBtn:SENSITIVE THEN RETURN.

DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    cDocIdList = cDocIdList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE) + ",".
END.
cDocIdList = TRIM(cDocIdList,",").

/* iDokId = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE. */

RUN SUPER.

DO ix = 1 TO NUM-ENTRIES(cDocIdList):
  DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",ENTRY(ix,cDocIdList),?).
END.

/* IF iDokId NE 0 THEN                                                      */
/*   DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",STRING(iDokId),?). */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.
hFillInDescription:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO hBrowse:NUM-DROPPED-FILES:
  DYNAMIC-FUNCTION("LoadDocs",hBrowse:GET-DROPPED-FILE(ix),cContext,cEntityId,cDocDescription).
END.
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
hFillInDescription:SENSITIVE = YES.
APPLY "entry" TO hFillInDescription.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE rectBrowse rectToolBar rectWinToolbar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  bEnglish = NOT DYNAMIC-FUNCTION("Scandinavian").

  IF bEnglish THEN {&WINDOW-NAME}:TITLE = "Documents".

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    rectBrowse:HANDLE,
                    100,            
                    "multiple",             
                    "JBoxDocRel"
                    + ";!cContext"
                    + ";!cEntityId"
                    + ";iJBoxDocumentId|" + (IF bEnglish THEN "Id" ELSE "Nr")
                  + ",JBoxDocument"
                    + ";cFileName"     + (IF bEnglish THEN "|File name" ELSE "")
                    + ";cDescription"  + (IF bEnglish THEN "|Description" ELSE "")
                    + ";dFileModDate"
                    + ";+cModTime|CHARACTER|x(8)|jbtime_hhmmss(iFileModTime)|" + (IF bEnglish THEN "Time" ELSE "Tid")
                    + ";cFullPathName" + (IF bEnglish THEN "|Full path" ELSE "")
                    + ";iDocSize"      + (IF bEnglish THEN "|Size" ELSE "")
                    + ";cFileType"
                    + ";!iFileModTime"
                    ,"WHERE false,FIRST JBoxDocument OF JBoxDocRel NO-LOCK", 
                    "sort|iJBoxDocumentId").

  hColumn = DYNAMIC-FUNCTION("getBrowseColumn",hBrowse,"cFileName").
  hColumn:WIDTH-PIXELS = 150.

  IF DYNAMIC-FUNCTION("Scandinavian") THEN
    hBrowse:TOOLTIP = "Dobbelklikk for å åpne dokument" +
                      (IF NOT CAN-DO(DYNAMIC-FUNCTION("getSecDisabledActions",""),"save") THEN
                        CHR(10) + "Dra og slipp her for å laste dokument"
                       ELSE "").
  ELSE 
    hBrowse:TOOLTIP = "Doubleclick to open document" +
                      (IF NOT CAN-DO(DYNAMIC-FUNCTION("getSecDisabledActions",""),"save") THEN
                        CHR(10) + "Drop new documents here"
                       ELSE "").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"fieldnamedeletewarning","cFileName").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"CustomUpdateValProc","=jbdoc_savedocdesc.p").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,
                    IF bEnglish THEN "File" ELSE "Fil",            
                    "Delete" + (IF NOT bEnglish THEN ";Slett" ELSE "")
                  + ",Edit;"  + (IF bEnglish THEN "Edit &description" ELSE "Endre &beskrivelse")
                  + ",BrowseConfig;" + (IF bEnglish THEN "Configure browse" ELSE "Kolonneoppsett")
                  + ",Excel"
                  + ",OpenDocs;"
                    + (IF bEnglish THEN
                         "Open document;Open selected documents"
                       ELSE
                         "Åpne dokument;Åpne valgte dokumenter")
                    + ";OpenDocs;bmp/folderopen.bmp"
                    ,"maxborder").

  hFillInDescription = DYNAMIC-FUNCTION("NewBrowseFillIn",hBrowse,
                                        "cDescription","",
                                        "","","","").
  DYNAMIC-FUNCTION("CreateOverlayLink",hBrowse,hFillInDescription,"cDescription").
  
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).

  DYNAMIC-FUNCTION("NewToolBar",
                    rectWinToolBar:HANDLE, 
                    IF bEnglish THEN "File" ELSE "Fil",            
                    "close;" + (IF bEnglish THEN "Exit&" ELSE "Avslutt&"),
                    "right,enable").

  IF NOT CAN-DO(DYNAMIC-FUNCTION("getSecDisabledActions",""),"save") AND
     DYNAMIC-FUNCTION("getIsEventAllowed",hToolbar,"save") THEN
    hBrowse:DROP-TARGET = TRUE.

END.

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE, "rectToolBar").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,200,100,0,0).

DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

IF iXpos NE ? THEN
  THIS-PROCEDURE:CURRENT-WINDOW:X = iXpos.
IF iYpos NE ? THEN
  THIS-PROCEDURE:CURRENT-WINDOW:Y = iYpos.

IF THIS-PROCEDURE:PERSISTENT THEN
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenDocs C-Win 
PROCEDURE OpenDocs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
  IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
    DYNAMIC-FUNCTION("ViewDocs","JBoxDocument",STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("iJBoxDocumentId"):BUFFER-VALUE),TRUE,"").
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetDocContext C-Win 
FUNCTION SetDocContext RETURNS LOGICAL
  ( INPUT icEntityId       AS CHAR,
    INPUT icContext        AS CHAR,
    INPUT icDocDescription AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cBaseQuery AS CHAR NO-UNDO.
DEF VAR ix         AS INT  NO-UNDO.

IF NUM-ENTRIES(icEntityId,";") > 1 THEN
  DO ix = 1 TO NUM-ENTRIES(icEntityId,";"):
    cBaseQuery = cBaseQuery + (IF cBaseQuery NE "" THEN " OR (cEntityId = '" ELSE "WHERE (cEntityId = '")
               + ENTRY(ix,icEntityId,";") + "' AND cContext = '" + ENTRY(ix,icContext,";") + "')".
  END.

ASSIGN cEntityId       = ENTRY(1,icEntityId,";")
       cContext        = ENTRY(1,icContext,";")
       cDocDescription = icDocDescription.


DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery",
                 IF cBaseQuery NE "" THEN cBaseQuery
                 ELSE
                 "WHERE cEntityId = '" + icEntityId + "'"
               + (IF icContext NE "" THEN 
                   " AND cContext BEGINS '" + icContext + "'"
                  ELSE "")
                 ).
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN OpenQuery.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setXYpos C-Win 
FUNCTION setXYpos RETURNS LOGICAL
  ( INPUT iiXpos AS INT,
    INPUT iiYpos AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN iXpos = iiXpos
       iYpos = iiYpos.

RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

