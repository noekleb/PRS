&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* ***************************  Definitions  ************************** */

CREATE WIDGET-POOL.

&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
 &SCOPED-DEFINE AdvGuiWin 

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VARIABLE oJboxImage     AS uc.JboxImage      NO-UNDO. 
DEF VAR oJboxEmail AS JBoxEmailViaDefaultClient     NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOK          AS LOG    NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR cContext     AS CHAR   NO-UNDO.
DEF VAR hParent      AS HANDLE NO-UNDO.
DEF VAR hToolbar     AS HANDLE NO-UNDO.
DEF VAR cDocId       AS CHAR   NO-UNDO.
DEF VAR cPictureFile AS CHAR   NO-UNDO.
DEF VAR cPictureDesc AS CHAR   NO-UNDO.
DEF VAR bScand       AS LOG  NO-UNDO.

DEF TEMP-TABLE ttPicture NO-UNDO
    FIELD iJBoxDocumentId  AS INT
    FIELD blDocument       AS BLOB
    FIELD iDocSize         AS INT
    FIELD cFileType        AS CHAR
    FIELD RowIdent         AS CHAR
    .
DEF VAR hBufPicture AS HANDLE.
hBufPicture = BUFFER ttPicture:HANDLE.

DEF STREAM strPicture.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmImage

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbLargeImage edPictureDesc 
&Scoped-Define DISPLAYED-OBJECTS edPictureDesc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */







&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPictureFile C-Win 
FUNCTION LoadPictureFile RETURNS LOGICAL
  ( INPUT icDocId       AS CHAR,
    INPUT icContext     AS CHAR,
    INPUT icDocDesc     AS CHAR, 
   INPUT icPictureFile AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PictureToDisk C-Win 
FUNCTION PictureToDisk RETURNS CHARACTER
  ( INPUT iiDocId AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setContext C-Win 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setPictureDesc C-Win 
FUNCTION setPictureDesc RETURNS LOGICAL
  ( INPUT icPictureDesc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShowPicture C-Win 
FUNCTION ShowPicture RETURNS LOGICAL
  ( INPUT iiDocId   AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE edPictureDesc AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 124.8 BY 3.81 NO-UNDO.

DEFINE RECTANGLE tbLargeImage
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmImage
     edPictureDesc AT ROW 19.81 COL 2.2 NO-LABEL
     tbLargeImage AT ROW 1.24 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127 BY 22.76.

DEFINE FRAME Image-View
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.52
         SIZE 125 BY 17.14
         TITLE "Frame A" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Tittel"
         HEIGHT             = 22.76
         WIDTH              = 127
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
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
/* REPARENT FRAME */
ASSIGN FRAME Image-View:FRAME = FRAME frmImage:HANDLE.

/* SETTINGS FOR FRAME frmImage
   FRAME-NAME                                                           */
ASSIGN 
       edPictureDesc:READ-ONLY IN FRAME frmImage        = TRUE.

/* SETTINGS FOR FRAME Image-View
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tittel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tittel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Tittel */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
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
ON CLOSE OF THIS-PROCEDURE 
DO:
  DYNAMIC-FUNCTION("setCleanUpResize", THIS-PROCEDURE:CURRENT-WINDOW).  
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
  DELETE OBJECT oJboxImage.
  IF VALID-OBJECT(oJboxEmail) THEN DELETE OBJECT oJboxEmail.  
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  /*
  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
   /* RUN MoveToTop.*/
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF
  */

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cLinkedDocTitles AS CHAR NO-UNDO.
DEF VAR cLinkedDocIds    AS CHAR NO-UNDO.
DEF VAR cParentDocIds    AS CHAR NO-UNDO.
DEF VAR cOtherRelToDocs  AS CHAR NO-UNDO.
DEF VAR cDocRelIdList    AS CHAR NO-UNDO.
DEF VAR iReturn          AS INT  NO-UNDO.
DEF VAR cDeleteParam     AS CHAR NO-UNDO.
DEF VAR ix               AS INT NO-UNDO.
DEF VAR oJBoxDocRel      AS JBoxBuffer NO-UNDO. 

oJBoxDocRel = NEW JBoxBuffer("JBoxDocRel").

IF oJBoxDocRel:Find("WHERE iJBoxDocumentId = " + cDocId + " AND cContext NE '" + cContext + "'") THEN      
  cOtherRelToDocs = cOtherRelToDocs + (IF cOtherRelToDocs NE "" THEN "," ELSE "") + STRING(oJBoxDocRel:BUFFER-HANDLE::iJBoxDocRelId).

cDocRelIdList = cDocRelIdList + (IF cDocRelIdList NE "" THEN "," ELSE "") 
              + JBoxServerAPI:Instance:FieldValues("JBoxDocRel",
                                                    "WHERE iJBoxDocumentId = " + cDocId + " AND cContext = '" + cContext + "'",
                                                    "iJBoxDocRelId"). 
     
IF cOtherRelToDocs NE "" THEN DO:
  iReturn = DYNAMIC-FUNCTION("DoMessage",0,3,
                        (IF bScand THEN
                          "Det finnes knytninger til andre begreper for bilde(r)." + CHR(10)
                        + "JA: Bilde(r) slettes sammen med alle knytninger" + CHR(10)
                        + "NEI: Bilde(r) og andre knytninger beholdes men denne knytningen (til " + cContext + ") slettes" + CHR(10)
                        + "AVBRYT: Ingen sletting"
                        ELSE
                          "There are links to other entities for document(s)." + CHR(10)
                        + "YES: Image(s) and all links to other entities are deleted" + CHR(10)
                        + "NO: Image(s) and other links are kept but this link (to " + cContext + ") is deleted" + CHR(10)
                        + "CANCEL: Nothing is deleted")
                        ,"","").
  IF iReturn = 2 THEN RETURN.
  IF iReturn = 6 THEN 
    cDeleteParam = "strong".  
  ELSE cDeleteParam = "weak".
END.
ELSE IF DYNAMIC-FUNCTION("DoMessage",0,4,
                      (IF DYNAMIC-FUNCTION("Scandinavian") THEN
                        "Slett bilde?"
                      ELSE
                        "Delete image?"),
                       "","") = 6 THEN
  cDeleteParam = "strong".
ELSE RETURN.    

IF cDeleteParam = "strong" AND NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",cDocId,?) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN.
END.

ELSE IF cDeleteParam = "weak" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cDocRelIdList):
    JBoxServerAPI:Instance:Delete("JBoxDocRel",
                                   "iJBoxDocRelId",
                                   ENTRY(ix,cDocRelIdList),
                                   "NO",
                                   "").
  END.
END.

PUBLISH "DocumentAddDelete" ("delete").

/*
IF DYNAMIC-FUNCTION("DoMessage",0,4,IF bScand THEN "Slett " + STRING(ix) + " bilder?" ELSE "Delete " + STRING(ix) + " images?"
                                       ,"","") = 6 THEN DO:
  FOR EACH ttPicture
      WHERE ttPicture.bSelected
      :
    IF NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",STRING(ttPicture.iDocId),?) THEN 
      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
    ELSE 
      DELETE ttPicture.    
  END.
  
  RUN SortPictures (cmbSort:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
END.



IF DYNAMIC-FUNCTION("DoMessage",0,4,
                    IF DYNAMIC-FUNCTION("Scandinavian") THEN "Slett bilde?" ELSE "Delete picture?","","") = 6 THEN DO:
  RUN DeletePicture IN hParent NO-ERROR.
  IF NOT ERROR-STATUS:ERROR THEN
    APPLY "close" TO THIS-PROCEDURE.
END.
*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR ocReturn AS CHAR NO-UNDO.

RUN JBoxDEditPictureDesc.w (cDocId,OUTPUT ocReturn).

IF ocReturn NE ? THEN DO:
  edPictureDesc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ocReturn.

  IF CAN-DO(hParent:INTERNAL-ENTRIES,"EditImageTextRecord") THEN
    RUN EditImageTextRecord IN hParent (cDocId,ocReturn).
END.    

/*setPictureDesc(ocReturn).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailRecord C-Win 
PROCEDURE EmailRecord :

IF VALID-OBJECT(oJboxEmail) THEN DELETE OBJECT oJboxEmail.

oJBoxEmail = NEW JBoxEmailViaDefaultClient(cPictureDesc,"",cPictureFile).

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
  DISPLAY edPictureDesc 
      WITH FRAME frmImage IN WINDOW C-Win.
  ENABLE tbLargeImage edPictureDesc 
      WITH FRAME frmImage IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frmImage}
  VIEW FRAME Image-View IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Image-View}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImageClickEvent C-Win 
PROCEDURE ImageClickEvent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RETURN. 

   /*APPLY "close" TO THIS-PROCEDURE.
   RETURN NO-APPLY. */
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
DEF VAR cPictureDesc AS CHAR NO-UNDO.
DEF VAR bCanDelete   AS LOG  NO-UNDO.

RUN enable_UI.

oJboxImage = NEW uc.JboxImage (THIS-PROCEDURE,FRAME IMAGE-view:HANDLE).
IF NOT oJboxImage:RegisterWithJukeBox(NO) THEN 
  MESSAGE "Failed to initiate .net form. Please retry." VIEW-AS ALERT-BOX WARNING.
ELSE DO:  
  oJboxImage:dock.
  oJboxImage:BringToFront().
END.  


IF VALID-HANDLE(hParent) THEN DO WITH FRAME {&FRAME-NAME}:                                         
/*  cPictureDesc = DYNAMIC-FUNCTION("getPictureDesc" IN hParent) NO-ERROR.        */
/*  IF cPictureDesc NE "" AND cPictureDesc NE ? THEN                              */
/*    edPictureDesc:SCREEN-VALUE = cPictureDesc.                                  */
/*  ELSE                                                                          */
/*    ASSIGN edPictureDesc:HIDDEN = TRUE                                          */
/*           FRAME IMAGE-view:HEIGHT-PIXELS = FRAME IMAGE-view:HEIGHT-PIXELS + 80.*/

  bScand = DYNAMIC-FUNCTION("Scandinavian").

  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    tbLargeImage:HANDLE,   
                    "",
                    "Edit"               
                    + (IF CAN-DO(hParent:INTERNAL-ENTRIES,"canDeletePicture")
                          AND CAN-DO(hParent:INTERNAL-ENTRIES,"DeletePicture")
                          AND DYNAMIC-FUNCTION("canDeletePicture" IN hParent) THEN
                        ",Delete;" + (IF bScand THEN "Slett bilde" ELSE "Delete image")
                       ELSE "") 
                    + ",SaveToDisc;" 
                      + (IF bScand THEN "Lagre bilde;Lagre bilde til disk"
                         ELSE "Save image;Save image to disk")
                      + ";SaveToDisc;bmp/save16e.bmp"                                                    
                    + ",OpenPictures;"
                      + (IF bScand THEN "Åpne bilde;Åpne bilde med standard behandlingsprogram"
                         ELSE "Open image;Open image in default application")
                    + ";OpenPictures;bmp/folderopen.bmp"
/*                    + ",Link;" + (IF bScand THEN "Lag kryssreferanse;" ELSE "Create cross-ref;") + ";;bmp\links.bmp"*/
                    + ",Email"
/*                    + ",first|,prev|,next|,last|"*/
                    ,"enable").  

END.

DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"edPictureDesc").
DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-view").
DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmImage:HANDLE,"frmImage,IMAGE-view").
DYNAMIC-FUNCTION("setOrgWinSize", THIS-PROCEDURE:CURRENT-WINDOW,300,150,100,100).

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

  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
  THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  DYNAMIC-FUNCTION("DoLockWindow",?).
  
  IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
    RUN ShowForm ("").                                                       

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenPictures C-Win
PROCEDURE OpenPictures:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setWebDoc","",cPictureFile).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToDisc C-Win
PROCEDURE SaveToDisc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF VAR cDir AS CHAR NO-UNDO.

SYSTEM-DIALOG GET-DIR cDir.
IF cDir = "" THEN RETURN.

OS-COPY VALUE(cPictureFile) VALUE(cDir). 

IF DYNAMIC-FUNCTION("DoMessage",0,4,
                    IF NOT bScand THEN
                      "Explore directory " + cDir
                    ELSE
                      "Vis filområde " + cDir
                    ,"","") = 6 THEN
  DYNAMIC-FUNCTION("ExploreDirectory",cDir).


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Function Implementations ***************** */







&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPictureFile C-Win 
FUNCTION LoadPictureFile RETURNS LOGICAL
  ( INPUT icDocId       AS CHAR,
    INPUT icContext     AS CHAR,
    INPUT icDocDesc     AS CHAR,
    INPUT icPictureFile AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF SEARCH(icPictureFile) = ? THEN 
    icPictureFile = SESSION:TEMP-DIR + icPictureFile.
    
  ASSIGN cDocId       = icDocId
         cPictureDesc = icDocDesc
         cPictureFile = icPictureFile
         cContext     = icContext
         .

         
  IF SEARCH(icPictureFile) NE ? THEN 
    oJboxImage:LoadFile(icPictureFile) .
  ELSE oJboxImage:LoadFile(?).
    
  oJboxImage:BringToFront().
  
  edPictureDesc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = icDocDesc. 
    
/*
/*   bør lage chimage-view:picbuf:filename og autoscale load prop i klasse TBD */ 
IF SEARCH(icPictureFile) NE ? THEN DO:
  ASSIGN chIMAGE-view:Picbuf:FILENAME = icPictureFile
         chIMAGE-view:Picbuf:AutoScale = TRUE.
  chIMAGE-view:Picbuf:LOAD NO-ERROR.
  IF ERROR-STATUS:GET-MESSAGE(1) BEGINS "Error" THEN
    DYNAMIC-FUNCTION('setWebDoc','open',icPictureFile).
  FRAME frmImage:MOVE-TO-TOP().
  SESSION:SET-WAIT-STATE("").
  RETURN TRUE. 
END.
ELSE RETURN FALSE.
*/

  RETURN TRUE. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PictureToDisk C-Win 
FUNCTION PictureToDisk RETURNS CHARACTER
  ( INPUT iiDocId AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPictureFile AS CHAR   NO-UNDO.
DEF VAR mpDocument   AS MEMPTR NO-UNDO.

MESSAGE 
VIEW-AS ALERT-BOX.

EMPTY TEMP-TABLE ttPicture.

DYNAMIC-FUNCTION("getTempTable","jbserv_gettemptable.p","JBoxDocument|WHERE iJBoxDocumentId = " + STRING(iiDocId),hBufPicture).

FIND FIRST ttPicture NO-ERROR.
IF NOT AVAIL ttPicture THEN 
  RETURN "".
ELSE cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + "." + ttPicture.cFileType.

SET-SIZE(mpDocument) = ttPicture.iDocSize.
COPY-LOB FROM OBJECT ttPicture.blDocument TO OBJECT mpDocument.
OUTPUT STREAM strPicture TO VALUE(cPictureFile) NO-MAP BINARY NO-CONVERT.
EXPORT STREAM strPicture mpDocument.
OUTPUT STREAM strPicture CLOSE.
  
RETURN cPictureFile.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setContext C-Win 
FUNCTION setContext RETURNS LOGICAL
  ( INPUT icContext AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
cContext = icContext.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
-----------------------------------------------------------------------------*/
hParent = ihParent.
RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setPictureDesc C-Win 
FUNCTION setPictureDesc RETURNS LOGICAL
  ( INPUT icPictureDesc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF icPictureDesc NE "" THEN DO:
    IF edPictureDesc:HIDDEN THEN 
      ASSIGN FRAME IMAGE-view:HEIGHT-PIXELS   = FRAME IMAGE-view:HEIGHT-PIXELS - 80
             edPictureDesc:HIDDEN       = FALSE.
    edPictureDesc:SCREEN-VALUE = icPictureDesc.
  END.
  ELSE
    ASSIGN FRAME IMAGE-view:HEIGHT-PIXELS   = FRAME IMAGE-view:HEIGHT-PIXELS + 80
           edPictureDesc:HIDDEN       = TRUE.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShowPicture C-Win 
FUNCTION ShowPicture RETURNS LOGICAL
  ( INPUT iiDocId   AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cPictureFile AS CHAR NO-UNDO.

oJboxImage:CLEAR().

IF iiDocId = 0 OR iiDocId = ? THEN
  RETURN FALSE.

IF cContext = "" THEN cContext = "img".

cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".jpg".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".jpeg".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".bmp".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".tif".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".tiff".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = SESSION:TEMP-DIR + cContext + "-" + STRING(iiDocId) + "-" + DYNAMIC-FUNCTION('getASuserId') + ".pcx".
IF SEARCH(cPictureFile) = ? THEN 
  cPictureFile = PictureToDisk (iiDocId). 

MESSAGE cPictureFile
VIEW-AS ALERT-BOX.
RETURN LoadPictureFile(STRING(iiDocId),cContext,cPictureDesc,cPictureFile).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

