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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT-OUTPUT PARAM iocFileNames AS CHAR NO-UNDO.
DEF INPUT        PARAM icLoadMode   AS CHAR NO-UNDO.
/* Local Variable Definitions ---                                       */

DEF VAR bOK     AS LOG NO-UNDO.
DEF VAR ix      AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Downscale C-Win 
FUNCTION Downscale RETURNS LOGICAL
  ( INPUT icFileName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SaveUsingMSWord C-Win 
FUNCTION SaveUsingMSWord RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Org_Image AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chOrg_Image AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Small_Image AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chSmall_Image AS COMPONENT-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118 BY 9.19.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 9.14
         WIDTH              = 118.2
         MAX-HEIGHT         = 33.38
         MAX-WIDTH          = 182
         VIRTUAL-HEIGHT     = 33.38
         VIRTUAL-WIDTH      = 182
         SHOW-IN-TASKBAR    = no
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Org_Image ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 2
       HEIGHT          = 7.38
       WIDTH           = 52
       HIDDEN          = yes
       SENSITIVE       = no.

CREATE CONTROL-FRAME Small_Image ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.95
       COLUMN          = 69
       HEIGHT          = 7.38
       WIDTH           = 45
       HIDDEN          = yes
       SENSITIVE       = no.
      Org_Image:NAME = "Org_Image":U .
/* Org_Image OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Small_Image:NAME = "Small_Image":U .
/* Small_Image OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Small_Image:MOVE-AFTER(Org_Image).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Org_Image,Small_Image").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Org_Image,Small_Image").
  RUN ScalePicture.
  LEAVE.
  /* vi skall aldrig vänta, bara om vi gör fönstret synlig:vid debug!! */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
APPLY "CLOSE" TO THIS-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "JBoxPictureScale.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chOrg_Image = Org_Image:COM-HANDLE
    UIB_S = chOrg_Image:LoadControls( OCXFile, "Org_Image":U)
    chSmall_Image = Small_Image:COM-HANDLE
    UIB_S = chSmall_Image:LoadControls( OCXFile, "Small_Image":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "JBoxPictureScale.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN chSmall_Image = chSmall_Image:Picbuf.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScalePicture C-Win 
PROCEDURE ScalePicture :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iHeight       AS INT  NO-UNDO.
DEF VAR cLoadFiles    AS CHAR NO-UNDO.
DEF VAR cClipText     AS CHAR NO-UNDO.
DEF VAR cClipType     AS CHAR NO-UNDO.
DEF VAR cFileType     AS CHAR NO-UNDO.
DEF VAR cWordFile     AS CHAR NO-UNDO.
DEF VAR bScale        AS LOG  NO-UNDO.

SESSION:SET-WAIT-STATE("general").
DO ix = 1 TO NUM-ENTRIES(iocFileNames,";"):
  bScale = FALSE.
  IF iocFileNames = "clipboard" THEN DO:
    chOrg_Image:Picbuf:errornumber = 0.
    IF CLIPBOARD:NUM-FORMATS > 0 THEN DO:
      DYNAMIC-FUNCTION("DoMessage",0,0,
                       IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                         "Clipboard er tomt/inneholder ugyldige data"
                       ELSE 
                         "Clipboar is empty or contains invalid data"
                       ,"(Invalid CLIPBOARD:NUM-FORMATS)","").
      iocFileNames = "".
      SESSION:SET-WAIT-STATE("").
      RETURN.
    END.
    ELSE DO:
      ASSIGN cClipText = CLIPBOARD:VALUE NO-ERROR.
      IF cClipText <> ? THEN DO:
        cWordFile = SaveUsingMSWord().
        IF cWordFile = "" THEN DO:
          DYNAMIC-FUNCTION("DoMessage",0,0,
                         IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                           "Kunne ikke lagre tekstinnhold i Word dokument for lasting"
                         ELSE 
                           "Failed to create Word file to load"
                         ,"","").
          iocFileNames = "".
          SESSION:SET-WAIT-STATE("").
          RETURN.
        END.
        ELSE ASSIGN cFileType = "doc"
                    cLoadFiles = cLoadFiles + cWordFile + ";".
      END.
      ELSE DO:
        chOrg_Image:Picbuf:PasteClipboard NO-ERROR.
        IF chOrg_Image:Picbuf:errornumber <> 0 THEN DO:
          cWordFile = SaveUsingMSWord().
          IF cWordFile = "" THEN DO:
            DYNAMIC-FUNCTION("DoMessage",0,0,
                           IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                             "Kunne ikke lagre tekstinnhold i Word dokument for lasting"
                           ELSE 
                             "Failed to create Word file to load"
                           ,"","").
            iocFileNames = "".
            SESSION:SET-WAIT-STATE("").
            RETURN.
          END.
          ELSE ASSIGN cFileType = "doc"
                      cLoadFiles = cLoadFiles + cWordFile + ";".
        END.
        ELSE DO:
          cClipType = chOrg_Image:Picbuf:OafFormat.
          CASE cClipType:
            WHEN "0" THEN cFileType = "tif".
            WHEN "2" THEN cFileType = "bmp".
            WHEN "5" THEN cFileType = "pcx".
            WHEN "6" THEN cFileType = "jpg".
            OTHERWISE DO:
              DYNAMIC-FUNCTION("DoMessage",0,0,
                               IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                 "Clipboard inneholder ugyldig filformat" + CHR(10) +
                                 "(Gyldige formater: tif,jpg,bmp og pcx)"
                               ELSE 
                                 "Clipboard file has invalid format" + CHR(10) +
                                 "(Valid: tif,jpg,bmp og pcx)"
                               ,"","").
              iocFileNames = "".
              SESSION:SET-WAIT-STATE("").
              RETURN.
            END.
          END CASE.
          chOrg_Image:Picbuf:FILENAME = SESSION:TEMP-DIR + "clipboard_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY)) + STRING(TIME) + "." + cFileType.
          chOrg_Image:Picbuf:Store().
          bScale = Downscale(SESSION:TEMP-DIR + "clipboard_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY)) + STRING(TIME) + "." + cFileType).
        END.
      END.
    END.
  END.
  ELSE DO:
    bScale = Downscale(ENTRY(ix,iocFileNames,";")).
    IF bScale THEN DO:
      cFileType = SUBSTR(ENTRY(ix,iocFileNames,";"),R-INDEX(ENTRY(ix,iocFileNames,";"),".") + 1).
      IF CAN-DO("bmp,jpg,jpeg,pcx,tif,tiff",cFileType) THEN DO:
        chOrg_Image:Picbuf:CLEAR(2).
        ASSIGN chOrg_Image:Picbuf:FILENAME = ENTRY(ix,iocFileNames,";")
               chOrg_Image:Picbuf:AutoScale = TRUE.
        chOrg_Image:Picbuf:LOAD.
      END.
    END.
  END.
  IF CAN-DO("bmp,jpg,jpeg,pcx,tif,tiff",cFileType) AND bScale THEN DO:
    IF icLoadMode NE "small" THEN
      cLoadFiles = cLoadFiles + chOrg_Image:Picbuf:FILENAME + ";".
    
    chSmall_Image:CLEAR(2).
    ASSIGN iHeight = 200 * (chOrg_Image:Picbuf:YResolution / chOrg_Image:Picbuf:XResolution)
           iHeight = IF iHeight > 200 THEN 150 ELSE iHeight.
           chSmall_Image:AutoSize = TRUE.
           chSmall_Image:AssignMode = 3.
           chSmall_Image:ResizeMode = 2.
           chSmall_Image:INIT(chOrg_Image:Picbuf:ColorDepth,200,iHeight,16777215).
           chSmall_Image:ASSIGN(FALSE,chOrg_Image:Picbuf,FALSE,0).
    
           chSmall_Image:FILENAME = SESSION:TEMP-DIR + "small_" + 
                                    ENTRY(NUM-ENTRIES(chOrg_Image:Picbuf:FILENAME,"\"),chOrg_Image:Picbuf:FILENAME,"\").
    chSmall_Image:Store().
    cLoadFiles = cLoadFiles + chSmall_Image:FILENAME + ";".
  END.
  ELSE IF cFileType NE "doc" THEN cLoadFiles = cLoadFiles + ENTRY(ix,iocFileNames,";") + ";".
END.
iocFileNames = TRIM(cLoadFiles,";").
SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Downscale C-Win 
FUNCTION Downscale RETURNS LOGICAL
  ( INPUT icFileName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
FILE-INFO:FILE-NAME = icFileName.
IF FILE-INFO:FILE-SIZE > 10000 THEN RETURN TRUE. 
ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SaveUsingMSWord C-Win 
FUNCTION SaveUsingMSWord RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hWord     AS COM-HANDLE NO-UNDO.
DEF VAR cFileName AS CHAR NO-UNDO.

cFileName = SESSION:TEMP-DIR + "clipboard_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY)) + STRING(TIME) + ".doc".

CREATE "Word.Application":U hWord NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN "".

hWord:VISIBLE = FALSE.    
hWord:Documents:ADD().
NO-RETURN-VALUE hWord:Selection:Paste().
NO-RETURN-VALUE hWord:ActiveDocument:SaveAs(cFileName,,,,,,,,,,).
NO-RETURN-VALUE hWord:QUIT(FALSE,,).
RELEASE OBJECT hWord.
ASSIGN hWord = ?.
  
RETURN cFileName. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

