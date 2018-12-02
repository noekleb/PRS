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

DEFINE VARIABLE oJboxImage     AS uc.JboxImageUtil      NO-UNDO.

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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 57.8 BY 5.24.


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
         HEIGHT             = 5.71
         WIDTH              = 80
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
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



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
  DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Org_Image,Small_Image") NO-ERROR.
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"Org_Image,Small_Image") NO-ERROR.
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
 
    /*   ASSIGN chSmall_Image = chSmall_Image:Picbuf. */
           

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


oJboxImage = NEW uc.JboxImageUtil ().


SESSION:SET-WAIT-STATE("general").
DO ix = 1 TO NUM-ENTRIES(iocFileNames,";"):
  bScale = FALSE.
  
  IF iocFileNames = "clipboard" THEN 
  DO:
     oJboxImage:errornumber = 0.  
    IF CLIPBOARD:NUM-FORMATS > 0 THEN 
    DO:
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
    ELSE 
    DO:
      ASSIGN cClipText = CLIPBOARD:VALUE NO-ERROR.
      IF cClipText <> ? THEN 
      DO:
        cWordFile = SaveUsingMSWord().
        IF cWordFile = "" THEN 
        DO:
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
      
      ELSE 
      DO:
        oJboxImage:PasteClipboard(1) NO-ERROR.
        IF oJboxImage:errornumber <> 0 THEN 
        DO:
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
        ELSE 
        DO:
          
          cClipType = oJboxImage:OafFormat.
          CASE cClipType:
            WHEN "0"  THEN cFileType  = "tif".
            WHEN "2"  THEN cFileType  = "bmp".
            WHEN "5"  THEN cFileType  = "pcx".
            WHEN "6"  THEN cFileType  = "jpg".
            WHEN "10" THEN cFileType  = "png". 
            OTHERWISE 
            DO:
              DYNAMIC-FUNCTION("DoMessage",0,0,
                               IF DYNAMIC-FUNCTION("Scandinavian") THEN 
                                 "Clipboard inneholder ugyldig filformat" + CHR(10) +
                                 "(Gyldige formater: tif,jpg,bmp og pcx)"
                               ELSE 
                                 "Clipboard file has invalid format" + CHR(10) +
                                 "(Valid: tif,jpg,bmp,png and pcx)"
                               ,"","").
              iocFileNames = "".
              SESSION:SET-WAIT-STATE("").
              RETURN.
            END.
          END CASE.

          oJboxImage:FileNameIndex[1] = SESSION:TEMP-DIR + "clipboard_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY)) + STRING(TIME) + "." + cFileType.
          oJboxImage:SaveImageToFile(1).
          bScale = Downscale(SESSION:TEMP-DIR + "clipboard_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + "_" + STRING(DAY(TODAY)) + STRING(TIME) + "." + cFileType).
       
        END.
      END.
    END.
  END.
  
  ELSE DO:
    bScale = Downscale(ENTRY(ix,iocFileNames,";")).
    IF bScale THEN DO:
      cFileType = SUBSTR(ENTRY(ix,iocFileNames,";"),R-INDEX(ENTRY(ix,iocFileNames,";"),".") + 1).
      IF CAN-DO("bmp,jpg,jpeg,pcx,tif,tiff",cFileType) THEN 
         oJboxImage:loadImageFromFile(ENTRY(ix,iocFileNames,";"),1).
    END.
  END.

  IF CAN-DO("bmp,jpg,jpeg,pcx,tif,tiff,png",cFileType) AND bScale THEN 
  DO:
    IF icLoadMode NE "small" THEN
      cLoadFiles = cLoadFiles + oJboxImage:FileNameIndex[1] + ";".

    ASSIGN iHeight = 200 * (oJboxImage:YResolution / oJboxImage:XResolution)
           iHeight = IF iHeight > 200 THEN 150 ELSE iHeight.
    
    oJboxImage:CopyAndResizeImage(1,2, 200,iHeight,cFileType).
    oJboxImage:FileNameIndex[2] = SESSION:TEMP-DIR + "small_" + 
                                  ENTRY(NUM-ENTRIES(oJboxImage:FileNameIndex[1],"\"),oJboxImage:FileNameIndex[1],"\").      
    
    oJboxImage:SaveImageToFile(2).
    cLoadFiles = cLoadFiles + oJboxImage:FileNameIndex[2] + ";".
     
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

