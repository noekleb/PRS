&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Local Variable Definitions ---                                       */

DEF VAR bOK               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR cContext          AS CHAR   NO-UNDO.
DEF VAR cKey              AS CHAR   NO-UNDO.
DEF VAR cTmpFileNames     AS CHAR   NO-UNDO.
DEF VAR cOrgFileNames     AS CHAR   NO-UNDO.
DEF VAR cFileDates        AS CHAR   NO-UNDO.
DEF VAR cFileDescs        AS CHAR   NO-UNDO.
DEF VAR cFileIds          AS CHAR   NO-UNDO.
DEF VAR cFileTypes        AS CHAR   NO-UNDO.
DEF VAR hRect             AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hSmallPicture     AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hButton           AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hMenuOpen         AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hPopupMenu        AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hMenuEditText     AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hMenuDeleteImage  AS HANDLE NO-UNDO EXTENT 100.
DEF VAR hLargePicture     AS HANDLE NO-UNDO EXTENT 100.
DEF VAR iHorSize          AS INT    NO-UNDO INIT 130.
DEF VAR iVerSize          AS INT    NO-UNDO INIT 118.
DEF VAR iMinHorSize       AS INT    NO-UNDO INIT 130.
DEF VAR iMinVerSize       AS INT    NO-UNDO INIT 118.
DEF VAR cCurrSmallPicture AS CHAR   NO-UNDO.
DEF VAR hZoomIn           AS HANDLE NO-UNDO.
DEF VAR hZoomOut          AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectTB slZoom 
&Scoped-Define DISPLAYED-OBJECTS slZoom 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPictureDesc C-Win 
FUNCTION getPictureDesc RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LoadPictures C-Win 
FUNCTION LoadPictures RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ViewPictures C-Win 
FUNCTION ViewPictures RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-76
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 5.62.

DEFINE RECTANGLE rectTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY .95
     BGCOLOR 12 .

DEFINE VARIABLE slZoom AS INTEGER INITIAL 1 
     VIEW-AS SLIDER MIN-VALUE 1 MAX-VALUE 5 HORIZONTAL 
     TIC-MARKS NONE 
     SIZE 13 BY .76 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     slZoom AT ROW 1.14 COL 13.6 NO-LABEL
     RECT-76 AT ROW 2.52 COL 2
     rectTB AT ROW 1.1 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 80 BY 16 DROP-TARGET.


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
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
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
       FRAME DEFAULT-FRAME:HEIGHT           = 16
       FRAME DEFAULT-FRAME:WIDTH            = 80.

/* SETTINGS FOR RECTANGLE RECT-76 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       RECT-76:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON DROP-FILE-NOTIFY OF FRAME DEFAULT-FRAME
DO:
  DEF VAR cFileNames AS CHAR   NO-UNDO.

  DO ix = 1 TO SELF:NUM-DROPPED-FILES:
    cFileNames = cFileNames + SELF:GET-DROPPED-FILE(ix) + ";".
  END.
  IF cFileNames NE "" THEN LoadPictures(TRIM(cFileNames,";")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slZoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slZoom C-Win
ON VALUE-CHANGED OF slZoom IN FRAME DEFAULT-FRAME
DO:
  ASSIGN slZoom
         iHorSize = iMinHorSize
         iVerSize = iMinVerSize
         .
  IF slZoom > 1 THEN
    DO ix = 1 TO slZoom - 1:
      ASSIGN iHorSize = iHorSize * 1.3
             iVerSize = iVerSize * 1.3.
    END.

  ViewPictures().
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
  DO ix = 1 TO 100:
    IF VALID-HANDLE(hSmallPicture[ix]) THEN DO:
      DELETE PROCEDURE hSmallPicture[ix].
      DELETE PROCEDURE hLargePicture[ix] NO-ERROR.
    END.
    ELSE LEAVE.
  END.
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteImageRecord C-Win 
PROCEDURE DeleteImageRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocId        AS CHAR   NO-UNDO.
DEF INPUT PARAM icSmallPicture AS CHAR NO-UNDO.

DEF VAR iy AS INT NO-UNDO.

cCurrSmallPicture = icSmallPicture.

IF DYNAMIC-FUNCTION("DoMessage",0,4,"Slett bilde?","","") = 6 THEN DO:
  IF NOT DYNAMIC-FUNCTION("runproc","jbdoc_delete_one_doc.p",icDocId,?) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  ELSE DO:
    ASSIGN ix = LOOKUP(icDocId,cFileIds)
           cFileIds      = DYNAMIC-FUNCTION("DeleteListItem",cFileIds,"",ix,"")
           cTmpFileNames = DYNAMIC-FUNCTION("DeleteListItem",cTmpFileNames,"",ix,"|")
           cOrgFileNames = DYNAMIC-FUNCTION("DeleteListItem",cOrgFileNames,"",ix,"|")
           cFileDates    = DYNAMIC-FUNCTION("DeleteListItem",cFileDates,"",ix,"")
           cFileDescs    = DYNAMIC-FUNCTION("DeleteListItem",cFileDescs,"",ix,"|")
           cFileTypes    = DYNAMIC-FUNCTION("DeleteListItem",cFileTypes,"",ix,"")
           .
    IF VALID-HANDLE(hLargePicture[ix]) THEN
      DELETE PROCEDURE hLargePicture[ix].

    DO iy = ix + 1 TO 100 - ix:
      hLargePicture[iy - 1] = hLargePicture[iy].
    END.

    ViewPictures().
  END.
END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditImageTextRecord C-Win 
PROCEDURE EditImageTextRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icDocId  AS CHAR   NO-UNDO.
DEF INPUT PARAM icSmallPicture AS CHAR NO-UNDO.

DEF VAR ocReturn AS CHAR NO-UNDO.

RUN JBoxDEditPictureDesc.w (icDocId,OUTPUT ocReturn).

cCurrSmallPicture = icSmallPicture.

ASSIGN ENTRY(LOOKUP(icDocId,cFileIds),cFileDescs,"|") = ocReturn
       hButton[LOOKUP(icDocId,cFileIds)]:TOOLTIP = (IF ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|") BEGINS "small_" THEN
                                                     SUBSTR(ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),7)
                                                    ELSE ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"))
                                                 + " - " 
                                                 + ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cFileDates) + CHR(10) + ocReturn.
IF VALID-HANDLE(hLargePicture[LOOKUP(icDocId,cFileIds)]) THEN
  DYNAMIC-FUNCTION("setPictureDesc" IN hLargePicture[LOOKUP(icDocId,cFileIds)],ocReturn).


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
  DISPLAY slZoom 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectTB slZoom 
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
SUBSCRIBE TO "ShowLargePicture" ANYWHERE.

DO WITH FRAME {&FRAME-NAME}:
  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectTB:HANDLE,             /* Rectangle to define coordinates for toolbar */
                    "Bilde",                          /* Corresponding menu label - no menu if blank */
                    "LoadFromDisc;Last inn bilder;Last inn bilder og dokumenter fra disk;LoadFromDisc;bmp/open16e.bmp"
                  + ",ZoomOut;For&minsk;;ZoomOutRecord;bmp/zoomout.bmp"                                                    
                  + ",ZoomIn;For&størr;;ZoomInRecord;bmp/zoomin.bmp"                                                    
                    ,"").  

  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","ZoomOut").
  DYNAMIC-FUNCTION("setToolbar",hToolbar,"avail").

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFromDisc C-Win 
PROCEDURE LoadFromDisc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFileNames AS CHAR NO-UNDO.

PUBLISH "SuspendJBoxTimer" (YES).

cFileNames = DYNAMIC-FUNCTION("SelectFileNames","Alle filer|*.*",?,"Velg fil(er) for opplasting").
IF cFileNames NE "" THEN LoadPictures(cFileNames).

PUBLISH "SuspendJBoxTimer" (NO).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MailRecord C-Win 
PROCEDURE MailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("sendOutlookMail","brynjar@chemistry.no","","",
                 THIS-PROCEDURE:CURRENT-WINDOW:TITLE,"",
                 REPLACE(cTmpFileNames,",",";")).
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  
APPLY "entry" TO slZoom IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenImageRecord C-Win 
PROCEDURE OpenImageRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM iIx           AS INT  NO-UNDO.
DEF INPUT PARAM icSmallPicture AS CHAR NO-UNDO.

IF VALID-HANDLE(hLargePicture[iIx]) THEN DO:    
  hLargePicture[iIx]:CURRENT-WINDOW:WINDOW-STATE = 3.
  hLargePicture[iIx]:CURRENT-WINDOW:MOVE-TO-TOP().
  RETURN.
END.

cCurrSmallPicture = icSmallPicture.

DYNAMIC-FUNCTION("ViewDocs",cContext,cKey + "|" + LEFT-TRIM(ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),"small_"),FALSE,""). 

RUN JBoxLargePicture.w PERSIST SET hLargePicture[iIx].
DYNAMIC-FUNCTION("setParent" IN hLargePicture[iIx],THIS-PROCEDURE).
hLargePicture[iIx]:CURRENT-WINDOW:TITLE = LEFT-TRIM(ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),"small_")
                                        + " - " + ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cFileDates).

RUN InitializeObject IN hLargePicture[iIx].

DYNAMIC-FUNCTION("LoadPictureFile" IN hLargePicture[iIx],ENTRY(1,DYNAMIC-FUNCTION("getTmpDocFileNames"))). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetPictures C-Win 
PROCEDURE SetPictures :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icContext AS CHAR NO-UNDO. 
DEF INPUT PARAM icKey     AS CHAR NO-UNDO.

DEF VAR cWrongTypes AS CHAR NO-UNDO.

ASSIGN cContext       = icContext
       cKey           = icKey
       CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.

IF DYNAMIC-FUNCTION("ViewDocs",icContext,icKey + "|small_",FALSE,"") THEN DO WITH FRAME {&FRAME-NAME}: 

  ASSIGN 
    cTmpFileNames = DYNAMIC-FUNCTION("getTmpDocFileNames")
    cOrgFileNames = DYNAMIC-FUNCTION("getOrgDocFileNames")
    cFileDates    = DYNAMIC-FUNCTION("getDocFileDates")
    cFileDescs    = DYNAMIC-FUNCTION("getDocFileDescs")
    cFileIds      = DYNAMIC-FUNCTION("getDocFileIds")
    cFileTypes    = DYNAMIC-FUNCTION("getDocFileTypes")
    .

  DO ix = 1 TO NUM-ENTRIES(cFileTypes):
    IF NOT CAN-DO("bmp,jpg,jpeg,pcx,tif,tiff",ENTRY(ix,cFileTypes)) THEN
      cWrongTypes = cWrongTypes + STRING(ix) + ",".
  END.
  cWrongTypes = TRIM(cWrongTypes,",").
  DO ix = 1 TO NUM-ENTRIES(cWrongTypes):
    ASSIGN cFileIds      = DYNAMIC-FUNCTION("DeleteListItem",cFileIds,"",ix,"")
           cTmpFileNames = DYNAMIC-FUNCTION("DeleteListItem",cTmpFileNames,"",ix,"|")
           cOrgFileNames = DYNAMIC-FUNCTION("DeleteListItem",cOrgFileNames,"",ix,"|")
           cFileDates    = DYNAMIC-FUNCTION("DeleteListItem",cFileDates,"",ix,"")
           cFileDescs    = DYNAMIC-FUNCTION("DeleteListItem",cFileDescs,"",ix,"|")
           cFileTypes    = DYNAMIC-FUNCTION("DeleteListItem",cFileTypes,"",ix,"")
           .
  END.

  ViewPictures().

END.
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Feil ved lesing av bildefiler fra database. Kontakt systemadministrator","","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowLargePicture C-Win 
PROCEDURE ShowLargePicture :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM  icSmallPicture AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk           AS LOG NO-UNDO.

bOk = FALSE.
DO ix = 1 TO 100:
  IF hSmallPicture[ix] = SOURCE-PROCEDURE THEN DO:
    bok = TRUE.
    LEAVE.
  END.
END.
IF NOT bOk THEN RETURN.

IF VALID-HANDLE(hLargePicture[ix]) THEN DO:   
   hLargePicture[ix]:CURRENT-WINDOW:WINDOW-STATE = 3.
   hLargePicture[ix]:CURRENT-WINDOW:MOVE-TO-TOP().
   obOk = YES.
   RETURN.
END.

cCurrSmallPicture = icSmallPicture.

DYNAMIC-FUNCTION("ViewDocs",cContext,cKey + "|" + 
                 (IF ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|") BEGINS "small_" THEN
                   SUBSTR(ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),7)
                  ELSE
                   ENTRY(LOOKUP(icSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|")),
                 FALSE,""). 

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  IF NOT VALID-HANDLE(hLargePicture) THEN
    RUN JBoxLargePicture.w PERSIST SET hLargePicture[ix].
&ELSE
  PUBLISH "StartChildWindow" ("JBoxLargePicture.w",
                              (IF ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|") BEGINS "small_" THEN
                                 SUBSTR(ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"),7)
                               ELSE ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cOrgFileNames,"|"))
                            + " - " + ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cFileDates),
                              THIS-PROCEDURE,
                              YES,
                              OUTPUT hLargePicture[ix]).  
&ENDIF

DYNAMIC-FUNCTION("LoadPictureFile" IN hLargePicture[ix],ENTRY(1,DYNAMIC-FUNCTION("getTmpDocFileNames"),"|")). 

obOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomInRecord C-Win 
PROCEDURE ZoomInRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN iHorSize = iHorSize * 1.3
       iVerSize = iVerSize * 1.3
       slZoom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(slZoom:SCREEN-VALUE) + 1)
       .
ViewPictures().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ZoomOutRecord C-Win 
PROCEDURE ZoomOutRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN iHorSize = iHorSize / 1.3
       iVerSize = iVerSize / 1.3
       slZoom:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(INTEGER(slZoom:SCREEN-VALUE) - 1)
       .

ViewPictures().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPictureDesc C-Win 
FUNCTION getPictureDesc RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN ENTRY(LOOKUP(cCurrSmallPicture,cTmpFileNames,"|"),cFileDescs,"|").  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LoadPictures C-Win 
FUNCTION LoadPictures RETURNS LOGICAL
  ( INPUT icFileNames AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF INDEX(icFileNames,",") > 0 THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,"Filnavn kan ikke inneholde komma","","").
  RETURN NO.
END.
RUN JBoxPictureScale.w (INPUT-OUTPUT icFileNames,"").

DYNAMIC-FUNCTION("LoadDocs",icFileNames,
                 cContext,cKey,"").
    
RUN SetPictures (cContext,cKey).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ViewPictures C-Win 
FUNCTION ViewPictures RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iy         AS INT    NO-UNDO.
DEF VAR iColumns   AS INT    NO-UNDO.
DEF VAR iRows      AS INT    NO-UNDO.
DEF VAR iCount     AS INT    NO-UNDO.

DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

DO WITH FRAME {&FRAME-NAME}:
  CURRENT-WINDOW = THIS-PROCEDURE:CURRENT-WINDOW.
  SESSION:SET-WAIT-STATE("general").

  /* Set the slider and zoom buttons: */
  IF INTEGER(slZoom:SCREEN-VALUE) = 1 THEN
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","ZoomOut").
  ELSE IF INTEGER(slZoom:SCREEN-VALUE) = 5 THEN
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","ZoomIn").
  ELSE
    DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").

  DYNAMIC-FUNCTION("setToolbar",hToolbar,"avail").


  /* Clean up previous view first: */
  DO ix = 1 TO 100:
    IF VALID-HANDLE(hRect[ix]) THEN DO:
      DELETE OBJECT hRect[ix].
      DELETE OBJECT hButton[ix].
      DELETE OBJECT hMenuOpen[ix].
      DELETE OBJECT hMenuEditText[ix].
      DELETE OBJECT hMenuDeleteImage[ix].
      DELETE OBJECT hPopupMenu[ix].
      DELETE PROCEDURE hSmallPicture[ix].
    END.
    ELSE LEAVE.
  END.

  ASSIGN 
    iColumns      = MIN(NUM-ENTRIES(cTmpFileNames,"|"),(SESSION:WIDTH-PIXELS - 100) / (iHorSize + 5))
    iRows         = NUM-ENTRIES(cTmpFileNames,"|") / iColumns + (IF NUM-ENTRIES(cTmpFileNames,"|") / iColumns > 1 AND 
                                                                NUM-ENTRIES(cTmpFileNames,"|") MOD iColumns < 5 THEN 1 ELSE 0)
    THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS  = iColumns * (iHorSize + 5) + 20
    THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS = MIN(SESSION:HEIGHT-PIXELS - 70,iRows * (iVerSize + 17)) + 22
    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS    = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS
    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS   = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS
    FRAME {&FRAME-NAME}:WIDTH-PIXELS            = THIS-PROCEDURE:CURRENT-WINDOW:WIDTH-PIXELS * 2
    FRAME {&FRAME-NAME}:HEIGHT-PIXELS           = THIS-PROCEDURE:CURRENT-WINDOW:HEIGHT-PIXELS * 2
    NO-ERROR.

  DO iy = 1 TO iRows:
    IF iy + (iy - 1) * (iVerSize + 5) + iVerSize GT FRAME {&FRAME-NAME}:HEIGHT-PIXELS THEN LEAVE.
    DO ix = 1 TO iColumns:
      iCount = iCount + 1.
      CREATE RECTANGLE hRect[iCount]
      ASSIGN FRAME            = FRAME {&FRAME-NAME}:HANDLE
             X                = ix + (ix - 1) * (iHorSize + 5)
             Y                = iy + (iy - 1) * (iVerSize + 15) + 22
             WIDTH-PIXELS     = iHorSize
             HEIGHT-PIXELS    = iVerSize
             VISIBLE          = TRUE
             TOOLTIP          = ENTRY(iCount,cOrgFileNames,"|")
             .
      RUN JBoxSmallPicture.w PERSIST SET hSmallPicture[iCount].
      RUN InitializeObject IN hSmallPicture[iCount] (hRect[iCount]).
      DYNAMIC-FUNCTION("LoadPictureFile" IN hSmallPicture[iCount],ENTRY(iCount,cTmpFileNames,"|")).
      DYNAMIC-FUNCTION("SetPictureDesc"  IN hSmallPicture[iCount],ENTRY(iCount,cFileDescs,"|")).

      CREATE BUTTON hButton[iCount]
      ASSIGN FRAME            = FRAME {&FRAME-NAME}:HANDLE
/*              FLAT-BUTTON      = TRUE */
             X                = ix + (ix - 1) * (iHorSize + 5)
             Y                = iy * iVerSize + 2 + (iy - 1) * 15 + 22
             FONT             = 3
             WIDTH-PIXELS     = iHorSize
             HEIGHT-PIXELS    = 14
             VISIBLE          = TRUE
             SENSITIVE        = TRUE
             TOOLTIP          = (IF ENTRY(iCount,cOrgFileNames,"|") BEGINS "small_" THEN
                                  SUBSTR(ENTRY(iCount,cOrgFileNames,"|"),7)
                                 ELSE
                                  ENTRY(iCount,cOrgFileNames,"|"))
                                + " - " + ENTRY(iCount,cFileDates) + CHR(10) + ENTRY(iCount,cFileDescs,"|")
             LABEL            = (IF ENTRY(iCount,cOrgFileNames,"|") BEGINS "small_" THEN
                                  SUBSTR(ENTRY(iCount,cOrgFileNames,"|"),7)
                                 ELSE
                                  ENTRY(iCount,cOrgFileNames,"|"))
       TRIGGERS:
         ON CHOOSE PERSISTENT RUN EditImageTextRecord IN THIS-PROCEDURE (ENTRY(iCount,cFileIds),ENTRY(iCount,cTmpFileNames,"|")).
       END TRIGGERS.
           

       CREATE MENU hPopupMenu[iCount]
              ASSIGN POPUP-ONLY = TRUE.

       hButton[iCount]:POPUP-MENU = hPopupMenu[iCount].

       CREATE MENU-ITEM hMenuOpen[iCount]
             ASSIGN PARENT     = hPopupMenu[iCount]
                    LABEL      = "Åpne bilde"
                    NAME       = "OpenImage" 
                    TRIGGERS:
                      ON CHOOSE PERSISTENT RUN OpenImageRecord IN THIS-PROCEDURE (iCount,ENTRY(iCount,cTmpFileNames,"|")).
                    END TRIGGERS.

       CREATE MENU-ITEM hMenuEditText[iCount]
             ASSIGN PARENT     = hPopupMenu[iCount]
                    LABEL      = "Rediger bildetekst"
                    NAME       = "EditImageText" 
                    TRIGGERS:
                      ON CHOOSE PERSISTENT RUN EditImageTextRecord IN THIS-PROCEDURE (ENTRY(iCount,cFileIds),ENTRY(iCount,cTmpFileNames,"|")).
                    END TRIGGERS.

       CREATE MENU-ITEM hMenuDeleteImage[iCount]
             ASSIGN PARENT     = hPopupMenu[iCount]
                    LABEL      = "Slett bilde"
                    NAME       = "DeleteImage" 
                    TRIGGERS:
                      ON CHOOSE PERSISTENT RUN DeleteImageRecord IN THIS-PROCEDURE (ENTRY(iCount,cFileIds),ENTRY(iCount,cTmpFileNames,"|")).
                    END TRIGGERS.

      IF iCount = NUM-ENTRIES(cTmpFileNames,"|") THEN LEAVE.
    END.
    IF iCount = NUM-ENTRIES(cTmpFileNames,"|") THEN LEAVE.
  END.

  SESSION:SET-WAIT-STATE("").
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.
  

DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

