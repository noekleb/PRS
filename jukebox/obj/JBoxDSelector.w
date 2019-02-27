&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE PureABLWin 0

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEF VAR ihParent             AS HANDLE NO-UNDO.

  DEF VAR iiBatchSize          AS INT    NO-UNDO INIT 200.

  DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "Dokument;iDokNr;cDokType".
  DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where can-do('80,82',string(iSakId))".

/*   DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "Dokument;iDokNr;cDokType,Sak;cSaksNr;cSaksTittel".          */
/*   DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true,first sak where can-do('80,82',string(iSakId))". */

/*   DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "HuvGr;Hg;HgBeskr,Avdeling;AvdelingNr;AvdelingNavn". */
/*   DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true, first Avdeling OF HuvGr NO-LOCK".       */

  DEF VAR iocSelectedRows      AS CHAR   NO-UNDO.
  DEF VAR icOutputFields       AS CHAR   NO-UNDO.
  DEF VAR iocOutputValues      AS CHAR   NO-UNDO.

  DEF VAR icServerSelectProc   AS CHAR   NO-UNDO. /* INIT "=test_updstrtstr.p".   */
  DEF VAR icServerDeSelectProc AS CHAR   NO-UNDO INIT "".

  DEF VAR obOK                 AS LOG    NO-UNDO.

/*   iocSelectedRows = "0x000440cc,0x000e09ec,0x00043ee1,0x000e09ed".  */

/*   DEF VAR icSourceTableAndFlds AS CHAR   NO-UNDO INIT "Kunde;KundeNr;Navn;Adresse1;Telefon;MobilTlf;RegistrertDato;ButikkNr;!TypeId,KundeType;Beskrivelse". */
/*   DEF VAR icSourceQuery        AS CHAR   NO-UNDO INIT "where true,first KundeType of Kunde no-lock".                                                        */
/*   DEF VAR icServerSelectProc   AS CHAR   NO-UNDO INIT "selectKunde.p".                                                                                      */
&ELSE
  DEF INPUT PARAM ihParent               AS HANDLE NO-UNDO.

  DEF INPUT PARAM iiBatchSize            AS INT  NO-UNDO.
  DEF INPUT PARAM icSourceTableAndFlds   AS CHAR NO-UNDO.
  DEF INPUT PARAM icSourceQuery          AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocSelectedRows AS CHAR NO-UNDO.
  DEF INPUT PARAM icOutputFields  AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocOutputValues AS CHAR NO-UNDO.

  DEF INPUT PARAM icServerSelectProc     AS CHAR NO-UNDO.
  DEF INPUT PARAM icServerDeSelectProc   AS CHAR NO-UNDO.

  DEF OUTPUT PARAM obOK                  AS LOG NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEF VAR ix                 AS INT    NO-UNDO.
DEF VAR bOK                AS LOG    NO-UNDO.

DEF VAR cSelRowidList      AS CHAR NO-UNDO.
DEF VAR cDeSelRowidList    AS CHAR NO-UNDO.

DEF VAR hBrwSource         AS HANDLE NO-UNDO.
DEF VAR hBrwTarget         AS HANDLE NO-UNDO.
DEF VAR hBuffSource        AS HANDLE NO-UNDO.
DEF VAR hBuffTarget        AS HANDLE NO-UNDO.

DEF VAR httTarget          AS HANDLE NO-UNDO.
DEF VAR cSortColumn        AS CHAR   NO-UNDO.
DEF VAR bDesc              AS LOG    NO-UNDO.
DEF VAR hFilToolBar        AS HANDLE NO-UNDO.
DEF VAR hWinToolBar        AS HANDLE NO-UNDO.

DEF VAR iSelectIndex       AS INT NO-UNDO.
DEF VAR iDeSelectIndex     AS INT NO-UNDO.
DEF VAR oSelector          AS JBoxDynSelector NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDeSelect rectBrwSource rectBrwTarget ~
btnDeSelectAll rectSearchSource Btn_OK btnSelect Btn_Cancel btnSelectAll 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD adjustBrowseHeight Dialog-Frame 
FUNCTION adjustBrowseHeight RETURNS HANDLE
  ( INPUT iiDeltaY AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOutputData Dialog-Frame 
FUNCTION getOutputData RETURNS CHARACTER
  ( INPUT icOutputFields AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectedRowids Dialog-Frame 
FUNCTION setSelectedRowids RETURNS LOGICAL
  ( INPUT icSelRowidList AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UndoSelection Dialog-Frame 
FUNCTION UndoSelection RETURNS LOGICAL
  ( INPUT icUndoType AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDeSelect 
     IMAGE-UP FILE "bmp/vcrrew.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnselectall 3" 
     SIZE 4.6 BY 1.1 TOOLTIP "Fjern valgt(e)".

DEFINE BUTTON btnDeSelectAll 
     IMAGE-UP FILE "bmp/vcrall.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnselectall 2" 
     SIZE 4.6 BY 1.1 TOOLTIP "Fjern alle".

DEFINE BUTTON btnSelect 
     IMAGE-UP FILE "bmp/vcrfwd.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "btnselectall 4" 
     SIZE 4.6 BY 1.1 TOOLTIP "Legg til valgt(e)".

DEFINE BUTTON btnSelectAll 
     IMAGE-UP FILE "bmp/vcrnon.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 2" 
     SIZE 4.6 BY 1.1 TOOLTIP "Legg til alle".

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE rectBrwSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 13.57.

DEFINE RECTANGLE rectBrwTarget
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.6 BY 13.57.

DEFINE RECTANGLE rectSearchSource
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnDeSelect AT ROW 9.19 COL 50.2
     btnDeSelectAll AT ROW 10.33 COL 50.2
     Btn_OK AT ROW 16.24 COL 72.6
     btnSelect AT ROW 8.05 COL 50.2
     Btn_Cancel AT ROW 16.24 COL 88.6
     btnSelectAll AT ROW 6.91 COL 50.2
     rectBrwSource AT ROW 2.43 COL 1.8
     rectBrwTarget AT ROW 2.43 COL 56
     rectSearchSource AT ROW 1.19 COL 2
     SPACE(86.99) SKIP(15.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeSelect Dialog-Frame
ON CHOOSE OF btnDeSelect IN FRAME Dialog-Frame /* btnselectall 3 */
DO:
  RUN DeSelectRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeSelectAll Dialog-Frame
ON CHOOSE OF btnDeSelectAll IN FRAME Dialog-Frame /* btnselectall 2 */
DO:
  RUN DeSelectAllRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelect Dialog-Frame
ON CHOOSE OF btnSelect IN FRAME Dialog-Frame /* btnselectall 4 */
DO:
  RUN SelectRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSelectAll Dialog-Frame
ON CHOOSE OF btnSelectAll IN FRAME Dialog-Frame /* Button 2 */
DO:
  RUN SelectAllRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:  
  DEF VAR iReturn   AS INT NO-UNDO INIT 6.
  DEF VAR hLastWidget AS HANDLE NO-UNDO.

  hLastWidget = DYNAMIC-FUNCTION("getLinkedObject",hBrwTarget,"browseoverlay","from").

  IF PROGRAM-NAME(2) MATCHES "*JBoxSelector.w" AND VALID-HANDLE(hLastWidget) AND hLastWidget:TYPE = "fill-in" THEN
    APPLY "return" TO hLastWidget.
  
  JBoxServerAPI:Instance:SelectorDeselectRowidList = TRIM(cDeSelRowidList,",").
  
  IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"getSelectorAttributes") THEN
    RUN getSelectorAttributes IN ihParent(hBrwSource,hBrwTarget,TRIM(cDeSelRowidList,","),OUTPUT iReturn).

  IF iReturn = 2 THEN RETURN NO-APPLY.

  IF icOutputFields NE "" THEN
    iocOutputValues = getOutputData(icOutputFields).

  ASSIGN cSelRowidList   = TRIM(cSelRowidList,",")
         cDeSelRowidList = TRIM(cDeSelRowidList,",")
         iocSelectedRows = cSelRowidList
         obOK            = TRUE
         .
  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.


  RUN InitWindow.
  IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"mySelectorObject") THEN DO:
    oSelector = NEW JBoxDynSelector(ihParent,THIS-PROCEDURE,hBrwSource,hBrwTarget,?).
    RUN mySelectorObject IN ihParent (oSelector).
  END.  
  ELSE IF VALID-HANDLE(ihParent) AND CAN-DO(ihParent:INTERNAL-ENTRIES,"setSelectorAttributes") THEN
    RUN setSelectorAttributes IN SOURCE-PROCEDURE(hBrwSource,hBrwTarget).

  FRAME {&FRAME-NAME}:HIDDEN = FALSE.

  ON 'default-action':U OF hBrwSource
  DO:
    APPLY "choose" TO btnSelect IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
  ON 'default-action':U OF hBrwTarget
  DO:
    APPLY "choose" TO btnDeSelect IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeSelectAllRecord Dialog-Frame 
PROCEDURE DeSelectAllRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrwTarget:QUERY:IS-OPEN THEN RETURN.

hBrwTarget:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwTarget:QUERY:QUERY-OFF-END:
  ASSIGN cSelRowidList = REPLACE(cSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
         cSelRowidList = REPLACE(cSelRowidList,",,",",")
         .

  IF NOT CAN-DO(cDeSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
    cDeSelRowidList = cDeSelRowidList + hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".

  IF icServerDeSelectProc = "" THEN 
    hBuffTarget:BUFFER-DELETE().

  hBrwTarget:QUERY:GET-NEXT().
END.

IF icServerDeSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cDeSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffTarget:NAME,
                  icServerDeSelectProc,
                  "",
                  ENTRY(ix,cDeSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.

RUN OpenQuerySource.
RUN OpenQueryTarget.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeSelectRecord Dialog-Frame 
PROCEDURE DeSelectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrwTarget:NUM-SELECTED-ROWS > 0 THEN 
  DO ix = 1 TO hBrwTarget:NUM-SELECTED-ROWS:
    bOk = hBrwTarget:FETCH-SELECTED-ROW(ix).
    IF bOK THEN DO:
      ASSIGN cSelRowidList = REPLACE(cSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
             cSelRowidList = REPLACE(cSelRowidList,",,",",")
             .
  
      IF NOT CAN-DO(cDeSelRowidList,hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
        cDeSelRowidList = cDeSelRowidList + hBuffTarget:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".
  
      IF icServerDeSelectProc = "" THEN 
        hBuffTarget:BUFFER-DELETE().
    END.
  END.
ELSE RETURN.

IF icServerDeSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cDeSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffTarget:NAME,
                  icServerDeSelectProc,
                  "",
                  ENTRY(ix,cDeSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.

RUN OpenQuerySource.
RUN OpenQueryTarget.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE btnDeSelect rectBrwSource rectBrwTarget btnDeSelectAll 
         rectSearchSource Btn_OK btnSelect Btn_Cancel btnSelectAll 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expandSelectorDialog Dialog-Frame
PROCEDURE expandSelectorDialog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM iiDeltaX AS INT NO-UNDO.
DEF INPUT PARAM iiDeltaY AS INT NO-UNDO.

IF ihBrowse NE hBrwSource THEN RETURN.


ASSIGN FRAME {&FRAME-NAME}:WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS + iiDeltaX
       FRAME {&FRAME-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS + iiDeltaY
       hBrwSource:HEIGHT-PIXELS = hBrwSource:HEIGHT-PIXELS + iiDeltaY
       hBrwTarget:HEIGHT-PIXELS = hBrwTarget:HEIGHT-PIXELS + iiDeltaY
       hBrwTarget:X = hBrwTarget:X + iiDeltaX / 2
       hBrwSource:WIDTH-PIXELS = hBrwSource:WIDTH-PIXELS + iiDeltaX / 2
       hBrwTarget:WIDTH-PIXELS = hBrwTarget:WIDTH-PIXELS + iiDeltaX / 2
       btnDeSelect:X = btnDeSelect:X + iiDeltaX / 2
       btnDeSelectAll:X = btnDeSelectAll:X + iiDeltaX / 2
       btnSelect:X = btnSelect:X + iiDeltaX / 2
       btnSelectAll:X = btnSelectAll:X + iiDeltaX / 2
       Btn_Cancel:X = Btn_Cancel:X + iiDeltaX 
       Btn_OK:X = Btn_OK:X + iiDeltaX
       .
       

END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cInitSourceSort    AS CHAR NO-UNDO.
DEF VAR hSearchFieldSource AS HANDLE NO-UNDO.
DEF VAR cJoin              AS CHAR NO-UNDO.
DEF VAR cBaseQuerySource   AS CHAR NO-UNDO.
DEF VAR cBaseQueryTarget   AS CHAR NO-UNDO.
DEF VAR iy                 AS INT  NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  cSelRowidList = IF iocSelectedRows NE "" THEN iocSelectedRows + "," ELSE "".

  IF NUM-ENTRIES(icSourceTableAndFlds,"@1") = 2 THEN DO:
    cInitSourceSort = ENTRY(1,icSourceTableAndFlds,"@1").
    cInitSourceSort = ENTRY(1,ENTRY(NUM-ENTRIES(cInitSourceSort,";"),cInitSourceSort,";"),"|").
    cSortColumn = cInitSourceSort.
    cInitSourceSort = "SORT|" + cInitSourceSort.
  END.  
  ELSE   IF NUM-ENTRIES(ENTRY(1,icSourceTableAndFlds),";") > 1 AND NOT ENTRY(2,ENTRY(1,icSourceTableAndFlds),";") BEGINS "!" THEN
    ASSIGN cInitSourceSort = "SORT|" + ENTRY(2,ENTRY(1,icSourceTableAndFlds),";")
           cSortColumn     = ENTRY(1,ENTRY(2,ENTRY(1,icSourceTableAndFlds),";"),"|")
           .
  ELSE DO: 
    rectSearchSource:HIDDEN = TRUE.
    IF NUM-ENTRIES(ENTRY(1,icSourceTableAndFlds),";") > 1 AND ENTRY(2,ENTRY(1,icSourceTableAndFlds),";") = "!" THEN
      icSourceTableAndFlds = REPLACE(icSourceTableAndFlds,";!;",";").
  END.  

  DO ix = 1 TO LENGTH(icSourceQuery):
    IF SUBSTR(icSourceQuery,ix,6) = "first " OR
       SUBSTR(icSourceQuery,ix,5) = "last " OR
       SUBSTR(icSourceQuery,ix,5) = "each " 
       THEN DO:
      DO iy = ix TO ix - 10 BY -1:
        IF SUBSTR(icSourceQuery,iy,1) = "," THEN LEAVE.
      END.
      LEAVE.
    END.
  END.
  
  IF iy > 1 THEN 
    ASSIGN cBaseQuerySource = SUBSTR(icSourceQuery,1,iy - 1)
           cJoin            = SUBSTR(icSourceQuery,iy)
           .
  ELSE cBaseQuerySource = icSourceQuery.

  /* Define the browsers: */
/*   IF NUM-ENTRIES(icSourceQuery) > 1 THEN DO:                                                                  */
/*     IF (INDEX(icSourceQuery,"can-do") > 0 OR INDEX(icSourceQuery,"dec(") > 0) AND                             */
/*        INDEX(icSourceQuery,"(") < INDEX(icSourceQuery,",") THEN                                               */
/*       ASSIGN cBaseQuerySource = SUBSTR(icSourceQuery,1,INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")) - 1) */
/*              cJoin            = IF INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")) > 0 THEN                 */
/*                                   SUBSTR(icSourceQuery,INDEX(icSourceQuery,",",INDEX(icSourceQuery,")")))     */
/*                                 ELSE ""                                                                       */
/*                                 .                                                                             */
/*     ELSE                                                                                                      */
/*       ASSIGN cBaseQuerySource = ENTRY(1,icSourceQuery)                                                        */
/*              cJoin            = SUBSTR(icSourceQuery,INDEX(icSourceQuery,","))                                */
/*              .                                                                                                */
/*   END.                                                                                                        */
/*   ELSE cBaseQuerySource = icSourceQuery.                                                                      */

  IF NUM-ENTRIES(icSourceTableAndFlds) > 1 THEN
    icSourceTableAndFlds = ENTRY(1,icSourceTableAndFlds) + ";+!iJBoxSelectIndex|INTEGER;+!iJBoxDeSelectIndex|INTEGER" + SUBSTR(icSourceTableAndFlds,INDEX(icSourceTableAndFlds,",")).
  ELSE 
    icSourceTableAndFlds = icSourceTableAndFlds + ";+!iJBoxSelectIndex|INTEGER;+!iJBoxDeSelectIndex|INTEGER".

/*   IF cInitSourceSort NE "" THEN DO:                                                                                                          */
/*     DYNAMIC-FUNCTION("setAttribute",rectBrwSource:HANDLE,"1stSortColumn",cSortColumn).                                                       */
/*     DYNAMIC-FUNCTION("setAttribute",rectBrwSource:HANDLE,"1stDbSortColumn",ENTRY(1,ENTRY(1,icSourceTableAndFlds),";") + "." + cSortColumn).  */
/*     DYNAMIC-FUNCTION("setAttribute",rectBrwSource:HANDLE,"querySort",ENTRY(1,ENTRY(1,icSourceTableAndFlds),";") + "." + cSortColumn).        */
/*   END.                                                                                                                                       */

  hBrwSource = DYNAMIC-FUNCTION("NewBrowse",           /* Create a browse object */
                    rectBrwSource:HANDLE,              /* Rectangle to define coordinates for browse */
                    iiBatchSize,                       /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",   /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    icSourceTableAndFlds,              /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    cBaseQuerySource + 
                       cJoin,
                    cInitSourceSort
                    ).    
  ASSIGN hBrwSource:NAME = "brwSource"
         hBuffSource = hBrwSource:QUERY:GET-BUFFER-HANDLE(1)
         .
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"basequery",cBaseQuerySource).
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querywhere","").
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"uselocaldata","yes").
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"windowsbrowse","yes").

  IF cInitSourceSort NE "" THEN DO:
    hSearchFieldSource = DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchSource:HANDLE,hBrwSource,1).
    DYNAMIC-FUNCTION("CreateObjectLink",hSearchFieldSource,hBrwSource).
  END.

  hBrwTarget = DYNAMIC-FUNCTION("NewBrowse",           /* Create a browse object */
                    rectBrwTarget:HANDLE,              /* Rectangle to define coordinates for browse */
                    10000,                             /* Rows to batch */
                    "MULTIPLE,NUM-LOCKED-COLUMNS|1",   /* Browse properties, ie MULTIPLE,NUM-LOCKED-COLUMNS, etc */
                    icSourceTableAndFlds,              /* Buffers and fields: <buffer1>;<field1>;<field2>..,<buffer2>;.. No fields gives all*/
                    "WHERE false" + cJoin,
                    cInitSourceSort
                    ).
  ASSIGN hBrwTarget:NAME = "brwTarget"
         hBuffTarget = hBrwTarget:QUERY:GET-BUFFER-HANDLE(1)
         .

  DYNAMIC-FUNCTION("setAttribute",hBrwTarget,"querywhere","").

  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"nextTabItem",STRING(hBrwTarget)).
  DYNAMIC-FUNCTION("setAttribute",hBrwTarget,"nextTabItem",STRING(Btn_OK:HANDLE)).

  IF iocSelectedRows NE "" THEN DO:
    DO ix = 1 TO NUM-ENTRIES(iocSelectedRows):
      bOk = hBuffSource:FIND-FIRST("where rowident1 = '" + ENTRY(ix,iocSelectedRows) + "'") NO-ERROR.
      IF bOK THEN DO:
        hBuffTarget:BUFFER-CREATE().
        hBuffTarget:BUFFER-COPY(hBuffSource).
      END.
    END.
    cSelRowidList = iocSelectedRows + ",".
    RUN OpenQuerySource.
    RUN OpenQueryTarget.
  END.

  LocalTranslation().

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

  APPLY "entry" TO hBrwSource.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffEnd Dialog-Frame 
PROCEDURE OffEnd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF icServerSelectProc = "" AND DYNAMIC-FUNCTION("GetCurrentObject") = hBrwTarget THEN
  RETURN.
ELSE RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffHome Dialog-Frame 
PROCEDURE OffHome :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF icServerSelectProc = "" AND DYNAMIC-FUNCTION("GetCurrentObject") = hBrwTarget THEN
  RETURN.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuerySource Dialog-Frame 
PROCEDURE OpenQuerySource :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF TRIM(cSelRowidList,",") NE "" THEN
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"queryfilter",
                  " AND NOT CAN-DO('" + TRIM(cSelRowidList,",") + "',Rowident1)").

ELSE 
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"queryfilter","").

DYNAMIC-FUNCTION("setCurrentObject",hBrwSource).
RUN OpenQuery.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryTarget Dialog-Frame 
PROCEDURE OpenQueryTarget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cModSortColumn AS CHAR NO-UNDO.

IF icServerSelectProc = "" THEN DO:

  IF cSortColumn NE "" AND SUBSTR(cSortColumn,LENGTH(cSortColumn)) = "]" THEN
    cModSortColumn = "jbextent_" + RIGHT-TRIM(SUBSTR(cSortColumn,R-INDEX(cSortColumn,"[") + 1),"]") + "_" + SUBSTR(cSortColumn,1,R-INDEX(cSortColumn,"[") - 1).
  ELSE cModSortColumn = cSortColumn.

  hBrwTarget:QUERY:QUERY-PREPARE("FOR EACH " + hBuffTarget:NAME + 
                                 (IF cSortColumn NE "" THEN " BY " + cModSortColumn +
                                    (IF bDesc THEN " DESC" ELSE "")
                                  ELSE "")).
  hBrwTarget:QUERY:QUERY-OPEN().
END.
ELSE DO:
  DYNAMIC-FUNCTION("SetCurrentObject",hBrwTarget).
  RUN OpenQuery.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectAllRecord Dialog-Frame 
PROCEDURE SelectAllRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iNewBatch AS INT NO-UNDO.

IF NOT hBrwSource:QUERY:IS-OPEN THEN RETURN.


IF DYNAMIC-FUNCTION("getAttribute",hBrwSource,"lastrowid") = "" THEN DO:
  RUN dAskForQueryBatch.w (OUTPUT iNewBatch).
  IF iNewBatch NE 0 THEN DO:
    bDesc = IF DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querydesc") = "desc" THEN TRUE ELSE FALSE.
    DYNAMIC-FUNCTION("setAttribute",hBrwSource,"rowstobatch",STRING(iNewBatch)).

    iNewBatch = DYNAMIC-FUNCTION("fillBrowse",hBrwSource,
                        iNewBatch,
                        INT(DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querystart")),
                        DYNAMIC-FUNCTION("getAttribute",hBrwSource,"buffersandfields"),
                        DYNAMIC-FUNCTION("getAttribute",hBrwSource,"basequery") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrwSource,"queryfilter") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querywhere") + 
                          DYNAMIC-FUNCTION("getAttribute",hBrwSource,"queryjoin"),
                        DYNAMIC-FUNCTION("getAttribute",hBrwSource,"querysort"),
                        bDesc).
    DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querystart",STRING(iNewBatch)).
  END.
  ELSE RETURN.
END.

iSelectIndex = iSelectIndex + 1.
hBrwSource:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwSource:QUERY:QUERY-OFF-END:
  IF icServerSelectProc = "" THEN DO:
    hBuffTarget:BUFFER-CREATE().
    hBuffTarget:BUFFER-COPY(hBuffSource).
    hBuffTarget:BUFFER-FIELD("iJBoxSelectIndex"):BUFFER-VALUE = iSelectIndex.
  END.

  ASSIGN cDeSelRowidList = REPLACE(cDeSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
         cDeSelRowidList = REPLACE(cDeSelRowidList,",,",",")
         .

  IF NOT CAN-DO(cSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
    cSelRowidList = cSelRowidList + hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".


  hBrwSource:QUERY:GET-NEXT().
END.
IF icServerSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffSource:NAME,
                  icServerSelectProc,
                  "",
                  ENTRY(ix,cSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.

RUN OpenQuerySource.
RUN OpenQueryTarget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectRecord Dialog-Frame 
PROCEDURE SelectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrwSource:NUM-SELECTED-ROWS > 0 THEN DO:
  iSelectIndex = iSelectIndex + 1.
  DO ix = 1 TO hBrwSource:NUM-SELECTED-ROWS:
    bOk = hBrwSource:FETCH-SELECTED-ROW(ix).
    IF bOK THEN DO:
      IF icServerSelectProc = "" THEN DO:
        hBuffTarget:BUFFER-CREATE().
        hBuffTarget:BUFFER-COPY(hBuffSource).
        hBuffTarget:BUFFER-FIELD("iJBoxSelectIndex"):BUFFER-VALUE = iSelectIndex.
      END.
  
      ASSIGN cDeSelRowidList = REPLACE(cDeSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE,"")
             cDeSelRowidList = REPLACE(cDeSelRowidList,",,",",")
             .
  
      IF NOT CAN-DO(cSelRowidList,hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE) THEN
        cSelRowidList = cSelRowidList + hBuffSource:BUFFER-FIELD("Rowident1"):BUFFER-VALUE + ",".
    END.
  END.
END.
ELSE RETURN.

IF icServerSelectProc NE "" THEN DO:
  DO ix = 1 TO NUM-ENTRIES(cSelRowidList):
    bOk = DYNAMIC-FUNCTION("DoUpdate",hBuffSource:NAME,
                  icServerSelectProc,
                  "",
                  ENTRY(ix,cSelRowidList),
                  "",
                  "",
                  FALSE).
  END.
  DYNAMIC-FUNCTION("DoCommit",FALSE).
END.


hBrwSource:DELETE-SELECTED-ROWS().
IF TRIM(cSelRowidList,",") NE "" THEN
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"queryfilter",
                  " AND NOT CAN-DO('" + TRIM(cSelRowidList,",") + "',Rowident1)").

ELSE 
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"queryfilter","").

/* RUN OpenQuerySource. */
RUN OpenQueryTarget.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch Dialog-Frame 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cModSortColumn AS CHAR NO-UNDO.

IF icServerSelectProc = "" AND DYNAMIC-FUNCTION("GetCurrentObject") = hBrwTarget THEN DO:
  IF cSortColumn = hBrwTarget:CURRENT-COLUMN:NAME THEN bDesc = NOT bDesc.
  ELSE bDesc = FALSE.
  cSortColumn = hBrwTarget:CURRENT-COLUMN:NAME.

  IF SUBSTR(cSortColumn,LENGTH(cSortColumn)) = "]" THEN
    cModSortColumn = "jbextent_" + RIGHT-TRIM(SUBSTR(cSortColumn,R-INDEX(cSortColumn,"[") + 1),"]") + "_" + SUBSTR(cSortColumn,1,R-INDEX(cSortColumn,"[") - 1).
  ELSE cModSortColumn = cSortColumn.
  
  hBrwTarget:QUERY:QUERY-PREPARE("FOR EACH " + hBuffTarget:NAME +
                                 (IF cSortColumn NE "" THEN " BY " + cModSortColumn + (IF bDesc THEN " DESC" ELSE "")
                                  ELSE "")
                                 ).
  hBrwTarget:QUERY:QUERY-OPEN().

  DYNAMIC-FUNCTION("setSortLabel",hBrwTarget,cSortColumn,bDesc).
END.
ELSE RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord Dialog-Frame 
PROCEDURE UndoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
UndoSelection("all").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION adjustBrowseHeight Dialog-Frame 
FUNCTION adjustBrowseHeight RETURNS HANDLE
  ( INPUT iiDeltaY AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
ASSIGN hBrwSource:HEIGHT-PIXELS = hBrwSource:HEIGHT-PIXELS + iiDeltaY
       hBrwTarget:HEIGHT-PIXELS = hBrwTarget:HEIGHT-PIXELS + iiDeltaY
       rectBrwSource:HEIGHT-PIXELS IN FRAME {&FRAME-NAME} = rectBrwSource:HEIGHT-PIXELS + iiDeltaY
       rectBrwTarget:HEIGHT-PIXELS = rectBrwTarget:HEIGHT-PIXELS + iiDeltaY
       .

RETURN FRAME {&FRAME-NAME}:HANDLE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOutputData Dialog-Frame 
FUNCTION getOutputData RETURNS CHARACTER
  ( INPUT icOutputFields AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cValueList AS CHAR NO-UNDO.

IF NOT hBrwTarget:QUERY:IS-OPEN THEN RETURN "".

hBrwTarget:QUERY:GET-FIRST().
REPEAT WHILE NOT hBrwTarget:QUERY:QUERY-OFF-END:
  DO ix = 1 TO NUM-ENTRIES(icOutputFields):
    cValueList = cValueList + (IF hBuffTarget:BUFFER-FIELD(ENTRY(ix,icOutputFields)):BUFFER-VALUE NE ? THEN 
                                STRING(hBuffTarget:BUFFER-FIELD(ENTRY(ix,icOutputFields)):BUFFER-VALUE)
                               ELSE "") + "|".
  END.
  hBrwTarget:QUERY:GET-NEXT().
END.

RETURN SUBSTR(cValueList,1,LENGTH(cValueList) - 1). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN Btn_Cancel:LABEL         = "Cancel" 
           btnDeSelect:TOOLTIP      = "Remove selected row(s) from selection" 
           btnDeSelectAll:TOOLTIP   = "Remove all rows from selection"
           btnSelect:TOOLTIP        = "Add selected row(s) to selection"
           btnSelectAll:TOOLTIP     = "Add all rows to selection"
           FRAME Dialog-Frame:TITLE = "Select"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectedRowids Dialog-Frame 
FUNCTION setSelectedRowids RETURNS LOGICAL
  ( INPUT icSelRowidList AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

cSelRowidList = icSelRowidList.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UndoSelection Dialog-Frame 
FUNCTION UndoSelection RETURNS LOGICAL
  ( INPUT icUndoType AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF icUndoType = "all" THEN DO:
  cSelRowidList = "".
  DYNAMIC-FUNCTION("setAttribute",hBrwSource,"querywhere","").
  RUN OpenQuerySource.
  hBrwTarget:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrwTarget:QUERY:QUERY-OFF-END:
    IF hBuffTarget:BUFFER-FIELD("iJBoxSelectIndex"):BUFFER-VALUE NE 0 THEN
      hBuffTarget:BUFFER-DELETE.
    hBrwTarget:QUERY:GET-NEXT().
  END.
  RUN OpenQueryTarget.
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

