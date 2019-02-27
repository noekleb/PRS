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
/*   DEF VAR icBuffersAndFields   AS CHAR NO-UNDO INIT "Kunde;KundeNr;Navn;Adresse1;Telefon;MobilTlf;RegistrertDato;ButikkNr;!TypeId,KundeType;Beskrivelse".  */
/*   DEF VAR icQueryString  AS CHAR NO-UNDO INIT "where navn matches '*a*',first KundeType of Kunde no-lock".                                           */
/*   DEF VAR iocValue  AS CHAR NO-UNDO INIT "Navn;Adresse1,KundeNr;Navn;Adresse1".                                                                 */

/*   DEF VAR icBuffersAndFields   AS CHAR NO-UNDO INIT "Medlem;MedlemsNr;EtterNavn;ForNavn;Adresse1;Telefon;MobilTlf". */
/*   DEF VAR icQueryString  AS CHAR NO-UNDO INIT "where true".                                                   */
/*   DEF VAR iocValue  AS CHAR NO-UNDO INIT "MedlemsNr".                                                    */

  DEF VAR icBuffersAndFields   AS CHAR NO-UNDO INIT "Fag;Fag;Navn;Faggr;Fagomr".
  DEF VAR icQueryString        AS CHAR NO-UNDO INIT "where true".
  DEF VAR iocValue             AS CHAR NO-UNDO INIT "Fag".
&ELSE
  DEF INPUT PARAM  icBuffersAndFields   AS CHAR NO-UNDO.
  DEF INPUT PARAM  icQueryString  AS CHAR NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocValue AS CHAR NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */

DEF VAR bOK              AS LOG    NO-UNDO.
DEF VAR ix               AS INT    NO-UNDO.
DEF VAR hBuffer          AS HANDLE NO-UNDO.
DEF VAR hBrowse          AS HANDLE NO-UNDO.
DEF VAR cSortString      AS CHAR   NO-UNDO.
                         
DEF VAR cQueryString     AS CHAR   NO-UNDO.
DEF VAR cRelTable        AS CHAR   NO-UNDO.
DEF VAR cTable           AS CHAR   NO-UNDO.
DEF VAR cReturnField     AS CHAR   NO-UNDO.
                         
DEF VAR hSearchField     AS HANDLE NO-UNDO.
                         
DEF VAR hFilterSearch1   AS HANDLE NO-UNDO.
DEF VAR hFilterSearch2   AS HANDLE NO-UNDO.
DEF VAR hFilterLabel2    AS HANDLE NO-UNDO.
DEF VAR hFilterLabel1    AS HANDLE NO-UNDO.
                         
DEF VAR bProgDefined     AS LOG    NO-UNDO.
DEF VAR bSwapLocal       AS LOG    NO-UNDO.
DEF VAR bMultiSelect     AS LOG    NO-UNDO.
DEF VAR hTargetObject    AS HANDLE NO-UNDO.
DEF VAR iNumReturnRows   AS INT    NO-UNDO.
DEF VAR bSortedSelect    AS LOG    NO-UNDO.
DEF VAR cReturnValue     AS CHAR   NO-UNDO.

DEF VAR hParent          AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttSelected 
    FIELD dSelected AS DATE
    FIELD fSelected AS DEC
    FIELD cSelected AS CHAR
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectBrowse rectSearchField tbUseLocal Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tbUseLocal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSortString Dialog-Frame 
FUNCTION setSortString RETURNS LOGICAL
  ( INPUT icSortString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE RECTANGLE rectBrowse
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 90 BY 15.05.

DEFINE RECTANGLE rectSearchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY .95.

DEFINE VARIABLE tbUseLocal AS LOGICAL INITIAL no 
     LABEL "Behold utvalg ved sortering" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tbUseLocal AT ROW 1.33 COL 58
     Btn_OK AT ROW 17.76 COL 61
     Btn_Cancel AT ROW 17.76 COL 77
     rectBrowse AT ROW 2.43 COL 2
     rectSearchField AT ROW 1.24 COL 2.4
     SPACE(69.79) SKIP(16.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Velg verdi"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Compile into: .\run9
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Velg verdi */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  cReturnValue = 'AVBRYT'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON ANY-PRINTABLE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF VALID-HANDLE(hSearchField) THEN DO:
    APPLY "entry" TO hSearchField.
    APPLY LASTKEY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR idx       AS INT  NO-UNDO.
  DEF VAR iSelCount AS INT  NO-UNDO.
  DEF VAR cDataType AS CHAR NO-UNDO.

  ASSIGN iocValue = ""
         cReturnField = REPLACE(cReturnField,",",";").
  IF NOT bMultiSelect AND hBuffer:AVAIL THEN DO:
    iocValue = STRING(hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):BUFFER-VALUE).
    DO ix = 2 TO NUM-ENTRIES(cReturnField,";"):
      iocValue = iocValue + "|" + STRING(hBuffer:BUFFER-FIELD(ENTRY(ix,cReturnField,";")):BUFFER-VALUE).
    END.
  END.
  ELSE IF bMultiSelect THEN DO:
    IF bSortedSelect THEN DO:
      EMPTY TEMP-TABLE ttSelected.
      DO idx = 1 TO hBrowse:NUM-SELECTED-ROWS:
        IF hBrowse:FETCH-SELECTED-ROW(idx) THEN DO:
          CREATE ttSelected.
          CASE hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):DATA-TYPE:
            WHEN "character" THEN
              ASSIGN ttSelected.cSelected = hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):BUFFER-VALUE
                     cDataType            = "character".
            WHEN "integer" OR WHEN "decimal" THEN
              ASSIGN ttSelected.fSelected = hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):BUFFER-VALUE
                     cDataType            = "number".
            WHEN "date" THEN
              ASSIGN ttSelected.dSelected = hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):BUFFER-VALUE
                     cDataType            = "date".
            OTHERWISE 
              ttSelected.cSelected = STRING(hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):BUFFER-VALUE).
          END CASE.
        END.
      END.
      iSelCount = hBrowse:NUM-SELECTED-ROWS.
      idx = 0.
      FOR EACH ttSelected
          BY ttSelected.fSelected
          BY ttSelected.dSelected
          BY ttSelected.cSelected:
        idx = idx + 1.
        IF iNumReturnRows NE 0 THEN DO:
          IF idx = 1 OR idx = hBrowse:NUM-SELECTED-ROWS THEN DO:
            iocValue = iocValue + (IF iocValue NE "" THEN "¤" ELSE "").
            CASE cDataType:
              WHEN "number" THEN iocValue = iocValue + STRING(ttSelected.fSelected).
              WHEN "date"   THEN iocValue = iocValue + STRING(ttSelected.dSelected).
              OTHERWISE          iocValue = iocValue + ttSelected.cSelected. 
            END.
          END.
        END.
      END.
    END.
    
    ELSE DO idx = 1 TO hBrowse:NUM-SELECTED-ROWS:
      IF hBrowse:FETCH-SELECTED-ROW(idx) THEN DO:
        iocValue = iocValue + STRING(hBuffer:BUFFER-FIELD(ENTRY(1,cReturnField,";")):BUFFER-VALUE).
        DO ix = 2 TO NUM-ENTRIES(cReturnField,";"):
          iocValue = iocValue + "|" + STRING(hBuffer:BUFFER-FIELD(ENTRY(ix,cReturnField,";")):BUFFER-VALUE).
        END.
        iocValue = TRIM(iocValue,"|") + "¤".
      END.
    END.
    DYNAMIC-FUNCTION("setAttribute",hTargetObject,"lastlookupreturnvalues",TRIM(iocValue,"¤")).
    iocValue = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbUseLocal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbUseLocal Dialog-Frame
ON VALUE-CHANGED OF tbUseLocal IN FRAME Dialog-Frame /* Behold utvalg ved sortering */
DO:  
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata",IF SELF:CHECKED THEN "yes" ELSE "").
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


  ENABLE rectBrowse rectSearchField Btn_OK Btn_Cancel 
         WITH FRAME Dialog-Frame.
  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"getCurrentSourceProc") THEN 
    hParent = DYNAMIC-FUNCTION("getCurrentSourceProc" IN SOURCE-PROCEDURE).  

  RUN InitWindow.

  IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"lastrowid") NE "" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"uselocaldata","yes").

  IF NOT VALID-HANDLE(hParent) AND CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"myLookupObject") THEN DO:
    RUN myLookupObject IN SOURCE-PROCEDURE(NEW JBoxDynLookup(hParent,THIS-PROCEDURE,hBrowse,?)).
    hParent = SOURCE-PROCEDURE.
  END.
  ELSE IF VALID-HANDLE(hParent) AND CAN-DO(hParent:INTERNAL-ENTRIES,"myLookupObject") THEN
    RUN myLookupObject IN hParent(NEW JBoxDynLookup(hParent,THIS-PROCEDURE,hBrowse,?)).
  ELSE IF NOT VALID-HANDLE(hParent) AND CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"setLookupAttributes") THEN DO:
    RUN setLookupAttributes IN SOURCE-PROCEDURE(hBrowse,hFilterSearch1,hFilterSearch2).
    hParent = SOURCE-PROCEDURE.
  END.
  ELSE IF VALID-HANDLE(hParent) AND CAN-DO(hParent:INTERNAL-ENTRIES,"setLookupAttributes") THEN
    RUN setLookupAttributes IN hParent (hBrowse,hFilterSearch1,hFilterSearch2).
  ELSE
    PUBLISH "setLookupAttributes" (hBrowse,hFilterSearch1,hFilterSearch2).

  FRAME {&FRAME-NAME}:HIDDEN = FALSE.

  DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
  DYNAMIC-FUNCTION("setAttribute",SESSION,"keepwindowlocked","").
  DYNAMIC-FUNCTION("DoLockWindow",?).
/*   ON 'default-action':U OF hBrowse                     */
/*   DO:                                                  */
/*     APPLY "choose":U TO btn_OK IN FRAME {&FRAME-NAME}. */
/*   END.                                                 */

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
DYNAMIC-FUNCTION("DeleteObject",hBrowse).
RUN disable_UI.

RETURN cReturnValue.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse Dialog-Frame 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
APPLY "choose":U TO btn_OK IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord Dialog-Frame 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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
  DISPLAY tbUseLocal 
      WITH FRAME Dialog-Frame.
  ENABLE rectBrowse rectSearchField tbUseLocal Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExpandSearchDialog Dialog-Frame 
PROCEDURE ExpandSearchDialog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM icDir    AS CHAR   NO-UNDO.
DEF INPUT PARAM iiDelta  AS INT    NO-UNDO.

IF ihBrowse NE hBrowse THEN RETURN.

IF icDir = "x" THEN DO:
  IF iiDelta > 0 THEN DO:
    ASSIGN FRAME {&FRAME-NAME}:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS + iiDelta
           rectBrowse:WIDTH-PIXELS = rectBrowse:WIDTH-PIXELS + iiDelta
           hBrowse:WIDTH-PIXELS = hBrowse:WIDTH-PIXELS + iiDelta
           Btn_OK:X = Btn_OK:X + iiDelta
           Btn_Cancel:X = Btn_Cancel:X + iiDelta
           .
  END.
  ELSE 
    ASSIGN hBrowse:WIDTH-PIXELS = hBrowse:WIDTH-PIXELS + iiDelta
           rectBrowse:WIDTH-PIXELS = rectBrowse:WIDTH-PIXELS + iiDelta
           Btn_OK:X = Btn_OK:X + iiDelta
           Btn_Cancel:X = Btn_Cancel:X + iiDelta
           FRAME {&FRAME-NAME}:WIDTH-PIXELS = FRAME {&FRAME-NAME}:WIDTH-PIXELS + iiDelta
           .
END.
ELSE IF icDir = "y" THEN DO:
  IF iiDelta > 0 THEN 
    ASSIGN FRAME {&FRAME-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS + iiDelta
           rectBrowse:HEIGHT-PIXELS = rectBrowse:HEIGHT-PIXELS + iiDelta
           hBrowse:HEIGHT-PIXELS = hBrowse:HEIGHT-PIXELS + iiDelta
           Btn_OK:Y = Btn_OK:Y + iiDelta
           Btn_Cancel:Y = Btn_Cancel:Y + iiDelta
           .
  ELSE 
    ASSIGN hBrowse:HEIGHT-PIXELS = hBrowse:HEIGHT-PIXELS + iiDelta
           rectBrowse:HEIGHT-PIXELS = rectBrowse:HEIGHT-PIXELS + iiDelta
           Btn_OK:Y = Btn_OK:Y + iiDelta
           Btn_Cancel:Y = Btn_Cancel:Y + iiDelta
           FRAME {&FRAME-NAME}:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS + iiDelta
           .
END.


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
DEF VAR cBuffsAndFlds    AS CHAR NO-UNDO.
DEF VAR iy               AS INT NO-UNDO.

DEF VAR cViewFlds1stBuff AS CHAR NO-UNDO.
DEF VAR cQueryString     AS CHAR NO-UNDO.

DEF VAR cDefaultFrameFont  AS CHAR   NO-UNDO.

cDefaultFrameFont = DYNAMIC-FUNCTION("getAttribute",SESSION,"defaultFrameFont").
IF cDefaultFrameFont NE "" THEN
  FRAME {&FRAME-NAME}:FONT = INTEGER(cDefaultFrameFont) NO-ERROR.


IF NUM-ENTRIES(ENTRY(1,icBuffersAndFields),";") > 1 AND ENTRY(2,ENTRY(1,icBuffersAndFields),";") BEGINS "!" OR ENTRY(2,ENTRY(1,icBuffersAndFields),";") BEGINS "+!" THEN DO:
  MESSAGE "First column in lookup must be defined as visible (cannot start with !)"
          VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.

IF VALID-HANDLE(hParent) THEN
  SUBSCRIBE TO "ExpandSearchDialog" IN hParent.
ELSE
  SUBSCRIBE TO "ExpandSearchDialog" ANYWHERE.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN tbUseLocal:HIDDEN = YES
         hTargetObject     = DYNAMIC-FUNCTION("getCurrentObject")
         bMultiSelect      = DYNAMIC-FUNCTION("getAttribute",hTargetObject,"lookup_multiselect") = "yes"
         iNumReturnRows    = INT(DYNAMIC-FUNCTION("getAttribute",hTargetObject,"lookup_numreturnrows"))
         bSortedSelect     = DYNAMIC-FUNCTION("getAttribute",hTargetObject,"lookup_sortedselect") = "yes".

  IF bMultiSelect AND bSortedSelect THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hTargetObject,"lastlookupreturnvalues","").
    IF NUM-ENTRIES(iocValue,";") > 1 THEN DO:
      MESSAGE PROGRAM-NAME(1) SKIP
              "Invalid usage of dynamic lookup. When returning multiple sorted rows only one field (from each row) can be selected"
              VIEW-AS ALERT-BOX ERROR TITLE "Programmers mistake".
      RETURN.
    END.
  END.

  IF INDEX(icQueryString," BY ") > 0 THEN
    ASSIGN cQueryString  = SUBSTR(icQueryString,1,INDEX(icQueryString," BY "))
           cSortString   = ENTRY(2,TRIM(SUBSTR(icQueryString,INDEX(icQueryString," BY ")))," ")
           .
  ELSE cQueryString = icQueryString.

  DO ix = 1 TO NUM-ENTRIES(icBuffersAndFields):
    cBuffsAndFlds = cBuffsAndFlds + (IF cBuffsAndFlds NE "" THEN "," ELSE "") + ENTRY(1,ENTRY(ix,icBuffersAndFields),";").
    IF ix = 1 THEN cTable = ENTRY(1,ENTRY(ix,icBuffersAndFields),";").
    DO iy = 2 TO NUM-ENTRIES(ENTRY(ix,icBuffersAndFields),";"):
      IF cSortString = "" THEN DO:
        cSortString = TRIM(ENTRY(1,ENTRY(iy,ENTRY(1,icBuffersAndFields),";"),"|"),"+").
        IF cSortString BEGINS "distinct " THEN
          cSortString = REPLACE(cSortString,"distinct ","").
      END.
      IF iocValue = "" THEN iocValue = cSortString.
      IF ix = 1 THEN 
        cViewFlds1stBuff = cViewFlds1stBuff + ENTRY(1,ENTRY(iy,ENTRY(ix,icBuffersAndFields),";"),"|") + ",".

      cBuffsAndFlds = cBuffsAndFlds + ";" + ENTRY(iy,ENTRY(ix,icBuffersAndFields),";").
    END.
  END.

  ASSIGN cReturnField = iocValue
         iocValue     = "".

  IF cQueryString = "" THEN cQueryString = "WHERE false".
  IF CAN-DO("_file,_field,_index",cTable) THEN cSortString = "".

  IF DYNAMIC-FUNCTION("IsFieldNameInTable",ENTRY(1,cTable,";"),"iJBoxCompanyId") THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery",
                     (IF DYNAMIC-FUNCTION("getAttribute",hBrowse,"basequery") NE "" THEN
                        " AND "
                       ELSE "WHERE ") +
                     " iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId"))).

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",       
                    rectBrowse:HANDLE,          
                    300,                        
                    IF bMultiSelect THEN "MULTIPLE" ELSE "",                         
                    cBuffsAndFlds,  
                    cQueryString,
                    IF cSortString NE "" THEN "SORT|" + cSortString ELSE ""
                    ).    

  bSwapLocal = DYNAMIC-FUNCTION("getAttribute",hBrowse,"lastrowid") NE "" OR TRIM(cQueryString) BEGINS "where false".

  IF bMultiSelect THEN DYNAMIC-FUNCTION("setAttribute",hBrowse,"windowsbrowse","yes").

  IF cQueryString BEGINS "WHERE false" THEN
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",rectSearchField:HANDLE,hBrowse,1).

  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1) NO-ERROR.

  IF cQueryString MATCHES "*WHERE false*" THEN
    APPLY "entry" TO hSearchField.
  ELSE DO:
    APPLY "entry" TO hBrowse.
    IF bMultiSelect THEN
      hBrowse:SELECT-ROW(1) NO-ERROR.
  END.
END.

LocalTranslation().
DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSortSearch Dialog-Frame 
PROCEDURE StartSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

tbUseLocal:VISIBLE IN FRAME {&FRAME-NAME} = bSwapLocal AND 
                                            DYNAMIC-FUNCTION("getAttribute",hBrowse,"lastrowid") NE "" AND
                                            hBrowse:QUERY:NUM-RESULTS > 0 NO-ERROR.
tbUseLocal:SENSITIVE = tbUseLocal:VISIBLE.   

IF bMultiSelect AND hBrowse:QUERY:NUM-RESULTS > 0 THEN 
  hBrowse:SELECT-FOCUSED-ROW().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

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
           FRAME Dialog-Frame:TITLE = "Select"
           tbUseLocal:LABEL         = "Keep selection when sorting"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSortString Dialog-Frame 
FUNCTION setSortString RETURNS LOGICAL
  ( INPUT icSortString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

cSortString = icSortString.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

