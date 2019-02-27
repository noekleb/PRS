&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sObject 
/*------------------------------------------------------------------------

  File:

  Description: from SMART.W - Template for basic ADM2 SmartObject

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

DEFINE VARIABLE hBrowse      AS HANDLE          NO-UNDO.
DEFINE VARIABLE hActiveCol   AS HANDLE          NO-UNDO.
DEFINE VARIABLE iSortBgColor AS INTEGER INIT 15 NO-UNDO.
DEFINE VARIABLE cTable       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_dHandle    AS HANDLE     NO-UNDO.
DEFINE VARIABLE h_ActiveSok  AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-Sort RECT-1 
&Scoped-Define DISPLAYED-OBJECTS FI-INTE CB-Sort RS-SortOrder TOGGLE-Filter 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "","Item 1"
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FI-CHAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DATE AS DATE FORMAT "99-99-99":U 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FI-INTE AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE RS-SortOrder AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Stigende", 1,
"Synkende", 2
     SIZE 31.2 BY 1.05 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 94 BY 1.67.

DEFINE VARIABLE TOGGLE-Filter AS LOGICAL INITIAL no 
     LABEL "Filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FI-CHAR AT ROW 1.29 COL 15 COLON-ALIGNED NO-LABEL
     FI-INTE AT ROW 1.29 COL 15 COLON-ALIGNED NO-LABEL
     FI-DATE AT ROW 1.29 COL 17 NO-LABEL
     CB-Sort AT ROW 1.29 COL 35.6 COLON-ALIGNED NO-LABEL
     RS-SortOrder AT ROW 1.29 COL 62.8 NO-LABEL
     TOGGLE-Filter AT ROW 1.33 COL 2.2
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW sObject ASSIGN
         HEIGHT             = 1.76
         WIDTH              = 94.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sObject 
/* ************************* Included-Libraries *********************** */

{src/adm2/visual.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sObject
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-CHAR IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-CHAR:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FI-DATE IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FI-DATE:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FI-INTE IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RS-SortOrder IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOGGLE-Filter IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CB-Sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Sort sObject
ON VALUE-CHANGED OF CB-Sort IN FRAME F-Main
DO:
  DEFINE VARIABLE hTmpHandle AS HANDLE NO-UNDO.
  DEFINE VARIABLE cWhere    AS CHARACTER  NO-UNDO.
  ASSIGN hTmpHandle = hBrowse:FIRST-COLUMN.
  DO WHILE VALID-HANDLE(hTmpHandle):
       IF hTmpHandle:NAME = CB-Sort:SCREEN-VALUE THEN
           LEAVE.
       ASSIGN hTmpHandle = hTmpHandle:NEXT-COLUMN.
  END.
  IF hActiveCol <> hTmpHandle THEN DO:
      ASSIGN hActiveCol:LABEL-BGCOLOR = ?
             hActiveCol = hTmpHandle 
             hActiveCol:LABEL-BGCOLOR = iSortBgColor.
  END.
  ELSE
      ASSIGN cWhere = DYNAMIC-FUNCTION('getQueryWhere':U IN h_dHandle).
  RUN FIX-FILL-IN (hActiveCol:DATA-TYPE).
  DYNAMIC-FUNCTION('setQueryWhere':U IN h_dHandle,
     INPUT cWhere /* CHARACTER */).
  DYNAMIC-FUNCTION('setQuerySort':U IN h_dHandle,
     INPUT CB-Sort:SCREEN-VALUE + " " + STRING(RS-SortOrder:SCREEN-VALUE = "2", "DESCENDING/") /* CHARACTER */).
  DYNAMIC-FUNCTION('openQuery':U IN h_dHandle).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-CHAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-CHAR sObject
ON RETURN OF FI-CHAR IN FRAME F-Main
DO:
    RUN WherePrep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-DATE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DATE sObject
ON ENTRY OF FI-DATE IN FRAME F-Main
DO:
    APPLY "HOME" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DATE sObject
ON LEAVE OF FI-DATE IN FRAME F-Main
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FI-DATE:SCREEN-VALUE) NO-ERROR.
    IF wDate = ? THEN DO:
        ASSIGN FI-DATE:SCREEN-VALUE = ?.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-DATE sObject
ON RETURN OF FI-DATE IN FRAME F-Main
DO:
    DEFINE VAR wDate AS DATE.
    wDate = DATE(FI-DATE:SCREEN-VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE "Feilaktigt tastet dato." VIEW-AS ALERT-BOX ERROR TITLE "Feil dato".
        ASSIGN FI-DATE:SCREEN-VALUE = "".
        APPLY "ENTRY" TO FI-DATE.
        RETURN NO-APPLY.
    END.
    RUN WherePrep.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-INTE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-INTE sObject
ON RETURN OF FI-INTE IN FRAME F-Main
DO:
    RUN WherePrep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-SortOrder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-SortOrder sObject
ON VALUE-CHANGED OF RS-SortOrder IN FRAME F-Main
DO:
    APPLY "RETURN" TO h_ActiveSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Filter sObject
ON VALUE-CHANGED OF TOGGLE-Filter IN FRAME F-Main /* Filter */
DO:
  IF h_ActiveSok:SCREEN-VALUE = "" OR (h_ActiveSok:DATA-TYPE = "DATE" AND DATE(h_ActiveSok:SCREEN-VALUE) = ?) THEN
      APPLY "VALUE-CHANGED" TO CB-Sort.
  ELSE
      APPLY "RETURN" TO h_ActiveSok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sObject 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sObject  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FIX-FILL-IN sObject 
PROCEDURE FIX-FILL-IN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE INPUT  PARAMETER cType AS CHARACTER  NO-UNDO.
 DO WITH FRAME {&FRAME-NAME}:
      IF CAN-DO("INTEGER,CHARACTER,DATE",cType) THEN DO:
          ASSIGN h_ActiveSok:SCREEN-VALUE = "".
          IF VALID-HANDLE(h_ActiveSok) AND h_ActiveSok:DATA-TYPE <> cType THEN DO:
              ASSIGN h_ActiveSok:SENSITIVE = FALSE
                     h_ActiveSok:HIDDEN    = TRUE
                     h_ActiveSok           = IF cType = "INTEGER" THEN FI-INTE:HANDLE ELSE
                                             IF cType = "CHARACTER" THEN FI-CHAR:HANDLE ELSE
                                                   FI-DATE:HANDLE
                     h_ActiveSok:HIDDEN    = FALSE.
          END.
          ASSIGN TOGGLE-Filter:SENSITIVE = TRUE
                 h_ActiveSok:SENSITIVE = TRUE
                 RS-SortOrder:SENSITIVE = TRUE
                 h_ActiveSok:FORMAT = IF cType = "INTEGER" THEN
                                      REPLACE(TRIM(REPLACE(hActiveCol:FORMAT,",",""),"-"),"9","Z") 
                                      ELSE hActiveCol:FORMAT.
      END.
      ELSE DO:
           ASSIGN TOGGLE-Filter:SENSITIVE = FALSE
                  h_ActiveSok:SENSITIVE   = FALSE.
      END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject sObject 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN h_ActiveSok = FI-INTE:HANDLE IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitierSok sObject 
PROCEDURE InitierSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER h_BrowseSDO AS HANDLE     NO-UNDO.
   DEFINE INPUT  PARAMETER cSortFields AS CHARACTER  NO-UNDO.
   DEFINE         VARIABLE hCol        AS HANDLE      NO-UNDO.
   DEFINE         VARIABLE cComboList  AS CHARACTER   NO-UNDO.
   
   ASSIGN h_dHandle  = DYNAMIC-FUNCTION('getDataSource':U IN h_BrowseSDO)
          cTable     = DYNAMIC-FUNCTION('getTables':U IN h_dHandle)
          hBrowse    = DYNAMIC-FUNCTION('getBrowseHandle':U IN h_BrowseSDO)
          hCol       = hBrowse:FIRST-COLUMN
          hActiveCol = hBrowse:FIRST-COLUMN.
   DO WHILE VALID-HANDLE(hCol):
       IF CAN-DO(cSortFields,hCol:NAME) THEN
           ASSIGN cComboList = cComboList + (IF cComboList = "" THEN "" ELSE ",") + hCol:LABEL + "," + hCol:NAME.
       ASSIGN hCol = hCol:NEXT-COLUMN.
   END.
   IF cComboList <> "" THEN DO:
       ASSIGN CB-Sort:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cComboList
              CB-Sort:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(2,cComboList)
              hActiveCol:LABEL-BGCOLOR = iSortBgColor.
       APPLY "VALUE-CHANGED" TO CB-Sort.
   END.
   ELSE 
       ASSIGN CB-Sort:SENSITIVE = FALSE
              hActiveCol:LABEL-BGCOLOR = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WherePrep sObject 
PROCEDURE WherePrep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cWhere AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    CASE hActiveCol:DATA-TYPE:
        WHEN "INTEGER" THEN DO:
            IF FI-INTE:SCREEN-VALUE <> "" THEN DO:
              ASSIGN cWhere = cTable + "." + CB-Sort:SCREEN-VALUE + (IF TOGGLE-Filter:CHECKED THEN " = "
                          ELSE " >= ") + "'" + FI-INTE:SCREEN-VALUE + "'".
            END.
        END.
        WHEN "CHARACTER" THEN DO:
            IF FI-CHAR:SCREEN-VALUE <> "" THEN DO:
/*                 IF FI-CHAR:SCREEN-VALUE BEGINS "*" THEN TOGGLE-Filter:CHECKED = TRUE. */
                ASSIGN  cWhere = cTable + "." + CB-Sort:SCREEN-VALUE.
                IF TOGGLE-Filter:CHECKED AND FI-CHAR:SCREEN-VALUE BEGINS "*" THEN
                    ASSIGN cWhere = cWhere + " MATCHES " + "'" + FI-CHAR:SCREEN-VALUE + "*" + "'".
                ELSE IF TOGGLE-Filter:CHECKED AND FI-CHAR:SCREEN-VALUE <> "" THEN
                    ASSIGN cWhere = cWhere + " BEGINS " + "'" + FI-CHAR:SCREEN-VALUE + "'".
                ELSE 
                    ASSIGN cWhere = cWhere + " >= " + "'" + FI-CHAR:SCREEN-VALUE + "'".
            END.
        END.
        WHEN "DATE" THEN DO:
            IF DATE(FI-DATE:SCREEN-VALUE) <> ? THEN DO:
                ASSIGN cWhere = cTable + "." + CB-Sort:SCREEN-VALUE + (IF TOGGLE-Filter:CHECKED THEN " = "
                        ELSE " >= ") + "'" + FI-DATE:SCREEN-VALUE + "'".
            END.
        END.
    END CASE.
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dHandle,
     INPUT cWhere /* CHARACTER */).
    DYNAMIC-FUNCTION('setQuerySort':U IN h_dHandle,
     INPUT CB-Sort:SCREEN-VALUE + " " + STRING(RS-SortOrder:SCREEN-VALUE = "2", "DESCENDING/") /* CHARACTER */).
    DYNAMIC-FUNCTION('openQuery':U IN h_dHandle).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

