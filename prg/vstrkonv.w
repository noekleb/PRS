&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dstrkonv.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
DEFINE VARIABLE hDataSource  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNavigation  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cRapLabels   AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "sdo/dstrkonv.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.SeqNr RowObject.Merknad 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-62 
&Scoped-Define DISPLAYED-FIELDS RowObject.StrKode RowObject.Storl ~
RowObject.fBrukt RowObject.SeqNr RowObject.Merknad 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setRapLabels vTableWin 
FUNCTION setRapLabels RETURNS CHARACTER
  ( INPUT cLabels AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 76 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.StrKode AT ROW 1.19 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     RowObject.Storl AT ROW 2.19 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.fBrukt AT ROW 2.29 COL 30.4
          LABEL "Brukt"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.SeqNr AT ROW 3.24 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.Merknad AT ROW 4.29 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     RECT-62 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sdo/dstrkonv.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dstrkonv.i}
      END-FIELDS.
   END-TABLES.
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
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 4.62
         WIDTH              = 76.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}
{sdo/dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.fBrukt IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.Storl IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.StrKode IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Storl vTableWin
ON ANY-PRINTABLE OF RowObject.Storl IN FRAME F-Main /* Alfa storl */
DO:
  IF KEYFUNCTION(LASTKEY) = " " THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Storl vTableWin
ON RETURN OF RowObject.Storl IN FRAME F-Main /* Alfa storl */
DO:
    APPLY "TAB" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.StrKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.StrKode vTableWin
ON ENTRY OF RowObject.StrKode IN FRAME F-Main /* Num storl */
DO:
    ASSIGN SELF:MODIFIED = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.StrKode vTableWin
ON RETURN OF RowObject.StrKode IN FRAME F-Main /* Num storl */
DO:
  APPLY "TAB" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN RowObject.StrKode:SCREEN-VALUE = DYNAMIC-FUNCTION('getNyStrKode':U IN hDataSource)
             RowObject.StrKode:SENSITIVE = TRUE
             RowObject.Storl:SENSITIVE   = TRUE. 
      APPLY "ENTRY" TO RowObject.StrKode.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN RowObject.StrKode:SENSITIVE = FALSE
             RowObject.Storl:SENSITIVE   = FALSE. 
      IF VALID-HANDLE(hNavigation) AND RowObject.fBrukt:CHECKED IN FRAME {&FRAME-NAME} THEN
          DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
              INPUT "DELETE" /* CHARACTER */).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */
  IF VALID-HANDLE(hNavigation) AND RowObject.fBrukt:CHECKED IN FRAME {&FRAME-NAME} THEN
      DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
          INPUT "DELETE" /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF RETURN-VALUE = "ADM-ERROR" THEN DO:
      DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U)
         hNavigation = DYNAMIC-FUNCTION('getTableIOSource':U).
  /*
  IF RowObject.fBrukt:CHECKED IN FRAME {&FRAME-NAME} THEN
      DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
          INPUT "DELETE" /* CHARACTER */).
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printObject vTableWin 
PROCEDURE printObject :
DEFINE INPUT  PARAMETER cObject AS CHARACTER  NO-UNDO.
DEFINE VARIABLE         iType   AS INTEGER INIT ?   NO-UNDO.
  IF cObject = "StrKonvXPrint" OR cObject = "StrKonvExcel" THEN DO:
      RUN PrintStrKonv IN hDataSource (INPUT cObject,INPUT cRapLabels).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cInputStorl AS CHARACTER  NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT INT(RowObject.StrKode:SCREEN-VALUE) > 0 THEN DO:
         MESSAGE "Registrer"
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO RowObject.StrKode.
         RETURN.
     END.
     ELSE IF RowObject.Storl:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Registrer"
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO RowObject.Storl.
         RETURN.
     END.
     ASSIGN cInputStorl = RowObject.Storl:SCREEN-VALUE.
     RUN FixStorl  IN h_dproclib (INPUT-OUTPUT cInputStorl).
     ASSIGN RowObject.Storl:SCREEN-VALUE = cInputStorl.
  END.
  RUN SUPER.
  IF RETURN-VALUE = "ADM-ERROR" THEN DO:
      RUN refreshRow IN hDataSource.
  END.
  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN RowObject.StrKode:SENSITIVE = FALSE
             RowObject.Storl:SENSITIVE   = FALSE. 
      IF VALID-HANDLE(hNavigation) AND RowObject.fBrukt:CHECKED IN FRAME {&FRAME-NAME} THEN
          DYNAMIC-FUNCTION('disableActions':U IN hNavigation,
              INPUT "DELETE" /* CHARACTER */).
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setRapLabels vTableWin 
FUNCTION setRapLabels RETURNS CHARACTER
  ( INPUT cLabels AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  ASSIGN cRapLabels = cLabels.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

