&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dbokforingsbilag.i"}.



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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dbokforingsbilag.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.GodkjentFlagg ~
RowObject.SendtRegnskap RowObject.EODDato RowObject.EODMottatt 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-63 RECT-64 
&Scoped-Define DISPLAYED-FIELDS RowObject.ButikkNr RowObject.Aar ~
RowObject.BokforingsNr RowObject.GodkjentDato RowObject.GodkjentFlagg ~
RowObject.GodkjentAv RowObject.SendtDato RowObject.SendtRegnskap ~
RowObject.SendAv RowObject.EODDato RowObject.EODMottatt 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-5 FILL-IN-6 FILL-IN-7 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Godkjenning" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U INITIAL "Sendt regnskap" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "EOD mottatt" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 6.48.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.62.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-5 AT ROW 1 COL 1 NO-LABEL
     RowObject.ButikkNr AT ROW 1.81 COL 14.6 COLON-ALIGNED
          LABEL "Butikknr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Aar AT ROW 2.81 COL 14.6 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.BokforingsNr AT ROW 3.81 COL 14.6 COLON-ALIGNED
          LABEL "Bokføringsnr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.GodkjentDato AT ROW 5.76 COL 14.6 COLON-ALIGNED
          LABEL "Godkjent"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.GodkjentFlagg AT ROW 5.86 COL 30.8
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 3.4 BY .81
     RowObject.GodkjentAv AT ROW 6.86 COL 14.6 COLON-ALIGNED
          LABEL "Godkjent av"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.SendtDato AT ROW 9.33 COL 14.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.SendtRegnskap AT ROW 9.48 COL 30.8
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     RowObject.SendAv AT ROW 10.48 COL 14.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     RowObject.EODDato AT ROW 12.91 COL 14.6 COLON-ALIGNED 
          LABEL "Dato"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     RowObject.EODMottatt AT ROW 13 COL 30.6
          LABEL ""
          VIEW-AS TOGGLE-BOX
          SIZE 4 BY .81
     FILL-IN-6 AT ROW 8.38 COL 1 NO-LABEL
     FILL-IN-7 AT ROW 11.95 COL 1.2 NO-LABEL
     RECT-2 AT ROW 1.67 COL 1
     RECT-63 AT ROW 9.1 COL 1
     RECT-64 AT ROW 12.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dbokforingsbilag.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dbokforingsbilag.i}
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
         HEIGHT             = 24.48
         WIDTH              = 36.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

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

/* SETTINGS FOR FILL-IN RowObject.Aar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.BokforingsNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.ButikkNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.EODDato IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.EODMottatt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN RowObject.GodkjentAv IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.GodkjentDato IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX RowObject.GodkjentFlagg IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.SendAv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.SendtDato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.SendtRegnskap IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE collectChanges vTableWin 
PROCEDURE collectChanges :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER pcChanges AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcInfo    AS CHARACTER NO-UNDO.

  DEF VAR iInfo1 AS INT  NO-UNDO.
  DEF VAR cInfo2 AS CHAR NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT-OUTPUT pcChanges, INPUT-OUTPUT pcInfo).

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF INPUT RowObject.godkjentFlagg = TRUE THEN
          ASSIGN
          pcChanges = pcChanges + 
                      (IF pcChanges = ""
                         THEN ""
                         ELSE CHR(1)) +
                      "GodkjentDato" + CHR(1) + STRING(TODAY) + CHR(1) +
                      "GodkjentTid"  + CHR(1) + STRING(time)  + CHR(1) +
                      "GodkjentAv"   + CHR(1) + userid("SkoTex")
          .
      ELSE 
          ASSIGN
          pcChanges = pcChanges + 
                      (IF pcChanges = ""
                         THEN ""
                         ELSE CHR(1)) +
                      "GodkjentDato" + CHR(1) + "?" + CHR(1) +
                      "GodkjentTid"  + CHR(1) + "0"  + CHR(1) +
                      "GodkjentAv"   + CHR(1) + ""
          .
      ASSIGN
          iInfo1    = INT(ENTRY(1,pcInfo))
          cInfo2    = ENTRY(2,pcInfo) + "," + ENTRY(3,pcInfo)
          pcInfo    = STRING(iInfo1 + 3) + "," + cInfo2
          .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF RowObject.GodkjentFlagg:SCREEN-VALUE = "yes" THEN
          ASSIGN
          RowObject.GodkjentFlagg:SENSITIVE = FALSE.
      ELSE
          ASSIGN
          RowObject.GodkjentFlagg:SENSITIVE = TRUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

