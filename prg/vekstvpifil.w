&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dekstvpifil.i"}.


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
DEF VAR cTekst  AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dekstvpifil.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.VPIFilAktiv RowObject.VPIFilTypeNr ~
RowObject.VPIFilBeskrivelse RowObject.VPIFilNavn RowObject.VPIOperator ~
RowObject.VPIEkst RowObject.VPIKatalog RowObject.VPIInnlesningsrutine ~
RowObject.VPIUtpakkingsrutine RowObject.VPIOppdateringsrutine 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS B-SokVPIFilType RECT-56 
&Scoped-Define DISPLAYED-FIELDS RowObject.EkstVPILevNr RowObject.VPIFilNr ~
RowObject.VPIFilAktiv RowObject.VPIFilTypeNr RowObject.fuVPIFilTypeKNavn ~
RowObject.VPIFilBeskrivelse RowObject.VPIFilNavn RowObject.VPIOperator ~
RowObject.VPIEkst RowObject.VPIKatalog RowObject.VPIInnlesningsrutine ~
RowObject.VPIUtpakkingsrutine RowObject.VPIOppdateringsrutine 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-FilInfo FI-Programmer 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokVPIFilType  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-FilInfo AS CHARACTER FORMAT "X(256)":U INITIAL "Filbeskrivelse" 
      VIEW-AS TEXT 
     SIZE 32 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Programmer AS CHARACTER FORMAT "X(256)":U INITIAL "Behandlingsprogrammer" 
      VIEW-AS TEXT 
     SIZE 32 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 12.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B-SokVPIFilType AT ROW 4.1 COL 25.2 NO-TAB-STOP 
     RowObject.EkstVPILevNr AT ROW 2.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     RowObject.VPIFilNr AT ROW 3.05 COL 17 COLON-ALIGNED
          LABEL "FilNr"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.VPIFilAktiv AT ROW 3.05 COL 33
          LABEL "&Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .81 NO-TAB-STOP 
     RowObject.VPIFilTypeNr AT ROW 4.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.fuVPIFilTypeKNavn AT ROW 4.05 COL 27.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     RowObject.VPIFilBeskrivelse AT ROW 5.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39.8 BY 1
     RowObject.VPIFilNavn AT ROW 6.05 COL 17 COLON-ALIGNED FORMAT "X(245)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.VPIOperator AT ROW 6.05 COL 49 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Lik",1,
                     "Start",2,
                     "Slutt",3,
                     "Inneholder",4
          DROP-DOWN-LIST
          SIZE 15 BY 1
     RowObject.VPIEkst AT ROW 7.05 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.VPIKatalog AT ROW 8.05 COL 17 COLON-ALIGNED FORMAT "X(245)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.VPIInnlesningsrutine AT ROW 10 COL 17 COLON-ALIGNED
          LABEL "Innlesning"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.VPIUtpakkingsrutine AT ROW 11 COL 17 COLON-ALIGNED
          LABEL "Utpakking"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.VPIOppdateringsrutine AT ROW 12 COL 17 COLON-ALIGNED
          LABEL "Oppdatering" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     FI-FilInfo AT ROW 1.33 COL 17 COLON-ALIGNED NO-LABEL
     FI-Programmer AT ROW 9.29 COL 17 COLON-ALIGNED NO-LABEL
     RECT-56 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dekstvpifil.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dekstvpifil.i}
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
         HEIGHT             = 12.43
         WIDTH              = 66.2.
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
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.EkstVPILevNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-FilInfo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI-FilInfo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FI-Programmer IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI-Programmer:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuVPIFilTypeKNavn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuVPIFilTypeKNavn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.VPIFilAktiv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.VPIFilNavn IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.VPIFilNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.VPIInnlesningsrutine IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.VPIKatalog IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.VPIOppdateringsrutine IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.VPIUtpakkingsrutine IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-SokVPIFilType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokVPIFilType vTableWin
ON CHOOSE OF B-SokVPIFilType IN FRAME F-Main /* ... */
DO:
    /* Kaller søkerutine */
  RUN gvpifiltyper.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.VPIFilTypeNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowOBject.VPIFilTypeNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        RowObject.VPIFilTypeNr:MODIFIED     = TRUE
        RowOBject.fuVPIFilTypeKNavn:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
        RowObject.fuVPIFilTypeKNavn:MODIFIED     = FALSE 
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "VALUE-CHANGED":U TO RowObject.VPIFilTypeNr.
        RETURN NO-APPLY.
  END.

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
    ASSIGN 
      RowObject.VPIFilNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

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
    ASSIGN
      RowObject.VPIFilNr:SENSITIVE = FALSE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
    RowObject.VPIFilNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFokus vTableWin 
PROCEDURE SetFokus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF RowObject.VPIFilTypeNr:SENSITIVE = TRUE THEN
      RUN ApplyEntry ("VPIFilNr").
    ELSE
      RUN ApplyEntry ("VPIFilTypeNr").
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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE <> "ADM-ERROR" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      RowObject.VPIFilNr:SENSITIVE = FALSE
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

