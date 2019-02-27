&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dkassererkontanter.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dkassererkontanter.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.AntallValor1 RowObject.AntallValor2 ~
RowObject.AntallValor3 RowObject.AntallValor4 RowObject.AntallValor5 ~
RowObject.AntallValor6 RowObject.AntallValor7 RowObject.AntallValor8 ~
RowObject.AntallValor9 RowObject.AntallValor10 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS RowObject.AntallValor1 RowObject.Belop1 ~
RowObject.AntallValor2 RowObject.Belop2 RowObject.AntallValor3 ~
RowObject.Belop3 RowObject.AntallValor4 RowObject.Belop4 ~
RowObject.AntallValor5 RowObject.Belop5 RowObject.AntallValor6 ~
RowObject.Belop6 RowObject.AntallValor7 RowObject.Belop7 ~
RowObject.AntallValor8 RowObject.Belop8 RowObject.AntallValor9 ~
RowObject.Belop9 RowObject.AntallValor10 RowObject.Belop10 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Sum FI-ValTxt-2 FI-ValTxt FI-BelopTxt 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-BelopTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Beløp" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Sum AS DECIMAL FORMAT "->,>>>,>>9.99":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 20.2 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ValTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Antall" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ValTxt-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Valør" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 48 BY 12.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.AntallValor1 AT ROW 1.95 COL 12 COLON-ALIGNED
          LABEL "50 øre"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop1 AT ROW 1.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor2 AT ROW 2.95 COL 12 COLON-ALIGNED
          LABEL "1 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop2 AT ROW 2.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor3 AT ROW 3.95 COL 12 COLON-ALIGNED
          LABEL "5 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop3 AT ROW 3.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor4 AT ROW 4.95 COL 12 COLON-ALIGNED
          LABEL "10 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop4 AT ROW 4.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor5 AT ROW 5.95 COL 12 COLON-ALIGNED
          LABEL "20 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop5 AT ROW 5.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor6 AT ROW 6.95 COL 12 COLON-ALIGNED
          LABEL "50 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop6 AT ROW 6.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor7 AT ROW 7.95 COL 12 COLON-ALIGNED
          LABEL "100 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop7 AT ROW 7.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor8 AT ROW 8.95 COL 12 COLON-ALIGNED
          LABEL "200 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop8 AT ROW 8.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor9 AT ROW 9.95 COL 12 COLON-ALIGNED
          LABEL "500 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop9 AT ROW 9.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     RowObject.AntallValor10 AT ROW 10.95 COL 12 COLON-ALIGNED
          LABEL "1000 kr"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.4 BY 1
     RowObject.Belop10 AT ROW 10.95 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN NATIVE 
          SIZE 20.2 BY 1
     FI-Sum AT ROW 11.95 COL 23 COLON-ALIGNED NO-LABEL
     FI-ValTxt-2 AT ROW 1.24 COL 2.2 COLON-ALIGNED NO-LABEL
     FI-ValTxt AT ROW 1.24 COL 12 COLON-ALIGNED NO-LABEL
     FI-BelopTxt AT ROW 1.24 COL 23 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dkassererkontanter.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dkassererkontanter.i}
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
         HEIGHT             = 12.38
         WIDTH              = 48.4.
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

/* SETTINGS FOR FILL-IN RowObject.AntallValor1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor10 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor3 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor4 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor5 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor6 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor7 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor8 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.AntallValor9 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Belop9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BelopTxt IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI-BelopTxt:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FI-Sum IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-ValTxt IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI-ValTxt:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN FI-ValTxt-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI-ValTxt-2:READ-ONLY IN FRAME F-Main        = TRUE.

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

&Scoped-define SELF-NAME RowObject.AntallValor1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor1 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor1 IN FRAME F-Main /* 50 øre */
DO:
  RUN Summer (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor10 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor10 IN FRAME F-Main /* 1000 kr */
DO:
  RUN Summer (10).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor2 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor2 IN FRAME F-Main /* 1 kr */
DO:
  RUN Summer (2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor3 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor3 IN FRAME F-Main /* 5 kr */
DO:
  RUN Summer (3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor4 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor4 IN FRAME F-Main /* 10 kr */
DO:
  RUN Summer (4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor5 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor5 IN FRAME F-Main /* 20 kr */
DO:
  RUN Summer (5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor6 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor6 IN FRAME F-Main /* 50 kr */
DO:
  RUN Summer (6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor7 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor7 IN FRAME F-Main /* 100 kr */
DO:
  RUN Summer (7).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor8 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor8 IN FRAME F-Main /* 200 kr */
DO:
  RUN Summer (8).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AntallValor9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AntallValor9 vTableWin
ON VALUE-CHANGED OF RowObject.AntallValor9 IN FRAME F-Main /* 500 kr */
DO:
  RUN Summer (9).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirmExit vTableWin 
PROCEDURE confirmExit :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER plCancel AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR pbDataModified AS LOG NO-UNDO.
  ASSIGN
    pbDataModified = DYNAMIC-FUNCTION('getDataModified':U)
      .
  DO WITH FRAME {&FRAME-NAME}:
    IF pbDataModified THEN
    DO:
        MESSAGE "Det er gjort endringer." SKIP
                "Disse må lagres eller kanseleres før programmet kan avsluttes."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN
            plCancel = TRUE /* Flagger at avsluttning skal avbrytes */
            .
        RETURN NO-APPLY.
    END.
    ELSE
        DYNAMIC-FUNCTION('setDataModified':U,INPUT false).
  END.

  RUN SUPER( INPUT-OUTPUT plCancel).

  /* Code placed here will execute AFTER standard behavior.    */

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
  RUN Summer(99).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Summer vTableWin 
PROCEDURE Summer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piFelt AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      CASE piFelt:
          WHEN  1 THEN assign
                         RowObject.Belop1:SCREEN-VALUE  = string(INPUT RowObject.AntallValor1  * 0.5)
                         RowObject.Belop1:MODIFIED      = TRUE
                         .
          WHEN  2 THEN assign
                         RowObject.Belop2:SCREEN-VALUE  = string(INPUT RowObject.AntallValor2  * 1.0)
                         RowObject.Belop2:MODIFIED      = TRUE
                         .
          WHEN  3 THEN assign
                         RowObject.Belop3:SCREEN-VALUE  = string(INPUT RowObject.AntallValor3  * 5.0)
                         RowObject.Belop3:MODIFIED      = TRUE
                         .
          WHEN  4 THEN assign
                         RowObject.Belop4:SCREEN-VALUE  = string(INPUT RowObject.AntallValor4  * 10.0)
                         RowObject.Belop4:MODIFIED      = TRUE
                         .
          WHEN  5 THEN assign
                         RowObject.Belop5:SCREEN-VALUE  = string(INPUT RowObject.AntallValor5  * 20.0)
                         RowObject.Belop5:MODIFIED      = TRUE
                         .
          WHEN  6 THEN assign
                         RowObject.Belop6:SCREEN-VALUE  = string(INPUT RowObject.AntallValor6  * 50.0)
                         RowObject.Belop6:MODIFIED      = TRUE
                         .
          WHEN  7 THEN assign
                         RowObject.Belop7:SCREEN-VALUE  = string(INPUT RowObject.AntallValor7  * 100.0)
                         RowObject.Belop7:MODIFIED      = TRUE
                         .
          WHEN  8 THEN assign
                         RowObject.Belop8:SCREEN-VALUE  = string(INPUT RowObject.AntallValor8  * 200.0)
                         RowObject.Belop8:MODIFIED      = TRUE
                         .
          WHEN  9 THEN assign
                         RowObject.Belop9:SCREEN-VALUE  = string(INPUT RowObject.AntallValor9  * 500.0)
                         RowObject.Belop9:MODIFIED      = TRUE
                         .
          WHEN 10 THEN assign
                         RowObject.Belop10:SCREEN-VALUE = string(INPUT RowObject.AntallValor10 * 1000.0)
                         RowObject.Belop10:MODIFIED     = TRUE
                         .
      END CASE.
      ASSIGN
          FI-Sum:SCREEN-VALUE = STRING(
                                dec(RowObject.Belop1:SCREEN-VALUE) +
                                dec(RowObject.Belop2:SCREEN-VALUE) + 
                                dec(RowObject.Belop3:SCREEN-VALUE) + 
                                dec(RowObject.Belop4:SCREEN-VALUE) + 
                                dec(RowObject.Belop5:SCREEN-VALUE) + 
                                dec(RowObject.Belop6:SCREEN-VALUE) + 
                                dec(RowObject.Belop7:SCREEN-VALUE) + 
                                dec(RowObject.Belop8:SCREEN-VALUE) + 
                                dec(RowObject.Belop9:SCREEN-VALUE) + 
                                dec(RowObject.Belop10:SCREEN-VALUE))
          FI-Sum:MODIFIED     = FALSE
          .
      IF piFelt <> 99 THEN
          DYNAMIC-FUNCTION('setDataModified':U,INPUT TRUE ).
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
  IF RETURN-VALUE = "ADM-ERROR" THEN
      RETURN NO-APPLY.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

