&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dartpris.i"}.



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
&Scoped-define DATA-FIELD-DEFS "sdo/dartpris.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Tilbud RowObject.AktivFraDato ~
RowObject.TilbudTilDato RowObject.InnkjopsPris1 RowObject.InnkjopsPris2 ~
RowObject.Rab1Kr1 RowObject.Rab1Kr2 RowObject.Rab1%2 RowObject.Rab1%1 ~
RowObject.Rab2Kr2 RowObject.Rab2Kr1 RowObject.Rab2%2 RowObject.Rab2%1 ~
RowObject.Frakt1 RowObject.Frakt2 RowObject.Frakt%2 RowObject.Frakt%1 ~
RowObject.DivKostKr1 RowObject.DivKostKr2 RowObject.DivKost%2 ~
RowObject.DivKost%1 RowObject.Rab3Kr2 RowObject.Rab3Kr1 RowObject.Rab3%1 ~
RowObject.Rab3%2 RowObject.VareKost1 RowObject.VareKost2 RowObject.DBKr1 ~
RowObject.DBKr2 RowObject.DB%1 RowObject.DB%2 RowObject.MvaKr1 ~
RowObject.MvaKr2 RowObject.Mva%2 RowObject.Mva%1 RowObject.Pris1 ~
RowObject.Pris2 RowObject.EuroPris2 RowObject.EuroPris1 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS FI-Gjeldende FI-Tilbud RECT-42 RECT-43 ~
RECT-44 RECT-55 RS-Kampanje FI-Kampanje FI-Profil FI-Kronor FI-Procent 
&Scoped-Define DISPLAYED-FIELDS RowObject.Tilbud RowObject.AktivFraDato ~
RowObject.TilbudTilDato RowObject.ProfilNr RowObject.InnkjopsPris1 ~
RowObject.InnkjopsPris2 RowObject.Rab1Kr1 RowObject.Rab1Kr2 ~
RowObject.Rab1%2 RowObject.Rab1%1 RowObject.Rab2Kr2 RowObject.Rab2Kr1 ~
RowObject.Rab2%2 RowObject.Rab2%1 RowObject.Frakt1 RowObject.Frakt2 ~
RowObject.Frakt%2 RowObject.Frakt%1 RowObject.DivKostKr1 ~
RowObject.DivKostKr2 RowObject.DivKost%2 RowObject.DivKost%1 ~
RowObject.Rab3Kr2 RowObject.Rab3Kr1 RowObject.Rab3%1 RowObject.Rab3%2 ~
RowObject.VareKost1 RowObject.VareKost2 RowObject.DBKr1 RowObject.DBKr2 ~
RowObject.DB%1 RowObject.DB%2 RowObject.MvaKr1 RowObject.MvaKr2 ~
RowObject.Mva%2 RowObject.Mva%1 RowObject.Pris1 RowObject.Pris2 ~
RowObject.EuroPris2 RowObject.EuroPris1 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Gjeldende FI-Tilbud RS-Kampanje ~
FI-Kampanje FI-Profil FI-Kronor FI-Procent 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Gjeldende AS CHARACTER FORMAT "X(256)":U INITIAL "  Gjeldende kalkyle" 
      VIEW-AS TEXT 
     SIZE 46.6 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kampanje AS CHARACTER FORMAT "X(256)":U INITIAL "  Kampanjekalkyle mot pristabell" 
      VIEW-AS TEXT 
     SIZE 46.6 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kronor AS CHARACTER FORMAT "X(256)":U INITIAL "Kronor" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Procent AS CHARACTER FORMAT "X(256)":U INITIAL "Procent" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Profil AS CHARACTER FORMAT "X(256)":U INITIAL "  Prisprofil" 
      VIEW-AS TEXT 
     SIZE 46.6 BY .62
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tilbud AS CHARACTER FORMAT "X(256)":U INITIAL "  Artikkel er på tilbud" 
      VIEW-AS TEXT 
     SIZE 46.6 BY .62
     BGCOLOR 12 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE RS-Kampanje AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ordinær", no,
"Tilbud", yes
     SIZE 12 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.6 BY .1.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.6 BY .1.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.6 BY .1.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 22.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FI-Gjeldende AT ROW 1 COL 1 NO-LABEL
     FI-Tilbud AT ROW 1 COL 1 NO-LABEL
     RowObject.Tilbud AT ROW 1.95 COL 2.2 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Ordinær", no,
"Tilbud", yes
          SIZE 14.8 BY 1.43
     RowObject.AktivFraDato AT ROW 2.14 COL 16.8 COLON-ALIGNED NO-LABEL FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RowObject.TilbudTilDato AT ROW 2.14 COL 31 COLON-ALIGNED NO-LABEL FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RS-Kampanje AT ROW 4.24 COL 4 NO-LABEL
     RowObject.ProfilNr AT ROW 7.19 COL 13.4 COLON-ALIGNED HELP
          "Prisprofil"
          LABEL "Prisprofil" FORMAT "->>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.InnkjopsPris1 AT ROW 10.48 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.InnkjopsPris2 AT ROW 10.48 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab1Kr1 AT ROW 11.52 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab1Kr2 AT ROW 11.52 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab1%2 AT ROW 11.52 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Rab1%1 AT ROW 11.52 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Rab2Kr2 AT ROW 12.57 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab2Kr1 AT ROW 12.57 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab2%2 AT ROW 12.57 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Rab2%1 AT ROW 12.57 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Frakt1 AT ROW 13.62 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Frakt2 AT ROW 13.62 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Frakt%2 AT ROW 13.62 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Frakt%1 AT ROW 13.62 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.DivKostKr1 AT ROW 14.67 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.DivKostKr2 AT ROW 14.67 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.DivKost%2 AT ROW 14.67 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.DivKost%1 AT ROW 14.67 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Rab3Kr2 AT ROW 15.71 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab3Kr1 AT ROW 15.71 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Rab3%1 AT ROW 15.71 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Rab3%2 AT ROW 15.71 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.VareKost1 AT ROW 17.1 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.VareKost2 AT ROW 17.1 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.DBKr1 AT ROW 18.14 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.DBKr2 AT ROW 18.14 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.DB%1 AT ROW 18.14 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.DB%2 AT ROW 18.14 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.MvaKr1 AT ROW 19.19 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.MvaKr2 AT ROW 19.19 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Mva%2 AT ROW 19.19 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Mva%1 AT ROW 19.19 COL 29.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     RowObject.Pris1 AT ROW 20.62 COL 13.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Pris2 AT ROW 20.62 COL 13.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.EuroPris2 AT ROW 21.76 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.EuroPris1 AT ROW 21.76 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     FI-Kampanje AT ROW 3.52 COL 1 NO-LABEL
     FI-Profil AT ROW 6.24 COL 1 NO-LABEL
     FI-Kronor AT ROW 9.57 COL 14.6 COLON-ALIGNED NO-LABEL
     FI-Procent AT ROW 9.57 COL 30 COLON-ALIGNED NO-LABEL
     RECT-42 AT ROW 10.29 COL 1
     RECT-43 AT ROW 16.91 COL 1
     RECT-44 AT ROW 20.43 COL 1
     RECT-55 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sdo/dartpris.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dartpris.i}
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
         HEIGHT             = 22.62
         WIDTH              = 52.2.
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

/* SETTINGS FOR FILL-IN RowObject.AktivFraDato IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FI-Gjeldende IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Kampanje IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Profil IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Tilbud IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.ProfilNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       RowObject.ProfilNr:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RADIO-SET RowObject.Tilbud IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.TilbudTilDato IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME RS-Kampanje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Kampanje vTableWin
ON VALUE-CHANGED OF RS-Kampanje IN FRAME F-Main
DO:
  IF SELF:SCREEN-VALUE = "no" THEN
      RUN FI-MoveToTop(1).
  ELSE
      RUN FI-MoveToTop(2).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FI-MoveToTop vTableWin 
PROCEDURE FI-MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipVilken AS INTEGER    NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF ipVilken = 1 THEN DO:
       RowObject.InnkjopsPris1:MOVE-TO-TOP().
       RowObject.Rab1Kr1:MOVE-TO-TOP().
       RowObject.Rab1%1:MOVE-TO-TOP().
       RowObject.Rab2Kr1:MOVE-TO-TOP().
       RowObject.Rab2%1:MOVE-TO-TOP().
       RowObject.Frakt1:MOVE-TO-TOP().
       RowObject.Frakt%1:MOVE-TO-TOP().
       RowObject.DivKostKr1:MOVE-TO-TOP().
       RowObject.DivKost%1:MOVE-TO-TOP().
       RowObject.Rab3Kr1:MOVE-TO-TOP().
       RowObject.Rab3%1:MOVE-TO-TOP().
       RowObject.VareKost1:MOVE-TO-TOP().
       RowObject.DBKr1:MOVE-TO-TOP().
       RowObject.DB%1:MOVE-TO-TOP().
       RowObject.MvaKr1:MOVE-TO-TOP().
       RowObject.Mva%1:MOVE-TO-TOP().
       RowObject.Pris1:MOVE-TO-TOP().
       RowObject.EuroPris1:MOVE-TO-TOP().
   END.
   ELSE DO:
       RowObject.InnkjopsPris2:MOVE-TO-TOP().
       RowObject.Rab1Kr2:MOVE-TO-TOP().
       RowObject.Rab1%2:MOVE-TO-TOP().
       RowObject.Rab2Kr2:MOVE-TO-TOP().
       RowObject.Rab2%2:MOVE-TO-TOP().
       RowObject.Frakt2:MOVE-TO-TOP().
       RowObject.Frakt%2:MOVE-TO-TOP().
       RowObject.DivKostKr2:MOVE-TO-TOP().
       RowObject.DivKost%2:MOVE-TO-TOP().
       RowObject.Rab3Kr2:MOVE-TO-TOP().
       RowObject.Rab3%2:MOVE-TO-TOP().
       RowObject.VareKost2:MOVE-TO-TOP().
       RowObject.DBKr2:MOVE-TO-TOP().
       RowObject.DB%2:MOVE-TO-TOP().
       RowObject.MvaKr2:MOVE-TO-TOP().
       RowObject.Mva%2:MOVE-TO-TOP().
       RowObject.Pris2:MOVE-TO-TOP().
       RowObject.EuroPris2:MOVE-TO-TOP().
   END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KampanjVerdier vTableWin 
PROCEDURE KampanjVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipPris AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  dEuroKurs AS DECIMAL NO-UNDO.

  /*IF AVAILABLE RowObject THEN */
  DO:
    DO WITH FRAME {&FRAME-NAME}:
      IF RowObject.TilBud:SCREEN-VALUE = "no" THEN DO:
        FI-Gjeldende:MOVE-TO-TOP().
        ASSIGN RowObject.TilbudTilDato:SCREEN-VALUE = "".
      END.
      ELSE
        FI-Tilbud:MOVE-TO-TOP().
      ASSIGN RS-Kampanje:SCREEN-VALUE = "no"
             RS-Kampanje:SENSITIVE = NOT (DECI(RowObject.InnKjopsPris2:SCREEN-VALUE) = 0 AND 
         DECI(RowObject.VareKost2:SCREEN-VALUE) = 0).
      RUN FI-MoveToTop(1).
      ASSIGN dEuroKurs = DECI(RowObject.EuroPris1:SCREEN-VALUE) / DECI(RowObject.Pris1:SCREEN-VALUE)
             RowObject.Pris1:SCREEN-VALUE  = STRING(ipPris)
             RowObject.MvaKr1:SCREEN-VALUE = STRING(ipPris * DECI(RowObject.Mva%1:SCREEN-VALUE) / (100 + DECI(RowObject.Mva%1:SCREEN-VALUE)))
             RowObject.DBKr1:SCREEN-VALUE  = STRING(ipPris - DECI(RowObject.MvaKr1:SCREEN-VALUE) -
                                              DECI(RowObject.VareKost1:SCREEN-VALUE))
             RowObject.DB%1:SCREEN-VALUE   = STRING(DECI(RowObject.DBKr1:SCREEN-VALUE) / 
                                              (ipPris - DECI(RowObject.MvaKr1:SCREEN-VALUE)) * 100)
             RowObject.EuroPris1:SCREEN-VALUE = STRING(dEuroKurs * ipPris).
      ASSIGN dEuroKurs = DECI(RowObject.EuroPris2:SCREEN-VALUE) / DECI(RowObject.Pris2:SCREEN-VALUE)
             RowObject.Pris2:SCREEN-VALUE  = STRING(ipPris)
             RowObject.MvaKr2:SCREEN-VALUE = STRING(ipPris * DECI(RowObject.Mva%2:SCREEN-VALUE) / (100 + DECI(RowObject.Mva%2:SCREEN-VALUE)))
             RowObject.DBKr2:SCREEN-VALUE  = STRING(ipPris - DECI(RowObject.MvaKr2:SCREEN-VALUE) -
                                              DECI(RowObject.VareKost2:SCREEN-VALUE))
             RowObject.DB%2:SCREEN-VALUE   = STRING(DECI(RowObject.DBKr2:SCREEN-VALUE) / 
                                              (ipPris - DECI(RowObject.MvaKr2:SCREEN-VALUE)) * 100)
             RowObject.EuroPris2:SCREEN-VALUE = STRING(dEuroKurs * ipPris).
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

