&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dbonghode.i"}.



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
DEF VAR h_dbonghode  AS HANDLE NO-UNDO.
DEF VAR hh_dbonghode AS HANDLE NO-UNDO.

DEFINE VARIABLE hFirstNext AS HANDLE     NO-UNDO.
DEFINE VARIABLE iButikkNr LIKE BongHode.ButikkNr NO-UNDO.
DEFINE VARIABLE iGruppeNr LIKE BongHode.GruppeNr NO-UNDO.
DEFINE VARIABLE iKasseNr  LIKE BongHode.KasseNr  NO-UNDO.
DEFINE VARIABLE dDato     LIKE BongHode.Dato     NO-UNDO.
DEFINE VARIABLE iBongNr   LIKE BongHode.BongNr   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dbonghode.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Belop RowObject.KundeNavn ~
RowObject.KortType RowObject.Makulert RowObject.ButikkNr ~
RowObject.Gradering 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-55 B-Xprint RECT-56 B-Forrige B-Neste 
&Scoped-Define DISPLAYED-FIELDS RowObject.BongNr RowObject.Dato ~
RowObject.KassererNr RowObject.KassererNavn RowObject.Belop ~
RowObject.SelgerNr RowObject.SelgerNavn RowObject.fuKl ~
RowObject.OverforingsNr RowObject.KundeKort RowObject.KundeNr ~
RowObject.KundeNavn RowObject.KortType RowObject.Makulert ~
RowObject.ButikkNr RowObject.DataSettId RowObject.Gradering ~
RowObject.MedlemsKort RowObject.MedlemsNr RowObject.MedlemNavn 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Nummer FI-Navn FI-Kort 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Forrige 
     LABEL "&Forrige" 
     SIZE 11 BY 1.

DEFINE BUTTON B-Neste 
     LABEL "&Neste" 
     SIZE 11 BY 1.

DEFINE BUTTON B-Xprint 
     IMAGE-UP FILE "icon/e-print.bmp":U NO-FOCUS
     LABEL "Print" 
     SIZE 4.6 BY 1.05 TOOLTIP "Print bongkopi".

DEFINE VARIABLE FI-Kort AS CHARACTER FORMAT "X(256)":U INITIAL "KortNr" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U INITIAL "Navn" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Nummer AS CHARACTER FORMAT "X(256)":U INITIAL "Nummer" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 158 BY 5.19.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 24 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B-Xprint AT ROW 1.86 COL 89
     RowObject.BongNr AT ROW 1.86 COL 11.6 COLON-ALIGNED
          LABEL "Bong"
          VIEW-AS FILL-IN 
          SIZE 22.4 BY 1
     RowObject.Dato AT ROW 1.86 COL 44.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     B-Forrige AT ROW 1.86 COL 64.8
     B-Neste AT ROW 1.86 COL 75.8
     RowObject.KassererNr AT ROW 1.86 COL 110.4 COLON-ALIGNED
          LABEL "Kasserer"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.KassererNavn AT ROW 1.86 COL 131 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     RowObject.Belop AT ROW 2.91 COL 11.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22.4 BY 1
     RowObject.SelgerNr AT ROW 2.91 COL 110.4 COLON-ALIGNED
          LABEL "Selger"
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.SelgerNavn AT ROW 2.91 COL 131 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     RowObject.fuKl AT ROW 2.95 COL 44.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.OverforingsNr AT ROW 3.95 COL 11.6 COLON-ALIGNED
          LABEL "Ov.nr"
          VIEW-AS FILL-IN 
          SIZE 22.4 BY 1
     RowObject.KundeKort AT ROW 3.95 COL 86 COLON-ALIGNED
          LABEL "Kunde"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     RowObject.KundeNr AT ROW 3.95 COL 110.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.KundeNavn AT ROW 3.95 COL 131 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     RowObject.KortType AT ROW 4 COL 44.2 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "[Alle]",0,
                     "Ingen",1,
                     "Kundekort",2,
                     "Medlemskort",3
          DROP-DOWN-LIST
          SIZE 18 BY 1
     RowObject.Makulert AT ROW 5 COL 55 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     RowObject.ButikkNr AT ROW 5 COL 64.8 COLON-ALIGNED
          LABEL "But"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.DataSettId AT ROW 5.05 COL 11.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22.4 BY 1
     RowObject.Gradering AT ROW 5.05 COL 44.2 COLON-ALIGNED
          LABEL "Feilkode"
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.MedlemsKort AT ROW 5.05 COL 86 COLON-ALIGNED
          LABEL "Medlem"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     RowObject.MedlemsNr AT ROW 5.05 COL 110.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     RowObject.MedlemNavn AT ROW 5.05 COL 131 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     FI-Nummer AT ROW 1.24 COL 110.8 COLON-ALIGNED NO-LABEL
     FI-Navn AT ROW 1.24 COL 131 COLON-ALIGNED NO-LABEL
     FI-Kort AT ROW 3.24 COL 87 COLON-ALIGNED NO-LABEL
     RECT-55 AT ROW 1.05 COL 1
     RECT-56 AT ROW 1.62 COL 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dbonghode.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dbonghode.i}
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
         HEIGHT             = 5.29
         WIDTH              = 158.2.
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

/* SETTINGS FOR FILL-IN RowObject.BongNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.ButikkNr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.DataSettId IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Dato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kort IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Navn IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Nummer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.fuKl IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuKl:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Gradering IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.KassererNavn IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.KassererNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.KundeKort IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.KundeNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.MedlemNavn IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.MedlemsKort IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.MedlemsNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OverforingsNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.SelgerNavn IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.SelgerNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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

&Scoped-define SELF-NAME B-Forrige
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Forrige vTableWin
ON CHOOSE OF B-Forrige IN FRAME F-Main /* Forrige */
DO:
    /* flyttat till inizialize.... */
/*     ASSIGN                                                                */
/*         hh_dbonghode = DYNAMIC-FUNCTION('getDataSource':U IN h_dBongHode) */
/*         .                                                                 */
    IF VALID-HANDLE(hFirstNext) THEN
        RUN BytPost ("Prev").
    ELSE
        RUN FetchPrev IN hh_dbonghode.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Neste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Neste vTableWin
ON CHOOSE OF B-Neste IN FRAME F-Main /* Neste */
DO:
/* flyttat till initialize.... */
/*   ASSIGN                                                                */
/*       hh_dbonghode = DYNAMIC-FUNCTION('getDataSource':U IN h_dBongHode) */
/*       .                                                                 */
    IF VALID-HANDLE(hFirstNext) THEN
        RUN BytPost ("Next").
    ELSE
        RUN FetchNext IN hh_dbonghode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Xprint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Xprint vTableWin
ON CHOOSE OF B-Xprint IN FRAME F-Main /* Print */
DO:
    RUN skrivbongkopi.p (ENTRY(1,ENTRY(2,DYNAMIC-FUNCTION('colValues':U IN DYNAMIC-FUNCTION('getDataSource':U),
     INPUT "BongNr" /* CHARACTER */)),CHR(1)),
     FALSE /* direktutskrift */,
     "" /* printer */,
     "" /* mailadress */).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytPost vTableWin 
PROCEDURE BytPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRettning AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE oiButikkNr LIKE BongHode.ButikkNr NO-UNDO.
    DEFINE VARIABLE oiGruppeNr LIKE BongHode.GruppeNr NO-UNDO.
    DEFINE VARIABLE oiKasseNr  LIKE BongHode.KasseNr  NO-UNDO.
    DEFINE VARIABLE odDato     LIKE BongHode.Dato     NO-UNDO.
    DEFINE VARIABLE oiBongNr   LIKE BongHode.BongNr   NO-UNDO.
    DEFINE VARIABLE pcWhere    AS CHARACTER  NO-UNDO.

    RUN GetRecord IN hFirstNext (cRettning,OUTPUT oiButikkNr,OUTPUT oiGruppeNr,OUTPUT oiKasseNr,OUTPUT odDato,OUTPUT oiBongNr).
    IF odDato <> ? AND (oiButikkNr <> iButikkNr OR oiGruppeNr <> iGruppeNr OR 
                        oiKasseNr <> iKasseNr OR odDato <> dDato OR oiBongNr <> iBongNr) THEN DO:
      ASSIGN iButikkNr = oiButikkNr
             iGruppeNr = oiGruppeNr
             iKasseNr  = oiKasseNr 
             dDato     = odDato    
             iBongNr   = oiBongNr  
             pcWhere = "ButikkNr = " + STRING(iButikkNr) +
                       " AND GruppeNr = " + STRING(iGruppeNr)   +
                       " AND KasseNr  = " + STRING(iKasseNr)    +
                       " AND Dato     = " + STRING(dDato)       +
                       " AND BongNr   = " + STRING(iBongNr).
          DYNAMIC-FUNCTION('setQueryWhere':U IN hh_dbonghode,
         INPUT pcWhere /* CHARACTER */).
         DYNAMIC-FUNCTION('openQuery':U IN hh_dbonghode).
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
  ASSIGN
      h_dbonghode = DYNAMIC-FUNCTION('getDataSource':U)
      .

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      hh_dbonghode = DYNAMIC-FUNCTION('getDataSource':U IN h_dBongHode)
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFirstNextHandle vTableWin 
PROCEDURE setFirstNextHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iphFirstNext AS HANDLE     NO-UNDO.
    ASSIGN hFirstNext = iphFirstNext.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

