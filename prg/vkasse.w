&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dkasse.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dkasse.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.FakturaLayout RowObject.Navn ~
RowObject.Aktiv RowObject.ModellNr RowObject.ElJournal1 ~
RowObject.ElJournalOperand RowObject.ElJournal2 RowObject.ElJournalId ~
RowObject.ElJournalKatalog RowObject.ElJournalKonv RowObject.ElJournalAktiv ~
RowObject.ElJournalInnles RowObject.ElJournalBehandle RowObject.Kvittering1 ~
RowObject.KvitteringOperand RowObject.Kvittering2 RowObject.KvitteringId ~
RowObject.KvitteringKatalog RowObject.KvitteringKonv ~
RowObject.KvitteringAktiv RowObject.KvitteringInnles ~
RowObject.KvitteringBehandle RowObject.Utskriftskopi1 ~
RowObject.UtskriftsKopiOperand RowObject.Utskriftskopi2 ~
RowObject.UtskriftsKopiId RowObject.UtskriftskopiKatalog ~
RowObject.UTskriftskopiKonv RowObject.UtskriftskopiAktiv ~
RowObject.UtskriftskopiInnles RowObject.UtskriftskopiBehandle ~
RowObject.DagsOpgj1 RowObject.DagsOppgjOperand RowObject.DagsOpgj2 ~
RowObject.DagsOppgjId RowObject.DagsOppgjKatalog RowObject.DagsOppgjKonv ~
RowObject.DagsOppgjAktiv RowObject.DagsOppgjInnles ~
RowObject.DagsOppgjBehandle RowObject.KassererOpgj1 ~
RowObject.KassererOppgjOperand RowObject.KassererOpgj2 ~
RowObject.KassererOppgjId RowObject.KassererOppgjKatalog ~
RowObject.KassererOppgjKonv RowObject.KassererOppgjAktiv ~
RowObject.KassererOppgjInnles RowObject.KassererOppgjBehandle ~
RowObject.FakturaKopi RowObject.Fakturaskriver 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS FI-Kasse FI-FilTekst FI-Kvittering ~
FI-Utskriftskopi FI-Dags FI-KassererO BUTTON-SokBut-3 FI-Faktura RECT-1 ~
RECT-2 RECT-3 
&Scoped-Define DISPLAYED-FIELDS RowObject.FakturaLayout RowObject.ButikkNr ~
RowObject.fuKortNavn RowObject.GruppeNr RowObject.fuGruppeNavn ~
RowObject.KasseNr RowObject.Navn RowObject.Aktiv RowObject.ModellNr ~
RowObject.ElJournal1 RowObject.ElJournalOperand RowObject.ElJournal2 ~
RowObject.ElJournalId RowObject.ElJournalKatalog RowObject.ElJournalKonv ~
RowObject.ElJournalAktiv RowObject.ElJournalInnles ~
RowObject.ElJournalBehandle RowObject.Kvittering1 ~
RowObject.KvitteringOperand RowObject.Kvittering2 RowObject.KvitteringId ~
RowObject.KvitteringKatalog RowObject.KvitteringKonv ~
RowObject.KvitteringAktiv RowObject.KvitteringInnles ~
RowObject.KvitteringBehandle RowObject.Utskriftskopi1 ~
RowObject.UtskriftsKopiOperand RowObject.Utskriftskopi2 ~
RowObject.UtskriftsKopiId RowObject.UtskriftskopiKatalog ~
RowObject.UTskriftskopiKonv RowObject.UtskriftskopiAktiv ~
RowObject.UtskriftskopiInnles RowObject.UtskriftskopiBehandle ~
RowObject.DagsOpgj1 RowObject.DagsOppgjOperand RowObject.DagsOpgj2 ~
RowObject.DagsOppgjId RowObject.DagsOppgjKatalog RowObject.DagsOppgjKonv ~
RowObject.DagsOppgjAktiv RowObject.DagsOppgjInnles ~
RowObject.DagsOppgjBehandle RowObject.KassererOpgj1 ~
RowObject.KassererOppgjOperand RowObject.KassererOpgj2 ~
RowObject.KassererOppgjId RowObject.KassererOppgjKatalog ~
RowObject.KassererOppgjKonv RowObject.KassererOppgjAktiv ~
RowObject.KassererOppgjInnles RowObject.KassererOppgjBehandle ~
RowObject.FakturaKopi RowObject.Fakturaskriver 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Kasse FI-FilTekst FI-ElJournal ~
FI-Operand FI-Kvittering FI-Utskriftskopi FI-Dags FI-KassererO FI-Faktura 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-SokBut-3 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i butikkregisteret".

DEFINE VARIABLE FI-Dags AS CHARACTER FORMAT "X(256)":U INITIAL "Dagsoppgjør" 
      VIEW-AS TEXT 
     SIZE 24 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-ElJournal AS CHARACTER FORMAT "X(256)":U INITIAL "ElJournal" 
      VIEW-AS TEXT 
     SIZE 13 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Faktura AS CHARACTER FORMAT "X(256)":U INITIAL "Faktura" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FilTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Oppsett av filtyper for kassen" 
      VIEW-AS TEXT 
     SIZE 69 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kasse AS CHARACTER FORMAT "X(256)":U INITIAL "Kasse" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KassererO AS CHARACTER FORMAT "X(256)":U INITIAL "Kassereroppgjør" 
      VIEW-AS TEXT 
     SIZE 24 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kvittering AS CHARACTER FORMAT "X(256)":U INITIAL "Kvittering" 
      VIEW-AS TEXT 
     SIZE 26 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Operand AS CHARACTER FORMAT "X(256)":U INITIAL "Operand" 
      VIEW-AS TEXT 
     SIZE 10 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Utskriftskopi AS CHARACTER FORMAT "X(256)":U INITIAL "Utskriftskopi" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 156 BY 9.29.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 5.71.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59 BY 5.71.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.FakturaLayout AT ROW 5.14 COL 114 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 33 BY 1
     RowObject.ButikkNr AT ROW 1.95 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.fuKortNavn AT ROW 1.95 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.GruppeNr AT ROW 2.95 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     RowObject.fuGruppeNavn AT ROW 2.95 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.KasseNr AT ROW 4.81 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     RowObject.Navn AT ROW 4.81 COL 24 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     RowObject.Aktiv AT ROW 4.86 COL 59
          VIEW-AS TOGGLE-BOX
          SIZE 16 BY .81
     RowObject.ModellNr AT ROW 6 COL 17 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 39 BY 1
     RowObject.ElJournal1 AT ROW 9.33 COL 13 COLON-ALIGNED
          LABEL "Prefiks"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.ElJournalOperand AT ROW 9.33 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 20
          LIST-ITEM-PAIRS "Lik",1,
                     "Starter",2,
                     "Slutter",3,
                     "Inneholder",4
          DROP-DOWN-LIST
          SIZE 13 BY 1
     RowObject.ElJournal2 AT ROW 10.29 COL 13 COLON-ALIGNED
          LABEL "Ekstent"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.ElJournalId AT ROW 11.24 COL 13 COLON-ALIGNED
          LABEL "Identifisering"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ElJournalKatalog AT ROW 12.19 COL 13 COLON-ALIGNED
          LABEL "Katalog"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ElJournalKonv AT ROW 13.38 COL 15
          LABEL "Konverter"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.ElJournalAktiv AT ROW 14.33 COL 15
          LABEL "Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RowObject.ElJournalInnles AT ROW 15.29 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.ElJournalBehandle AT ROW 16.24 COL 13 COLON-ALIGNED
          LABEL "Behandling"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.Kvittering1 AT ROW 9.33 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.KvitteringOperand AT ROW 9.33 COL 54 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Lik",1,
                     "Starter",2,
                     "Slutter",3,
                     "Inneholder",4
          DROP-DOWN-LIST
          SIZE 13 BY 1
     RowObject.Kvittering2 AT ROW 10.29 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.KvitteringId AT ROW 11.24 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.KvitteringKatalog AT ROW 12.19 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.KvitteringKonv AT ROW 13.38 COL 42
          LABEL "Konverter"
          VIEW-AS TOGGLE-BOX
          SIZE 28 BY .81
     RowObject.KvitteringAktiv AT ROW 14.33 COL 42
          LABEL "Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RowObject.KvitteringInnles AT ROW 15.29 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.KvitteringBehandle AT ROW 16.24 COL 40 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     RowObject.Utskriftskopi1 AT ROW 9.33 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.UtskriftsKopiOperand AT ROW 9.33 COL 81 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Lik",1,
                     "Starter",2,
                     "Slutter",3,
                     "Inneholder",4
          DROP-DOWN-LIST
          SIZE 12 BY 1
     RowObject.Utskriftskopi2 AT ROW 10.29 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.UtskriftsKopiId AT ROW 11.24 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.UtskriftskopiKatalog AT ROW 12.19 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.UTskriftskopiKonv AT ROW 13.38 COL 69
          LABEL "Konverter"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.UtskriftskopiAktiv AT ROW 14.33 COL 69
          LABEL "Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.UtskriftskopiInnles AT ROW 15.29 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.UtskriftskopiBehandle AT ROW 16.24 COL 67 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.DagsOpgj1 AT ROW 9.33 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.DagsOppgjOperand AT ROW 9.33 COL 107 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Lik",1,
                     "Starter",2,
                     "Slutter",3,
                     "Inneholder",4
          DROP-DOWN-LIST
          SIZE 12 BY 1
     RowObject.DagsOpgj2 AT ROW 10.29 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.DagsOppgjId AT ROW 11.24 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.DagsOppgjKatalog AT ROW 12.19 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.DagsOppgjKonv AT ROW 13.38 COL 95
          LABEL "Konverter"
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .81
     RowObject.DagsOppgjAktiv AT ROW 14.33 COL 95
          LABEL "Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.DagsOppgjInnles AT ROW 15.29 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.DagsOppgjBehandle AT ROW 16.24 COL 93 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.KassererOpgj1 AT ROW 9.33 COL 119 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.KassererOppgjOperand AT ROW 9.33 COL 133 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Lik",1,
                     "Starter",2,
                     "Slutter",3,
                     "Inneholder",4
          DROP-DOWN-LIST
          SIZE 12 BY 1
     RowObject.KassererOpgj2 AT ROW 10.29 COL 119 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.KassererOppgjId AT ROW 11.24 COL 119 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.KassererOppgjKatalog AT ROW 12.19 COL 119 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.KassererOppgjKonv AT ROW 13.38 COL 121
          LABEL "Konverter"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .81
     RowObject.KassererOppgjAktiv AT ROW 14.33 COL 121
          LABEL "Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     RowObject.KassererOppgjInnles AT ROW 15.29 COL 119 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     RowObject.KassererOppgjBehandle AT ROW 16.24 COL 119 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     FI-Kasse AT ROW 1 COL 2 NO-LABEL
     FI-FilTekst AT ROW 7.67 COL 2 NO-LABEL
     FI-ElJournal AT ROW 8.62 COL 13.4 COLON-ALIGNED NO-LABEL
     FI-Operand AT ROW 8.62 COL 27.2 COLON-ALIGNED NO-LABEL
     FI-Kvittering AT ROW 8.62 COL 42 NO-LABEL
     FI-Utskriftskopi AT ROW 8.62 COL 69 NO-LABEL
     FI-Dags AT ROW 8.62 COL 95 NO-LABEL
     FI-KassererO AT ROW 8.62 COL 121 NO-LABEL
     RowObject.FakturaKopi AT ROW 3 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     RowObject.Fakturaskriver AT ROW 4.05 COL 114 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     BUTTON-SokBut-3 AT ROW 4.05 COL 148.4 NO-TAB-STOP 
     FI-Faktura AT ROW 1 COL 97.2 NO-LABEL
     RECT-1 AT ROW 8.38 COL 1
     RECT-2 AT ROW 1.71 COL 1
     RECT-3 AT ROW 1.71 COL 97
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dkasse.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dkasse.i}
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
         HEIGHT             = 16.71
         WIDTH              = 156.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.ButikkNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.DagsOppgjAktiv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.DagsOppgjKonv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.ElJournal1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.ElJournal2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.ElJournalAktiv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.ElJournalBehandle IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.ElJournalId IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.ElJournalKatalog IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.ElJournalKonv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FI-Dags IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-ElJournal IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Faktura IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-FilTekst IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Kasse IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-KassererO IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Kvittering IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Operand IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Utskriftskopi IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN RowObject.fuGruppeNavn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuGruppeNavn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuKortNavn IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuKortNavn:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.GruppeNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.KasseNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.KassererOppgjAktiv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.KassererOppgjKonv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.KvitteringAktiv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.KvitteringKonv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.UtskriftskopiAktiv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.UTskriftskopiKonv IN FRAME F-Main
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

&Scoped-define SELF-NAME BUTTON-SokBut-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut-3 vTableWin
ON CHOOSE OF BUTTON-SokBut-3 IN FRAME F-Main /* ... */
or F10 of RowObject.Fakturaskriver
DO:
  DEFINE VARIABLE cValgtVerdi    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrinter       AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-1:
      DO iCount = 1 TO NUM-ENTRIES(SESSION:GET-PRINTERS()):
          ASSIGN cPrinter = ENTRY(iCount,SESSION:GET-PRINTERS())
                 cListItemPairs = cListItemPairs + (IF cListItemPairs <> "" THEN "," ELSE "") + 
                                    cPrinter + "," + STRING(iCount).
                 IF cPrinter = RowObject.Fakturaskriver:SCREEN-VALUE THEN
                     cValgtVerdi = STRING(iCount).
      END.
      RUN d-VelgGenerellCombo.w ("Velg printer",cListItemPairs,INPUT-OUTPUT cValgtVerdi).
      IF NOT cValgtVerdi = "" THEN DO:
          ASSIGN RowObject.Fakturaskriver:SCREEN-VALUE = ENTRY(int(cValgtVerdi),SESSION:GET-PRINTERS())
                 RowObject.Fakturaskriver:MODIFIED     = TRUE.
          APPLY "TAB" TO RowObject.Fakturaskriver.
      END.
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
      RowObject.KasseNr:SENSITIVE = TRUE
      RowObject.ButikkNr:SENSITIVE = TRUE
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
        RowObject.KasseNr:SENSITIVE = FALSE
        RowObject.ButikkNr:SENSITIVE = FALSE
        .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
          MESSAGE "Det er gjort endringer på posten." SKIP
                  "Disse må lagres eller kanseleres før programmet kan avsluttes."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN
              plCancel = TRUE /* Flagger at avsluttning skal avbrytes */
              .
          RETURN NO-APPLY.
      END.
  END.
  
  RUN SUPER( INPUT-OUTPUT plCancel).

  /* Code placed here will execute AFTER standard behavior.    */

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
      RowObject.KasseNr:SENSITIVE = TRUE
      RowObject.ButikkNr:SENSITIVE = TRUE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCBModell vTableWin 
PROCEDURE InitCBModell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR h_dkasse AS HANDLE NO-UNDO.
  DEF VAR pcListe  AS CHAR   NO-UNDO.
  ASSIGN
      h_dkasse = DYNAMIC-FUNCTION('getDataSource':U)
      .
  RUN ModellListe IN h_dkasse (OUTPUT pcListe).

  ASSIGN
      pcListe = IF pcListe = "" 
                  THEN "SkoTex POS 1.0,1"
                  ELSE pcListe
      RowObject.ModellNr:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = pcListe
      RowObject.ModellNr:SCREEN-VALUE = "1"
      .

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
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  ASSIGN cListItemPairs = DYNAMIC-FUNCTION('getLayoutListItemPairs':U IN DYNAMIC-FUNCTION('getDataSource':U)).
  /* Code placed here will execute PRIOR to standard behavior. */
  IF cListItemPairs <> "" THEN
      ASSIGN RowObject.FakturaLayout:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
          DYNAMIC-FUNCTION('getLayoutListItemPairs':U IN DYNAMIC-FUNCTION('getDataSource':U)).
  RUN InitCBModell.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
/*   RUN InitCBModell.  */

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
    IF RowObject.KasseNr:SENSITIVE = TRUE THEN
      RUN ApplyEntry ("KasseNr").
    ELSE
      RUN ApplyEntry ("Navn").
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
        RowObject.KasseNr:SENSITIVE = FALSE
        RowObject.ButikkNr:SENSITIVE = FALSE
        .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

