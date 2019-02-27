&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dkampanjehode.i"}.



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
  DEFINE VARIABLE hDataSource AS HANDLE     NO-UNDO.
  DEFINE VARIABLE iCL      LIKE Butiker.Butik NO-UNDO.
  DEFINE VARIABLE iProfilNr LIKE PrisProfil.ProfilNr    NO-UNDO.
  DEF VAR ipProfilNr AS INT NO-UNDO.

  DEF VAR hUtvalg         AS HANDLE NO-UNDO.
  DEF VAR bUtvalgIsMaster AS LOG    NO-UNDO.
  DEFINE VARIABLE hWindow AS HANDLE     NO-UNDO.
  DEF VAR bEndretRab AS LOG NO-UNDO.

  DEFINE VARIABLE pcKeyValues AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValues     AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "sdo/dkampanjehode.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.ProfilNr ~
RowObject.LeverandorKampanje RowObject.NormalPris RowObject.AvslagType ~
RowObject.Kamp% RowObject.Beskrivelse RowObject.StartDato ~
RowObject.SluttDato RowObject.Notat RowObject.KampanjePris 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RS-KampType btnHentFraUtvalg FI-FraTid ~
FI-TilTid B-Overfor B-Komplett B-Aaterstall B-Gjennbruk 
&Scoped-Define DISPLAYED-FIELDS RowObject.ProfilNr ~
RowObject.LeverandorKampanje RowObject.NormalPris RowObject.AvslagType ~
RowObject.KampanjeId RowObject.Kamp% RowObject.Aktivert RowObject.Komplett ~
RowObject.Beskrivelse RowObject.StartDato RowObject.SluttDato ~
RowObject.Notat RowObject.AktiveresTid RowObject.GyldigTilTid ~
RowObject.KampanjePris 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-KamptypeTekst RS-KampType FI-FraTid ~
FI-TilTid 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKampanjeId vTableWin 
FUNCTION getKampanjeId RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNoResizeXfields vTableWin 
FUNCTION getNoResizeXfields RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitUtvalgToKampanje vTableWin 
FUNCTION InitUtvalgToKampanje RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButtons vTableWin 
FUNCTION setButtons RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFITid vTableWin 
FUNCTION setFITid RETURNS CHARACTER
  ( INPUT iTid AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setTid vTableWin 
FUNCTION setTid RETURNS CHARACTER
  ( INPUT hDbFillIn AS HANDLE,INPUT hUpdFillIn AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TidOk vTableWin 
FUNCTION TidOk RETURNS LOGICAL
  ( INPUT hUpdFillIn AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UtvalgIsMaster vTableWin 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dprisprofil AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Aaterstall 
     LABEL "Angre/Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Gjennbruk 
     LABEL "Gjenbruk" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Komplett 
     LABEL "Komplett" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Overfor 
     LABEL "Til priskø" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnHentFraUtvalg 
     LABEL "Hent artikler fra utvalg" 
     SIZE 26 BY 1.14.

DEFINE BUTTON BtnUtvalg 
     LABEL "Utvalg" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-FraTid AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KamptypeTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Kampanjetype" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TilTid AS DECIMAL FORMAT "99.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE RS-KampType AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Tilbudskampanje", 1,
"Normalprisendring", 2,
"Leverandørkampanje", 3
     SIZE 35 BY 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.ProfilNr AT ROW 3.14 COL 12 COLON-ALIGNED HELP
          "Prisprofil"
          LABEL "Prisprofil" FORMAT "->>>>>>9"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 45 BY 1
     RowObject.LeverandorKampanje AT ROW 6.67 COL 128
          LABEL "Leverandørkampanje"
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     FI-KamptypeTekst AT ROW 1.05 COL 116 COLON-ALIGNED NO-LABEL
     RowObject.NormalPris AT ROW 7.52 COL 128
          LABEL "NormalPris"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     RS-KampType AT ROW 1.71 COL 117 NO-LABEL
     RowObject.AvslagType AT ROW 1.05 COL 31.4 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Prosent", 1,
"Fast pris", 2
          SIZE 24.8 BY .95
     btnHentFraUtvalg AT ROW 7.1 COL 99
     RowObject.KampanjeId AT ROW 1 COL 12 COLON-ALIGNED
          LABEL "Endringsnr"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     RowObject.Kamp% AT ROW 1 COL 72.6 COLON-ALIGNED
          LABEL "%Endring"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1 TOOLTIP "Negativ verdi: Nedsettelse av pris"
     RowObject.Aktivert AT ROW 5.1 COL 117
          LABEL "Aktivert"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .81
     RowObject.Komplett AT ROW 5.91 COL 117
          LABEL "Komplett"
          VIEW-AS TOGGLE-BOX
          SIZE 34 BY .81
     RowObject.Beskrivelse AT ROW 2.05 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     RowObject.StartDato AT ROW 2.05 COL 72.6 COLON-ALIGNED
          LABEL "Startdato/tid"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     FI-FraTid AT ROW 2.05 COL 86.6 COLON-ALIGNED NO-LABEL
     RowObject.SluttDato AT ROW 3.14 COL 72.6 COLON-ALIGNED
          LABEL "Sluttdato/tid"
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     FI-TilTid AT ROW 3.14 COL 86.6 COLON-ALIGNED NO-LABEL
     RowObject.Notat AT ROW 4.29 COL 13.8 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL
          SIZE 84.2 BY 4
     RowObject.AktiveresTid AT ROW 2.05 COL 91.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     RowObject.GyldigTilTid AT ROW 3.14 COL 91.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     B-Overfor AT ROW 2.05 COL 98.6
     B-Komplett AT ROW 3.24 COL 98.6
     BtnUtvalg AT ROW 4.43 COL 98.6
     RowObject.KampanjePris AT ROW 1 COL 96.6 COLON-ALIGNED
          LABEL "Fast pris"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     B-Aaterstall AT ROW 5.62 COL 98.6
     B-Gjennbruk AT ROW 5.62 COL 137.2 WIDGET-ID 2
     SPACE(0.20) SKIP(1.39)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "sdo/dkampanjehode.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dkampanjehode.i}
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
         HEIGHT             = 7.48
         WIDTH              = 152.2.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.AktiveresTid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Aktivert IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET RowObject.AvslagType IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       btnHentFraUtvalg:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON BtnUtvalg IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KamptypeTekst IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FI-KamptypeTekst:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.GyldigTilTid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Kamp% IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.KampanjeId IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.KampanjeId:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.KampanjePris IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Komplett IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX RowObject.LeverandorKampanje IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.LeverandorKampanje:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.NormalPris IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.NormalPris:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR EDITOR RowObject.Notat IN FRAME F-Main
   EXP-LABEL                                                            */
ASSIGN 
       RowObject.Notat:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR COMBO-BOX RowObject.ProfilNr IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.SluttDato IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.StartDato IN FRAME F-Main
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

&Scoped-define SELF-NAME RowObject.Aktivert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Aktivert vTableWin
ON RETURN OF RowObject.Aktivert IN FRAME F-Main /* Aktivert */
DO:
  APPLY "TAB":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.AvslagType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.AvslagType vTableWin
ON VALUE-CHANGED OF RowObject.AvslagType IN FRAME F-Main
DO:
  IF RowObject.AvslagType:SCREEN-VALUE = "1" THEN
    ASSIGN RowObject.Kamp%:SENSITIVE           = TRUE
           RowObject.KampanjePris:SENSITIVE    = FALSE
           RowObject.KampanjePris:SCREEN-VALUE = "0"
           .
  ELSE 
    ASSIGN RowObject.Kamp%:SENSITIVE        = FALSE
           RowObject.Kamp%:SCREEN-VALUE     = "0" 
           RowObject.KampanjePris:SENSITIVE = TRUE
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Aaterstall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Aaterstall vTableWin
ON CHOOSE OF B-Aaterstall IN FRAME F-Main /* Angre/Avbryt */
DO:
   DEFINE VARIABLE pcKeyValues AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cValues     AS CHARACTER  NO-UNDO.

   MESSAGE "Ønsker du å avslutte kampanje"
       VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
   IF NOT choice THEN
       RETURN NO-APPLY.
   cValues = ",KAMPANJE," + rowObject.KampanjeId:SCREEN-VALUE.

   IF VALID-HANDLE(hWindow) THEN
       RUN avlysKampanje IN hWindow (cValues).
   IF VALID-HANDLE(hDataSource) THEN DO:
       pcKeyValues = RowObject.KampanjeId:SCREEN-VALUE.
       DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
       DYNAMIC-FUNCTION('findRow':U IN hDataSource,
     INPUT pcKeyValues /* CHARACTER */).
/*        RETURN NO-APPLY. */
/*        RowObject.StartDato:MODIFIED = TRUE. */
/*        RUN UpdateRecord.                    */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Gjennbruk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Gjennbruk vTableWin
ON CHOOSE OF B-Gjennbruk IN FRAME F-Main /* Gjenbruk */
DO:
   MESSAGE "Ønsker du å gjenbruke kampanjene"
       VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
   IF NOT choice THEN
       RETURN NO-APPLY.
   cValues = ",GJENBRUK," + rowObject.KampanjeId:SCREEN-VALUE.

   IF VALID-HANDLE(hWindow) THEN
   DO:
       RUN avlysKampanje IN hWindow (cValues).
       IF VALID-HANDLE(hDataSource) THEN 
       DO:
           pcKeyValues = RowObject.KampanjeId:SCREEN-VALUE.
           DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
           /*DYNAMIC-FUNCTION('findRow':U IN hDataSource,
           INPUT pcKeyValues /* CHARACTER */)*/.
           APPLY 'ENTRY' TO RowObject.StartDato IN FRAME F-Main.
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Komplett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Komplett vTableWin
ON CHOOSE OF B-Komplett IN FRAME F-Main /* Komplett */
DO:
  ASSIGN RowObject.Komplett:CHECKED  IN FRAME {&FRAME-NAME} = TRUE
         RowObject.Komplett:MODIFIED IN FRAME {&FRAME-NAME} = TRUE.
  RUN updateRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Overfor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Overfor vTableWin
ON CHOOSE OF B-Overfor IN FRAME F-Main /* Til priskø */
DO:
  DEF VAR iAntOK     AS INTE    NO-UNDO.
  DEF VAR iAntLinjer AS INTE    NO-UNDO.
  DEF VAR plCancel   AS LOGI    NO-UNDO.

  IF NOT RowObject.Aktivert:CHECKED AND (DATE(RowObject.StartDato:SCREEN-VALUE) < TODAY OR 
     DATE(RowObject.StartDato:SCREEN-VALUE) > DATE(RowObject.SluttDato:SCREEN-VALUE)) THEN DO:
     MESSAGE "Feil startdato"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "ENTRY" TO RowObject.StartDato.
     RETURN NO-APPLY.
  END.

  RUN confirmExit
    ( INPUT-OUTPUT plCancel /* LOGICAL */).
  IF plCancel THEN
      RETURN NO-APPLY.
  MESSAGE "Skal artiklene overføres til priskø?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE
                              lOverfor AS LOGICAL.
   IF lOverfor THEN DO:
       RUN Aktiver IN hDataSource (INPUT INT(RowObject.KampanjeId:SCREEN-VALUE)).
/*        IF NOT lSimuler THEN DO:                                                         */
/*            ASSIGN iAntOK     = INT(ENTRY(2,RETURN-VALUE))                               */
/*                   iAntLinjer = INT(ENTRY(3,RETURN-VALUE)).                              */
/*            MESSAGE "Resultat: Godkända: " + STRING(iAntOK) + CHR(10) +                  */
/*                    "av: " + STRING(iAntLinjer) + "." VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*        END.                                                                             */
   END.
   RUN refreshRow IN hDataSource.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHentFraUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHentFraUtvalg vTableWin
ON CHOOSE OF btnHentFraUtvalg IN FRAME F-Main /* Hent artikler fra utvalg */
DO:
  DEF VAR bOK AS LOG NO-UNDO.
  IF NOT DYNAMIC-FUNCTION("columnValue" IN DYNAMIC-FUNCTION("getDataSource"),"Aktivert") AND VALID-HANDLE(hUtvalg) THEN DO:
    RUN OverfUtvalgTilKampanje IN hUtvalg(DYNAMIC-FUNCTION("columnValue" IN DYNAMIC-FUNCTION("getDataSource"),"KampanjeId"), OUTPUT bOK). 
    IF bOk THEN
      RUN selectPage IN DYNAMIC-FUNCTION("getContainerSource") (2).
  END.
  ELSE IF NOT VALID-HANDLE(hUtvalg) THEN
    DYNAMIC-FUNCTION("DoMessage",0,0,"Utvalg ikke tilgjengelig","","").
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Kampanje &1 er aktivert og kan ikke tilføres flere artikler","",DYNAMIC-FUNCTION("columnValue" IN DYNAMIC-FUNCTION("getDataSource"),"Beskrivelse")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnUtvalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnUtvalg vTableWin
ON CHOOSE OF BtnUtvalg IN FRAME F-Main /* Utvalg */
DO:
  SESSION:SET-WAIT-STATE("general").
  IF NOT VALID-HANDLE(hUtvalg) THEN DO:
    RUN wtmpartbas.w PERSIST SET hUtvalg.
    DYNAMIC-FUNCTION("setKampanjeIsMaster" IN hUtvalg,TRUE).
    RUN InitializeObject IN hUtvalg.
/*     InitUtvalgToKampanje(hUtvalg,NO). /* NO: Utvalg er IKKE master */ */
  END.
  DYNAMIC-FUNCTION("InitFromKampanje" IN hUtvalg, THIS-PROCEDURE).
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Kamp%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Kamp% vTableWin
ON LEAVE OF RowObject.Kamp% IN FRAME F-Main /* %Endring */
DO:
  IF bEndretRab THEN
  DO:
      MESSAGE "Rabatt% er endret (" + STRING(rowObject.Kamp%:SCREEN-VALUE) + " )." SKIP
          "Alle rader i kampanjen vil bli oppdatert med ny rabatt%." SKIP
          "Eventuelle spesailrabatter på artikkelnivå i kampanjen vil bli overskrevet."
          VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
      IF NOT choice THEN
          RETURN NO-APPLY.
      cValues = ",RAB%," + rowObject.KampanjeId:SCREEN-VALUE + ',' + REPLACE(STRING(rowObject.Kamp%:SCREEN-VALUE),',','|').

      IF VALID-HANDLE(hWindow) THEN
      DO:
          RUN avlysKampanje IN hWindow (cValues).
          IF VALID-HANDLE(hDataSource) THEN 
          DO:
              pcKeyValues = RowObject.KampanjeId:SCREEN-VALUE.
              DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
              DYNAMIC-FUNCTION('findRow':U IN hDataSource,
              INPUT pcKeyValues /* CHARACTER */).
          END.
      END.
  END.

  bEndretRab = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Kamp% vTableWin
ON VALUE-CHANGED OF RowObject.Kamp% IN FRAME F-Main /* %Endring */
DO:
  bEndretRab = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.KampanjePris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.KampanjePris vTableWin
ON LEAVE OF RowObject.KampanjePris IN FRAME F-Main /* Fast pris */
DO:
  DEF VAR lOldPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR iOldValg AS INT NO-UNDO.

  IF bEndretRab THEN
  DO:
      MESSAGE "Kampanjepris er endret (" + STRING(rowObject.KampanjePris:SCREEN-VALUE) + " )." SKIP
          "Alle rader i kampanjen vil bli oppdatert med ny KampanjePris." SKIP
          "Eventuelle kampanjepriser på artikkelnivå i kampanjen vil bli overskrevet."
          VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice2 AS LOGICAL.
      IF NOT choice2 THEN
          RETURN NO-APPLY.
      cValues = ",KPRIS," + rowObject.KampanjeId:SCREEN-VALUE + ',' + REPLACE(STRING(rowObject.Kampanjepris:SCREEN-VALUE),',','|').
      lOldPris = DEC(STRING(rowObject.Kampanjepris:SCREEN-VALUE)).
      iOldValg = INT(rowObject.Avslagtype:SCREEN-VALUE).

      IF VALID-HANDLE(hWindow) THEN
      DO:
          RUN avlysKampanje IN hWindow (cValues).
          IF VALID-HANDLE(hDataSource) THEN 
          DO:
              pcKeyValues = RowObject.KampanjeId:SCREEN-VALUE.
              DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
              DYNAMIC-FUNCTION('findRow':U IN hDataSource,
              INPUT pcKeyValues /* CHARACTER */).
          END.
          ASSIGN
              rowObject.Kampanjepris:SENSITIVE = TRUE
              rowObject.Kampanjepris:SCREEN-VALUE = STRING(lOldPris)
              rowObject.Kamp%:SENSITIVE = FALSE
              rowObject.Avslagtype:SCREEN-VALUE = STRING(iOldValg)
              rowObject.Kamp%:SCREEN-VALUE = "0"
              .

      END.
  END.

  bEndretRab = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.KampanjePris vTableWin
ON VALUE-CHANGED OF RowObject.KampanjePris IN FRAME F-Main /* Fast pris */
DO:
   bEndretRab = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Komplett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Komplett vTableWin
ON RETURN OF RowObject.Komplett IN FRAME F-Main /* Komplett */
DO:
  APPLY "TAB":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-KampType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-KampType vTableWin
ON VALUE-CHANGED OF RS-KampType IN FRAME F-Main
DO:
  IF RS-Kamptype:SCREEN-VALUE = '1' THEN 
        ASSIGN
          RowObject.NormalPris:CHECKED = FALSE
          RowObject.Leverandorkampanje:CHECKED = FALSE 
          RowObject.SluttDato:HIDDEN = FALSE
          FI-TilTid:HIDDEN = FALSE
          RowObject.GyldigTilTid:HIDDEN = FALSE
          .
  ELSE IF RS-Kamptype:SCREEN-VALUE = '2' THEN 
      ASSIGN
        RowObject.NormalPris:CHECKED = TRUE
        RowObject.Leverandorkampanje:CHECKED = FALSE 
        RowObject.SluttDato:HIDDEN = TRUE 
        FI-TilTid:HIDDEN = TRUE
        RowObject.GyldigTilTid:HIDDEN = TRUE
        .
  ELSE  
      ASSIGN
        RowObject.NormalPris:CHECKED = FALSE
        RowObject.Leverandorkampanje:CHECKED = TRUE 
        RowObject.SluttDato:HIDDEN = FALSE
        FI-TilTid:HIDDEN = FALSE
        RowObject.GyldigTilTid:HIDDEN = FALSE
        .
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
RUN SUPER.

DO WITH FRAME {&FRAME-NAME}:

  RUN enableFields.
  ASSIGN RowObject.StartDato:SCREEN-VALUE  = STRING(TODAY)
         RowObject.SluttDato:SCREEN-VALUE  = STRING(TODAY)
         BtnUtvalg:SENSITIVE               = FALSE
         FI-FraTid:SCREEN-VALUE = STRING(0)
         FI-TilTid:SCREEN-VALUE = ('23,59')
         RowObject.ProfilNr:SCREEN-VALUE = IF ipProfilNr > 0 
                                             THEN STRING(ipProfilNr)
                                             ELSE STRING(iProfilNr)
         .
  APPLY "value-changed" TO RowObject.AvslagType.
  APPLY "ENTRY" TO RowObject.Kamp%.
END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'sdo/dprisprofil.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessnoDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedprisprofilOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dprisprofil ).
       RUN repositionObject IN h_dprisprofil ( 6.24 , 145.00 ) NO-ERROR.
       /* Size in AB:  ( 1.91 , 7.40 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BkuCHOOSEoverfor vTableWin 
PROCEDURE BkuCHOOSEoverfor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEF VAR iAntOK     AS INTE    NO-UNDO.                                                       */
/*   DEF VAR iAntLinjer AS INTE    NO-UNDO.                                                       */
/*   DEF VAR plCancel   AS LOGI    NO-UNDO.                                                       */
/*   IF NOT RowObject.Aktivert:CHECKED AND (DATE(RowObject.StartDato:SCREEN-VALUE) < TODAY OR     */
/*      DATE(RowObject.StartDato:SCREEN-VALUE) > DATE(RowObject.SluttDato:SCREEN-VALUE)) THEN DO: */
/*      MESSAGE "Feil startdato"                                                                  */
/*          VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                    */
/*      APPLY "ENTRY" TO RowObject.StartDato.                                                     */
/*      RETURN NO-APPLY.                                                                          */
/*   END.                                                                                         */
/*   RUN confirmExit                                                                              */
/*     ( INPUT-OUTPUT plCancel /* LOGICAL */).                                                    */
/*   IF plCancel THEN                                                                             */
/*       RETURN NO-APPLY.                                                                         */
/*   MESSAGE "Konsekvensanalys ?"                                                                 */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSimuler AS LOGICAL.                    */
/*   IF lSimuler THEN DO:                                                                         */
/*       RUN SimulerAktiver IN hDataSource (INPUT INT(RowObject.KampanjeId:SCREEN-VALUE)).        */
/*       ASSIGN iAntOK     = INT(ENTRY(2,RETURN-VALUE))                                           */
/*              iAntLinjer = INT(ENTRY(3,RETURN-VALUE)).                                          */
/*   END.                                                                                         */
/*   IF lSimuler AND iAntOK = 0 THEN DO:                                                          */
/*       MESSAGE "Ingen godkänd linje. Överföring kan ikke skje"                                  */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                   */
/*       RETURN NO-APPLY.                                                                         */
/*   END.                                                                                         */
/*   MESSAGE (IF lSimuler THEN "Resultat: Godkända: " + STRING(iAntOK) + CHR(10) +                */
/*                            "av: " + STRING(iAntLinjer) + "." + CHR(10) ELSE "") +              */
/*            "Önsker du overföring?" VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE                */
/*                               lOverfor AS LOGICAL.                                             */
/*    IF lOverfor THEN DO:                                                                        */
/*        RUN Aktiver IN hDataSource (INPUT INT(RowObject.KampanjeId:SCREEN-VALUE)).              */
/*        IF NOT lSimuler THEN DO:                                                                */
/*            ASSIGN iAntOK     = INT(ENTRY(2,RETURN-VALUE))                                      */
/*                   iAntLinjer = INT(ENTRY(3,RETURN-VALUE)).                                     */
/*            MESSAGE "Resultat: Godkända: " + STRING(iAntOK) + CHR(10) +                         */
/*                    "av: " + STRING(iAntLinjer) + "." VIEW-AS ALERT-BOX INFO BUTTONS OK.        */
/*        END.                                                                                    */
/*    END.                                                                                        */
/*    RUN refreshRow IN hDataSource.                                                              */
/*    RETURN NO-APPLY.                                                                            */

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
      ASSIGN FI-FraTid:SCREEN-VALUE = setFITid(INT(RowObject.AktiveresTid:SCREEN-VALUE))
             FI-FraTid:MODIFIED     = FALSE
             FI-TilTid:SCREEN-VALUE = setFITid(INT(RowObject.GyldigTilTid:SCREEN-VALUE))
             FI-TilTid:MODIFIED     = FALSE.
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
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-FraTid:SCREEN-VALUE = setFITid(INT(RowObject.AktiveresTid:SCREEN-VALUE))
             FI-FraTid:MODIFIED     = FALSE
             FI-TilTid:SCREEN-VALUE = setFITid(INT(RowObject.GyldigTilTid:SCREEN-VALUE))
             FI-TilTid:MODIFIED     = FALSE.


      IF NOT DYNAMIC-FUNCTION("columnValue" IN DYNAMIC-FUNCTION("getDataSource"),"Aktivert") AND VALID-HANDLE(hUtvalg) THEN DO:
        setButtons(FALSE).
        IF VALID-HANDLE(hUtvalg) THEN 
          DYNAMIC-FUNCTION("setEnableBtnSendUtvalg" IN hUtvalg,FALSE).
      END.
      ELSE DO:
        setButtons(TRUE).
        IF VALID-HANDLE(hUtvalg) THEN 
          DYNAMIC-FUNCTION("setEnableBtnSendUtvalg" IN hUtvalg,TRUE).
      END.
  END.
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
  IF DYNAMIC-FUNCTION('getRecordState':U) = "NoRecordAvailable" THEN DO:
      DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
       RUN disableObject.
       ASSIGN RowObject.AktiveresTid:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE 
            RowObject.GyldigTilTid:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

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
    IF pcColValues = ? THEN
       ASSIGN B-Overfor:SENSITIVE = FALSE
              FI-FraTid:SENSITIVE = FALSE
              FI-TilTid:SENSITIVE = FALSE.
    ELSE IF VALID-HANDLE(hDataSource) THEN DO WITH FRAME {&FRAME-NAME}:
      IF (RowObject.Komplett:CHECKED /* AND 
             DYNAMIC-FUNCTION('getObjectEnabled':U) */ ) THEN DO:
          B-Komplett:SENSITIVE = FALSE.
          BtnUtvalg:SENSITIVE = FALSE.
          RUN disableObject.
      END.
      ELSE DO:
          IF NOT DYNAMIC-FUNCTION('getObjectEnabled':U) THEN
              RUN enableObject.
          ASSIGN B-Komplett:SENSITIVE = RowObject.Aktivert:CHECKED
                 B-Overfor:SENSITIVE = 
                    ENTRY(2,DYNAMIC-FUNCTION('colValues':U IN hDataSource,
                        INPUT "KannAktiveres" /* CHARACTER */),CHR(1)) = "J"
                        AND NOT RowObject.Komplett:CHECKED
                 /*RowObject.NormalPris:SENSITIVE = NOT B-Overfor:SENSITIVE*/
                 RS-Kamptype:SENSITIVE         = NOT B-Overfor:SENSITIVE
                 RowObject.StartDato:SENSITIVE = NOT RowObject.Aktivert:CHECKED
                 FI-FraTid:SENSITIVE           = NOT RowObject.Aktivert:CHECKED
                 RowObject.SluttDato:SENSITIVE = NOT RowObject.Aktivert:CHECKED
                 FI-TilTid:SENSITIVE           = NOT RowObject.Aktivert:CHECKED
                 .
          APPLY "value-changed" TO RowObject.AvslagType.

          IF B-Overfor:SENSITIVE OR Bruker.BrukerType = 2 THEN
              RowObject.ProfilNr:SENSITIVE = FALSE.
          ELSE
              RowObject.ProfilNr:SENSITIVE = TRUE.
      END.
/*       ASSIGN FI-FraTid:MODIFIED = FALSE  */
/*              FI-TilTid:MODIFIED = FALSE. */

    END.
  END.
  B-Aaterstall:SENSITIVE = RowObject.Aktivert:CHECKED = TRUE.
  B-Gjennbruk:SENSITIVE = RowObject.Aktivert:CHECKED = TRUE.
  FIND Kampanjehode NO-LOCK WHERE
      Kampanjehode.KampanjeId = int(RowObject.KampanjeId:SCREEN-VALUE) NO-ERROR.
  IF AVAILABLE KampanjeHode THEN
    RS-KampType:SCREEN-VALUE = IF KampanjeHode.NormalPris THEN '2'
                               ELSE IF Kampanjehode.LeverandorKampanje THEN '3'
                               ELSE '1'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableFields vTableWin 
PROCEDURE enableFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  IF VALID-HANDLE(hDataSource) AND NOT DYNAMIC-FUNCTION('getObjectEnabled':U) AND
        DYNAMIC-FUNCTION('columnValue':U IN hDataSource,
           INPUT "Komplett") = "yes" THEN
      RETURN.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hentProfilNr vTableWin 
PROCEDURE hentProfilNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER iProfilNr AS INT FORMAT ">>>>>>9" NO-UNDO.

  FIND KampanjeHode NO-LOCK where
      KampanjeHode.KampanjeId = int(DYNAMIC-FUNCTION("columnValue" IN DYNAMIC-FUNCTION("getDataSource"),"KampanjeId")) NO-ERROR.

  IF AVAILABLE KampanjeHode THEN
      ASSIGN
      iProfilNr = int(KampanjeHode.ProfilNr).

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
  
  DEF VAR cTekst AS CHAR NO-UNDO.

  FIND Bruker NO-LOCK WHERE
      Bruker.BrukerId = USERID('SkoTex') NO-ERROR.
  IF NOT AVAILABLE Bruker THEN
      MESSAGE 'Ukjent bruker: ' USERID('SkoTex')
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
  hWindow = SOURCE-PROCEDURE.
  DO WITH FRAME {&FRAME-NAME}:
      FI-FraTid:MOVE-TO-TOP().
      FI-TilTid:MOVE-TO-TOP().
  END.
  {syspara.i 5 1 1 iCL INT}
  FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
  IF AVAIL Butiker THEN
  DO:
      ASSIGN iProfilNr = Butiker.ProfilNr.
      FIND PrisProfil NO-LOCK WHERE 
          PrisProfil.ProfilNr = iProfilNr NO-ERROR.
  END.
  ELSE DO:
      FIND FIRST Prisprofil NO-LOCK NO-ERROR.
      ASSIGN iProfilNr = Prisprofil.ProfilNr.
  END.

  ASSIGN
      cTekst = STRING(PrisProfil.ProfilNr) + ' ' + REPLACE(PrisProfil.Beskrivelse,",","") + ',' + STRING(PrisProfil.ProfilNr).

  IF AVAILABLE Bruker AND Bruker.BrukerType = 2 THEN
  DO:
      FOR EACH PrisProfil NO-LOCK WHERE
          CAN-FIND(FIRST Butiker WHERE
                         Butiker.ProfilNr = PrisProfil.ProfilNr AND
                         Butiker.Butik    = Bruker.ButikkNr):
          ASSIGN
              cTekst = cTekst +
                       (IF cTekst = '' THEN '' ELSE ',') + 
                       STRING(PrisProfil.ProfilNr) + ' ' + REPLACE(PrisProfil.Beskrivelse,",","") + ',' + STRING(PrisProfil.ProfilNr).
          ASSIGN
              ipProfilNr                         = PrisProfil.ProfilNr
              RowObject.ProfilNr:LIST-ITEM-PAIRS = cTekst
              RowObject.ProfilNr:SCREEN-VALUE    = STRING(PrisProfil.ProfilNr).
      END.
  END.
  ELSE DO:
      FOR EACH PrisProfil NO-LOCK WHERE
          CAN-FIND(FIRST Butiker WHERE
                         Butiker.ProfilNr = PrisProfil.ProfilNr):
          ASSIGN
              cTekst = cTekst +
                       (IF cTekst = '' THEN '' ELSE ',') + 
                       STRING(PrisProfil.ProfilNr) + ' ' + PrisProfil.Beskrivelse + ',' + STRING(PrisProfil.ProfilNr).
      END.
      ASSIGN
          RowObject.ProfilNr:LIST-ITEM-PAIRS = cTekst
          RowObject.ProfilNr:SCREEN-VALUE = STRING(iProfilNr).
  END.
    
  RowObject.KampanjePris:FORMAT = ">>>,>>9.99".

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U).
  IF VALID-HANDLE(hDataSource) THEN
      RUN refreshRow IN hDataSource.

  SUBSCRIBE TO 'hentProfilNr' ANYWHERE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printObject vTableWin 
PROCEDURE printObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cObject AS CHARACTER  NO-UNDO.
DEFINE VARIABLE         iType   AS INTEGER INIT ?   NO-UNDO.
  IF cObject = "KampanjeHode" THEN DO:
      RUN d-velglistetype.w (INPUT-OUTPUT iType).
      IF iType = ? THEN
          RETURN.
      RUN PrintKampanje IN hDataSource
           ( INPUT INT(RowObject.KampanjeId:SCREEN-VALUE IN FRAME {&FRAME-NAME}),INPUT iType).
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
  DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
      IF RowObject.Kamp%:SENSITIVE AND INPUT RowObject.Kamp% = 0 THEN DO:
          MESSAGE RowObject.Kamp%:LABEL + " = 0. OK?"
              VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE lOk.
          IF NOT lOK THEN DO:
              APPLY "ENTRY" TO RowObject.Kamp%.
              RETURN NO-APPLY.
          END.
      END.
      IF RowObject.KampanjePris:SENSITIVE AND INPUT RowObject.KampanjePris = 0 THEN DO:
          MESSAGE RowObject.KampanjePris:LABEL + " = 0"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO RowObject.KampanjePris.
          RETURN NO-APPLY.
      END.
      /*IF NOT RowObject.NormalPris:CHECKED AND INPUT RowObject.Kamp% > 0 THEN DO:*/
      IF RS-Kamptype:SCREEN-VALUE <> '2' AND INPUT RowObject.Kamp% > 0 THEN DO:
          MESSAGE "Hvis kampanjen ikke er av type normalpris må rabatt%" + RowObject.Kamp%:LABEL + " være < 0"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "ENTRY" TO RowObject.KampanjePris.
          RETURN NO-APPLY.
      END.
      IF FI-FraTid:MODIFIED THEN DO:
          IF NOT TidOk(FI-FraTid:HANDLE) THEN DO:
              APPLY "ENTRY" TO FI-FraTid.
              RETURN.
          END.
          ASSIGN RowObject.AktiveresTid:SCREEN-VALUE = setTid(RowObject.AktiveresTid:HANDLE,FI-FraTid:HANDLE).
      END.
      IF FI-TilTid:MODIFIED THEN DO:
          IF NOT TidOk(FI-TilTid:HANDLE) THEN DO:
              APPLY "ENTRY" TO FI-TilTid.
              RETURN.
          END.
          ASSIGN RowObject.GyldigTilTid:SCREEN-VALUE = setTid(RowObject.GyldigTilTid:HANDLE,FI-TilTid:HANDLE).
      END.
      ASSIGN
          RowObject.LeverandorKampanje:CHECKED = IF RS-KampType:SCREEN-VALUE = '3' 
                                                   THEN TRUE 
                                                   ELSE FALSE
          RowObject.LeverandorKampanje:MODIFIED = TRUE
          .
  END.
  ASSIGN FI-FraTid:MODIFIED = FALSE
         FI-TilTid:MODIFIED = FALSE
         .
  
  RUN SUPER.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKampanjeId vTableWin 
FUNCTION getKampanjeId RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN INT(DYNAMIC-FUNCTION("columnValue" IN DYNAMIC-FUNCTION("getDataSource"),"KampanjeId")).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNoResizeXfields vTableWin 
FUNCTION getNoResizeXfields RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN "Notat".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitUtvalgToKampanje vTableWin 
FUNCTION InitUtvalgToKampanje RETURNS LOGICAL
  ( INPUT ihUtvalg         AS HANDLE,
    INPUT ibUtvalgIsMaster AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose: Enable / disable buttons according to who is master
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN hUtvalg                    = ihUtvalg
         bUtvalgIsMaster            = ibUtvalgIsMaster
         btnUtvalg:HIDDEN           = IF bUtvalgIsMaster THEN TRUE ELSE FALSE
         btnUtvalg:SENSITIVE        = IF bUtvalgIsMaster THEN FALSE ELSE TRUE
         btnHentFraUtvalg:HIDDEN    = IF VALID-HANDLE(hUtvalg) THEN FALSE ELSE TRUE
         btnHentFraUtvalg:SENSITIVE = IF VALID-HANDLE(hUtvalg) THEN TRUE ELSE FALSE
         .

  DYNAMIC-FUNCTION("setUtvalgHandle" IN DYNAMIC-FUNCTION("getContainerSource"), hUtvalg).
END.
  
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButtons vTableWin 
FUNCTION setButtons RETURNS LOGICAL
  ( INPUT ibSensitive AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN B-Komplett:SENSITIVE = ibSensitive
         BtnUtvalg:SENSITIVE  = ibSensitive
         .
END.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFITid vTableWin 
FUNCTION setFITid RETURNS CHARACTER
  ( INPUT iTid AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN STRING( DECI(SUBSTR(STRING(iTid,"HH:MM"),1,2)) + DECI(SUBSTR(STRING(iTid,"HH:MM"),4,2)) / 100,"99.99").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setTid vTableWin 
FUNCTION setTid RETURNS CHARACTER
  ( INPUT hDbFillIn AS HANDLE,INPUT hUpdFillIn AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  STRING(INT(SUBSTR(hUpdFillIn:SCREEN-VALUE,1,2)) * 3600 +
                 INT(SUBSTR(hUpdFillIn:SCREEN-VALUE,4,2)) * 60).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TidOk vTableWin 
FUNCTION TidOk RETURNS LOGICAL
  ( INPUT hUpdFillIn AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
     DEFINE VARIABLE lReturVerdi AS LOGICAL    NO-UNDO.
     ASSIGN lReturVerdi = SUBSTR(hUpdFillIn:SCREEN-VALUE,1,2) < "24" AND  
                SUBSTR(hUpdFillIn:SCREEN-VALUE,4,2) < "60".
      IF NOT lReturVerdi THEN
          MESSAGE "Feil tid angitt"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN lReturVerdi.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UtvalgIsMaster vTableWin 
FUNCTION UtvalgIsMaster RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN bUtvalgIsMaster.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

