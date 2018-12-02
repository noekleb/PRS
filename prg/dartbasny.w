&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER iArtSlag AS INT NO-UNDO.
DEF INPUT-OUTPUT PARAMETER lArtikkelNr AS DEC.

/* Local Variable Definitions ---                                       */

DEF VAR cEAN13       AS CHAR NO-UNDO.
DEF VAR bOk          AS LOG  NO-UNDO.
DEF VAR cTekst       AS CHAR NO-UNDO.
DEF VAR cLookupValue AS CHAR NO-UNDO.

DEF VAR plMvaKr AS DEC NO-UNDO.
DEF VAR plDbKr  AS DEC NO-UNDO.
DEF VAR bLagerstyrt AS LOG  NO-UNDO.
DEF VAR bVaremottak AS LOG NO-UNDO.
DEF VAR cCl         AS CHAR NO-UNDO.
DEF VAR cTmpCl      AS CHAR NO-UNDO.
DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iProfilnr AS INTEGER     NO-UNDO.
DEF BUFFER bStrekkode FOR Strekkode.
DEF BUFFER bufArtBAs FOR ArtBas.
DEF BUFFER duplicateArtpris FOR artpris.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Strekkode FI-LevNr FI-LevKod ~
FI-Bestillingsnummer FI-Vg FI-Beskr FI-Bongtekst FI-VMId BUTTON-SokVg ~
FI-ProdNr FI-Etikettekst1 FI-LinkVareNr FI-Innkjopspris FI-Pris ~
CB-SalgsEnhet CB-JamforEnhet FI-Mengde FI-AntIPakn T-Anbrekk T-LokPris ~
T-Kjedevare T-Lagerstyrt iButikkNr lAntall Btn_OK Btn_Cancel B-SokLinkvare ~
BUTTON-SokLev BUTTON-SokVaremerke BUTTON-SokProdusent BUTTON-SokButikk ~
RECT-70 
&Scoped-Define DISPLAYED-OBJECTS FI-Db% FI-Strekkode FI-LevNr FI-LevNamn ~
FI-LevKod FI-Bestillingsnummer FI-Vg FI-VgBeskr FI-Beskr FI-Bongtekst ~
FI-VMId FI-VMBeskrivelse FI-ProdNr FI-ProdBeskrivelse FI-Etikettekst1 ~
FI-LinkVareNr FI-LinkVareTekst FI-Innkjopspris FI-Pris CB-SalgsEnhet ~
CB-JamforEnhet FI-Mengde FI-JamforPris FI-AntIPakn T-Anbrekk T-LokPris ~
T-Kjedevare T-Lagerstyrt iButikkNr lAntall 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD settArtSlag Dialog-Frame 
FUNCTION settArtSlag RETURNS INTEGER
  ( INPUT iArtSlag AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokLinkvare 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Opprett artikkel" 
     SIZE 40 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokButikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Søk i leverandørregister".

DEFINE BUTTON BUTTON-SokProdusent 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokVaremerke 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokVg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-JamforEnhet AS CHARACTER FORMAT "X(4)" 
     LABEL "Jamførenhet" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "<Ikke angitt>"," ",
                     "Stykk (Stk)","stk",
                     "Kilo (Kg)","kg",
                     "Hekto (Hg)","hg",
                     "Meter (m)","m",
                     "Kvadratmeter (m2)","m2",
                     "Liter (l)","l"
     DROP-DOWN-LIST
     SIZE 27 BY 1 TOOLTIP "Enhet som jamfør pris angis i (stk, kg, hg, m, m2, l)".

DEFINE VARIABLE CB-SalgsEnhet AS CHARACTER FORMAT "X(30)" INITIAL "Par" 
     LABEL "Salgsenhet" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 27 BY 1 TOOLTIP "Artikkelens salgsenhetstekst".

DEFINE VARIABLE FI-AntIPakn AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Ant i pkn." 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 TOOLTIP "Artikkelens varetekst" NO-UNDO.

DEFINE VARIABLE FI-Bestillingsnummer AS CHARACTER FORMAT "X(30)":U 
     LABEL "Bestillingsnr" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Artikkelens bestillingsnr" NO-UNDO.

DEFINE VARIABLE FI-Bongtekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Artikkelens bongtekst (Maks 20 char i kassen)" NO-UNDO.

DEFINE VARIABLE FI-Db% AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Db%" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Etikettekst1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etikettekst 1" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 TOOLTIP "Etikettekst 1" NO-UNDO.

DEFINE VARIABLE FI-Innkjopspris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Innkjøpspris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Artikkelens innkjøpspris før rabatter." NO-UNDO.

DEFINE VARIABLE FI-JamforPris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Jamførpris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Mengde i salgsenhet" NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(30)":U 
     LABEL "Lev.artikkelnr" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Leverandørens modell eller artikkelnr." NO-UNDO.

DEFINE VARIABLE FI-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 37.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Artikkelens leverandør" NO-UNDO.

DEFINE VARIABLE FI-LinkVareNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Link til pantevare" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 20.2 BY 1 TOOLTIP "Artikkelnr. på artikkel som representerer pant på varen".

DEFINE VARIABLE FI-LinkVareTekst AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mengde AS DECIMAL FORMAT "->>9.999" INITIAL 1 
     LABEL "Mengde i salgsenhet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Mengde i salgsenhet".

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Pris til kunde inkl. mva." NO-UNDO.

DEFINE VARIABLE FI-ProdBeskrivelse AS CHARACTER FORMAT "x(50)" 
     VIEW-AS FILL-IN 
     SIZE 37.8 BY 1.

DEFINE VARIABLE FI-ProdNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Kode som beskriver artikkelens produsent".

DEFINE VARIABLE FI-Strekkode AS CHARACTER FORMAT "X(13)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 TOOLTIP "Angi artikkelens strekkode" NO-UNDO.

DEFINE VARIABLE FI-Vg AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Artikkelens varegruppe" NO-UNDO.

DEFINE VARIABLE FI-VgBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VMBeskrivelse AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 37.8 BY 1.

DEFINE VARIABLE FI-VMId AS INTEGER FORMAT "zzz9" INITIAL 0 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Kode som beskriver artikkelens varemerke".

DEFINE VARIABLE iButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Butikknr. som lagerantall skal innleveres på" NO-UNDO.

DEFINE VARIABLE lAntall AS DECIMAL FORMAT "->>>,>>9.999":U INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Antall som skal leveres inn på lager" NO-UNDO.

DEFINE RECTANGLE RECT-70
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.2 BY 19.05.

DEFINE VARIABLE T-Anbrekk AS LOGICAL INITIAL no 
     LABEL "Anbrekk" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE T-Kjedevare AS LOGICAL INITIAL no 
     LABEL "Kjedelevert" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE T-Lagerstyrt AS LOGICAL INITIAL no 
     LABEL "Lagerstyrt" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE T-LokPris AS LOGICAL INITIAL no 
     LABEL "Lokal pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Db% AT ROW 13.52 COL 20.8 COLON-ALIGNED
     FI-Strekkode AT ROW 1.48 COL 20.8 COLON-ALIGNED HELP
          "Angi artikkelens strekkode"
     FI-LevNr AT ROW 2.48 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens leverandør"
     FI-LevNamn AT ROW 2.48 COL 39 COLON-ALIGNED NO-LABEL
     FI-LevKod AT ROW 3.48 COL 20.8 COLON-ALIGNED HELP
          "Leverandørens modell eller artikkelnr."
     FI-Bestillingsnummer AT ROW 4.48 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens bestillingsnr"
     FI-Vg AT ROW 5.48 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens varegruppe"
     FI-VgBeskr AT ROW 5.48 COL 39 COLON-ALIGNED NO-LABEL
     FI-Beskr AT ROW 6.48 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens varetekst"
     FI-Bongtekst AT ROW 7.48 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens bongtekst (Maks 20 char i kassen)"
     FI-VMId AT ROW 8.52 COL 20.8 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens varemerke (F10-Oppslag)"
     FI-VMBeskrivelse AT ROW 8.52 COL 39 COLON-ALIGNED NO-LABEL
     BUTTON-SokVg AT ROW 5.48 COL 36.8
     FI-ProdNr AT ROW 9.52 COL 20.8 COLON-ALIGNED HELP
          "Kode som beskriver artikkelens produsent (F10 Oppslag)"
     FI-ProdBeskrivelse AT ROW 9.52 COL 39 COLON-ALIGNED NO-LABEL
     FI-Etikettekst1 AT ROW 10.52 COL 20.8 COLON-ALIGNED HELP
          "Etikettekst 1"
     FI-LinkVareNr AT ROW 11.52 COL 20.8 COLON-ALIGNED HELP
          "Artikkelnr. på artikkel som representerer pant på varen (F10)"
     FI-LinkVareTekst AT ROW 11.52 COL 45.4 COLON-ALIGNED NO-LABEL
     FI-Innkjopspris AT ROW 12.52 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens innkjøpspris før rabatter."
     FI-Pris AT ROW 14.52 COL 20.8 COLON-ALIGNED HELP
          "Pris til kunde inkl. mva."
     CB-SalgsEnhet AT ROW 15.57 COL 20.8 COLON-ALIGNED HELP
          "Artikkelens salgsenhetstekst"
     CB-JamforEnhet AT ROW 16.62 COL 20.8 COLON-ALIGNED HELP
          "Enhet som jamfør pris angis i (stk, kg, hg, m, m2, l)."
     FI-Mengde AT ROW 17.62 COL 20.8 COLON-ALIGNED HELP
          "Mengde i salgsenhet"
     FI-JamforPris AT ROW 18.62 COL 20.8 COLON-ALIGNED HELP
          "Mengde i salgsenhet"
     FI-AntIPakn AT ROW 12.57 COL 56.8 COLON-ALIGNED WIDGET-ID 2
     T-Anbrekk AT ROW 14 COL 59.2 WIDGET-ID 4
     T-LokPris AT ROW 14.95 COL 59
     T-Kjedevare AT ROW 15.81 COL 59
     T-Lagerstyrt AT ROW 16.71 COL 59
     iButikkNr AT ROW 17.62 COL 57 COLON-ALIGNED
     lAntall AT ROW 18.62 COL 57 COLON-ALIGNED
     Btn_OK AT ROW 20.67 COL 2
     Btn_Cancel AT ROW 20.67 COL 65
     B-SokLinkvare AT ROW 11.52 COL 43 NO-TAB-STOP 
     BUTTON-SokLev AT ROW 2.43 COL 36.6 HELP
          "Søk i leverandørregister"
     BUTTON-SokVaremerke AT ROW 8.52 COL 36.6
     BUTTON-SokProdusent AT ROW 9.52 COL 36.6
     BUTTON-SokButikk AT ROW 17.67 COL 73
     RECT-70 AT ROW 1 COL 1
     SPACE(0.99) SKIP(2.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Forenklet artikkelregistrering"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Db% IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-JamforPris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LevNamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-LinkVareTekst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-LinkVareTekst:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-ProdBeskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VgBeskr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VMBeskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Forenklet artikkelregistrering */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLinkvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLinkvare Dialog-Frame
ON CHOOSE OF B-SokLinkvare IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-LinkVareNr
DO:
  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "ArtikkelNr".
      RUN JBoxDLookup.w ("ArtBas;Beskr;ArtikkelNr", "where ArtBas.Pant = true", INPUT-OUTPUT cLookupValue).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = DEC(cLookupValue) NO-ERROR.
      IF AVAILABLE ArtBas THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-LinkVareNr:SCREEN-VALUE    = cLookupValue
            FI-LinkVareTekst:SCREEN-VALUE = ArtBas.Beskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-LinkVareNr:SCREEN-VALUE    = ''
            FI-LinkVareTekst:SCREEN-VALUE  = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Opprett artikkel */
DO:
    IF DEC(FI-Strekkode:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE 'Strekkode er ikke angitt.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-Strekkode.
        RETURN NO-APPLY.
    END.
    IF NOT can-find(VarGr where
                    VarGr.Vg = INT(FI-Vg:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig varegruppe.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-Vg.
        RETURN NO-APPLY.
    END.
    IF NOT can-find(LevBas where
                    LevBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig leverandør.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-LevNr.
        RETURN NO-APPLY.
    END.
    IF NOT can-find(Varemerke where
                    Varemerke.VmId = INT(FI-VmId:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig varemerke.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-VmId.
        RETURN NO-APPLY.
    END.
    IF NOT can-find(Produsent where
                    Produsent.ProdNr = INT(FI-ProdNr:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig produsent.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-ProdNr.
        RETURN NO-APPLY.
    END.
    IF FI-Beskr:SCREEN-VALUE = '' THEN
    DO:
        MESSAGE 'Varetekst er ikke angitt.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-Beskr.
        RETURN NO-APPLY.
    END.
    IF DEC(FI-PRIS:SCREEN-VALUE) = 0 THEN
    DO:
        MESSAGE 'Pris må angis.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-Pris.
        RETURN NO-APPLY.
    END.
    IF DEC(FI-LinkVareNr:SCREEN-VALUE) <> 0 AND 
        NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DEC(FI-LinkVareNr:SCREEN-VALUE))
        THEN
    DO:
        MESSAGE 'Ukjent pantvare er angitt.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY 'ENTRY' TO FI-LinkVareNr.
        RETURN NO-APPLY.
    END.
    IF lArtikkelNr = 0 THEN
        RUN opprettArtikkel.
    IF RETURN-VALUE <> '' THEN
    DO:
        MESSAGE 'Feil oppsto ved opprettelse av artikkel..:' SKIP
            RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF bVaremottak AND NOT CAN-FIND(Butiker WHERE
                                    Butiker.Butik = INT(iButikkNr:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig butikknr angitt for varemottak..:' SKIP
            RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButikk Dialog-Frame
ON CHOOSE OF BUTTON-SokButikk IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-VMId
DO:
  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "Butik".
      RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", "where Butiker.HarButikkSystem = true ", INPUT-OUTPUT cLookupValue).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      ASSIGN
          iButikkNr:SCREEN-VALUE = cLookupValue.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev Dialog-Frame
ON CHOOSE OF BUTTON-SokLev IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-LevNr
DO:
  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "LevNr".
      RUN JBoxDLookup.w ("LevBas;LevNamn;LevNr;Kjedeavtale", "where true", INPUT-OUTPUT cLookupValue).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND LevBas NO-LOCK WHERE
        LevBas.LevNr = INT(cLookupValue) NO-ERROR.
      IF AVAILABLE LevBas THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-LevNr:SCREEN-VALUE   = cLookupValue
            FI-LevNamn:SCREEN-VALUE = LevBas.LevNamn
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-LevNr:SCREEN-VALUE    = ''
            FI-LevNamn:SCREEN-VALUE  = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokProdusent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokProdusent Dialog-Frame
ON CHOOSE OF BUTTON-SokProdusent IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-ProdNr
DO:
  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "ProdNr".
      RUN JBoxDLookup.w ("Produsent;Beskrivelse;ProdNr", "where true", INPUT-OUTPUT cLookupValue).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Produsent NO-LOCK WHERE
        Produsent.ProdNr = INT(cLookupValue) NO-ERROR.
      IF AVAILABLE Produsent THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-ProdNr:SCREEN-VALUE   = cLookupValue
            FI-ProdBeskrivelse:SCREEN-VALUE = Produsent.Beskrivelse
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-ProdNr:SCREEN-VALUE    = ''
            FI-ProdBeskrivelse:SCREEN-VALUE  = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVaremerke Dialog-Frame
ON CHOOSE OF BUTTON-SokVaremerke IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-VMId
DO:
  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "VmId".
      RUN JBoxDLookup.w ("Varemerke;Beskrivelse;KortNavn;VmId", "where true", INPUT-OUTPUT cLookupValue).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Varemerke NO-LOCK WHERE
        Varemerke.VmId = INT(cLookupValue) NO-ERROR.
      IF AVAILABLE Varemerke THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-VmId:SCREEN-VALUE   = cLookupValue
            FI-VmBeskrivelse:SCREEN-VALUE = Varemerke.Beskrivelse
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-VmId:SCREEN-VALUE    = ''
            FI-VmBeskrivelse:SCREEN-VALUE  = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg Dialog-Frame
ON CHOOSE OF BUTTON-SokVg IN FRAME Dialog-Frame /* ... */
OR F10 OF FI-Vg
DO:
  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "Vg".
      RUN JBoxDLookup.w ("VarGr;VgBeskr|Beskrivelse|x(30);Vg", "where true", INPUT-OUTPUT cLookupValue).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INT(cLookupValue) NO-ERROR.
      IF AVAILABLE VarGr THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Vg:SCREEN-VALUE      = cLookupValue
            FI-VgBeskr:SCREEN-VALUE = VarGr.VgBeskr
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Vg:SCREEN-VALUE    = ''
            FI-VgBeskr:SCREEN-VALUE  = ''
            .
      END.
      VIEW FRAME DIALOG-FRAME.
      APPLY 'ENTRY' TO FRAME DIALOG-FRAME.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-JamforEnhet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-JamforEnhet Dialog-Frame
ON RETURN OF CB-JamforEnhet IN FRAME Dialog-Frame /* Jamførenhet */
DO:
  APPLY 'TAB' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-SalgsEnhet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-SalgsEnhet Dialog-Frame
ON RETURN OF CB-SalgsEnhet IN FRAME Dialog-Frame /* Salgsenhet */
DO:
  APPLY 'TAB' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr Dialog-Frame
ON LEAVE OF FI-Beskr IN FRAME Dialog-Frame /* Varetekst */
DO:
    DO WITH FRAME DIALOG-FRAME:
        /*
        IF FI-Beskr:SCREEN-VALUE = '' THEN
        DO:
            MESSAGE 'Varetekst må angis.'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        */

        /*IF FI-Bongtekst:SCREEN-VALUE = '' THEN*/
            FI-BongTekst:SCREEN-VALUE = SUBSTRING(FI-Beskr:SCREEN-VALUE,1,20).
        /*IF FI-Etikettekst1:SCREEN-VALUE = '' THEN*/
            FI-Etikettekst1:SCREEN-VALUE = FI-Beskr:SCREEN-VALUE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Bestillingsnummer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Bestillingsnummer Dialog-Frame
ON TAB OF FI-Bestillingsnummer IN FRAME Dialog-Frame /* Bestillingsnr */
OR RETURN OF FI-Bestillingsnummer DO:

    IF TRIM(FI-Bestillingsnummer:SCREEN-VALUE) <> '' THEN 
    BEST_NR_SJEKK:
    DO:
        STREKKODE:
        FOR EACH bStrekkode WHERE
            bStrekkode.Bestillingsnummer = FI-Bestillingsnummer:SCREEN-VALUE:
            bOk = FALSE.
            FIND ArtBas OF bStrekkode NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN NEXT.
            IF ArtBas.LevNr <> INT(FI-LevNr:SCREEN-VALUE) THEN NEXT.

            MESSAGE 'Artikkel med dette bestilingsnummer er allerede registrert på denne leverandør/produsent.' SKIP
                    'Skal artikkelen hentes opp i artikkelkortet?'
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Artikkel funnet'
                UPDATE bOk.
            IF bOk THEN DO:
                FIND FIRST Strekkode NO-LOCK WHERE
                    Strekkode.Bestillingsnummer = FI-Bestillingsnummer:SCREEN-VALUE NO-ERROR.
                IF AVAILABLE Strekkode THEN
                DO:
                  lArtikkelNr = Strekkode.ArtikkelNr.
                  FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
                  FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
                  IF AVAILABLE ArtBas THEN 
                      ASSIGN
                      FI-LevKod:SCREEN-VALUE = ArtBas.LevKod
                      FI-Beskr:SCREEN-VALUE = ArtBas.Beskr
                      FI-Vg:SCREEN-VALUE = STRING(ArtBas.Vg)
                      FI-Bongtekst:SCREEN-VALUE = ArtBas.BongTekst
                      FI-LevNr:SCREEN-VALUE = STRING(ArtBas.LevNr)
                      FI-Etikettekst1:SCREEN-VALUE = ArtBas.Etikettekst1
                      FI-Innkjopspris:SCREEN-VALUE = STRING(ArtPris.InnkjopsPris[1])
                      FI-Pris:SCREEN-VALUE = STRING(ArtPris.Pris[1])
                      .
                END.
                APPLY 'choose' TO Btn_Ok.
                LEAVE STREKKODE.
            END.
            ELSE DO:
                FI-Bestillingsnummer:SCREEN-VALUE = ''.
                RETURN NO-APPLY.
            END.
        END. /* STREKKODE */
    END. /* BEST_NR_SJEKK */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Innkjopspris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Innkjopspris Dialog-Frame
ON LEAVE OF FI-Innkjopspris IN FRAME Dialog-Frame /* Innkjøpspris */
DO:
    DO WITH FRAME Default-Frame:
        IF DEC(FI-Pris:SCREEN-VALUE) = 0 THEN
            RUN initPris.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Innkjopspris Dialog-Frame
ON VALUE-CHANGED OF FI-Innkjopspris IN FRAME Dialog-Frame /* Innkjøpspris */
DO:
  IF DEC(FI-Innkjopspris:SCREEN-VALUE) <> 0 AND 
      DEC(FI-Pris:SCREEN-VALUE) <> 0 THEN
  DO:
    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INT(FI-Vg:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
    IF AVAILABLE Moms THEN
    DO:
        ASSIGN
            plMvaKr = DEC(FI-Pris:SCREEN-VALUE) - (DEC(FI-Pris:SCREEN-VALUE) / (1 + (Moms.MomsProc / 100)))
            plDbKr  = DEC(FI-Pris:SCREEN-VALUE) - plMvaKr - DEC(FI-Innkjopspris:SCREEN-VALUE)
            FI-Db%  = ROUND((plDbKr * 100) / (DEC(FI-Innkjopspris:SCREEN-VALUE) + plDbKr),2)
            FI-Db%  = IF FI-Db% = ? THEN 0 ELSE FI-Db%
            FI-Db%:SCREEN-VALUE = STRING(FI-Db%)
            .
    END.
    ELSE
      FI-DB%:SCREEN-VALUE = ''.

  END.
    
  ELSE
    FI-DB%:SCREEN-VALUE = ''.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod Dialog-Frame
ON TAB OF FI-LevKod IN FRAME Dialog-Frame /* Lev.artikkelnr */
OR RETURN OF FI-LevKod DO:

    IF TRIM(FI-LevKod:SCREEN-VALUE) <> '' AND
       INT(FI-LevNr:SCREEN-VALUE) <> 0 AND 
       CAN-FIND(FIRST ArtBas WHERE
                ArtBas.LevNr = INT(FI-LevNr:SCREEN-VALUE) AND
                ArtBas.LEvKod = FI-LevKod:SCREEN-VALUE) THEN
    DO:
        bOk = FALSE.
        MESSAGE 'Artikkel med dette leverandør og leverandørs artikkelnr er allerede registrert.' SKIP
                'Skal artikkelen hentes opp i artikkelkortet?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Artikkel funnet'
            UPDATE bOk.
        IF bOk THEN DO:
            FIND FIRST ArtBas WHERE
                ArtBas.LevNr = INT(FI-LevNr:SCREEN-VALUE) AND
                ArtBas.LevKod = FI-LevKod:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN
            DO:
              lArtikkelNr = ArtBas.ArtikkelNr.
              FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
              IF AVAILABLE ArtBas THEN 
                  ASSIGN
                  FI-LevKod:SCREEN-VALUE = ArtBas.LevKod
                  FI-Beskr:SCREEN-VALUE = ArtBas.Beskr
                  FI-Vg:SCREEN-VALUE = STRING(ArtBas.Vg)
                  FI-Bongtekst:SCREEN-VALUE = ArtBas.BongTekst
                  FI-LevNr:SCREEN-VALUE = STRING(ArtBas.LevNr)
                  FI-Etikettekst1:SCREEN-VALUE = ArtBas.Etikettekst1
                  FI-Innkjopspris:SCREEN-VALUE = STRING(ArtPris.InnkjopsPris[1])
                  FI-Pris:SCREEN-VALUE = STRING(ArtPris.Pris[1])
                  .
            END.
            APPLY 'choose' TO Btn_Ok.
        END.
        ELSE DO:
            FI-LevKod:SCREEN-VALUE = ''.
            RETURN NO-APPLY.
        END.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr Dialog-Frame
ON LEAVE OF FI-LevNr IN FRAME Dialog-Frame /* Leverandør */
DO:
    IF NOT can-find(LevBas where
                    LevBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig leverandør.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr Dialog-Frame
ON TAB OF FI-LevNr IN FRAME Dialog-Frame /* Leverandør */
OR 'RETURN' OF FI-LevNr
DO:
    DO WITH FRAME DIALOG-FRAME:
        IF INT(FI-LevNr:SCREEN-VALUE) > 0 THEN
        DO:
            FIND LevBas NO-LOCK WHERE
                LevBas.LevNr = int(FI-LevNr:SCREEN-VALUE) NO-ERROR.
            IF AVAILABLE LevBas THEN
                DISPLAY LevBas.LevNamn @ FI-LevNamn.
            ELSE DO:
                FI-LevNamn:SCREEN-VALUE = ''.
                RETURN NO-APPLY.
            END.
            APPLY LASTKEY.
        END.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LinkVareNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LinkVareNr Dialog-Frame
ON TAB OF FI-LinkVareNr IN FRAME Dialog-Frame /* Link til pantevare */
OR 'RETURN' OF FI-LinkVareNr
DO:
    
  DO WITH FRAME DIALOG-FRAME:
      IF INPUT FI-LinkVareNr > 0 AND
        NOT CAN-FIND(ArtBas WHERE
                     ArtBas.ArtikkelNr = INPUT FI-LinkVareNr AND
                     ArtBas.Pant = TRUE) THEN
        DO:
          MESSAGE 'Link kan bare legges inn på varer som er satt opp som pantvare.'
            VIEW-AS ALERT-BOX WARNING.
          FI-Linkvaretekst:SCREEN-VALUE = ''.
          RETURN NO-APPLY.
        END.

        FI-Linkvaretekst:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","ArtBas;Beskr","WHERE ArtikkelNr = '" + FI-LinkVareNr:SCREEN-VALUE + "'").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Mengde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Mengde Dialog-Frame
ON VALUE-CHANGED OF FI-Mengde IN FRAME Dialog-Frame /* Mengde i salgsenhet */
DO:
    DO WITH FRAME Dilaog-Frame:
        FI-JamforPris:screen-value = STRING(ROUND(DEC(FI-Pris:SCREEN-VALUE) / DEC(FI-Mengde:SCREEN-VALUE),2)).
        IF INPUT FI-JamforPris:screen-value = ? then FI-JamforPris:screen-value = ''.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris Dialog-Frame
ON LEAVE OF FI-Pris IN FRAME Dialog-Frame /* Pris */
DO:
  /*
  IF dec(FI-PRIS:SCREEN-VALUE) = 0 THEN
  DO:
      MESSAGE 'Pris må angis.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Pris Dialog-Frame
ON VALUE-CHANGED OF FI-Pris IN FRAME Dialog-Frame /* Pris */
DO:
    DO WITH FRAME Dilaog-Frame:
        FI-JamforPris:screen-value = STRING(ROUND(DEC(FI-Pris:SCREEN-VALUE) / DEC(FI-Mengde:SCREEN-VALUE),2)).
        IF INPUT FI-JamforPris:screen-value = ? then FI-JamforPris:screen-value = ''.
    END.
  
    IF DEC(FI-Innkjopspris:SCREEN-VALUE) <> 0 AND 
        DEC(FI-Pris:SCREEN-VALUE) <> 0 THEN
    DO:
      FIND VarGr NO-LOCK WHERE
          VarGr.Vg = INT(FI-Vg:SCREEN-VALUE) NO-ERROR.
      IF AVAILABLE VarGr THEN
          FIND Moms OF VarGr NO-LOCK NO-ERROR.
      IF AVAILABLE Moms THEN
      DO:
          ASSIGN
              plMvaKr = DEC(FI-Pris:SCREEN-VALUE) - (DEC(FI-Pris:SCREEN-VALUE) / (1 + (Moms.MomsProc / 100)))
              plDbKr  = DEC(FI-Pris:SCREEN-VALUE) - plMvaKr - DEC(FI-Innkjopspris:SCREEN-VALUE)
              FI-Db%  = ROUND((plDbKr * 100) / (DEC(FI-Innkjopspris:SCREEN-VALUE) + plDbKr),2)
              FI-Db%  = IF FI-Db% = ? THEN 0 ELSE FI-Db%
              FI-Db%:SCREEN-VALUE = STRING(FI-Db%)
              .
      END.
      ELSE
        FI-DB%:SCREEN-VALUE = ''.

    END.

    ELSE
      FI-DB%:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ProdNr Dialog-Frame
ON LEAVE OF FI-ProdNr IN FRAME Dialog-Frame /* Produsent */
DO:
    IF NOT can-find(Produsent where
                    Produsent.ProdNr = INT(FI-ProdNr:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig produsent.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ProdNr Dialog-Frame
ON TAB OF FI-ProdNr IN FRAME Dialog-Frame /* Produsent */
OR "RETURN":U OF FI-ProdNr
DO:
  DO WITH FRAME DIALOG-FRAME:
      IF INT(FI-ProdNr:SCREEN-VALUE) > 0 THEN
      DO:
          FIND Produsent NO-LOCK WHERE
            Produsent.ProdNr = int(FI-ProdNr:Screen-value) NO-ERROR.
          IF NOT AVAILABLE Produsent THEN
            DO:
              DISPLAY
                "" @ FI-ProdNr
                "" @ FI-ProdBeskrivelse
              WITH FRAME DIALOG-FRAME. 
              RETURN NO-APPLY.
            END.
          DISPLAY
              Produsent.Beskrivelse @ FI-ProdBeskrivelse
              WITH FRAME DIALOG-FRAME.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON ANY-PRINTABLE OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
DO:
  /* Bare tillatt med tallene 0 - 9. */
  IF NOT CAN-DO('48,49,50,51,52,53,54,55,56,57',STRING(LASTKEY)) THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON LEAVE OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
DO:
    IF SELF:SCREEN-VALUE = '' THEN
    DO:
        MESSAGE 'Strekkode må angis.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Strekkode Dialog-Frame
ON TAB OF FI-Strekkode IN FRAME Dialog-Frame /* Strekkode */
OR RETURN OF FI-Strekkode
DO:
  /* Legger riktig konvertert kode i cEAN13. */
  RUN sjekkStrekkode.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  IF CAN-FIND(Strekkode WHERE
                  Strekkode.Kode = cEAN13) THEN
  DO:
      bOk = FALSE.
      MESSAGE 'Artikkel med denne strekkoden er allerede registrert.' SKIP
              'Skal artikkelen hentes opp i artikkelkortet?'
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE 'Artikkel funnet'
          UPDATE bOk.
      IF bOk THEN DO:
          FIND Strekkode NO-LOCK WHERE
              Strekkode.Kode = cEAN13 NO-ERROR.
          IF AVAILABLE Strekkode THEN
          DO:
            lArtikkelNr = Strekkode.ArtikkelNr.
            FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
            FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE ArtBas THEN 
                ASSIGN
                FI-LevKod:SCREEN-VALUE = ArtBas.LevKod
                FI-Beskr:SCREEN-VALUE = ArtBas.Beskr
                FI-Vg:SCREEN-VALUE = STRING(ArtBas.Vg)
                FI-Bongtekst:SCREEN-VALUE = ArtBas.BongTekst
                FI-LevNr:SCREEN-VALUE = STRING(ArtBas.LevNr)
                FI-Etikettekst1:SCREEN-VALUE = ArtBas.Etikettekst1
                FI-Innkjopspris:SCREEN-VALUE = STRING(ArtPris.InnkjopsPris[1])
                FI-Pris:SCREEN-VALUE = STRING(ArtPris.Pris[1])
                .
          END.
          APPLY 'choose' TO Btn_Ok.
      END.
      ELSE DO:
          FI-Strekkode:SCREEN-VALUE = ''.
          RETURN NO-APPLY.
      END.
  END.


  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON LEAVE OF FI-Vg IN FRAME Dialog-Frame /* Varegruppe */
DO:
    IF NOT can-find(VarGr where
                    VarGr.Vg = INT(FI-Vg:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig varegruppe.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Vg Dialog-Frame
ON TAB OF FI-Vg IN FRAME Dialog-Frame /* Varegruppe */
OR 'RETURN' OF FI-Vg
DO:
  DO WITH FRAME DIALOG-FRAME:
      IF INT(FI-Vg:SCREEN-VALUE) > 0 THEN
      DO:
          FIND VarGr NO-LOCK WHERE
              VarGr.Vg = int(FI-Vg:SCREEN-VALUE) NO-ERROR.
          IF AVAILABLE VarGr THEN
              DISPLAY VarGr.VgBeskr @ FI-VgBeskr.
          ELSE DO:
              FI-VgBeskr:SCREEN-VALUE = ''.
              RETURN NO-APPLY.
          END.
          APPLY LASTKEY.
      END.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-VMId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VMId Dialog-Frame
ON LEAVE OF FI-VMId IN FRAME Dialog-Frame /* Varemerke */
DO:
    IF NOT can-find(Varemerke where
                    Varemerke.VmId = INT(FI-VmId:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig varemerke.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VMId Dialog-Frame
ON TAB OF FI-VMId IN FRAME Dialog-Frame /* Varemerke */
OR "RETURN":U OF FI-VMId
DO:
    DO WITH FRAME  DIALOG-FRAME:
        IF INT(FI-VmId:SCREEN-VALUE) > 0 THEN
        DO:
            FIND VareMerke NO-LOCK WHERE
              Varemerke.VMId = int(FI-VMId:SCREEN-VALUE) NO-ERROR.
            IF NOT AVAILABLE Varemerke THEN
              DO:
                DISPLAY
                  "" @ FI-VmId
                  "" @ FI-VmBeskrivelse
                WITH FRAME DIALOG-FRAME. 
                RETURN NO-APPLY.
              END.
            DISPLAY
              VareMerke.Beskrivelse @ FI-VmBeskrivelse
            WITH FRAME DIALOG-FRAME.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iButikkNr Dialog-Frame
ON TAB OF iButikkNr IN FRAME Dialog-Frame /* Butikk */
OR RETURN OF ibutikkNr
DO:
    IF NOT CAN-FIND(Butiker WHERE
                    Butiker.Butik = INT(iButikkNr:SCREEN-VALUE)) THEN
    DO:
        MESSAGE 'Ugyldig butikknr. for varemottak angitt. '
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON "ALT-O":U ANYWHERE
  DO:
    DO WITH FRAME DIALOG-FRAME:
        APPLY 'CHOOSE' TO Btn_Ok.
    END.
  END.
iProfilNr = 1.
{syspara.i 5 1 1 cCl}
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik).
{syspara.i 2 4 12 cTekst}
IF CAN-DO('1,J,Y,Ja,Yes,True',cTekst) THEN
    bLagerstyrt = TRUE.
{syspara.i 2 5 104 cTekst}
IF CAN-DO('1,J,Y,Ja,Yes,True',cTekst) THEN
    bVaremottak = TRUE.

IF cOptProfilbutik <> "" THEN DO:
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
    IF AVAIL bruker THEN DO:
        FIND butiker WHERE butiker.butik = Bruker.ButikkNr NO-LOCK NO-ERROR.
        IF AVAIL butiker AND string(Butiker.clButikkNr) <> TRIM(cCL) THEN DO:
            IF butiker.butik <> Butiker.clButikkNr THEN DO:
                cTmpCL = STRING(Butiker.clButikkNr).
                FIND butiker WHERE butiker.butik = INT(cTmpCL) NO-LOCK NO-ERROR.
            END.
            IF AVAIL butiker AND CAN-FIND(Prisprofil WHERE Prisprofil.ProfilNr = butiker.profilnr) THEN DO:
                cCL = cTmpCL.
                iProfilNr = butiker.profilnr.
            END.
        END.
    END.
END.
ELSE DO:
    FIND butiker WHERE butiker.butik = INT(cCL) NO-LOCK NO-ERROR.
    IF AVAIL butiker THEN
        iProfilNr = butiker.profilnr.
END.



/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
{lng.i}
  RUN initCB.
  
  DO WITH FRAME DIALOG-FRAME:
      IF bVareMottak = FALSE THEN
        ASSIGN
          iButikkNr:HIDDEN        = TRUE
          BUTTON-SokButikk:HIDDEN = TRUE
          lAntall:HIDDEN          = TRUE
          .

      ASSIGN
      T-LokPris:CHECKED          = TRUE 
      T-Lagerstyrt:CHECKED       = bLagerstyrt
      CB-SalgsEnhet:screen-value = 'Stk'
      iButikkNr:SCREEN-VALUE     = cCL
      .
      IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = lArtikkelNr) THEN
      DO:
          FIND ArtBas NO-LOCK WHERE 
              ArtBas.ArtikkelNr = lArtikkelNr.
          FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
          FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
          FIND VarGr OF ArtBAs NO-LOCK NO-ERROR.
          FIND Varemerke OF ArtBAs NO-LOCK NO-ERROR.
          FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
          FIND bufArtBas WHERE bufArtBas.ArtikkelNr = ArtBAs.LinkVareNr NO-LOCK NO-ERROR.
          DISPLAY
              ArtBas.LevNr @ FI-LevNr
              LevBas.LevNamn WHEN AVAILABLE LevBas @ FI-LevNamn
              ArtBas.LevKod @ FI-LEvKod
              ArtBas.Vg @ FI-Vg
              VarGr.VgBeskr WHEN AVAILABLE VarGr @ FI-VgBeskr
              ArtBas.Beskr @ FI-Beskr
              ArtBas.BongTekst @ FI-BongTekst
              ArtBas.VmId @ FI-VmId
              Varemerke.Beskrivelse WHEN AVAILABLE Varemerke @ FI-VmBeskrivelse
              ArtBas.ProdNr @ FI-ProdNr
              Produsent.Beskrivelse WHEN AVAILABLE Produsent @ FI-ProdBeskrivelse
              ArtBas.Etikettekst1 @ FI-Etikettekst1
              ArtBas.LinkVareNr @ FI-LinkVareNr
              bufArtBas.Beskr WHEN AVAILABLE bufArtBAs @ FI-LinkVareTekst
              ArtBas.Mengde @ FI-Mengde
              ArtPris.Varekost[1] @ FI-InnkjopsPris
              ArtPris.Db%[1] @ FI-Db%
              ArtPris.Pris[1] @ FI-Pris
              ArtBas.AntIPakn @ FI-AntIPakn
              .
          ASSIGN 
              T-Anbrekk:SCREEN-VALUE = STRING(ArtBas.Anbrekk)
              FI-JamforPris:SCREEN-VALUE = STRING(ROUND(ArtPris.Pris[1] / ArtBas.Mengde,2))
              CB-SalgsEnhet:SCREEN-VALUE = STRING(ArtBas.SalgsEnhet) 
              CB-JamforEnhet:SCREEN-VALUE = STRING(ArtBas.JamforEnhet) 
              T-Lagerstyrt:SCREEN-VALUE = STRING(ArtBas.Lager)
              T-Kjedevare:SCREEN-VALUE = STRING(ArtBas.KjedeVare)
              T-LokPris:SCREEN-VALUE = STRING(ArtBas.LokPris)
              lArtikkelNr = 0
              .
          RELEASE ArtBas.
      END.
  END.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dupliceraArtpris Dialog-Frame 
PROCEDURE dupliceraArtpris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH butiker WHERE butiker.sentrallager = TRUE NO-LOCK.
        IF butiker.profilnr = artpris.profilnr THEN
            NEXT.
        IF butiker.profilnr = 0 THEN
            NEXT.
        IF NOT CAN-FIND(duplicateArtpris WHERE duplicateArtpris.artikkelnr = artpris.artikkelnr AND
                                               duplicateArtpris.profilnr   = butiker.profilnr) THEN DO:
            CREATE duplicateArtpris.
            BUFFER-COPY artpris EXCEPT profilnr TO duplicateArtpris
                ASSIGN duplicateArtpris.profilnr = butiker.profilnr NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE duplicateArtpris.
            RELEASE duplicateArtpris.
        END.
    END.
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
  DISPLAY FI-Db% FI-Strekkode FI-LevNr FI-LevNamn FI-LevKod FI-Bestillingsnummer 
          FI-Vg FI-VgBeskr FI-Beskr FI-Bongtekst FI-VMId FI-VMBeskrivelse 
          FI-ProdNr FI-ProdBeskrivelse FI-Etikettekst1 FI-LinkVareNr 
          FI-LinkVareTekst FI-Innkjopspris FI-Pris CB-SalgsEnhet CB-JamforEnhet 
          FI-Mengde FI-JamforPris FI-AntIPakn T-Anbrekk T-LokPris T-Kjedevare 
          T-Lagerstyrt iButikkNr lAntall 
      WITH FRAME Dialog-Frame.
  ENABLE FI-Strekkode FI-LevNr FI-LevKod FI-Bestillingsnummer FI-Vg FI-Beskr 
         FI-Bongtekst FI-VMId BUTTON-SokVg FI-ProdNr FI-Etikettekst1 
         FI-LinkVareNr FI-Innkjopspris FI-Pris CB-SalgsEnhet CB-JamforEnhet 
         FI-Mengde FI-AntIPakn T-Anbrekk T-LokPris T-Kjedevare T-Lagerstyrt 
         iButikkNr lAntall Btn_OK Btn_Cancel B-SokLinkvare BUTTON-SokLev 
         BUTTON-SokVaremerke BUTTON-SokProdusent BUTTON-SokButikk RECT-70 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCB Dialog-Frame 
PROCEDURE initCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  DO WITH FRAME DIALOG-FRAME:
      pcTekst = ' '.    
      FOR EACH Salgsenhet NO-LOCK
        BY SalgsEnhet.SalgsEnhTekst:

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst <> ""
                           THEN ","
                           ELSE "") + 
                        Salgsenhet.SalgsEnhTekst
              .
      END.
      IF pcTekst <> "" THEN
          ASSIGN
          CB-SalgsEnhet:LIST-ITEMS = pcTekst
          CB-SalgsEnhet:SCREEN-VALUE    = ENTRY(2,pcTekst).

      ASSIGN
        pcTekst = '<Ikke angitt>, '.
      FOR EACH JamforEnhet NO-LOCK:
          ASSIGN
              pcTekst = pcTekst + 
                   (IF pcTekst <> ""
                     THEN ","
                     ELSE "") + 
                   JamforEnhet.JamforEnhTekst + "," + 
                   JamforEnhet.JamforEnhet.
      END.
      IF pcTekst <> "" THEN
          ASSIGN
          CB-JamforEnhet:LIST-ITEM-PAIRS = pcTekst
          CB-JamforEnhet:SCREEN-VALUE = ENTRY(2,pcTekst).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initPris Dialog-Frame 
PROCEDURE initPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
             ArtPris.Mva%[1]         = Moms.MomsProc
             ArtPris.MvaKr[1]        = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
             ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
             ArtPris.Db%[1]          = ROUND((ArtPris.DbKr[1] * 100) / (ArtPris.VareKost[1] + ArtPris.DbKr[1]),2)
             ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
  
------------------------------------------------------------------------------*/
DEF VAR lInnkjopsPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lRabatt       AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lFrakt        AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lDivKost      AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lVarekost     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lDBKr         AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lMvaKr        AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lPris         AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.

DO WITH FRAME Dialog-Frame:
    IF DEC(FI-InnkjopsPris:SCREEN-VALUE) = 0 THEN
        RETURN.
    IF DEC(FI-Pris:SCREEN-VALUE) <> 0 THEN
        RETURN.

    IF INT(FI-LevNr:SCREEN-VALUE) <> 0 AND 
        CAN-FIND(LevBas WHERE 
                 LevBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)) THEN
    DO:
        FIND LevBas NO-LOCK WHERE
            LevBas.LevNr = INT(FI-LevNr:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE LevBas THEN
            RETURN.
        FIND VarGr NO-LOCK WHERE
            VarGr.Vg = INT(FI-Vg:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE VarGr THEN
            RETURN.
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Moms THEN
            RETURN.

        ASSIGN lInnkjopsPris = DEC(FI-InnkjopsPris:SCREEN-VALUE).

        IF LevBas.Rab1% > 0 THEN
            ASSIGN lRabatt  = ROUND((lInnkjopsPris * LevBas.Rab1%) / 100,2)
                   lVareKost = lInnkjopspris - lRabatt.
        IF LevBas.Frakt% > 0 THEN
            ASSIGN lFrakt    = ROUND((lVareKost * LevBas.Frakt%) / 100,2)
                   lVareKost = lVareKost + lFrakt.
        IF LevBas.DivKost% > 0 THEN
            ASSIGN lDivKost  = ROUND((lVareKost * LevBas.DivKost%) / 100,2)
                   lVareKost = lVareKost + lDivKost.
        IF LevBas.Rab2% > 0 THEN
            ASSIGN lDbKr  = ROUND((lVareKost / (1 - (LevBas.Rab2% / 100))),2) - lVareKost.
        IF Moms.MomsProc > 0 THEN
            ASSIGN lMvaKr  = ROUND(((lVareKost + lDbKr) * Moms.MomsProc) / 100,2).
        ASSIGN 
            lPris = lVareKost + lDbKr + lMvaKr
            FI-Pris:SCREEN-VALUE = STRING(lPris).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettArtikkel Dialog-Frame 
PROCEDURE opprettArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lInnkjopsPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lRabatt       AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lFrakt        AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lDivKost      AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR lVarekost     AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  
DEF VAR iTransNr AS INTE NO-UNDO.
DEF VAR iBatchNr AS INT NO-UNDO.
              
/* Legger riktig konvertert kode i cEAN13. */
  RUN sjekkStrekkode.

  DO WITH FRAME DIALOG-FRAME:
      /* Valideringer */
      IF NOT CAN-FIND(VarGr WHERE
                      VarGr.Vg = INT(FI-VG:SCREEN-VALUE)) THEN
      DO:
          MESSAGE 'Ugyldig varegruppe'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-Vg.
          RETURN 'ERROR Ugyldig varegruppe'.
      END.
      IF NOT CAN-FIND(LevBas WHERE
                      LevBas.LevNr = INT(FI-LevNr:SCREEN-VALUE)) THEN
      DO:
          MESSAGE 'Ugyldig leverandør'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-LevNr.
          RETURN 'ERROR Ugyldig leverandør'.
      END.
      IF NOT CAN-FIND(Varemerke WHERE
                      Varemerke.VmId = INT(FI-VmId:SCREEN-VALUE)) THEN
      DO:
          MESSAGE 'Ugyldig varemerke'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-Vg.
          RETURN 'ERROR Ugyldig varemerke'.
      END.
      IF NOT CAN-FIND(Produsent WHERE
                      Produsent.ProdNr = INT(FI-ProdNr:SCREEN-VALUE)) THEN
      DO:
          MESSAGE 'Ugyldig produsent'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-ProdNr.
          RETURN 'ERROR Ugyldig produsent'.
      END.
      IF DEC(FI-LinkVareNr:SCREEN-VALUE) > 0 AND  NOT CAN-FIND(ArtBas WHERE
                      ArtBas.ArtikkelNr = DEC(FI-LinkVareNr:SCREEN-VALUE)) THEN
      DO:
          MESSAGE 'Ugyldig pantartikkel'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-LinkVareNr.
          RETURN 'ERROR Ugyldig pantartikkel'.
      END.
      IF CAN-FIND(Strekkode WHERE
                      Strekkode.Kode = cEAN13) THEN
      DO:
          MESSAGE 'Artikkel er allerede registrert med denne strekkoden.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-Strekkode.
          RETURN 'ERROR Artikkel er allerede registrert med denne strekkoden.'.
      END.
      IF CAN-FIND(ArtBas WHERE
                      ArtBAs.ArtikkelNr = dec(cEAN13)) THEN
      DO:
          MESSAGE 'Artikkel er allerede registrert med denne strekkoden som artikkelnr.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-Strekkode.
          RETURN 'ERROR Artikkel er allerede registrert med denne strekkoden som artikkelnr'.
      END.
      IF DEC(FI-Pris:SCREEN-VALUE) = 0 THEN
      DO:
          MESSAGE 'Det må legges inn pris på artikkelen.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY 'ENTRY' TO FI-Pris.
          RETURN 'ERROR Det må legges inn pris på artikkelen'.
      END.

      /* Oppretter artikkelen. */
      DO TRANSACTION:
        FIND LevBas NO-LOCK WHERE
            LevBas.LevNr = INT(FI-LEvNr:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE LevBas THEN RETURN 'ERROR Ukjent leverandørnummer'.

        FIND VarGr NO-LOCK WHERE
            VarGr.Vg = INT(FI-Vg:SCREEN-VALUE) NO-ERROR.
        IF NOT AVAILABLE VarGr THEN RETURN 'ERROR Ukjent varegruppe'.

        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Moms THEN RETURN 'ERROR Finner ikke Mva på varegruppe'.
        
        FIND FIRST VgKat WHERE VgKat.Vg = VarGr.Vg NO-ERROR.
        IF NOT AVAILABLE VgKat THEN RETURN 'ERROR Det er ikke lagt opp kategorier på varegruppen'.

        CREATE ArtBas.
        ASSIGN
            ArtBas.ArtikkelNr  = DEC(cEAN13)
            ArtBas.LevKod      = FI-LevKod:SCREEN-VALUE
            ArtBas.StrTypeId   = 2
            ArtBas.Vg          = INT(FI-Vg:SCREEN-VALUE)
            ArtBas.Hg          = VarGr.Hg
            ArtBas.VgKat       = VgKat.VgKat
            ArtBas.Beskr       = FI-Beskr:SCREEN-VALUE
            ArtBas.BongTekst   = FI-BongTekst:SCREEN-VALUE
            ArtBas.LevNr       = INT(FI-LEvNr:SCREEN-VALUE)
            ArtBas.VmId        = INT(FI-VmId:SCREEN-VALUE)
            ArtBas.ProdNr      = INT(FI-ProdNr:SCREEN-VALUE)
            ArtBas.Etikettekst1 = FI-Etikettekst1:SCREEN-VALUE
            ArtBas.LinkVareNr  = DEC(FI-LinkVareNr:SCREEN-VALUE)
            ArtBas.SalgsEnhet  = CB-Salgsenhet:SCREEN-VALUE
            ArtBas.JamforEnhet = CB-JamforEnhet:SCREEN-VALUE
            ArtBas.ValKod      = LevBas.ValKod
            ArtBas.Etikett     = 2
            ArtBas.Mengde      = DEC(FI-Mengde:SCREEN-VALUE)
            ArtBas.Lager       = T-Lagerstyrt:CHECKED
            ArtBas.KjedeVare   = T-Kjedevare:CHECKED 
            ArtBas.LokPris     = T-LokPris:CHECKED  
            lArtikkelNr        = ArtBas.ArtikkelNr
            ArtBas.Pant        = CAN-DO('8',STRING(iArtSlag))
            ArtBas.Pakke       = CAN-DO('7',STRING(iArtSlag))
            ArtBas.Vekt        = CAN-DO('1,2',STRING(iArtSlag))
            ArtBas.iKasse      = CAN-DO('0,1,2,3,4,5,6,8',STRING(iArtSlag))
            ArtBas.Anbrekk     = T-Anbrekk:CHECKED
            ArtBas.AntIPakn    = INT(FI-AntIPakn:SCREEN-VALUE)
            .
        ASSIGN
            ArtBas.ArtSlag     = settArtSlag(iArtSlag)
            .
         CREATE Strekkode.
         ASSIGN
             Strekkode.Kode              = cEAN13
             Strekkode.ArtikkelNr        = DEC(cEAN13)
             Strekkode.STrKode           = 1
             Strekkode.KodeType          = 1
             Strekkode.HovedNr           = TRUE
             Strekkode.iKasse            = TRUE
             STrekkode.Bestillingsnummer = FI-BestillingsNummer:SCREEN-VALUE
             .

         ASSIGN lInnkjopsPris = DEC(FI-InnkjopsPris:SCREEN-VALUE).

         IF LevBas.Rab1% > 0 THEN
             ASSIGN lRabatt  = (lInnkjopsPris * LevBas.Rab1%) / 100
                    lVareKost = lInnkjopspris - lRabatt.
         IF LevBas.Frakt% > 0 THEN
             ASSIGN lFrakt    = (lVareKost * LevBas.Frakt%) / 100
                    lVareKost = lVareKost + lFrakt.
         IF LevBas.DivKost% > 0 THEN
             ASSIGN lDivKost  = (lVareKost * LevBas.DivKost%) / 100
                    lVareKost = lVareKost + lDivKost.

         CREATE ArtPris.
         ASSIGN
             ArtPris.ArtikkelNr = DEC(cEAN13)
             ArtPris.ProfilNr   = iProfilnr
             ArtPris.ValPris[1]      = DEC(FI-InnkjopsPris:SCREEN-VALUE)
             ArtPris.InnkjopsPris[1] = DEC(FI-InnkjopsPris:SCREEN-VALUE)
             ArtPris.Rab1%[1]        = LevBas.Rab1%
             ArtPris.Rab1Kr[1]       = lRabatt
             ArtPris.Frakt%[1]       = LevBas.Frakt%
             ArtPris.Frakt[1]        = lFrakt
             ArtPris.DivKost%[1]     = LevBas.DivKost%
             ArtPris.DivKostKr[1]    = lDivKost
             ArtPris.VareKost[1]     = ArtPris.InnkjopsPris[1] - lRabatt + lFrakt + lDivKost             
             ArtPris.Pris[1]         = DEC(FI-Pris:SCREEN-VALUE)
             ArtPris.MomsKod         = VarGr.MomsKod
             ArtPris.Mva%[1]         = Moms.MomsProc
             ArtPris.MvaKr[1]        = ArtPris.Pris[1] - (ArtPris.Pris[1] / (1 + (ArtPris.Mva%[1] / 100)))
             ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
             ArtPris.Db%[1]          = ROUND((ArtPris.DbKr[1] * 100) / (ArtPris.VareKost[1] + ArtPris.DbKr[1]),2)
             ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
             ArtPris.AktivFraDato    = TODAY
             ArtPris.AktivFraTid     = TIME
             ArtPris.Tilbud          = FALSE
             .
          FIND CURRENT ArtPris NO-LOCK.
          RUN settlopnr.p (INPUT ArtBas.Vg, INPUT 'N', OUTPUT ArtBas.LopNr).
          FIND CURRENT ArtBas NO-LOCK.
          IF cOptProfilbutik <> "" THEN DO:
              RUN dupliceraArtpris.
          END.
          /* Varemottak */
          IF bVaremottak AND 
              INT(iButikkNr:SCREEN-VALUE) > 0 AND 
             CAN-FIND(Butiker WHERE Butiker.Butik = INT(iButikkNr:SCREEN-VALUE)) AND 
             DEC(lAntall:SCREEN-VALUE) <> 0 THEN
          DO:
              RUN batchlogg.w (program-name(1),"Varemottak ny artikkel: " + STRING(ArtBas.Artikkelnr),output iBatchNr).

              /* Oppretter transaksjon */
              IF CAN-FIND(TransLogg WHERE
                          TransLogg.Butik   = INT(iButikkNr:SCREEN-VALUE) AND
                          TransLogg.TransNr = iTransNr) THEN
                NESTE_NR:
                DO WHILE TRUE:
                  ASSIGN iTransNr = iTransNr + 1.
                  IF CAN-FIND(TransLogg WHERE
                              TransLogg.Butik   = INT(iButikkNr:SCREEN-VALUE) AND
                              TransLogg.TransNr = iTransNr) THEN
                    NEXT NESTE_NR.
                  ELSE
                    LEAVE NESTE_NR.
                END. /* NESTE_NR */

              CREATE TransLogg.
              ASSIGN TransLogg.Butik         = INT(iButikkNr:SCREEN-VALUE) 
                     TransLogg.TransNr       = iTransNr
                     TransLogg.SeqNr         = 1.
              ASSIGN TransLogg.BatchNr       = iBatchNr
                     TransLogg.KundNr        = 0
                     TransLogg.TTId          = 5 /* Varekjøp */
                     TransLogg.TBId          = 1
                     TransLogg.ArtikkelNr    = ArtBas.ArtikkelNr
                     TransLogg.LevNr         = ArtBas.LevNr
                     TransLogg.BongId        = 0
                     TransLogg.BongLinjeNr   = 0
                     TransLogg.KassaNr       = 0
                     TransLogg.Vg            = ArtBas.Vg
                     TransLogg.LopNr         = ArtBas.LopNr
                     TransLogg.Antall        = DEC(lAntall:SCREEN-VALUE)
                     TransLogg.Pris          = ArtPris.VareKost[1]
                     TransLogg.RabKr         = 0
                     TransLogg.Mva           = 0
                     TransLogg.Plukket       = TRUE /* Skal ikke ut på plukkliste */
                     TransLogg.Dato          = TODAY
                     TransLogg.Tid           = TIME
                     TransLogg.SattVVareKost = FALSE /* TRUE */
                     TransLogg.BestNr        = 99 /* förslag när vi gör Forenklet varemottak */
                     TransLogg.Postert       = FALSE
                     TransLogg.IndividNr     = 0                                             
                     TransLogg.Storl         = ' 1'
                     TransLogg.Kode          = FI-Strekkode:SCREEN-VALUE
                     .
              RELEASE Translogg.
              RUN batchstatus.p (iBatchNr, 2).
          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkStrekkode Dialog-Frame 
PROCEDURE sjekkStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME DIALOG-FRAME:
      cEAN13 = FI-Strekkode:SCREEN-VALUE.
      IF LENGTH(cEAN13) > 5 THEN
      DO:
        ASSIGN 
            cEAN13 = FILL("0",13 - LENGTH(cEAN13)) + cEAN13.
        RUN bibl_chkean.p (INPUT-OUTPUT cEAN13).
        IF NOT CAN-DO(',0',TRIM(RETURN-VALUE)) THEN 
          DO:
            MESSAGE 'Kontroll av strekkode avbrutt med melding:' SKIP 
                RETURN-VALUE
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN "AVBRYT".
          END.
        FI-Strekkode:SCREEN-VALUE = cEAN13.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION settArtSlag Dialog-Frame 
FUNCTION settArtSlag RETURNS INTEGER
  ( INPUT iArtSlag AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE iArtSLag:
      /* Stykkvare (Stk)       */ WHEN  0 THEN iArtSLag = 0.
      /* Vektvare (Kg)         */ WHEN  1 THEN iArtSLag = 1.
      /* Vektvare (Hg)         */ WHEN  2 THEN iArtSLag = 2.
      /* Metervare (m)         */ WHEN  3 THEN iArtSLag = 3.
      /* Kvadratmetervare (m2) */ WHEN  4 THEN iArtSLag = 4.
      /* Volumvare (l)         */ WHEN  5 THEN iArtSLag = 5.
      /* Pakkevare (Stk)       */ WHEN  7 THEN iArtSLag = 0.
      /* Pant (Stk)            */ WHEN  8 THEN iArtSLag = 0.
      OTHERWISE iArtSlag = 0.  /* Stk er default. */
  END CASE.


  RETURN iArtSlag.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

