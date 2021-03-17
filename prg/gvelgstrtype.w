&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
&IF "{&UIB_is_Running}" = ""  &THEN
  DEF INPUT-OUTPUT PARAMETER iStrTypeId AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iSaSong    AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iVarGr     AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iLevNr     AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iFarg      AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER bLager     AS LOG  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iMatKod    AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iProdNr    AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER iVmId      AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER cValKod    AS CHAR NO-UNDO.
&else
  DEF VAR iStrTypeId AS INT  NO-UNDO.
  DEF VAR iSaSong    AS INT  NO-UNDO.
  DEF VAR iVarGr     AS INT  NO-UNDO.
  DEF VAR iLevNr     AS INT  NO-UNDO.
  DEF VAR iFarg      AS INT  NO-UNDO.
  DEF VAR bLager     AS LOG  NO-UNDO.
  DEF VAR iMatKod    AS INT  NO-UNDO.
  DEF VAR iProdNr    AS INT  NO-UNDO.
  DEF VAR iVmId      AS INT  NO-UNDO.
  DEF VAR cValKod    AS CHAR NO-UNDO.
&ENDIF  

/* Local Variable Definitions ---                                       */
DEF VAR cTekst     AS CHAR NO-UNDO.
DEF VAR cStrTypeId AS CHAR NO-UNDO.
DEF VAR iInt       AS INT  NO-UNDO.
DEF VAR cOk        AS CHAR NO-UNDO.
DEF VAR cLookupValue AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SokFarg RECT-55 FI-StrTypeId FI-SaSong ~
FI-VareGr FI-LevNr FI-Farg T-Lager FI-MatKod FI-ProdNr FI-VmId FI-ValKod ~
Btn_Cancel Btn_OK B-SokMatKod B-SokProdNr B-SokVmId B-SokValKod B-SokLevNr ~
BUTTON-SokSesong B-SokVarGr BUTTON-SokStrType 
&Scoped-Define DISPLAYED-OBJECTS FI-StrTypeId FI-SaSong FI-VareGr FI-LevNr ~
FI-Farg T-Lager FI-MatKod FI-ProdNr FI-VmId FI-ValKod 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokFarg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokLevNr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokMatKod 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokProdNr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokValKod 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokVarGr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokVmId 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SokSesong 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokStrType 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Farg AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MatKod AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Material" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ProdNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Produsent" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SaSong AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Sesong" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrTypeId AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Størrelsestype" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ValKod AS CHARACTER FORMAT "X(3)":U 
     LABEL "Valuta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VareGr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VmId AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Varemerke" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 12.86.

DEFINE VARIABLE T-Lager AS LOGICAL INITIAL no 
     LABEL "Lagerstyrt" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     B-SokFarg AT ROW 6.48 COL 35
     FI-StrTypeId AT ROW 2.67 COL 19 COLON-ALIGNED
     FI-SaSong AT ROW 3.62 COL 19 COLON-ALIGNED
     FI-VareGr AT ROW 4.57 COL 19 COLON-ALIGNED
     FI-LevNr AT ROW 5.52 COL 19 COLON-ALIGNED
     FI-Farg AT ROW 6.48 COL 19 COLON-ALIGNED
     T-Lager AT ROW 7.67 COL 21
     FI-MatKod AT ROW 8.62 COL 19 COLON-ALIGNED
     FI-ProdNr AT ROW 9.57 COL 19 COLON-ALIGNED
     FI-VmId AT ROW 10.52 COL 19 COLON-ALIGNED
     FI-ValKod AT ROW 11.48 COL 19 COLON-ALIGNED
     Btn_Cancel AT ROW 14.1 COL 39
     Btn_OK AT ROW 14.19 COL 1
     B-SokMatKod AT ROW 8.62 COL 35
     B-SokProdNr AT ROW 9.57 COL 35
     B-SokVmId AT ROW 10.52 COL 35
     B-SokValKod AT ROW 11.48 COL 35
     B-SokLevNr AT ROW 5.57 COL 35
     BUTTON-SokSesong AT ROW 3.67 COL 35
     B-SokVarGr AT ROW 4.62 COL 35
     BUTTON-SokStrType AT ROW 2.67 COL 35
     RECT-55 AT ROW 1 COL 1
     SPACE(0.00) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Aksept/endring av artikkelparametre"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON GO OF FRAME gDialog /* Aksept/endring av artikkelparametre */
DO:
  ASSIGN
      FI-StrTypeId
      FI-Sasong
      FI-VareGr
      FI-LevNr
      FI-Farg
      T-Lager 
      FI-MatKod
      FI-ProdNr
      FI-VmId  
      FI-ValKod
      iStrTypeId = FI-StrTypeId
      iSaSong    = FI-Sasong
      iVarGr     = FI-VareGr
      iLevNr     = FI-LevNr
      iFarg      = FI-Farg
      bLager     = T-Lager
      iMatKod    = FI-MatKod
      iProdNr    = FI-ProdNr
      iVmId      = FI-VmId
      cValKod    = FI-ValKod
      .

  IF NOT CAN-FIND(StrType WHERE
                StrType.StrTypeId = iStrTypeId) THEN
  DO:
    MESSAGE "Størrelsestype må angis og være forskjellig fra 1."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
  IF NOT CAN-FIND(Sasong WHERE
                  Sasong.Sasong = iSaSong) THEN
  DO:
    MESSAGE "Ugyldig sesongkode."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
  IF NOT CAN-FIND(VarGr WHERE
                  VarGr.Vg = iVarGr) THEN
  DO:
    MESSAGE "Ugyldig varegruppe."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
  IF NOT CAN-FIND(LevBas WHERE
                LevBas.LevNr = iLevNr) THEN
  DO:
    MESSAGE "Ukjent leverandør."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.
  IF NOT CAN-FIND(Farg WHERE
                Farg.Farg = iFarg) THEN
  DO:
    MESSAGE "Ukjent fargekode."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.

  IF NOT CAN-FIND(Material WHERE
                Material.MatKod = iMatKod) THEN
  DO:
    MESSAGE "Ukjent materialkode."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.

  IF NOT CAN-FIND(Produsent WHERE
                Produsent.ProdNr = iProdNr) THEN
  DO:
    MESSAGE "Ukjent produsentnummer."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.

  IF NOT CAN-FIND(Varemerke WHERE
                Varemerke.VmId = iVmId) THEN
  DO:
    MESSAGE "Ukjent varemerke."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.

  IF NOT CAN-FIND(Valuta WHERE
                Valuta.ValKod = cValKod) THEN
  DO:
    MESSAGE "Ukjent valutakode."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
  END.



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Aksept/endring av artikkelparametre */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
    ASSIGN
        cOk = "AVBRYT"
        .
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokFarg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokFarg gDialog
ON CHOOSE OF B-SokFarg IN FRAME gDialog /* ... */
or F10 of FI-LevNr
DO:
  DO WITH FRAME DIALOG-FRAME:
      cTekst = "Farg".
      RUN JBoxDLookup.w ("Farg;FarBeskr;Farg", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Farg NO-LOCK WHERE
        Farg.Farg = INT(cTekst) NO-ERROR.
      IF AVAILABLE Farg THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Farg:SCREEN-VALUE   = cTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Farg:SCREEN-VALUE    = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr gDialog
ON CHOOSE OF B-SokLevNr IN FRAME gDialog /* ... */
or F10 of FI-LevNr
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
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-LevNr:SCREEN-VALUE    = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMatKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMatKod gDialog
ON CHOOSE OF B-SokMatKod IN FRAME gDialog /* ... */
or F10 of FI-MatKod
DO:
  DO WITH FRAME DIALOG-FRAME:
      cTekst = "MatKod".
      RUN JBoxDLookup.w ("Material;MatBeskr;MatKod", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Material NO-LOCK WHERE
        Material.MatKod = INT(cTekst) NO-ERROR.
      IF AVAILABLE Material THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-MatKod:SCREEN-VALUE   = cTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-MatKod:SCREEN-VALUE    = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokProdNr gDialog
ON CHOOSE OF B-SokProdNr IN FRAME gDialog /* ... */
or F10 of FI-ProdNr
DO:
  DO WITH FRAME DIALOG-FRAME:
      cTekst = "ProdNr".
      RUN JBoxDLookup.w ("Produsent;Beskrivelse;ProdNr", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Produsent NO-LOCK WHERE
        Produsent.ProdNr = INT(cTekst) NO-ERROR.
      IF AVAILABLE Produsent THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-ProdNr:SCREEN-VALUE   = cTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-ProdNr:SCREEN-VALUE    = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokValKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokValKod gDialog
ON CHOOSE OF B-SokValKod IN FRAME gDialog /* ... */
or F10 of FI-ValKod
DO:
  DO WITH FRAME DIALOG-FRAME:
      cTekst = "ValKod".
      RUN JBoxDLookup.w ("Valuta;ValKod;ValNavn;ValKurs;ValDatum;ValLand", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Valuta NO-LOCK WHERE
        Valuta.ValKod = (cTekst) NO-ERROR.
      IF AVAILABLE Handtering THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-ValKod:SCREEN-VALUE   = cTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-ValKod:SCREEN-VALUE    = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokVarGr gDialog
ON CHOOSE OF B-SokVarGr IN FRAME gDialog /* ... */
or F10 of FI-VareGr
DO:

  DO WITH FRAME DIALOG-FRAME:
      cLookupValue = "Vg".
      RUN JBoxDLookup.w ("VarGr;VgBeskr|Beskrivelse|x(30);Vg", "where true", INPUT-OUTPUT cLookupValue).
      IF cLookupValue <> "" THEN
      DO:
          ASSIGN
              FI-VareGr:SCREEN-VALUE = STRING(cLookupValue)
              FI-VareGr:BGCOLOR      = ?
              .
      END.
      ELSE RETURN NO-APPLY.

      FIND VarGr NO-LOCK WHERE
        VarGr.Vg = INT(cLookupValue) NO-ERROR.
      IF AVAILABLE VarGr THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Varegr:SCREEN-VALUE = cLookupValue
            iInt                   = INT(cLookupValue)
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Varegr:SCREEN-VALUE    = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokVmId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokVmId gDialog
ON CHOOSE OF B-SokVmId IN FRAME gDialog /* ... */
or F10 of FI-VMId
DO:
  DO WITH FRAME DIALOG-FRAME:
      cTekst = "VmId".
      RUN JBoxDLookup.w ("Varemerke;Beskrivelse;KortNavn;VmId", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Varemerke NO-LOCK WHERE
        Varemerke.VmId = INT(cTekst) NO-ERROR.
      IF AVAILABLE Varemerke THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-VmId:SCREEN-VALUE   = cTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-VmId:SCREEN-VALUE    = ''
            .
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel gDialog
ON CHOOSE OF Btn_Cancel IN FRAME gDialog /* Avbryt */
DO:
  ASSIGN
      cOk = "AVBRYT"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokSesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokSesong gDialog
ON CHOOSE OF BUTTON-SokSesong IN FRAME gDialog /* ... */
or F10 of FI-SaSong
DO:
  DO WITH FRAME DIALOG-FRAME:
      cTekst = "Sasong".
      RUN JBoxDLookup.w ("Sasong;SasBeskr;Sasong", "where true", INPUT-OUTPUT cTekst).

      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      FIND Sasong NO-LOCK WHERE
        Sasong.Sasong = INT(cTekst) NO-ERROR.
      IF AVAILABLE Sasong THEN
      DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Sasong:SCREEN-VALUE   = cTekst
            .
      END.
      ELSE DO:
          /* Legger opp verdier I de aktuelle feltene */
          ASSIGN
            FI-Sasong:SCREEN-VALUE    = ''
            .
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokStrType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokStrType gDialog
ON CHOOSE OF BUTTON-SokStrType IN FRAME gDialog /* ... */
OR F10 OF FI-StrTypeId DO:
    /* Kaller søkerutine */
    RUN gstrtype.w (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "", /* Feltliste avgrensningsfelt (kommaseparert) */
      "", /* Feltverdier (chr(1) sep) */ 
      "",  /* Post markøren skal stå på */
      0,  /* avdnr */
      0,   /* hg */
      ""  /* where-sats ex strtypeid = 2 */
      ).
    IF RETURN-VALUE = "AVBRYT" THEN 
        RETURN NO-APPLY.
    ELSE
        FI-StrTypeId:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1)).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Farg gDialog
ON TAB OF FI-Farg IN FRAME gDialog /* Farge */
OR "RETURN":U OF FI-Farg
DO:
  IF NOT CAN-FIND(Farg WHERE
                  Farg.Farg = INPUT FI-Farg) THEN
  DO:
    MESSAGE "Ugyldig fargekode."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr gDialog
ON TAB OF FI-LevNr IN FRAME gDialog /* Leverandør */
OR "RETURN":U OF FI-LevNr
DO:
  IF NOT CAN-FIND(LevBas WHERE
                  LevBas.LevNr = INPUT FI-LevNr) THEN
  DO:
    MESSAGE "Ugyldig leverandør."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-MatKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-MatKod gDialog
ON TAB OF FI-MatKod IN FRAME gDialog /* Material */
OR "RETURN":U OF FI-MatKod
DO:
  IF NOT CAN-FIND(Material WHERE
                  Material.MatKod = INPUT FI-MatKod) THEN
  DO:
    MESSAGE "Ugyldig materialkode."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ProdNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ProdNr gDialog
ON TAB OF FI-ProdNr IN FRAME gDialog /* Produsent */
OR "RETURN":U OF FI-ProdNr
DO:
  IF NOT CAN-FIND(Produsent WHERE
                  Produsent.ProdNr = INPUT FI-ProdNr) THEN
  DO:
    MESSAGE "Ugyldig produsentnummer."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-SaSong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SaSong gDialog
ON TAB OF FI-SaSong IN FRAME gDialog /* Sesong */
OR "RETURN":U OF FI-Sasong
DO:
  IF NOT CAN-FIND(Sasong WHERE
                  Sasong.Sasong = INPUT FI-Sasong) THEN
  DO:
    MESSAGE "Ugyldig sesongkode."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-StrTypeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrTypeId gDialog
ON TAB OF FI-StrTypeId IN FRAME gDialog /* Størrelsestype */
OR "RETURN":U OF FI-StrTypeId
DO:
  IF INPUT FI-StrTypeId = 1 OR 
  NOT CAN-FIND(StrType WHERE
                  StrType.StrTypeId = INPUT FI-StrTypeId) THEN
  DO:
    MESSAGE "Størrelsestype må angis og være forskjellig fra 1."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ValKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ValKod gDialog
ON TAB OF FI-ValKod IN FRAME gDialog /* Valuta */
OR "RETURN":U OF FI-ValKod
DO:
  IF NOT CAN-FIND(Valuta WHERE
                  Valuta.ValKod = INPUT FI-ValKod) THEN
  DO:
    MESSAGE "Ugyldig valutakode."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-VareGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VareGr gDialog
ON TAB OF FI-VareGr IN FRAME gDialog /* Varegruppe */
OR "RETURN":U OF FI-VareGr
DO:
  IF NOT CAN-FIND(VarGr WHERE
                  VarGr.Vg = INPUT FI-VareGr) THEN
  DO:
    MESSAGE "Ugyldig varegruppe."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
  ELSE
      FI-VareGr:BGCOLOR = ?.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-VmId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VmId gDialog
ON TAB OF FI-VmId IN FRAME gDialog /* Varemerke */
OR "RETURN":U OF FI-VmId
DO:
  IF NOT CAN-FIND(Varemerke WHERE
                  Varemerke.VmId = INPUT FI-VmId) THEN
  DO:
    MESSAGE "Ugyldig varemerke."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{lng.i &SDO = "SDO"}     
ASSIGN
    FI-StrTypeId = iStrTypeId
    FI-Sasong    = iSasong
    FI-VareGr    = iVarGr
    FI-LevNr     = iLevNr
    FI-Farg      = iFarg
    T-Lager      = bLager
    FI-MatKod    = iMatKod
    FI-ProdNr    = iProdNr
    FI-VmId      = iVmId
    FI-ValKod    = cValKod
    .

{src/adm2/dialogmn.i}

RETURN cOk.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY FI-StrTypeId FI-SaSong FI-VareGr FI-LevNr FI-Farg T-Lager FI-MatKod 
          FI-ProdNr FI-VmId FI-ValKod 
      WITH FRAME gDialog.
  ENABLE B-SokFarg RECT-55 FI-StrTypeId FI-SaSong FI-VareGr FI-LevNr FI-Farg 
         T-Lager FI-MatKod FI-ProdNr FI-VmId FI-ValKod Btn_Cancel Btn_OK 
         B-SokMatKod B-SokProdNr B-SokVmId B-SokValKod B-SokLevNr 
         BUTTON-SokSesong B-SokVarGr BUTTON-SokStrType 
      WITH FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SwitchLng.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  {syspara.i 50 15 1 cStrTypeId}
  IF iStrTypeId > 1 THEN
      cStrTypeId = STRING(iStrTypeId).
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FI-StrTypeId:SCREEN-VALUE = cStrTypeId
          .
  END.

  IF CAN-FIND(FIRST VarGr WHERE
              VarGr.Vg = iVarGr) THEN
      ASSIGN
      FI-VareGr:SENSITIVE = TRUE 
      B-SokVarGr:SENSITIVE = TRUE
      FI-VareGr:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-VareGr:SENSITIVE = TRUE 
      B-SokVarGr:SENSITIVE = TRUE
      FI-VareGr:BGCOLOR = 12
      .

  IF CAN-FIND(FIRST LevBas WHERE
              LevBas.LevNr = iLevNr) THEN
      ASSIGN
      FI-LevNR:SENSITIVE = TRUE
      B-SokLevNr:SENSITIVE = TRUE
      FI-LevNr:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-LevNr:SENSITIVE = TRUE 
      B-SokLevNr:SENSITIVE = TRUE
      FI-LevNr:BGCOLOR = 12
      .

  IF CAN-FIND(FIRST Farg WHERE
              Farg.Farg = iFarg) THEN
      ASSIGN
      FI-Farg:SENSITIVE = TRUE
      B-SokFarg:SENSITIVE = TRUE
      FI-Farg:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-Farg:SENSITIVE = TRUE 
      B-SokFarg:SENSITIVE = TRUE
      FI-Farg:BGCOLOR = 12
      .

  IF CAN-FIND(FIRST Material WHERE
              Material.MatKod = iMatKod) THEN
      ASSIGN
      FI-MatKod:SENSITIVE = TRUE
      B-SokMatKod:SENSITIVE = TRUE
      FI-MatKod:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-MatKod:SENSITIVE = TRUE 
      B-SokMatKod:SENSITIVE = TRUE
      FI-MatKod:BGCOLOR = 12
      .

  IF CAN-FIND(FIRST Produsent WHERE
              Produsent.ProdNr = iProdNr) THEN
      ASSIGN
      FI-ProdNr:SENSITIVE = TRUE
      B-SokProdNr:SENSITIVE = TRUE
      FI-ProdNr:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-ProdNr:SENSITIVE = TRUE 
      B-SokProdNr:SENSITIVE = TRUE
      FI-ProdNr:BGCOLOR = 12
      .

  IF CAN-FIND(FIRST Varemerke WHERE
              Varemerke.VmId = iVmId) THEN
      ASSIGN
      FI-VmId:SENSITIVE = TRUE
      B-SokVmId:SENSITIVE = TRUE
      FI-VmId:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-VmId:SENSITIVE = TRUE 
      B-SokVmId:SENSITIVE = TRUE
      FI-VmId:BGCOLOR = 12
      .

  IF CAN-FIND(FIRST Valuta WHERE
              Valuta.ValKod = cValKod) THEN
      ASSIGN
      FI-ValKod:SENSITIVE = TRUE
      B-SokValKod:SENSITIVE = TRUE
      FI-ValKod:BGCOLOR = ?
      .
  ELSE
      ASSIGN
      FI-ValKod:SENSITIVE = TRUE 
      B-SokValKod:SENSITIVE = TRUE
      FI-ValKod:BGCOLOR = 12
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

