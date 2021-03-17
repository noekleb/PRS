&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dkassereroppgj.i"}.



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

DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR lSum   AS DEC  NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR lInngVeksel AS DEC NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dkassereroppgj.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.OpptaltInnVeksel ~
RowObject.OpptaltKontanter RowObject.OpptaltSjekk RowObject.OpptaltReserve ~
RowObject.OpptaltVeksel RowObject.OpptaltLevertBank RowObject.PoseNr ~
RowObject.OpptaltKupong 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS B-Gavekort B-Tilgode B-Bilag B-Kontant ~
B-Valuta RECT-57 RECT-59 RECT-60 RECT-61 RECT-63 RECT-64 RECT-65 RECT-66 
&Scoped-Define DISPLAYED-FIELDS RowObject.ButikkNr RowObject.Dato ~
RowObject.KassererNr RowObject.z_nummer RowObject.OpptaltInnVeksel ~
RowObject.OpptaltKontanter RowObject.OpptaltSjekk RowObject.OpptaltValuta ~
RowObject.OpptaltReserve RowObject.OpptaltBilag RowObject.OpptaltVeksel ~
RowObject.OpptaltTilgode RowObject.OpptaltTilgodeAndre ~
RowObject.OpptaltTilgodeUtlevert RowObject.OpptaltGavekort ~
RowObject.OpptaltGavekortAndre RowObject.OpptaltGavekortUtlevert ~
RowObject.OpptaltLevertBank RowObject.PoseNr RowObject.fuEndretInfo ~
RowObject.OpptaltKupong 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Forklaring FILL-IN-3 FI-Tilgode ~
FI-Gavekort FI-Total FILL-IN-4 FILL-IN-5 FILL-IN-6 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Bilag  NO-FOCUS
     LABEL "&Bilag..." 
     SIZE 15 BY 1.

DEFINE BUTTON B-Butikk 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Dato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Forsalj 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Gavekort  NO-FOCUS
     LABEL "&Gavekort" 
     SIZE 15 BY 1.

DEFINE BUTTON B-Kontant  NO-FOCUS
     LABEL "&Kontant..." 
     SIZE 15 BY 1.

DEFINE BUTTON B-Tilgode  NO-FOCUS
     LABEL "&Tilgode" 
     SIZE 15 BY 1.

DEFINE BUTTON B-Valuta  NO-FOCUS
     LABEL "&Valuta..." 
     SIZE 15 BY 1.

DEFINE VARIABLE FI-Forklaring AS CHARACTER FORMAT "X(256)":U INITIAL "Felter markert med lys grønn fage legges ut på bokføringsbilaget." 
      VIEW-AS TEXT 
     SIZE 126.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Gavekort AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Gavekort" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tilgode AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Tilgode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Total AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Total" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17.4 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Oppgjør" 
      VIEW-AS TEXT 
     SIZE 36 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Beholdninger" 
      VIEW-AS TEXT 
     SIZE 36 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Beholdning tilgode" 
      VIEW-AS TEXT 
     SIZE 36 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U INITIAL "Beholdning gavekort" 
      VIEW-AS TEXT 
     SIZE 36 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.8 BY 14.52.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE .2 BY 14.52.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY .1.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY .1.

DEFINE RECTANGLE RECT-63
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54 BY .1.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.2 BY 14.52.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.2 BY .1.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.2 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B-Gavekort AT ROW 9.57 COL 73.6
     B-Tilgode AT ROW 8.57 COL 73.6
     FI-Forklaring AT ROW 16.48 COL 1.4 NO-LABEL
     B-Butikk AT ROW 2.14 COL 30.6
     B-Dato AT ROW 3.14 COL 30.6
     B-Forsalj AT ROW 4.14 COL 30.6
     FILL-IN-3 AT ROW 1 COL 2 NO-LABEL
     B-Bilag AT ROW 7.57 COL 73.6
     B-Kontant AT ROW 3.57 COL 73.6
     B-Valuta AT ROW 5.57 COL 73.6
     RowObject.ButikkNr AT ROW 2.14 COL 15.2 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1
     RowObject.Dato AT ROW 3.14 COL 15.2 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1
     RowObject.KassererNr AT ROW 4.14 COL 15.2 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1
     RowObject.z_nummer AT ROW 5.14 COL 15.2 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1
     RowObject.OpptaltInnVeksel AT ROW 2.14 COL 53.8 COLON-ALIGNED
          LABEL "Inngående veksel"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
          BGCOLOR 18 
     RowObject.OpptaltKontanter AT ROW 3.57 COL 53.8 COLON-ALIGNED
          LABEL "Kontant"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
          BGCOLOR 18 FONT 6
     RowObject.OpptaltSjekk AT ROW 4.57 COL 53.8 COLON-ALIGNED
          LABEL "Sjekk"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
          BGCOLOR 18 
     RowObject.OpptaltValuta AT ROW 5.57 COL 53.8 COLON-ALIGNED
          LABEL "Valuta"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
          BGCOLOR 18 FONT 6
     RowObject.OpptaltReserve AT ROW 6.57 COL 53.8 COLON-ALIGNED
          LABEL "Reserve"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
          BGCOLOR 18 
     RowObject.OpptaltBilag AT ROW 7.57 COL 53.8 COLON-ALIGNED
          LABEL "Bilag"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
          FONT 6
     RowObject.OpptaltVeksel AT ROW 12.76 COL 54 COLON-ALIGNED
          LABEL "Utgående veksel"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     FI-Tilgode AT ROW 8.57 COL 53.8 COLON-ALIGNED
     RowObject.OpptaltTilgode AT ROW 2.14 COL 107 COLON-ALIGNED
          LABEL "Tilgode, egne"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.OpptaltTilgodeAndre AT ROW 3.14 COL 107 COLON-ALIGNED
          LABEL "Tilgode, andre"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.OpptaltTilgodeUtlevert AT ROW 4.14 COL 107 COLON-ALIGNED
          LABEL "Tilgode, utlevert"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     FI-Gavekort AT ROW 9.57 COL 53.8 COLON-ALIGNED
     RowObject.OpptaltGavekort AT ROW 7.67 COL 107 COLON-ALIGNED
          LABEL "Gavekort, egne"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.OpptaltGavekortAndre AT ROW 8.67 COL 107 COLON-ALIGNED
          LABEL "Gavekort, andre"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RowObject.OpptaltGavekortUtlevert AT ROW 9.67 COL 107 COLON-ALIGNED
          LABEL "Gavekort utlevert" FORMAT "->>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     FI-Total AT ROW 11.52 COL 53.8 COLON-ALIGNED
     RowObject.OpptaltLevertBank AT ROW 14 COL 54 COLON-ALIGNED
          LABEL "Levert bank/pose"
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.PoseNr AT ROW 15.05 COL 53.8 COLON-ALIGNED FORMAT "X(100)"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.fuEndretInfo AT ROW 18.38 COL 1 NO-LABEL FORMAT "x(80)"
           VIEW-AS TEXT 
          SIZE 91 BY .62
     FILL-IN-4 AT ROW 1 COL 39.8 NO-LABEL
     FILL-IN-5 AT ROW 1 COL 90.6 NO-LABEL
     FILL-IN-6 AT ROW 6.71 COL 90.6 NO-LABEL
     RowObject.OpptaltKupong AT ROW 10.52 COL 54 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     RECT-57 AT ROW 1.67 COL 1.2
     RECT-59 AT ROW 1.67 COL 35.6
     RECT-60 AT ROW 13.86 COL 35.8
     RECT-61 AT ROW 12.62 COL 35.8
     RECT-63 AT ROW 3.33 COL 35.8
     RECT-64 AT ROW 1.67 COL 90
     RECT-65 AT ROW 7.43 COL 89.8
     RECT-66 AT ROW 10.95 COL 89.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dkassereroppgj.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dkassereroppgj.i}
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
         HEIGHT             = 18.14
         WIDTH              = 127.8.
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

/* SETTINGS FOR BUTTON B-Butikk IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Dato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Forsalj IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.ButikkNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Dato IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Forklaring IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Gavekort IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tilgode IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Total IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN RowObject.fuEndretInfo IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-FORMAT                                         */
ASSIGN 
       RowObject.fuEndretInfo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.KassererNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.OpptaltBilag IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltGavekort IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltGavekortAndre IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltGavekortUtlevert IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN RowObject.OpptaltInnVeksel IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.OpptaltKontanter IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.OpptaltLevertBank IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.OpptaltReserve IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.OpptaltSjekk IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.OpptaltTilgode IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltTilgodeAndre IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltTilgodeUtlevert IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltValuta IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.OpptaltVeksel IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.PoseNr IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.z_nummer IN FRAME F-Main
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

&Scoped-define SELF-NAME B-Bilag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bilag vTableWin
ON CHOOSE OF B-Bilag IN FRAME F-Main /* Bilag... */
OR "F10":U OF RowObject.OpptaltKontanter
DO:

  /* Kaller søkerutine */
  RUN gkassererbilag.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "ButikkNr,Dato,KassererNr,Z_Nummer", /* Feltliste avgrensningsfelt (kommaseparert) */
    (RowObject.ButikkNr:SCREEN-VALUE + chr(1) +
     RowObject.Dato:SCREEN-VALUE + CHR(1) + 
     RowObject.KassererNr:SCREEN-VALUE + CHR(1) + 
     RowObject.Z_Nummer:SCREEN-VALUE), /* Feltverdier (chr(1) sep) */ 
     RowObject.Z_Nummer:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
/*   IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN */
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        lSum  = dec(cTekst) 
        .
      IF DEC(RowObject.OpptaltBilag:SCREEN-VALUE) <> lSum THEN
      DO:
          ASSIGN
              RowObject.OpptaltBilag:SCREEN-VALUE = string(lSum) 
              RowObject.OpptaltBilag:MODIFIED = TRUE
              .
          RUN SumTotal(1).
          DYNAMIC-FUNCTION('setDataModified':U, INPUT TRUE).
      END.

        RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikk vTableWin
ON CHOOSE OF B-Butikk IN FRAME F-Main /* ... */
OR "F10":U OF RowObject.ButikkNr
DO:

   DO WITH FRAME {&FRAME-NAME}:
     IF RowObject.ButikkNr:SCREEN-VALUE = "[Alle]"
       THEN cTekst = "".
     ELSE cTekst = RowObject.ButikkNr:SCREEN-VALUE. 
   END.

  /* Kaller søkerutine */
  RUN gButiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    RowObject.ButikkNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowObject.ButikkNr:SCREEN-VALUE   = ENTRY(2,cTekst,CHR(1))
        .

        RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Dato vTableWin
ON CHOOSE OF B-Dato IN FRAME F-Main /* ... */
or F10 of RowObject.Dato
DO:

  def var wTittel as char no-undo.
/*   assign RowObject.Dato = date(RowObject.Dato:screen-value in frame {&FRAME-NAME}). */

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = RowObject.Dato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Forsalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Forsalj vTableWin
ON CHOOSE OF B-Forsalj IN FRAME F-Main /* ... */
OR "F10":U OF RowObject.KassererNr
DO:

   DO WITH FRAME {&FRAME-NAME}:
     IF RowObject.KassererNr:SCREEN-VALUE = "[Alle]"
       THEN cTekst = "".
     ELSE cTekst = RowObject.KassererNr:SCREEN-VALUE. 
   END.

  /* Kaller søkerutine */
  RUN gforsalj.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    "", /* Operator */
    RowObject.KassererNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        RowObject.KassererNr:SCREEN-VALUE   = ENTRY(2,cTekst,CHR(1))
        .

        RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Gavekort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Gavekort vTableWin
ON CHOOSE OF B-Gavekort IN FRAME F-Main /* Gavekort */
DO:
    ASSIGN
      RowObject.OpptaltGavekort:SENSITIVE         = IF RowObject.OpptaltGavekort:SENSITIVE = TRUE THEN FALSE ELSE TRUE
      RowObject.OpptaltGavekortAndre:SENSITIVE    = IF RowObject.OpptaltGavekortAndre:SENSITIVE = TRUE THEN FALSE ELSE TRUE
      RowObject.OpptaltGavekortUtlevert:SENSITIVE = IF RowObject.OpptaltGavekortUtlevert:SENSITIVE = TRUE THEN FALSE ELSE TRUE
      .

    RUN SumGavekort.

    ASSIGN
        FI-Gavekort:MODIFIED = FALSE
        .
    RUN SumTotal(1).
    DYNAMIC-FUNCTION('setDataModified':U, INPUT TRUE).

    APPLY "entry" TO RowObject.OpptaltGavekort.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kontant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kontant vTableWin
ON CHOOSE OF B-Kontant IN FRAME F-Main /* Kontant... */
OR "F10":U OF RowObject.OpptaltKontanter
DO:

  /* Kaller søkerutine */
  RUN gkassererkontanter.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "ButikkNr,Dato,KassererNr,Z_Nummer", /* Feltliste avgrensningsfelt (kommaseparert) */
    (RowObject.ButikkNr:SCREEN-VALUE + chr(1) +
     RowObject.Dato:SCREEN-VALUE + CHR(1) + 
     RowObject.KassererNr:SCREEN-VALUE + CHR(1) + 
     RowObject.Z_Nummer:SCREEN-VALUE), /* Feltverdier (chr(1) sep) */ 
     RowObject.Z_Nummer:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        lSum  = dec(ENTRY( 2,cTekst,CHR(1))) + 
                dec(ENTRY( 3,cTekst,CHR(1))) + 
                dec(ENTRY( 4,cTekst,CHR(1))) + 
                dec(ENTRY( 5,cTekst,CHR(1))) + 
                dec(ENTRY( 6,cTekst,CHR(1))) + 
                dec(ENTRY( 7,cTekst,CHR(1))) + 
                dec(ENTRY( 8,cTekst,CHR(1))) + 
                dec(ENTRY( 9,cTekst,CHR(1))) + 
                dec(ENTRY(10,cTekst,CHR(1))) + 
                dec(ENTRY(11,cTekst,CHR(1))) 
        .
      IF DEC(RowObject.OpptaltKontanter:SCREEN-VALUE) <> lSum THEN
      DO:
          ASSIGN
              RowObject.OpptaltKontanter:SCREEN-VALUE = string(lSum) 
              RowObject.OpptaltKontanter:MODIFIED     = TRUE
              .
          RUN SumTotal(1).
          DYNAMIC-FUNCTION('setDataModified':U, INPUT TRUE).

      END.

        RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tilgode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tilgode vTableWin
ON CHOOSE OF B-Tilgode IN FRAME F-Main /* Tilgode */
DO:
  ASSIGN
      RowObject.OpptaltTilgode:SENSITIVE         = IF RowObject.OpptaltTilgode:SENSITIVE = TRUE THEN FALSE ELSE TRUE
      RowObject.OpptaltTilgodeAndre:SENSITIVE    = IF RowObject.OpptaltTilgodeAndre:SENSITIVE = TRUE THEN FALSE ELSE TRUE
      RowObject.OpptaltTilgodeUtlevert:SENSITIVE = IF RowObject.OpptaltTilgodeUtlevert:SENSITIVE = TRUE THEN FALSE ELSE TRUE
      .

  RUN SumTilgode.

  ASSIGN
      FI-Tilgode:MODIFIED = FALSE 
      .
  RUN SumTotal(1).
  DYNAMIC-FUNCTION('setDataModified':U, INPUT TRUE).

  APPLY "entry" TO RowObject.OpptaltTilgode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Valuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Valuta vTableWin
ON CHOOSE OF B-Valuta IN FRAME F-Main /* Valuta... */
OR "F10":U OF RowObject.OpptaltValuta
DO:

  /* Kaller søkerutine */
  RUN gkasserervaluta.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "ButikkNr,Dato,KassererNr,Z_Nummer", /* Feltliste avgrensningsfelt (kommaseparert) */
    (RowObject.ButikkNr:SCREEN-VALUE + chr(1) +
     RowObject.Dato:SCREEN-VALUE + CHR(1) + 
     RowObject.KassererNr:SCREEN-VALUE + CHR(1) + 
     RowObject.Z_Nummer:SCREEN-VALUE), /* Feltverdier (chr(1) sep) */ 
     RowObject.Z_Nummer:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
/*   IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN */
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        lSum  = dec(cTekst) 
        .
      IF DEC(RowObject.OpptaltValuta:SCREEN-VALUE) <> lSum THEN
      DO:
          ASSIGN
              RowObject.OpptaltValuta:SCREEN-VALUE = string(lSum) 
              RowObject.OpptaltValuta:MODIFIED     = TRUE
              .
          RUN SumTotal(1).
          DYNAMIC-FUNCTION('setDataModified':U, INPUT TRUE).
      END.

        RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.ButikkNr vTableWin
ON TAB OF RowObject.ButikkNr IN FRAME F-Main /* Butikknummer */
OR "RETURN" OF RowObject.ButikkNr
DO:
  RUN GetInngVeksel IN DYNAMIC-FUNCTION('getDataSource':U) 
      (INPUT int(SELF:SCREEN-VALUE), OUTPUT lInngVeksel).  
  IF lInngVeksel > 0 THEN
  DO:
      ASSIGN
          RowObject.OpptaltInnVeksel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(lInngVeksel)
          .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Gavekort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Gavekort vTableWin
ON VALUE-CHANGED OF FI-Gavekort IN FRAME F-Main /* Gavekort */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Tilgode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Tilgode vTableWin
ON VALUE-CHANGED OF FI-Tilgode IN FRAME F-Main /* Tilgode */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltBilag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltBilag vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltBilag IN FRAME F-Main /* Bilag */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltGavekort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltGavekort vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltGavekort IN FRAME F-Main /* Gavekort, egne */
DO:
  RUN SumGavekort.
  RUN SumTotal(1).
  ASSIGN
      SELF:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltGavekortAndre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltGavekortAndre vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltGavekortAndre IN FRAME F-Main /* Gavekort, andre */
DO:
  RUN SumGavekort.
  RUN SumTotal(1).
  ASSIGN
      SELF:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltGavekortUtlevert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltGavekortUtlevert vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltGavekortUtlevert IN FRAME F-Main /* Gavekort utlevert */
DO:
  RUN SumGavekort.
  RUN SumTotal(1).
  ASSIGN
      SELF:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltKontanter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltKontanter vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltKontanter IN FRAME F-Main /* Kontant */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltKupong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltKupong vTableWin
ON LEAVE OF RowObject.OpptaltKupong IN FRAME F-Main /* Opptalt kupong */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltReserve
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltReserve vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltReserve IN FRAME F-Main /* Reserve */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltSjekk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltSjekk vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltSjekk IN FRAME F-Main /* Sjekk */
DO:
   RUN SumTotal (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltTilgode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltTilgode vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltTilgode IN FRAME F-Main /* Tilgode, egne */
DO:
  RUN SumTilgode.
  RUN SumTotal(1).
  ASSIGN
      SELF:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltTilgodeAndre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltTilgodeAndre vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltTilgodeAndre IN FRAME F-Main /* Tilgode, andre */
DO:
  RUN SumTilgode.
  RUN SumTotal(1).
  ASSIGN
      SELF:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltTilgodeUtlevert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltTilgodeUtlevert vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltTilgodeUtlevert IN FRAME F-Main /* Tilgode, utlevert */
DO:
  RUN SumTilgode.
  RUN SumTotal(1).
  ASSIGN
      SELF:MODIFIED = TRUE
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.OpptaltValuta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.OpptaltValuta vTableWin
ON VALUE-CHANGED OF RowObject.OpptaltValuta IN FRAME F-Main /* Valuta */
DO:
   RUN SumTotal (1).
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
    RUN disableFields ( INPUT "All").
    ASSIGN 
        RowObject.ButikkNr:SENSITIVE   = TRUE
        RowObject.Dato:SENSITIVE       = TRUE
        RowObject.KassererNr:SENSITIVE = TRUE
        RowObject.Z_Nummer:SENSITIVE   = FALSE 
        RowObject.OpptaltTilgode:SENSITIVE          = FALSE
        RowObject.OpptaltTilgodeAndre:SENSITIVE     = FALSE
        RowObject.OpptaltTilgodeUtlevert:SENSITIVE  = FALSE
        RowObject.OpptaltGavekort:SENSITIVE         = FALSE
        RowObject.OpptaltGavekortAndre:SENSITIVE    = FALSE
        RowObject.OpptaltGavekortUtlevert:SENSITIVE = FALSE
        RowObject.Z_Nummer:SCREEN-VALUE = "1"
        B-Kontant:SENSITIVE  = FALSE
        B-Valuta:SENSITIVE   = FALSE
        B-Bilag:SENSITIVE    = FALSE
        B-Tilgode:SENSITIVE  = FALSE
        B-Gavekort:SENSITIVE = FALSE
        B-Butikk:SENSITIVE   = TRUE
        B-Dato:SENSITIVE     = TRUE
        B-Forsalj:SENSITIVE  = TRUE
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
    RUN enableFields.
    ASSIGN 
        RowObject.ButikkNr:SENSITIVE   = FALSE
        RowObject.Dato:SENSITIVE       = FALSE
        RowObject.KassererNr:SENSITIVE = FALSE
        RowObject.Z_Nummer:SENSITIVE   = FALSE
        B-Kontant:SENSITIVE  = TRUE
        B-Valuta:SENSITIVE   = TRUE
        B-Bilag:SENSITIVE    = TRUE
        B-Tilgode:SENSITIVE  = TRUE
        B-Gavekort:SENSITIVE = TRUE
        B-Butikk:SENSITIVE   = FALSE
        B-Dato:SENSITIVE     = FALSE
        B-Forsalj:SENSITIVE  = FALSE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

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
        RowObject.ButikkNr:SENSITIVE   = TRUE
        RowObject.Dato:SENSITIVE       = TRUE
        RowObject.KassererNr:SENSITIVE = TRUE
        RowObject.Z_Nummer:SENSITIVE   = TRUE
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
  RUN SumGavekort.
  RUN SumTilgode.
  RUN SumTotal (99).

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
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
/*           fuValorer:SENSITIVE = FALSE */
/*           fuBilag:SENSITIVE   = FALSE */
/*           fuValuta:SENSITIVE  = FALSE */
          .
  END.

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
  IF RowObject.ButikkNr:SENSITIVE = TRUE THEN
    RUN ApplyEntry ("ButikkNr").
  ELSE
    RUN ApplyEntry ("OpptaltInnVeksel").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumGavekort vTableWin 
PROCEDURE SumGavekort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      IF DEC(FI-Gavekort:SCREEN-VALUE) <> (INPUT RowObject.OpptaltGavekort +
                                      INPUT RowObject.OpptaltGavekortAndre -
                                      INPUT RowObject.OpptaltGavekortUtlevert) THEN
      DO:
          ASSIGN
              FI-Gavekort:SCREEN-VALUE = STRING(INPUT RowObject.OpptaltGavekort +
                                          INPUT RowObject.OpptaltGavekortAndre -
                                          INPUT RowObject.OpptaltGavekortUtlevert)
              .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumTilgode vTableWin 
PROCEDURE SumTilgode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DO WITH FRAME {&FRAME-NAME}:
      IF DEC(FI-Tilgode:SCREEN-VALUE) <> (INPUT RowObject.OpptaltTilgode +
                                      INPUT RowObject.OpptaltTilgodeAndre -
                                      INPUT RowObject.OpptaltTilgodeUtlevert) THEN
      DO:
          ASSIGN
              FI-Tilgode:SCREEN-VALUE = STRING(INPUT RowObject.OpptaltTilgode +
                                          INPUT RowObject.OpptaltTilgodeAndre -
                                          INPUT RowObject.OpptaltTilgodeUtlevert)
              .
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumTotal vTableWin 
PROCEDURE SumTotal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piInt AS INT NO-UNDO.

  DEF VAR plTotal AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          plTotal = /*INPUT RowObject.OpptaltInnVeksel +*/
                    INPUT RowObject.OpptaltKontanter +
                    INPUT RowObject.OpptaltSjekk +
                    INPUT RowObject.OpptaltValuta +
                    INPUT RowObject.OpptaltReserve +
                    INPUT RowObject.OpptaltBilag + 
                    INPUT RowObject.OpptaltKupong + 
                    /*INPUT RowObject.OpptaltVeksel +*/
                    INPUT FI-Gavekort + 
                    INPUT FI-Tilgode
          .
      IF DEC(FI-Total:SCREEN-VALUE) <> plTotal THEN
      DO:
          CASE piInt:
              WHEN 1 THEN
              DO:
                  ASSIGN
                  FI-Total:SCREEN-VALUE = STRING(plTotal)
                  FI-Total:MODIFIED     = FALSE 
                  .
                  DYNAMIC-FUNCTION('setDataModified':U, INPUT TRUE).
              END.
              WHEN 99 THEN
              DO:
                  ASSIGN
                  FI-Total:SCREEN-VALUE = STRING(plTotal)
                  FI-Total:MODIFIED     = FALSE
                  .
              END.
          END CASE.
      END.
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
      RUN enableFields.
      ASSIGN 
          RowObject.ButikkNr:SENSITIVE   = FALSE
          RowObject.Dato:SENSITIVE       = FALSE
          RowObject.KassererNr:SENSITIVE = FALSE
          RowObject.Z_Nummer:SENSITIVE   = FALSE
          RowObject.OpptaltTilgode:SENSITIVE          = FALSE
          RowObject.OpptaltTilgodeAndre:SENSITIVE     = FALSE
          RowObject.OpptaltTilgodeUtlevert:SENSITIVE  = FALSE
          RowObject.OpptaltGavekort:SENSITIVE         = FALSE
          RowObject.OpptaltGavekortAndre:SENSITIVE    = FALSE
          RowObject.OpptaltGavekortUtlevert:SENSITIVE = FALSE
          B-Kontant:SENSITIVE  = TRUE
          B-Valuta:SENSITIVE   = TRUE
          B-Bilag:SENSITIVE    = TRUE
          B-Tilgode:SENSITIVE  = TRUE
          B-Gavekort:SENSITIVE = TRUE
        .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

