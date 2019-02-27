&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

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

DEFINE VARIABLE cAlle AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Utvidetsok CB-IndType CB-Vareslag ~
B-AnbefaltPris B-LevNrBlank B-SokLevNr B-VgBlank BUTTON-SokVg FI-Beskr ~
B-BeskrBlank FI-Bongtekst FI-LevKod B-Bongtekst FI-FraOpprettet ~
FI-TilOpprettet FI-FraEndret FI-TilEndret B-EndretBlank FI-FraArtikkelNr ~
FI-TilartikkelNr FI-AnbefaltPris1 B-LevKodBlank FI-AnbefaltPris2 CB-Sasong ~
CB-Farg CB-Material B-ArtikkelNrBlank CB-Avdeling CB-HuvGr FI-LevNr ~
CB-Pakke CB-Kunderabatt CB-Aktiv CB-Annonse CB-HovedModellFarge ~
CB-Storrelser CB-StrTypeId CB-OPris CB-Bildeikasse CB-HKstyrt CB-LokPris ~
CB-IKasse B-OpprettetBlank B-AnbefaltPris-2 RECT-1 
&Scoped-Define DISPLAYED-OBJECTS FI-Utvidetsok CB-IndType CB-Vareslag ~
FI-Beskr FI-Bongtekst FI-LevKod FI-FraOpprettet FI-TilOpprettet ~
FI-FraEndret FI-TilEndret FI-FraArtikkelNr FI-TilartikkelNr ~
FI-AnbefaltPris1 FI-AnbefaltPris2 CB-Sasong CB-Farg CB-Material CB-Avdeling ~
CB-HuvGr FI-LevNr FI-Vg CB-Pakke CB-Kunderabatt CB-Aktiv CB-Annonse ~
CB-HovedModellFarge CB-Storrelser CB-StrTypeId CB-OPris CB-Bildeikasse ~
CB-HKstyrt CB-LokPris CB-IKasse FI-Fltertekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AnbefaltPris  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-AnbefaltPris-2  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-ArtikkelNrBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 6.8 BY 1.

DEFINE BUTTON B-BeskrBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 6.8 BY 1.

DEFINE BUTTON B-Bongtekst  NO-FOCUS
     LABEL "Blank" 
     SIZE 6.8 BY 1.

DEFINE BUTTON B-EndretBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 6.8 BY 1.

DEFINE BUTTON B-LevKodBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 6.8 BY 1.

DEFINE BUTTON B-LevNrBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-OpprettetBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 6.8 BY 1.

DEFINE BUTTON B-SokLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-VgBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON BUTTON-SokVg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Aktiv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aktiv" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Annonse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Annonse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Avdeling AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdeling" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Bildeikasse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bilde i kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Farg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE CB-HKstyrt AS CHARACTER FORMAT "X(256)":U 
     LABEL "HK styrt" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-HovedModellFarge AS CHARACTER FORMAT "X(256)":U 
     LABEL "Modell" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-HuvGr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hovedgr" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE CB-IKasse AS CHARACTER FORMAT "X(256)":U 
     LABEL "I kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-IndType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ind.type" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kunderabatt AS CHARACTER FORMAT "X(256)" INITIAL "no" 
     LABEL "Knd.rab" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1"," Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-LokPris AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lokal pris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Material AS CHARACTER FORMAT "X(256)":U 
     LABEL "Material" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE CB-OPris AS CHARACTER FORMAT "X(256)":U 
     LABEL "Åpen pris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Pakke AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pakke" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sasong AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ses&ong" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Storrelser AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelser" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-StrTypeId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Str.type" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Vareslag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vareslag" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 33.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPris1 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Anbef.pris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fra og med anbefalt pris" NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPris2 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Til og med anbefalt pris" NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bongtekst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bongtekst" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Fltertekst AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FraArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     LABEL "ArtikkelNr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraEndret AS DATE FORMAT "99/99/99":U 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraOpprettet AS DATE FORMAT "99/99/99":U 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Levartnr" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNr AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "Levnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilartikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilEndret AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilOpprettet AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Utvidetsok AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utv.søk" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE FI-Vg AS CHARACTER FORMAT "X(8)" 
     LABEL "Vgr." 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-Utvidetsok AT ROW 8.71 COL 10.2 COLON-ALIGNED
     CB-IndType AT ROW 9.86 COL 89.6 COLON-ALIGNED
     CB-Vareslag AT ROW 8.86 COL 89.6 COLON-ALIGNED
     B-AnbefaltPris AT ROW 7.71 COL 40 NO-TAB-STOP 
     B-LevNrBlank AT ROW 6.76 COL 74.4 NO-TAB-STOP 
     B-SokLevNr AT ROW 6.76 COL 70.2 NO-TAB-STOP 
     B-VgBlank AT ROW 7.71 COL 68.2 NO-TAB-STOP 
     BUTTON-SokVg AT ROW 7.71 COL 63.8
     FI-Beskr AT ROW 1.71 COL 10.2 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     B-BeskrBlank AT ROW 1.76 COL 40.2 NO-TAB-STOP 
     FI-Bongtekst AT ROW 2.71 COL 10.2 COLON-ALIGNED
     FI-LevKod AT ROW 3.76 COL 10.2 COLON-ALIGNED
     B-Bongtekst AT ROW 2.71 COL 40.2 NO-TAB-STOP 
     FI-FraOpprettet AT ROW 4.81 COL 10.2 COLON-ALIGNED
     FI-TilOpprettet AT ROW 4.81 COL 24 COLON-ALIGNED NO-LABEL
     FI-FraEndret AT ROW 5.76 COL 10.2 COLON-ALIGNED
     FI-TilEndret AT ROW 5.76 COL 24 COLON-ALIGNED NO-LABEL
     B-EndretBlank AT ROW 5.76 COL 40.2 NO-TAB-STOP 
     FI-FraArtikkelNr AT ROW 6.71 COL 10.2 COLON-ALIGNED
     FI-TilartikkelNr AT ROW 6.71 COL 24 COLON-ALIGNED NO-LABEL
     FI-AnbefaltPris1 AT ROW 7.71 COL 10.2 COLON-ALIGNED
     B-LevKodBlank AT ROW 3.76 COL 40.2 NO-TAB-STOP 
     FI-AnbefaltPris2 AT ROW 7.71 COL 24 COLON-ALIGNED NO-LABEL
     CB-Sasong AT ROW 1.76 COL 54.6 COLON-ALIGNED
     CB-Farg AT ROW 2.71 COL 54.6 COLON-ALIGNED
     CB-Material AT ROW 3.71 COL 54.6 COLON-ALIGNED
     B-ArtikkelNrBlank AT ROW 6.71 COL 40.2 NO-TAB-STOP 
     CB-Avdeling AT ROW 4.76 COL 54.6 COLON-ALIGNED
     CB-HuvGr AT ROW 5.81 COL 54.6 COLON-ALIGNED
     FI-LevNr AT ROW 6.76 COL 54.4 COLON-ALIGNED
     FI-Vg AT ROW 7.71 COL 54.4 COLON-ALIGNED HELP
          "Varegruppe"
     CB-Pakke AT ROW 1.76 COL 89.6 COLON-ALIGNED
     CB-Kunderabatt AT ROW 2.76 COL 89.6 COLON-ALIGNED HELP
          "Åpner for at kunderabatt gis på artikkelen."
     CB-Aktiv AT ROW 3.86 COL 89.6 COLON-ALIGNED
     CB-Annonse AT ROW 4.86 COL 89.6 COLON-ALIGNED
     CB-HovedModellFarge AT ROW 5.86 COL 89.6 COLON-ALIGNED
     CB-Storrelser AT ROW 6.91 COL 89.6 COLON-ALIGNED
     CB-StrTypeId AT ROW 7.86 COL 89.6 COLON-ALIGNED
     CB-OPris AT ROW 1.67 COL 113.4 COLON-ALIGNED
     CB-Bildeikasse AT ROW 2.71 COL 113.4 COLON-ALIGNED
     CB-HKstyrt AT ROW 3.81 COL 113.4 COLON-ALIGNED
     CB-LokPris AT ROW 4.81 COL 113.4 COLON-ALIGNED
     CB-IKasse AT ROW 5.81 COL 113.4 COLON-ALIGNED
     B-OpprettetBlank AT ROW 4.81 COL 40.2 NO-TAB-STOP 
     FI-Fltertekst AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL
     B-AnbefaltPris-2 AT ROW 8.76 COL 40 NO-TAB-STOP 
     RECT-1 AT ROW 1.24 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.6 BY 10.43.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 10.43
         WIDTH              = 125.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Fltertekst IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Fltertekst:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FI-Vg IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Vg:READ-ONLY IN FRAME fMain        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-AnbefaltPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AnbefaltPris fFrameWin
ON CHOOSE OF B-AnbefaltPris IN FRAME fMain /* Blank */
DO:
  ASSIGN 
      FI-AnbefaltPris1:SCREEN-VALUE = ""
      FI-AnbefaltPris2:SCREEN-VALUE = ""
      .
  APPLY "TAB" TO FI-AnbefaltPris2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AnbefaltPris-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AnbefaltPris-2 fFrameWin
ON CHOOSE OF B-AnbefaltPris-2 IN FRAME fMain /* Blank */
DO:
  ASSIGN 
      FI-UtvidetSok:SCREEN-VALUE = ""
      .
  APPLY "TAB" TO FI-UtvidetSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ArtikkelNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ArtikkelNrBlank fFrameWin
ON CHOOSE OF B-ArtikkelNrBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN 
      FI-FraArtikkelNr:SCREEN-VALUE = ""
      FI-TilArtikkelNr:SCREEN-VALUE = ""
      .
  APPLY "TAB" TO FI-FraArtikkelNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BeskrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BeskrBlank fFrameWin
ON CHOOSE OF B-BeskrBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-Beskr:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Beskr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bongtekst fFrameWin
ON CHOOSE OF B-Bongtekst IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-BongTekst:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Bongtekst.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-EndretBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-EndretBlank fFrameWin
ON CHOOSE OF B-EndretBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN 
      FI-FraEndret:SCREEN-VALUE = ""
      FI-TilEndret:SCREEN-VALUE = ""
      .
  APPLY "TAB" TO FI-FraEndret.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevKodBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevKodBlank fFrameWin
ON CHOOSE OF B-LevKodBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-LevKod:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-LevKod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank fFrameWin
ON CHOOSE OF B-LevNrBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-LevNr:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-LevNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OpprettetBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OpprettetBlank fFrameWin
ON CHOOSE OF B-OpprettetBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN 
      FI-FraOpprettet:SCREEN-VALUE = ""
      FI-TilOpprettet:SCREEN-VALUE = ""
      .
  APPLY "TAB" TO FI-FraOpprettet.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr fFrameWin
ON CHOOSE OF B-SokLevNr IN FRAME fMain /* ... */
OR F10 OF FI-LevNr
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gLevbas.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    FI-LevNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) = 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        FI-LevNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "TAB":U TO FI-LevNr.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VgBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VgBlank fFrameWin
ON CHOOSE OF B-VgBlank IN FRAME fMain /* Blank */
DO:
    IF FI-Vg:SCREEN-VALUE <> cAlle THEN DO:
        ASSIGN FI-Vg:SCREEN-VALUE = cAlle
               FI-Vg:TOOLTIP      = "".
        RUN StartSok.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg fFrameWin
ON CHOOSE OF BUTTON-SokVg IN FRAME fMain /* ... */
or F10 of FI-Vg
DO:
    DEFINE VARIABLE iVg AS INTEGER    NO-UNDO.

    RUN gvargr.w (INPUT-OUTPUT iVg).
    IF RETURN-VALUE <> "AVBRYT" THEN
    DO:
        ASSIGN
            CB-Avdeling:SCREEN-VALUE = " "
            CB-HuvGr:SCREEN-VALUE = " "
            FI-Vg:SCREEN-VALUE = STRING(iVg)
            FI-Vg:TOOLTIP      = RETURN-VALUE
            .
        RUN StartSok.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aktiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aktiv fFrameWin
ON VALUE-CHANGED OF CB-Aktiv IN FRAME fMain /* Aktiv */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Annonse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Annonse fFrameWin
ON VALUE-CHANGED OF CB-Annonse IN FRAME fMain /* Annonse */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Avdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Avdeling fFrameWin
ON VALUE-CHANGED OF CB-Avdeling IN FRAME fMain /* Avdeling */
DO:
    ASSIGN
    CB-HuvGr:SCREEN-VALUE = " "
    .
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Bildeikasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Bildeikasse fFrameWin
ON VALUE-CHANGED OF CB-Bildeikasse IN FRAME fMain /* Bilde i kasse */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Farg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Farg fFrameWin
ON VALUE-CHANGED OF CB-Farg IN FRAME fMain /* Farge */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-HKstyrt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HKstyrt fFrameWin
ON VALUE-CHANGED OF CB-HKstyrt IN FRAME fMain /* HK styrt */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-HovedModellFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HovedModellFarge fFrameWin
ON VALUE-CHANGED OF CB-HovedModellFarge IN FRAME fMain /* Modell */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HuvGr fFrameWin
ON VALUE-CHANGED OF CB-HuvGr IN FRAME fMain /* Hovedgr */
DO:
    ASSIGN
        CB-Avdeling:SCREEN-VALUE = " "
        .
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-IKasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-IKasse fFrameWin
ON VALUE-CHANGED OF CB-IKasse IN FRAME fMain /* I kasse */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-IndType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-IndType fFrameWin
ON VALUE-CHANGED OF CB-IndType IN FRAME fMain /* Ind.type */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Kunderabatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Kunderabatt fFrameWin
ON VALUE-CHANGED OF CB-Kunderabatt IN FRAME fMain /* Knd.rab */
DO:
   RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-LokPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-LokPris fFrameWin
ON VALUE-CHANGED OF CB-LokPris IN FRAME fMain /* Lokal pris */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Material
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Material fFrameWin
ON VALUE-CHANGED OF CB-Material IN FRAME fMain /* Material */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-OPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-OPris fFrameWin
ON VALUE-CHANGED OF CB-OPris IN FRAME fMain /* Åpen pris */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Pakke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Pakke fFrameWin
ON VALUE-CHANGED OF CB-Pakke IN FRAME fMain /* Pakke */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Sasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Sasong fFrameWin
ON VALUE-CHANGED OF CB-Sasong IN FRAME fMain /* Sesong */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Storrelser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Storrelser fFrameWin
ON VALUE-CHANGED OF CB-Storrelser IN FRAME fMain /* Størrelser */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-StrTypeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-StrTypeId fFrameWin
ON VALUE-CHANGED OF CB-StrTypeId IN FRAME fMain /* Str.type */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Vareslag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Vareslag fFrameWin
ON VALUE-CHANGED OF CB-Vareslag IN FRAME fMain /* Vareslag */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AnbefaltPris2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AnbefaltPris2 fFrameWin
ON LEAVE OF FI-AnbefaltPris2 IN FRAME fMain
OR RETURN OF FI-AnbefaltPris2
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Beskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Beskr fFrameWin
ON RETURN OF FI-Beskr IN FRAME fMain /* Varetekst */
OR "TAB":U OF FI-Beskr DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Bongtekst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Bongtekst fFrameWin
ON RETURN OF FI-Bongtekst IN FRAME fMain /* Bongtekst */
OR "TAB":U OF FI-BongTekst DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraArtikkelNr fFrameWin
ON DELETE-CHARACTER OF FI-FraArtikkelNr IN FRAME fMain /* ArtikkelNr */
DO:
    ASSIGN
      SELF:SCREEN-VALUE = ?
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraEndret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraEndret fFrameWin
ON DELETE-CHARACTER OF FI-FraEndret IN FRAME fMain /* Endret */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ?
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraOpprettet fFrameWin
ON DELETE-CHARACTER OF FI-FraOpprettet IN FRAME fMain /* Opprettet */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ?
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevKod fFrameWin
ON RETURN OF FI-LevKod IN FRAME fMain /* Levartnr */
OR "TAB":U OF FI-LevKod DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNr fFrameWin
ON RETURN OF FI-LevNr IN FRAME fMain /* Levnr */
OR "TAB":U OF FI-LevNr DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilartikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilartikkelNr fFrameWin
ON TAB OF FI-TilartikkelNr IN FRAME fMain
OR RETURN OF FI-TilArtikkelNr
DO:
   RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilEndret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilEndret fFrameWin
ON DELETE-CHARACTER OF FI-TilEndret IN FRAME fMain
DO:
    ASSIGN
      SELF:SCREEN-VALUE = ?
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilEndret fFrameWin
ON TAB OF FI-TilEndret IN FRAME fMain
OR RETURN OF FI-TilEndret
DO:
   RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilOpprettet fFrameWin
ON DELETE-CHARACTER OF FI-TilOpprettet IN FRAME fMain
DO:
    ASSIGN
      SELF:SCREEN-VALUE = ?
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilOpprettet fFrameWin
ON TAB OF FI-TilOpprettet IN FRAME fMain
OR RETURN OF FI-TilOpprettet
DO:
   RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Utvidetsok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Utvidetsok fFrameWin
ON TAB OF FI-Utvidetsok IN FRAME fMain /* Utv.søk */
OR "TAB":U OF FI-UtvidetSok DO:
    RUN StartSok.
    APPLY 'ENTRY' TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
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
  DISPLAY FI-Utvidetsok CB-IndType CB-Vareslag FI-Beskr FI-Bongtekst FI-LevKod 
          FI-FraOpprettet FI-TilOpprettet FI-FraEndret FI-TilEndret 
          FI-FraArtikkelNr FI-TilartikkelNr FI-AnbefaltPris1 FI-AnbefaltPris2 
          CB-Sasong CB-Farg CB-Material CB-Avdeling CB-HuvGr FI-LevNr FI-Vg 
          CB-Pakke CB-Kunderabatt CB-Aktiv CB-Annonse CB-HovedModellFarge 
          CB-Storrelser CB-StrTypeId CB-OPris CB-Bildeikasse CB-HKstyrt 
          CB-LokPris CB-IKasse FI-Fltertekst 
      WITH FRAME fMain.
  ENABLE FI-Utvidetsok CB-IndType CB-Vareslag B-AnbefaltPris B-LevNrBlank 
         B-SokLevNr B-VgBlank BUTTON-SokVg FI-Beskr B-BeskrBlank FI-Bongtekst 
         FI-LevKod B-Bongtekst FI-FraOpprettet FI-TilOpprettet FI-FraEndret 
         FI-TilEndret B-EndretBlank FI-FraArtikkelNr FI-TilartikkelNr 
         FI-AnbefaltPris1 B-LevKodBlank FI-AnbefaltPris2 CB-Sasong CB-Farg 
         CB-Material B-ArtikkelNrBlank CB-Avdeling CB-HuvGr FI-LevNr CB-Pakke 
         CB-Kunderabatt CB-Aktiv CB-Annonse CB-HovedModellFarge CB-Storrelser 
         CB-StrTypeId CB-OPris CB-Bildeikasse CB-HKstyrt CB-LokPris CB-IKasse 
         B-OpprettetBlank B-AnbefaltPris-2 RECT-1 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCombos fFrameWin 
PROCEDURE initCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-Vg:SCREEN-VALUE = cAlle.
      
      ASSIGN CB-Avdeling:LIST-ITEM-PAIRS = cAlle + ","
             CB-Avdeling:screen-value = " ".
      for each Avdeling FIELDS(AvdelingNr AvdelingNavn) no-lock:
        CB-Avdeling:add-last(string(Avdeling.AvdelingNr,"zzz9") + "   " + 
                         REPLACE(Avdeling.AvdelingNavn,","," "),string(Avdeling.AvdelingNr)).
      end.
      ASSIGN CB-HuvGr:LIST-ITEM-PAIRS = cAlle + ","
             CB-HuvGr:screen-value = " ".
      for each HuvGr FIELDS(Hg HgBeskr) no-lock:
        CB-HuvGr:add-last(string(HuvGr.Hg,"zzz9") + "   " + 
                         REPLACE(HuvGr.HgBeskr,","," "),string(HuvGr.Hg)).
      end.
      ASSIGN CB-VareSlag:LIST-ITEM-PAIRS = cAlle + ","
             CB-VareSlag:screen-value = " ".
      for each SysPara NO-LOCK WHERE
          SysPara.SysHId    = 2 AND
          SysPAra.SysGr = 7:
          CB-VareSlag:add-last(string(SysPara.Parameter1,"99") + "   " + 
                                              REPLACE(SysPara.Beskrivelse,","," "),STRING(SysPara.Parameter1)).
      end.
      ASSIGN CB-StrTypeId:LIST-ITEM-PAIRS = cAlle + ","
             CB-StrTypeId:screen-value = " ".
      for each StrType FIELDS(StrTypeId Beskrivelse) no-lock:
          CB-StrTypeId:add-last(string(StrType.StrTypeId,"999999") + "   " + 
                                              REPLACE(StrType.Beskrivelse,","," "),STRING(StrType.StrTypeId)).
      end.
/*       CB-Sasong:DELETE(1). */
      ASSIGN CB-Sasong:LIST-ITEM-PAIRS = cAlle + ","
             CB-Sasong:screen-value = " ".
      for each Sasong FIELDS(SaSong SasBeskr) no-lock:
        CB-Sasong:add-last(string(SaSong.SaSong,"zzz9") + "   " + 
                                                 REPLACE(Sasong.SasBeskr,","," "),string(SaSong.SaSong)).
      end.
      ASSIGN CB-Farg:LIST-ITEM-PAIRS = cAlle + ","
             CB-Farg:screen-value = " ".
      for each Farg FIELDS(Farg FarBeskr) NO-LOCK:
        CB-Farg:add-last(string(Farg.Farg,"zzzz9") + "   " + 
                         REPLACE(Farg.FarBeskr,","," "),string(Farg.Farg)).
      end.
      ASSIGN CB-Material:LIST-ITEM-PAIRS = cAlle + ","
             CB-Material:screen-value = " ".
      for each Material no-lock:
        CB-Material:add-last(string(Material.MatKod,"zz9") + "   " + 
                         REPLACE(Material.MatBeskr,","," "),string(Material.MatKod)).
      end.
      ASSIGN CB-IndType:LIST-ITEM-PAIRS = cAlle + ","
             CB-IndType:screen-value = " ".
      for each IndType no-lock:
        CB-IndType:add-last(string(IndType.Individtype,"z9") + "   " + 
                         REPLACE(IndType.individBeskr,","," "),string(IndType.Individtype)).
      end.
      ASSIGN CB-Pakke:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Pakke:screen-value = " ".

      ASSIGN CB-OPris:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-OPris:screen-value = " ".

      ASSIGN CB-BildeIKasse:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-BildeIKasse:screen-value = " ".

      ASSIGN CB-HkStyrt:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-HkStyrt:screen-value = " ".

      ASSIGN CB-LokPris:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-LokPris:screen-value = " ".

      ASSIGN CB-IKasse:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-IKasse:screen-value = " ".

      ASSIGN CB-Kunderabatt:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Kunderabatt:screen-value = " ".

      ASSIGN CB-Storrelser:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Storrelser:screen-value = " ".

      ASSIGN CB-Aktiv:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Aktiv:screen-value = " ".

      ASSIGN CB-Annonse:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Annonse:screen-value = " ".

      ASSIGN CB-HovedModellFarge:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-HovedModellFarge:screen-value = " ".

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
          FI-AnbefaltPris1:SCREEN-VALUE = ""
          FI-AnbefaltPris2:SCREEN-VALUE = ""
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillKrit fFrameWin 
PROCEDURE NullstillKrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN FI-Vg:SCREEN-VALUE = cAlle
         CB-Avdeling:screen-value = " "
         CB-HuvGr:screen-value = " "
         CB-VareSlag:screen-value = " "
         CB-StrTypeId:screen-value = " "
         CB-Sasong:screen-value = " "
         CB-Farg:screen-value = " "
         CB-Material:screen-value = " "
         CB-IndType:screen-value = " "
         CB-Pakke:screen-value = " "
         CB-OPris:screen-value = "NO"
         CB-BildeIKasse:screen-value = " "
         CB-HkStyrt:screen-value = " "
         CB-LokPris:screen-value = " "
         CB-IKasse:screen-value = " "
         CB-Kunderabatt:screen-value = " "
         CB-Storrelser:screen-value = " "
         CB-Aktiv:screen-value = " "
         CB-Annonse:screen-value = " "
         CB-HovedModellFarge:screen-value = " "
         .

  APPLY "choose" TO B-AnbefaltPris.
  APPLY "choose" TO B-ArtikkelNrBlank.
  APPLY "choose" TO B-BeskrBlank. 
  APPLY "choose" TO B-Bongtekst.
  APPLY "choose" TO B-EndretBlank. 
  APPLY "choose" TO B-LevKodBlank. 
  APPLY "choose" TO B-LevNrBlank. 
  APPLY "choose" TO B-OpprettetBlank. 
  APPLY "choose" TO B-VgBlank.

  APPLY "entry" TO FI-Beskr.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFilterVal fFrameWin 
PROCEDURE SettFilterVal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER icParam AS CHAR NO-UNDO.

 DEF VAR cField AS CHAR NO-UNDO.
 DEF VAR cValue AS CHAR NO-UNDO.

 DO WITH FRAME fMain:
     IF NUM-ENTRIES(icParam,"|") >= 2 THEN
     DO:
         CASE ENTRY(1,icParam,"|"):
             WHEN 'FI-Utvidetsok' THEN ASSIGN FI-Utvidetsok:SCREEN-VALUE = ENTRY(2,icParam,'|').
             WHEN 'FI-LevKod' THEN ASSIGN FI-LevKod:SCREEN-VALUE = ENTRY(2,icParam,'|').
         END CASE.
     END.
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFields   AS CHAR NO-UNDO.
  DEF VAR pcValues   AS CHAR NO-UNDO.
  DEF VAR pcSort     AS CHAR NO-UNDO.
  DEF VAR pcOperator AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR iCount      AS INTE NO-UNDO.
  DEF VAR iTst        AS INTE NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN pcFeltListe = "LevNr,Vg,Sasong,LevKod,Pakke,OPris,BildeIKasse,HkStyrt,LokPris,IKasse,Beskr,BongTekst,Kunderabatt,Storrelser,Aktivert,AnonseArtikkel,HovedModellFarge,StrTypeId,Farg,MatKod,RegistrertDato,EDato,ArtikkelNr,AnbefaltPris,Hg,AvdelingNr,Individtype,Vareslag,UtvidetSok".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "ArtikkelNr" THEN DO:
                IF FI-FraArtikkelNr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ArtikkelNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-FraArtikkelNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilArtikkelNr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ArtikkelNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-TilArtikkelNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "LevNr" THEN DO:
                IF FI-LevNr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LevNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-LevNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Vg" THEN DO:
                ASSIGN iTst = INT(FI-Vg:SCREEN-VALUE) NO-ERROR.

                IF NOT ERROR-STATUS:ERROR THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Vg"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-Vg:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "AvdelingNr" THEN DO:
                IF CB-Avdeling:SCREEN-VALUE <> "" THEN
                DO:
                    FIND FIRST HuvGr NO-LOCK WHERE
                        HuvGr.AvdelingNr = INT(CB-Avdeling:SCREEN-VALUE) NO-ERROR.
                    IF AVAILABLE HuvGr THEN
                    ASSIGN
                    pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Hg"
                    pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                               string(HuvGr.Hg)
                    pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                    FIND LAST HuvGr NO-LOCK WHERE
                        HuvGr.AvdelingNr = INT(CB-Avdeling:SCREEN-VALUE) NO-ERROR.
                    IF AVAILABLE HuvGr THEN
                    ASSIGN
                    pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Hg"
                    pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                               string(HuvGr.Hg)
                    pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
                END.
            END.
            WHEN "Hg" THEN DO:
                IF CB-HuvGr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Hg"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-HuvGr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Sasong" THEN DO:
                IF CB-Sasong:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Sasong"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Sasong:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Levkod" THEN DO:
                IF FI-Levkod:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Levkod:SCREEN-VALUE = TRIM(FI-Levkod:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Levkod"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    IF NUM-ENTRIES(FI-Levkod:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Levkod:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Levkod:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-Levkod:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "AnbefaltPris" THEN DO:
                IF int(FI-AnbefaltPris1:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnbefaltPris"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-AnbefaltPris1:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF int(FI-AnbefaltPris2:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnbefaltPris"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-AnbefaltPris2:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "Pakke" THEN DO:
                IF CB-Pakke:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Pakke"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Pakke:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Beskr" THEN DO:
                IF FI-Beskr:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Beskr:SCREEN-VALUE = TRIM(FI-Beskr:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Beskr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    IF NUM-ENTRIES(FI-Beskr:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Beskr:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Beskr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-Beskr:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "Bongtekst" THEN DO:
                IF FI-Bongtekst:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Bongtekst:SCREEN-VALUE = TRIM(FI-Bongtekst:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BongTekst"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    IF NUM-ENTRIES(FI-Bongtekst:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Bongtekst:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Bongtekst:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-BongTekst:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "RegistrertDato" THEN DO:
                IF FI-FraOpprettet:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "RegistrertDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-FraOpprettet:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilOpprettet:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "RegistrertDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-TilOpprettet:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "EDato" THEN DO:
                IF FI-FraEndret:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-FraEndret:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilEndret:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-TilEndret:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "OPris" THEN DO:
                IF trim(CB-OPris:SCREEN-VALUE) <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "OPris"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-OPris:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "BildeIKasse" THEN DO:
                IF CB-BildeIKasse:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BildeIKasse"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-BildeIKasse:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "HkStyrt" THEN DO:
                IF CB-HkStyrt:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "HkStyrt"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-HkStyrt:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "LokPris" THEN DO:
                IF CB-LokPris:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LokPris"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-LokPris:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "IKasse" THEN DO:
                IF CB-IKasse:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "IKasse"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-IKasse:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Kunderabatt" THEN DO:
                IF CB-Kunderabatt:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Kunderabatt"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Kunderabatt:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Storrelser" THEN DO:
                IF CB-Storrelser:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Storrelser"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Storrelser:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Aktivert" THEN DO:
                IF CB-Aktiv:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Aktivert"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Aktiv:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "AnonseArtikkel" THEN DO:
                IF CB-Annonse:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnonseArtikkel"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Annonse:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
          WHEN "HovedModellFarge" THEN DO:
              IF CB-HovedModellFarge:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "HovedModellFarge"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                         CB-HovedModellFarge:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "StrTypeId" THEN DO:
              IF CB-StrTypeId:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "StrTypeId"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                         CB-StrTypeId:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "VareSlag" THEN DO:
              IF CB-VareSlag:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "VareSlag"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                         CB-VareSlag:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "Individtype" THEN DO:
              IF CB-IndType:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "IndividType"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                         CB-IndType:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "Farg" THEN DO:
              IF CB-Farg:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Farg"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                         CB-Farg:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "MatKod" THEN DO:
              IF CB-Material:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "MatKod"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                         CB-Material:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "UtvidetSok" THEN DO:
              IF FI-UtvidetSok:SCREEN-VALUE <> "" THEN
              ASSIGN
              FI-UtvidetSok:SCREEN-VALUE = TRIM(FI-UtvidetSok:SCREEN-VALUE)
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "UtvidetSok"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                  FI-UtvidetSok:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "CONTAINS".
          END.

        END CASE.
    END.
  END.
/* 

      Lager,Storrelser,Aktiv,Annonse

 
      ASSIGN CB-Bildeikasse:LIST-ITEM-PAIRS = "[Alle],"
      ASSIGN CB-HKstyrt:LIST-ITEM-PAIRS = "[Alle],"
      ASSIGN CB-LokalPris:LIST-ITEM-PAIRS = "[Alle],"
      ASSIGN CB-Vareikasse:LIST-ITEM-PAIRS = "[Alle],"

 
 */


/* Tar bort siste entry i listen som er dummy */
ENTRY ( NUM-ENTRIES(pcFeltListe) , pcFeltListe ) = "Hg".

/*   MESSAGE pcFields SKIP                                 */
/*           pcValues SKIP                                 */
/*           pcSort SKIP                                   */
/*           pcOperator SKIP                               */
/*           pcFeltListe SKIP                              */
/*           "CB-OPris:SCREEN-VALUE" CB-OPris:SCREEN-VALUE */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                */
  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

