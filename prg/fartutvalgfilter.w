&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for FRAME fMain                                          */
&Scoped-define QUERY-STRING-fMain FOR EACH ArtBas NO-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH ArtBas NO-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-fMain ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 CB-Grunnsortiment FI-Beskr ~
FI-FraOpprettet FI-TilOpprettet CB-Pakke CB-OPris FI-Bongtekst FI-FraEndret ~
FI-TilEndret CB-Kunderabatt CB-Bildeikasse CB-Bonus_Givende FI-LevKod ~
FI-FraVPIdato FI-TilVPIdato CB-ManRabIKas CB-HKstyrt CB-Medlemsutbytte ~
FI-FraSanertDato FI-TilSanertDato FI-LevFargKod CB-Utgatt ~
CB-ManueltOpprettet CB-Pant FI-FraUtgattDato FI-TilUtgattDato CB-Annonse ~
CB-IKasse FI-FraArtikkelNr FI-TilartikkelNr CB-Telefonkort ~
CB-HovedModellFarge CB-Lager FI-AnbefaltPris1 FI-AnbefaltPris2 ~
FI-Kjokkenskriver CB-Web CB-GjFakturert CB-Storrelser CB-HoyLavMva ~
CB-BestForslag FI-Lokasjon CB-VareType CB-KjedeVare CB-NON-Sale CB-NegVare ~
CB-SalgsStopp FI-Kampanjekode 
&Scoped-Define DISPLAYED-OBJECTS CB-Grunnsortiment FI-Beskr FI-FraOpprettet ~
FI-TilOpprettet CB-Pakke CB-OPris FI-Bongtekst FI-FraEndret FI-TilEndret ~
CB-Kunderabatt CB-Bildeikasse CB-Bonus_Givende FI-LevKod FI-FraVPIdato ~
FI-TilVPIdato CB-ManRabIKas CB-HKstyrt CB-Medlemsutbytte FI-FraSanertDato ~
FI-TilSanertDato FI-LevFargKod CB-Utgatt CB-ManueltOpprettet CB-Pant ~
FI-FraUtgattDato FI-TilUtgattDato CB-Annonse CB-IKasse FI-FraArtikkelNr ~
FI-TilartikkelNr CB-Telefonkort CB-HovedModellFarge CB-Lager ~
FI-AnbefaltPris1 FI-AnbefaltPris2 FI-Kjokkenskriver CB-Web CB-GjFakturert ~
CB-Storrelser CB-HoyLavMva CB-BestForslag FI-Lokasjon CB-VareType ~
CB-KjedeVare CB-NON-Sale CB-NegVare CB-SalgsStopp FI-Kampanjekode ~
FI-Fltertekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ChooseBeskr fFrameWin 
FUNCTION ChooseBeskr RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE CB-Annonse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Annonse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-BestForslag AS CHARACTER FORMAT "X(256)":U 
     LABEL "Best.forsl" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Flagg for automatisk bestillingsforslag" NO-UNDO.

DEFINE VARIABLE CB-Bildeikasse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bilde i kass" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Bonus_Givende AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rab.grunnl" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Artikler som inngår i rabattgrunnlag medlemsklubb" NO-UNDO.

DEFINE VARIABLE CB-GjFakturert AS CHARACTER FORMAT "X(256)":U 
     LABEL "G.fakt" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Grunnsortiment AS CHARACTER FORMAT "X(256)":U 
     LABEL "Gronnsort." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE CB-HoyLavMva AS CHARACTER FORMAT "X(256)":U 
     LABEL "H/L mva" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-IKasse AS CHARACTER FORMAT "X(256)":U 
     LABEL "I kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-KjedeVare AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kjedelev." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kunderabatt AS CHARACTER FORMAT "X(256)" INITIAL "no" 
     LABEL "Knd.rab" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "<Alle>","",
                     "Ja","yes",
                     "Nei","no"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Lager AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lagerstyrt" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-ManRabIKas AS CHARACTER FORMAT "X(256)" INITIAL "no" 
     LABEL "Man.rab" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","no"
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Manuell rabatt" NO-UNDO.

DEFINE VARIABLE CB-ManueltOpprettet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Man.oppr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Medlemsutbytte AS CHARACTER FORMAT "X(256)":U 
     LABEL "Med.utb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-NegVare AS CHARACTER FORMAT "X(256)" INITIAL "no" 
     LABEL "Neg.vare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","no"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-NON-Sale AS CHARACTER FORMAT "X(256)" INITIAL "no" 
     LABEL "Non-Sale" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","no"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

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

DEFINE VARIABLE CB-Pant AS CHARACTER FORMAT "X(256)" INITIAL "no" 
     LABEL "Pant" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","no"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-SalgsStopp AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Salgsstopp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Storrelser AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelser" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Telefonkort AS CHARACTER FORMAT "X(256)":U 
     LABEL "Telefonkort" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Utgatt AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utgått" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CB-VareType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Web AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nettbutikk" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPris1 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     LABEL "Anbefalt pris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fra og med anbefalt pris" NO-UNDO.

DEFINE VARIABLE FI-AnbefaltPris2 AS DECIMAL FORMAT "->>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Til og med anbefalt pris" NO-UNDO.

DEFINE VARIABLE FI-Beskr AS CHARACTER FORMAT "x(250)" 
     LABEL "Utvidetsøk" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1
     BGCOLOR 16  NO-UNDO.

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

DEFINE VARIABLE FI-FraSanertDato AS DATE FORMAT "99/99/99":U 
     LABEL "Sanert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraUtgattDato AS DATE FORMAT "99/99/99":U 
     LABEL "Utgått dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraVPIdato AS DATE FORMAT "99/99/99":U 
     LABEL "VPI dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kampanjekode AS CHARACTER FORMAT "X(30)" 
     LABEL "Kampanjekode" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1.

DEFINE VARIABLE FI-Kjokkenskriver AS DECIMAL FORMAT ">9":U INITIAL 0 
     LABEL "Kjøkkenskrv" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Kjøkkenskriver" NO-UNDO.

DEFINE VARIABLE FI-LevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverandørs fargekode" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverandørs art.nr" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Lokasjon AS CHARACTER FORMAT "X(30)" 
     LABEL "Lokasjon" 
     VIEW-AS FILL-IN 
     SIZE 27.8 BY 1.

DEFINE VARIABLE FI-TilartikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilEndret AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilOpprettet AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilSanertDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilUtgattDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilVPIdato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 161 BY 9.24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     CB-Grunnsortiment AT ROW 1.48 COL 149 COLON-ALIGNED
     FI-Beskr AT ROW 1.52 COL 23 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     FI-FraOpprettet AT ROW 1.52 COL 65.4 COLON-ALIGNED
     FI-TilOpprettet AT ROW 1.52 COL 79.2 COLON-ALIGNED NO-LABEL
     CB-Pakke AT ROW 1.52 COL 104 COLON-ALIGNED
     CB-OPris AT ROW 1.52 COL 125.8 COLON-ALIGNED
     FI-Bongtekst AT ROW 2.57 COL 23 COLON-ALIGNED
     FI-FraEndret AT ROW 2.57 COL 65.4 COLON-ALIGNED
     FI-TilEndret AT ROW 2.57 COL 79.2 COLON-ALIGNED NO-LABEL
     CB-Kunderabatt AT ROW 2.57 COL 104 COLON-ALIGNED HELP
          "Åpner for at kunderabatt gis på artikkelen."
     CB-Bildeikasse AT ROW 2.57 COL 125.8 COLON-ALIGNED
     CB-Bonus_Givende AT ROW 2.57 COL 149 COLON-ALIGNED HELP
          "Artikler som inngår i rabattgrunnlag medlemsklubb"
     FI-LevKod AT ROW 3.62 COL 23 COLON-ALIGNED
     FI-FraVPIdato AT ROW 3.62 COL 65.4 COLON-ALIGNED
     FI-TilVPIdato AT ROW 3.62 COL 79.2 COLON-ALIGNED NO-LABEL
     CB-ManRabIKas AT ROW 3.67 COL 104 COLON-ALIGNED HELP
          "Åpner for at kunderabatt gis på artikkelen."
     CB-HKstyrt AT ROW 3.67 COL 125.8 COLON-ALIGNED
     CB-Medlemsutbytte AT ROW 3.67 COL 149 COLON-ALIGNED
     FI-FraSanertDato AT ROW 4.62 COL 65.4 COLON-ALIGNED
     FI-TilSanertDato AT ROW 4.62 COL 79.2 COLON-ALIGNED NO-LABEL
     FI-LevFargKod AT ROW 4.67 COL 23 COLON-ALIGNED
     CB-Utgatt AT ROW 4.71 COL 104 COLON-ALIGNED
     CB-ManueltOpprettet AT ROW 4.71 COL 125.8 COLON-ALIGNED
     CB-Pant AT ROW 4.71 COL 148.8 COLON-ALIGNED HELP
          "Angir om artikkelen er av type Non-Sale"
     FI-FraUtgattDato AT ROW 5.62 COL 65.4 COLON-ALIGNED
     FI-TilUtgattDato AT ROW 5.62 COL 79.2 COLON-ALIGNED NO-LABEL
     CB-Annonse AT ROW 5.67 COL 104 COLON-ALIGNED
     CB-IKasse AT ROW 5.67 COL 125.8 COLON-ALIGNED
     FI-FraArtikkelNr AT ROW 5.71 COL 23 COLON-ALIGNED
     FI-TilartikkelNr AT ROW 5.71 COL 36.8 COLON-ALIGNED NO-LABEL
     CB-Telefonkort AT ROW 5.71 COL 149 COLON-ALIGNED
     CB-HovedModellFarge AT ROW 6.67 COL 104 COLON-ALIGNED
     CB-Lager AT ROW 6.67 COL 117.4
     FI-AnbefaltPris1 AT ROW 6.71 COL 23 COLON-ALIGNED
     FI-AnbefaltPris2 AT ROW 6.71 COL 36.8 COLON-ALIGNED NO-LABEL
     FI-Kjokkenskriver AT ROW 6.71 COL 65 COLON-ALIGNED WIDGET-ID 2
     CB-Web AT ROW 6.71 COL 149 COLON-ALIGNED
     CB-GjFakturert AT ROW 7.67 COL 83.4 COLON-ALIGNED
     CB-Storrelser AT ROW 7.67 COL 104 COLON-ALIGNED
     CB-HoyLavMva AT ROW 7.67 COL 149 COLON-ALIGNED
     CB-BestForslag AT ROW 7.71 COL 118
     FI-Lokasjon AT ROW 7.76 COL 23 COLON-ALIGNED HELP
          "Lokasjon"
     CB-VareType AT ROW 8.67 COL 52.8 COLON-ALIGNED NO-LABEL
     CB-KjedeVare AT ROW 8.67 COL 83.4 COLON-ALIGNED
     CB-NON-Sale AT ROW 8.67 COL 104 COLON-ALIGNED HELP
          "Angir om artikkelen er av type Non-Sale"
     CB-NegVare AT ROW 8.67 COL 125.8 COLON-ALIGNED HELP
          "Angir om artikkelen er av type Non-Sale"
     CB-SalgsStopp AT ROW 8.71 COL 149 COLON-ALIGNED WIDGET-ID 6
     FI-Kampanjekode AT ROW 8.76 COL 23 COLON-ALIGNED HELP
          "Lokasjon"
     FI-Fltertekst AT ROW 1 COL 1 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.29 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 162 BY 9.52.


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
         HEIGHT             = 9.52
         WIDTH              = 162.
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
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-BestForslag IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CB-Lager IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Fltertekst IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Fltertekst:READ-ONLY IN FRAME fMain        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "skotex.ArtBas"
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CB-Annonse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Annonse fFrameWin
ON VALUE-CHANGED OF CB-Annonse IN FRAME fMain /* Annonse */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Bildeikasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Bildeikasse fFrameWin
ON VALUE-CHANGED OF CB-Bildeikasse IN FRAME fMain /* Bilde i kass */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Bonus_Givende
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Bonus_Givende fFrameWin
ON VALUE-CHANGED OF CB-Bonus_Givende IN FRAME fMain /* Rab.grunnl */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-GjFakturert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-GjFakturert fFrameWin
ON VALUE-CHANGED OF CB-GjFakturert IN FRAME fMain /* G.fakt */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Grunnsortiment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Grunnsortiment fFrameWin
ON VALUE-CHANGED OF CB-Grunnsortiment IN FRAME fMain /* Gronnsort. */
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


&Scoped-define SELF-NAME CB-HoyLavMva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HoyLavMva fFrameWin
ON VALUE-CHANGED OF CB-HoyLavMva IN FRAME fMain /* H/L mva */
DO:
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


&Scoped-define SELF-NAME CB-KjedeVare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-KjedeVare fFrameWin
ON VALUE-CHANGED OF CB-KjedeVare IN FRAME fMain /* Kjedelev. */
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


&Scoped-define SELF-NAME CB-ManRabIKas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ManRabIKas fFrameWin
ON VALUE-CHANGED OF CB-ManRabIKas IN FRAME fMain /* Man.rab */
DO:
   RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ManueltOpprettet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ManueltOpprettet fFrameWin
ON VALUE-CHANGED OF CB-ManueltOpprettet IN FRAME fMain /* Man.oppr */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Medlemsutbytte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Medlemsutbytte fFrameWin
ON VALUE-CHANGED OF CB-Medlemsutbytte IN FRAME fMain /* Med.utb */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-NegVare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-NegVare fFrameWin
ON VALUE-CHANGED OF CB-NegVare IN FRAME fMain /* Neg.vare */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-NON-Sale
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-NON-Sale fFrameWin
ON VALUE-CHANGED OF CB-NON-Sale IN FRAME fMain /* Non-Sale */
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


&Scoped-define SELF-NAME CB-Pant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Pant fFrameWin
ON VALUE-CHANGED OF CB-Pant IN FRAME fMain /* Pant */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-SalgsStopp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-SalgsStopp fFrameWin
ON VALUE-CHANGED OF CB-SalgsStopp IN FRAME fMain /* Salgsstopp */
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


&Scoped-define SELF-NAME CB-Telefonkort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Telefonkort fFrameWin
ON VALUE-CHANGED OF CB-Telefonkort IN FRAME fMain /* Telefonkort */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Utgatt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Utgatt fFrameWin
ON VALUE-CHANGED OF CB-Utgatt IN FRAME fMain /* Utgått */
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-VareType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-VareType fFrameWin
ON VALUE-CHANGED OF CB-VareType IN FRAME fMain
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Web
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Web fFrameWin
ON VALUE-CHANGED OF CB-Web IN FRAME fMain /* Nettbutikk */
DO:
    RUN StartSok.
/*     MESSAGE INPUT cb-web = "YES"           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
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


&Scoped-define SELF-NAME FI-FraSanertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraSanertDato fFrameWin
ON DELETE-CHARACTER OF FI-FraSanertDato IN FRAME fMain /* Sanert */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ?
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraUtgattDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraUtgattDato fFrameWin
ON DELETE-CHARACTER OF FI-FraUtgattDato IN FRAME fMain /* Utgått dato */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ?
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-FraVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-FraVPIdato fFrameWin
ON DELETE-CHARACTER OF FI-FraVPIdato IN FRAME fMain /* VPI dato */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ?
      .
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


&Scoped-define SELF-NAME FI-TilSanertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilSanertDato fFrameWin
ON DELETE-CHARACTER OF FI-TilSanertDato IN FRAME fMain
DO:
    ASSIGN
      SELF:SCREEN-VALUE = ?
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilUtgattDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilUtgattDato fFrameWin
ON DELETE-CHARACTER OF FI-TilUtgattDato IN FRAME fMain
DO:
    ASSIGN
      SELF:SCREEN-VALUE = ?
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilVPIdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilVPIdato fFrameWin
ON DELETE-CHARACTER OF FI-TilVPIdato IN FRAME fMain
DO:
    ASSIGN
      SELF:SCREEN-VALUE = ?
      .

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

ASSIGN CB-SalgsStopp:SCREEN-VALUE IN FRAME fMain = '0'.

ON 'return':U OF FRAME fMain ANYWHERE
    RUN OKbutton IN DYNAMIC-FUNCTION("getContainerSource") NO-ERROR.
/*     DYNAMIC-FUNCTION("applyOKbutton" IN DYNAMIC-FUNCTION("getContainerSource")). */

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
  DISPLAY CB-Grunnsortiment FI-Beskr FI-FraOpprettet FI-TilOpprettet CB-Pakke 
          CB-OPris FI-Bongtekst FI-FraEndret FI-TilEndret CB-Kunderabatt 
          CB-Bildeikasse CB-Bonus_Givende FI-LevKod FI-FraVPIdato FI-TilVPIdato 
          CB-ManRabIKas CB-HKstyrt CB-Medlemsutbytte FI-FraSanertDato 
          FI-TilSanertDato FI-LevFargKod CB-Utgatt CB-ManueltOpprettet CB-Pant 
          FI-FraUtgattDato FI-TilUtgattDato CB-Annonse CB-IKasse 
          FI-FraArtikkelNr FI-TilartikkelNr CB-Telefonkort CB-HovedModellFarge 
          CB-Lager FI-AnbefaltPris1 FI-AnbefaltPris2 FI-Kjokkenskriver CB-Web 
          CB-GjFakturert CB-Storrelser CB-HoyLavMva CB-BestForslag FI-Lokasjon 
          CB-VareType CB-KjedeVare CB-NON-Sale CB-NegVare CB-SalgsStopp 
          FI-Kampanjekode FI-Fltertekst 
      WITH FRAME fMain.
  ENABLE RECT-1 CB-Grunnsortiment FI-Beskr FI-FraOpprettet FI-TilOpprettet 
         CB-Pakke CB-OPris FI-Bongtekst FI-FraEndret FI-TilEndret 
         CB-Kunderabatt CB-Bildeikasse CB-Bonus_Givende FI-LevKod FI-FraVPIdato 
         FI-TilVPIdato CB-ManRabIKas CB-HKstyrt CB-Medlemsutbytte 
         FI-FraSanertDato FI-TilSanertDato FI-LevFargKod CB-Utgatt 
         CB-ManueltOpprettet CB-Pant FI-FraUtgattDato FI-TilUtgattDato 
         CB-Annonse CB-IKasse FI-FraArtikkelNr FI-TilartikkelNr CB-Telefonkort 
         CB-HovedModellFarge CB-Lager FI-AnbefaltPris1 FI-AnbefaltPris2 
         FI-Kjokkenskriver CB-Web CB-GjFakturert CB-Storrelser CB-HoyLavMva 
         CB-BestForslag FI-Lokasjon CB-VareType CB-KjedeVare CB-NON-Sale 
         CB-NegVare CB-SalgsStopp FI-Kampanjekode 
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
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
  
  
    DO WITH FRAME {&FRAME-NAME}:
      
      ASSIGN CB-Pakke:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Pakke:screen-value = " "

             CB-OPris:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-OPris:screen-value = " "

             CB-Grunnsortiment:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Grunnsortiment:screen-value = " "

             CB-Telefonkort:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Telefonkort:screen-value = " "

             CB-Bonus_Givende:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Bonus_Givende:screen-value = " "

             CB-NON-Sale:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-NON-Sale:screen-value = " "

             CB-Pant:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Pant:screen-value = " "

             CB-NegVare:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-NegVare:screen-value = " "

             CB-KjedeVare:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-KjedeVare:screen-value = " "

             CB-GjFakturert:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-GjFakturert:screen-value = " "

             CB-Medlemsutbytte:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Medlemsutbytte:screen-value = " "

             CB-HoyLavMva:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-HoyLavMva:screen-value = " "

             CB-Web:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Web:screen-value = " "

             CB-BildeIKasse:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-BildeIKasse:screen-value = " "

             CB-HkStyrt:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-HkStyrt:screen-value = " "

             CB-ManueltOpprettet:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-ManueltOpprettet:screen-value = " "

             CB-IKasse:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-IKasse:screen-value = " "

             CB-Kunderabatt:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Kunderabatt:screen-value = " "

             CB-ManRabIKas:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-ManRabIKas:screen-value = " "

             CB-Storrelser:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Storrelser:screen-value = " "

             CB-Utgatt:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Utgatt:screen-value = " "

             CB-Annonse:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Annonse:screen-value = " "

             CB-HovedModellFarge:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-HovedModellFarge:screen-value = " "

             CB-Lager:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-Lager:SCREEN-VALUE = " "

             CB-BestForslag:LIST-ITEM-PAIRS = cAlle + ",,Ja,YES,Nei,no"
             CB-BestForslag:SCREEN-VALUE = " "
             .

             cTekst = "<Leveringstype>,". 
             FOR EACH SysPara NO-LOCK WHERE
               SysPara.SysHId = 2 AND 
               SysPara.SysGr = 11:
          
          
               ASSIGN
                 cTekst = cTekst + (IF cTekst = '' THEN '' ELSE ',') + SysPara.Beskrivelse + ',' + STRING(SysPara.ParaNr). 
             END.
             ASSIGN
             CB-Varetype:LIST-ITEM-PAIRS = cTekst
             CB-VareType:screen-value = " ".
          
             cTekst = "". 
             FOR EACH SysPara NO-LOCK WHERE
               SysPara.SysHId = 2 AND 
               SysPara.SysGr = 12:


               ASSIGN
                 cTekst = cTekst + (IF cTekst = '' THEN '' ELSE ',') + SysPara.Beskrivelse + ',' + STRING(SysPara.Parameter1). 
             END.
             IF cTekst = '' THEN cTekst = 'Åpen,0,Mykt stopp,1,Hardt stopp,2'.

             ASSIGN
             CB-SalgsStopp:LIST-ITEM-PAIRS = cTekst
             CB-SalgsStopp:screen-value = "0"
             .
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
  ASSIGN CB-Pakke:screen-value = ""
         CB-OPris:screen-value = " "
         CB-Grunnsortiment:SCREEN-VALUE = " "
         CB-Telefonkort:SCREEN-VALUE = " "
         CB-KjedeVare:screen-value = " "
         CB-NON-Sale:screen-value = " "
         CB-NegVare:screen-value = " "
         CB-GjFakturert:screen-value = " "
         CB-BildeIKasse:screen-value = ""
         CB-HkStyrt:screen-value = ""
         CB-ManueltOpprettet:screen-value = ""
         CB-IKasse:screen-value = ""
         CB-Kunderabatt:screen-value = ""
         CB-ManRabIKas:SCREEN-VALUE = ""
         CB-Storrelser:screen-value = ""
         CB-Utgatt:screen-value = ""
         CB-Annonse:screen-value = ""
         CB-HovedModellFarge:screen-value = ""
         CB-Lager:SCREEN-VALUE = ""
         CB-BestForslag:SCREEN-VALUE = ""
         FI-AnbefaltPris1:SCREEN-VALUE = STRING(0)
         FI-AnbefaltPris2:SCREEN-VALUE = STRING(0) 
         FI-Beskr:SCREEN-VALUE = "" 
         FI-Bongtekst:SCREEN-VALUE = "" 
         FI-FraArtikkelNr:SCREEN-VALUE = STRING(0) 
         FI-FraEndret:SCREEN-VALUE = STRING(?) 
         FI-FraVPIdato:SCREEN-VALUE = STRING(?) 
         FI-FraOpprettet:SCREEN-VALUE = STRING(?) 
         FI-LevFargKod:SCREEN-VALUE = "" 
         FI-LevKod:SCREEN-VALUE = "" 
         FI-Lokasjon:SCREEN-VALUE = "" 
         FI-TilartikkelNr:SCREEN-VALUE = STRING(0) 
         FI-TilEndret:SCREEN-VALUE = STRING(?)
         FI-TilVPIdato:SCREEN-VALUE = STRING(?)
         FI-TilOpprettet:SCREEN-VALUE = STRING(?)
         FI-FraSanertDato:SCREEN-VALUE = STRING(?)
         FI-TilSanertDato:SCREEN-VALUE = STRING(?)
         FI-FraUtgattDato:SCREEN-VALUE = STRING(?)
         FI-TilUtgattDato:SCREEN-VALUE = STRING(?)
         FI-Kjokkenskriver:SCREEN-VALUE = ""
         CB-Web:SCREEN-VALUE = " ".

  APPLY "entry" TO FI-Beskr.
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
    IF CB-OPris:SCREEN-VALUE = "" THEN
        ASSIGN pcFeltListe = "LevKod,LevFargKod,Pakke,BildeIKasse,HkStyrt,LokPris,IKasse,Beskr,BongTekst,Kunderabatt,ManRabIKas,Storrelser,Utgatt,AnonseArtikkel,HovedModellFarge,MatKod,RegistrertDato,EDato,ArtikkelNr,AnbefaltPris,lager,BestForslag,VPIdato,ModellFarge,KjedeVare,Gjennomfaktureres,Medlemsutbytte,HoyLavMva,Lokasjon,SanertDato,UtgattDato,WebButikkArtikkel,VareType,ManueltOpprettet,NON_Sale,Grunnsortiment,Bonus_Givende,NegVare,Pant,Telefonkort,KampanjeKode,Kjokkenskriver,SalgsStopp".
    ELSE IF CB-OPris:SCREEN-VALUE = "YES" THEN
        ASSIGN pcFeltListe = "OPris,LevKod,LevFargKod,Pakke,BildeIKasse,HkStyrt,LokPris,IKasse,Beskr,BongTekst,Kunderabatt,ManRabIKas,Storrelser,Utgatt,AnonseArtikkel,HovedModellFarge,MatKod,RegistrertDato,EDato,ArtikkelNr,AnbefaltPris,lager,BestForslag,VPIdato,ModellFarge,KjedeVare,Gjennomfaktureres,Medlemsutbytte,HoyLavMva,Lokasjon,SanertDato,UtgattDato,WebButikkArtikkel,VareType,ManueltOpprettet,NON_Sale,Grunnsortiment,Bonus_Givende,NegVare,Pant,Telefonkort,KampanjeKode,Kjokkenskriver,SalgsStopp".
         ELSE
        ASSIGN pcFeltListe = "LevKod,LevFargKod,Pakke,BildeIKasse,HkStyrt,LokPris,IKasse,Beskr,BongTekst,Kunderabatt,ManRabIKas,Storrelser,Utgatt,AnonseArtikkel,HovedModellFarge,MatKod,RegistrertDato,EDato,ArtikkelNr,AnbefaltPris,lager,BestForslag,VPIdato,ModellFarge,KjedeVare,OPris,Gjennomfaktureres,Medlemsutbytte,HoyLavMva,Lokasjon,SanertDato,UtgattDato,WebButikkArtikkel,VareType,ManueltOpprettet,NON_Sale,Grunnsortiment,Bonus_Givende,NegVare,Pant,Telefonkort,KampanjeKode,Kjokkenskriver,SalgsStopp".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        {fartutvalgfilter.i}
    END.

  END.
  /* Tar bort siste entry i listen som er dummy */
  ENTRY ( NUM-ENTRIES(pcFeltListe) , pcFeltListe ) = "Hg".
  PUBLISH "SokSdo" (pcFields,pcValues,pcSort,pcOperator,pcFeltListe).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ChooseBeskr fFrameWin 
FUNCTION ChooseBeskr RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
APPLY "entry" TO FRAME fMain.
APPLY "choose" TO FI-Beskr IN FRAME fMain.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

