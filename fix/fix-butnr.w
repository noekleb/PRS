&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-Butiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Butiker ButikkKobling ButikkTilgang Gruppe ~
Kasse

/* Definitions for BROWSE BR-Butiker                                    */
&Scoped-define FIELDS-IN-QUERY-BR-Butiker Butiker.Butik ~
Butiker.Sentrallager Butiker.LanButikk Butiker.ButNamn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Butiker 
&Scoped-define QUERY-STRING-BR-Butiker FOR EACH Butiker NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Butiker OPEN QUERY BR-Butiker FOR EACH Butiker NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Butiker Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Butiker Butiker


/* Definitions for BROWSE BR-ButKobl                                    */
&Scoped-define FIELDS-IN-QUERY-BR-ButKobl ButikkKobling.Butik ~
ButikkKobling.BrGrpNr ButikkKobling.TeamNr ButikkKobling.TeamTypeId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-ButKobl 
&Scoped-define QUERY-STRING-BR-ButKobl FOR EACH ButikkKobling ~
      WHERE ButikkKobling.Butik = Butiker.Butik ~
 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-ButKobl OPEN QUERY BR-ButKobl FOR EACH ButikkKobling ~
      WHERE ButikkKobling.Butik = Butiker.Butik ~
 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-ButKobl ButikkKobling
&Scoped-define FIRST-TABLE-IN-QUERY-BR-ButKobl ButikkKobling


/* Definitions for BROWSE BR-ButTilgang                                 */
&Scoped-define FIELDS-IN-QUERY-BR-ButTilgang ButikkTilgang.Butik ~
ButikkTilgang.BrGrpNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-ButTilgang 
&Scoped-define QUERY-STRING-BR-ButTilgang FOR EACH ButikkTilgang ~
      WHERE ButikkTilgang.Butik = Butiker.Butik NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-ButTilgang OPEN QUERY BR-ButTilgang FOR EACH ButikkTilgang ~
      WHERE ButikkTilgang.Butik = Butiker.Butik NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-ButTilgang ButikkTilgang
&Scoped-define FIRST-TABLE-IN-QUERY-BR-ButTilgang ButikkTilgang


/* Definitions for BROWSE BR-Gruppe                                     */
&Scoped-define FIELDS-IN-QUERY-BR-Gruppe Gruppe.ButikkNr Gruppe.GruppeNr ~
Gruppe.Navn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Gruppe 
&Scoped-define QUERY-STRING-BR-Gruppe FOR EACH Gruppe ~
      WHERE Gruppe.ButikkNr = Butiker.Butik NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Gruppe OPEN QUERY BR-Gruppe FOR EACH Gruppe ~
      WHERE Gruppe.ButikkNr = Butiker.Butik NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Gruppe Gruppe
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Gruppe Gruppe


/* Definitions for BROWSE BR-Kasse                                      */
&Scoped-define FIELDS-IN-QUERY-BR-Kasse Kasse.ButikkNr Kasse.KasseNr ~
Kasse.ElJournal[1] Kasse.ElJournal[2] Kasse.ElJournalId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Kasse 
&Scoped-define QUERY-STRING-BR-Kasse FOR EACH Kasse ~
      WHERE Kasse.ButikkNr = Butiker.Butik NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-Kasse OPEN QUERY BR-Kasse FOR EACH Kasse ~
      WHERE Kasse.ButikkNr = Butiker.Butik NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-Kasse Kasse
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Kasse Kasse


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-Butiker}~
    ~{&OPEN-QUERY-BR-ButTilgang}~
    ~{&OPEN-QUERY-BR-Gruppe}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-58 RECT-59 RECT-60 B-HentNyCL B-BytCL ~
FI-CL-Ny FI-FirmanavnNy TG-BytAdressinfo BR-Butiker BR-Kasse BUTTON-6 ~
B-Kontroller FI-Butik-Ny FI-KjFirmanavn FI-KjButikkNavn FI-KjAdresse1 ~
FI-KjAdresse2 BR-Gruppe BR-ButKobl BR-ButTilgang FI-KjPostNr ~
FI-KjDagligLeder FI-KjKontaktperson FI-KjTelefon FI-BrgrpNavn FI-Teamnavn ~
FI-Butikk-NyTxt FILL-IN-9 FILL-IN-10 FILL-IN-8 FILL-IN-6 FILL-IN-7 
&Scoped-Define DISPLAYED-OBJECTS FI-CL FI-CL-Ny FI-Firmanavn FI-FirmanavnNy ~
FI-VPIEkst FI-VPIEkstNy FI-Butik-Gammel TG-BytAdressinfo FI-Butik-Ny ~
FI-KjFirmanavn FI-KjButikkNavn FI-KjAdresse1 FI-KjAdresse2 FI-KjPostNr ~
FI-KjDagligLeder FI-KjKontaktperson FI-KjTelefon FI-BrgrpNavn TG-Akt_rapp ~
TG-ButLok TG-KassererValuta TG-KundeTrans TG-OvOrdre-Frabutik TG-ApnSkjema ~
TG-dags_rap TG-kas_konter TG-Lager TG-OvOrdre-TilButik FI-Teamnavn ~
TG-ArtLag TG-etikett TG-kas_logg TG-Medlem TG-ReklamasjonsLinje TG-ArtLok ~
TG-Faktura TG-kas_rap TG-MedlemBetTrans-betButik ~
TG-ReklamasjonsLinje-SolgtIBut TG-AnalyseLogg TG-BestKasse TG-Fributik ~
TG-konto TG-MedlemBetTrans TG-StLager TG-BongHode TG-BestLevert TG-Gavekort ~
TG-kont_mal TG-MedlemSaldo TG-StLinje TG-BongLinje TG-BestLinje ~
TG-Gavekort-Frabutikk TG-Kort_Spes TG-MedTrans TG-TelleHode ~
TG-BongLinje-MButikkNr TG-BestStr TG-KassererBilag TG-Kunde TG-OvBuffer-Fra ~
TG-TelleLinje TG-BongLinje-ReturButikk TG-BokforingsBilag TG-KassererDag ~
TG-KundeBetTrans-betButik TG-OvBuffer-Til TG-Tilgode TG-Datasett ~
TG-Tilgode-FraButikkNr TG-KassererKontanter TG-KundeBetTrans ~
TG-OvLinje-Frabutik TG-butikkforsalj TG-hgrdag TG-TransLogg TG-ButikkSelger ~
TG-KassererOppgj TG-KundeSaldo TG-OvLinje-TilButik TG-timedag ~
TG-TransLogg-OvButik TG-varedag TG-z_nummer FILL-IN-20 FILL-IN-21 ~
FI-Butikk-NyTxt FILL-IN-9 FILL-IN-10 FILL-IN-8 FILL-IN-6 FILL-IN-7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-BytCL 
     LABEL "Byt CL" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-HentNyCL 
     LABEL "Hent" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Kontroller 
     LABEL "Kontroller" 
     SIZE 13 BY 1.14.

DEFINE BUTTON BUTTON-6 
     LABEL "Bytt butnr" 
     SIZE 12 BY 1.14.

DEFINE VARIABLE FI-BrgrpNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Brgruppenavn" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butik-Gammel AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Gammelt butikknr" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butik-Ny AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Nytt butikknr" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikk-NyTxt AS CHARACTER FORMAT "X(256)":U INITIAL "  -        Ny butikk        -" 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE FI-CL AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-CL-Ny AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 7.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Firmanavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Firmanavn" 
     VIEW-AS FILL-IN 
     SIZE 52.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FirmanavnNy AS CHARACTER FORMAT "X(256)":U 
     LABEL "Firmanavn" 
     VIEW-AS FILL-IN 
     SIZE 52.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KjAdresse1 AS CHARACTER FORMAT "X(30)" 
     LABEL "Adresse 1" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-KjAdresse2 AS CHARACTER FORMAT "X(30)" 
     LABEL "Adresse 2" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-KjButikkNavn AS CHARACTER FORMAT "X(30)" 
     LABEL "Butikknavn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-KjDagligLeder AS CHARACTER FORMAT "X(30)" 
     LABEL "Daglig leder" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-KjFirmanavn AS CHARACTER FORMAT "X(30)" 
     LABEL "Firmanavn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-KjKontaktperson AS CHARACTER FORMAT "X(30)" 
     LABEL "Kontakt" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FI-KjPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE FI-KjTelefon AS CHARACTER FORMAT "X(25)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.

DEFINE VARIABLE FI-Teamnavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Teamnavn" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VPIEkst AS CHARACTER FORMAT "X(256)":U 
     LABEL "VPI-ekst" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VPIEkstNy AS CHARACTER FORMAT "X(256)":U 
     LABEL "VPI-ekst" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U INITIAL " Kasse" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL "  -        Gammelt sentrallager        -" 
      VIEW-AS TEXT 
     SIZE 32.4 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U INITIAL "  -        Nytt sentrallager        -" 
      VIEW-AS TEXT 
     SIZE 29.6 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U INITIAL " Butikk-kobling" 
      VIEW-AS TEXT 
     SIZE 23 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL " Butikk-tilgang" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U INITIAL " Gruppe" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U INITIAL " Butiker" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72 BY 3.81.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.57.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.6 BY 11.1.

DEFINE VARIABLE TG-Akt_rapp AS LOGICAL INITIAL no 
     LABEL "akt_rapp" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-AnalyseLogg AS LOGICAL INITIAL no 
     LABEL "AnalyseLogg" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.8 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ApnSkjema AS LOGICAL INITIAL no 
     LABEL "ApnSkjema" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ArtLag AS LOGICAL INITIAL no 
     LABEL "ArtLag" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ArtLok AS LOGICAL INITIAL no 
     LABEL "ArtLok" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BestKasse AS LOGICAL INITIAL no 
     LABEL "BestKasse" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BestLevert AS LOGICAL INITIAL no 
     LABEL "BestLevert" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BestLinje AS LOGICAL INITIAL no 
     LABEL "BestLinje" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BestStr AS LOGICAL INITIAL no 
     LABEL "BestStr" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BokforingsBilag AS LOGICAL INITIAL no 
     LABEL "BokforingsBilag" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BongHode AS LOGICAL INITIAL no 
     LABEL "BongHode" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BongLinje AS LOGICAL INITIAL no 
     LABEL "BongLinje" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BongLinje-MButikkNr AS LOGICAL INITIAL no 
     LABEL "BongLinje-MButikkNr" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.8 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BongLinje-ReturButikk AS LOGICAL INITIAL no 
     LABEL "BongLinje-ReturButikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.8 BY .81 NO-UNDO.

DEFINE VARIABLE TG-butikkforsalj AS LOGICAL INITIAL no 
     LABEL "butikkforsalj" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ButikkSelger AS LOGICAL INITIAL no 
     LABEL "ButikkSelger" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ButLok AS LOGICAL INITIAL no 
     LABEL "ButLok" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-BytAdressinfo AS LOGICAL INITIAL yes 
     LABEL "Bytt navn-Adresse" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-dags_rap AS LOGICAL INITIAL no 
     LABEL "dags_rapp" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.2 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Datasett AS LOGICAL INITIAL no 
     LABEL "Datasett" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-etikett AS LOGICAL INITIAL no 
     LABEL "etikett" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Faktura AS LOGICAL INITIAL no 
     LABEL "Faktura" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Fributik AS LOGICAL INITIAL no 
     LABEL "Fributik" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Gavekort AS LOGICAL INITIAL no 
     LABEL "Gavekort" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Gavekort-Frabutikk AS LOGICAL INITIAL no 
     LABEL "Gavekort-Frabutikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-hgrdag AS LOGICAL INITIAL no 
     LABEL "hgrdag" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KassererBilag AS LOGICAL INITIAL no 
     LABEL "KassererBilag" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KassererDag AS LOGICAL INITIAL no 
     LABEL "KassererDag" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KassererKontanter AS LOGICAL INITIAL no 
     LABEL "KassererKontanter" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KassererOppgj AS LOGICAL INITIAL no 
     LABEL "KassererOppgj" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KassererValuta AS LOGICAL INITIAL no 
     LABEL "KassererValuta" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-kas_konter AS LOGICAL INITIAL no 
     LABEL "kas_konter" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE TG-kas_logg AS LOGICAL INITIAL no 
     LABEL "kas_logg" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE TG-kas_rap AS LOGICAL INITIAL no 
     LABEL "kas_rap" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-konto AS LOGICAL INITIAL no 
     LABEL "konto" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-kont_mal AS LOGICAL INITIAL no 
     LABEL "kont_mal" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kort_Spes AS LOGICAL INITIAL no 
     LABEL "Kort_spes" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Kunde AS LOGICAL INITIAL no 
     LABEL "Kunde" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KundeBetTrans AS LOGICAL INITIAL no 
     LABEL "KundeBetTrans" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KundeBetTrans-betButik AS LOGICAL INITIAL no 
     LABEL "KundeBetTrans-betButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 27.4 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KundeSaldo AS LOGICAL INITIAL no 
     LABEL "KundeSaldo" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE TG-KundeTrans AS LOGICAL INITIAL no 
     LABEL "KundeTrans" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Lager AS LOGICAL INITIAL no 
     LABEL "Lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Medlem AS LOGICAL INITIAL no 
     LABEL "Medlem" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-MedlemBetTrans AS LOGICAL INITIAL no 
     LABEL "MedlemBetTrans" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE TG-MedlemBetTrans-betButik AS LOGICAL INITIAL no 
     LABEL "MedlemBetTrans-betButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE TG-MedlemSaldo AS LOGICAL INITIAL no 
     LABEL "MedlemSaldo" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE TG-MedTrans AS LOGICAL INITIAL no 
     LABEL "MedTrans" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE TG-OvBuffer-Fra AS LOGICAL INITIAL no 
     LABEL "OvBuffer-Fra" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE TG-OvBuffer-Til AS LOGICAL INITIAL no 
     LABEL "OvBuffer-Til" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE TG-OvLinje-Frabutik AS LOGICAL INITIAL no 
     LABEL "OvLinje-FraButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-OvLinje-TilButik AS LOGICAL INITIAL no 
     LABEL "OvLinje-TilButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-OvOrdre-Frabutik AS LOGICAL INITIAL no 
     LABEL "OvOrdre-FraButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-OvOrdre-TilButik AS LOGICAL INITIAL no 
     LABEL "OvOrdre-TilButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ReklamasjonsLinje AS LOGICAL INITIAL no 
     LABEL "ReklamasjonsLinje" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-ReklamasjonsLinje-SolgtIBut AS LOGICAL INITIAL no 
     LABEL "ReklamasjonsLinje-SolgtIButikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE TG-StLager AS LOGICAL INITIAL no 
     LABEL "StLager" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-StLinje AS LOGICAL INITIAL no 
     LABEL "StLinje" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-TelleHode AS LOGICAL INITIAL no 
     LABEL "TelleHode" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE TG-TelleLinje AS LOGICAL INITIAL no 
     LABEL "TelleLinje" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Tilgode AS LOGICAL INITIAL no 
     LABEL "Tilgode" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Tilgode-FraButikkNr AS LOGICAL INITIAL no 
     LABEL "Tilgode-FraButikkNr" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .81 NO-UNDO.

DEFINE VARIABLE TG-timedag AS LOGICAL INITIAL no 
     LABEL "timedag" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-TransLogg AS LOGICAL INITIAL no 
     LABEL "TransLogg" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-TransLogg-OvButik AS LOGICAL INITIAL no 
     LABEL "TransLogg-OvButik" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE TG-varedag AS LOGICAL INITIAL no 
     LABEL "varedag" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TG-z_nummer AS LOGICAL INITIAL no 
     LABEL "z_nummer" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-Butiker FOR 
      Butiker SCROLLING.

DEFINE QUERY BR-ButKobl FOR 
      ButikkKobling SCROLLING.

DEFINE QUERY BR-ButTilgang FOR 
      ButikkTilgang SCROLLING.

DEFINE QUERY BR-Gruppe FOR 
      Gruppe SCROLLING.

DEFINE QUERY BR-Kasse FOR 
      Kasse SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Butiker C-Win _STRUCTURED
  QUERY BR-Butiker NO-LOCK DISPLAY
      Butiker.Butik FORMAT ">>>>>9":U
      Butiker.Sentrallager FORMAT "yes/no":U
      Butiker.LanButikk FORMAT "yes/no":U
      Butiker.ButNamn FORMAT "x(20)":U WIDTH 27.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 4.29 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BR-ButKobl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-ButKobl C-Win _STRUCTURED
  QUERY BR-ButKobl NO-LOCK DISPLAY
      ButikkKobling.Butik FORMAT ">>>>>9":U
      ButikkKobling.BrGrpNr FORMAT "zz9":U
      ButikkKobling.TeamNr FORMAT "zzz9":U
      ButikkKobling.TeamTypeId FORMAT "z9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 4.19 FIT-LAST-COLUMN.

DEFINE BROWSE BR-ButTilgang
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-ButTilgang C-Win _STRUCTURED
  QUERY BR-ButTilgang NO-LOCK DISPLAY
      ButikkTilgang.Butik FORMAT ">>>>>9":U
      ButikkTilgang.BrGrpNr FORMAT "zz9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 20 BY 4.19 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BR-Gruppe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Gruppe C-Win _STRUCTURED
  QUERY BR-Gruppe NO-LOCK DISPLAY
      Gruppe.ButikkNr FORMAT ">>>>>9":U WIDTH 5.2
      Gruppe.GruppeNr FORMAT ">9":U WIDTH 6.2
      Gruppe.Navn FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 4.19 FIT-LAST-COLUMN.

DEFINE BROWSE BR-Kasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Kasse C-Win _STRUCTURED
  QUERY BR-Kasse NO-LOCK DISPLAY
      Kasse.ButikkNr FORMAT ">>>>>9":U
      Kasse.KasseNr FORMAT ">>9":U
      Kasse.ElJournal[1] FORMAT "X(8)":U
      Kasse.ElJournal[2] FORMAT "X(8)":U WIDTH 13.2
      Kasse.ElJournalId FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 56 BY 4.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-HentNyCL AT ROW 1.62 COL 135
     B-BytCL AT ROW 1.62 COL 151
     FI-CL AT ROW 1.86 COL 14 COLON-ALIGNED
     FI-CL-Ny AT ROW 1.86 COL 95.2 COLON-ALIGNED HELP
          "Butikknummer"
     FI-Firmanavn AT ROW 2.91 COL 14 COLON-ALIGNED
     FI-FirmanavnNy AT ROW 2.91 COL 95.2 COLON-ALIGNED
     FI-VPIEkst AT ROW 3.95 COL 14 COLON-ALIGNED
     FI-VPIEkstNy AT ROW 3.95 COL 95.2 COLON-ALIGNED
     FI-Butik-Gammel AT ROW 6.05 COL 135.6 COLON-ALIGNED HELP
          "Butikknummer"
     TG-BytAdressinfo AT ROW 6.1 COL 146.2
     BR-Butiker AT ROW 6.48 COL 4
     BR-Kasse AT ROW 6.48 COL 54
     BUTTON-6 AT ROW 7 COL 160
     B-Kontroller AT ROW 7.05 COL 146
     FI-Butik-Ny AT ROW 7.1 COL 135.6 COLON-ALIGNED HELP
          "Butikknummer"
     FI-KjFirmanavn AT ROW 8.33 COL 132.6 COLON-ALIGNED HELP
          "Fullt navn på firma som eier butikken."
     FI-KjButikkNavn AT ROW 9.33 COL 132.6 COLON-ALIGNED HELP
          "Butikkens navn i kjeden."
     FI-KjAdresse1 AT ROW 10.33 COL 132.6 COLON-ALIGNED HELP
          "Adresselinje 1"
     FI-KjAdresse2 AT ROW 11.33 COL 132.6 COLON-ALIGNED HELP
          "Adresselinje 2"
     BR-Gruppe AT ROW 11.86 COL 4
     BR-ButKobl AT ROW 11.86 COL 54.2
     BR-ButTilgang AT ROW 11.86 COL 96.8
     FI-KjPostNr AT ROW 12.33 COL 132.6 COLON-ALIGNED HELP
          "Postnummer"
     FI-KjDagligLeder AT ROW 13.33 COL 132.6 COLON-ALIGNED HELP
          "Navn på daglig leder"
     FI-KjKontaktperson AT ROW 14.33 COL 132.6 COLON-ALIGNED HELP
          "Navn på kontaktperson"
     FI-KjTelefon AT ROW 15.33 COL 132.6 COLON-ALIGNED HELP
          "Telefonnummer til butikk eller kontaktperson."
     FI-BrgrpNavn AT ROW 17.19 COL 149 COLON-ALIGNED
     TG-Akt_rapp AT ROW 17.24 COL 5
     TG-ButLok AT ROW 17.24 COL 27
     TG-KassererValuta AT ROW 17.24 COL 52
     TG-KundeTrans AT ROW 17.24 COL 81
     TG-OvOrdre-Frabutik AT ROW 17.24 COL 111
     TG-ApnSkjema AT ROW 18.14 COL 5
     TG-dags_rap AT ROW 18.14 COL 27
     TG-kas_konter AT ROW 18.14 COL 52
     TG-Lager AT ROW 18.14 COL 81
     TG-OvOrdre-TilButik AT ROW 18.14 COL 111
     FI-Teamnavn AT ROW 18.33 COL 149 COLON-ALIGNED
     TG-ArtLag AT ROW 19.05 COL 5
     TG-etikett AT ROW 19.05 COL 27
     TG-kas_logg AT ROW 19.05 COL 52
     TG-Medlem AT ROW 19.05 COL 81
     TG-ReklamasjonsLinje AT ROW 19.05 COL 111
     TG-ArtLok AT ROW 19.95 COL 5
     TG-Faktura AT ROW 19.95 COL 27
     TG-kas_rap AT ROW 19.95 COL 52
     TG-MedlemBetTrans-betButik AT ROW 19.95 COL 81
     TG-ReklamasjonsLinje-SolgtIBut AT ROW 19.95 COL 111
     TG-AnalyseLogg AT ROW 20.52 COL 148
     TG-BestKasse AT ROW 20.86 COL 5
     TG-Fributik AT ROW 20.86 COL 27
     TG-konto AT ROW 20.86 COL 52
     TG-MedlemBetTrans AT ROW 20.86 COL 81
     TG-StLager AT ROW 20.86 COL 111
     TG-BongHode AT ROW 21.43 COL 148
     TG-BestLevert AT ROW 21.71 COL 5
     TG-Gavekort AT ROW 21.71 COL 27
     TG-kont_mal AT ROW 21.71 COL 52
     TG-MedlemSaldo AT ROW 21.71 COL 81
     TG-StLinje AT ROW 21.71 COL 111
     TG-BongLinje AT ROW 22.33 COL 148
     TG-BestLinje AT ROW 22.62 COL 5
     TG-Gavekort-Frabutikk AT ROW 22.62 COL 27
     TG-Kort_Spes AT ROW 22.62 COL 52
     TG-MedTrans AT ROW 22.62 COL 81
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 176.6 BY 28.52.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     TG-TelleHode AT ROW 22.62 COL 111
     TG-BongLinje-MButikkNr AT ROW 23.24 COL 148
     TG-BestStr AT ROW 23.48 COL 5
     TG-KassererBilag AT ROW 23.48 COL 27
     TG-Kunde AT ROW 23.48 COL 52
     TG-OvBuffer-Fra AT ROW 23.48 COL 81
     TG-TelleLinje AT ROW 23.48 COL 111
     TG-BongLinje-ReturButikk AT ROW 24.14 COL 148
     TG-BokforingsBilag AT ROW 24.33 COL 5
     TG-KassererDag AT ROW 24.33 COL 27
     TG-KundeBetTrans-betButik AT ROW 24.33 COL 52
     TG-OvBuffer-Til AT ROW 24.33 COL 81
     TG-Tilgode AT ROW 24.33 COL 111
     TG-Datasett AT ROW 25 COL 148
     TG-Tilgode-FraButikkNr AT ROW 25.19 COL 111
     TG-KassererKontanter AT ROW 25.24 COL 27
     TG-KundeBetTrans AT ROW 25.24 COL 52
     TG-OvLinje-Frabutik AT ROW 25.24 COL 81
     TG-butikkforsalj AT ROW 25.29 COL 5
     TG-hgrdag AT ROW 25.91 COL 148
     TG-TransLogg AT ROW 26.1 COL 111
     TG-ButikkSelger AT ROW 26.14 COL 5
     TG-KassererOppgj AT ROW 26.14 COL 27
     TG-KundeSaldo AT ROW 26.14 COL 52
     TG-OvLinje-TilButik AT ROW 26.14 COL 81
     TG-timedag AT ROW 26.76 COL 148
     TG-TransLogg-OvButik AT ROW 27 COL 111
     TG-varedag AT ROW 27.62 COL 148
     TG-z_nummer AT ROW 27.86 COL 111
     FILL-IN-20 AT ROW 1.14 COL 17.6 COLON-ALIGNED NO-LABEL
     FILL-IN-21 AT ROW 1.14 COL 102.4 COLON-ALIGNED NO-LABEL
     FI-Butikk-NyTxt AT ROW 5.33 COL 124 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 5.76 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-10 AT ROW 5.76 COL 52 COLON-ALIGNED NO-LABEL
     FILL-IN-8 AT ROW 11.1 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-6 AT ROW 11.1 COL 53 COLON-ALIGNED NO-LABEL
     FILL-IN-7 AT ROW 11.1 COL 95.8 COLON-ALIGNED NO-LABEL
     RECT-58 AT ROW 1.48 COL 2
     RECT-59 AT ROW 1.48 COL 83
     RECT-60 AT ROW 5.62 COL 119
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 176.6 BY 28.52.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Bytt CL og butikknr"
         HEIGHT             = 28.52
         WIDTH              = 176.6
         MAX-HEIGHT         = 35.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 35.33
         VIRTUAL-WIDTH      = 204.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BR-Butiker TG-BytAdressinfo DEFAULT-FRAME */
/* BROWSE-TAB BR-Kasse BR-Butiker DEFAULT-FRAME */
/* BROWSE-TAB BR-Gruppe FI-KjAdresse2 DEFAULT-FRAME */
/* BROWSE-TAB BR-ButKobl BR-Gruppe DEFAULT-FRAME */
/* BROWSE-TAB BR-ButTilgang BR-ButKobl DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-Butik-Gammel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-CL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Firmanavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VPIEkst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-VPIEkstNy IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-20 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-21 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Akt_rapp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-AnalyseLogg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ApnSkjema IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ArtLag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ArtLok IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BestKasse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BestLevert IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BestLinje IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BestStr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BokforingsBilag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BongHode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BongLinje IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BongLinje-MButikkNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-BongLinje-ReturButikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-butikkforsalj IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ButikkSelger IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ButLok IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-dags_rap IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Datasett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-etikett IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Faktura IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Fributik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Gavekort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Gavekort-Frabutikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-hgrdag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KassererBilag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KassererDag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KassererKontanter IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KassererOppgj IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KassererValuta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-kas_konter IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-kas_logg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-kas_rap IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-konto IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-kont_mal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Kort_Spes IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Kunde IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KundeBetTrans IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KundeBetTrans-betButik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KundeSaldo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-KundeTrans IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Lager IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Medlem IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-MedlemBetTrans IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-MedlemBetTrans-betButik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-MedlemSaldo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-MedTrans IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-OvBuffer-Fra IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-OvBuffer-Til IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-OvLinje-Frabutik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-OvLinje-TilButik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-OvOrdre-Frabutik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-OvOrdre-TilButik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ReklamasjonsLinje IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-ReklamasjonsLinje-SolgtIBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-StLager IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-StLinje IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-TelleHode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-TelleLinje IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Tilgode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-Tilgode-FraButikkNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-timedag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-TransLogg IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-TransLogg-OvButik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-varedag IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TG-z_nummer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Butiker
/* Query rebuild information for BROWSE BR-Butiker
     _TblList          = "skotex.Butiker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = skotex.Butiker.Butik
     _FldNameList[2]   = skotex.Butiker.Sentrallager
     _FldNameList[3]   = skotex.Butiker.LanButikk
     _FldNameList[4]   > skotex.Butiker.ButNamn
"Butiker.ButNamn" ? ? "character" ? ? ? ? ? ? no ? no no "27.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-Butiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-ButKobl
/* Query rebuild information for BROWSE BR-ButKobl
     _TblList          = "skotex.ButikkKobling"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.ButikkKobling.Butik = Butiker.Butik
"
     _FldNameList[1]   = skotex.ButikkKobling.Butik
     _FldNameList[2]   = skotex.ButikkKobling.BrGrpNr
     _FldNameList[3]   = skotex.ButikkKobling.TeamNr
     _FldNameList[4]   = skotex.ButikkKobling.TeamTypeId
     _Query            is NOT OPENED
*/  /* BROWSE BR-ButKobl */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-ButTilgang
/* Query rebuild information for BROWSE BR-ButTilgang
     _TblList          = "skotex.ButikkTilgang"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.ButikkTilgang.Butik = Butiker.Butik"
     _FldNameList[1]   = skotex.ButikkTilgang.Butik
     _FldNameList[2]   = skotex.ButikkTilgang.BrGrpNr
     _Query            is OPENED
*/  /* BROWSE BR-ButTilgang */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Gruppe
/* Query rebuild information for BROWSE BR-Gruppe
     _TblList          = "skotex.Gruppe"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.Gruppe.ButikkNr = Butiker.Butik"
     _FldNameList[1]   > skotex.Gruppe.ButikkNr
"Gruppe.ButikkNr" ? ? "integer" ? ? ? ? ? ? no ? no no "5.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > skotex.Gruppe.GruppeNr
"Gruppe.GruppeNr" ? ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = skotex.Gruppe.Navn
     _Query            is OPENED
*/  /* BROWSE BR-Gruppe */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Kasse
/* Query rebuild information for BROWSE BR-Kasse
     _TblList          = "skotex.Kasse"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "skotex.Kasse.ButikkNr = Butiker.Butik"
     _FldNameList[1]   = skotex.Kasse.ButikkNr
     _FldNameList[2]   = skotex.Kasse.KasseNr
     _FldNameList[3]   = skotex.Kasse.ElJournal[1]
     _FldNameList[4]   > skotex.Kasse.ElJournal[2]
"Kasse.ElJournal[2]" ? ? "character" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = skotex.Kasse.ElJournalId
     _Query            is NOT OPENED
*/  /* BROWSE BR-Kasse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bytt CL og butikknr */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bytt CL og butikknr */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BytCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BytCL C-Win
ON CHOOSE OF B-BytCL IN FRAME DEFAULT-FRAME /* Byt CL */
DO:
/*   IF bButiker.Sentrallager THEN */
    RUN KontrollerNyCL.
    IF RETURN-VALUE <> "OK" THEN
        RETURN NO-APPLY.
    RUN LagreNyCL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-HentNyCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-HentNyCL C-Win
ON CHOOSE OF B-HentNyCL IN FRAME DEFAULT-FRAME /* Hent */
DO:
    DEFINE BUFFER bButiker FOR Butiker.
    IF INPUT FI-CL-Ny = 0 THEN
      RETURN NO-APPLY.
    FIND bButiker WHERE bButiker.Butik = INPUT FI-CL-Ny NO-LOCK NO-ERROR.
    IF NOT AVAIL bButiker THEN DO:
        MESSAGE "Butikker finnes ikke i butikkregisteret."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ASSIGN FI-FirmanavnNy:SCREEN-VALUE = bButiker.butnamn.
/*            FI-VPIEkstNy:SCREEN-VALUE   = STRING(bButiker.butik). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Kontroller
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Kontroller C-Win
ON CHOOSE OF B-Kontroller IN FRAME DEFAULT-FRAME /* Kontroller */
DO:
  RUN KontrollerByttBut.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Butiker
&Scoped-define SELF-NAME BR-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-Butiker C-Win
ON VALUE-CHANGED OF BR-Butiker IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BR-Kasse}
  {&OPEN-QUERY-BR-Gruppe}
  {&OPEN-QUERY-BR-ButKobl}
  {&OPEN-QUERY-BR-ButTilgang}
  ASSIGN FI-Butik-Gammel:SCREEN-VALUE = STRING(Butiker.butik).
  RUN DataKontroll.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* Bytt butnr */
DO:
    RUN KontrollerByttBut.
    IF RETURN-VALUE <> "OK" THEN
        RETURN NO-APPLY.
    ASSIGN INPUT FI-Butik-Ny.
    RUN ByttButikkNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-CL-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-CL-Ny C-Win
ON LEAVE OF FI-CL-Ny IN FRAME DEFAULT-FRAME /* Sentrallager */
DO:
    ASSIGN FI-VPIEkstNy:SCREEN-VALUE = IF INPUT FI-CL-Ny > 0 THEN FI-CL-Ny:SCREEN-VALUE ELSE "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN InitVerdier.
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO BROWSE BR-Butiker.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttButikkNr C-Win 
PROCEDURE ByttButikkNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.

    ASSIGN iButik = FI-Butik-Ny.
    FOR EACH akt_rapp WHERE akt_rapp.butik = Butiker.Butik:
        ASSIGN akt_rapp.butik = iButik.
    END.
    FOR EACH ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik:
        ASSIGN ApnSkjema.ButikkNr = iButik. 
    END.
    FOR EACH ArtLag WHERE ArtLag.butik = Butiker.Butik:
        ASSIGN ArtLag.butik = iButik. 
    END.
    FOR EACH ArtLok WHERE ArtLok.ButikkNr = Butiker.Butik:
        ASSIGN ArtLok.ButikkNr = iButik. 
    END.
    FOR EACH BestKasse WHERE BestKasse.Butik = Butiker.Butik:
        ASSIGN BestKasse.Butik = iButik. 
    END.
    FOR EACH BestLevert WHERE BestLevert.Butik = Butiker.Butik:
        ASSIGN BestLevert.Butik = iButik. 
    END.
    FOR EACH BestLinje WHERE BestLinje.Butik = Butiker.Butik:
        ASSIGN BestLinje.Butik = iButik. 
    END.
    FOR EACH BestStr WHERE BestStr.Butik = Butiker.Butik:
        ASSIGN BestStr.Butik = iButik. 
    END.
    FOR EACH BokforingsBilag WHERE BokforingsBilag.ButikkNr = Butiker.Butik:
        ASSIGN BokforingsBilag.ButikkNr = iButik. 
    END.
    
    FOR EACH forsalj WHERE forsalj.forsnr > Butiker.Butik * 1000 AND forsalj.forsnr <= Butiker.Butik * 1999:
        ASSIGN forsalj.forsnr = iButik * 1000 + INT(SUBSTR(STRING(forsalj.Forsnr),LENGTH(STRING(forsalj.Forsnr)) - 2)).
    END.
    FOR EACH butikkforsalj WHERE butikkforsalj.Butik = Butiker.Butik:
        ASSIGN butikkforsalj.Forsnr = iButik * 1000 + INT(SUBSTR(STRING(butikkforsalj.Forsnr),LENGTH(STRING(butikkforsalj.Forsnr)) - 2))
               butikkforsalj.Butik  = iButik.
    END.
    FOR EACH Selger WHERE Selger.Selgernr > Butiker.Butik * 100000 AND Selger.Selgernr <= Butiker.Butik * 199999:
        ASSIGN Selger.SelgerNr = iButik * 100000 + INT(SUBSTR(STRING(Selger.SelgerNr),LENGTH(STRING(Selger.SelgerNr)) - 4)).
    END.
    FOR EACH ButikkSelger WHERE ButikkSelger.ButikkNr = Butiker.Butik:
        ASSIGN ButikkSelger.SelgerNr = iButik * 100000 + INT(SUBSTR(STRING(ButikkSelger.SelgerNr),LENGTH(STRING(ButikkSelger.SelgerNr)) - 4))
               ButikkSelger.ButikkNr = iButik. 
    END.
    FOR EACH ButLok WHERE ButLok.ButikkNr = Butiker.Butik:
        ASSIGN ButLok.ButikkNr = iButik. 
    END.
    
    FOR EACH dags_rap WHERE dags_rap.butikk = Butiker.Butik:
        ASSIGN dags_rap.butikk = iButik. 
    END.
    FOR EACH etikett WHERE etikett.butik = Butiker.Butik:
        ASSIGN etikett.butik = iButik. 
    END.
/*     FOR EACH Faktura WHERE Faktura.ButikkNr = Butiker.Butik: */
/*         ASSIGN Faktura.ButikkNr = iButik.                    */
/*     END.                                                     */
    FOR EACH Fributik WHERE Fributik.Butik = Butiker.Butik:
        ASSIGN Fributik.Butik = iButik. 
    END.
    FOR EACH Gavekort WHERE Gavekort.butnr = Butiker.Butik:
        ASSIGN Gavekort.butnr = iButik. 
    END.
    FOR EACH Gavekort WHERE Gavekort.BruktButNr = Butiker.Butik:
        ASSIGN Gavekort.BruktButNr = iButik. 
    END.
    
    
    FOR EACH KassererBilag WHERE KassererBilag.ButikkNr = Butiker.Butik:
        ASSIGN KassererBilag.ButikkNr  = iButik.  
    END.
    FOR EACH KassererDag WHERE KassererDag.ButikkNr = Butiker.Butik:
        ASSIGN KassererDag.ButikkNr  = iButik.  
    END.
    FOR EACH KassererKontanter WHERE KassererKontanter.ButikkNr = Butiker.Butik:
        ASSIGN KassererKontanter.ButikkNr  = iButik.  
    END.
    FOR EACH KassererOppgj WHERE KassererOppgj.ButikkNr = Butiker.Butik:
        ASSIGN KassererOppgj.ButikkNr  = iButik.  
    END.
    FOR EACH KassererValuta WHERE KassererValuta.ButikkNr = Butiker.Butik:
        ASSIGN KassererValuta.ButikkNr  = iButik.  
    END.
    FOR EACH kas_konter WHERE kas_konter.butikk = Butiker.Butik:
        ASSIGN kas_konter.butikk  = iButik.  
    END.
    FOR EACH kas_logg WHERE kas_logg.butikk = Butiker.Butik:
        ASSIGN kas_logg.butikk  = iButik.  
    END.
    FOR EACH kas_rap WHERE kas_rap.butikk = Butiker.Butik:
        ASSIGN kas_rap.butikk  = iButik.  
    END.
    
    
    FOR EACH konto WHERE konto.butikk = Butiker.Butik:
        ASSIGN konto.butikk = iButik.  
    END.
    FOR EACH kont_mal WHERE kont_mal.butikk = Butiker.Butik:
        ASSIGN kont_mal.butikk = iButik.  
    END.
    FOR EACH Kort_Spes WHERE Kort_Spes.butikk = Butiker.Butik:
        ASSIGN Kort_Spes.butikk = iButik.  
    END.
    FOR EACH Kunde WHERE Kunde.ButikkNr = Butiker.Butik:
        ASSIGN Kunde.ButikkNr = iButik.  
    END.
    FOR EACH KundeBetTrans WHERE KundeBetTrans.betButik = Butiker.Butik:
        ASSIGN KundeBetTrans.betButik = iButik.  
    END.
    FOR EACH KundeBetTrans WHERE KundeBetTrans.Butik = Butiker.Butik:
        ASSIGN KundeBetTrans.Butik = iButik.  
    END.
    FOR EACH KundeSaldo WHERE KundeSaldo.ButikkNr = Butiker.Butik:
        ASSIGN KundeSaldo.ButikkNr = iButik.  
    END.
    FOR EACH KundeTrans WHERE KundeTrans.Butik = Butiker.Butik:
        ASSIGN KundeTrans.Butik = iButik.  
    END.
    FOR EACH Lager WHERE Lager.Butik = Butiker.Butik:
        ASSIGN Lager.Butik = iButik.  
    END.
    FOR EACH Medlem WHERE Medlem.ButikkNr = Butiker.Butik:
        ASSIGN Medlem.ButikkNr = iButik.  
    END.
    FOR EACH MedlemBetTrans WHERE MedlemBetTrans.betButik = Butiker.Butik:
        ASSIGN MedlemBetTrans.betButik = iButik.  
    END.
    FOR EACH MedlemBetTrans WHERE MedlemBetTrans.Butik = Butiker.Butik:
        ASSIGN MedlemBetTrans.Butik = iButik.  
    END.
    FOR EACH MedlemSaldo WHERE MedlemSaldo.ButikkNr = Butiker.Butik:
        ASSIGN MedlemSaldo.ButikkNr = iButik.  
    END.
    FOR EACH MedTrans WHERE MedTrans.Butik = Butiker.Butik:
        ASSIGN MedTrans.Butik = iButik.  
    END.
    FOR EACH OvBuffer WHERE OvBuffer.ButikkNrFra = Butiker.Butik:
        ASSIGN OvBuffer.ButikkNrFra = iButik.  
    END.
    FOR EACH OvBuffer WHERE OvBuffer.ButikkNrTil = Butiker.Butik:
        ASSIGN OvBuffer.ButikkNrTil = iButik.  
    END.
    FOR EACH OvLinje WHERE OvLinje.FraButik = Butiker.Butik:
        ASSIGN OvLinje.FraButik = iButik.  
    END.
    FOR EACH OvLinje WHERE OvLinje.TilButik = Butiker.Butik:
        ASSIGN OvLinje.TilButik = iButik.  
    END.
    FOR EACH OvOrdre WHERE OvOrdre.FraButik = Butiker.Butik:
        ASSIGN OvOrdre.FraButik = iButik.  
    END.
    FOR EACH OvOrdre WHERE OvOrdre.TilButik = Butiker.Butik:
        ASSIGN OvOrdre.TilButik = iButik.  
    END.
    FOR EACH ReklamasjonsLinje WHERE ReklamasjonsLinje.Butik = Butiker.Butik:
        ASSIGN ReklamasjonsLinje.Butik = iButik.  
    END.
    FOR EACH ReklamasjonsLinje WHERE ReklamasjonsLinje.SolgtIButikk = Butiker.Butik:
        ASSIGN ReklamasjonsLinje.SolgtIButikk = iButik.  
    END.
    FOR EACH StLager WHERE StLager.Butik = Butiker.Butik:
        ASSIGN StLager.Butik = iButik.  
    END.
    FOR EACH StLinje WHERE StLinje.Butik = Butiker.Butik:
        ASSIGN StLinje.Butik = iButik.  
    END.
    FOR EACH StLinje WHERE StLinje.StTypeId = "BUTSTAT" AND StLinje.DataObjekt = STRING(Butiker.Butik,"999999"):
        ASSIGN StLinje.DataObjekt = STRING(iButik,"999999").
    END.
    
    /*        TG-StLinje-DataObj:CHECKED = CAN-FIND(FIRST StLinje WHERE StLinje.Butik = Butiker.Butik) */
    
    FOR EACH TelleHode WHERE CAN-DO(TelleHode.ButikkListe,STRING(Butiker.Butik)):
        DO iCount = 1 TO NUM-ENTRIES(TelleHode.ButikkListe):
            IF ENTRY(iCount,TelleHode.ButikkListe) = STRING(Butiker.Butik) THEN
                ASSIGN ENTRY(iCount,TelleHode.ButikkListe) = STRING(iButik).
        END.
    END.
    FOR EACH TelleLinje WHERE TelleLinje.Butik = Butiker.Butik:
        ASSIGN TelleLinje.Butik = iButik.  
    END.
    FOR EACH Tilgode WHERE Tilgode.butnr = Butiker.Butik:
        ASSIGN Tilgode.butnr = iButik.  
    END.
    FOR EACH Tilgode WHERE Tilgode.BruktButNr = Butiker.Butik:
        ASSIGN Tilgode.BruktButNr = iButik.  
    END.
    FOR EACH TransLogg WHERE TransLogg.Butik = Butiker.Butik:
        ASSIGN TransLogg.Butik = iButik.  
    END.
    FOR EACH TransLogg WHERE TransLogg.OvButik = Butiker.Butik:
        ASSIGN TransLogg.OvButik = iButik.  
    END.
    FOR EACH z_nummer WHERE z_nummer.butikk = Butiker.Butik:
        ASSIGN z_nummer.butikk = iButik.  
    END.
    FOR EACH AnalyseLogg WHERE AnalyseLogg.ButikkNr = Butiker.Butik:
        ASSIGN AnalyseLogg.ButikkNr = iButik.  
    END.
    FOR EACH BongHode WHERE BongHode.ButikkNr = Butiker.Butik:
        ASSIGN BongHode.ButikkNr = iButik.  
    END.
    FOR EACH BongLinje WHERE BongLinje.ButikkNr = Butiker.Butik:
        ASSIGN BongLinje.ButikkNr = iButik.  
    END.
    FOR EACH BongLinje WHERE BongLinje.MButikkNr = Butiker.Butik:
        ASSIGN BongLinje.MButikkNr = iButik.  
    END.
    FOR EACH BongLinje WHERE BongLinje.ReturButikk = Butiker.Butik:
        ASSIGN BongLinje.ReturButikk = iButik.  
    END.
    FOR EACH Datasett WHERE Datasett.ButikkNr = Butiker.Butik:
        ASSIGN Datasett.ButikkNr = iButik.  
    END.
    FOR EACH hgrdag WHERE hgrdag.butnr = Butiker.Butik:
        ASSIGN hgrdag.butnr = iButik.  
    END.
    FOR EACH timedag WHERE timedag.butnr = Butiker.Butik:
        ASSIGN timedag.butnr = iButik.  
    END.
    FOR EACH varedag WHERE varedag.butnr = Butiker.Butik:
        ASSIGN varedag.butnr = iButik.  
    END.

    FOR EACH Bruker WHERE Bruker.BrGrpNr = Butiker.Butik:
        ASSIGN Bruker.BrGrpNr = iButik.
    END.
    FOR EACH ButikkKobling WHERE ButikkKobling.Butik = Butiker.Butik:
        ASSIGN ButikkKobling.BrGrpNr = IF ButikkKobling.BrGrpNr = Butiker.Butik THEN iButik ELSE ButikkKobling.BrGrpNr
               ButikkKobling.TeamNr  = IF ButikkKobling.TeamNr = Butiker.Butik THEN iButik ELSE ButikkKobling.TeamNr
               ButikkKobling.Butik   = iButik.
    END.
/*     FOR EACH ButikkKobling WHERE ButikkKobling.Butik = Butiker.Butik: */
/*         ASSIGN ButikkKobling.Butik = iButik.                          */
/*     END.                                                              */
    FOR EACH ButikkTilgang WHERE ButikkTilgang.Butik = Butiker.Butik:
        ASSIGN ButikkTilgang.BrGrpNr = IF ButikkTilgang.BrGrpNr = Butiker.Butik THEN iButik ELSE ButikkTilgang.BrGrpNr
               ButikkTilgang.Butik = iButik.
    END.
    FOR EACH Butikkteam:
        ASSIGN Butikkteam.Beskrivelse = IF Butikkteam.TeamNr = Butiker.butik THEN FI-Teamnavn:SCREEN-VALUE  IN FRAME {&FRAME-NAME} ELSE Butikkteam.Beskrivelse
               Butikkteam.BrGrpNr     = IF Butikkteam.BrGrpNr = Butiker.butik THEN iButik ELSE Butikkteam.BrGrpNr
               Butikkteam.TeamNr      = IF Butikkteam.TeamNr = Butiker.Butik THEN iButik ELSE Butikkteam.TeamNr.
    END.
    FOR EACH BrukerGrp WHERE BrukerGrp.BrGrpNr = Butiker.Butik:
        FOR EACH ProgBrGrp OF Brukergrp:
              ProgBrGrp.BrGrpNr     = iButik.
        END.
        ASSIGN BrukerGrp.BrGrpNr     = iButik
               BrukerGrp.Beskrivelse = FI-BrgrpNavn:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
    END.
    FOR EACH Gruppe WHERE Gruppe.ButikkNr = Butiker.Butik:
        ASSIGN Gruppe.ButikkNr = iButik.
    END.
    FOR EACH Kasse WHERE Kasse.ButikkNr = Butiker.Butik:
        ASSIGN Kasse.ButikkNr = iButik
               Kasse.ElJournal[2] = STRING(iButik)
               ENTRY(1,Kasse.ElJournalId,";")  = STRING(iButik).
    FIND CURRENT Butiker EXCLUSIVE.
    ASSIGN Butiker.Butik = iButik.
    IF TG-BytAdressinfo:CHECKED IN FRAME {&FRAME-NAME} THEN DO:
        FIND Post WHERE Post.Postnr = FI-KjPostNr:SCREEN-VALUE NO-LOCK NO-ERROR.
        ASSIGN
            Butiker.ButNamn     = IF FI-KjButikknavn:SCREEN-VALUE = "" THEN FI-KjFirmanavn:SCREEN-VALUE
                                        ELSE FI-KjButikknavn:SCREEN-VALUE
            Butiker.BuAdr       = FI-KjAdresse1:SCREEN-VALUE
            Butiker.BuKon       = FI-KjKontaktperson:SCREEN-VALUE
            Butiker.BuPadr      = IF AVAIL Post THEN Post.Beskrivelse ELSE ""
            Butiker.BuPonr      = FI-KjPostNr:SCREEN-VALUE
            Butiker.BuTel       = FI-KjTelefon:SCREEN-VALUE
            Butiker.KortNavn    = ""
            Butiker.LevAdresse1 = ""
            Butiker.LevAdresse2 = ""
            Butiker.LevKontakt  = ""
            Butiker.LevMerknad  = ""
            Butiker.LevPostBoks = ""
            Butiker.LevPostNr   = ""
            Butiker.LevTelefon  = ""
            Butiker.OrganisasjonsNr = ""
            .
    END.
    {&OPEN-QUERY-BR-Butiker}
    APPLY "VALUE-CHANGED" TO BROWSE BR-Butiker.
    /* KjedensButikker KjedensButikker.ButikkNr */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DataKontroll C-Win 
PROCEDURE DataKontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
ASSIGN TG-akt_rapp:CHECKED = CAN-FIND(FIRST akt_rapp WHERE akt_rapp.butik = Butiker.Butik)
       TG-ApnSkjema:CHECKED = CAN-FIND(FIRST ApnSkjema WHERE ApnSkjema.ButikkNr = Butiker.Butik)
       TG-ArtLag:CHECKED = CAN-FIND(FIRST ArtLag WHERE ArtLag.butik = Butiker.Butik)
       TG-ArtLok:CHECKED = CAN-FIND(FIRST ArtLok WHERE ArtLok.ButikkNr = Butiker.Butik)
       TG-BestKasse:CHECKED = CAN-FIND(FIRST BestKasse WHERE BestKasse.Butik = Butiker.Butik)
       TG-BestLevert:CHECKED = CAN-FIND(FIRST BestLevert WHERE BestLevert.Butik = Butiker.Butik)
       TG-BestLinje:CHECKED = CAN-FIND(FIRST BestLinje WHERE BestLinje.Butik = Butiker.Butik)
       TG-BestStr:CHECKED = CAN-FIND(FIRST BestStr WHERE BestStr.Butik = Butiker.Butik)
       TG-BokforingsBilag:CHECKED = CAN-FIND(FIRST BokforingsBilag WHERE BokforingsBilag.ButikkNr = Butiker.Butik)
/* Butiker Butiker.Butik */
       TG-butikkforsalj:CHECKED = CAN-FIND(FIRST butikkforsalj WHERE butikkforsalj.Butik = Butiker.Butik)
/* ButikkKobling ButikkKobling.Butik */
       TG-ButikkSelger:CHECKED = CAN-FIND(FIRST ButikkSelger WHERE ButikkSelger.ButikkNr = Butiker.Butik)
       TG-ButLok:CHECKED = CAN-FIND(FIRST ButLok WHERE ButLok.ButikkNr = Butiker.Butik)
/* ButikkTilgang ButikkTilgang.Butik */
       TG-dags_rap:CHECKED = CAN-FIND(FIRST dags_rap WHERE dags_rap.butikk = Butiker.Butik)
       TG-etikett:CHECKED = CAN-FIND(FIRST etikett WHERE etikett.butik = Butiker.Butik)
/*        TG-Faktura:CHECKED = CAN-FIND(FIRST Faktura WHERE Faktura.ButikkNr = Butiker.Butik) */
       TG-Fributik:CHECKED = CAN-FIND(FIRST Fributik WHERE Fributik.Butik = Butiker.Butik)
       TG-Gavekort:CHECKED = CAN-FIND(FIRST Gavekort WHERE Gavekort.butnr = Butiker.Butik)
       TG-Gavekort-Frabutikk:CHECKED = CAN-FIND(FIRST Gavekort WHERE Gavekort.BruktButNr = Butiker.Butik)
/* Gruppe Gruppe.ButikkNr */
/* Kasse Kasse.ButikkNr */
    TG-KassererBilag:CHECKED = CAN-FIND(FIRST KassererBilag WHERE KassererBilag.ButikkNr = Butiker.Butik)
    TG-KassererDag:CHECKED = CAN-FIND(FIRST KassererDag WHERE KassererDag.ButikkNr = Butiker.Butik)
    TG-KassererKontanter:CHECKED = CAN-FIND(FIRST KassererKontanter WHERE KassererKontanter.ButikkNr = Butiker.Butik)
       TG-KassererOppgj:CHECKED = CAN-FIND(FIRST KassererOppgj WHERE KassererOppgj.ButikkNr = Butiker.Butik)
    TG-KassererValuta:CHECKED = CAN-FIND(FIRST KassererValuta WHERE KassererValuta.ButikkNr = Butiker.Butik)
    TG-kas_konter:CHECKED = CAN-FIND(FIRST kas_konter WHERE kas_konter.butikk = Butiker.Butik)
    TG-kas_logg:CHECKED = CAN-FIND(FIRST kas_logg WHERE kas_logg.butikk = Butiker.Butik)
       TG-kas_rap:CHECKED = CAN-FIND(FIRST kas_rap WHERE kas_rap.butikk = Butiker.Butik).
/* KjedensButikker KjedensButikker.ButikkNr */
ASSIGN TG-konto:CHECKED = CAN-FIND(FIRST konto WHERE konto.butikk = Butiker.Butik)
    TG-kont_mal:CHECKED = CAN-FIND(FIRST kont_mal WHERE kont_mal.butikk = Butiker.Butik)
       TG-Kort_Spes:CHECKED = CAN-FIND(FIRST Kort_Spes WHERE Kort_Spes.butikk = Butiker.Butik)
       TG-Kunde:CHECKED = CAN-FIND(FIRST Kunde WHERE Kunde.ButikkNr = Butiker.Butik)
    TG-KundeBetTrans-betButik:CHECKED = CAN-FIND(FIRST KundeBetTrans WHERE KundeBetTrans.betButik = Butiker.Butik)
       TG-KundeBetTrans:CHECKED = CAN-FIND(FIRST KundeBetTrans WHERE KundeBetTrans.Butik = Butiker.Butik)
       TG-KundeSaldo:CHECKED = CAN-FIND(FIRST KundeSaldo WHERE KundeSaldo.ButikkNr = Butiker.Butik)
       TG-KundeTrans:CHECKED = CAN-FIND(FIRST KundeTrans WHERE KundeTrans.Butik = Butiker.Butik)
       TG-Lager:CHECKED = CAN-FIND(FIRST Lager WHERE Lager.Butik = Butiker.Butik)
       TG-Medlem:CHECKED = CAN-FIND(FIRST Medlem WHERE Medlem.ButikkNr = Butiker.Butik)
    TG-MedlemBetTrans-betButik:CHECKED = CAN-FIND(FIRST MedlemBetTrans WHERE MedlemBetTrans.betButik = Butiker.Butik)
    TG-MedlemBetTrans:CHECKED = CAN-FIND(FIRST MedlemBetTrans WHERE MedlemBetTrans.Butik = Butiker.Butik)
    TG-MedlemSaldo:CHECKED = CAN-FIND(FIRST MedlemSaldo WHERE MedlemSaldo.ButikkNr = Butiker.Butik)
    TG-MedTrans:CHECKED = CAN-FIND(FIRST MedTrans WHERE MedTrans.Butik = Butiker.Butik)
       TG-OvBuffer-Fra:CHECKED = CAN-FIND(FIRST OvBuffer WHERE OvBuffer.ButikkNrFra = Butiker.Butik)
       TG-OvBuffer-Til:CHECKED = CAN-FIND(FIRST OvBuffer WHERE OvBuffer.ButikkNrTil = Butiker.Butik)
    TG-OvLinje-FraButik:CHECKED = CAN-FIND(FIRST OvLinje WHERE OvLinje.FraButik = Butiker.Butik)
    TG-OvLinje-TilButik:CHECKED = CAN-FIND(FIRST OvLinje WHERE OvLinje.TilButik = Butiker.Butik)
    TG-OvOrdre-FraButik:CHECKED = CAN-FIND(FIRST OvOrdre WHERE OvOrdre.FraButik = Butiker.Butik)
    TG-OvOrdre-TilButik:CHECKED = CAN-FIND(FIRST OvOrdre WHERE OvOrdre.TilButik = Butiker.Butik)
       TG-ReklamasjonsLinje:CHECKED = CAN-FIND(FIRST ReklamasjonsLinje WHERE ReklamasjonsLinje.Butik = Butiker.Butik)
    TG-ReklamasjonsLinje-SolgtIBut:CHECKED = CAN-FIND(FIRST ReklamasjonsLinje WHERE ReklamasjonsLinje.SolgtIButikk = Butiker.Butik)
       TG-StLager:CHECKED = CAN-FIND(FIRST StLager WHERE StLager.Butik = Butiker.Butik)
       TG-StLinje:CHECKED = CAN-FIND(FIRST StLinje WHERE StLinje.Butik = Butiker.Butik)
/*        TG-StLinje-DataObj:CHECKED = CAN-FIND(FIRST StLinje WHERE StLinje.Butik = Butiker.Butik) */
    TG-TelleHode:CHECKED = CAN-FIND(FIRST TelleHode WHERE CAN-DO(TelleHode.ButikkListe,STRING(Butiker.Butik)))
       TG-TelleLinje:CHECKED = CAN-FIND(FIRST TelleLinje WHERE TelleLinje.Butik = Butiker.Butik)
    TG-Tilgode:CHECKED = CAN-FIND(FIRST Tilgode WHERE Tilgode.butnr = Butiker.Butik)
    TG-Tilgode-FraButikkNr:CHECKED = CAN-FIND(FIRST Tilgode WHERE Tilgode.BruktButNr = Butiker.Butik)
       TG-TransLogg:CHECKED = CAN-FIND(FIRST TransLogg WHERE TransLogg.Butik = Butiker.Butik)
    TG-TransLogg-OvButik:CHECKED = CAN-FIND(FIRST TransLogg WHERE TransLogg.OvButik = Butiker.Butik)
       TG-z_nummer:CHECKED = CAN-FIND(FIRST z_nummer WHERE z_nummer.butikk = Butiker.Butik)
       TG-AnalyseLogg:CHECKED = CAN-FIND(FIRST AnalyseLogg WHERE AnalyseLogg.ButikkNr = Butiker.Butik)
       TG-BongHode:CHECKED = CAN-FIND(FIRST BongHode WHERE BongHode.ButikkNr = Butiker.Butik)
       TG-BongLinje:CHECKED = CAN-FIND(FIRST BongLinje WHERE BongLinje.ButikkNr = Butiker.Butik)
    TG-BongLinje-MButikkNr:CHECKED = CAN-FIND(FIRST BongLinje WHERE BongLinje.MButikkNr = Butiker.Butik)
    TG-BongLinje-ReturButikk:CHECKED = CAN-FIND(FIRST BongLinje WHERE BongLinje.ReturButikk = Butiker.Butik)
       TG-Datasett:CHECKED = CAN-FIND(FIRST Datasett WHERE Datasett.ButikkNr = Butiker.Butik)
       TG-hgrdag:CHECKED = CAN-FIND(FIRST hgrdag WHERE hgrdag.butnr = Butiker.Butik)
       TG-timedag:CHECKED = CAN-FIND(FIRST timedag WHERE timedag.butnr = Butiker.Butik)
       TG-varedag:CHECKED = CAN-FIND(FIRST varedag WHERE varedag.butnr = Butiker.Butik).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FI-CL FI-CL-Ny FI-Firmanavn FI-FirmanavnNy FI-VPIEkst FI-VPIEkstNy 
          FI-Butik-Gammel TG-BytAdressinfo FI-Butik-Ny FI-KjFirmanavn 
          FI-KjButikkNavn FI-KjAdresse1 FI-KjAdresse2 FI-KjPostNr 
          FI-KjDagligLeder FI-KjKontaktperson FI-KjTelefon FI-BrgrpNavn 
          TG-Akt_rapp TG-ButLok TG-KassererValuta TG-KundeTrans 
          TG-OvOrdre-Frabutik TG-ApnSkjema TG-dags_rap TG-kas_konter TG-Lager 
          TG-OvOrdre-TilButik FI-Teamnavn TG-ArtLag TG-etikett TG-kas_logg 
          TG-Medlem TG-ReklamasjonsLinje TG-ArtLok TG-Faktura TG-kas_rap 
          TG-MedlemBetTrans-betButik TG-ReklamasjonsLinje-SolgtIBut 
          TG-AnalyseLogg TG-BestKasse TG-Fributik TG-konto TG-MedlemBetTrans 
          TG-StLager TG-BongHode TG-BestLevert TG-Gavekort TG-kont_mal 
          TG-MedlemSaldo TG-StLinje TG-BongLinje TG-BestLinje 
          TG-Gavekort-Frabutikk TG-Kort_Spes TG-MedTrans TG-TelleHode 
          TG-BongLinje-MButikkNr TG-BestStr TG-KassererBilag TG-Kunde 
          TG-OvBuffer-Fra TG-TelleLinje TG-BongLinje-ReturButikk 
          TG-BokforingsBilag TG-KassererDag TG-KundeBetTrans-betButik 
          TG-OvBuffer-Til TG-Tilgode TG-Datasett TG-Tilgode-FraButikkNr 
          TG-KassererKontanter TG-KundeBetTrans TG-OvLinje-Frabutik 
          TG-butikkforsalj TG-hgrdag TG-TransLogg TG-ButikkSelger 
          TG-KassererOppgj TG-KundeSaldo TG-OvLinje-TilButik TG-timedag 
          TG-TransLogg-OvButik TG-varedag TG-z_nummer FILL-IN-20 FILL-IN-21 
          FI-Butikk-NyTxt FILL-IN-9 FILL-IN-10 FILL-IN-8 FILL-IN-6 FILL-IN-7 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-58 RECT-59 RECT-60 B-HentNyCL B-BytCL FI-CL-Ny FI-FirmanavnNy 
         TG-BytAdressinfo BR-Butiker BR-Kasse BUTTON-6 B-Kontroller FI-Butik-Ny 
         FI-KjFirmanavn FI-KjButikkNavn FI-KjAdresse1 FI-KjAdresse2 BR-Gruppe 
         BR-ButKobl BR-ButTilgang FI-KjPostNr FI-KjDagligLeder 
         FI-KjKontaktperson FI-KjTelefon FI-BrgrpNavn FI-Teamnavn 
         FI-Butikk-NyTxt FILL-IN-9 FILL-IN-10 FILL-IN-8 FILL-IN-6 FILL-IN-7 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVerdier C-Win 
PROCEDURE InitVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND syspara WHERE SysPara.SysHId = 5 AND  /* CL */
                     SysPara.SysGr  = 1 AND
                     SysPara.ParaNr = 1 NO-LOCK.
  ASSIGN FI-CL = INT(SysPara.Parameter1).

  FIND syspara WHERE SysPara.SysHId = 1 AND /* firmanavn  */
                     SysPara.SysGr  = 1 AND
                     SysPara.ParaNr = 100 NO-LOCK.
  ASSIGN FI-Firmanavn = SysPara.Parameter1.
  
/*   FIND ekstvpifil WHERE EkstVPILevNr = 1. */
/*   ASSIGN FI-VPIEkst = STRING(VPIEkst).    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerByttBut C-Win 
PROCEDURE KontrollerByttBut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bButiker FOR Butiker.
  DO WITH FRAME {&FRAME-NAME}:
      FIND bButiker WHERE bButiker.butik = INPUT FI-Butik-Ny NO-LOCK NO-ERROR.
      IF AVAIL bButiker THEN DO:
          MESSAGE "Butikken finnes allerede."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      FIND KjedensButikker WHERE KjedensButikker.ButikkNr = INPUT FI-Butik-Ny NO-LOCK NO-ERROR.
      IF NOT AVAIL KjedensButikker THEN DO:
          MESSAGE "Ønsket butikk finnes ikke blant Kjedens butikker." SKIP
                   "skall den skapas"
              VIEW-AS ALERT-BOX BUTTONS YES-NO-CANCEL UPDATE lOK AS LOGICAL.
          MESSAGE "avbryt"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ASSIGN FI-KjAdresse1:SCREEN-VALUE      = KjedensButikker.Adresse1
             FI-KjAdresse2:SCREEN-VALUE      = KjedensButikker.Adresse2
             FI-KjButikkNavn:SCREEN-VALUE    = KjedensButikker.ButikkNavn
             FI-KjDagligLeder:SCREEN-VALUE   = KjedensButikker.DagligLeder
             FI-KjFirmanavn:SCREEN-VALUE     = KjedensButikker.Firmanavn
             FI-KjKontaktperson:SCREEN-VALUE = KjedensButikker.Kontaktperson
             FI-KjPostNr:SCREEN-VALUE        = KjedensButikker.PostNr
             FI-KjTelefon:SCREEN-VALUE       = KjedensButikker.Telefon
             FI-BrgrpNavn:SCREEN-VALUE       = KjedensButikker.ButikkNavn
             FI-Teamnavn:SCREEN-VALUE        = KjedensButikker.ButikkNavn
             .
  END.
  RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerNyCL C-Win 
PROCEDURE KontrollerNyCL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bButiker FOR Butiker.
  DEFINE BUFFER cButiker FOR Butiker.
  DO WITH FRAME {&FRAME-NAME}:
      IF FI-CL-Ny = FI-CL THEN DO:
          MESSAGE "Registrert sentrallager = systemets sentrallager"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT FI-CL-Ny = 0 THEN DO:
          MESSAGE "Angi nummer for sentrallager."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.

      FIND bButiker WHERE bButiker.butik = INPUT FI-CL-Ny NO-LOCK NO-ERROR.
      IF NOT AVAIL bButiker THEN DO:
          MESSAGE "Angivet sentrallager finnes ikke i butikkregisteret."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF bButiker.LanButikk = FALSE THEN DO:
          MESSAGE "Angiven butikk ikke registrert som LAN-butikk"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF INPUT FI-FirmanavnNy = "" THEN DO:
          MESSAGE "Registrer firmanavn."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      FOR EACH bButiker:
          ASSIGN bButiker.sentrallager = FALSE.
      END.
  END.
  RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreNyCL C-Win 
PROCEDURE LagreNyCL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bButiker FOR Butiker.
  DO TRANSACTION:
      DO WITH FRAME {&FRAME-NAME}:
          FIND bButiker WHERE bButiker.butik = INPUT FI-Cl-Ny.
          ASSIGN bButiker.sentrallager = TRUE.
          FIND syspara WHERE SysPara.SysHId = 5 AND  /* CL */
                             SysPara.SysGr  = 1 AND
                             SysPara.ParaNr = 1.
          ASSIGN SysPara.Parameter1 = STRING(INPUT FI-Cl-Ny)
                 FI-CL              = INPUT FI-Cl-Ny.

          FIND syspara WHERE SysPara.SysHId = 1 AND /* firmanavn  */
                             SysPara.SysGr  = 1 AND
                             SysPara.ParaNr = 100.
          ASSIGN SysPara.Parameter1 = FI-FirmanavnNy:SCREEN-VALUE
                 FI-Firmanavn       = FI-FirmanavnNy:SCREEN-VALUE.

/*           FIND ekstvpifil WHERE EkstVPILevNr = 1. */
          ASSIGN 
/*                  ekstvpifil.VPIEkst = FI-VPIEkst:SCREEN-VALUE */
/*                  FI-VPIEkst         = FI-VPIEkst:SCREEN-VALUE */
                 FI-CL:SCREEN-VALUE        = STRING(FI-CL)
                 FI-Firmanavn:SCREEN-VALUE = FI-Firmanavn
/*                  FI-VPIEkst:SCREEN-VALUE   = FI-VPIEkst */
                 FI-CL-Ny                  = 0
                 FI-CL-Ny:SCREEN-VALUE     = STRING(FI-CL-Ny)
                 FI-FirmanavnNy:SCREEN-VALUE  = ""
              .
/*                  FI-VPIEkstNy:SCREEN-VALUE = "". */
          FOR EACH EkstVPIFil WHERE EkstVPIFil.EkstVPILevNr = 1. 
              EkstVPIFil.VPIEkst = STRING(FI-CL).
          END.
      END.
            {&OPEN-QUERY-BR-Butiker}
            APPLY "VALUE-CHANGED" TO BROWSE BR-Butiker.
        END.
      END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

