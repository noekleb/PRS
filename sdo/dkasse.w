&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
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

DEFINE VARIABLE cLayoutListItemPairs AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Kasse

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BrukerId ButikkNr EDato ETid GruppeNr KasseNr LayoutNr Navn RegistrertAv~
 RegistrertDato RegistrertTid Aktiv ElJournal1 ElJournal2 Kvittering1~
 Kvittering2 Utskriftskopi1 Utskriftskopi2 DagsOpgj1 DagsOpgj2 KassererOpgj1~
 KassererOpgj2 DagsOppgj DagsOppgjAktiv DagsOppgjKatalog DagsOppgjKonv~
 ElJournalAktiv ElJournalId ElJournalKatalog ElJournalKonv~
 KassererOppgjAktiv KassererOppgjId KassererOppgjKatalog KassererOppgjKonv~
 KvitteringAktiv KvitteringId KvitteringKatalog KvitteringKonv~
 UtskriftskopiAktiv UtskriftsKopiId UtskriftskopiKatalog UTskriftskopiKonv~
 DagsOppgjId DagsOppgjOperand ElJournalOperand KassererOppgjOperand~
 KvitteringOperand UtskriftsKopiOperand DagsOppgjBehandle DagsOppgjInnles~
 ElJournalBehandle ElJournalInnles KassererOppgjBehandle KassererOppgjInnles~
 KvitteringBehandle KvitteringInnles UtskriftskopiBehandle~
 UtskriftskopiInnles ModellNr FakturaKopi FakturaLayout Fakturaskriver
&Scoped-define ENABLED-FIELDS-IN-Kasse BrukerId ButikkNr EDato ETid ~
GruppeNr KasseNr LayoutNr Navn RegistrertAv RegistrertDato RegistrertTid ~
Aktiv ElJournal1 ElJournal2 Kvittering1 Kvittering2 Utskriftskopi1 ~
Utskriftskopi2 DagsOpgj1 DagsOpgj2 KassererOpgj1 KassererOpgj2 DagsOppgj ~
DagsOppgjAktiv DagsOppgjKatalog DagsOppgjKonv ElJournalAktiv ElJournalId ~
ElJournalKatalog ElJournalKonv KassererOppgjAktiv KassererOppgjId ~
KassererOppgjKatalog KassererOppgjKonv KvitteringAktiv KvitteringId ~
KvitteringKatalog KvitteringKonv UtskriftskopiAktiv UtskriftsKopiId ~
UtskriftskopiKatalog UTskriftskopiKonv DagsOppgjId DagsOppgjOperand ~
ElJournalOperand KassererOppgjOperand KvitteringOperand ~
UtskriftsKopiOperand DagsOppgjBehandle DagsOppgjInnles ElJournalBehandle ~
ElJournalInnles KassererOppgjBehandle KassererOppgjInnles ~
KvitteringBehandle KvitteringInnles UtskriftskopiBehandle ~
UtskriftskopiInnles ModellNr FakturaKopi FakturaLayout Fakturaskriver 
&Scoped-Define DATA-FIELDS  BrukerId fuModellNavn ButikkNr fuKortNavn EDato ETid GruppeNr KasseNr~
 LayoutNr Navn RegistrertAv RegistrertDato RegistrertTid fuGruppeNavn Aktiv~
 ElJournal1 ElJournal2 Kvittering1 Kvittering2 Utskriftskopi1 Utskriftskopi2~
 DagsOpgj1 DagsOpgj2 KassererOpgj1 KassererOpgj2 DagsOppgj DagsOppgjAktiv~
 DagsOppgjKatalog DagsOppgjKonv ElJournalAktiv ElJournalId ElJournalKatalog~
 ElJournalKonv KassererOppgjAktiv KassererOppgjId KassererOppgjKatalog~
 KassererOppgjKonv KvitteringAktiv KvitteringId KvitteringKatalog~
 KvitteringKonv UtskriftskopiAktiv UtskriftsKopiId UtskriftskopiKatalog~
 UTskriftskopiKonv DagsOppgjId DagsOppgjOperand ElJournalOperand~
 KassererOppgjOperand KvitteringOperand UtskriftsKopiOperand~
 DagsOppgjBehandle DagsOppgjInnles ElJournalBehandle ElJournalInnles~
 KassererOppgjBehandle KassererOppgjInnles KvitteringBehandle~
 KvitteringInnles UtskriftskopiBehandle UtskriftskopiInnles ModellNr~
 FakturaKopi FakturaLayout Fakturaskriver
&Scoped-define DATA-FIELDS-IN-Kasse BrukerId ButikkNr EDato ETid GruppeNr ~
KasseNr LayoutNr Navn RegistrertAv RegistrertDato RegistrertTid Aktiv ~
ElJournal1 ElJournal2 Kvittering1 Kvittering2 Utskriftskopi1 Utskriftskopi2 ~
DagsOpgj1 DagsOpgj2 KassererOpgj1 KassererOpgj2 DagsOppgj DagsOppgjAktiv ~
DagsOppgjKatalog DagsOppgjKonv ElJournalAktiv ElJournalId ElJournalKatalog ~
ElJournalKonv KassererOppgjAktiv KassererOppgjId KassererOppgjKatalog ~
KassererOppgjKonv KvitteringAktiv KvitteringId KvitteringKatalog ~
KvitteringKonv UtskriftskopiAktiv UtskriftsKopiId UtskriftskopiKatalog ~
UTskriftskopiKonv DagsOppgjId DagsOppgjOperand ElJournalOperand ~
KassererOppgjOperand KvitteringOperand UtskriftsKopiOperand ~
DagsOppgjBehandle DagsOppgjInnles ElJournalBehandle ElJournalInnles ~
KassererOppgjBehandle KassererOppgjInnles KvitteringBehandle ~
KvitteringInnles UtskriftskopiBehandle UtskriftskopiInnles ModellNr ~
FakturaKopi FakturaLayout Fakturaskriver 
&Scoped-Define MANDATORY-FIELDS  ButikkNr GruppeNr KasseNr DagsOppgj ElJournalId KassererOppgjId~
 KvitteringId UtskriftsKopiId DagsOppgjId
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.ElJournal1 = Kasse.ElJournal[1]~
  rowObject.ElJournal2 = Kasse.ElJournal[2]~
  rowObject.Kvittering1 = Kasse.Kvittering[1]~
  rowObject.Kvittering2 = Kasse.Kvittering[2]~
  rowObject.Utskriftskopi1 = Kasse.Utskriftskopi[1]~
  rowObject.Utskriftskopi2 = Kasse.Utskriftskopi[2]~
  rowObject.DagsOpgj1 = Kasse.DagsOpgj[1]~
  rowObject.DagsOpgj2 = Kasse.DagsOpgj[2]~
  rowObject.KassererOpgj1 = Kasse.KassererOpgj[1]~
  rowObject.KassererOpgj2 = Kasse.KassererOpgj[2]
&Scoped-Define DATA-FIELD-DEFS "dkasse.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Kasse NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Kasse NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Kasse
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Kasse


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addRow dTables  _DB-REQUIRED
FUNCTION addRow RETURNS CHARACTER
  ( INPUT pcViewColList AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetGruppeNavn dTables  _DB-REQUIRED
FUNCTION GetGruppeNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetKortNavn dTables  _DB-REQUIRED
FUNCTION GetKortNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLayoutListItemPairs dTables  _DB-REQUIRED
FUNCTION getLayoutListItemPairs RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ModellNavn dTables  _DB-REQUIRED
FUNCTION ModellNavn RETURNS CHARACTER
  ( INPUT piModellNr AS int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD submitRow dTables  _DB-REQUIRED
FUNCTION submitRow RETURNS LOGICAL
  ( INPUT pcRowIdent AS CHARACTER,
    INPUT pcValueList AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Kasse SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "skotex.Kasse"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.Kasse.BrukerId
"BrukerId" "BrukerId" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[2]   > "_<CALC>"
"ModellNavn(RowObject.ModellNr)" "fuModellNavn" "Modell" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[3]   > skotex.Kasse.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[4]   > "_<CALC>"
"GetKortNavn()" "fuKortNavn" "KortNavn" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[5]   > skotex.Kasse.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[6]   > skotex.Kasse.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[7]   > skotex.Kasse.GruppeNr
"GruppeNr" "GruppeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.4 yes ""
     _FldNameList[8]   > skotex.Kasse.KasseNr
"KasseNr" "KasseNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 8 yes ""
     _FldNameList[9]   > skotex.Kasse.LayoutNr
"LayoutNr" "LayoutNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[10]   > skotex.Kasse.Navn
"Navn" "Navn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[11]   > skotex.Kasse.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[12]   > skotex.Kasse.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[13]   > skotex.Kasse.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[14]   > "_<CALC>"
"GetGruppeNavn()" "fuGruppeNavn" "Navn" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[15]   > skotex.Kasse.Aktiv
"Aktiv" "Aktiv" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[16]   > skotex.Kasse.ElJournal[1]
"ElJournal[1]" "ElJournal1" ? ? "character" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[17]   > skotex.Kasse.ElJournal[2]
"ElJournal[2]" "ElJournal2" ? ? "character" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[18]   > skotex.Kasse.Kvittering[1]
"Kvittering[1]" "Kvittering1" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[19]   > skotex.Kasse.Kvittering[2]
"Kvittering[2]" "Kvittering2" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[20]   > skotex.Kasse.Utskriftskopi[1]
"Utskriftskopi[1]" "Utskriftskopi1" ? ? "character" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[21]   > skotex.Kasse.Utskriftskopi[2]
"Utskriftskopi[2]" "Utskriftskopi2" ? ? "character" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[22]   > skotex.Kasse.DagsOpgj[1]
"DagsOpgj[1]" "DagsOpgj1" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[23]   > skotex.Kasse.DagsOpgj[2]
"DagsOpgj[2]" "DagsOpgj2" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[24]   > skotex.Kasse.KassererOpgj[1]
"KassererOpgj[1]" "KassererOpgj1" ? ? "character" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[25]   > skotex.Kasse.KassererOpgj[2]
"KassererOpgj[2]" "KassererOpgj2" "Ekstent" ? "character" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[26]   > skotex.Kasse.DagsOppgj
"DagsOppgj" "DagsOppgj" ? "X(50)" "character" ? ? ? ? ? ? yes ? yes 50 yes ""
     _FldNameList[27]   > skotex.Kasse.DagsOppgjAktiv
"DagsOppgjAktiv" "DagsOppgjAktiv" "DagsOppgj" "*~~/" "logical" ? ? ? ? ? ? yes ? no 10.6 yes ""
     _FldNameList[28]   > skotex.Kasse.DagsOppgjKatalog
"DagsOppgjKatalog" "DagsOppgjKatalog" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[29]   > skotex.Kasse.DagsOppgjKonv
"DagsOppgjKonv" "DagsOppgjKonv" ? ? "logical" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[30]   > skotex.Kasse.ElJournalAktiv
"ElJournalAktiv" "ElJournalAktiv" "ElJournal" "*~~/" "logical" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[31]   > skotex.Kasse.ElJournalId
"ElJournalId" "ElJournalId" ? "X(50)" "character" ? ? ? ? ? ? yes ? yes 50 yes ""
     _FldNameList[32]   > skotex.Kasse.ElJournalKatalog
"ElJournalKatalog" "ElJournalKatalog" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[33]   > skotex.Kasse.ElJournalKonv
"ElJournalKonv" "ElJournalKonv" ? ? "logical" ? ? ? ? ? ? yes ? no 13.6 yes ""
     _FldNameList[34]   > skotex.Kasse.KassererOppgjAktiv
"KassererOppgjAktiv" "KassererOppgjAktiv" "KassererOppgj" "*~~/" "logical" ? ? ? ? ? ? yes ? no 13.8 yes ""
     _FldNameList[35]   > skotex.Kasse.KassererOppgjId
"KassererOppgjId" "KassererOppgjId" ? "X(50)" "character" ? ? ? ? ? ? yes ? yes 50 yes ""
     _FldNameList[36]   > skotex.Kasse.KassererOppgjKatalog
"KassererOppgjKatalog" "KassererOppgjKatalog" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[37]   > skotex.Kasse.KassererOppgjKonv
"KassererOppgjKonv" "KassererOppgjKonv" ? ? "logical" ? ? ? ? ? ? yes ? no 18.8 yes ""
     _FldNameList[38]   > skotex.Kasse.KvitteringAktiv
"KvitteringAktiv" "KvitteringAktiv" "Kvittering" "*~~/" "logical" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[39]   > skotex.Kasse.KvitteringId
"KvitteringId" "KvitteringId" ? "X(50)" "character" ? ? ? ? ? ? yes ? yes 50 yes ""
     _FldNameList[40]   > skotex.Kasse.KvitteringKatalog
"KvitteringKatalog" "KvitteringKatalog" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[41]   > skotex.Kasse.KvitteringKonv
"KvitteringKonv" "KvitteringKonv" ? ? "logical" ? ? ? ? ? ? yes ? no 13.8 yes ""
     _FldNameList[42]   > skotex.Kasse.UtskriftskopiAktiv
"UtskriftskopiAktiv" "UtskriftskopiAktiv" "Utskriftskopi" "*~~/" "logical" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[43]   > skotex.Kasse.UtskriftsKopiId
"UtskriftsKopiId" "UtskriftsKopiId" ? "X(50)" "character" ? ? ? ? ? ? yes ? yes 50 yes ""
     _FldNameList[44]   > skotex.Kasse.UtskriftskopiKatalog
"UtskriftskopiKatalog" "UtskriftskopiKatalog" ? "X(50)" "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[45]   > skotex.Kasse.UTskriftskopiKonv
"UTskriftskopiKonv" "UTskriftskopiKonv" ? ? "logical" ? ? ? ? ? ? yes ? no 17.4 yes ""
     _FldNameList[46]   > skotex.Kasse.DagsOppgjId
"DagsOppgjId" "DagsOppgjId" ? "X(50)" "character" ? ? ? ? ? ? yes ? yes 50 yes ""
     _FldNameList[47]   > skotex.Kasse.DagsOppgjOperand
"DagsOppgjOperand" "DagsOppgjOperand" ? ? "integer" ? ? ? ? ? ? yes ? no 18.8 yes ""
     _FldNameList[48]   > skotex.Kasse.ElJournalOperand
"ElJournalOperand" "ElJournalOperand" ? ? "integer" ? ? ? ? ? ? yes ? no 16.8 yes ""
     _FldNameList[49]   > skotex.Kasse.KassererOppgjOperand
"KassererOppgjOperand" "KassererOppgjOperand" ? ? "integer" ? ? ? ? ? ? yes ? no 22 yes ""
     _FldNameList[50]   > skotex.Kasse.KvitteringOperand
"KvitteringOperand" "KvitteringOperand" ? ? "integer" ? ? ? ? ? ? yes ? no 17 yes ""
     _FldNameList[51]   > skotex.Kasse.UtskriftsKopiOperand
"UtskriftsKopiOperand" "UtskriftsKopiOperand" ? ? "integer" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[52]   > skotex.Kasse.DagsOppgjBehandle
"DagsOppgjBehandle" "DagsOppgjBehandle" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[53]   > skotex.Kasse.DagsOppgjInnles
"DagsOppgjInnles" "DagsOppgjInnles" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[54]   > skotex.Kasse.ElJournalBehandle
"ElJournalBehandle" "ElJournalBehandle" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[55]   > skotex.Kasse.ElJournalInnles
"ElJournalInnles" "ElJournalInnles" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[56]   > skotex.Kasse.KassererOppgjBehandle
"KassererOppgjBehandle" "KassererOppgjBehandle" ? ? "character" ? ? ? ? ? ? yes ? no 22.8 yes ""
     _FldNameList[57]   > skotex.Kasse.KassererOppgjInnles
"KassererOppgjInnles" "KassererOppgjInnles" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[58]   > skotex.Kasse.KvitteringBehandle
"KvitteringBehandle" "KvitteringBehandle" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[59]   > skotex.Kasse.KvitteringInnles
"KvitteringInnles" "KvitteringInnles" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[60]   > skotex.Kasse.UtskriftskopiBehandle
"UtskriftskopiBehandle" "UtskriftskopiBehandle" ? ? "character" ? ? ? ? ? ? yes ? no 20.6 yes ""
     _FldNameList[61]   > skotex.Kasse.UtskriftskopiInnles
"UtskriftskopiInnles" "UtskriftskopiInnles" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[62]   > skotex.Kasse.ModellNr
"ModellNr" "ModellNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes ""
     _FldNameList[63]   > skotex.Kasse.FakturaKopi
"FakturaKopi" "FakturaKopi" ? ? "integer" ? ? ? ? ? ? yes ? no 4.2 yes ""
     _FldNameList[64]   > skotex.Kasse.FakturaLayout
"FakturaLayout" "FakturaLayout" ? ? "integer" ? ? ? ? ? ? yes ? no 12.8 yes ""
     _FldNameList[65]   > skotex.Kasse.Fakturaskriver
"Fakturaskriver" "Fakturaskriver" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.fuGruppeNavn = (GetGruppeNavn())
         rowObject.fuKortNavn = (GetKortNavn())
         rowObject.fuModellNavn = (ModellNavn(RowObject.ModellNr))
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN InitListItemPairs.
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitListItemPairs dTables  _DB-REQUIRED
PROCEDURE InitListItemPairs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME FRAME-1:
        ASSIGN cLayoutListItemPairs = "Ukjent,0".
        FOR EACH SysPara WHERE SysPara.SysHId = 19 AND
                               SysPara.SysGr  = 12 NO-LOCK: 
            ASSIGN cLayoutListItemPairs = cLayoutListItemPairs + (IF cLayoutListItemPairs <> "" THEN "," ELSE "") + 
                                      SysPara.Beskrivelse + "," + STRING(SysPara.ParaNr).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KasseNrValidate dTables  _DB-REQUIRED
PROCEDURE KasseNrValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piKasseNr LIKE Kasse.KasseNr NO-UNDO.

  IF piKasseNr = 0 THEN
      RETURN "Ugyldig kassanummer. Det må være forskjellig fra 0.".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ModellListe dTables  _DB-REQUIRED
PROCEDURE ModellListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcListe AS CHAR NO-UNDO.

  FOR EACH SysPara NO-LOCK WHERE
      SysPara.SysHId = 1 AND
      SysPara.SysGr  = 10:

      ASSIGN
          pcListe = pcListe +
                    (IF pcListe = ""
                       THEN ""
                       ELSE ",") + 
                    SysPara.Parameter1 + "," + string(SysPara.ParaNr)
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostTransactionValidate dTables  _DB-REQUIRED
PROCEDURE PostTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Kontroller ved ny/kopier post */
  FOR EACH RowObject WHERE
      CAN-DO("A,C",RowObject.RowMod):

      IF RowObject.KasseNr = 0 THEN
          RETURN.
      DO TRANSACTION:
          IF NOT CAN-FIND(Gruppe WHERE
                          Gruppe.ButikkNr = RowObject.ButikkNr AND
                          Gruppe.GruppeNr = 1) THEN
          DO:
              CREATE Gruppe.
              ASSIGN
                  Gruppe.ButikkNr = RowObject.ButikkNr
                  Gruppe.GruppeNr = 1
                  .
          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreTransactionValidate dTables  _DB-REQUIRED
PROCEDURE PreTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Kontroller ved ny/kopier post */
  FOR EACH RowObject WHERE
      CAN-DO("A,C",RowObject.RowMod):

      IF RowObject.KasseNr = 0 THEN
          RETURN "Ugyldig kassenummer. Kassanummer må være forskjellig fra 0.".

      /* ID Maske feltet må fylles ut. */
      /*
      IF RowObject.IdMaske = "" THEN
          RETURN "Ugyldig IdMaske. Sett inn '*' hvis maske ikke skal benyttes.".
      */

      /* Kontrollerer dubletter */
      IF CAN-FIND(Kasse WHERE
                  Kasse.ButikkNr = RowObject.ButikkNr AND
                  Kasse.GruppeNr = RowObject.GruppeNr AND
                  Kasse.KasseNr  = RowObject.KasseNr) THEN
          RETURN "Kasse " + string(RowObject.KasseNr) +  " er allerede registrert.".
  END.
  /* Kontroll ved sletting av poster. */
  FOR EACH RowObjUpd WHERE
      CAN-DO("D",RowObject.RowMod):

      /* Sjekker om det finnes kvitteringer */
      IF CAN-FIND(FIRST BongHode WHERE
                  BongHode.ButikkNr = RowObject.ButikkNr AND
                  BongHode.GruppeNr = RowObject.GruppeNr AND
                  BongHode.KasseNr  = RowObject.KasseNr) THEN
          RETURN "Det finnes kvitteringer på denne kassen - " + string(RowObject.KasseNr) +  ". " +
                 "Posten kan ikke slettes.".

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addRow dTables  _DB-REQUIRED
FUNCTION addRow RETURNS CHARACTER
  ( INPUT pcViewColList AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE cReturnString AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iPosition     AS INTEGER   NO-UNDO.

  /* Store the position of the "OrderNum" field in the list.  If the  */
  /* field does not exist in the list then zero (0) is stored.        */

  ASSIGN iPosition = LOOKUP('ModellNr':U, pcViewColList).

  /* Store the result of the "default" addRow() function so that we   */
  /* can modify the initial value.                                    */

  ASSIGN cReturnString = SUPER(pcViewColList).

  /* Now that the list has been filled with the default initial       */
  /* values for all of the fields we will modify the field we are     */
  /* interested in.                                                   */

  IF iPosition <> 0 THEN
    ENTRY(iPosition + 1, cReturnString, CHR(1)) = '1'.

  RETURN cReturnString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetGruppeNavn dTables  _DB-REQUIRED
FUNCTION GetGruppeNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Gruppe NO-LOCK WHERE
      Gruppe.ButikkNr = RowObject.ButikkNr AND
      Gruppe.GruppeNr = RowObject.GruppeNr NO-ERROR.

  IF AVAILABLE Gruppe THEN
      RETURN Gruppe.Navn.
  ELSE
      RETURN "*".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetKortNavn dTables  _DB-REQUIRED
FUNCTION GetKortNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = RowObject.Butik NO-ERROR.

  IF AVAILABLE Butiker THEN
      RETURN Butiker.KortNavn.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLayoutListItemPairs dTables  _DB-REQUIRED
FUNCTION getLayoutListItemPairs RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cLayoutListItemPairs.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ModellNavn dTables  _DB-REQUIRED
FUNCTION ModellNavn RETURNS CHARACTER
  ( INPUT piModellNr AS int ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 10 piModellNr pcTekst}

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION submitRow dTables  _DB-REQUIRED
FUNCTION submitRow RETURNS LOGICAL
  ( INPUT pcRowIdent AS CHARACTER,
    INPUT pcValueList AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  /* Search the list of fields that have been modified and if the     */
  /* field we care about is not in the list add it to the list and    */
  /* put our default value in.  This needs to be done because the     */
  /* is currently no way to put the initial value into the physical   */
  /* RowObject temp table record itself.                              */

  DEFINE VARIABLE cNewRecord    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cSource       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hUpdateSource AS HANDLE    NO-UNDO.

  {get UpdateSource cSource}.
  hUpdateSource = WIDGET-HANDLE(cSource).
  cNewRecord = DYNAMIC-FUNCTION('getNewRecord':U IN hUpdateSource).

  IF cNewRecord = "ADD" THEN
    IF LOOKUP('ModellNr', pcValueList, CHR(1)) = 0 THEN
        ASSIGN pcValueList = pcValueList + CHR(1) + 'ModellNr'
                                         + CHR(1) + '1'.

  RETURN SUPER(pcRowIdent, pcValueList).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

