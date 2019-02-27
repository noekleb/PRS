&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          vpi              PROGRESS
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

DEF VAR piCl        AS INT  NO-UNDO.
DEF VAR cTekst      AS CHAR NO-UNDO.
DEF VAR iStrTypeId  AS INT  NO-UNDO.
DEF VAR cStrTypeId  AS CHAR NO-UNDO.
DEF VAR bHk         AS LOG  NO-UNDO.

{syspara.i 5   1 1 piCl       INT}
{syspara.i 50 15 1 cStrTypeId}

DEF BUFFER clButiker FOR Butiker.

DEF VAR lArtikkelNr AS DEC  NO-UNDO.
DEF VAR bAuto       AS LOG  NO-UNDO.
DEF VAR cGenEan     AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES VPIArtBas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Alder AnbefaltPris AnonseArtikkel Anv-Id ArtikkelNr BehKode Beskr~
 BildeIKasse BildNr BongTekst BrukerID DivInfo1 DivInfo2 DivInfo3 DivInfo4~
 DivInfo5 DivInfo6 DivInfo7 DivInfo8 DivInfo9 DivInfo10 DivInfo11 DivInfo12~
 DivInfo13 DivInfo14 DivInfo15 DivInfo16 DivInfo17 DivInfo18 DivInfo19~
 DivInfo20 EDato EkstVPILevNr ETid Etikett Farg Foder-Id Hg HkStyrt HKVareId~
 IKasse Inner-Id KjentPaHK Klack Kommentar KundeRabatt Lager Last-Id~
 LevDato1 LevDato2 LevFargKod LevKod LevNr LokPris LopNr MatKod Notat~
 OLLager OPris Ov-Id Pakke Pakkenr ProdNr ProvKod RabKod RegistrertAv~
 RegistrertDato RegistrertTid SalgsEnhet SaSong SattPaKampanje Slit-Id~
 Storrelser StrTypeID Tilv-Land ValKod VareNr Vg VgKat VMId HandKode~
 HovedModellFarge LokArtikkelNr ModellFarge Oppdatert PrisGrpNr~
 SentralBestilling forhRab%1 forhRab%2 KatalogPris1 KatalogPris2 KjedeVare~
 LevDato3 LevDato4 Linjemerknad suppRab%1 suppRab%2 VPIBildeKode VPIDato
&Scoped-define ENABLED-FIELDS-IN-VPIArtBas Alder AnbefaltPris ~
AnonseArtikkel Anv-Id ArtikkelNr BehKode Beskr BildeIKasse BildNr BongTekst ~
BrukerID DivInfo1 DivInfo2 DivInfo3 DivInfo4 DivInfo5 DivInfo6 DivInfo7 ~
DivInfo8 DivInfo9 DivInfo10 DivInfo11 DivInfo12 DivInfo13 DivInfo14 ~
DivInfo15 DivInfo16 DivInfo17 DivInfo18 DivInfo19 DivInfo20 EDato ~
EkstVPILevNr ETid Etikett Farg Foder-Id Hg HkStyrt HKVareId IKasse Inner-Id ~
KjentPaHK Klack Kommentar KundeRabatt Lager Last-Id LevDato1 LevDato2 ~
LevFargKod LevKod LevNr LokPris LopNr MatKod Notat OLLager OPris Ov-Id ~
Pakke Pakkenr ProdNr ProvKod RabKod RegistrertAv RegistrertDato ~
RegistrertTid SalgsEnhet SaSong SattPaKampanje Slit-Id Storrelser StrTypeID ~
Tilv-Land ValKod VareNr Vg VgKat VMId HandKode HovedModellFarge ~
LokArtikkelNr ModellFarge Oppdatert PrisGrpNr SentralBestilling forhRab%1 ~
forhRab%2 KatalogPris1 KatalogPris2 KjedeVare LevDato3 LevDato4 ~
Linjemerknad suppRab%1 suppRab%2 VPIBildeKode VPIDato 
&Scoped-Define DATA-FIELDS  Alder FinnesLokalt AnbefaltPris fuGetInnkjopsPris AnonseArtikkel Anv-Id~
 ArtikkelNr fuGetPris BehKode Beskr BildeIKasse fuLevNamn BildNr BongTekst~
 BrukerID DivInfo1 DivInfo2 DivInfo3 DivInfo4 fuEndretInfo DivInfo5 DivInfo6~
 DivInfo7 DivInfo8 DivInfo9 DivInfo10 DivInfo11 DivInfo12 DivInfo13~
 DivInfo14 DivInfo15 DivInfo16 DivInfo17 DivInfo18 DivInfo19 DivInfo20 EDato~
 EkstVPILevNr ETid Etikett Farg Foder-Id Hg HkStyrt HKVareId IKasse Inner-Id~
 KjentPaHK Klack Kommentar KundeRabatt Lager Last-Id LevDato1 LevDato2~
 LevFargKod LevKod LevNr LokPris LopNr MatKod Notat OLLager OPris Ov-Id~
 Pakke Pakkenr ProdNr ProvKod RabKod RegistrertAv RegistrertDato~
 RegistrertTid SalgsEnhet SaSong SattPaKampanje Slit-Id Storrelser StrTypeID~
 Tilv-Land ValKod VareNr Vg VgKat VMId HandKode HovedModellFarge~
 LokArtikkelNr ModellFarge Oppdatert PrisGrpNr SentralBestilling forhRab%1~
 forhRab%2 KatalogPris1 KatalogPris2 KjedeVare LevDato3 LevDato4~
 Linjemerknad suppRab%1 suppRab%2 VPIBildeKode VPIDato
&Scoped-define DATA-FIELDS-IN-VPIArtBas Alder AnbefaltPris AnonseArtikkel ~
Anv-Id ArtikkelNr BehKode Beskr BildeIKasse BildNr BongTekst BrukerID ~
DivInfo1 DivInfo2 DivInfo3 DivInfo4 DivInfo5 DivInfo6 DivInfo7 DivInfo8 ~
DivInfo9 DivInfo10 DivInfo11 DivInfo12 DivInfo13 DivInfo14 DivInfo15 ~
DivInfo16 DivInfo17 DivInfo18 DivInfo19 DivInfo20 EDato EkstVPILevNr ETid ~
Etikett Farg Foder-Id Hg HkStyrt HKVareId IKasse Inner-Id KjentPaHK Klack ~
Kommentar KundeRabatt Lager Last-Id LevDato1 LevDato2 LevFargKod LevKod ~
LevNr LokPris LopNr MatKod Notat OLLager OPris Ov-Id Pakke Pakkenr ProdNr ~
ProvKod RabKod RegistrertAv RegistrertDato RegistrertTid SalgsEnhet SaSong ~
SattPaKampanje Slit-Id Storrelser StrTypeID Tilv-Land ValKod VareNr Vg ~
VgKat VMId HandKode HovedModellFarge LokArtikkelNr ModellFarge Oppdatert ~
PrisGrpNr SentralBestilling forhRab%1 forhRab%2 KatalogPris1 KatalogPris2 ~
KjedeVare LevDato3 LevDato4 Linjemerknad suppRab%1 suppRab%2 VPIBildeKode ~
VPIDato 
&Scoped-Define MANDATORY-FIELDS  ModellFarge PrisGrpNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.DivInfo1 = VPIArtBas.DivInfo[1]~
  rowObject.DivInfo2 = VPIArtBas.DivInfo[2]~
  rowObject.DivInfo3 = VPIArtBas.DivInfo[3]~
  rowObject.DivInfo4 = VPIArtBas.DivInfo[4]~
  rowObject.DivInfo5 = VPIArtBas.DivInfo[5]~
  rowObject.DivInfo6 = VPIArtBas.DivInfo[6]~
  rowObject.DivInfo7 = VPIArtBas.DivInfo[7]~
  rowObject.DivInfo8 = VPIArtBas.DivInfo[8]~
  rowObject.DivInfo9 = VPIArtBas.DivInfo[9]~
  rowObject.DivInfo10 = VPIArtBas.DivInfo[10]~
  rowObject.DivInfo11 = VPIArtBas.DivInfo[11]~
  rowObject.DivInfo12 = VPIArtBas.DivInfo[12]~
  rowObject.DivInfo13 = VPIArtBas.DivInfo[13]~
  rowObject.DivInfo14 = VPIArtBas.DivInfo[14]~
  rowObject.DivInfo15 = VPIArtBas.DivInfo[15]~
  rowObject.DivInfo16 = VPIArtBas.DivInfo[16]~
  rowObject.DivInfo17 = VPIArtBas.DivInfo[17]~
  rowObject.DivInfo18 = VPIArtBas.DivInfo[18]~
  rowObject.DivInfo19 = VPIArtBas.DivInfo[19]~
  rowObject.DivInfo20 = VPIArtBas.DivInfo[20]~
  rowObject.forhRab%1 = VPIArtBas.forhRab%[1]~
  rowObject.forhRab%2 = VPIArtBas.forhRab%[2]~
  rowObject.KatalogPris1 = VPIArtBas.KatalogPris[1]~
  rowObject.KatalogPris2 = VPIArtBas.KatalogPris[2]~
  rowObject.suppRab%1 = VPIArtBas.suppRab%[1]~
  rowObject.suppRab%2 = VPIArtBas.suppRab%[2]
&Scoped-Define DATA-FIELD-DEFS "dvpiartbas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH VPIArtBas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH VPIArtBas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main VPIArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main VPIArtBas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ByttElement dTables  _DB-REQUIRED
FUNCTION ByttElement RETURNS CHARACTER
  ( INPUT ipSkjerm AS CHAR,
    INPUT ipElement AS INT,
    INPUT ipNyttElement AS CHAR,
    INPUT ipDelimiter AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndretInfo dTables  _DB-REQUIRED
FUNCTION EndretInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FinnesLokalt dTables  _DB-REQUIRED
FUNCTION FinnesLokalt RETURNS LOGICAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk dTables  _DB-REQUIRED
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuGetInnkjopsPris dTables  _DB-REQUIRED
FUNCTION fuGetInnkjopsPris RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuGetLevNavn dTables  _DB-REQUIRED
FUNCTION fuGetLevNavn RETURNS CHARACTER
  ( INPUT piLevNr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuGetPris dTables  _DB-REQUIRED
FUNCTION fuGetPris RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuVg dTables  _DB-REQUIRED
FUNCTION fuVg RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuVgBeskr dTables  _DB-REQUIRED
FUNCTION fuVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLopeNr dTables  _DB-REQUIRED
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      VPIArtBas SCROLLING.
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
{soksdo.i}

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
     _TblList          = "Vpi.VPIArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", OUTER"
     _FldNameList[1]   > Vpi.VPIArtBas.Alder
"Alder" "Alder" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[2]   > "_<CALC>"
"FinnesLokalt(dec(RowObject.VareNr))" "FinnesLokalt" "Koblet" "yes/no" "Logical" ? ? ? ? ? ? no ? no 6 no ?
     _FldNameList[3]   > Vpi.VPIArtBas.AnbefaltPris
"AnbefaltPris" "AnbefaltPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[4]   > "_<CALC>"
"fuGetInnkjopsPris()" "fuGetInnkjopsPris" "InnkjopsPris" "->>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 15.6 no ?
     _FldNameList[5]   > Vpi.VPIArtBas.AnonseArtikkel
"AnonseArtikkel" "AnonseArtikkel" ? ? "logical" ? ? ? ? ? ? yes ? no 2.8 yes ""
     _FldNameList[6]   > Vpi.VPIArtBas.Anv-Id
"Anv-Id" "Anv-Id" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[7]   > Vpi.VPIArtBas.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[8]   > "_<CALC>"
"fuGetPris()" "fuGetPris" "Pris" "->>>,>>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[9]   > Vpi.VPIArtBas.BehKode
"BehKode" "BehKode" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[10]   > Vpi.VPIArtBas.Beskr
"Beskr" "Beskr" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[11]   > Vpi.VPIArtBas.BildeIKasse
"BildeIKasse" "BildeIKasse" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[12]   > "_<CALC>"
"fuGetLevNavn(RowObject.LevNr)" "fuLevNamn" "Leverandør" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no ?
     _FldNameList[13]   > Vpi.VPIArtBas.BildNr
"BildNr" "BildNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[14]   > Vpi.VPIArtBas.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[15]   > Vpi.VPIArtBas.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[16]   > Vpi.VPIArtBas.DivInfo[1]
"DivInfo[1]" "DivInfo1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[17]   > Vpi.VPIArtBas.DivInfo[2]
"DivInfo[2]" "DivInfo2" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[18]   > Vpi.VPIArtBas.DivInfo[3]
"DivInfo[3]" "DivInfo3" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[19]   > Vpi.VPIArtBas.DivInfo[4]
"DivInfo[4]" "DivInfo4" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[20]   > "_<CALC>"
"EndretInfo()" "fuEndretInfo" "EndretInfo" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[21]   > Vpi.VPIArtBas.DivInfo[5]
"DivInfo[5]" "DivInfo5" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[22]   > Vpi.VPIArtBas.DivInfo[6]
"DivInfo[6]" "DivInfo6" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[23]   > Vpi.VPIArtBas.DivInfo[7]
"DivInfo[7]" "DivInfo7" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[24]   > Vpi.VPIArtBas.DivInfo[8]
"DivInfo[8]" "DivInfo8" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[25]   > Vpi.VPIArtBas.DivInfo[9]
"DivInfo[9]" "DivInfo9" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[26]   > Vpi.VPIArtBas.DivInfo[10]
"DivInfo[10]" "DivInfo10" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[27]   > Vpi.VPIArtBas.DivInfo[11]
"DivInfo[11]" "DivInfo11" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[28]   > Vpi.VPIArtBas.DivInfo[12]
"DivInfo[12]" "DivInfo12" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[29]   > Vpi.VPIArtBas.DivInfo[13]
"DivInfo[13]" "DivInfo13" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[30]   > Vpi.VPIArtBas.DivInfo[14]
"DivInfo[14]" "DivInfo14" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[31]   > Vpi.VPIArtBas.DivInfo[15]
"DivInfo[15]" "DivInfo15" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[32]   > Vpi.VPIArtBas.DivInfo[16]
"DivInfo[16]" "DivInfo16" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[33]   > Vpi.VPIArtBas.DivInfo[17]
"DivInfo[17]" "DivInfo17" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[34]   > Vpi.VPIArtBas.DivInfo[18]
"DivInfo[18]" "DivInfo18" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[35]   > Vpi.VPIArtBas.DivInfo[19]
"DivInfo[19]" "DivInfo19" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[36]   > Vpi.VPIArtBas.DivInfo[20]
"DivInfo[20]" "DivInfo20" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[37]   > Vpi.VPIArtBas.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[38]   > Vpi.VPIArtBas.EkstVPILevNr
"EkstVPILevNr" "EkstVPILevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[39]   > Vpi.VPIArtBas.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[40]   > Vpi.VPIArtBas.Etikett
"Etikett" "Etikett" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[41]   > Vpi.VPIArtBas.Farg
"Farg" "Farg" ? ? "integer" ? ? ? ? ? ? yes ? no 4.2 yes ""
     _FldNameList[42]   > Vpi.VPIArtBas.Foder-Id
"Foder-Id" "Foder-Id" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[43]   > Vpi.VPIArtBas.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[44]   > Vpi.VPIArtBas.HkStyrt
"HkStyrt" "HkStyrt" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[45]   > Vpi.VPIArtBas.HKVareId
"HKVareId" "HKVareId" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[46]   > Vpi.VPIArtBas.IKasse
"IKasse" "IKasse" ? ? "logical" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[47]   > Vpi.VPIArtBas.Inner-Id
"Inner-Id" "Inner-Id" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[48]   > Vpi.VPIArtBas.KjentPaHK
"KjentPaHK" "KjentPaHK" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 yes ""
     _FldNameList[49]   > Vpi.VPIArtBas.Klack
"Klack" "Klack" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ""
     _FldNameList[50]   > Vpi.VPIArtBas.Kommentar
"Kommentar" "Kommentar" ? ? "character" ? ? ? ? ? ? yes ? no 64 yes ""
     _FldNameList[51]   > Vpi.VPIArtBas.KundeRabatt
"KundeRabatt" "KundeRabatt" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[52]   > Vpi.VPIArtBas.Lager
"Lager" "Lager" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[53]   > Vpi.VPIArtBas.Last-Id
"Last-Id" "Last-Id" ? ? "integer" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[54]   > Vpi.VPIArtBas.LevDato1
"LevDato1" "LevDato1" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 yes ""
     _FldNameList[55]   > Vpi.VPIArtBas.LevDato2
"LevDato2" "LevDato2" ? ? "date" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[56]   > Vpi.VPIArtBas.LevFargKod
"LevFargKod" "LevFargKod" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[57]   > Vpi.VPIArtBas.LevKod
"LevKod" "LevKod" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[58]   > Vpi.VPIArtBas.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 yes ""
     _FldNameList[59]   > Vpi.VPIArtBas.LokPris
"LokPris" "LokPris" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[60]   > Vpi.VPIArtBas.LopNr
"LopNr" "LopNr" ? "zzzzz9" "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[61]   > Vpi.VPIArtBas.MatKod
"MatKod" "MatKod" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[62]   > Vpi.VPIArtBas.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[63]   > Vpi.VPIArtBas.OLLager
"OLLager" "OLLager" ? ? "logical" ? ? ? ? ? ? yes ? no 5.6 yes ""
     _FldNameList[64]   > Vpi.VPIArtBas.OPris
"OPris" "OPris" ? ? "logical" ? ? ? ? ? ? yes ? no 5 yes ""
     _FldNameList[65]   > Vpi.VPIArtBas.Ov-Id
"Ov-Id" "Ov-Id" ? ">>9" "integer" ? ? ? ? ? ? yes ? no 5.2 yes ""
     _FldNameList[66]   > Vpi.VPIArtBas.Pakke
"Pakke" "Pakke" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[67]   > Vpi.VPIArtBas.Pakkenr
"Pakkenr" "Pakkenr" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[68]   > Vpi.VPIArtBas.ProdNr
"ProdNr" "ProdNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[69]   > Vpi.VPIArtBas.ProvKod
"ProvKod" "ProvKod" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[70]   > Vpi.VPIArtBas.RabKod
"RabKod" "RabKod" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[71]   > Vpi.VPIArtBas.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[72]   > Vpi.VPIArtBas.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[73]   > Vpi.VPIArtBas.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[74]   > Vpi.VPIArtBas.SalgsEnhet
"SalgsEnhet" "SalgsEnhet" ? ? "character" ? ? ? ? ? ? yes ? no 5.6 yes ""
     _FldNameList[75]   > Vpi.VPIArtBas.SaSong
"SaSong" "SaSong" ? ">>9" "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[76]   > Vpi.VPIArtBas.SattPaKampanje
"SattPaKampanje" "SattPaKampanje" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[77]   > Vpi.VPIArtBas.Slit-Id
"Slit-Id" "Slit-Id" ? ">>9" "integer" ? ? ? ? ? ? yes ? no 5.2 yes ""
     _FldNameList[78]   > Vpi.VPIArtBas.Storrelser
"Storrelser" "Storrelser" ? ? "logical" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[79]   > Vpi.VPIArtBas.StrTypeID
"StrTypeID" "StrTypeID" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no 7.4 yes ""
     _FldNameList[80]   > Vpi.VPIArtBas.Tilv-Land
"Tilv-Land" "Tilv-Land" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[81]   > Vpi.VPIArtBas.ValKod
"ValKod" "ValKod" ? ? "character" ? ? ? ? ? ? yes ? no 3 yes ""
     _FldNameList[82]   > Vpi.VPIArtBas.VareNr
"VareNr" "VareNr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[83]   > Vpi.VPIArtBas.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[84]   > Vpi.VPIArtBas.VgKat
"VgKat" "VgKat" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[85]   > Vpi.VPIArtBas.VMId
"VMId" "VMId" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[86]   > Vpi.VPIArtBas.HandKode
"HandKode" "HandKode" ? ? "integer" ? ? ? ? ? ? yes ? no 3 yes ""
     _FldNameList[87]   > Vpi.VPIArtBas.HovedModellFarge
"HovedModellFarge" "HovedModellFarge" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[88]   > Vpi.VPIArtBas.LokArtikkelNr
"LokArtikkelNr" "LokArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[89]   > Vpi.VPIArtBas.ModellFarge
"ModellFarge" "ModellFarge" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[90]   > Vpi.VPIArtBas.Oppdatert
"Oppdatert" "Oppdatert" ? ? "logical" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[91]   > Vpi.VPIArtBas.PrisGrpNr
"PrisGrpNr" "PrisGrpNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.8 yes ""
     _FldNameList[92]   > Vpi.VPIArtBas.SentralBestilling
"SentralBestilling" "SentralBestilling" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 yes ""
     _FldNameList[93]   > Vpi.VPIArtBas.forhRab%[1]
"forhRab%[1]" "forhRab%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[94]   > Vpi.VPIArtBas.forhRab%[2]
"forhRab%[2]" "forhRab%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[95]   > Vpi.VPIArtBas.KatalogPris[1]
"KatalogPris[1]" "KatalogPris1" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[96]   > Vpi.VPIArtBas.KatalogPris[2]
"KatalogPris[2]" "KatalogPris2" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[97]   > Vpi.VPIArtBas.KjedeVare
"KjedeVare" "KjedeVare" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[98]   > Vpi.VPIArtBas.LevDato3
"LevDato3" "LevDato3" ? ? "date" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[99]   > Vpi.VPIArtBas.LevDato4
"LevDato4" "LevDato4" ? ? "date" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[100]   > Vpi.VPIArtBas.Linjemerknad
"Linjemerknad" "Linjemerknad" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[101]   > Vpi.VPIArtBas.suppRab%[1]
"suppRab%[1]" "suppRab%1" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[102]   > Vpi.VPIArtBas.suppRab%[2]
"suppRab%[2]" "suppRab%2" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[103]   > Vpi.VPIArtBas.VPIBildeKode
"VPIBildeKode" "VPIBildeKode" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[104]   > Vpi.VPIArtBas.VPIDato
"VPIDato" "VPIDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _Design-Parent    is WINDOW dTables @ ( 1.24 , 2.6 )
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
         rowObject.FinnesLokalt = (FinnesLokalt(dec(RowObject.VareNr)))
         rowObject.fuEndretInfo = (EndretInfo())
         rowObject.fuGetInnkjopsPris = (fuGetInnkjopsPris())
         rowObject.fuGetPris = (fuGetPris())
         rowObject.fuLevNamn = (fuGetLevNavn(RowObject.LevNr))
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dummy dTables  _DB-REQUIRED
PROCEDURE Dummy :
/*------------------------------------------------------------------------------
  Purpose:     Dummy rutine for nullstilling av error flagg.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeOppdaterte dTables  _DB-REQUIRED
PROCEDURE GetAlleIkkeOppdaterte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      

 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAnt           AS INT  NO-UNDO.
  DEF INPUT  PARAMETER piEkstVpiLevNr  AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe         AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore          AS LOG  NO-UNDO.

  DEF BUFFER bufVPIArtBas FOR VPIArtBas.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH bufVPIArtBas NO-LOCK WHERE
      bufVPIArtBas.EkstVpiLevNr    = piEkstVpiLevNr AND
      bufVPIArtBas.Oppdatert       = FALSE 
      BY bufVPIArtBas.EkstVpiLevNr
      BY bufVPIArtBas.VareNr:

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(bufVPIArtBas.VareNr).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAnt THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetArtikkelNr dTables  _DB-REQUIRED
PROCEDURE GetArtikkelNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  /* Returneres 0 i plArtikkelNr, er artikkelen ikke koblet. */
  RUN GetArtikkelNr IN h_dvpiartbas (piEkstVpiLevNr, pcVareNr, OUTPUT plArtikkelNr).
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcVareNr       AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  FIND VPIArtBas NO-LOCK WHERE
      VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
      VPIArtBas.VareNr       = pcVareNr NO-ERROR.
  IF NOT AVAILABLE vpiArtBas THEN
      plArtikkelNr = 0.
  ELSE DO:
      plArtikkelNr = dec(VPIArtBas.VareNr) /*VPIArtBas.ArtikkelNr*/.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetPrevVPIArtBas dTables  _DB-REQUIRED
PROCEDURE GetPrevVPIArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT        PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT-OUTPUT PARAMETER pcVareNr       AS CHAR NO-UNDO.

  IF pcVareNr = "" THEN
      RETURN.

  FIND VPIArtBas NO-LOCK WHERE
      VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
      VPIArtBas.VareNr       = pcVareNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN
      FIND PREV VPIArtBas WHERE
      VPIArtBas.EkstVPILEvNr = piEkstVPILevNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN
      pcVareNr = VPIArtBas.VareNr.
  ELSE 
      FIND FIRST VPIArtBas WHERE
      VPIArtBas.EkstVPILEvNr = piEkstVPILevNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN
      pcVareNr = VPIArtBas.VareNr.
  ELSE
      pcVareNr = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetRecidArtBas dTables  _DB-REQUIRED
PROCEDURE GetRecidArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    RUN GetRecidArtBas  (INPUT  piEkstVPILevNr,
                       INPUT  pcValgteArtikler, 
                       OUTPUT prRecid).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT   NO-UNDO.
  DEF INPUT  PARAMETER pcVareNr       AS CHAR  NO-UNDO.
  DEF OUTPUT PARAMETER prRecid        AS RECID NO-UNDO.

  FIND VPIArtBas NO-LOCK WHERE
      VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
      VPIArtBas.VareNr       = pcVareNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = dec(VPIArtBas.VareNr) NO-ERROR.

  IF NOT AVAILABLE ArtBas THEN
      prRecid = ?.
  ELSE DO:
      prRecid = RECID(ArtBas).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {syspara.i 2 4 8 cGenEan} 
   {syspara.i 1 1 18 cTekst}
   IF CAN-DO("1,yes,true,Ja",cTekst) THEN
       bHk = TRUE.
   ELSE
       bHk = FALSE.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
     INPUT TRUE /* LOGICAL */).
  {syspara.i 50 15 1 iStrTypeID INT}
  IF iStrTypeId = 0 THEN
      iStrTypeId = 2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerKobling dTables  _DB-REQUIRED
PROCEDURE KontrollerKobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER piEkstVPILevNr   AS INT  NO-UNDO.
    DEF INPUT PARAMETER pcValgteArtikler AS CHAR NO-UNDO.

    DEF VAR piLoop1 AS INT NO-UNDO.

    IF pcValgteArtikler <> "" THEN
    UTVALG:
    DO piLoop1 = 1 TO NUM-ENTRIES(pcValgteArtikler,CHR(1)):
        RUN KontrollerKobling2 (piEkstVPILevNr, ENTRY(piLoop1,pcValgteArtikler,CHR(1))).
    END. /* UTVALG */
    ELSE 
    ALLE:
    DO:
        FOR EACH VPIArtBas NO-LOCK WHERE
            VPIArtBas.EkstVPILevNr = piEkstVPILevNr:
            RUN KontrollerKobling2 (piEkstVPILevNr, VPIArtBas.VareNr).
        END.
    END. /* ALLE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerKobling2 dTables  _DB-REQUIRED
PROCEDURE KontrollerKobling2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER piEkstVPILevNr   AS INT  NO-UNDO.
    DEF INPUT PARAMETER pcVareNr         AS CHAR NO-UNDO.

    DEF VAR piLoop1 AS INT NO-UNDO.

    KONTROLL:
    DO TRANSACTION:
      FIND VPIArtBas EXCLUSIVE-LOCK WHERE
        VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
        VPIArtBas.VareNr       = pcVareNr NO-ERROR.
      /* VPI på EAN kode nivå. */
      IF piEkstVPILevNr >= 100 THEN
      DO:
          FIND FIRST Strekkode NO-LOCK WHERE
               Strekkode.Kode = VPIArtBas.VareNr NO-ERROR.
          IF NOT AVAILABLE Strekkode THEN
              FIND FIRST Strekkode NO-LOCK WHERE
                   Strekkode.Kode = trim(VPIArtBas.VareNr) NO-ERROR.
          IF AVAILABLE Strekkode THEN
              VPIArtBas.ArtikkelNr = Strekkode.ArtikkelNr.
          ELSE
              VPIArtBas.ArtikkelNr = 0.

          IF VPIArtBas.ArtikkelNr = 0 THEN
          SJEKK-STREKKODER:
          FOR EACH VPIStrekkode OF VPIArtBas EXCLUSIVE-LOCK:
              FIND FIRST Strekkode NO-LOCK WHERE
                   Strekkode.Kode = VPIStrekkode.Kode NO-ERROR.
              IF NOT AVAILABLE Strekkode THEN
                  FIND FIRST Strekkode NO-LOCK WHERE
                      Strekkode.Kode = trim(VPIStrekkode.Kode) NO-ERROR.
              IF AVAILABLE Strekkode THEN
                  VPIArtBas.ArtikkelNr = Strekkode.ArtikkelNr.
              IF VPIArtBas.ArtikkelNr <> 0 THEN
                  LEAVE SJEKK-STREKKODER.
          END. /* SJEKK-STREKKODER */
      END.
      /* VPI på artikkelnivå (Vpi fra HK)        */
      /* Her skal det ikke kontrolleres kobling. */
      ELSE DO:

      END.
    END. /* KONTROLL */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterInfo dTables  _DB-REQUIRED
PROCEDURE OppdaterInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  DEF BUFFER bVPIArtBas  FOR VPIArtBas.
  DEF BUFFER bVPIArtPris FOR VPIArtPris.

  /* Henter sentrallager. */
  FIND clButiker NO-LOCK WHERE
      clButiker.Butik = piCl.

  DO TRANSACTION:
      /* Finner artikkelen som skal oppdateres. */
      FIND ArtBas EXCLUSIVE-LOCK WHERE
          ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          RETURN "AVBRYT".

      FIND VPIArtBas EXCLUSIVE-LOCK WHERE
           VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
           VPIArtBas.VareNr       = ENTRY(1,pcListe,CHR(1))
          NO-ERROR.
      IF NOT AVAILABLE VPIArtBas THEN
          RETURN "AVBRYT".

      /* Kontroll av registerinformasjon */
      RUN Registerkontroll.
      IF RETURN-VALUE <> "" THEN
          RETURN RETURN-VALUE.

      /* Ved kobling, kobles kun de nye strekkodene inn. Ingen annen */
      /* artikkelinformasjon oppdateres.                             */
      /*
      ASSIGN
          ArtBas.Beskr        = VPIArtBas.Beskr
          ArtBas.BongTekst    = VPIArtBas.BongTekst  
          ArtBas.LevKod       = VPIArtBas.LevKod   
          ArtBas.VmId         = VPIArtBas.VmId      
          ArtBas.LevNr        = VPIArtBas.LevNr      
          /*ArtBas.Notat        = VPIArtBas.Notat  */
          ArtBas.Farg         = VPIArtBas.Farg
          ArtBas.ValKod       = VPIArtBas.ValKod
          /*
          ArtBas.Storrelser   = IF VPIArtBas.StrTypeId = 1
                                  THEN FALSE 
                                  ELSE TRUE
          ArtBas.Aktivert      = TRUE
          */
          VPIArtBas.ArtikkelNr = ArtBas.ArtikkelNr
          .
    */      

    /* Opprettelse av Strekkoder.                                            */
    /* Må gjøres her under ny, da vi må kunne teste på om StrType må endres. */
    RUN OpprettStrekkoder (INPUT piEkstVPILevNr, INPUT pcListe, ArtBas.ArtikkelNr).

    /* Legger opp interne EAN koder på de størrelser som ikke har dette. */
    /*RUN genStrekKode.p (VPIArtBas.ArtikkelNr,IF cGenEan = "yes" THEN 1 ELSE 0,"").*/
    IF pcListe <> "" THEN
    DO piLoop = 1 TO NUM-ENTRIES(pcListe,CHR(1)):
        RUN genStrekKode.p (ENTRY(piLoop,pcListe,CHR(1)),IF cGenEan = "yes" THEN 1 ELSE 0,"").
    END.
    /* Kommer vi ut med error her, skal det ikke settes error flag. */
    IF ERROR-STATUS:ERROR THEN
        RUN dummy.
  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPris dTables  _DB-REQUIRED
PROCEDURE OppdaterPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  ------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  DEF VAR piErrNr      AS INT    NO-UNDO.
  DEF VAR piAntallOk   AS INT    NO-UNDO.
  DEF VAR piAntallTot  AS INT    NO-UNDO.
  DEF VAR h_PrisKo     AS HANDLE NO-UNDO.
  DEF VAR pcError      AS CHAR   NO-UNDO.
  DEF VAR pcSkjerm     AS CHAR   NO-UNDO.
  DEF VAR pbSvar       AS LOG    NO-UNDO.
  DEF VAR pcAddOn      AS CHAR   NO-UNDO.
  DEF VAR piProfilNr   AS INT    NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  DEF BUFFER bVPIArtBas  FOR VPIArtBas.
  DEF BUFFER bufButiker FOR Butiker.

  ASSIGN
      pcAddOn = ";no;" + STRING(TODAY,"99/99/99") + ";0;;;0;0;no"
      .

  FIND clButiker NO-LOCK WHERE
      clButiker.Butik = piCl.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  DO TRANSACTION:
      /* Finner artikkelen som skal oppdateres. */
      FIND ArtBas EXCLUSIVE-LOCK WHERE
          ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          RETURN "AVBRYT".

      /* HK artikler skal ikke ha overstyrt pris. */
      IF ArtBas.KjedeVare THEN
          RETURN "AVBRYT".

      FIND VPIArtBas EXCLUSIVE-LOCK WHERE
           VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
           VPIArtBas.VareNr       = ENTRY(1,pcListe,CHR(1))
          NO-ERROR.
      IF NOT AVAILABLE VPIArtBas THEN
          RETURN "AVBRYT".

      /* Oppdaterer artikkelinformasjon */
      ASSIGN
          /* VPI info */
          ArtBas.KatalogPris   = VPIArtBas.KatalogPris[1]
          ArtBas.ForhRab%      = VPIArtBas.forhRab%[1]
          ArtBas.SupRab%       = VPIArtBas.suppRab%[1]
          ArtBas.VPIDato       = TODAY
          ArtBas.LinjeMerknad  = VPIArtBas.LinjeMerknad
          ArtBas.LevDato1      = VPIArtBAs.LevDato1
          ArtBas.LevDato2      = VPIArtBAs.LevDato2
          ArtBas.LevDato3      = VPIArtBAs.LevDato3
          ArtBas.LevDato4      = VPIArtBAs.LevDato4
          ArtBas.VPIBildeKode  = VPIArtBas.VPIBildeKode
          ArtBas.ModellFarge   = VPIArtBas.ModellFarge
          ArtBas.HovedmodellFarge = VPIArtBas.HovedmodellFarge
          .

      /* Henter VPI kalkyle med den profil som er importert */
      FIND FIRST VPIArtPris NO-LOCK WHERE
          VPIArtPris.EkstVpiLevNr = VPIArtBas.EkstVPILevNr AND
          VPIArtPris.VareNr       = VPIArtBas.VareNr NO-ERROR.
      IF NOT AVAILABLE VPIArtPris THEN
        FIND VPIArtPris NO-LOCK WHERE
            VPIArtPris.EkstVpiLevNr = VPIArtBas.EkstVPILevNr AND
            VPIArtPris.VareNr       = VPIArtBas.VareNr AND
            VPIArtPris.ProfilNr     = clButiker.ProfilNr NO-ERROR.
      IF NOT AVAILABLE VPIArtPris THEN
          RETURN.

      /* Oppretter kalkyle for sentrallageret - hvis den ikke finnes. */
      IF NOT CAN-FIND(ArtPris WHERE
                      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                      ArtPris.ProfilNr   = VPIArtPris.ProfilNr) THEN
      KALKYLE:
      DO:
          CREATE ArtPris.
          ASSIGN
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr 
              ArtPris.ProfilNr   = VPIArtPris.ProfilNr
              .
          IF AVAILABLE VPIArtPris THEN
              BUFFER-COPY VPIArtPris 
                          EXCEPT ArtikkelNr ProfilNr
                          TO ArtPris.

      END. /* KALKYLE */

      /* Henter aktiv lokal kalkyle. */
      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.

      /* Totalt antall linjer. */
      ASSIGN
          pcError     = ""
          piAntallTot = piAntallTot + 1
          .

      RUN SjekkNyPrisKo IN h_PrisKo 
                       (ArtBas.ArtikkelNr, 
                        VPIArtPris.ProfilNr,    
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN VPIArtPris.AktivFraDato
                           ELSE VPIArtPris.TilbudFraDato),        
                        (IF VPIArtPRis.Tilbud = FALSE 
                           THEN VPIArtPRis.AktivFraTid
                           ELSE VPIArtPRis.TilbudFraTid),         
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN ?
                           ELSE VPIArtPris.TilbudTilDato),       
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN 0
                           ELSE VPIArtPris.TilbudTilTid),        
                        ?,  
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN FALSE /* Normalpris */
                           ELSE TRUE /* Tilbud */),      
                        (IF VPIArtPris.Tilbud = FALSE 
                           THEN 1 /* Normalpris */
                           ELSE 2 /* Tilbud */),      
                        OUTPUT pcError).        
      IF pcError <> "" THEN
      DO:
          ASSIGN
              piErrNr = int(ENTRY(1,pcError,CHR(1)))
              .
          /* Datofeil. Tillatt å gå videre med dagens dato/tid. */
          IF piErrNr < 20 AND VPIArtPris.Tilbud = FALSE THEN
          DO:
              /*
              MESSAGE ENTRY(2,pcError,CHR(1)) SKIP(1)
                  "Priskøpost vil bli opprettet med dagens dato og tid."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              */
              ASSIGN
                  pcError = ""
                  .
          END.
          ELSE DO:
              /*
              MESSAGE ENTRY(2,pcError)
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              */
              RETURN.
          END.
      END.

      /* Merker linjer med feilkode. */
      IF pcError <> "" THEN. /* Gjør ingenting. */
      ELSE /* Antall godkjente */
      OPPRETT:
      DO: /* Oppretter priskøpost. */

        FIND VarGr  OF ArtBas NO-LOCK NO-ERROR.
        FIND Moms   OF VarGr  NO-LOCK NO-ERROR.
        FIND Valuta OF ArtBas NO-LOCK NO-ERROR.
        
        /* Bygge skjerm streng ut fra aktiv kalkyle */
        RUN InitKalkyle IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               18,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     

        /* Oppdaterer strengen med den nye prisen. */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 1,
                        INPUT STRING(VPIArtPris.ValPris[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").  
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
        (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               1,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)). 
        pcSkjerm = pcSkjerm + pcAddOn.
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 3,
                        /* Det er kun 2 som benyttes 
                        input (IF KampanjeHode.NormalPris
                                THEN string(KampanjeLinje.Pris[1])
                                ELSE STRING(KampanjeLinje.Pris[2])) ,
                        */
                        INPUT STRING(VPIArtPris.Rab1Kr[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
        (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               3,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 5,
                        /* Det er kun 2 som benyttes 
                        input (IF KampanjeHode.NormalPris
                                THEN string(KampanjeLinje.Pris[1])
                                ELSE STRING(KampanjeLinje.Pris[2])) ,
                        */
                        INPUT STRING(VPIArtPris.Rab2Kr[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               5,     
               (IF AVAILABLE VPIArtPris
               THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 7,
                        INPUT STRING(VPIArtPris.Frakt[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               7,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 9,
                        INPUT STRING(VPIArtPris.DivKostKr[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               9,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
              ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 11,
                        INPUT STRING(VPIArtPris.Rab3Kr[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               11,     
               (IF AVAILABLE VPIArtPris
               THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 14,
                        INPUT STRING(VPIArtPris.DbKr[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               14,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 18,
                        /* Det er kun 2 som benyttes 
                        input (IF KampanjeHode.NormalPris
                                THEN string(KampanjeLinje.Pris[1])
                                ELSE STRING(KampanjeLinje.Pris[2])) ,
                        */
                        INPUT STRING(VPIArtPris.Pris[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       
        RUN Omregning IN h_PrisKo
              (RECID(ArtBas),
               VPIArtPris.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
               (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
        18,     
        (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     
        pcSkjerm = pcSkjerm + pcAddOn.

        IF VPIArtPris.Tilbud = TRUE THEN
        TILB-OPPDAT-STRENG:
        DO:
          /* Tilbud fra */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 23,
                                INPUT STRING(VPIArtPris.TilbudFraDato),
                                INPUT ";").
          /* Tilbud fra tid */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 24,
                                INPUT STRING(VPIArtPris.TilbudFraTid),
                                INPUT ";").
          /* Tilbud til */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 25,
                                INPUT STRING(VPIArtPris.TilbudTilDato),
                                INPUT ";").
          /* Tilbud til tid */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 26,
                                INPUT STRING(VPIArtPRis.TilbudTilTid),
                                INPUT ";").

        END. /* TILB-OPPDAT-STRENG */
        ELSE 
        NORM-OPPDAT-STRENG:
        DO:
          /* Fra */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 21,
                                INPUT (IF piErrNr > 0
                                         THEN STRING(TODAY)
                                         ELSE STRING(VPIArtPris.AktivFraDato)),
                                INPUT ";").
          /* Til tid */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 22,
                                INPUT (IF piErrNr > 0
                                         THEN (IF TIME > 60 THEN STRING(TIME - 60) ELSE STRING(TIME))
                                         ELSE STRING(VPIArtPris.AktivFraTid)),
                                INPUT ";").

        END. /* NORM-OPPDAT-STRENG */

        /* Oppdatering skal utføres. */
        RUN NyPrisKo IN h_PrisKo
            (RECID(ArtBas),
             VPIArtPris.ProfilNr,   
             INPUT-OUTPUT pcSkjerm,     
             (IF VPIArtPris.Tilbud = FALSE
                         THEN FALSE /* Normalpris */
                         ELSE TRUE /* Tilbud */),
             (IF VPIArtPRis.Tilbud = FALSE
                THEN 1
                ELSE 2)).    

        ASSIGN
          piAntallOk = piAntallOk + 1
          .
      END. /* OPPRETT */

  END. /* TRANSACTION */

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPris_old dTables  _DB-REQUIRED
PROCEDURE OppdaterPris_old PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  ------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  DEF VAR piErrNr      AS INT    NO-UNDO.
  DEF VAR piAntallOk   AS INT    NO-UNDO.
  DEF VAR piAntallTot  AS INT    NO-UNDO.
  DEF VAR h_PrisKo     AS HANDLE NO-UNDO.
  DEF VAR pcError      AS CHAR   NO-UNDO.
  DEF VAR pcSkjerm     AS CHAR   NO-UNDO.
  DEF VAR pbSvar       AS LOG    NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

MESSAGE PROGRAM-NAME(1) SKIP
        "Oppdater pris"
        VIEW-AS ALERT-BOX.

  DEF BUFFER bVPIArtBas  FOR VPIArtBas.

  /* Henter sentrallager. */
  FIND clButiker NO-LOCK WHERE
      clButiker.Butik = piCl.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  DO TRANSACTION:
      /* Finner artikkelen som skal oppdateres. */
      FIND ArtBas EXCLUSIVE-LOCK WHERE
          ArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          RETURN "AVBRYT".

      FIND VPIArtBas EXCLUSIVE-LOCK WHERE
           VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
           VPIArtBas.VareNr       = ENTRY(1,pcListe,CHR(1))
          NO-ERROR.
      IF NOT AVAILABLE VPIArtBas THEN
          RETURN "AVBRYT".

      /* Oppdaterer artikkelinformasjon */
      ASSIGN
          /* VPI info */
          ArtBas.KatalogPris   = VPIArtBas.KatalogPris[1]
          ArtBas.ForhRab%      = VPIArtBas.forhRab%[1]
          ArtBas.SupRab%       = VPIArtBas.suppRab%[1]
          ArtBas.VPIDato       = TODAY
          ArtBas.LinjeMerknad  = VPIArtBas.LinjeMerknad
          ArtBas.LevDato1      = VPIArtBAs.LevDato1
          ArtBas.LevDato2      = VPIArtBAs.LevDato2
          ArtBas.LevDato3      = VPIArtBAs.LevDato3
          ArtBas.LevDato4      = VPIArtBAs.LevDato4
          .

      /* Henter VPI kalkyle */
      FIND VPIArtPris NO-LOCK WHERE
          VPIArtPris.EkstVpiLevNr = VPIArtBas.EkstVPILevNr AND
          VPIArtPris.VareNr       = VPIArtBas.VareNr AND
          VPIArtPris.ProfilNr     = clButiker.ProfilNr NO-ERROR.

      /* Oppretter kalkyle for sentrallageret - hvis den ikke finnes. */
      IF NOT CAN-FIND(ArtPris WHERE
                      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                      ArtPris.ProfilNr   = clButiker.ProfilNr) THEN
      KALKYLE:
      DO:
          CREATE ArtPris.
          ASSIGN
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr 
              ArtPris.ProfilNr   = clButiker.ProfilNr
              .
          IF AVAILABLE VPIArtPris THEN
              BUFFER-COPY VPIArtPris 
                          EXCEPT ArtikkelNr ProfilNr
                          TO ArtPris.

      END. /* KALKYLE */

      /* Henter aktiv lokal kalkyle. */
      FIND ArtPris NO-LOCK WHERE
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.

      /* Totalt antall linjer. */
      ASSIGN
          pcError     = ""
          piAntallTot = piAntallTot + 1
          .

      RUN SjekkNyPrisKo IN h_PrisKo 
                       (ArtBas.ArtikkelNr, 
                        clButiker.ProfilNr,    
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN VPIArtPris.AktivFraDato
                           ELSE VPIArtPris.TilbudFraDato),        
                        (IF VPIArtPRis.Tilbud = FALSE 
                           THEN VPIArtPRis.AktivFraTid
                           ELSE VPIArtPRis.TilbudFraTid),         
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN ?
                           ELSE VPIArtPris.TilbudTilDato),       
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN 0
                           ELSE VPIArtPris.TilbudTilTid),        
                        ?,  
                        (IF VPIArtPris.Tilbud = FALSE
                           THEN FALSE /* Normalpris */
                           ELSE TRUE /* Tilbud */),      
                        (IF VPIArtPris.Tilbud = FALSE 
                           THEN 1 /* Normalpris */
                           ELSE 2 /* Tilbud */),      
                        OUTPUT pcError).        

      IF pcError <> "" THEN
      DO:
          ASSIGN
              piErrNr = int(ENTRY(1,pcError,CHR(1)))
              .
          /* Datofeil. Tillatt å gå videre med dagens dato/tid. */
          IF piErrNr < 20 AND VPIArtPris.Tilbud = FALSE THEN
          DO:
              MESSAGE ENTRY(2,pcError,CHR(1)) SKIP(1)
                  "Priskøpost vil bli opprettet med dagens dato og tid."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              ASSIGN
                  pcError = ""
                  .
          END.
          ELSE DO:
              MESSAGE ENTRY(2,pcError)
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN.
          END.
      END.

      /* Merker linjer med feilkode. */
      IF pcError <> "" THEN. /* Gjør ingenting. */
      ELSE /* Antall godkjente */
      OPPRETT:
      DO: /* Oppretter priskøpost. */

        FIND VarGr  OF ArtBas NO-LOCK NO-ERROR.
        FIND Moms   OF VarGr  NO-LOCK NO-ERROR.
        FIND Valuta OF ArtBas NO-LOCK NO-ERROR.
        
        /* Bygge skjerm streng ut fra aktiv kalkyle */
        RUN InitKalkyle IN h_PrisKo
              (RECID(ArtBas),
               clButiker.ProfilNr, 
               INPUT-OUTPUT pcSkjerm,     
               Moms.MomsProc,   
        (IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 1),    
               18,     
               (IF AVAILABLE VPIArtPris
                  THEN VPIArtPris.Tilbud
                  ELSE FALSE)).     

        /* Oppdaterer strengen med den nye prisen. */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                        INPUT 18,
                        /* Det er kun 2 som benyttes 
                        input (IF KampanjeHode.NormalPris
                                THEN string(KampanjeLinje.Pris[1])
                                ELSE STRING(KampanjeLinje.Pris[2])) ,
                        */
                        INPUT STRING(VPIArtPris.Pris[IF VPIArtPris.Tilbud THEN 2 ELSE 1]) ,
                        INPUT ";").       

        IF VPIArtPris.Tilbud = TRUE THEN
        TILB-OPPDAT-STRENG:
        DO:
          /* Tilbud fra */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 23,
                                INPUT STRING(VPIArtPris.TilbudFraDato),
                                INPUT ";").
          /* Tilbud fra tid */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 24,
                                INPUT STRING(VPIArtPris.TilbudFraTid),
                                INPUT ";").
          /* Tilbud til */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 25,
                                INPUT STRING(VPIArtPris.TilbudTilDato),
                                INPUT ";").
          /* Tilbud til tid */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 26,
                                INPUT STRING(VPIArtPRis.TilbudTilTid),
                                INPUT ";").

        END. /* TILB-OPPDAT-STRENG */
        ELSE 
        NORM-OPPDAT-STRENG:
        DO:
          /* Fra */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 21,
                                INPUT (IF piErrNr > 0
                                         THEN STRING(TODAY)
                                         ELSE STRING(VPIArtPris.AktivFraDato)),
                                INPUT ";").
          /* Til tid */
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                                INPUT 22,
                                INPUT (IF piErrNr > 0
                                         THEN STRING(TIME)
                                         ELSE STRING(VPIArtPris.AktivFraTid)),
                                INPUT ";").

        END. /* NORM-OPPDAT-STRENG */

        /* Oppdatering skal utføres. */
        RUN NyPrisKo IN h_PrisKo
            (RECID(ArtBas),
             clButiker.ProfilNr,   
             INPUT-OUTPUT pcSkjerm,     
             (IF VPIArtPris.Tilbud = FALSE
                         THEN FALSE /* Normalpris */
                         ELSE TRUE /* Tilbud */),
             (IF VPIArtPRis.Tilbud = FALSE
                THEN 1
                ELSE 2)).    

        ASSIGN
          piAntallOk = piAntallOk + 1
          .
      END. /* OPPRETT */

  END. /* TRANSACTION */

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpphevKobling dTables  _DB-REQUIRED
PROCEDURE OpphevKobling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  DO piLoop = 1 TO NUM-ENTRIES(pcListe,CHR(1)) TRANSACTION:
      FIND VPIArtBas EXCLUSIVE-LOCK WHERE
          VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
          VPIArtBas.VareNr       = ENTRY(piLoop,pcListe,CHR(1)) NO-ERROR.
      IF AVAILABLE VPIArtBas THEN
          ASSIGN
          VPIArtBas.ArtikkelNr = 0
          .
      /* Strekkoden skal også slettes */
      FOR EACH Strekkode EXCLUSIVE-LOCK WHERE
          Strekkode.Kode = ENTRY(piLoop,pcListe,CHR(1)):
          DELETE STrekkode.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettBilder dTables  _DB-REQUIRED
PROCEDURE OpprettBilder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNr       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  BILDER:
  FOR EACH VPIBildeRegister NO-LOCK WHERE
      VPIBildeRegister.EkstVPILevNr = piEkstVPILevNr AND
      VPIBildeRegister.VareNr       = pcVareNr:

      /* Renser bort gammelt bilde */
      FIND BildeRegister EXCLUSIVE-LOCK WHERE
          BildeRegister.BildNr = VPIBildeRegister.BildNr NO-ERROR.
      IF AVAILABLE BildeRegister THEN
      DO:
          FOR EACH BildeData OF BildeRegister EXCLUSIVE-LOCK:
              DELETE BildeData.
          END.
          DELETE BildeRegister.
      END.
      /* Legger opp nytt bilde. */
      IF NOT CAN-FIND(BildeRegister WHERE
                      BildeRegister.BildNr = VPIBilderegister.BildNr) THEN
      DO:
          CREATE BildeRegister.
          BUFFER-COPY VPIBildeRegister TO BildeRegister
              .
      END.
      FOR EACH VPIBildeData OF VPIBildeRegister:
          IF NOT CAN-FIND(BildeData WHERE
                          BildeData.BildNr = VPIBildeData.BildNr AND
                          BildeData.Teller = VPIBildeData.Teller) THEN
          DO:
              CREATE BildeData.
              BUFFER-COPY VPIBildeData TO BildeData
                  .
          END.
      END.
  END. /* BILDER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettErstattningsvarer dTables  _DB-REQUIRED
PROCEDURE OpprettErstattningsvarer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNr       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  ERSTATTNINGSVARER:
  FOR EACH VPIErstattningsvare NO-LOCK WHERE
      VPIErstattningsvare.EkstVPILevNr = piEkstVPILevNr AND
      VPIErstattningsvare.VareNr       = pcVareNr:

      FIND Erstattningsvare EXCLUSIVE-LOCK WHERE
          Erstattningsvare.ArtikkelNr = VPIErstattningsvare.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE Erstattningsvare THEN
      DO:
          CREATE Erstattningsvare.
      END.
      BUFFER-COPY VPIErstattningsvare TO Erstattningsvare.
      RELEASE Erstattningsvare.
  END. /* ERSTATTNINGSVARER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettHKStrekkoder dTables  _DB-REQUIRED
PROCEDURE OpprettHKStrekkoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    RUN OpprettHKStrekkoder (INPUT piEkstVPILevNr, INPUT VPIArtBas.VareNr, INPUT VPIArtBas.ArtikkelNr).

------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNr       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO.
  
  DEF VAR pcEAN AS CHAR NO-UNDO.

  EAN-128-KODER:
  FOR EACH VPIStrekkode NO-LOCK WHERE
      VPIStrekkode.EkstVPILevNr = piEkstVPILevNr AND
      VPIStrekkode.VareNr       = pcVareNr AND 
      VPIStrekkode.Kodetype     = 9:

      ASSIGN
          pcEAN = "02" + 
                  string(VPIStrekkode.VareNr,"9999999") + 
                  STRING(VPIStrekkode.StrKode,"999")
          pcEAN = fixChk(pcEAN)
          .

      /* Henter Strekkoden for å legge på koden i kommentarfeltet. */
      FIND Strekkode EXCLUSIVE-LOCK WHERE
           Strekkode.Kode = pcEAN NO-ERROR.
      /* Har strekkoden blitt flyttet til annen størrelse, skal det endres. */
      IF AVAILABLE Strekkode AND
          Strekkode.StrKode <> VPISTrekkode.StrKode THEN
          DELETE STrekkode.
      IF NOT AVAILABLE Strekkode THEN
      DO:
          CREATE Strekkode.
          ASSIGN
              Strekkode.ArtikkelNr        = plArtikkelNr 
              Strekkode.Kode              = pcEAN
              Strekkode.StrKode           = VPIStrekkode.StrKode
              Strekkode.KodeType          = 1
              Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer
              .
      END.
  END. /* EAN-128-KODER */

  STREKKODER:
  FOR EACH VPIStrekkode NO-LOCK WHERE
      VPIStrekkode.EkstVPILevNr = piEkstVPILevNr AND
      VPIStrekkode.VareNr       = pcVareNr AND 
      VPIStrekkode.Kodetype     < 9:
      /* Bedriftsinterne EAN skal ikke kunne hentes inn. */
      IF bHk = FALSE THEN
      DO:
          IF LENGTH(VPIStrekkode.Kode) = 13 AND
             SUBSTRING(VPIStrekkode.Kode,1,2) = "02" THEN
            NEXT STREKKODER.
      END.

      /* Skal strekkoden slettes fordi den ligger på feil artikkel? */
      PA-FEIL-ARTIKKEL:
      DO:

          FIND Strekkode WHERE
              Strekkode.Kode = trim(VPIStrekkode.Kode) EXCLUSIVE-LOCK NO-ERROR.
          /* Ligger den på feil artikkel? */
          IF AVAILABLE Strekkode AND Strekkode.ArtikkelNr <> DEC(VPISTrekkode.VareNr) THEN
              DELETE Strekkode.
          /* Har strekkoden blitt flyttet til annen størrelse, skal det endres. */
          IF AVAILABLE Strekkode AND
              Strekkode.StrKode <> VPISTrekkode.StrKode THEN
              DELETE STrekkode.
          IF AVAILABLE Strekkode THEN RELEASE Strekkode.
      END.

      IF NOT CAN-FIND(Strekkode WHERE
                      Strekkode.Kode = trim(VPIStrekkode.Kode)) THEN
      DO:
          CREATE Strekkode.
          BUFFER-COPY VPIStrekkode TO Strekkode
              ASSIGN
              Strekkode.Kode       = TRIM(VPIStrekkode.Kode)
              Strekkode.ArtikkelNr = plArtikkelNr
              Strekkode.VareId     = plArtikkelNr
              Strekkode.HovedNr    = FALSE
              /*Strekkode.Bestillingsnummer = VPIStrekkode.EkstStorl*/
              .
      END.
  END. /* STREKKODER */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettNy dTables 
PROCEDURE OpprettNy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  RUN OpprettNy    IN h_dvpiartbas (piEkstVpiLevNr, pcListe, OUTPUT plArtikkelNr).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcListe        AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  DEF VAR pcArtListe    AS CHAR NO-UNDO.
  DEF VAR piLoop        AS INT  NO-UNDO.
  DEF VAR pcAvbrudd     AS CHAR NO-UNDO.
  DEF VAR pbArtikkelNr  AS DEC  NO-UNDO.
  DEF VAR piLoop2       AS INT  NO-UNDO.
  DEF VAR piLopNr       AS INT  NO-UNDO.
  DEF VAR plModellFarge AS DEC  NO-UNDO.
  DEF VAR pl2ModellFarge AS DEC NO-UNDO.

  DEF BUFFER bVPIArtBas           FOR VPIArtBas.
  DEF BUFFER b2VPIArtBas          FOR VPIArtBas.
  DEF BUFFER bVPIArtPris          FOR VPIArtPris.
  DEF BUFFER bVPIErstattningsvare FOR VPIErstattningsvare.

  /* Henter sentrallager. */
  FIND clButiker NO-LOCK WHERE
      clButiker.Butik = piCl.

  /*IF piEkstVpiLevNr < 100 THEN*/
  DO:
      FIND bVPIArtBas NO-LOCK WHERE
          bVPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
          bVPIArtBas.VareNr       = ENTRY(1,pcListe,CHR(1)) NO-ERROR.
      /* Håndterer pakkevare for komplette VPI sett. */
      IF AVAILABLE bVPIArtBas THEN
      PAKKE:
      DO:
          FOR EACH VPIPakkeLinje OF bVPIArtBas NO-LOCK:
              IF NOT CAN-DO(pcArtListe,STRING(VPIPakkeLinje.PkArtikkelNr)) THEN
                  pcArtListe = pcArtListe + 
                               (IF pcArtListe = ""
                                  THEN ""
                                  ELSE ",") + 
                               STRING(VPIPAkkeLinje.PkArtikkelNr).
              /* Er pakkemedlemmet med i en modell, skal hele modellen ut. */
              FIND bVPIArtBas NO-LOCK WHERE
                   bVPIArtBas.EkstVPILevNr = VPIPakkeLinje.EkstVPILevNr AND
                   bVPIArtBas.VareNr       = string(VPIPakkeLinje.PkArtikkelNr)
                   NO-ERROR.
              IF AVAILABLE bVPIArtBas AND bVPIArtBas.ModellFarge <> 0 THEN
              DO:
                  FOR EACH b2VPIArtBas NO-LOCK WHERE
                      b2VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
                      b2VPIArtBas.ModellFarge  = bVPIArtBas.ModellFarge:
                    IF NOT CAN-DO(pcArtListe,STRING(b2VPIArtBas.VareNr)) THEN
                        pcArtListe = pcArtListe + 
                                     (IF pcArtListe = ""
                                        THEN ""
                                        ELSE ",") + 
                                     STRING(b2VPIArtBas.VareNr).
                  END.
              END.
          END.
      END. /* PAKKE */
      /* Håndterer modell */
      IF bVPIArtBas.ModellFarge <> 0 THEN
      MODELL:
      DO:
          ASSIGN
              plModellFarge = bVPIArtBas.ModellFarge
              .
          FOR EACH b2VPIArtBas NO-LOCK WHERE
              b2VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
              b2VPIArtBas.ModellFarge  = bVPIArtBas.ModellFarge:
            IF NOT CAN-DO(pcArtListe,STRING(b2VPIArtBas.VareNr)) THEN
            DO:
                pcArtListe = pcArtListe + 
                             (IF pcArtListe = ""
                                THEN ""
                                ELSE ",") + 
                             STRING(b2VPIArtBas.VareNr).
            END.
          END.
      END. /* MODELL */
      ELSE plModellFarge = 0.
      /* Håndterer erstattningsvarer. */
      FIND FIRST VPIErstattningsvare NO-LOCK WHERE
                 VPIErstattningsvare.EkstVPILEvNr = piEkstVPILevNr AND
                 VPIErstattningsvare.VareNr       = bVPIArtBas.VareNr NO-ERROR.
      IF AVAILABLE VPIErstattningsvare THEN
      DO:
          FOR EACH bVPIErstattningsvare WHERE
              bVPIErstattningsvare.EkstVPILevNr = piEkstVPILevNr AND
              bVPIErstattningsvare.ErstattId    = VPIErstattningsvare.ErstattId:
              IF NOT CAN-DO(pcArtListe,STRING(bVPIErstattningsvare.VareNr)) THEN
              DO:
                  pcArtListe = pcArtListe + 
                               (IF pcArtListe = ""
                                  THEN ""
                                  ELSE ",") + 
                               STRING(bVPIErstattningsvare.VareNr).
              END.
          END.
      END.
  END.

  /* Legger inn valgt artikkel */
  IF NOT CAN-DO(pcArtListe,ENTRY(1,pcListe,CHR(1))) THEN
      ASSIGN
      pcArtListe = ENTRY(1,pcListe,CHR(1)) + 
                   (IF pcArtListe = ""
                      THEN ""
                      ELSE ",") +
                   pcArtListe
      .

  OPPRETTELSE:
  DO piLoop2 = 1 TO NUM-ENTRIES(pcArtListe) TRANSACTION:
    /* Legger ut Modell, pakkevare og sammenligningsvarer. */
    IF ENTRY(piLoop2,pcArtListe) = "" THEN
        LEAVE OPPRETTELSE.

    FIND VPIArtBas EXCLUSIVE-LOCK WHERE
         VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
         VPIArtBas.VareNr       = ENTRY(piLoop2,pcArtListe)
        NO-ERROR.
    IF NOT AVAILABLE VPIArtBas THEN
        RETURN "AVBRYT".
    /* Kontroll av registerinformasjon */

    RUN Registerkontroll.

    IF RETURN-VALUE <> "" THEN
        RETURN RETURN-VALUE.
    /* Slipper artbas hvis den er tilgjengelig fra før. */
    IF AVAILABLE ArtBas THEN
        RELEASE ArtBas.

    /* Sjekker om artikkelen finnes fra før.                        */
    /* VPI artikkler med VPINr < 100 skal beholde artikkelnummeret. */
    /*IF piEkstVPILevNr < 100 THEN */
    /* TN 10/9-04 Skal alltid beholde artikkelnr. */
    DO:
        ASSIGN
            pbArtikkelNr         = dec(VPIArtBas.VareNr)
            VPIArtBas.ArtikkelNr = dec(VPIArtBas.VareNr)
            .
    END.
    /*
    ELSE
        pbArtikkelNr = 0.
    */

    /* Oppstandelsen.                                             */
    /* Er det et ArtikkelNr fra HK's nummerserie, sjekkes det     */
    /* om artikkelen er opprettet lokalt tidligere.               */
    /* Artikkler som ikke kommer fra HK, har alltid artikkelnr 0. */
    /* De skal tildeles nytt artikkelnummer lokalt.               */
/*     IF VPIArtBas.ArtikkelNr > 8500000 THEN */
    FIND ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr NO-ERROR.

/* TN 19/12-07
/* Er det en kjedelevert vare, skal ikke artikkelinformasjonen oppdateres. */
    IF AVAILABLE ArtBas AND ArtBas.KjedeVare THEN
        RETURN. /*"Kjedevare skal ikke oppdateres. " + 
               STRING(ArtBas.ArtikkelNr) + " " + ArtBas.Beskr + ".".*/
*/
    DO:
        ASSIGN
            plArtikkelNr = VPIArtBas.ArtikkelNr.

        /* Oppretter ny artikkel */
        IF NOT AVAILABLE ArtBas THEN
        DO:
            CREATE ArtBas.
            BUFFER-COPY VPIArtBas EXCEPT ArtikkelNr LopNr
                TO ArtBas
                ASSIGN
                ArtBas.LopNr       = ?
                ArtBas.ArtikkelNr  = plArtikkelNr
                ArtBas.KatalogPris = VPIArtBas.KatalogPris[1]
                ArtBas.ForhRab%    = VPIArtBas.forhRab%[1]
                ArtBas.SupRab%     = VPIArtBas.suppRab%[1]
                ArtBas.VPIDato     = VPIArtBas.VPIDato
                ArtBas.LevKod      = VPIArtBas.LevKod   
                .
            /* Tildeler løpenummer til ny artikkel */
            RUN SetLopeNr2 (OUTPUT ArtBas.LopNr).
            /* Overstyrer tildelte verdier */
            ASSIGN
                /*VPIArtBas.ArtikkelNr = ArtBas.ArtikkelNr*/
                ArtBas.StrTypeId     = (IF ArtBas.Pakke 
                                        THEN 1                                                                         
                                        ELSE ArtBas.StrTypeId)
                ArtBas.Lager         = IF ArtBas.Pakke 
                                        THEN ArtBas.Lager = FALSE
                                        ELSE ArtBas.Lager
                ArtBas.Storrelser    = IF ArtBas.Pakke
                                         THEN FALSE
                                       ELSE IF ArtBas.StrTypeId = 1
                                         THEN FALSE 
                                      ELSE TRUE
                ArtBas.LevNr         = VPIArtBas.LevNr      
                ArtBas.Aktivert      = TRUE
                ArtBas.Beskr         = VPIArtBas.Beskr
                ArtBas.BongTekst     = (IF VPIArtBas.BongTekst <> ""
                                          THEN VPIArtBas.BongTekst  
                                          ELSE SUBSTRING(VPIArtBas.Beskr,1,30))
                ArtBas.VmId          = VPIArtBas.VmId      
                ArtBas.Notat         = VPIArtBas.Notat  
                ArtBas.Farg          = VPIArtBas.Farg
                ArtBas.ValKod        = VPIArtBas.ValKod
                .
        END.
        /* Oppdaterer annen info på eksisterende artikler. */
        ASSIGN
            ArtBas.VPIBildeKode   = VPIArtBas.VPIBildekode
            ArtBas.KatalogPris    = VPIArtBas.KatalogPris[1]
            ArtBas.ForhRab%       = VPIArtBas.forhRab%[1]
            ArtBas.SupRab%        = VPIArtBas.suppRab%[1]
            ArtBas.VPIDato        = VPIArtBas.VPIDato
            ArtBas.LevKod         = VPIArtBas.LevKod   
            ArtBas.Anbefalt       = VPIArtBAs.AnbefaltPris
            ArtBas.LevDato1       = VPIArtBas.LevDato1
            ArtBas.LevDato2       = VPIArtBas.LevDato2
            ArtBas.LevDato3       = VPIArtBas.LevDato3
            ArtBas.LevDato4       = VPIArtBas.LevDato4
            ArtBas.Beskr          = VPIArtBas.Beskr
            ArtBas.BongTekst      = (IF VPIArtBas.BongTekst <> ""
                                     THEN VPIArtBas.BongTekst  
                                     ELSE SUBSTRING(VPIArtBas.Beskr,1,30))
            ArtBas.AnonseArtikkel = VPIArtBas.AnonseArtikkel
            ArtBas.VmId          = VPIArtBas.VmId      
            ArtBas.ValKod        = VPIArtBas.ValKod
            ArtBas.StrKode1      = VPIArtBas.StrKode1
            ArtBas.StrKode2      = VPIArtBas.StrKode2
            ArtBas.LevVaretekst  = VPIArtBas.LevVareTekst
            ArtBas.AntIPakn      = VPIArtBas.AntIPkn
            ArtBas.Gjennomfaktureres = IF ArtBas.Gjennomfaktureres THEN ArtBas.Gjennomfaktureres ELSE VPIArtBas.Gjennomfaktureres
            ArtBas.Kjedevare         = IF ArtBas.Kjedevare = TRUE THEN ArtBas.KjedeVare ELSE VPIArtBas.Kjedevare
            ArtBas.ManRabIKas    = TRUE
            ArtBas.ModellFarge   = VPIArtBas.ModellFarge
            ArtBas.HovedmodellFarge = VPIArtBas.HovedmodellFarge
            ArtBas.EkstStrTypeNavn  = VPIArtBas.EkstStrTypeNavn
            ArtBas.Lager            = (IF ArtBas.Opris THEN FALSE ELSE TRUE)
            ArtBas.LevFargKod       = VPIArtBas.LevFargKod
            ArtBas.Etikettekst1     = VPIArtBas.Etikettekst1
            .

        /* Opprettelse av Strekkoder.                                            */
        /* Må gjøres her under ny, da vi må kunne teste på om StrType må endres. */
        IF bAuto THEN
        DO:
            IF piEkstVPILevNr >= 100 THEN DO:                
                RUN OpprettStrekkoder (INPUT piEkstVPILevNr, STRING(ArtBas.ArtikkelNr), ArtBas.ArtikkelNr).
            RUN genStrekKode.p (ArtBas.ArtikkelNr,IF cGenEan = "yes" THEN 1 ELSE 0,""). 
            END.
            ELSE DO:
                RUN OpprettHKStrekkoder (INPUT piEkstVPILevNr, INPUT VPIArtBas.VareNr, INPUT VPIArtBas.ArtikkelNr).
                RUN genStrekKode.p (INPUT VPIArtBas.VareNr,IF cGenEan = "yes" THEN 1 ELSE 0,""). 
            END.
        END.
    END.

    /* Håndtering av varemerke og valutakode */
    REGISTERFIX:
    DO:
        IF NOT CAN-FIND(VareMerke WHERE
                        VareMerke.VmId = ArtBas.VmId) THEN
        DO:
            FIND FIRST VareMerke NO-LOCK NO-ERROR.
            IF AVAILABLE VareMerke THEN
                ArtBas.VmId = VareMerke.VmId.
        END.
        FIND LevBas NO-LOCK WHERE LevBas.LevNr = ArtBas.LevNr NO-ERROR.
        IF AVAILABLE LevBas THEN
            ArtBas.ValKod = LevBas.ValKod.
    END. /* REGISTERFIX */

    /* Oppretter lagerposter */
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik > 0:
        IF NOT CAN-FIND(Lager WHERE
                        Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                        Lager.Butik      = Butiker.Butik) THEN
        DO:
            CREATE Lager.
            ASSIGN
                Lager.ArtikkelNr = ArtBas.ArtikkelNr
                Lager.Butik      = Butiker.Butik
                .
        END.
    END.

    /* Oppretter kalkyle for sentrallageret hvis det ligger VPIArtPris. */
    IF NOT CAN-FIND(ArtPris WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = clButiker.ProfilNr) THEN
    DO:
        FIND bVPIArtPris NO-LOCK WHERE
            bVPIArtPris.EkstVpiLevNr = VPIArtBas.EkstVPILevNr AND
            bVPIArtPris.VareNr       = VPIArtBas.VareNr AND
            bVPIArtPris.ProfilNr     = clButiker.ProfilNr NO-ERROR.

        IF AVAILABLE bVPIArtPris THEN
        DO:
            CREATE ArtPris.
            ASSIGN
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr 
                ArtPris.ProfilNr   = clButiker.ProfilNr
                .
            BUFFER-COPY bVPIArtPris 
                        EXCEPT ArtikkelNr ProfilNr
                        TO ArtPris.
        END.
    END.

    /* Bilderegister og bildedata */
    RUN OpprettBilder (INPUT piEkstVPILevNr, INPUT VPIArtBas.VareNr, INPUT VPIArtBas.ArtikkelNr).
    /* Erstattningsvarer */
    RUN OpprettErstattningsvarer (INPUT piEkstVPILevNr, INPUT VPIArtBas.VareNr, INPUT VPIArtBas.ArtikkelNr).
    /* Pakkevare */
    IF VPIArtBas.Pakke THEN
        RUN OpprettPakkelinjer (INPUT piEkstVPILevNr, INPUT VPIArtBas.VareNr, INPUT VPIArtBas.ArtikkelNr).

    /* Opprettelse av Strekkoder.                                            */
    /* Må gjøres her under ny, da vi må kunne teste på om StrType må endres. */
    /* Det kan gjøres tidliger når det kjøres AUTO import.                   */
    IF bAuto = FALSE THEN
    DO:
        IF piEkstVPILevNr >= 100 THEN DO:
            RUN OpprettStrekkoder (INPUT piEkstVPILevNr, STRING(ArtBas.ArtikkelNr), ArtBas.ArtikkelNr).
            RUN genStrekKode.p (ArtBas.ArtikkelNr,IF cGenEan = "yes" THEN 1 ELSE 0,""). 
        END.
        ELSE  DO:
            RUN OpprettHKStrekkoder (INPUT piEkstVPILevNr, INPUT VPIArtBas.VareNr, INPUT VPIArtBas.ArtikkelNr).
            RUN genStrekKode.p (INPUT VPIArtBas.ArtikkelNr,IF cGenEan = "yes" THEN 1 ELSE 0,""). 
        END.
    END.

    /* Finnes det nå strekkoder med størrelseskode <> 0 og det ikke allerede */
    /* er tildelt StrType, skal bruker tvinges til å angi en størrelsestype. */
    /*IF VPIArtBas.StrType = 1 THEN*/
    STRTYPE:
    DO WHILE TRUE:
        /* Tvinger bruker til å velge størrelsestype */
        IF VPIArtBas.EkstVPILevNr >= 100 THEN
        VALG:
        DO:
            /* Setter default størrelsestype hvis den er ukjent */
            IF VPIArtBas.StrTypeId <= 1 THEN
                VPIArtBas.StrTypeId = int(cStrTypeId).

            IF bAuto = FALSE THEN
            DO:
                RUN gvelgstrtype.w (INPUT-OUTPUT VPIArtBas.StrTypeId,
                                    INPUT-OUTPUT VPIArtBas.SaSong,
                                    INPUT-OUTPUT VPIArtBas.Vg,
                                    INPUT-OUTPUT VPIArtBas.LevNr,
                                    INPUT-OUTPUT VPIArtBAs.Farg,
                                    INPUT-OUTPUT VPIArtBas.Lager,
                                    INPUT-OUTPUT VPIArtBas.MatKod,
                                    INPUT-OUTPUT VPIArtBas.ProdNr,
                                    INPUT-OUTPUT VPIArtBas.VmId,
                                    INPUT-OUTPUT VPIArtBas.ValKod).
                IF RETURN-VALUE = "AVBRYT" THEN
                DO:
                  ASSIGN
                    pcAvbrudd = "Opprettelse av artikkel er avbrutt.".
                  UNDO OPPRETTELSE, LEAVE OPPRETTELSE.
                END.
            END.
            FIND VarGr NO-LOCK WHERE
                VarGr.Vg = VPIArtBas.VG NO-ERROR.
            IF AVAILABLE VarGr THEN
            DO:
                FIND Moms OF VarGr NO-LOCK NO-ERROR.
                ASSIGN
                VPIArtBas.Hg = VarGr.Hg
                ArtBas.Hg    = VarGr.Hg
                .
            END.
            {syspara.i 2 4 10 cTekst}
            ASSIGN
                VPIArtBas.SalgsEnhet = IF VPIArtBas.SalgsEnhet = ""
                                         THEN ENTRY(1,cTekst)
                                         ELSE VPIArtBas.SalgsEnhet
                .
            ASSIGN
                ArtBas.LopNr         = IF VPIArtBas.Vg <> ArtBas.Vg
                                         THEN ?
                                         ELSE ArtBas.LopNr
                ArtBas.StrTypeId     = VPIArtBas.STrTypeId
                ArtBas.LevNr         = VPIArtBas.LevNr
                ArtBas.Vg            = VPIArtBas.Vg
                ArtBas.Farg          = VPIArtBas.Farg
                ArtBas.Lager         = VPIArtBas.Lager
                ArtBas.MatKod        = VPIArtBas.MatKod
                ArtBas.ProdNr        = VPIArtBas.ProdNr
                ArtBas.VmId          = VPIArtBas.VmId
                ArtBas.ValKod        = CAPS(VPIArtBas.ValKod)
                ArtBas.SaSong        = VPIArtBas.SaSong
                ArtBas.SalgsEnhet    = VPIArtBas.SalgsEnhet
                .
            /* Tildeler løpenummer pånytt hvis varegruppe er endret. */
            IF ArtBas.LopNr = ? THEN
                RUN SetLopeNr2 (OUTPUT ArtBas.LopNr).
        END. /* VALG */

        IF bAuto = FALSE THEN
        DO:
            /*
            IF ArtBas.StrTypeId >= 1 THEN
            DO:
                /* Gyldig StrType */
                IF NOT CAN-FIND(StrType WHERE
                            StrType.StrTypeId = ArtBas.StrTypeId) THEN
                DO:
                    MESSAGE "Ugyldig størrelsestype."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    NEXT STRTYPE.
                END.
                ELSE DO:
                    LEAVE STRTYPE.
                END.
            END.
            ELSE DO: /* Pakkevarer */
                MESSAGE "Angi en størrelsestype <> 0 og 1."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
            */
            LEAVE STRTYPE.
        END.
        ELSE /* Forlater blokken i god tro. */
            LEAVE STRTYPE.
    END. /* STRTYPE */
  END. /* OPPRETTELSE TRANSACTION */
  RETURN pcAvbrudd.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPakkelinjer dTables  _DB-REQUIRED
PROCEDURE OpprettPakkelinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNr       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO.

  PAKKELINJE:
  FOR EACH VPIPakkelinje NO-LOCK WHERE
      VPIPakkelinje.EkstVPILevNr = piEkstVPILevNr AND
      VPIPakkeLinje.VareNr       = pcVareNr:

      FIND Pakkelinje EXCLUSIVE-LOCK WHERE
          Pakkelinje.ArtikkelNr   = VPIPakkelinje.ArtikkelNr AND
          Pakkelinje.PkArtikkelNr = VPIPakkelinje.PkArtikkelNr AND
          Pakkelinje.StrKode      = VPIPakkelinje.StrKode           
          NO-ERROR.
      IF NOT AVAILABLE PakkeLinje THEN
      DO:
          CREATE PakkeLinje.
      END.
      BUFFER-COPY VPIPakkeLinje TO PakkeLinje.
      RELEASE Pakkelinje.
  END. /* PAKKELINJE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettStrekkoder dTables  _DB-REQUIRED
PROCEDURE OpprettStrekkoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
    DEF INPUT PARAMETER pcListe        AS CHAR NO-UNDO. /* VPIArtBas.VAreNr  */
    DEF INPUT PARAMETER plArtikkelNr   AS DEC  NO-UNDO. /* Artbas.ArtikkelNr */

    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcEAN   AS CHAR NO-UNDO.

    DEF BUFFER bVPIArtBas FOR VPIArtBas.

    /* Oppretter strekkoder og setter ArtikkelNr  */
    /* Her kommer alle strekkoder med..           */
    /* Det kommer inn bare ett og ett artikkelnr. */
    STREKKODER:
    DO FOR bVPIArtBas piLoop = 1 TO NUM-ENTRIES(pcListe,CHR(1)):

        FIND bVPIArtBas EXCLUSIVE-LOCK WHERE
             bVPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
             bVPIArtBas.VareNr       = ENTRY(piLoop,pcListe,CHR(1))
            NO-ERROR.

        /* Legger opp strekkoder. */
        STREKKODER-2:
        FOR EACH VPIStrekkode OF bVPIArtBas NO-LOCK
            WHERE VPISTrekkode.KodeType < 9:
            IF bHk = FALSE THEN
            DO:
                /* Bedriftsinterne EAN skal ikke kunne hentes inn fra VPI registeret. */
                IF LENGTH(VPIStrekkode.Kode) = 13 AND
                   SUBSTRING(VPIStrekkode.Kode,1,2) = "02" THEN
                  NEXT STREKKODER-2.
            END.

            /* Blanke koder skal ikke legges opp */
            IF VPIStrekkode.Kode = "" THEN
                NEXT STREKKODER-2.

            /* Skal strekkoden slettes fordi den ligger på feil artikkel? */
            PA-FEIL-ARTIKKEL:
            DO:
                FIND Strekkode WHERE
                    Strekkode.Kode = trim(VPIStrekkode.Kode) EXCLUSIVE-LOCK NO-ERROR.
                /* Ligger strekkoden på feil artikkel skal den slettes. */
                IF AVAILABLE Strekkode AND Strekkode.ArtikkelNr <> bVPIArtBas.ArtikkelNr THEN
                    DELETE Strekkode.
                /* Har strekkoden blitt flyttet til annen størrelse, skal det endres. */
                IF AVAILABLE Strekkode AND
                    Strekkode.StrKode <> VPISTrekkode.StrKode THEN
                    DELETE Strekkode.
                IF AVAILABLE Strekkode THEN RELEASE Strekkode.
            END.

            IF NOT CAN-FIND(Strekkode WHERE
                            Strekkode.Kode = trim(VPIStrekkode.Kode)) THEN
            DO:
                CREATE Strekkode.
                BUFFER-COPY VPIStrekkode TO Strekkode
                    ASSIGN
                    Strekkode.Kode       = TRIM(VPIStrekkode.Kode)
                    Strekkode.ArtikkelNr = plArtikkelNr
                    Strekkode.VareId     = plArtikkelNr
                    Strekkode.HovedNr    = FALSE
                    /*Strekkode.Bestillingsnummer = VPIStrekkode.EkstStorl*/
                    .
                /* Håndtering av ukjente størrelseskoder. */
                IF  Strekkode.StrKode = 0 OR
                    NOT CAN-FIND(StrKonv WHERE
                                StrKonv.StrKode = Strekkode.StrKode) THEN
                DO:
                    ASSIGN
                        Strekkode.StrKode = 1
                        .
                END.
            END.
            ELSE DO:
                FIND Strekkode EXCLUSIVE-LOCK WHERE
                    Strekkode.Kode = trim(VPIStrekkode.Kode) NO-ERROR.
                IF AVAILABLE Strekkode THEN
                    ASSIGN
                        Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer
                        .
            END.
        END. /* STREKKODER-2 */

        /* Legger opp strekkoder. */
        EAN-128:
        FOR EACH VPIStrekkode OF bVPIArtBas NO-LOCK
            WHERE VPISTrekkode.KodeType = 9:

            ASSIGN
                pcEAN = "02" + 
                        string(VPIStrekkode.VareNr,"9999999") + 
                        STRING(VPIStrekkode.StrKode,"999")
                pcEAN = fixChk(pcEAN)
                .

            /* Henter Strekkoden for å legge på koden i kommentarfeltet. */
            FIND Strekkode EXCLUSIVE-LOCK WHERE
                 Strekkode.Kode = pcEAN NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN
            DO:
                CREATE Strekkode.
                ASSIGN
                    Strekkode.ArtikkelNr        = plArtikkelNr 
                    Strekkode.Kode              = pcEAN
                    Strekkode.StrKode           = VPIStrekkode.StrKode
                    Strekkode.KodeType          = 1
                    Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer
                    .
            END.
            ELSE DO:
                ASSIGN
                    Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer
                    .
            END.
        END. /* EAN-128 */
    END. /* STREKKODER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registerkontroll dTables  _DB-REQUIRED
PROCEDURE Registerkontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*
  IF NOT CAN-FIND(HuvGr WHERE
                  HuvGr.Hg = VPIArtBas.Hg) THEN
      RETURN "VPI Posten har en ugyldig hovedgruppe (Hg: " + STRING(VPIArtBas.Hg) + ").".
  IF NOT CAN-FIND(VarGr WHERE
                  VarGr.Vg = VPIArtBas.Vg) THEN
      RETURN "VPI Posten har en ugyldig varegruppe (Vg: " + STRING(VPIArtBas.Vg) + ").".
  IF NOT CAN-FIND(LevBas WHERE
                  LevBas.LevNr = VPIArtBas.LevNr) THEN
      RETURN "VPI Posten har en ugyldig leverandør (LevNr: " + STRING(VPIArtBas.LevNr) + ").".
  */
  /*
  IF NOT CAN-FIND(Farg WHERE
                  Farg.Farg = VPIArtBas.Farg) THEN
      RETURN "VPI Posten har en ugyldig fargekode (Fargekode: " + STRING(VPIArtBas.Farg) + ").".
  IF NOT CAN-FIND(StrType WHERE
                  StrType.StrTypeId = VPIArtBas.StrTypeId) THEN
      RETURN "VPI Posten har en ugyldig størrelsestype (Størrelsestype: " + STRING(VPIArtBas.StrTypeId) + ").".
  IF NOT CAN-FIND(SaSong WHERE
                  SaSong.SaSong = VPIArtBas.SaSong) THEN
      RETURN "VPI Posten har en ugyldig sesongkode (Sesong: " + STRING(VPIArtBas.SaSong) + ").".
  */
  /*
  IF NOT CAN-FIND(VareMerke WHERE
                  VareMerke.VmId = VPIArtBas.VmId) THEN
      RETURN "VPI Posten har et ugyldig varemerke (Varemerke: " + STRING(VPIArtBas.VmId) + ").".
  IF NOT CAN-FIND(Valuta WHERE
                  Valuta.ValKod = VPIArtBas.ValKod) THEN
      RETURN "VPI Posten har en ugyldig valutakode (Valuta: " + STRING(VPIArtBas.ValKod) + ").".
  ELSE
  */
      RETURN "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLopeNr2 dTables  _DB-REQUIRED
PROCEDURE SetLopeNr2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wLoop AS INT NO-UNDO.

  DEF BUFFER bufArtBas FOR ArtBas.

  FIND LAST bufArtBas NO-LOCK WHERE
      bufArtBas.Vg = ArtBas.Vg AND
      bufArtBas.LopNr < 1000000 USE-INDEX ArtIn NO-ERROR.

  IF AVAILABLE bufArtBas THEN
  FINN-NESTE:  
  REPEAT wLoop = bufArtBas.LopNr + 1 TO 1000000:
  
    IF wLoop = 0 THEN
      NEXT FINN-NESTE.
      
    IF CAN-FIND(bufArtBas NO-LOCK WHERE
      bufArtBas.Vg    = ArtBas.Vg AND
      bufArtBas.LopNr = wLoop) THEN
      DO:
        NEXT FINN-NESTE.
      END.
    ELSE
      LEAVE FINN-NESTE.          
  END. /* FINN-NESTE */
  
  /* Ikke tillatte verdier. */
  IF wLoop > 999999 THEN
      wLoop = ?.
  IF wLoop = 0 THEN
      wLoop = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettAutoImport dTables  _DB-REQUIRED
PROCEDURE SettAutoImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pbAuto AS LOG NO-UNDO.

  ASSIGN
      bAuto = pbAuto
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkVPIPris dTables  _DB-REQUIRED
PROCEDURE SjekkVPIPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piEkstVpiLevNr AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pcVareNr       AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER plPris         AS DEC  NO-UNDO.

  FIND VPIArtBas NO-LOCK WHERE
      VPIArtBas.EkstVpiLevNr = piEkstVpiLevNr AND
      VPIArtBas.VareNr       = pcVareNr NO-ERROR.
  IF NOT AVAILABLE vpiArtBas THEN
      plPris = 0.
  ELSE DO:
      FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE VPIArtPRis THEN
          plPris = VPIArtPris.Pris[IF VPIArtPris.Tilbud THEN 2 ELSE 1].
      ELSE
          plPris = 0.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettVPIRegister dTables  _DB-REQUIRED
PROCEDURE SlettVPIRegister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  RUN SlettVPIRegister IN h_dvpiartbas (INPUT piEkstVPILevNr).
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcListe        AS CHAR NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  DEF BUFFER bufVPIArtBas FOR VPIArtBas.

  IF pcListe <> "" THEN
  SLETTING:
  DO piLoop = 1 TO NUM-ENTRIES(pcListe,CHR(1)):
      DO TRANSACTION:
          FIND VPIArtBas EXCLUSIVE-LOCK WHERE
              VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
              VPIArtBas.VareNr = ENTRY(piLoop,pcListe,CHR(1)) NO-ERROR.
          IF NOT AVAILABLE VPIArtBas THEN
              LEAVE SLETTING.

          FOR EACH VPIArtPris OF VPIArtBas EXCLUSIVE-LOCK:
              DELETE VPIArtPRis.
          END.

          FOR EACH VPIStrekkode OF VPIArtBas EXCLUSIVE-LOCK:
              DELETE VPIStrekkode.
          END.

          FOR EACH VPIBildeRegister WHERE 
                   VPIBildeRegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
                   VPIBildeRegister.VareNr       = VPIArtBas.VareNr AND
                   VPIBildeRegister.BildNr       = VPIArtBas.BildNr:
              FOR EACH VPIBildeData OF VPIBildeRegister:
                  DELETE VPIBildeData.
              END.
              DELETE VPIBildeRegister.
          END.

          FOR EACH VPIErstattningsvare WHERE
              VPIErstattningsvare.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
              VPIErstattningsvare.VareNr       = VPIArtBas.VareNr AND
              VPIErstattningsvare.ArtikkelNr   = VPIArtBas.ArtikkelNr:
              DELETE VPIErstattningsvare.
          END.

          FOR EACH VPIPakkeLinje OF VPIArtBas:
              DELETE VPIPakkelinje.
          END.

          DELETE VPIArtBas.
          ASSIGN piLoop = piLoop + 1.
          STATUS DEFAULT "Antall slettede VPI artikler " + STRING(piLoop) + ".".

      END. /* TRANSACTION */
      STATUS DEFAULT "".
  END. /* SLETTING */

  ELSE 
  SLETTALLE:
  DO:
      DEF VAR hField1  AS HANDLE NO-UNDO.
      DEF VAR hField2  AS HANDLE NO-UNDO.
      DEF VAR wQY          AS CHAR     NO-UNDO.
      DEF VAR iHt          AS INTEGER  NO-UNDO.
      DEF VAR hQuery   AS HANDLE NO-UNDO.
      DEF VAR hBuffer  AS HANDLE NO-UNDO.

      CREATE QUERY  hQuery.
      CREATE BUFFER hBuffer FOR TABLE 'vpiArtBas'.

      hQuery:SET-BUFFERS(hBuffer).
      hQuery:QUERY-PREPARE(DYNAMIC-FUNCTION('getQueryWhere':U)).
      hQuery:QUERY-OPEN().

      STATUS DEFAULT "".
      REPEAT:
         hQuery:GET-NEXT() NO-ERROR.
         IF NOT hBuffer:AVAILABLE THEN 
         DO:
             LEAVE.
         END.
         ASSIGN hField1 = hBuffer:BUFFER-FIELD("EkstVPILevNr")
                hField2 = hBuffer:BUFFER-FIELD("VareNr").
      /*       IF hField3:BUFFER-VALUE() = FALSE THEN */
      /*           NEXT. */
         FOR EACH VPIArtPris WHERE 
             VPIArtPRis.EkstVPILevNr = INT(hField1:STRING-VALUE()) AND
             VPIArtPris.VareNr = (hField2:STRING-VALUE()) :
             DELETE VPIArtPris.
         END.

         FOR EACH VPIStrekkode WHERE 
             VPIStrekkode.EkstVPILevNr = INT(hField1:STRING-VALUE()) AND
             VPIStrekkode.VareNr = (hField2:STRING-VALUE()) :
             DELETE VPIStrekkode.
         END.

         FIND bufVPIArtBas WHERE
             bufVPIArtBas.EkstVPILevNr = INT(hField1:STRING-VALUE()) AND
             bufVPIArtBas.VareNr   = (hField2:STRING-VALUE()) NO-ERROR.
         IF AVAILABLE bufVPIArtBas THEN
             DELETE bufVPIArtBas.

         ASSIGN piLoop = piLoop + 1.
         STATUS DEFAULT "Antall slettede VPI artikler " + STRING(piLoop) + ".".
      END.
      STATUS DEFAULT "".
  END. /* SLETTALLE */
                                                              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ByttElement dTables  _DB-REQUIRED
FUNCTION ByttElement RETURNS CHARACTER
  ( INPUT ipSkjerm AS CHAR,
    INPUT ipElement AS INT,
    INPUT ipNyttElement AS CHAR,
    INPUT ipDelimiter AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ipLoop  AS INT NO-UNDO.
  DEF VAR ipTekst AS CHAR NO-UNDO.
  
  ipTekst = "".
  DO ipLoop = 1 TO NUM-ENTRIES(ipSkjerm,ipDelimiter):
    ASSIGN ipTekst = ipTekst + 
           (IF ipTekst = ""
              THEN ""
              ELSE ipDelimiter) +
           (IF ipLoop = ipElement 
              THEN ipNyttElement
              ELSE ENTRY(ipLoop,ipSkjerm,ipDelimiter)). 
  END.

  RETURN ipTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EndretInfo dTables  _DB-REQUIRED
FUNCTION EndretInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = "Opprettet: " + 
                (IF RowObject.RegistrertDato <> ? 
                   THEN STRING(RowOBject.RegistrertDato)
                   ELSE "") + " " + 
                string(RowObject.RegistrertTid,"HH:MM:SS") + " " + 
                STRING(RowObject.RegistrertAv) + " " +
                "Endret: " + 
                (IF RowObject.EDato <> ? 
                   THEN STRING(RowOBject.EDato)
                   ELSE "") + " " + 
                STRING(RowObject.ETid,"HH:MM:SS") + " " +
                RowObject.BrukerId
      .

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FinnesLokalt dTables  _DB-REQUIRED
FUNCTION FinnesLokalt RETURNS LOGICAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pbSvar AS LOG NO-UNDO.

  IF CAN-FIND(ArtBas WHERE
              ArtBas.ArtikkelNr = plArtikkelNr) THEN
      RETURN TRUE.
  ELSE
      RETURN FALSE.   /* Function return value. */

/*   IF VPIArtBas.EkstVPILevNr >= 100 THEN                           */
/*   DO:                                                             */
/*       IF CAN-FIND(ArtBas WHERE                                    */
/*                   ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr) /* OR */
/*          CAN-FIND(Strekkode WHERE                                 */
/*                   Strekkode.Kode = TRIM(VPIArtBas.VareNr)) OR     */
/*          CAN-FIND(Strekkode WHERE                                 */
/*                   Strekkode.Kode = VPIArtBas.VareNr) */ THEN      */
/*           RETURN TRUE.                                            */
/*       ELSE                                                        */
/*           RETURN FALSE.   /* Function return value. */            */
/*   END.                                                            */
/*   /* HK Sjekk */                                                  */
/*   ELSE DO:                                                        */
/*       IF CAN-FIND(ArtBas WHERE                                    */
/*                   ArtBas.ArtikkelNr = VPIArtBas.ArtikkelNr) THEN  */
/*           RETURN TRUE.                                            */
/*       ELSE                                                        */
/*           RETURN FALSE.   /* Function return value. */            */
/*   END.                                                            */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChk dTables  _DB-REQUIRED
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
        DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
            ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                   iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
        END.
        RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuGetInnkjopsPris dTables  _DB-REQUIRED
FUNCTION fuGetInnkjopsPris RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
  IF AVAILABLE VPIArtPris THEN
      RETURN VPIArtPris.InnkjopsPris[IF VPIArtPris.Tilbud
                                       THEN 2
                                       ELSE 1].
  ELSE
      RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuGetLevNavn dTables  _DB-REQUIRED
FUNCTION fuGetLevNavn RETURNS CHARACTER
  ( INPUT piLevNr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LevBas NO-LOCK WHERE
      LevBas.LevNr = piLevNr NO-ERROR.
  IF AVAILABLE LevBas THEN
      RETURN LevBas.LevNamn.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuGetPris dTables  _DB-REQUIRED
FUNCTION fuGetPris RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST VPIArtPris OF VPIArtBas NO-LOCK NO-ERROR.
  IF AVAILABLE VPIArtPris THEN
      RETURN VPIArtPris.Pris[IF VPIArtPris.Tilbud
                               THEN 2
                               ELSE 1].
  ELSE
      RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuVg dTables  _DB-REQUIRED
FUNCTION fuVg RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE ArtBas OR
      ArtBas.ArtikkelNr <> RowObject.ArtikkelNr THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-ERROR.
      
  IF AVAILABLE ArtBas THEN
      RETURN ArtBas.Vg.
  ELSE
      RETURN 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuVgBeskr dTables  _DB-REQUIRED
FUNCTION fuVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  IF NOT AVAILABLE ArtBas OR
      ArtBas.ArtikkelNr <> RowObject.ArtikkelNr THEN
      FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-ERROR.
  
  IF AVAILABLE ArtBas THEN
      FIND VarGr NO-LOCK WHERE
      VarGr.Vg = ArtBas.Vg NO-ERROR.
  IF AVAILABLE VarGr THEN
      RETURN STRING(VarGr.Vg) + " " + VarGr.VgBeskr.
  ELSE IF AVAILABLE ArtBas THEN
      RETURN STRING(ArtBas.Vg).
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLopeNr dTables  _DB-REQUIRED
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wLoop AS INT NO-UNDO.
      
  DEF BUFFER bufArtBas FOR ArtBas.

  FINN-NESTE:  
  REPEAT wLoop = 1 TO 10000:
  
    IF wLoop = 0 THEN
      NEXT FINN-NESTE.
      
    IF CAN-FIND(bufArtBas NO-LOCK WHERE
      bufArtBas.Vg    = ArtBas.Vg AND
      bufArtBas.LopNr = wLoop) THEN
      DO:
        NEXT FINN-NESTE.
      END.
    ELSE
      LEAVE FINN-NESTE.          
  END. /* FINN-NESTE */
  
  IF wLoop > 9999 THEN
      RETURN ?.
  ELSE
      RETURN wLoop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

