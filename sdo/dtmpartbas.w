&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tmpArtBas NO-UNDO LIKE tmpArtBas.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999

    IF bOK AND bTrans NE ? AND (dFraTrans NE ? OR dTilTrans NE ?) THEN 

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



{tmp2artbasdef.i &NEW=NEW}  

DEF VAR iLager                  AS INT  NO-UNDO.
DEF VAR cButikerIdList          AS CHAR NO-UNDO.
DEF VAR cButikerIdList-2        AS CHAR NO-UNDO.
DEF VAR iTilbud                 AS INT  NO-UNDO.
DEF VAR cPrisprofilIdList       AS CHAR NO-UNDO.
DEF VAR cKategoriIdList         AS CHAR NO-UNDO.
DEF VAR bSalg                   AS LOG  NO-UNDO.
DEF VAR bTrans             AS LOG  NO-UNDO.
DEF VAR dFraSalg                AS DATE NO-UNDO.
DEF VAR dTilSalg                AS DATE NO-UNDO.
DEF VAR iBest                   AS INT  NO-UNDO.
DEF VAR dFraBest                AS DATE NO-UNDO.
DEF VAR dTilBest                AS DATE NO-UNDO.
DEF VAR dFraTrans          AS DATE NO-UNDO.
DEF VAR dTilTrans          AS DATE NO-UNDO.
DEF VAR cBestStatIdList         AS CHAR NO-UNDO.
DEF VAR iBestType               AS INT  NO-UNDO.
DEF VAR iBekreftet              AS INT NO-UNDO.
DEF VAR cKategoriList           AS CHAR NO-UNDO.
DEF VAR cAktivitetIdList        AS CHAR NO-UNDO.
DEF VAR cMesseIdList            AS CHAR NO-UNDO.
DEF VAR cVarebokIdList          AS CHAR NO-UNDO.
DEF VAR cKampanjeIdList         AS CHAR NO-UNDO.
DEF VAR cKampanjeIdList-2       AS CHAR NO-UNDO.
DEF VAR cKombKampanjeIdList     AS CHAR NO-UNDO.
DEFINE VARIABLE cAnv-KodIdList AS CHARACTER NO-UNDO.
DEF VAR cStrekkode              AS CHAR NO-UNDO.
DEF VAR cBestillingsnummer      AS CHAR NO-UNDO.
DEF VAR cERPNr                  AS CHAR NO-UNDO.
DEFINE VARIABLE iKombKampanje   AS INTEGER NO-UNDO.
DEFINE VARIABLE cButikerIdList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cListArtButiker AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cRAvdNr-IdList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lUtanVareomrade AS LOGICAL     NO-UNDO.
DEF VAR iTotalCount             AS INT NO-UNDO.
DEF VAR cStatusString           AS CHAR NO-UNDO.
DEF VAR bStopSelection          AS LOG NO-UNDO.
DEF VAR bVisQuery               AS LOG NO-UNDO.
DEF VAR ix                      AS INT NO-UNDO.
DEF VAR iTTId                   AS INT NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEF TEMP-TABLE ttArtNr NO-UNDO
    FIELD ArtNr LIKE ProduktFamMedlem.ProdFamArtikkelNr.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Global-define DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES tmpArtBas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AktivAv AktivDato Aktivert Alder AnbefaltPris AnonseArtikkel anv-id~
 ArtikkelNr ArtSlag BehKode Beskr BestForslag BildeIKasse BildNr BongTekst~
 BrukerID Dato1gSendtHk EDato ETid Etikett Etikettekst1 EtiLayout Farg~
 foder-id forhRab% GarantiKl Hg HKArtikkelNr HkStyrt HKVareId~
 HovedModellFarge IKasse IndividType inner-id inn_dato KatalogPris KjedeVare~
 KjentPaHK Klack Kommentar KundeRabatt lager LapTop last-id LevDato1~
 LevDato2 LevDato3 LevDato4 LevFargKod LevKod LevNr LinjeMerknad LinkVareNr~
 LokPris LopNr ManRabIKas MatKod Mengde ModellFarge Notat ny_dato OLLager~
 OPris ov-id Pakke Pakkenr Pant PrisGrpNr ProdNr ProvKod RabKod RegistrertAv~
 RegistrertDato RegistrertTid SalgsEnhet SaSong SattPaKampanje~
 SentralBestilling SlaskArtikkelNr Slasket slit-id Storrelser StrTypeID~
 supRab% Gjennomfaktureres Tilv-Land valkod Vg VgKat VMId VPIBildeKode~
 VPIDato Lokasjon RAvdNr
&Scoped-define ENABLED-FIELDS-IN-tmpArtBas AktivAv AktivDato Aktivert Alder ~
AnbefaltPris AnonseArtikkel anv-id ArtikkelNr ArtSlag BehKode Beskr ~
BestForslag BildeIKasse BildNr BongTekst BrukerID Dato1gSendtHk EDato ETid ~
Etikett Etikettekst1 EtiLayout Farg foder-id forhRab% GarantiKl Hg ~
HKArtikkelNr HkStyrt HKVareId HovedModellFarge IKasse IndividType inner-id ~
inn_dato KatalogPris KjedeVare KjentPaHK Klack Kommentar KundeRabatt lager ~
LapTop last-id LevDato1 LevDato2 LevDato3 LevDato4 LevFargKod LevKod LevNr ~
LinjeMerknad LinkVareNr LokPris LopNr ManRabIKas MatKod Mengde ModellFarge ~
Notat ny_dato OLLager OPris ov-id Pakke Pakkenr Pant PrisGrpNr ProdNr ~
ProvKod RabKod RegistrertAv RegistrertDato RegistrertTid SalgsEnhet SaSong ~
SattPaKampanje SentralBestilling SlaskArtikkelNr Slasket slit-id Storrelser ~
StrTypeID supRab% Gjennomfaktureres Tilv-Land valkod Vg VgKat VMId ~
VPIBildeKode VPIDato Lokasjon RAvdNr 
&Scoped-Define DATA-FIELDS  fuVarekost AktivAv AktivDato Aktivert Alder fuAktivvarekost AnbefaltPris~
 AnonseArtikkel anv-id ArtikkelNr ArtSlag fuAktivPris BehKode Beskr~
 BestForslag BildeIKasse fuTilbud BildNr BongTekst BrukerID Dato1gSendtHk~
 EDato ETid Etikett Etikettekst1 fuPris EtiLayout Farg foder-id forhRab%~
 GarantiKl Hg HKArtikkelNr HkStyrt fiSasong HKVareId HovedModellFarge IKasse~
 IndividType inner-id inn_dato KatalogPris KjedeVare fuLevNavn KjentPaHK~
 Klack Kommentar KundeRabatt lager LapTop last-id LevDato1 fVgBeskr LevDato2~
 LevDato3 LevDato4 LevFargKod LevKod LevNr LinjeMerknad LinkVareNr LokPris~
 LopNr ManRabIKas MatKod Mengde ModellFarge Notat ny_dato OLLager OPris~
 ov-id Pakke Pakkenr Pant PrisGrpNr ProdNr ProvKod RabKod RegistrertAv~
 RegistrertDato RegistrertTid SalgsEnhet SaSong SattPaKampanje~
 SentralBestilling SlaskArtikkelNr Slasket slit-id Storrelser StrTypeID~
 supRab% Gjennomfaktureres Tilv-Land valkod Vg VgKat VMId VPIBildeKode~
 VPIDato Lokasjon RAvdNr
&Scoped-define DATA-FIELDS-IN-tmpArtBas AktivAv AktivDato Aktivert Alder ~
AnbefaltPris AnonseArtikkel anv-id ArtikkelNr ArtSlag BehKode Beskr ~
BestForslag BildeIKasse BildNr BongTekst BrukerID Dato1gSendtHk EDato ETid ~
Etikett Etikettekst1 EtiLayout Farg foder-id forhRab% GarantiKl Hg ~
HKArtikkelNr HkStyrt HKVareId HovedModellFarge IKasse IndividType inner-id ~
inn_dato KatalogPris KjedeVare KjentPaHK Klack Kommentar KundeRabatt lager ~
LapTop last-id LevDato1 LevDato2 LevDato3 LevDato4 LevFargKod LevKod LevNr ~
LinjeMerknad LinkVareNr LokPris LopNr ManRabIKas MatKod Mengde ModellFarge ~
Notat ny_dato OLLager OPris ov-id Pakke Pakkenr Pant PrisGrpNr ProdNr ~
ProvKod RabKod RegistrertAv RegistrertDato RegistrertTid SalgsEnhet SaSong ~
SattPaKampanje SentralBestilling SlaskArtikkelNr Slasket slit-id Storrelser ~
StrTypeID supRab% Gjennomfaktureres Tilv-Land valkod Vg VgKat VMId ~
VPIBildeKode VPIDato Lokasjon RAvdNr 
&Scoped-Define MANDATORY-FIELDS  GarantiKl ModellFarge PrisGrpNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dtmpartbas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH tmpArtBas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH tmpArtBas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main tmpArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main tmpArtBas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD byggTempTbl dTables 
FUNCTION byggTempTbl RETURNS LOGICAL
        (  INPUT icListe AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD deleteRow dTables  _DB-REQUIRED
FUNCTION deleteRow RETURNS LOGICAL
  ( INPUT pcRowIdent AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuAktivpris dTables  _DB-REQUIRED
FUNCTION fuAktivpris RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuAktivvarekost dTables  _DB-REQUIRED
FUNCTION fuAktivvarekost RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuDb% dTables  _DB-REQUIRED
FUNCTION fuDb% RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuPris dTables  _DB-REQUIRED
FUNCTION fuPris RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuTilbud dTables  _DB-REQUIRED
FUNCTION fuTilbud RETURNS LOGICAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuVarekost dTables  _DB-REQUIRED
FUNCTION fuVarekost RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getAktivICombKampanje dTables 
FUNCTION getAktivICombKampanje RETURNS LOGICAL
        ( INPUT inpArtikkelNr AS DECIMAL,
          INPUT inpKombKampanjeIdList AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getButikkIdListe dTables  _DB-REQUIRED
FUNCTION getButikkIdListe RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevNavn dTables  _DB-REQUIRED
FUNCTION getLevNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRecordCount dTables  _DB-REQUIRED
FUNCTION getRecordCount RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSasong dTables  _DB-REQUIRED
FUNCTION getSasong RETURNS CHARACTER
  ( INPUT ipSasong AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStatusString dTables  _DB-REQUIRED
FUNCTION getStatusString RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTmpArtBasHandle dTables  _DB-REQUIRED
FUNCTION getTmpArtBasHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVgBeskr dTables  _DB-REQUIRED
FUNCTION getVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setSelectorFilter dTables  _DB-REQUIRED
FUNCTION setSelectorFilter RETURNS LOGICAL
  ( INPUT iiLager            AS INT,
    INPUT icButikerIdList    AS CHAR,
    INPUT iiTilbud           AS INT,
    INPUT icPrisprofilIdList AS CHAR,
    INPUT ibSalg             AS LOG,
    INPUT idFraSalg          AS DATE,
    INPUT idTilSalg          AS DATE,
    INPUT iiBest             AS INT,
    INPUT idFraBest          AS DATE,
    INPUT idTilBest          AS DATE,
    INPUT icBestStatIdList   AS CHAR,
    INPUT iiBestType         AS INT,
    INPUT iiBekreftet        AS INT,
    INPUT icKategoriList     AS CHAR,
    INPUT icAktivitetIdList  AS CHAR,
    INPUT icMesseIdList      AS CHAR,
    INPUT icVarebokIdList    AS CHAR,
    INPUT icKampanjeIdList   AS CHAR,
    INPUT icStrekkode        AS CHAR,
    INPUT icBestillingsnummer AS CHAR,
    INPUT icERPNr            AS CHAR,
    INPUT ibTrans       AS LOG,
    INPUT idFraTrans    AS DATE,
    INPUT idTilTrans    AS DATE,
    INPUT icButikerIdList-2  AS CHAR,
    INPUT iiTTId             AS INT,
    INPUT icKombKampanjeIdList AS CHARACTER, 
    INPUT iiKombKampanje     AS INTEGER,
    INPUT icButikerIdList-3  AS CHAR,
    INPUT fi-cListArtButiker AS CHAR,
    INPUT icRAvdNr-IdList     AS CHAR,
    INPUT ilUtanVareomrade  AS LOG,
    INPUT icAnv-KodIdList AS CHARACTER 
    )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVisQuery dTables  _DB-REQUIRED
FUNCTION setVisQuery RETURNS LOGICAL
  ( INPUT ibVisQuery AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD sjekkBestilling dTables  _DB-REQUIRED
FUNCTION sjekkBestilling RETURNS LOGICAL
  ( INPUT idArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkTrans dTables  _DB-REQUIRED
FUNCTION SjekkTrans RETURNS LOGICAL
  ( INPUT idArtikkelNr AS DEC,
    INPUT iiButikkNr   AS INT,
    INPUT i2TTId       AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD stopSelection dTables  _DB-REQUIRED
FUNCTION stopSelection RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      tmpArtBas
    FIELDS(tmpArtBas.AktivAv
      tmpArtBas.AktivDato
      tmpArtBas.Aktivert
      tmpArtBas.Alder
      tmpArtBas.AnbefaltPris
      tmpArtBas.AnonseArtikkel
      tmpArtBas.anv-id
      tmpArtBas.ArtikkelNr
      tmpArtBas.ArtSlag
      tmpArtBas.BehKode
      tmpArtBas.Beskr
      tmpArtBas.BestForslag
      tmpArtBas.BildeIKasse
      tmpArtBas.BildNr
      tmpArtBas.BongTekst
      tmpArtBas.BrukerID
      tmpArtBas.Dato1gSendtHk
      tmpArtBas.EDato
      tmpArtBas.ETid
      tmpArtBas.Etikett
      tmpArtBas.Etikettekst1
      tmpArtBas.EtiLayout
      tmpArtBas.Farg
      tmpArtBas.foder-id
      tmpArtBas.forhRab%
      tmpArtBas.GarantiKl
      tmpArtBas.Hg
      tmpArtBas.HKArtikkelNr
      tmpArtBas.HkStyrt
      tmpArtBas.HKVareId
      tmpArtBas.HovedModellFarge
      tmpArtBas.IKasse
      tmpArtBas.IndividType
      tmpArtBas.inner-id
      tmpArtBas.inn_dato
      tmpArtBas.KatalogPris
      tmpArtBas.KjedeVare
      tmpArtBas.KjentPaHK
      tmpArtBas.Klack
      tmpArtBas.Kommentar
      tmpArtBas.KundeRabatt
      tmpArtBas.lager
      tmpArtBas.LapTop
      tmpArtBas.last-id
      tmpArtBas.LevDato1
      tmpArtBas.LevDato2
      tmpArtBas.LevDato3
      tmpArtBas.LevDato4
      tmpArtBas.LevFargKod
      tmpArtBas.LevKod
      tmpArtBas.LevNr
      tmpArtBas.LinjeMerknad
      tmpArtBas.LinkVareNr
      tmpArtBas.LokPris
      tmpArtBas.LopNr
      tmpArtBas.ManRabIKas
      tmpArtBas.MatKod
      tmpArtBas.Mengde
      tmpArtBas.ModellFarge
      tmpArtBas.Notat
      tmpArtBas.ny_dato
      tmpArtBas.OLLager
      tmpArtBas.OPris
      tmpArtBas.ov-id
      tmpArtBas.Pakke
      tmpArtBas.Pakkenr
      tmpArtBas.Pant
      tmpArtBas.PrisGrpNr
      tmpArtBas.ProdNr
      tmpArtBas.ProvKod
      tmpArtBas.RabKod
      tmpArtBas.RegistrertAv
      tmpArtBas.RegistrertDato
      tmpArtBas.RegistrertTid
      tmpArtBas.SalgsEnhet
      tmpArtBas.SaSong
      tmpArtBas.SattPaKampanje
      tmpArtBas.SentralBestilling
      tmpArtBas.SlaskArtikkelNr
      tmpArtBas.Slasket
      tmpArtBas.slit-id
      tmpArtBas.Storrelser
      tmpArtBas.StrTypeID
      tmpArtBas.supRab%
      tmpArtBas.Gjennomfaktureres
      tmpArtBas.Tilv-Land
      tmpArtBas.valkod
      tmpArtBas.Vg
      tmpArtBas.VgKat
      tmpArtBas.VMId
      tmpArtBas.VPIBildeKode
      tmpArtBas.VPIDato
      tmpArtBas.Lokasjon
      tmpArtBas.RAvdNr) SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: tmpArtBas T "NEW SHARED" NO-UNDO Temp-DB tmpArtBas
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
     _TblList          = "Temp-Tables.tmpArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = "USED"
     _FldNameList[1]   > "_<CALC>"
"fuVareKost(RowObject.ArtikkelNr)" "fuVarekost" "Varekost" "->>>>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.8 no ?
     _FldNameList[2]   > Temp-Tables.tmpArtBas.AktivAv
"AktivAv" "AktivAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 no ""
     _FldNameList[3]   > Temp-Tables.tmpArtBas.AktivDato
"AktivDato" "AktivDato" ? ? "date" ? ? ? ? ? ? yes ? no 12 no ""
     _FldNameList[4]   > Temp-Tables.tmpArtBas.Aktivert
"Aktivert" "Aktivert" "A" "*~~/" "logical" ? ? ? ? ? ? yes ? no 1.4 no ""
     _FldNameList[5]   > Temp-Tables.tmpArtBas.Alder
"Alder" "Alder" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 no ""
     _FldNameList[6]   > "_<CALC>"
"fuAktivvarekost(RowObject.ArtikkelNr)" "fuAktivvarekost" ? "->>>>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.8 no ?
     _FldNameList[7]   > Temp-Tables.tmpArtBas.AnbefaltPris
"AnbefaltPris" "AnbefaltPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.6 no ""
     _FldNameList[8]   > Temp-Tables.tmpArtBas.AnonseArtikkel
"AnonseArtikkel" "AnonseArtikkel" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 no ""
     _FldNameList[9]   > Temp-Tables.tmpArtBas.anv-id
"anv-id" "anv-id" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 no ""
     _FldNameList[10]   > Temp-Tables.tmpArtBas.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 no ""
     _FldNameList[11]   > Temp-Tables.tmpArtBas.ArtSlag
"ArtSlag" "ArtSlag" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 no ""
     _FldNameList[12]   > "_<CALC>"
"fuAktivPris(RowObject.ArtikkelNr)" "fuAktivPris" ? "->>>>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.8 no ?
     _FldNameList[13]   > Temp-Tables.tmpArtBas.BehKode
"BehKode" "BehKode" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 no ""
     _FldNameList[14]   > Temp-Tables.tmpArtBas.Beskr
"Beskr" "Beskr" ? "x(40)" "character" ? ? ? ? ? ? yes ? no 40 no ""
     _FldNameList[15]   > Temp-Tables.tmpArtBas.BestForslag
"BestForslag" "BestForslag" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 no ""
     _FldNameList[16]   > Temp-Tables.tmpArtBas.BildeIKasse
"BildeIKasse" "BildeIKasse" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 no ""
     _FldNameList[17]   > "_<CALC>"
"fuTilbud(RowObject.ArtikkelNr)" "fuTilbud" ? "J/N" "Logical" ? ? ? ? ? ? no ? no 1.6 no ?
     _FldNameList[18]   > Temp-Tables.tmpArtBas.BildNr
"BildNr" "BildNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 no ""
     _FldNameList[19]   > Temp-Tables.tmpArtBas.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 no ""
     _FldNameList[20]   > Temp-Tables.tmpArtBas.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 no ""
     _FldNameList[21]   > Temp-Tables.tmpArtBas.Dato1gSendtHk
"Dato1gSendtHk" "Dato1gSendtHk" ? ? "date" ? ? ? ? ? ? yes ? no 15.4 no ""
     _FldNameList[22]   > Temp-Tables.tmpArtBas.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no ""
     _FldNameList[23]   > Temp-Tables.tmpArtBas.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 no ""
     _FldNameList[24]   > Temp-Tables.tmpArtBas.Etikett
"Etikett" "Etikett" ? ? "integer" ? ? ? ? ? ? yes ? no 6 no ""
     _FldNameList[25]   > Temp-Tables.tmpArtBas.Etikettekst1
"Etikettekst1" "Etikettekst1" ? ? "character" ? ? ? ? ? ? yes ? no 30 no ""
     _FldNameList[26]   > "_<CALC>"
"fuPris(RowObject.ArtikkelNr)" "fuPris" ? "->>>>>9.99" "Decimal" ? ? ? ? ? ? no ? no 10.8 no ?
     _FldNameList[27]   > Temp-Tables.tmpArtBas.EtiLayout
"EtiLayout" "EtiLayout" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 no ""
     _FldNameList[28]   > Temp-Tables.tmpArtBas.Farg
"Farg" "Farg" ? "zzzzzz9" "integer" ? ? ? ? ? ? yes ? no 7.2 no ""
     _FldNameList[29]   > Temp-Tables.tmpArtBas.foder-id
"foder-id" "foder-id" ? ? "integer" ? ? ? ? ? ? yes ? no 7 no ""
     _FldNameList[30]   > Temp-Tables.tmpArtBas.forhRab%
"forhRab%" "forhRab%" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.8 no ""
     _FldNameList[31]   > Temp-Tables.tmpArtBas.GarantiKl
"GarantiKl" "GarantiKl" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.4 no ""
     _FldNameList[32]   > Temp-Tables.tmpArtBas.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 13 no ""
     _FldNameList[33]   > Temp-Tables.tmpArtBas.HKArtikkelNr
"HKArtikkelNr" "HKArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 no ""
     _FldNameList[34]   > Temp-Tables.tmpArtBas.HkStyrt
"HkStyrt" "HkStyrt" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 no ""
     _FldNameList[35]   > "_<CALC>"
"getSasong(RowObject.Sasong)" "fiSasong" "Sesong" "x(14)" "character" ? ? ? ? ? ? no ? no 14 no ?
     _FldNameList[36]   > Temp-Tables.tmpArtBas.HKVareId
"HKVareId" "HKVareId" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 no ""
     _FldNameList[37]   > Temp-Tables.tmpArtBas.HovedModellFarge
"HovedModellFarge" "HovedModellFarge" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 no ""
     _FldNameList[38]   > Temp-Tables.tmpArtBas.IKasse
"IKasse" "IKasse" "IK" "*~~/" "logical" ? ? ? ? ? ? yes ? no 2 no ""
     _FldNameList[39]   > Temp-Tables.tmpArtBas.IndividType
"IndividType" "IndividType" ? ? "integer" ? ? ? ? ? ? yes ? no 11 no ""
     _FldNameList[40]   > Temp-Tables.tmpArtBas.inner-id
"inner-id" "inner-id" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 no ""
     _FldNameList[41]   > Temp-Tables.tmpArtBas.inn_dato
"inn_dato" "inn_dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 no ""
     _FldNameList[42]   > Temp-Tables.tmpArtBas.KatalogPris
"KatalogPris" "KatalogPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.4 no ""
     _FldNameList[43]   > Temp-Tables.tmpArtBas.KjedeVare
"KjedeVare" "KjedeVare" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 no ""
     _FldNameList[44]   > "_<CALC>"
"getLevNavn()" "fuLevNavn" "Leverandør" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[45]   > Temp-Tables.tmpArtBas.KjentPaHK
"KjentPaHK" "KjentPaHK" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 no ""
     _FldNameList[46]   > Temp-Tables.tmpArtBas.Klack
"Klack" "Klack" ? ? "integer" ? ? ? ? ? ? yes ? no 4 no ""
     _FldNameList[47]   > Temp-Tables.tmpArtBas.Kommentar
"Kommentar" "Kommentar" ? ? "character" ? ? ? ? ? ? yes ? no 64 no ""
     _FldNameList[48]   > Temp-Tables.tmpArtBas.KundeRabatt
"KundeRabatt" "KundeRabatt" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 no ""
     _FldNameList[49]   > Temp-Tables.tmpArtBas.lager
"lager" "lager" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 no ""
     _FldNameList[50]   > Temp-Tables.tmpArtBas.LapTop
"LapTop" "LapTop" ? ? "logical" ? ? ? ? ? ? yes ? no 19.8 no ""
     _FldNameList[51]   > Temp-Tables.tmpArtBas.last-id
"last-id" "last-id" ? ? "integer" ? ? ? ? ? ? yes ? no 5.4 no ""
     _FldNameList[52]   > Temp-Tables.tmpArtBas.LevDato1
"LevDato1" "LevDato1" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 no ""
     _FldNameList[53]   > "_<CALC>"
"getVgBeskr()" "fVgBeskr" "Varegr" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[54]   > Temp-Tables.tmpArtBas.LevDato2
"LevDato2" "LevDato2" ? ? "date" ? ? ? ? ? ? yes ? no 15 no ""
     _FldNameList[55]   > Temp-Tables.tmpArtBas.LevDato3
"LevDato3" "LevDato3" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 no ""
     _FldNameList[56]   > Temp-Tables.tmpArtBas.LevDato4
"LevDato4" "LevDato4" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 no ""
     _FldNameList[57]   > Temp-Tables.tmpArtBas.LevFargKod
"LevFargKod" "LevFargKod" ? "X(30)" "character" ? ? ? ? ? ? yes ? no 30 no ""
     _FldNameList[58]   > Temp-Tables.tmpArtBas.LevKod
"LevKod" "LevKod" ? ? "character" ? ? ? ? ? ? yes ? no 20 no ""
     _FldNameList[59]   > Temp-Tables.tmpArtBas.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 no ""
     _FldNameList[60]   > Temp-Tables.tmpArtBas.LinjeMerknad
"LinjeMerknad" "LinjeMerknad" ? ? "character" ? ? ? ? ? ? yes ? no 40 no ""
     _FldNameList[61]   > Temp-Tables.tmpArtBas.LinkVareNr
"LinkVareNr" "LinkVareNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 no ""
     _FldNameList[62]   > Temp-Tables.tmpArtBas.LokPris
"LokPris" "LokPris" ? ? "logical" ? ? ? ? ? ? yes ? no 7 no ""
     _FldNameList[63]   > Temp-Tables.tmpArtBas.LopNr
"LopNr" "LopNr" ? "zzzzz9" "integer" ? ? ? ? ? ? yes ? no 12.2 no ""
     _FldNameList[64]   > Temp-Tables.tmpArtBas.ManRabIKas
"ManRabIKas" "ManRabIKas" ? ? "logical" ? ? ? ? ? ? yes ? no 7.8 no ""
     _FldNameList[65]   > Temp-Tables.tmpArtBas.MatKod
"MatKod" "MatKod" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 no ""
     _FldNameList[66]   > Temp-Tables.tmpArtBas.Mengde
"Mengde" "Mengde" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.8 no ""
     _FldNameList[67]   > Temp-Tables.tmpArtBas.ModellFarge
"ModellFarge" "ModellFarge" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 no ""
     _FldNameList[68]   > Temp-Tables.tmpArtBas.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 40 no ""
     _FldNameList[69]   > Temp-Tables.tmpArtBas.ny_dato
"ny_dato" "ny_dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 no ""
     _FldNameList[70]   > Temp-Tables.tmpArtBas.OLLager
"OLLager" "OLLager" ? ? "logical" ? ? ? ? ? ? yes ? no 5.6 no ""
     _FldNameList[71]   > Temp-Tables.tmpArtBas.OPris
"OPris" "OPris" ? ? "logical" ? ? ? ? ? ? yes ? no 5 no ""
     _FldNameList[72]   > Temp-Tables.tmpArtBas.ov-id
"ov-id" "ov-id" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 no ""
     _FldNameList[73]   > Temp-Tables.tmpArtBas.Pakke
"Pakke" "Pakke" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 no ""
     _FldNameList[74]   > Temp-Tables.tmpArtBas.Pakkenr
"Pakkenr" "Pakkenr" ? ? "integer" ? ? ? ? ? ? yes ? no 8 no ""
     _FldNameList[75]   > Temp-Tables.tmpArtBas.Pant
"Pant" "Pant" ? ? "logical" ? ? ? ? ? ? yes ? no 4.4 no ""
     _FldNameList[76]   > Temp-Tables.tmpArtBas.PrisGrpNr
"PrisGrpNr" "PrisGrpNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.8 no ""
     _FldNameList[77]   > Temp-Tables.tmpArtBas.ProdNr
"ProdNr" "ProdNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 no ""
     _FldNameList[78]   > Temp-Tables.tmpArtBas.ProvKod
"ProvKod" "ProvKod" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 no ""
     _FldNameList[79]   > Temp-Tables.tmpArtBas.RabKod
"RabKod" "RabKod" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 no ""
     _FldNameList[80]   > Temp-Tables.tmpArtBas.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 no ""
     _FldNameList[81]   > Temp-Tables.tmpArtBas.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no ""
     _FldNameList[82]   > Temp-Tables.tmpArtBas.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 no ""
     _FldNameList[83]   > Temp-Tables.tmpArtBas.SalgsEnhet
"SalgsEnhet" "SalgsEnhet" ? ? "character" ? ? ? ? ? ? yes ? no 5.6 no ""
     _FldNameList[84]   > Temp-Tables.tmpArtBas.SaSong
"SaSong" "SaSong" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 no ""
     _FldNameList[85]   > Temp-Tables.tmpArtBas.SattPaKampanje
"SattPaKampanje" "SattPaKampanje" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 no ""
     _FldNameList[86]   > Temp-Tables.tmpArtBas.SentralBestilling
"SentralBestilling" "SentralBestilling" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 no ""
     _FldNameList[87]   > Temp-Tables.tmpArtBas.SlaskArtikkelNr
"SlaskArtikkelNr" "SlaskArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 no ""
     _FldNameList[88]   > Temp-Tables.tmpArtBas.Slasket
"Slasket" "Slasket" ? ? "logical" ? ? ? ? ? ? yes ? no 7 no ""
     _FldNameList[89]   > Temp-Tables.tmpArtBas.slit-id
"slit-id" "slit-id" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 no ""
     _FldNameList[90]   > Temp-Tables.tmpArtBas.Storrelser
"Storrelser" "Storrelser" ? ? "logical" ? ? ? ? ? ? yes ? no 8.8 no ""
     _FldNameList[91]   > Temp-Tables.tmpArtBas.StrTypeID
"StrTypeID" "StrTypeID" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 no ""
     _FldNameList[92]   > Temp-Tables.tmpArtBas.supRab%
"supRab%" "supRab%" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.6 no ""
     _FldNameList[93]   > Temp-Tables.tmpArtBas.Gjennomfaktureres
"Gjennomfaktureres" "Gjennomfaktureres" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 6.2 no ""
     _FldNameList[94]   > Temp-Tables.tmpArtBas.Tilv-Land
"Tilv-Land" "Tilv-Land" ? ? "character" ? ? ? ? ? ? yes ? no 20 no ""
     _FldNameList[95]   > Temp-Tables.tmpArtBas.valkod
"valkod" "valkod" ? ? "character" ? ? ? ? ? ? yes ? no 3 no ""
     _FldNameList[96]   > Temp-Tables.tmpArtBas.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 no ""
     _FldNameList[97]   > Temp-Tables.tmpArtBas.VgKat
"VgKat" "VgKat" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 no ""
     _FldNameList[98]   > Temp-Tables.tmpArtBas.VMId
"VMId" "VMId" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 no ""
     _FldNameList[99]   > Temp-Tables.tmpArtBas.VPIBildeKode
"VPIBildeKode" "VPIBildeKode" ? ? "character" ? ? ? ? ? ? yes ? no 30 no ""
     _FldNameList[100]   > Temp-Tables.tmpArtBas.VPIDato
"VPIDato" "VPIDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 no ""
     _FldNameList[101]   > Temp-Tables.tmpArtBas.Lokasjon
"Lokasjon" "Lokasjon" ? ? "character" ? ? ? ? ? ? yes ? no 20 no ""
     _FldNameList[102]   > Temp-Tables.tmpArtBas.RAvdNr
"RAvdNr" "RAvdNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 no ""
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmp2ArtBas dTables  _DB-REQUIRED
PROCEDURE ByggTmp2ArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER table FOR tmp2ArtBas.

DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR iSelectType AS INT NO-UNDO.      /* 1: Alle, 2: Valgte rader (iReturn i cont.) */

hBrowse     = DYNAMIC-FUNCTION("getBrowseHandle" IN DYNAMIC-FUNCTION("getContainerSource")).
iSelectType = DYNAMIC-FUNCTION("getSelectType" IN DYNAMIC-FUNCTION("getContainerSource")).

FOR EACH tmp2ArtBas:
  DELETE tmp2ArtBas.
END.

IF VALID-HANDLE(hBrowse) AND hBrowse:NUM-SELECTED-ROWS > 0 AND iSelectType = 2 THEN
  DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
    IF hBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
      FIND FIRST tmpArtBas
           WHERE tmpArtBas.Artikkelnr = DEC(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
           NO-LOCK NO-ERROR.
      IF AVAIL tmpArtBas THEN DO:
        CREATE tmp2ArtBas.
        ASSIGN
            tmp2ArtBas.ArtikkelNr  = tmpArtBas.ArtikkelNr
            tmp2ArtBas.Beskr       = tmpArtBas.Beskr
            tmp2ArtBas.Lager       = tmpArtBas.Lager
            tmp2ArtBas.ModellFarge = tmpArtBas.ModellFarge
            .
      END.
    END.
  END.
ELSE FOR EACH tmpArtBas NO-LOCK:
  IF NOT CAN-FIND(tmp2ArtBas WHERE
                  tmp2ArtBas.ArtikkelNr = tmpArtBas.ArtikkelNr) THEN
  DO:
    CREATE tmp2ArtBas.
    ASSIGN
        tmp2ArtBas.ArtikkelNr  = tmpArtBas.ArtikkelNr
        tmp2ArtBas.Beskr       = tmpArtBas.Beskr
        tmp2ArtBas.Lager       = tmpArtBas.Lager
        tmp2ArtBas.ModellFarge = tmpArtBas.ModellFarge
        .
  END.
END.

FOR EACH tmp2ArtBas WHERE
    NOT CAN-FIND(ArtBas WHERE
                 ArtBas.ArtikkelNr = tmp2ArtBas.ArtikkelNr):
  DELETE tmp2ArtBas.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.fiSasong = (getSasong(RowObject.Sasong))
         rowObject.fuAktivPris = (fuAktivPris(RowObject.ArtikkelNr))
         rowObject.fuAktivvarekost = (fuAktivvarekost(RowObject.ArtikkelNr))
         rowObject.fuLevNavn = (getLevNavn())
         rowObject.fuPris = (fuPris(RowObject.ArtikkelNr))
         rowObject.fuTilbud = (fuTilbud(RowObject.ArtikkelNr))
         rowObject.fuVarekost = (fuVareKost(RowObject.ArtikkelNr))
         rowObject.fVgBeskr = (getVgBeskr())
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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,INPUT TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nullstill dTables  _DB-REQUIRED
PROCEDURE Nullstill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH tmpArtBas NO-LOCK:
      DELETE tmpArtBas.
  END.
  iTotalCount = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utvalg dTables  _DB-REQUIRED
PROCEDURE Utvalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER lcWhere AS CHAR NO-UNDO.

  DEF BUFFER bArtBas FOR ArtBas.

  DEF VAR hQuery       AS HANDLE  NO-UNDO.
  DEF VAR hBuffer      AS HANDLE  NO-UNDO.
  DEF VAR hBufferField AS HANDLE  NO-UNDO.
  DEF VAR dArtikkelNr  AS DECIMAL NO-UNDO.
  DEF VAR dModellFarge AS DECIMAL NO-UNDO.
  DEF VAR ix           AS INT NO-UNDO.
  DEF VAR bOK          AS LOG NO-UNDO.
  DEF VAR bTmpOk       AS LOG NO-UNDO.
  DEF VAR iCount       AS INT NO-UNDO.
  DEF VAR idx          AS INT NO-UNDO.
  DEF VAR iReturn      AS INT NO-UNDO.
  DEF VAR hVgField     AS HANDLE NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE lokKombKampanjeIdList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cListe AS CHARACTER NO-UNDO.
   
  bStopSelection = FALSE.

  lokKombKampanjeIdList = ''.
  IF CAN-DO('2,3',STRING(iKombKampanje)) THEN 
  BYGG_KAMPLISTE:
  DO:
      lokKombKampanjeIdList = ''.
      FOR EACH KampanjeMixMatch NO-LOCK WHERE 
          KampanjeMixMatch.KampKlar = TRUE AND 
          KampanjeMixMatch.KampStartDato <= TODAY AND
          KampanjeMixMatch.KampSluttDato >= TODAY:
              
          IF cButikerIdList-3 <> '' THEN 
          DO:
            IF CAN-FIND(FIRST KampanjeButikker WHERE 
                              KampanjeButikker.KampId = KampanjeMixMatch.KampId AND 
                              CAN-DO(cButikerIdList-3,STRING(KampanjeButikker.Butik))) THEN 
            ASSIGN
                lokKombKampanjeIdList = lokKombKampanjeIdList + 
                                      (IF lokKombKampanjeIdList <> '' THEN ',' ELSE '') + 
                                      STRING(KampanjeMixMatch.KampId).
          END.
          ELSE 
            ASSIGN
                lokKombKampanjeIdList = lokKombKampanjeIdList + 
                                      (IF lokKombKampanjeIdList <> '' THEN ',' ELSE '') + 
                                      STRING(KampanjeMixMatch.KampId).
      END.
  END. /* BYGG_KAMPLISTE*/ 

  /* Bygg liste over artikler som står på aktive kampanjer. */
  cListe = ''.
  EMPTY TEMP-TABLE ttArtNr.
  IF lokKombKampanjeIdList <> '' OR cKombKampanjeIdList <> '' THEN 
  DO piLoop = 1 TO NUM-ENTRIES(lokKombKampanjeIdList + ',' + cKombKampanjeIdList):
      IF ENTRY(piLoop,lokKombKampanjeIdList + ',' + cKombKampanjeIdList) <> '' THEN 
        cListe = cListe + 
                 (IF cListe = '' THEN '' ELSE ',') + 
                 ENTRY(piLoop,lokKombKampanjeIdList + ',' + cKombKampanjeIdList).
      
  END.
  IF cListe <> '' THEN 
    byggTempTbl(cListe).

  IF bVisQuery THEN DO:
    MESSAGE PROGRAM-NAME(1) SKIP(1)
            lcWhere SKIP(1)
            "Index: " hQuery:INDEX-INFORMATION(1)      SKIP(1)
            "iLager (sjekk hvis > 1): " iLager         SKIP
            "  cButikerIdList: "        cButikerIdList     SKIP(1)
            "iTilbud (sjekk hvis > 1): "             iTilbud            SKIP
            "  cPrisprofilIdList: "     cPrisprofilIdList  SKIP(1)
            "bTrans (sjekk hvis <> ?): "         bTrans        SKIP
            "  dFraTrans: "             dFraTrans     SKIP
            "  dTilTrans: "             dTilTrans     SKIP(1)       SKIP
            "        Transtype:"        iTTId              SKIP(1)
            "cButikerIdList-2: "        cButikerIdList-2   SKIP(1)
            "iBest (sjekk hvis > 1): "  iBest              SKIP
            "  dFraBest: "              dFraBest           SKIP
            "  dTilBest: "              dTilBest           SKIP
            "  cBestStatIdList: "       cBestStatIdList    SKIP
            "  iBestType: "             iBestType          SKIP(1)
            "  iBekreftet: "            iBekreftet          SKIP(1)
            "cAktivitetIdList: "        cAktivitetIdList   SKIP
            "cMesseIdList: "            cMesseIdList       SKIP
            "cVarebokIdList: "          cVarebokIdList     SKIP
            "cKampanjeIdList: "         cKampanjeIdList    SKIP
            "cKombKampanjeIdList: "     cKombKampanjeIdList    SKIP
            "cPrisProfilIdList: "       cPrisProfilIdList  SKIP(1)
            "cStrekkode: "              cStrekkode         SKIP
            "cBestillingsnummer: "      cBestillingsnummer SKIP
            "cERPNr: "                  cERPNr             SKIP
            "iKombKampanje (sjekk hvis > 1): " iKombKampanje SKIP
            "  lokKombKampanjeIdList: " lokKombKampanjeIdList SKIP
            "  cButikerIdList-3: "      cButikerIdList-3     SKIP
            "   cListArtButiker: "      cListArtButiker SKIP 
            "    cRAvdNr-IdList: "      cRAvdNr-IdList SKIP 
            "   lUtanVareomrade: " lUtanVareomrade SKIP(1)
            VIEW-AS ALERT-BOX BUTTONS OK-CANCEL UPDATE bOK.
    IF NOT bOk THEN RETURN.
  END.
    
  CREATE QUERY  hQuery.
  CREATE BUFFER hBuffer FOR TABLE 'ArtBas'.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(lcWhere).

  ASSIGN cButikerIdList    = REPLACE(cButikerIdList,"|",",")
         cButikerIdList-2  = REPLACE(cButikerIdList-2,"|",",")
         cPrisprofilIdList = REPLACE(cPrisprofilIdList,"|",",")
         cMesseIdList      = REPLACE(cMesseIdList,"|",",") 
         cAktivitetIdList  = REPLACE(cAktivitetIdList,"|",",")
         cVarebokIdList    = REPLACE(cVarebokIdList,"|",",")
         cKampanjeIdList   = REPLACE(cKampanjeIdList,"|",",")
         cKombKampanjeIdList = REPLACE(cKombKampanjeIdList,"|",",")
         cListArtButiker   = REPLACE(cListArtButiker,"|",",")
         cKategoriList     = REPLACE(cKategoriIdList,"|",",")
         cAnv-KodIdList    = REPLACE(cAnv-KodIdList,"|",",")
         
         .

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  ASSIGN hBufferField = hBuffer:BUFFER-FIELD("ArtikkelNr") 
         hVgField     = hBuffer:BUFFER-FIELD("Vg")
         .

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    ASSIGN dArtikkelNr  = hBufferField:BUFFER-VALUE()
           bOK          = TRUE
           idx          = idx + 1
           .

    /* Sjekk om art. er på tilbud / ikke på tilbud: */
    IF bOK AND iTilbud = 2 THEN DO: /* På tilbud */
      bOK = FALSE.
      IF cPrisprofilIdList NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cPrisprofilIdList):
          IF CAN-FIND(FIRST ArtPris WHERE ArtPris.ArtikkelNr = dArtikkelNr
                                      AND ArtPris.ProfilNr   = INT(ENTRY(ix,cPrisprofilIdList))
                                      AND ArtPris.Tilbud) THEN DO:
            bOk = TRUE.
            LEAVE.
          END.
        END.
      END.
      ELSE IF CAN-FIND(FIRST ArtPris WHERE ArtPris.ArtikkelNr = dArtikkelNr
                                       AND ArtPris.Tilbud) THEN
        bOK = TRUE.
    END.
    ELSE IF bOK AND iTilbud = 3 THEN DO: /* Ikke på tilbud */
      bOK = FALSE.
      IF cPrisprofilIdList NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cPrisprofilIdList):
          IF CAN-FIND(FIRST ArtPris WHERE ArtPris.ArtikkelNr = dArtikkelNr
                                      AND ArtPris.ProfilNr   = INT(ENTRY(ix,cPrisprofilIdList))
                                      AND NOT ArtPris.Tilbud) THEN DO:
            bOk = TRUE.
            LEAVE.
          END.
        END.
      END.
      ELSE IF CAN-FIND(FIRST ArtPris WHERE ArtPris.ArtikkelNr = dArtikkelNr
                                       AND NOT ArtPris.Tilbud) THEN
        bOK = TRUE.
    END.

    /* Sjekk om artikkelens vareguppe tilhører aktivitet: */
    IF bOK AND cAktivitetIdList NE "" AND
       NOT CAN-FIND(FIRST VgAkt WHERE VgAkt.Vg = hVgField:BUFFER-VALUE) THEN
      bOK = FALSE.

    /* Sjekk om artikkel er med i varebok: */
    IF bOK AND cVarebokIdList NE "" THEN DO:
      bTmpOk = FALSE.
      FOR EACH VareBokLinje FIELDS() NO-LOCK
          WHERE VareBokLinje.ArtikkelNr = dArtikkelNr
            AND CAN-DO(cVarebokIdList,STRING(VareBokLinje.VareBokNr)):
        bTmpOk = TRUE.
        LEAVE.
      END.
      bOk = bTmpOk.
    END.

    /* Sjekk om artikkel er med i messe (via varebok): */
    IF bOK AND cMesseIdList NE "" THEN DO:
      bTmpOk = FALSE.
      FOR EACH VareBokLinje FIELDS() NO-LOCK
          WHERE VareBokLinje.ArtikkelNr = dArtikkelNr,
          FIRST VareBokHode FIELDS() 
                OF VareBokLinje
                WHERE CAN-DO(cMesseIdList,STRING(VareBokHode.MesseNr)):
        bTmpOk = TRUE.
        LEAVE.
      END.
      bOk = bTmpOk.
    END.

    /* Sjekk om artikkel er med i kampanje (via kampanjehode): */
    IF bOK AND cKampanjeIdList NE "" THEN DO:
      bTmpOk = FALSE.
      FOR EACH KampanjeLinje FIELDS() NO-LOCK
          WHERE KampanjeLinje.ArtikkelNr = dArtikkelNr
            AND CAN-DO(cKampanjeIdList,STRING(KampanjeLinje.KampanjeId)):
        bTmpOk = TRUE.
        LEAVE.
      END.
      bOk = bTmpOk.
    END.

    /* Sjekk om artikkel er med i kombinasjonskampanje (via KampanjeMixMatch): */
    IF bOK AND cKombKampanjeIdList NE "" THEN 
    DO:
        bOK = getAktivICombKampanje (dArtikkelNr, cKombKampanjeIdList).
    END.
    
    /* ---- Aktiv på kombinasjonskampanje (MixMatch) -------- */
    IF bOk AND lokKombKampanjeIdList NE "" THEN 
    DO:
        IF iKombKampanje = 2 THEN 
            bOK = getAktivICombKampanje (dArtikkelNr, lokKombKampanjeIdList).
        ELSE IF iKombKampanje = 3 THEN   
            bOK = NOT getAktivICombKampanje (dArtikkelNr, lokKombKampanjeIdList).
    END.

    /* Sjekk lagerbeholdning: */
    IF bOK AND iLager = 2 THEN DO:   /* Pos.lager */
    
      bOK = FALSE.
      IF cButikerIdList NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cButikerIdList):
          IF CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = dArtikkelNr
                                    AND ArtLag.Butik      = INT(ENTRY(ix,cButikerIdList))
                                    AND ArtLag.LagAnt     > 0) THEN DO:
            bOk = TRUE.
            LEAVE.
          END.
        END.
      END.
      ELSE IF CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = dArtikkelNr
                                     AND ArtLag.LagAnt     > 0) THEN
        bOK = TRUE.
    END.
    ELSE IF bOK AND iLager = 3 THEN DO:  /* Null i lager */
      bOK = FALSE.
      IF cButikerIdList NE "" THEN DO:
        /* Lager skal være lik 0 i alle butikkene som sjekkes.            */
        /* Feiler testen i et av tilfellene, skal man hoppe ut av loopen. */
        DO ix = 1 TO NUM-ENTRIES(cButikerIdList):
          IF NOT CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = dArtikkelNr
                                    AND ArtLag.Butik      = INT(ENTRY(ix,cButikerIdList))
                                    AND ArtLag.LagAnt     <> 0) THEN 
              bOk = TRUE.
          ELSE DO:
              bOk = FALSE.
              LEAVE.
          END.
        END.
      END.
      ELSE IF NOT CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = dArtikkelNr
                                         AND ArtLag.LagAnt     <> 0) THEN
        bOK = TRUE.
    END.
    ELSE IF bOK AND iLager = 4 THEN DO:   /* Neg.lager */
      bOk = FALSE.
      IF cButikerIdList NE "" THEN DO:
        DO ix = 1 TO NUM-ENTRIES(cButikerIdList):
          IF CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = dArtikkelNr
                                    AND ArtLag.Butik      = INT(ENTRY(ix,cButikerIdList))
                                    AND ArtLag.LagAnt     < 0) THEN DO:
            bOk = TRUE.
            LEAVE.
          END.
        END.
      END.
      ELSE IF CAN-FIND(FIRST ArtLag WHERE ArtLag.ArtikkelNr = dArtikkelNr
                                  AND ArtLag.LagAnt        < 0) THEN
         bOK = TRUE.
    END.

    /* Sjekk bestilling - Går mot Bestillingsregsiteret: */
    IF bOK AND iBest > 1 THEN 
      bOK = SjekkBestilling(dArtikkelNr).


    /* Sjekk for transaksjoner / ikke Transaksjoner: */
    IF bOK AND bTrans NE ? /*AND (dFraTrans NE ? OR dTilTrans NE ?)*/ THEN 
    DO:
        bOk = FALSE.
        IF cButikerIdList-2 NE "" THEN 
        BUTSJEKK:
        DO:
          DO ix = 1 TO NUM-ENTRIES(cButikerIdList-2):
              bOK = SjekkTrans(dArtikkelNr,INT(ENTRY(ix,cButikerIdList-2)),iTTId).
              IF bOK THEN LEAVE BUTSJEKK.
          END.
        END. /* BUTSJEKK */
        ELSE bOK = SjekkTrans(dArtikkelNr,0,iTTId).
    END.

    /* Sjekker hoved kategori */
    IF bOk AND cKategoriList <> "" THEN DO:
        bOk = FALSE.
        FIND FIRST HovedKategori NO-LOCK WHERE
            HovedKategori.HovedKatNr = int(hBuffer:BUFFER-FIELD("HovedKatNr"):BUFFER-VALUE) NO-ERROR.
        IF AVAILABLE HovedKategori THEN DO:
            IF CAN-DO(cKategoriList,STRING(HovedKategori.HovedKatNr)) THEN
                bOk = TRUE.
        END.
    END.

    /* Sjekker brukskoder */
    IF bOk AND cAnv-KodIdList <> "" THEN DO:
        bOk = FALSE.
        FIND FIRST Anv-Kod NO-LOCK WHERE
            Anv-Kod.Anv-Id = int(hBuffer:BUFFER-FIELD("Anv-Id"):BUFFER-VALUE) NO-ERROR.
        IF AVAILABLE Anv-Kod THEN DO:
            IF CAN-DO(cKategoriList,STRING(Anv-Kod.Anv-Id)) THEN
                bOk = TRUE.
        END.
    END.

    /* sjekk artbut */
    IF bOk AND cListArtButiker <> "" THEN DO:
        bOk = TRUE.
        DO ii = 1 TO NUM-ENTRIES(cListArtButiker):
            IF NOT CAN-FIND(artbut WHERE ArtBut.ArtikkelNr = dArtikkelnr AND ArtBut.Butik = INT(ENTRY(ii,cListArtButiker)) AND Artbut.deleted = FALSE) THEN DO:
                bOk = FALSE.
                LEAVE.
            END.
        END.
    END.
    /* Varuområde  */
    IF bOk AND cRAvdNr-IdList <> "" THEN DO:
        bOK = FALSE.
        DO ii = 1 TO NUM-ENTRIES(cRAvdNr-IdList):
            IF CAN-FIND(ArtBasVo WHERE ArtBasVo.artikkelnr = dArtikkelnr AND ArtBasVo.RavdNr = INT(ENTRY(ii,cRAvdNr-IdList))) THEN DO:
                bOk = TRUE.
                LEAVE.
            END.
        END.
    END.
    IF bOk AND cRAvdNr-IdList = "" AND lUtanVareomrade = TRUE THEN DO:
        IF CAN-FIND(FIRST ArtBasVo WHERE ArtBasVo.artikkelnr = dArtikkelnr) THEN
            bOk = FALSE.
    END.
    /* Sjekker strekkode */
    IF bOk AND cStrekkode <> "" THEN DO:
        bOk = FALSE.
        FIND FIRST Strekkode NO-LOCK WHERE
             Strekkode.ArtikkelNr = dArtikkelNr AND
             Strekkode.Kode BEGINS cStrekkode NO-ERROR.
        IF AVAILABLE Strekkode THEN
            bOk = TRUE.
    END.

    /* Sjekker Bestillingsnummer */
    IF bOk AND cBestillingsnummer <> "" THEN DO:
        bOk = FALSE.
        FIND FIRST Strekkode NO-LOCK WHERE
             Strekkode.ArtikkelNr = dArtikkelNr AND
             Strekkode.Bestillingsnummer BEGINS cBestillingsnummer NO-ERROR.
        IF AVAILABLE Strekkode THEN
            bOk = TRUE.
    END.

    /* Sjekker ERPNr */
    IF bOk AND cERPNr <> "" THEN DO:
        bOk = FALSE.
        FIND FIRST Strekkode NO-LOCK WHERE
             Strekkode.ArtikkelNr = dArtikkelNr AND
             Strekkode.ERPNr BEGINS cERPNr NO-ERROR.
        IF AVAILABLE Strekkode THEN
            bOk = TRUE.
    END.

    IF bOK THEN DO:
      FIND bArtBas NO-LOCK WHERE
          bArtBas.ArtikkelNr = dArtikkelNr NO-ERROR.
      IF AVAILABLE bArtBas THEN
      DO:
        IF NOT CAN-FIND(tmpArtBas WHERE
                        tmpArtBas.ArtikkelNr = bArtBas.ArtikkelNr) THEN
        DO:
          CREATE tmpArtBas.
          BUFFER-COPY bArtBas TO tmpArtBas.
          iCount = iCount + 1.
        END.
      END.
    END.
    
    hQuery:GET-NEXT().

    IF idx MOD 500 = 0 THEN DO:
      bOK = SESSION:SET-WAIT-STATE("").
      PROCESS EVENTS.
      IF bStopSelection THEN DO:
        iReturn = DYNAMIC-FUNCTION("DoMessage",0,4,
                                   "Avbryt søk?" + CHR(10) + "antall nye poster så langt: &1",
                                   "Avbryt",
                                   STRING(iCount)).
        IF iReturn = 6 THEN LEAVE.
      END.
      bOK = SESSION:SET-WAIT-STATE("general").
    END.
  END.

  bOK = SESSION:SET-WAIT-STATE("").
  hQuery:QUERY-CLOSE().
  hBuffer:BUFFER-RELEASE().
  /*
  FOR EACH ArtBas NO-LOCK WHERE
      ArtBas.LevNr = 10:
      IF NOT CAN-FIND(tmpArtBas WHERE
                      tmpArtBas.ArtikkelNr = ArtBas.ArtikkelNr) THEN
      DO:
          CREATE tmpArtBas.
          BUFFER-COPY ArtBas TO tmpArtBas.
      END.
  END.
  */
  DELETE OBJECT hQuery.
  DELETE OBJECT hBuffer.

  iTotalCount = iTotalCount + iCount.

  cStatusString = STRING(iTotalCount) + (IF iCount > 0 THEN " (+" + STRING(iCount) + ")" ELSE "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION byggTempTbl dTables 
FUNCTION byggTempTbl RETURNS LOGICAL
        (  INPUT icListe AS CHARACTER ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

                DEFINE VARIABLE result AS LOGICAL NO-UNDO.
                DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
  KAMPLOOP:
  DO piLoop = 1 TO NUM-ENTRIES(icListe):
      FOR EACH KampanjeTilbArtikkel FIELDS() NO-LOCK
          WHERE KampanjeTilbArtikkel.KampId        = DEC(ENTRY(piLoop,icListe)) AND
                KampanjeTilbArtikkel.KampTilbId   >= 0 AND                
                KampanjeTilbArtikkel.KampTilbArtId > 0:
          IF NOT CAN-FIND(FIRST ttArtNr WHERE 
                          ttArtNr.ArtNr = KampanjeTilbArtikkel.KampTilbArtId) THEN 
            DO:
              CREATE ttArtNr.
              ASSIGN
                iAnt = iAnt + 1.
                ttArtNr.ArtNr = KampanjeTilbArtikkel.KampTilbArtId.
            END.
      END.
  END. /* KAMPLOOP */
  FAMSJEKK:
  DO:
      DO piLoop = 1 TO NUM-ENTRIES(icListe):
          FOR EACH KampanjeTilbArtikkel FIELDS() NO-LOCK
              WHERE KampanjeTilbArtikkel.KampId        = DEC(ENTRY(piLoop,icListe)) AND
                    KampanjeTilbArtikkel.KampTilbId   >= 0 AND                
                    KampanjeTilbArtikkel.KampTilbArtId = 0:
            FOR EACH ProduktFamMedlem FIELDS() NO-LOCK WHERE 
                ProduktFamMedlem.ProdFamId = KampanjeTilbArtikkel.ProdFamId:
              IF NOT CAN-FIND(FIRST ttArtNr WHERE 
                              ttArtNr.ArtNr = KampanjeTilbArtikkel.KampTilbArtId) THEN 
                DO:
                  CREATE ttArtNr.
                  ASSIGN
                    iAnt = iAnt + 1.
                    ttArtNr.ArtNr = ProduktFamMedlem.ProdFamArtikkelNr.
                END.
            END.
          END.
      END. 
  END. /* FAMSJEKK */

  RETURN result.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION deleteRow dTables  _DB-REQUIRED
FUNCTION deleteRow RETURNS LOGICAL
  ( INPUT pcRowIdent AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  IF SUPER( INPUT pcRowIdent ) THEN DO:
    iTotalCount = iTotalCount - 1.
    RETURN TRUE.
  END.
  ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuAktivpris dTables  _DB-REQUIRED
FUNCTION fuAktivpris RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF plArtikkelNr > 0 THEN
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.

  IF AVAILABLE ArtPris THEN
      RETURN IF ArtPris.Tilbud THEN ArtPris.Pris[2] ELSE ArtPris.Pris[1].
  ELSE
      RETURN 0.00.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuAktivvarekost dTables  _DB-REQUIRED
FUNCTION fuAktivvarekost RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF plArtikkelNr > 0 THEN
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.

  IF AVAILABLE ArtPris THEN
      RETURN ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1].
  ELSE
      RETURN 0.00.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuDb% dTables  _DB-REQUIRED
FUNCTION fuDb% RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF plArtikkelNr > 0 THEN
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.

  IF AVAILABLE ArtPris THEN
      RETURN ArtPris.Db%[IF ArtPris.Tilbud THEN 2 ELSE 1].
  ELSE
      RETURN 0.00.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuPris dTables  _DB-REQUIRED
FUNCTION fuPris RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF plArtikkelNr > 0 THEN
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.

  IF AVAILABLE ArtPris THEN
      RETURN ArtPris.Pris[1].
  ELSE
      RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuTilbud dTables  _DB-REQUIRED
FUNCTION fuTilbud RETURNS LOGICAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lTilbud AS LOGICAL INIT FALSE   NO-UNDO.
  IF plArtikkelNr > 0 THEN
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.
  IF AVAIL ArtPris THEN
      lTilbud = ArtPris.Tilbud. /* Function return value. */
  RETURN lTilbud.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuVarekost dTables  _DB-REQUIRED
FUNCTION fuVarekost RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF plArtikkelNr > 0 THEN
  FIND FIRST ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr NO-ERROR.

  IF AVAILABLE ArtPris THEN
      RETURN ArtPris.VareKost[1].
  ELSE
      RETURN 0.00.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getAktivICombKampanje dTables 
FUNCTION getAktivICombKampanje RETURNS LOGICAL
        ( INPUT inpArtikkelNr AS DECIMAL,
          INPUT inpKombKampanjeIdList AS CHARACTER ):
        /*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DEFINE VARIABLE ibTmpOk AS LOGICAL NO-UNDO.
  
  IF CAN-FIND(FIRST ttArtNr WHERE 
                    ttArtNr.ArtNr = inpArtikkelNr) THEN
                    ibTmpOk = TRUE.
  ELSE 
      ibTmpOk = FALSE.
       
  RETURN ibTmpOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getButikkIdListe dTables  _DB-REQUIRED
FUNCTION getButikkIdListe RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cButikerIdList.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLevNavn dTables  _DB-REQUIRED
FUNCTION getLevNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LevBas NO-LOCK WHERE
      LevBas.LevNr = RowObject.LevNr NO-ERROR.
  IF AVAILABLE LevBas THEN
      RETURN LevBas.LevNamn.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRecordCount dTables  _DB-REQUIRED
FUNCTION getRecordCount RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN iTotalCount.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSasong dTables  _DB-REQUIRED
FUNCTION getSasong RETURNS CHARACTER
  ( INPUT ipSasong AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND SaSong WHERE SaSong.Sasong = ipSasong NO-LOCK NO-ERROR.
  RETURN IF AVAIL Sasong THEN SaSong.SasBeskr ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStatusString dTables  _DB-REQUIRED
FUNCTION getStatusString RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN cStatusString.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTmpArtBasHandle dTables  _DB-REQUIRED
FUNCTION getTmpArtBasHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN BUFFER tmpArtBas:HANDLE:TABLE-HANDLE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getVgBeskr dTables  _DB-REQUIRED
FUNCTION getVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = RowObject.Vg NO-ERROR.
  IF AVAILABLE VarGr THEN
      RETURN VarGr.VgBeskr.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setSelectorFilter dTables  _DB-REQUIRED
FUNCTION setSelectorFilter RETURNS LOGICAL
  ( INPUT iiLager            AS INT,
    INPUT icButikerIdList    AS CHAR,
    INPUT iiTilbud           AS INT,
    INPUT icPrisprofilIdList AS CHAR,
    INPUT ibSalg             AS LOG,
    INPUT idFraSalg          AS DATE,
    INPUT idTilSalg          AS DATE,
    INPUT iiBest             AS INT,
    INPUT idFraBest          AS DATE,
    INPUT idTilBest          AS DATE,
    INPUT icBestStatIdList   AS CHAR,
    INPUT iiBestType         AS INT,
    INPUT iiBekreftet        AS INT,
    INPUT icKategoriList     AS CHAR,
    INPUT icAktivitetIdList  AS CHAR,
    INPUT icMesseIdList      AS CHAR,
    INPUT icVarebokIdList    AS CHAR,
    INPUT icKampanjeIdList   AS CHAR,
    INPUT icStrekkode        AS CHAR,
    INPUT icBestillingsnummer AS CHAR,
    INPUT icERPNr            AS CHAR,
    INPUT ibTrans       AS LOG,
    INPUT idFraTrans    AS DATE,
    INPUT idTilTrans    AS DATE,
    INPUT icButikerIdList-2  AS CHAR,
    INPUT iiTTId             AS INT,
    INPUT icKombKampanjeIdList AS CHARACTER, 
    INPUT iiKombKampanje     AS INTEGER,
    INPUT icButikerIdList-3  AS CHAR,
    INPUT fi-cListArtButiker AS CHAR,
    INPUT icRAvdNr-IdList     AS CHAR,
    INPUT ilUtanVareomrade  AS LOG,
    INPUT icAnv-KodIdList AS CHARACTER 
    ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: iiLager:  2: Pos.lager, 3: Ikke lager, 4: Neg.lager 
           iiTilbud: 2: Aktiv på tilbud, 3: Ikke aktiv på tilbud
           iiBest:   2: Plan.lev, 3: IKKE plan.lev, 4: Varemott, 5: IKKE varemott, 6: Bestilling, 7: IKKE bestilling  
           idFra/TilBest: Hvis iiBest = 2 eller 3: Lev.dato, ellers best.dato
------------------------------------------------------------------------------*/
ASSIGN iLager             = iiLager
       cButikerIdList     = icButikerIdList
       iTilbud            = iiTilbud 
       cPrisprofilIdList  = icPrisprofilIdList
       bSalg              = ibSalg       
       dFraSalg           = idFraSalg    
       dTilSalg           = idTilSalg    
       iBest              = iiBest       
       dFraBest           = idFraBest    
       dTilBest           = idTilBest
       cBestStatIdList    = icBestStatIdList
       iBestType          = iiBestType
       iBekreftet         = iiBekreftet
       cKategoriList      = icKategoriList
       cAktivitetIdList   = icAktivitetIdList
       cMesseIdList       = icMesseIdList
       cVarebokIdList     = icVarebokIdList
       cKampanjeIdList    = icKampanjeIdList
       cKombKampanjeIdList = icKombKampanjeIdList
       cStrekkode         = icStrekkode
       cBestillingsnummer = icBestillingsnummer
       cERPNr             = icERPNr
       bTrans        = ibTrans    
       dFraTrans     = idFraTrans    
       dTilTrans     = idTilTrans 
       cButikerIdList-2   = icButikerIdList-2
       iTTId              = iiTTId
       iKombKampanje      = iiKombKampanje
       cButikerIdList-3   = icButikerIdList-3
       cListArtButiker    = fi-cListArtButiker
       cRAvdNr-IdList    = icRAvdNr-IdList
       lUtanVareomrade  = ilUtanVareomrade.


RETURN TRUE.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVisQuery dTables  _DB-REQUIRED
FUNCTION setVisQuery RETURNS LOGICAL
  ( INPUT ibVisQuery AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bVisQuery = ibVisQuery.
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION sjekkBestilling dTables  _DB-REQUIRED
FUNCTION sjekkBestilling RETURNS LOGICAL
  ( INPUT idArtikkelNr AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bBestOk AS LOG NO-UNDO.

IF iBestType NE 0 AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelnr
                                                    AND BestHode.BestStat   = iBestType) THEN
  RETURN FALSE.

IF iBestType NE 0 AND iBekreftet NE 0 THEN DO:
    IF iBekreftet = 1 THEN DO: /* 1 = bekräftade */
        IF NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelnr
                                                    AND BestHode.BestStat = iBestType
                                                    AND BestHode.Bekreftetdato <> ?) THEN
            RETURN FALSE.
    END.
    ELSE IF iBekreftet = 2 THEN DO: /* 2 = ? */
        IF NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelnr
                                                    AND BestHode.BestStat = iBestType
                                                    AND BestHode.Bekreftetdato = ?) THEN
            RETURN FALSE.
    END.
END.

IF cBestStatIdList NE "" THEN 
  DO ix = 1 TO NUM-ENTRIES(cBestStatIdList):
    IF NOT bBestOk AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                 AND BestHode.BestStat   = INT(ENTRY(ix,cBestStatIdList))) THEN DO:
      IF iBekreftet NE 0 THEN DO:
          IF iBekreftet = 1 THEN DO: /* 1 = bekräftade */
              IF CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelnr
                                                          AND BestHode.BestStat = INT(ENTRY(ix,cBestStatIdList))
                                                          AND BestHode.Bekreftetdato <> ?) THEN
                  bBestOk = TRUE.
          END.
          ELSE IF iBekreftet = 2 THEN DO: /* 2 = ? */
              IF CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelnr
                                                          AND BestHode.BestStat = INT(ENTRY(ix,cBestStatIdList))
                                                          AND BestHode.Bekreftetdato = ?) THEN
                  bBestOk = TRUE.
          END.
      END.
      ELSE
          bBestOk = TRUE.
    END.
  END.
ELSE bBestOk = TRUE.

IF NOT bBestOk THEN RETURN bBestOk.

/* Planlagt levering: */
IF iBest = 2 THEN DO: /* Planlagt levering */
  IF dFraBest NE ? AND dTilBest NE ? AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                       AND BestHode.LevDato    GE dFraBest
                                                       AND BestHode.LevDato    LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
    bBestOk = FALSE.

  IF bBestOk THEN DO:
    IF dFraBest NE ? AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                       AND BestHode.LevDato    GE dFraBest 
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  
    ELSE IF dTilBest NE ? AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                       AND BestHode.LevDato LE dTilBest 
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  END.
END.
ELSE IF iBest = 3 THEN DO: /* IKKE planlagt levering */
    IF dFraBest NE ? AND dTilBest NE ? AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                       AND BestHode.LevDato    GE dFraBest
                                                       AND BestHode.LevDato    LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
    bBestOk = FALSE.

  IF bBestOk THEN DO:
    IF dFraBest NE ? AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                       AND BestHode.LevDato    GE dFraBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  
    ELSE IF dTilBest NE ? AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                       AND BestHode.LevDato LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  END.
END.

/* Varemottak: */
ELSE IF iBest = 4 THEN DO: /* Varemottak */
  IF dFraBest NE ? AND dTilBest NE ? THEN
     FIND FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                            AND TransLogg.Dato       GE dFraBest
                            AND TransLogg.Dato       LE dTilBest
                            AND TransLogg.Tid        > -1
                            AND TransLogg.Butik      > 0
                            AND TransLogg.TTid       = 5 NO-LOCK NO-ERROR. 
  IF NOT AVAIL TransLogg THEN
    bBestOk = FALSE.

  IF bBestOk THEN DO:
    IF dFraBest NE ? THEN DO: 
      FIND FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                             AND TransLogg.Dato       GE dFraBest
                             AND TransLogg.Tid        > -1
                             AND TransLogg.Butik      > 0
                             AND TransLogg.TTid       = 5 NO-LOCK NO-ERROR. 
      IF NOT AVAIL TransLogg THEN
        bBestOk = FALSE.
    END.
  
    ELSE IF dTilBest NE ? THEN DO:
      FIND FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                             AND TransLogg.Dato       LE dTilBest
                             AND TransLogg.Tid        > -1
                             AND TransLogg.Butik      > 0
                             AND TransLogg.TTid       = 5 NO-LOCK NO-ERROR.
      IF NOT AVAIL TransLogg THEN
        bBestOk = FALSE.
    END.
  END.
  IF bBestOk AND AVAIL TransLogg AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                                   AND BestHode.BestNr     = TransLogg.BestNr
                                                                   AND (IF iBestType NE 0 THEN 
                                                                          BestHode.BestStat = iBestType
                                                                        ELSE TRUE)
                                                                   AND (IF cBestStatIdList NE "" THEN 
                                                                          CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                                        ELSE TRUE)) THEN
    bBestOk = FALSE.

END.
ELSE IF iBest = 5 THEN DO: /* IKKE varemottak */
  IF dFraBest NE ? AND dTilBest NE ? THEN
     FIND FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                            AND TransLogg.Dato       GE dFraBest
                            AND TransLogg.Dato       LE dTilBest
                            AND TransLogg.Tid        > -1
                            AND TransLogg.Butik      > 0
                            AND TransLogg.TTid       = 5 NO-LOCK NO-ERROR. 
  IF AVAIL TransLogg THEN
    bBestOk = FALSE.

  IF bBestOk THEN DO:
    IF dFraBest NE ? THEN DO: 
      FIND FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                             AND TransLogg.Dato       GE dFraBest
                             AND TransLogg.Tid        > -1
                             AND TransLogg.Butik      > 0
                             AND TransLogg.TTid       = 5 NO-LOCK NO-ERROR. 
      IF AVAIL TransLogg THEN
        bBestOk = FALSE.
    END.
  
    ELSE IF dTilBest NE ? THEN DO:
      FIND FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                             AND TransLogg.Dato       LE dTilBest
                             AND TransLogg.Tid        > -1
                             AND TransLogg.Butik      > 0
                             AND TransLogg.TTid       = 5 NO-LOCK NO-ERROR.
      IF AVAIL TransLogg THEN
        bBestOk = FALSE.
    END.
  END.
  IF bBestOk AND AVAIL TransLogg AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr = idArtikkelNr
                                                                   AND BestHode.BestNr     = TransLogg.BestNr
                                                                   AND (IF iBestType NE 0 THEN 
                                                                          BestHode.BestStat = iBestType
                                                                        ELSE TRUE)
                                                                   AND (IF cBestStatIdList NE "" THEN 
                                                                          CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                                        ELSE TRUE)) THEN
    bBestOk = FALSE.
END.

/* Bestilling: */
ELSE IF iBest = 6 THEN DO: /* Best */
  IF dFraBest NE ? AND dTilBest NE ? AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr      = idArtikkelNr
                                                       AND BestHode.BestillingsDato GE dFraBest
                                                       AND BestHode.BestillingsDato LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
    bBestOk = FALSE.

  IF bBestOk THEN DO:
    IF dFraBest NE ? AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr      = idArtikkelNr
                                                       AND BestHode.BestillingsDato GE dFraBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  
    ELSE IF dTilBest NE ? AND NOT CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr      = idArtikkelNr
                                                       AND BestHode.BestillingsDato LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  END.
END.
ELSE DO: /* 7: Ikke Best */
  IF dFraBest NE ? AND dTilBest NE ? AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr      = idArtikkelNr
                                                       AND BestHode.BestillingsDato GE dFraBest
                                                       AND BestHode.BestillingsDato LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
    bBestOk = FALSE.

  IF bBestOk THEN DO:
    IF dFraBest NE ? AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr      = idArtikkelNr
                                                       AND BestHode.BestillingsDato GE dFraBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  
    ELSE IF dTilBest NE ? AND CAN-FIND(FIRST BestHode WHERE BestHode.ArtikkelNr      = idArtikkelNr
                                                       AND BestHode.BestillingsDato LE dTilBest
                                                       AND (IF iBestType NE 0 THEN 
                                                              BestHode.BestStat = iBestType
                                                            ELSE TRUE)
                                                       AND (IF cBestStatIdList NE "" THEN 
                                                              CAN-DO(cBestStatIdList,STRING(BestHode.BestStat))
                                                            ELSE TRUE)) THEN 
      bBestOk = FALSE.
  END.
END.

RETURN bBestOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SjekkTrans dTables  _DB-REQUIRED
FUNCTION SjekkTrans RETURNS LOGICAL
  ( INPUT idArtikkelNr AS DEC,
    INPUT iiButikkNr   AS INT,
    INPUT i2TTId       AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR bTransOk  AS LOG NO-UNDO INIT FALSE.

IF bTrans THEN DO: /* Sjekk transaksjoner */
  IF dFraTrans NE ? AND dTilTrans NE ? THEN
  DO:
      IF CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                AND TransLogg.Dato       GE dFraTrans
                                AND TransLogg.Dato       LE dTilTrans
                                AND TransLogg.Tid        > -1
                                AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                AND TransLogg.TTid       = i2TTId) THEN 
      bTransOk = TRUE.
  END.
  
  ELSE DO:
    IF dFraTrans NE ? THEN 
    DO:
      IF CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                  AND TransLogg.Dato       GE dFraTrans
                                  AND TransLogg.Tid        > -1
                                  AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                  AND TransLogg.TTid       = i2TTId) THEN 
        bTransOk = TRUE.
    END.
  
    ELSE IF dTilTrans NE ? THEN 
    DO:
      IF CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                  AND TransLogg.Dato       LE dTilTrans
                                  AND TransLogg.Tid        > -1
                                  AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                  AND TransLogg.TTid       = i2TTId) THEN 
      bTransOk = TRUE.
    END.
    /* Ingen dato er angitt. */
    ELSE DO: 
        IF CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                  AND TransLogg.Tid        > -1
                                  AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                  AND TransLogg.TTid       = i2TTId) THEN 
        bTransOk = TRUE.
    END.
  END.
END.

ELSE DO: /* Ikke sjekk transaksjoner */
  IF dFraTrans NE ? AND dTilTrans NE ? THEN 
  DO:
    IF NOT CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                AND TransLogg.Dato       GE dFraTrans
                                AND TransLogg.Dato       LE dTilTrans
                                AND TransLogg.Tid        > -1
                                AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                AND TransLogg.TTid       = i2TTId) THEN 
      bTransOk = TRUE.
  END.

  ELSE DO:
    IF dFraTrans NE ? THEN 
    DO:
      IF NOT CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                  AND TransLogg.Dato       GE dFraTrans
                                  AND TransLogg.Tid        > -1
                                  AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                  AND TransLogg.TTid       = i2TTId) THEN 
      bTransOk = TRUE.
    END.
  
    ELSE IF dTilTrans NE ? THEN 
    DO:
      IF NOT CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                  AND TransLogg.Dato       LE dTilTrans
                                  AND TransLogg.Tid        > -1
                                  AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                  AND TransLogg.TTid       = i2TTId) THEN 
      bTransOk = TRUE.
    END.
    ELSE DO: 
      IF NOT CAN-FIND(FIRST TransLogg WHERE TransLogg.ArtikkelNr = idArtikkelNr
                                  AND TransLogg.Tid        > -1
                                  AND (IF iiButikkNr > 0 THEN TransLogg.Butik = iiButikkNr ELSE TransLogg.Butik > 0)
                                  AND TransLogg.TTid       = i2TTId) THEN 
      bTransOk = TRUE.
    END.
  END.
END.

RETURN bTransOk.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION stopSelection dTables  _DB-REQUIRED
FUNCTION stopSelection RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bStopSelection = TRUE.
RETURN TRUE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

