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
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AktivAv AktivDato Aktivert Alder AnonseArtikkel anv-id ArtikkelNr BehKode~
 Beskr BildeIKasse BildNr BongTekst BrukerID EDato ETid Farg foder-id Hg~
 HkStyrt HKVareId IKasse inner-id inn_dato KjentPaHK Klack Kommentar lager~
 LapTop last-id LevDato1 LevDato2 LevFargKod LevKod LevNr LokPris LopNr~
 MatKod Notat OLLager OPris ov-id Pakke Pakkenr ProdNr ProvKod RegistrertAv~
 RegistrertDato RegistrertTid SaSong SattPaKampanje slit-id Storrelser~
 StrTypeID Tilv-Land valkod Vg VgKat VMId AnbefaltPris Etikett HKArtikkelNr~
 HovedModellFarge KundeRabatt ModellFarge ny_dato PrisGrpNr RabKod~
 SalgsEnhet SentralBestilling SlaskArtikkelNr Slasket
&Scoped-define ENABLED-FIELDS-IN-ArtBas AktivAv AktivDato Aktivert Alder ~
AnonseArtikkel anv-id ArtikkelNr BehKode Beskr BildeIKasse BildNr BongTekst ~
BrukerID EDato ETid Farg foder-id Hg HkStyrt HKVareId IKasse inner-id ~
inn_dato KjentPaHK Klack Kommentar lager LapTop last-id LevDato1 LevDato2 ~
LevFargKod LevKod LevNr LokPris LopNr MatKod Notat OLLager OPris ov-id ~
Pakke Pakkenr ProdNr ProvKod RegistrertAv RegistrertDato RegistrertTid ~
SaSong SattPaKampanje slit-id Storrelser StrTypeID Tilv-Land valkod Vg ~
VgKat VMId AnbefaltPris Etikett HKArtikkelNr HovedModellFarge KundeRabatt ~
ModellFarge ny_dato PrisGrpNr RabKod SalgsEnhet SentralBestilling ~
SlaskArtikkelNr Slasket 
&Scoped-Define DATA-FIELDS  AktivAv fVgBeskr fuLevNavn AktivDato Aktivert Alder AnonseArtikkel anv-id~
 ArtikkelNr BehKode Beskr BildeIKasse BildNr BongTekst BrukerID EDato ETid~
 Farg foder-id Hg HkStyrt HKVareId IKasse inner-id inn_dato KjentPaHK Klack~
 Kommentar lager LapTop last-id LevDato1 LevDato2 LevFargKod LevKod LevNr~
 LokPris LopNr MatKod Notat OLLager OPris ov-id Pakke Pakkenr ProdNr ProvKod~
 RegistrertAv RegistrertDato RegistrertTid SaSong SattPaKampanje fiSasong~
 slit-id Storrelser StrTypeID Tilv-Land valkod Vg VgKat VMId AnbefaltPris~
 Etikett HKArtikkelNr HovedModellFarge KundeRabatt ModellFarge ny_dato~
 PrisGrpNr RabKod SalgsEnhet SentralBestilling SlaskArtikkelNr Slasket
&Scoped-define DATA-FIELDS-IN-ArtBas AktivAv AktivDato Aktivert Alder ~
AnonseArtikkel anv-id ArtikkelNr BehKode Beskr BildeIKasse BildNr BongTekst ~
BrukerID EDato ETid Farg foder-id Hg HkStyrt HKVareId IKasse inner-id ~
inn_dato KjentPaHK Klack Kommentar lager LapTop last-id LevDato1 LevDato2 ~
LevFargKod LevKod LevNr LokPris LopNr MatKod Notat OLLager OPris ov-id ~
Pakke Pakkenr ProdNr ProvKod RegistrertAv RegistrertDato RegistrertTid ~
SaSong SattPaKampanje slit-id Storrelser StrTypeID Tilv-Land valkod Vg ~
VgKat VMId AnbefaltPris Etikett HKArtikkelNr HovedModellFarge KundeRabatt ~
ModellFarge ny_dato PrisGrpNr RabKod SalgsEnhet SentralBestilling ~
SlaskArtikkelNr Slasket 
&Scoped-Define MANDATORY-FIELDS  ModellFarge PrisGrpNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dartbas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ArtBas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ArtBas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ArtBas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevNavn dTables  _DB-REQUIRED
FUNCTION getLevNavn RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getVgBeskr dTables  _DB-REQUIRED
FUNCTION getVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ArtBas SCROLLING.
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
{sdo/soksdo.i}

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
     _TblList          = "skotex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.ArtBas.AktivAv
"AktivAv" "AktivAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[2]   > "_<CALC>"
"getVgBeskr()" "fVgBeskr" "Varegr" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[3]   > "_<CALC>"
"getLevNavn()" "fuLevNavn" "Leverandør" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[4]   > skotex.ArtBas.AktivDato
"AktivDato" "AktivDato" ? ? "date" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[5]   > skotex.ArtBas.Aktivert
"Aktivert" "Aktivert" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[6]   > skotex.ArtBas.Alder
"Alder" "Alder" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[7]   > skotex.ArtBas.AnonseArtikkel
"AnonseArtikkel" "AnonseArtikkel" ? ? "logical" ? ? ? ? ? ? yes ? no 2.8 yes ""
     _FldNameList[8]   > skotex.ArtBas.anv-id
"anv-id" "anv-id" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[9]   > skotex.ArtBas.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[10]   > skotex.ArtBas.BehKode
"BehKode" "BehKode" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[11]   > skotex.ArtBas.Beskr
"Beskr" "Beskr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[12]   > skotex.ArtBas.BildeIKasse
"BildeIKasse" "BildeIKasse" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[13]   > skotex.ArtBas.BildNr
"BildNr" "BildNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[14]   > skotex.ArtBas.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[15]   > skotex.ArtBas.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[16]   > skotex.ArtBas.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[17]   > skotex.ArtBas.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[18]   > skotex.ArtBas.Farg
"Farg" "Farg" ? ">>>>9" "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[19]   > skotex.ArtBas.foder-id
"foder-id" "foder-id" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[20]   > skotex.ArtBas.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[21]   > skotex.ArtBas.HkStyrt
"HkStyrt" "HkStyrt" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[22]   > skotex.ArtBas.HKVareId
"HKVareId" "HKVareId" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[23]   > skotex.ArtBas.IKasse
"IKasse" "IKasse" ? ? "logical" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[24]   > skotex.ArtBas.inner-id
"inner-id" "inner-id" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes ""
     _FldNameList[25]   > skotex.ArtBas.inn_dato
"inn_dato" "inn_dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[26]   > skotex.ArtBas.KjentPaHK
"KjentPaHK" "KjentPaHK" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 yes ""
     _FldNameList[27]   > skotex.ArtBas.Klack
"Klack" "Klack" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ""
     _FldNameList[28]   > skotex.ArtBas.Kommentar
"Kommentar" "Kommentar" ? ? "character" ? ? ? ? ? ? yes ? no 64 yes ""
     _FldNameList[29]   > skotex.ArtBas.lager
"lager" "lager" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[30]   > skotex.ArtBas.LapTop
"LapTop" "LapTop" ? ? "logical" ? ? ? ? ? ? yes ? no 19.8 yes ""
     _FldNameList[31]   > skotex.ArtBas.last-id
"last-id" "last-id" ? ? "integer" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[32]   > skotex.ArtBas.LevDato1
"LevDato1" "LevDato1" ? ? "date" ? ? ? ? ? ? yes ? no 15.8 yes ""
     _FldNameList[33]   > skotex.ArtBas.LevDato2
"LevDato2" "LevDato2" ? ? "date" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[34]   > skotex.ArtBas.LevFargKod
"LevFargKod" "LevFargKod" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[35]   > skotex.ArtBas.LevKod
"LevKod" "LevKod" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[36]   > skotex.ArtBas.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 yes ""
     _FldNameList[37]   > skotex.ArtBas.LokPris
"LokPris" "LokPris" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[38]   > skotex.ArtBas.LopNr
"LopNr" "LopNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[39]   > skotex.ArtBas.MatKod
"MatKod" "MatKod" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[40]   > skotex.ArtBas.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[41]   > skotex.ArtBas.OLLager
"OLLager" "OLLager" ? ? "logical" ? ? ? ? ? ? yes ? no 5.6 yes ""
     _FldNameList[42]   > skotex.ArtBas.OPris
"OPris" "OPris" ? ? "logical" ? ? ? ? ? ? yes ? no 5 yes ""
     _FldNameList[43]   > skotex.ArtBas.ov-id
"ov-id" "ov-id" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[44]   > skotex.ArtBas.Pakke
"Pakke" "Pakke" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[45]   > skotex.ArtBas.Pakkenr
"Pakkenr" "Pakkenr" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[46]   > skotex.ArtBas.ProdNr
"ProdNr" "ProdNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[47]   > skotex.ArtBas.ProvKod
"ProvKod" "ProvKod" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[48]   > skotex.ArtBas.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[49]   > skotex.ArtBas.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[50]   > skotex.ArtBas.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[51]   > skotex.ArtBas.SaSong
"SaSong" "SaSong" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[52]   > skotex.ArtBas.SattPaKampanje
"SattPaKampanje" "SattPaKampanje" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[53]   > "_<CALC>"
"getSasong(RowObject.Sasong)" "fiSasong" "Sesong" "x(14)" "character" ? ? ? ? ? ? no ? no 14 no ?
     _FldNameList[54]   > skotex.ArtBas.slit-id
"slit-id" "slit-id" ? ? "integer" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[55]   > skotex.ArtBas.Storrelser
"Storrelser" "Storrelser" ? ? "logical" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[56]   > skotex.ArtBas.StrTypeID
"StrTypeID" "StrTypeID" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes ""
     _FldNameList[57]   > skotex.ArtBas.Tilv-Land
"Tilv-Land" "Tilv-Land" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[58]   > skotex.ArtBas.valkod
"valkod" "valkod" ? ? "character" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[59]   > skotex.ArtBas.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[60]   > skotex.ArtBas.VgKat
"VgKat" "VgKat" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[61]   > skotex.ArtBas.VMId
"VMId" "VMId" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[62]   > skotex.ArtBas.AnbefaltPris
"AnbefaltPris" "AnbefaltPris" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[63]   > skotex.ArtBas.Etikett
"Etikett" "Etikett" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[64]   > skotex.ArtBas.HKArtikkelNr
"HKArtikkelNr" "HKArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 17.4 yes ""
     _FldNameList[65]   > skotex.ArtBas.HovedModellFarge
"HovedModellFarge" "HovedModellFarge" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[66]   > skotex.ArtBas.KundeRabatt
"KundeRabatt" "KundeRabatt" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[67]   > skotex.ArtBas.ModellFarge
"ModellFarge" "ModellFarge" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[68]   > skotex.ArtBas.ny_dato
"ny_dato" "ny_dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[69]   > skotex.ArtBas.PrisGrpNr
"PrisGrpNr" "PrisGrpNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.8 yes ""
     _FldNameList[70]   > skotex.ArtBas.RabKod
"RabKod" "RabKod" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[71]   > skotex.ArtBas.SalgsEnhet
"SalgsEnhet" "SalgsEnhet" ? ? "character" ? ? ? ? ? ? yes ? no 5.6 yes ""
     _FldNameList[72]   > skotex.ArtBas.SentralBestilling
"SentralBestilling" "SentralBestilling" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 yes ""
     _FldNameList[73]   > skotex.ArtBas.SlaskArtikkelNr
"SlaskArtikkelNr" "SlaskArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[74]   > skotex.ArtBas.Slasket
"Slasket" "Slasket" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes ""
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
         rowObject.fiSasong = (getSasong(RowObject.Sasong))
         rowObject.fuLevNavn = (getLevNavn())
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

/* ************************  Function Implementations ***************** */

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

