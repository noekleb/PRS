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

&glob DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES Individ

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Adresse1 Adresse2 ArtikkelNr AvdelingNr BatchNr Beskr BrukerID~
 BruktVareKost butnr B_Id dato DB% DBKr ean EDato ePostAdresse ETid feilvare~
 ForsNr Garantinummer Hg individnr individtekst IndividType Jegerkort~
 Kaliber KjoptDato KKundeNr kordnr kradnr kundenr LevNamn levnr MobilTlf~
 Navn NyVare PersOrgNr PostNr Poststed Pris rapportert RegistrertAv~
 RegistrertDato RegistrertTid SAdresse1 SAdresse2 salgdato SelgerNr SeqNr~
 serienr SMobilTlf SNavn SPostNr SPoststed STelefon Storl StrKode Telefon~
 VapenKort VareKost vDB% vDBKr Vg VmBeskrivelse VMId VVarekost
&Scoped-define ENABLED-FIELDS-IN-Individ Adresse1 Adresse2 ArtikkelNr ~
AvdelingNr BatchNr Beskr BrukerID BruktVareKost butnr B_Id dato DB% DBKr ~
ean EDato ePostAdresse ETid feilvare ForsNr Garantinummer Hg individnr ~
individtekst IndividType Jegerkort Kaliber KjoptDato KKundeNr kordnr kradnr ~
kundenr LevNamn levnr MobilTlf Navn NyVare PersOrgNr PostNr Poststed Pris ~
rapportert RegistrertAv RegistrertDato RegistrertTid SAdresse1 SAdresse2 ~
salgdato SelgerNr SeqNr serienr SMobilTlf SNavn SPostNr SPoststed STelefon ~
Storl StrKode Telefon VapenKort VareKost vDB% vDBKr Vg VmBeskrivelse VMId ~
VVarekost 
&Scoped-Define DATA-FIELDS  Adresse1 Adresse2 ArtikkelNr AvdelingNr BatchNr Beskr BrukerID~
 BruktVareKost butnr B_Id dato DB% DBKr ean EDato ePostAdresse ETid feilvare~
 ForsNr Garantinummer Hg individnr individtekst IndividType Jegerkort~
 Kaliber KjoptDato KKundeNr kordnr kradnr kundenr LevNamn levnr MobilTlf~
 Navn NyVare PersOrgNr PostNr Poststed Pris rapportert RegistrertAv~
 RegistrertDato RegistrertTid SAdresse1 SAdresse2 salgdato SelgerNr SeqNr~
 serienr SMobilTlf SNavn SPostNr SPoststed STelefon Storl StrKode Telefon~
 VapenKort VareKost vDB% vDBKr Vg VmBeskrivelse VMId VVarekost fFarbeskr
&Scoped-define DATA-FIELDS-IN-Individ Adresse1 Adresse2 ArtikkelNr ~
AvdelingNr BatchNr Beskr BrukerID BruktVareKost butnr B_Id dato DB% DBKr ~
ean EDato ePostAdresse ETid feilvare ForsNr Garantinummer Hg individnr ~
individtekst IndividType Jegerkort Kaliber KjoptDato KKundeNr kordnr kradnr ~
kundenr LevNamn levnr MobilTlf Navn NyVare PersOrgNr PostNr Poststed Pris ~
rapportert RegistrertAv RegistrertDato RegistrertTid SAdresse1 SAdresse2 ~
salgdato SelgerNr SeqNr serienr SMobilTlf SNavn SPostNr SPoststed STelefon ~
Storl StrKode Telefon VapenKort VareKost vDB% vDBKr Vg VmBeskrivelse VMId ~
VVarekost 
&Scoped-Define MANDATORY-FIELDS  butnr dato ean feilvare individnr kordnr kradnr kundenr levnr rapportert
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dindivid.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Individ NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Individ NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Individ
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Individ


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFarge dTables  _DB-REQUIRED
FUNCTION getFarge RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Individ SCROLLING.
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
     _TblList          = "SkoTex.Individ"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > SkoTex.Individ.Adresse1
"Adresse1" "Adresse1" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[2]   > SkoTex.Individ.Adresse2
"Adresse2" "Adresse2" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[3]   > SkoTex.Individ.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[4]   > SkoTex.Individ.AvdelingNr
"AvdelingNr" "AvdelingNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[5]   > SkoTex.Individ.BatchNr
"BatchNr" "BatchNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[6]   > SkoTex.Individ.Beskr
"Beskr" "Beskr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[7]   > SkoTex.Individ.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[8]   > SkoTex.Individ.BruktVareKost
"BruktVareKost" "BruktVareKost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[9]   > SkoTex.Individ.butnr
"butnr" "butnr" ? ? "integer" ? ? ? ? ? ? yes ? yes 6 yes ""
     _FldNameList[10]   > SkoTex.Individ.B_Id
"B_Id" "B_Id" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.2 yes ""
     _FldNameList[11]   > SkoTex.Individ.dato
"dato" "dato" ? ? "date" ? ? ? ? ? ? yes ? yes 8.4 yes ""
     _FldNameList[12]   > SkoTex.Individ.DB%
"DB%" "DB%" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[13]   > SkoTex.Individ.DBKr
"DBKr" "DBKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[14]   > SkoTex.Individ.ean
"ean" "ean" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[15]   > SkoTex.Individ.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[16]   > SkoTex.Individ.ePostAdresse
"ePostAdresse" "ePostAdresse" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[17]   > SkoTex.Individ.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[18]   > SkoTex.Individ.feilvare
"feilvare" "feilvare" ? ? "logical" ? ? ? ? ? ? yes ? yes 7.4 yes ""
     _FldNameList[19]   > SkoTex.Individ.ForsNr
"ForsNr" "ForsNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[20]   > SkoTex.Individ.Garantinummer
"Garantinummer" "Garantinummer" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[21]   > SkoTex.Individ.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[22]   > SkoTex.Individ.individnr
"individnr" "individnr" ? ? "decimal" ? ? ? ? ? ? yes ? yes 14.4 yes ""
     _FldNameList[23]   > SkoTex.Individ.individtekst
"individtekst" "individtekst" ? ? "character" ? ? ? ? ? ? yes ? no 1000 yes ""
     _FldNameList[24]   > SkoTex.Individ.IndividType
"IndividType" "IndividType" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[25]   > SkoTex.Individ.Jegerkort
"Jegerkort" "Jegerkort" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[26]   > SkoTex.Individ.Kaliber
"Kaliber" "Kaliber" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[27]   > SkoTex.Individ.KjoptDato
"KjoptDato" "KjoptDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[28]   > SkoTex.Individ.KKundeNr
"KKundeNr" "KKundeNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[29]   > SkoTex.Individ.kordnr
"kordnr" "kordnr" ? ? "integer" ? ? ? ? ? ? yes ? yes 9.6 yes ""
     _FldNameList[30]   > SkoTex.Individ.kradnr
"kradnr" "kradnr" ? ? "integer" ? ? ? ? ? ? yes ? yes 6 yes ""
     _FldNameList[31]   > SkoTex.Individ.kundenr
"kundenr" "kundenr" ? ? "integer" ? ? ? ? ? ? yes ? yes 8 yes ""
     _FldNameList[32]   > SkoTex.Individ.LevNamn
"LevNamn" "LevNamn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[33]   > SkoTex.Individ.levnr
"levnr" "levnr" ? ? "integer" ? ? ? ? ? ? yes ? yes 9.6 yes ""
     _FldNameList[34]   > SkoTex.Individ.MobilTlf
"MobilTlf" "MobilTlf" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[35]   > SkoTex.Individ.Navn
"Navn" "Navn" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[36]   > SkoTex.Individ.NyVare
"NyVare" "NyVare" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[37]   > SkoTex.Individ.PersOrgNr
"PersOrgNr" "PersOrgNr" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[38]   > SkoTex.Individ.PostNr
"PostNr" "PostNr" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[39]   > SkoTex.Individ.Poststed
"Poststed" "Poststed" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[40]   > SkoTex.Individ.Pris
"Pris" "Pris" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[41]   > SkoTex.Individ.rapportert
"rapportert" "rapportert" ? ? "logical" ? ? ? ? ? ? yes ? yes 12 yes ""
     _FldNameList[42]   > SkoTex.Individ.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[43]   > SkoTex.Individ.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[44]   > SkoTex.Individ.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[45]   > SkoTex.Individ.SAdresse1
"SAdresse1" "SAdresse1" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[46]   > SkoTex.Individ.SAdresse2
"SAdresse2" "SAdresse2" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[47]   > SkoTex.Individ.salgdato
"salgdato" "salgdato" ? ? "date" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[48]   > SkoTex.Individ.SelgerNr
"SelgerNr" "SelgerNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[49]   > SkoTex.Individ.SeqNr
"SeqNr" "SeqNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[50]   > SkoTex.Individ.serienr
"serienr" "serienr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[51]   > SkoTex.Individ.SMobilTlf
"SMobilTlf" "SMobilTlf" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[52]   > SkoTex.Individ.SNavn
"SNavn" "SNavn" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[53]   > SkoTex.Individ.SPostNr
"SPostNr" "SPostNr" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[54]   > SkoTex.Individ.SPoststed
"SPoststed" "SPoststed" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[55]   > SkoTex.Individ.STelefon
"STelefon" "STelefon" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[56]   > SkoTex.Individ.Storl
"Storl" "Storl" ? ? "character" ? ? ? ? ? ? yes ? no 4 yes ""
     _FldNameList[57]   > SkoTex.Individ.StrKode
"StrKode" "StrKode" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[58]   > SkoTex.Individ.Telefon
"Telefon" "Telefon" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[59]   > SkoTex.Individ.VapenKort
"VapenKort" "VapenKort" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[60]   > SkoTex.Individ.VareKost
"VareKost" "VareKost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[61]   > SkoTex.Individ.vDB%
"vDB%" "vDB%" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[62]   > SkoTex.Individ.vDBKr
"vDBKr" "vDBKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[63]   > SkoTex.Individ.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[64]   > SkoTex.Individ.VmBeskrivelse
"VmBeskrivelse" "VmBeskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[65]   > SkoTex.Individ.VMId
"VMId" "VMId" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[66]   > SkoTex.Individ.VVarekost
"VVarekost" "VVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[67]   > "_<CALC>"
"getFarge()" "fFarbeskr" "Farge" "x(8)" "character" ? ? ? ? ? ? no ? no 15 no ?
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
         rowObject.fFarbeskr = (getFarge())
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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFarge dTables  _DB-REQUIRED
FUNCTION getFarge RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = Individ.Artikkelnr NO-LOCK NO-ERROR.
  IF AVAIL ArtBas THEN
      FIND Farg OF ArtBas NO-LOCK NO-ERROR.
  RETURN IF AVAIL Farg THEN Farg.farbeskr ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

