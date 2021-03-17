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
DEF VAR iVg   AS INT NO-UNDO.
DEF VAR iMode AS INT NO-UNDO. /* 0-Alle, 1-kun 1 -> 9999 */
DEF VAR iCl   AS INT NO-UNDO.

{syspara.i 5 1 1 iCl INT}

DEF BUFFER clButiker FOR Butiker.

FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl.

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
&Scoped-Define ENABLED-FIELDS  ArtikkelNr Vg LopNr Beskr BongTekst Aktivert IKasse LevNr LevKod LevFargKod~
 Farg SaSong Pakke Lokasjon OPris BildeIKasse HkStyrt LokPris BildNr~
 RegistrertDato EDato Hg
&Scoped-define ENABLED-FIELDS-IN-ArtBas ArtikkelNr Vg LopNr Beskr BongTekst ~
Aktivert IKasse LevNr LevKod LevFargKod Farg SaSong Pakke Lokasjon OPris ~
BildeIKasse HkStyrt LokPris BildNr RegistrertDato EDato Hg 
&Scoped-Define DATA-FIELDS  ArtikkelNr fuBildeFilNavn Vg LopNr Beskr BongTekst Aktivert fSesong IKasse~
 LevNr LevKod LevFargKod Farg SaSong Pakke Lokasjon fuPris OPris BildeIKasse~
 fuVarekost HkStyrt LokPris BildNr fFargBeskr RegistrertDato EDato Hg~
 fLevNamn
&Scoped-define DATA-FIELDS-IN-ArtBas ArtikkelNr Vg LopNr Beskr BongTekst ~
Aktivert IKasse LevNr LevKod LevFargKod Farg SaSong Pakke Lokasjon OPris ~
BildeIKasse HkStyrt LokPris BildNr RegistrertDato EDato Hg 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dartbassok.i"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BildeFilNavn dTables  _DB-REQUIRED
FUNCTION BildeFilNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFargBeskr dTables  _DB-REQUIRED
FUNCTION fFargBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLevNavn dTables  _DB-REQUIRED
FUNCTION fLevNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSesong dTables  _DB-REQUIRED
FUNCTION fSesong RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuVarekost dTables  _DB-REQUIRED
FUNCTION fuVarekost RETURNS DECIMAL
  ( INPUT plArtikkelNr AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD openQuery dTables  _DB-REQUIRED
FUNCTION openQuery RETURNS LOGICAL
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
     _TblList          = "skotex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.ArtBas.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[2]   > "_<CALC>"
"BildeFilNavn()" "fuBildeFilNavn" "Bilde" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[3]   > skotex.ArtBas.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[4]   > skotex.ArtBas.LopNr
"LopNr" "LopNr" ? "zzzzz9" "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[5]   > skotex.ArtBas.Beskr
"Beskr" "Beskr" ? "x(30)" "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[6]   > skotex.ArtBas.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[7]   > skotex.ArtBas.Aktivert
"Aktivert" "Aktivert" "A" "*~~/" "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[8]   > "_<CALC>"
"fSesong()" "fSesong" "Sesong" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no ?
     _FldNameList[9]   > skotex.ArtBas.IKasse
"IKasse" "IKasse" "IK" "*~~/" "logical" ? ? ? ? ? ? yes ? no 2 yes ""
     _FldNameList[10]   > skotex.ArtBas.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 yes ""
     _FldNameList[11]   > skotex.ArtBas.LevKod
"LevKod" "LevKod" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[12]   > skotex.ArtBas.LevFargKod
"LevFargKod" "LevFargKod" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[13]   > skotex.ArtBas.Farg
"Farg" "Farg" ? "zzzz9" "integer" ? ? ? ? ? ? yes ? no 5.2 yes ""
     _FldNameList[14]   > skotex.ArtBas.SaSong
"SaSong" "SaSong" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[15]   > skotex.ArtBas.Pakke
"Pakke" "Pakke" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[16]   > skotex.ArtBas.Lokasjon
"Lokasjon" "Lokasjon" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[17]   > "_<CALC>"
"fuPris(RowObject.ArtikkelNr)" "fuPris" "Pris" "->>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 11.4 no ?
     _FldNameList[18]   > skotex.ArtBas.OPris
"OPris" "OPris" ? ? "logical" ? ? ? ? ? ? yes ? no 5 yes ""
     _FldNameList[19]   > skotex.ArtBas.BildeIKasse
"BildeIKasse" "BildeIKasse" ? ? "logical" ? ? ? ? ? ? yes ? no 4.6 yes ""
     _FldNameList[20]   > "_<CALC>"
"fuVarekost(RowObject.ArtikkelNr)" "fuVarekost" "Varekost" "->>>,>>9.99" "Decimal" ? ? ? ? ? ? no ? no 11.4 no ?
     _FldNameList[21]   > skotex.ArtBas.HkStyrt
"HkStyrt" "HkStyrt" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[22]   > skotex.ArtBas.LokPris
"LokPris" "LokPris" ? ? "logical" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[23]   > skotex.ArtBas.BildNr
"BildNr" "BildNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[24]   > "_<CALC>"
"fFargBeskr()" "fFargBeskr" "Farge" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[25]   > skotex.ArtBas.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[26]   > skotex.ArtBas.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[27]   > skotex.ArtBas.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[28]   > "_<CALC>"
"fLevNavn()" "fLevNamn" "Leverandør" "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvgrensGyldigeLopNr dTables  _DB-REQUIRED
PROCEDURE AvgrensGyldigeLopNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcWhere AS CHAR NO-UNDO.
  DEF VAR pcBuffer AS CHAR NO-UNDO.
  DEF VAR pcAndOr  AS CHAR NO-UNDO.

  ASSIGN
    pcWhere = "ArtBas.LopNr >= 1 and ArtBas.LopNr <= 9999".

  DYNAMIC-FUNCTION('addQueryWhere':U,
     INPUT pcWhere /* CHARACTER */,
      INPUT pcBuffer /* CHARACTER */,
      INPUT pcAndOr /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvgrensPaVg dTables  _DB-REQUIRED
PROCEDURE AvgrensPaVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcWhere  AS CHAR NO-UNDO.
  DEF VAR pcBuffer AS CHAR NO-UNDO.
  DEF VAR pcAndOr  AS CHAR NO-UNDO.

  ASSIGN
    pcWhere = "ArtBas.Vg = " + STRING(iVg).

  DYNAMIC-FUNCTION('addQueryWhere':U,
     INPUT pcWhere /* CHARACTER */,
      INPUT pcBuffer /* CHARACTER */,
      INPUT pcAndOr /* CHARACTER */).
  
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
         rowObject.fFargBeskr = (fFargBeskr())
         rowObject.fLevNamn = (fLevNavn())
         rowObject.fSesong = (fSesong())
         rowObject.fuBildeFilNavn = (BildeFilNavn())
         rowObject.fuPris = (fuPris(RowObject.ArtikkelNr))
         rowObject.fuVarekost = (fuVarekost(RowObject.ArtikkelNr))
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetMode dTables  _DB-REQUIRED
PROCEDURE SetMode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT parameter piMode AS INT NO-UNDO.

  ASSIGN
      iMode = piMode
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSortering dTables  _DB-REQUIRED
PROCEDURE SetSortering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piSortering AS INT NO-UNDO.

  DEF var pcSort AS CHAR NO-UNDO.

  CASE piSortering:
      WHEN 1 THEN pcSort = "by ArtBas.ArtikkelNr".
      WHEN 2 THEN pcSort = "by ArtBas.Vg by ArtBas.LopNr".
      WHEN 3 THEN pcSort = "by ArtBas.Beskr".
      WHEN 4 THEN pcSort = "by ArtBas.LevNr by ArtBas.LevKod".
  END CASE.

  DYNAMIC-FUNCTION('setQuerySort':U,
     INPUT pcSort /* CHARACTER */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVg dTables  _DB-REQUIRED
PROCEDURE SetVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piVg AS INT NO-UNDO.

  ASSIGN
      iVg = piVg.
  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BildeFilNavn dTables  _DB-REQUIRED
FUNCTION BildeFilNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Bilderegister NO-LOCK where
      BildeRegister.BildNr = RowObject.BildNr NO-ERROR.
  IF AVAILABLE BildeRegister THEN
      BildeRegister.FilNavn.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFargBeskr dTables  _DB-REQUIRED
FUNCTION fFargBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Farg NO-LOCK WHERE
      Farg.Farg = RowObject.Farg NO-ERROR.
  IF AVAILABLE Farg THEN
      RETURN Farg.FarBeskr.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLevNavn dTables  _DB-REQUIRED
FUNCTION fLevNavn RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSesong dTables  _DB-REQUIRED
FUNCTION fSesong RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Sasong NO-LOCK WHERE
      RowObject.Sasong = Sasong.Sasong NO-ERROR.
  IF AVAILABLE Sasong THEN
      RETURN Sasong.SasBeskr.
  ELSE
      RETURN "".   /* Function return value. */

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
  FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr AND
      ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
      RETURN 0.00.   /* Function return value. */
  ELSE
      RETURN ArtPris.Pris[1].

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
  FIND ArtPris NO-LOCK WHERE
      ArtPris.ArtikkelNr = plArtikkelNr AND
      ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
      RETURN 0.00.   /* Function return value. */
  ELSE
      RETURN ArtPris.Varekost[1].

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION openQuery dTables  _DB-REQUIRED
FUNCTION openQuery RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RETURN SUPER( ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

