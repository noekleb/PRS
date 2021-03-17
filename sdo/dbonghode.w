&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
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
&Scoped-define INTERNAL-TABLES BongHode

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BongNr BongStatus Makulert ButikkNr DataSettId Dato EAv EDato ETid GruppeNr~
 KasseNr KassererNavn KassererNr Konvertert KundeKort KundeNr MedlemNavn~
 MedlemsKort MedlemsNr OAv ODato OpdKvit OpdUtskKopi OTid OverforingsNr~
 SelgerNavn SelgerNr Tid UtskriftsKopi Logg KundeNavn Belop KortType~
 Gradering
&Scoped-define ENABLED-FIELDS-IN-BongHode BongNr BongStatus Makulert ~
ButikkNr DataSettId Dato EAv EDato ETid GruppeNr KasseNr KassererNavn ~
KassererNr Konvertert KundeKort KundeNr MedlemNavn MedlemsKort MedlemsNr ~
OAv ODato OpdKvit OpdUtskKopi OTid OverforingsNr SelgerNavn SelgerNr Tid ~
UtskriftsKopi Logg KundeNavn Belop KortType Gradering 
&Scoped-Define DATA-FIELDS  BongNr fuKl BongStatus Makulert ButikkNr fuStatusTekst DataSettId Dato EAv~
 EDato ETid GruppeNr KasseNr KassererNavn KassererNr Konvertert KundeKort~
 KundeNr MedlemNavn MedlemsKort MedlemsNr OAv ODato OpdKvit OpdUtskKopi OTid~
 OverforingsNr SelgerNavn SelgerNr Tid UtskriftsKopi Logg KundeNavn Belop~
 KortType Gradering
&Scoped-define DATA-FIELDS-IN-BongHode BongNr BongStatus Makulert ButikkNr ~
DataSettId Dato EAv EDato ETid GruppeNr KasseNr KassererNavn KassererNr ~
Konvertert KundeKort KundeNr MedlemNavn MedlemsKort MedlemsNr OAv ODato ~
OpdKvit OpdUtskKopi OTid OverforingsNr SelgerNavn SelgerNr Tid ~
UtskriftsKopi Logg KundeNavn Belop KortType Gradering 
&Scoped-Define MANDATORY-FIELDS  BongNr ButikkNr GruppeNr KasseNr KassererNr SelgerNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dbonghode.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH BongHode NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH BongHode NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main BongHode
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main BongHode


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Kl dTables  _DB-REQUIRED
FUNCTION Kl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StatusTekst dTables  _DB-REQUIRED
FUNCTION StatusTekst RETURNS CHARACTER
  ( INPUT piStatus AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      BongHode SCROLLING.
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
         HEIGHT             = 1.67
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}
{sdo/dproclibstart.i}

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
     _TblList          = "Data.BongHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Data.BongHode.BongNr
"BongNr" "BongNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 9.6 yes ""
     _FldNameList[2]   > "_<CALC>"
"Kl()" "fuKl" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[3]   > Data.BongHode.BongStatus
"BongStatus" "BongStatus" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[4]   > Data.BongHode.Makulert
"Makulert" "Makulert" "Mak" ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[5]   > Data.BongHode.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[6]   > "_<CALC>"
"StatusTekst(RowObject.BongStatus)" "fuStatusTekst" "Status" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[7]   > Data.BongHode.DataSettId
"DataSettId" "DataSettId" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.8 yes ""
     _FldNameList[8]   > Data.BongHode.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[9]   > Data.BongHode.EAv
"EAv" "EAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[10]   > Data.BongHode.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[11]   > Data.BongHode.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[12]   > Data.BongHode.GruppeNr
"GruppeNr" "GruppeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.4 yes ""
     _FldNameList[13]   > Data.BongHode.KasseNr
"KasseNr" "KasseNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 8 yes ""
     _FldNameList[14]   > Data.BongHode.KassererNavn
"KassererNavn" "KassererNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[15]   > Data.BongHode.KassererNr
"KassererNr" "KassererNr" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[16]   > Data.BongHode.Konvertert
"Konvertert" "Konvertert" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 9.8 yes ""
     _FldNameList[17]   > Data.BongHode.KundeKort
"KundeKort" "KundeKort" ? ? "character" ? ? ? ? ? ? yes ? no 22 yes ""
     _FldNameList[18]   > Data.BongHode.KundeNr
"KundeNr" "KundeNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[19]   > Data.BongHode.MedlemNavn
"MedlemNavn" "MedlemNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[20]   > Data.BongHode.MedlemsKort
"MedlemsKort" "MedlemsKort" ? ? "character" ? ? ? ? ? ? yes ? no 16 yes ""
     _FldNameList[21]   > Data.BongHode.MedlemsNr
"MedlemsNr" "MedlemsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[22]   > Data.BongHode.OAv
"OAv" "OAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[23]   > Data.BongHode.ODato
"ODato" "ODato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[24]   > Data.BongHode.OpdKvit
"OpdKvit" "OpdKvit" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[25]   > Data.BongHode.OpdUtskKopi
"OpdUtskKopi" "OpdUtskKopi" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[26]   > Data.BongHode.OTid
"OTid" "OTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[27]   > Data.BongHode.OverforingsNr
"OverforingsNr" "OverforingsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[28]   > Data.BongHode.SelgerNavn
"SelgerNavn" "SelgerNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[29]   > Data.BongHode.SelgerNr
"SelgerNr" "SelgerNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[30]   > Data.BongHode.Tid
"Tid" "Tid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[31]   > Data.BongHode.UtskriftsKopi
"UtskriftsKopi" "UtskriftsKopi" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[32]   > Data.BongHode.Logg
"Logg" "Logg" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[33]   > Data.BongHode.KundeNavn
"KundeNavn" "KundeNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[34]   > Data.BongHode.Belop
"Belop" "Belop" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[35]   > Data.BongHode.KortType
"KortType" "KortType" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[36]   > Data.BongHode.Gradering
"Gradering" "Gradering" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
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
         rowObject.fuKl = (Kl())
         rowObject.fuStatusTekst = (StatusTekst(RowObject.BongStatus))
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
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
     INPUT TRUE /* LOGICAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSdo dTables  _DB-REQUIRED
PROCEDURE SokSdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcSokFelt    AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcSokValues  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcSokSort    AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcOperators  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pbFKeys      AS LOG  NO-UNDO.
  
  DEF VAR pcWhere  AS CHAR NO-UNDO.
  DEF VAR piLoop1  AS INT  NO-UNDO.
  DEF VAR piLoop2  AS INT  NO-UNDO.
  DEF VAR pcFelt   AS CHAR NO-UNDO.
  DEF VAR pcValues AS CHAR NO-UNDO.
  DEF VAR pcOp     AS CHAR NO-UNDO.


  /* Setter opp betingelse for fremmednøkler. */
  IF pbFKeys = FALSE THEN
  FKEYS:
  DO:
      ASSIGN
          pcFelt   = DYNAMIC-FUNCTION('getForeignFields':U)
          pcValues = DYNAMIC-FUNCTION('getForeignValues':U)
          pcWhere  = ""
          piLoop2  = 1
          .

      /* Legger på ForegnKeys */
      IF pcFelt <> "" THEN
      DO piLoop1 = 1 TO (NUM-ENTRIES(pcFelt) / 2):
        ASSIGN
          pcWhere  = pcWhere +
                     (IF pcWhere <> "" 
                         THEN " and "
                         ELSE "") +
                     ENTRY(piLoop2,pcFelt) + " = " + ENTRY(piLoop1,pcValues,CHR(1))
          piLoop2  = piLoop2 + 2 
          .
      END.
  END. /* FKEYS */

  /* Legger på søkemasken */
  IF pcSokFelt <> "" THEN
  DO piLoop1 = 1 TO (NUM-ENTRIES(pcSokFelt)):
    ASSIGN
      pcOp     = (IF pcOperators = ""
                    THEN "="
                    ELSE ENTRY(piLoop1,pcOperators))
      pcWhere  = pcWhere +
                 (IF pcWhere <> "" 
                     THEN " and "
                     ELSE "") +
                 ENTRY(piLoop1,pcSokFelt) + 
                 " " + pcOp + " " + 
                 ENTRY(piLoop1,pcSokValues,CHR(1))
      .
  END.

  /* Setter tilbake BASE Query */
  DYNAMIC-FUNCTION('setQueryWhere':U,
     INPUT "" /* CHARACTER */).

  /* Legger inn ny filterverdi */
  IF pcWhere <> "" THEN
    DYNAMIC-FUNCTION('addQueryWhere':U,
       INPUT pcWhere /* CHARACTER */,
        INPUT "",
        INPUT "").

  /* Legger inn valgt sortering */
  IF pcSokSort <> "" THEN
    DYNAMIC-FUNCTION('setQuerySort':U,
       INPUT pcSokSort /* CHARACTER */).

  /* Åpner query med ny filterverdi. */
  DYNAMIC-FUNCTION('openQuery':U).
         
 /*
 MESSAGE  DYNAMIC-FUNCTION('getQueryWhere':U)
     VIEW-AS ALERT-BOX INFO BUTTONS OK.
 */

  /* ?????
  /* For at ikke query skal forstyrres hvis man endrer på andre */
  /* fliker, settes query tilbake til standard qyery.           */
  /* Dette gjøres uten å kjøre OpenQuery. Det blir gjort når    */
  /* det f.eks byttes datasett på fliken foran.                 */
  /* Setter tilbake BASE Query */
  DYNAMIC-FUNCTION('setQueryWhere':U,
     INPUT "" /* CHARACTER */).
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Kl dTables  _DB-REQUIRED
FUNCTION Kl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN STRING(RowObject.Tid,"HH:MM:SS").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StatusTekst dTables  _DB-REQUIRED
FUNCTION StatusTekst RETURNS CHARACTER
  ( INPUT piStatus AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst       AS CHAR NO-UNDO.
  DEF VAR pcStatusListe AS CHAR NO-UNDO.


  RUN BongStatsTekst IN h_dproclib (INPUT piStatus).

  RETURN RETURN-VALUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

