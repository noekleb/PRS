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
&Scoped-define INTERNAL-TABLES KjedensButikker

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Adresse1 Adresse2 ButikkNavn ButikkNr DagligLeder E-Mail Firmanavn~
 Kontaktperson Medlemsstatus Mobil PostNr Telefaks Telefon DistriktNr~
 KjedeNr RegionNr BrukerID EDato ETid OrganisasjonsNr RegistrertAv~
 RegistrertDato RegistrertTid SegmentKode BeliggenhetId DriftsFormId~
 OppstartButikkdata UtmeldtDato DriftsTypeId
&Scoped-define ENABLED-FIELDS-IN-KjedensButikker Adresse1 Adresse2 ~
ButikkNavn ButikkNr DagligLeder E-Mail Firmanavn Kontaktperson ~
Medlemsstatus Mobil PostNr Telefaks Telefon DistriktNr KjedeNr RegionNr ~
BrukerID EDato ETid OrganisasjonsNr RegistrertAv RegistrertDato ~
RegistrertTid SegmentKode BeliggenhetId DriftsFormId OppstartButikkdata ~
UtmeldtDato DriftsTypeId 
&Scoped-Define DATA-FIELDS  Adresse1 fuPostSted Adresse2 ButikkNavn ButikkNr DagligLeder E-Mail~
 Firmanavn fuStatusTekst Kontaktperson Medlemsstatus Mobil PostNr Telefaks~
 Telefon DistriktNr KjedeNr RegionNr BrukerID EDato ETid OrganisasjonsNr~
 RegistrertAv RegistrertDato RegistrertTid SegmentKode BeliggenhetId~
 DriftsFormId OppstartButikkdata UtmeldtDato DriftsTypeId fuKjedenavn~
 fuRegionavn fuDistriktnavn
&Scoped-define DATA-FIELDS-IN-KjedensButikker Adresse1 Adresse2 ButikkNavn ~
ButikkNr DagligLeder E-Mail Firmanavn Kontaktperson Medlemsstatus Mobil ~
PostNr Telefaks Telefon DistriktNr KjedeNr RegionNr BrukerID EDato ETid ~
OrganisasjonsNr RegistrertAv RegistrertDato RegistrertTid SegmentKode ~
BeliggenhetId DriftsFormId OppstartButikkdata UtmeldtDato DriftsTypeId 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dkjedensbutikker.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH KjedensButikker NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH KjedensButikker NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main KjedensButikker
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main KjedensButikker


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Distriktnavn dTables  _DB-REQUIRED
FUNCTION Distriktnavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Kjedenavn dTables  _DB-REQUIRED
FUNCTION Kjedenavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD PostSted dTables  _DB-REQUIRED
FUNCTION PostSted RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Regionnavn dTables  _DB-REQUIRED
FUNCTION Regionnavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StatusTekst dTables  _DB-REQUIRED
FUNCTION StatusTekst RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      KjedensButikker SCROLLING.
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
     _TblList          = "SkoTex.KjedensButikker"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > SkoTex.KjedensButikker.Adresse1
"Adresse1" "Adresse1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[2]   > "_<CALC>"
"Poststed()" "fuPostSted" "Poststed" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[3]   > SkoTex.KjedensButikker.Adresse2
"Adresse2" "Adresse2" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[4]   > SkoTex.KjedensButikker.ButikkNavn
"ButikkNavn" "ButikkNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[5]   > SkoTex.KjedensButikker.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[6]   > SkoTex.KjedensButikker.DagligLeder
"DagligLeder" "DagligLeder" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[7]   > SkoTex.KjedensButikker.E-Mail
"E-Mail" "E-Mail" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[8]   > SkoTex.KjedensButikker.Firmanavn
"Firmanavn" "Firmanavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[9]   > "_<CALC>"
"StatusTekst()" "fuStatusTekst" ? "x(25)" "character" ? ? ? ? ? ? no ? no 25 no ?
     _FldNameList[10]   > SkoTex.KjedensButikker.Kontaktperson
"Kontaktperson" "Kontaktperson" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[11]   > SkoTex.KjedensButikker.Medlemsstatus
"Medlemsstatus" "Medlemsstatus" ? ? "integer" ? ? ? ? ? ? yes ? no 14 yes ""
     _FldNameList[12]   > SkoTex.KjedensButikker.Mobil
"Mobil" "Mobil" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[13]   > SkoTex.KjedensButikker.PostNr
"PostNr" "PostNr" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[14]   > SkoTex.KjedensButikker.Telefaks
"Telefaks" "Telefaks" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[15]   > SkoTex.KjedensButikker.Telefon
"Telefon" "Telefon" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[16]   > SkoTex.KjedensButikker.DistriktNr
"DistriktNr" "DistriktNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.4 yes ""
     _FldNameList[17]   > SkoTex.KjedensButikker.KjedeNr
"KjedeNr" "KjedeNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[18]   > SkoTex.KjedensButikker.RegionNr
"RegionNr" "RegionNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes ""
     _FldNameList[19]   > SkoTex.KjedensButikker.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[20]   > SkoTex.KjedensButikker.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[21]   > SkoTex.KjedensButikker.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[22]   > SkoTex.KjedensButikker.OrganisasjonsNr
"OrganisasjonsNr" "OrganisasjonsNr" ? ? "character" ? ? ? ? ? ? yes ? no 15.4 yes ""
     _FldNameList[23]   > SkoTex.KjedensButikker.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[24]   > SkoTex.KjedensButikker.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[25]   > SkoTex.KjedensButikker.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[26]   > SkoTex.KjedensButikker.SegmentKode
"SegmentKode" "SegmentKode" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes ""
     _FldNameList[27]   > SkoTex.KjedensButikker.BeliggenhetId
"BeliggenhetId" "BeliggenhetId" ? ? "integer" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[28]   > SkoTex.KjedensButikker.DriftsFormId
"DriftsFormId" "DriftsFormId" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[29]   > SkoTex.KjedensButikker.OppstartButikkdata
"OppstartButikkdata" "OppstartButikkdata" ? ? "date" ? ? ? ? ? ? yes ? no 18.6 yes ""
     _FldNameList[30]   > SkoTex.KjedensButikker.UtmeldtDato
"UtmeldtDato" "UtmeldtDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[31]   > SkoTex.KjedensButikker.DriftsTypeId
"DriftsTypeId" "DriftsTypeId" ? ? "integer" ? ? ? ? ? ? yes ? no 8.8 yes ""
     _FldNameList[32]   > "_<CALC>"
"Kjedenavn()" "fuKjedenavn" "Kjedenavn" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[33]   > "_<CALC>"
"Regionnavn()" "fuRegionavn" "Regionnavn" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[34]   > "_<CALC>"
"Distriktnavn()" "fuDistriktnavn" "Distriktnavn" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
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
         rowObject.fuDistriktnavn = (Distriktnavn())
         rowObject.fuKjedenavn = (Kjedenavn())
         rowObject.fuPostSted = (Poststed())
         rowObject.fuRegionavn = (Regionnavn())
         rowObject.fuStatusTekst = (StatusTekst())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE finnKjedeButikk dTables  _DB-REQUIRED
PROCEDURE finnKjedeButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipKjButikk LIKE KjedensButikker.ButikkNr NO-UNDO.
    DEFINE OUTPUT PARAMETER cVerdier AS CHARACTER  NO-UNDO.
    DEFINE BUFFER bKjedensButikker FOR KjedensButikker.
    FIND bKjedensButikker WHERE bKjedensButikker.ButikkNr = ipKjButikk NO-LOCK NO-ERROR.
    IF AVAIL bKjedensButikker THEN
        ASSIGN cVerdier = STRING(bKjedensButikker.KjedeNr)    + CHR(1) +
                          STRING(bKjedensButikker.RegionNr)   + CHR(1) +
                          STRING(bKjedensButikker.DistriktNr) + CHR(1) +
                          STRING(bKjedensButikker.ButikkNr).

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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
                             INPUT TRUE).                                                
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

      /* Kontrollerer dubletter */
      IF CAN-FIND(KjedensButikker WHERE
                  KjedensButikker.ButikkNr = RowObject.ButikkNr) THEN
          RETURN "Kjedebutikk " + string(RowObject.ButikkNr) +  " er allerede registrert.".
  END.
  /* Kontroll ved sletting av poster. */
  FOR EACH RowObjUpd WHERE
      CAN-DO("D",RowObjUpd.RowMod):

      /* Sjekker om det finnes betalingstransaksjoner */
      IF CAN-FIND(FIRST Butiker WHERE
                  Butiker.Butik = RowObjUpd.ButikkNr) THEN
          RETURN "Det er lagt opp en butikk på denne kjedebutikkern - " + 
                  STRING(RowObjUpd.ButikkNr) +  ". " +
                 "Posten kan ikke slettes.".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkPostNr dTables  _DB-REQUIRED
PROCEDURE SjekkPostNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcPostNr AS CHAR NO-UNDO.

  FIND Post NO-LOCK WHERE
      Post.PostNr = pcPostNr NO-ERROR.
  IF AVAILABLE Post THEN
      RETURN STRING(Post.Beskrivelse).
  ELSE
      RETURN "AVBRYT".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Distriktnavn dTables  _DB-REQUIRED
FUNCTION Distriktnavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND KjedeDistrikt WHERE KjedeDistrikt.KjedeNr = RowObject.KjedeNr AND 
                           KjedeDistrikt.RegionNr = RowObject.RegionNr AND 
                           KjedeDistrikt.DistriktNr = RowObject.DistriktNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL KjedeDistrikt THEN KjedeDistrikt.DistriktNavn ELSE "Ukjent".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Kjedenavn dTables  _DB-REQUIRED
FUNCTION Kjedenavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Kjede WHERE Kjede.KjedeNr = RowObject.KjedeNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL Kjede THEN Kjede.KjedeNavn ELSE "Ukjent".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION PostSted dTables  _DB-REQUIRED
FUNCTION PostSted RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Post NO-LOCK WHERE
      Post.PostNr = RowObject.PostNr NO-ERROR.
  IF AVAILABLE Post THEN
      RETURN Post.Beskrivelse.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Regionnavn dTables  _DB-REQUIRED
FUNCTION Regionnavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND KjedeRegion WHERE KjedeRegion.KjedeNr = RowObject.KjedeNr AND 
                         KjedeRegion.RegionNr = RowObject.RegionNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL KjedeRegion THEN KjedeRegion.RegionNavn ELSE "Ukjent".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StatusTekst dTables  _DB-REQUIRED
FUNCTION StatusTekst RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE RowObject.Medlemsstatus:
      WHEN 1 THEN RETURN "Medlem".
      WHEN 8 THEN RETURN "Observatør".
      WHEN 9 THEN RETURN "Sluttet".
      OTHERWISE RETURN "*Ukjent*".
  END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

