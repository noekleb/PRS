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
DEF VAR cStatusTekster    AS CHAR NO-UNDO.
DEF VAR cBehStatusTekster AS CHAR NO-UNDO.
DEF VAR cGraderingFilLogg AS CHAR NO-UNDO.
DEF VAR cFilTypeTekst     AS CHAR NO-UNDO.

DEF VAR hInstance         AS INT  NO-UNDO.

/* Språkhåndtering */
DEF VAR cCurrLng   AS CHAR   NO-UNDO.

DEF VAR wSystemNavn       AS CHAR   NO-UNDO INIT "[SkoTex]".
DEF VAR wLokalIniFil      AS CHAR   NO-UNDO INIT "sk-lok.ini".
DEF VAR wMappeLokalIniFil AS CHAR   NO-UNDO INIT "C:\WINDOWS".
DEF VAR wHuskPos          AS LOG    NO-UNDO INIT TRUE.
DEF VAR wSistKompilert    AS CHAR   NO-UNDO.
DEF VAR wSkoDB            AS CHAR   NO-UNDO.
DEF VAR wWrDB             AS CHAR   NO-UNDO.
DEF VAR wProgram          AS HANDLE EXTENT 20 NO-UNDO.
DEF VAR wEndringsNr       AS INT    NO-UNDO.
DEF VAR hLngHandle        AS HANDLE NO-UNDO.
DEFINE VARIABLE cVikt         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTidskrift    AS CHARACTER   NO-UNDO.


DEF STREAM Stream1.

/* For kalkyle av artikkel. */
DEF VAR FI-EuroKurs  AS DEC NO-UNDO.
DEF VAR FI-Pris      AS DEC NO-UNDO.
DEF VAR FI-Mva%      AS DEC NO-UNDO.
DEF VAR wWork        AS DEC NO-UNDO.

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
&Scoped-define INTERNAL-TABLES Fylke

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  FylkesNr
&Scoped-define ENABLED-FIELDS-IN-Fylke FylkesNr 
&Scoped-Define DATA-FIELDS  FylkesNr
&Scoped-define DATA-FIELDS-IN-Fylke FylkesNr 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dproclib.i"
&Scoped-define SELF-NAME Query-Main
&Scoped-define QUERY-STRING-Query-Main FOR EACH Fylke       WHERE true NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY {&SELF-NAME} FOR EACH Fylke       WHERE true NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Fylke
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Fylke


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EANprefixKonv dTables  _DB-REQUIRED
FUNCTION EANprefixKonv RETURNS CHARACTER
  ( INPUT cEANkode AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EkspanderUPC dTables  _DB-REQUIRED
FUNCTION EkspanderUPC RETURNS CHARACTER
  ( INPUT cUPCtoExpand AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fixChkEAN dTables  _DB-REQUIRED
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Fylke SCROLLING.
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
{win\windows.i}
{hjelp.i}

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
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Fylke
      WHERE true NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "true"
     _FldNameList[1]   > skotex.Fylke.FylkesNr
"FylkesNr" "FylkesNr" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Query-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */
  
  SUBSCRIBE "NyFilLogg" ANYWHERE.
  
  RUN InitVariabler.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Appdir dTables  _DB-REQUIRED
PROCEDURE Appdir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wAppdir AS CHAR NO-UNDO.

  GET-KEY-VALUE SECTION "SYSPARA" KEY "Appdir" VALUE wAppdir.
  IF wAppDir = ? THEN
    DO:
      {syspara.i 1 1 7 wAppdir}
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BehandletStatus dTables  _DB-REQUIRED
PROCEDURE BehandletStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piBehandlet AS INT NO-UNDO.

  DEF VAR pcBehandletStatus AS CHAR NO-UNDO.

  CASE piBehandlet:
      WHEN 1 THEN pcBehandletStatus = ENTRY(1,cBehStatusTekster).
      WHEN 2 THEN pcBehandletStatus = ENTRY(3,cBehStatusTekster).
      WHEN 3 THEN pcBehandletStatus = ENTRY(5,cBehStatusTekster).
      WHEN 4 THEN pcBehandletStatus = ENTRY(7,cBehStatusTekster).
      WHEN 5 THEN pcBehandletStatus = ENTRY(9,cBehStatusTekster).
      WHEN 8 THEN pcBehandletStatus = ENTRY(11,cBehStatusTekster).
      WHEN 9 THEN pcBehandletStatus = ENTRY(13,cBehStatusTekster).
      OTHERWISE   pcbehandletStatus = "*Ukjent*".
  END CASE.

  RETURN pcBehandletStatus.   /* Function return value. */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongStatsTekst dTables  _DB-REQUIRED
PROCEDURE BongStatsTekst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piStatus AS INT NO-UNDO.

  DEF VAR pcTekst       AS CHAR NO-UNDO.
  DEF VAR pcStatusTekst AS CHAR NO-UNDO.

  {syspara.i 1 11 3 pcTekst}
  IF pcTekst = "" THEN
    ASSIGN
      pcTekst = "Ikke klar,1,Klar,2,Under oppdatering,3,Delhvis oppdatert,4,Oppdatert,5,Delhvis overført,6,Overført,7,Slettet,9"
      .

  CASE piStatus:
      WHEN 1 THEN pcStatusTekst = ENTRY(1,pcTekst).
      WHEN 2 THEN pcStatusTekst = ENTRY(3,pcTekst).
      WHEN 3 THEN pcStatusTekst = ENTRY(5,pcTekst).
      WHEN 4 THEN pcStatusTekst = ENTRY(7,pcTekst).
      WHEN 5 THEN pcStatusTekst = ENTRY(9,pcTekst).
      WHEN 6 THEN pcStatusTekst = ENTRY(11,pcTekst).
      WHEN 7 THEN pcStatusTekst = ENTRY(13,pcTekst).
      WHEN 9 THEN pcStatusTekst = ENTRY(15,pcTekst).
      OTHERWISE   pcStatusTekst = "*Ukjent*".
  END CASE.

  RETURN pcStatusTekst.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DirektePrisOppdat dTables  _DB-REQUIRED
PROCEDURE DirektePrisOppdat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wSvar AS LOG NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.


/* Denne parameter skal ikke lenger benyttes..........................
  GET-KEY-VALUE SECTION "SYSPARA" KEY "DirektePrisOppdat" VALUE wTekst.
  if wTekst = ? then
    DO:
      {syspara.i 2 1 10 wtekst}
    END.
  if CAN-DO("True,Ja,Yes,1",wTekst) then
    wSvar = TRUE.
  else
    wSvar = FALSE.
*/
  wSvar = FALSE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Euro dTables  _DB-REQUIRED
PROCEDURE Euro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wKurs AS DEC NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.


  GET-KEY-VALUE SECTION "SYSPARA" KEY "EuroKurs" VALUE wTekst.
  IF wTekst = ? THEN
    DO:
      {syspara.i 2 1 1 wtekst}
    END.
  ASSIGN wKurs = DEC(wTekst).
  IF wKurs = ? OR wKurs = 0 THEN
    wKurs = 0.5.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilMottakStatus dTables 
PROCEDURE FilMottakStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piStatus AS INT NO-UNDO.

  DEF VAR pcStatusTekst AS CHAR NO-UNDO.

  CASE piStatus:
      WHEN 1 THEN pcStatusTekst = ENTRY(1,cStatusTekster).
      WHEN 2 THEN pcStatusTekst = ENTRY(3,cStatusTekster).
      WHEN 3 THEN pcStatusTekst = ENTRY(5,cStatusTekster).
      WHEN 8 THEN pcStatusTekst = ENTRY(7,cStatusTekster).
      WHEN 9 THEN pcStatusTekst = ENTRY(9,cStatusTekster).
      OTHERWISE   pcStatusTekst = "*Ukjent*".
  END CASE.

  RETURN pcStatusTekst.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilTypeTekst dTables  _DB-REQUIRED
PROCEDURE FilTypeTekst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piFilType AS INT NO-UNDO.

  DEF VAR pcFilTypeTekst AS CHAR NO-UNDO.  

  CASE piFilType:
      WHEN 1 THEN pcFilTypeTekst = ENTRY(1,cFiltypeTekst).
      WHEN 2 THEN pcFilTypeTekst = ENTRY(3,cFiltypeTekst).
      WHEN 3 THEN pcFilTypeTekst = ENTRY(5,cFiltypeTekst).
      WHEN 4 THEN pcFilTypeTekst = ENTRY(7,cFiltypeTekst).
      WHEN 5 THEN pcFilTypeTekst = ENTRY(9,cFiltypeTekst).
      OTHERWISE pcFilTypeTekst   = "*Ukjent*".
  END CASE.

  RETURN pcFilTypeTekst.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl dTables  _DB-REQUIRED
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAMETER wStorl AS CHAR NO-UNDO.

  DEF VAR wDecimaler AS CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  ASSIGN
     wStorl = TRIM(wStorl)
     wStorl = CAPS(wStorl)
     wStorl = IF (LENGTH(wStorl) = 1 OR
                  LENGTH(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(wStorl,",") <> 0 THEN
    OVERLAY(wStorl, INDEX(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  /* TN 14/4-11 Denne kontrollen er blitt meningsløs da det kommer inn så mye 
     rare størrelser.
  IF NUM-ENTRIES(wStorl,".") = 2 THEN
    DO:
      IF NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) THEN
        wStorl = ENTRY(1,wStorl,".").
    END.
  */

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlle dTables  _DB-REQUIRED
PROCEDURE GetAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcAlle AS CHAR NO-UNDO.

  {syspara.i 1 100 1 pcAlle}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBehandletStatus dTables  _DB-REQUIRED
PROCEDURE GetBehandletStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 11 2 pcTekst}.
  IF pcTekst <> "" THEN
      cBehStatusTekster = pcTekst.


  ASSIGN
    pcTekst = cBehStatusTekster
    .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetBongStatusListe dTables  _DB-REQUIRED
PROCEDURE GetBongStatusListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst    AS CHAR NO-UNDO.

  DEF VAR piLoop1 AS INT  NO-UNDO.

  {syspara.i 1 11 3 pcTekst}
  IF pcTekst = "" THEN
    ASSIGN
      pcTekst = "Ikke klar,1,Klar,2,Delhvis oppdatert,3,Oppdatert,4,Delhvis overført5,Overført,6,Slettet,9"
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetButikkListe dTables  _DB-REQUIRED
PROCEDURE GetButikkListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  BUTIKKLOOP:
  FOR EACH Butiker NO-LOCK:
      IF piLoop > 39 THEN
      DO:
          ASSIGN
              pcTekst = pcTekst +
                        (IF pcTekst = ""
                           THEN ""
                           ELSE ",") + 
                        "* For mange butikker *," +
                        STRING(Butiker.Butik)
              .
          LEAVE BUTIKKLOOP.
      END.

      ASSIGN
          piLoop  = piLoop  + 1
          pcTekst = pcTekst +
                    (IF pcTekst = ""
                       THEN ""
                       ELSE ",") + 
                    string(Butiker.Butik,"zzzzz9") + ": " + Butiker.KortNavn + "," +
                    STRING(Butiker.Butik)
          .
  END. /* BUTIKKLOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetCl dTables  _DB-REQUIRED
PROCEDURE GetCl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER piCl AS INT NO-UNDO.

  {syspara.i 5 1 1 piCl INT}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetExcelExtent dTables  _DB-REQUIRED
PROCEDURE GetExcelExtent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 4 1 pcTekst}
  pcTekst = IF pcTekst = "" 
              THEN "sdv" 
              ELSE pcTekst.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFilTypeTekst dTables  _DB-REQUIRED
PROCEDURE GetFilTypeTekst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 11 5 pcTekst}.
  IF pcTekst <> "" THEN
      cFilTypeTekst = pcTekst.


  ASSIGN
    pcTekst = cFilTypeTekst.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetGraderingFilLogg dTables  _DB-REQUIRED
PROCEDURE GetGraderingFilLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 11 4 pcTekst}.
  IF pcTekst <> "" THEN
      cGraderingFilLogg = pcTekst.


  ASSIGN
    pcTekst = cGraderingFilLogg
    .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKasseListe dTables  _DB-REQUIRED
PROCEDURE GetKasseListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcButikkNr AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcTekst    AS CHAR NO-UNDO.

  FOR EACH Kasse NO-LOCK WHERE
      Kasse.ButikkNr = pcButikkNr:
      ASSIGN
          pcTekst = pcTekst +
                    (IF pcTekst = ""
                       THEN ""
                       ELSE ",") + 
                    (IF pcButikkNr <> 0
                       THEN ""
                       ELSE STRING(Kasse.ButikkNr,"zzzzz9") + " ") + 
                    string(Kasse.KasseNr,"zz9") + ": " + Kasse.Navn + "," +
                    string(Kasse.butikkNr) + "|" + STRING(Kasse.KasseNr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLng dTables  _DB-REQUIRED
PROCEDURE GetLng :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcCurrLng   AS CHAR NO-UNDO.

  ASSIGN
      pcCurrLng   = cCurrLng
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLngHandle dTables  _DB-REQUIRED
PROCEDURE GetLngHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER phLngHandle AS HANDLE NO-UNDO.
  
  ASSIGN
      phLngHandle = hLngHandle
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetMottakStatus dTables  _DB-REQUIRED
PROCEDURE GetMottakStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 11 1 pcTekst}.
  IF pcTekst <> "" THEN
      cStatusTekster = pcTekst.


  ASSIGN
    pcTekst = cStatusTekster.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSorteringStatRap dTables  _DB-REQUIRED
PROCEDURE GetSorteringStatRap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER piSysHId AS INT  NO-UNDO.
DEF INPUT  PARAMETER piSysGr  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER pcTekst  AS CHAR NO-UNDO.
        
FOR EACH SysPara NO-LOCK WHERE
    SysPara.SysHID = 9 AND
    SysPara.SysGr  = 250:

    ASSIGN
        pcTekst = pcTekst + 
                  (IF pcTekst <> ""
                     THEN ","
                     ELSE "") + 
                  SysPara.Parameter1 + "," + string(SysPara.ParaNr)
        .

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSysPara dTables  _DB-REQUIRED
PROCEDURE GetSysPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  DEF VAR pcParam AS CHAR NO-UNDO.

  IF NUM-ENTRIES(pcTekst) <> 3 THEN
  DO:
      ASSIGN
          pcTekst = ""
          .
      RETURN.
  END.
  ASSIGN
      pcParam = pcTekst
      pcTekst = ""
      .

  {syspara.i int(entry(1,pcParam))
             int(entry(2,pcParam))
             int(entry(3,pcParam)) pcTekst}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTempFileName dTables  _DB-REQUIRED
PROCEDURE GetTempFileName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipPrefix      AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER ipExtent      AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER iptmpFileName AS CHAR NO-UNDO.

  /* Dette gir et windows tempfile name. Men da virker ikke macroen i Excel??
  RUN gettmpfile.p (INPUT ipPrefix).
  assign
    iptmpFileName = RETURN-VALUE.
  OVERLAY(iptmpFileName, R-INDEX(iptmpFileName,".") + 1, 3) = ipExtent.
  */

  DEF VAR iptmpDirName AS CHAR NO-UNDO.
  
  ASSIGN
    iptmpDirName = SESSION:TEMP-DIRECTORY.
  
  LET_LOOP:
  DO WHILE TRUE:
    ASSIGN
      iptmpFileName = iptmpDirName +
                      ipPrefix + string(RANDOM(1,9999),"9999") +
                      "." + ipExtent.
    IF SEARCH(iptmpFileName) = ? THEN
      LEAVE LET_LOOP.
  END. /* LET_LOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTransTypeTekster dTables  _DB-REQUIRED
PROCEDURE GetTransTypeTekster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  FOR EACH TransType NO-LOCK:
      ASSIGN
          pcTekst = pcTekst + 
                    (IF pcTekst = ""
                       THEN ""
                       ELSE ",") + 
                    STRING(TransType.TTId,"999") + " " + 
                    TransType.Beskrivelse + "," + 
                    STRING(TransType.TTId,"999")
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GraderingFilLogg dTables  _DB-REQUIRED
PROCEDURE GraderingFilLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piStatus AS INT NO-UNDO.

  DEF VAR pcStatusTekst AS CHAR NO-UNDO.
  
  IF NUM-ENTRIES(cGraderingFilLogg) >= 9 THEN
  CASE piStatus:
      WHEN 0 THEN pcStatusTekst = ENTRY(1,cGraderingFilLogg).
      WHEN 1 THEN pcStatusTekst = ENTRY(3,cGraderingFilLogg).
      WHEN 2 THEN pcStatusTekst = ENTRY(5,cGraderingFilLogg).
      WHEN 3 THEN pcStatusTekst = ENTRY(7,cGraderingFilLogg).
      WHEN 4 THEN pcStatusTekst = ENTRY(9,cGraderingFilLogg).
      WHEN 5 THEN pcStatusTekst = ENTRY(11,cGraderingFilLogg).
      WHEN 8 THEN pcStatusTekst = ENTRY(15,cGraderingFilLogg).
      WHEN 9 THEN pcStatusTekst = ENTRY(13,cGraderingFilLogg).
      OTHERWISE   pcStatusTekst = "*Ukjent*".
  END CASE.
  ELSE
      pcStatusTekst = "*Ukjent*".

  RETURN pcStatusTekst.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentLapTop dTables  _DB-REQUIRED
PROCEDURE HentLapTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wLapTopNr AS INT NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.


  GET-KEY-VALUE SECTION "SYSPARA" KEY "LapTopNr" VALUE wTekst.
  IF wTekst = ? THEN
    wLapTopNr = ?.
  ELSE
    wLapTopNr = INT(wTekst).
  IF NOT CAN-FIND(LapTop WHERE
                  LapTop.LapTopNr = wLapTopNr) THEN
    wLapTopNr = ?.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentParametre dTables  _DB-REQUIRED
PROCEDURE HentParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER wSection AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER wKey     AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wValue   AS CHAR NO-UNDO.
  IF wSection = ? OR wKey = ? THEN
    RETURN "AVBRYT".

  SaveSettings:
  DO:
    IF wMappeLokalInifil <> ? THEN DO:
       LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
          IF ERROR-STATUS:ERROR THEN LEAVE SaveSettings.
       END.
           
       USE wLokalIniFil NO-ERROR.
       IF NOT ERROR-STATUS:ERROR THEN DO:
          GET-KEY-VALUE SECTION wSection KEY wKey VALUE wValue.
          UNLOAD wLokalIniFil NO-ERROR.
       END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IconFil dTables  _DB-REQUIRED
PROCEDURE IconFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wIconFil AS CHAR NO-UNDO.

  GET-KEY-VALUE SECTION "SYSPARA" KEY "IconFil" VALUE wIconFil.
  IF wIconFil = ? THEN
    DO:
      {syspara.i 1 1 3 wIconFil}
    END.

  IF SEARCH(wIconFil) <> ? THEN
    ASSIGN
      wIconFil = SEARCH(wIconFil).
  ELSE
    ASSIGN wIconFil = "icon\sk-ico.ico".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IniFil dTables  _DB-REQUIRED
PROCEDURE IniFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wIniFil AS CHAR NO-UNDO.

  wIniFil = "lokal.ini".

/*   GET-KEY-VALUE SECTION "SYSPARA" KEY "IniFil" VALUE wIniFil. */
/*   IF wIniFil = ? then                                         */
/*     DO:                                                       */
/*       {syspara.i 1 1 5 wIniFil}                               */
/*     END.                                                      */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitVariabler dTables 
PROCEDURE InitVariabler :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {syspara.i 1 11 1 cStatusTekster}
  IF cStatusTekster = "" THEN
     cStatusTekster = "Ventet,1,Ankommet,2,Ekstra,3,Innlesning avbrutt,8,Ikke koblet,9".
  {syspara.i 1 11 2 cBehStatusTekster}
  IF cBehStatusTekster = "" THEN
     cBehStatusTekster = "Ikke oppdatert,1,Delhvis,2,Oppdatert,3,Slettet,9".
  {syspara.i 1 11 4 cGraderingFilLogg}
  IF cGraderingFilLogg = "" THEN
     cGraderingFilLogg = "Melding,0,Feil,1,Alvorlig feil,2,Graverende feil,3,* 4 *,4,Oppdaterer,5,Start logging,9,Slutt logging,8".
  {syspara.i 1 11 5 cFiltypeTekst}
  IF cFiltypeTekst = "" THEN
     cFiltypeTekst = "El-Journal,1,Kvittering,2,Utskriftskopi,3,Dagsoppgjør,4,Kassereroppgjør,5".
  {syspara.i 2 4 22 cVikt}
  IF cVikt = "" THEN
      cVikt = "0".
  {syspara.i 2 4 23 cTidskrift}
  IF cTidskrift = "0" THEN
      cTidskrift = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreParametre dTables  _DB-REQUIRED
PROCEDURE LagreParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wSection AS CHAR NO-UNDO.
  DEF INPUT PARAMETER wKey     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER wValue   AS CHAR NO-UNDO.

  IF wSection = ? OR wKey = ? OR wValue = ? THEN
    RETURN "AVBRYT".

  SaveSettings:
  DO:
    IF wMappeLokalInifil <> ? THEN DO:
       LOAD wLokalIniFil DIR wMappeLokalInifil BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN DO:
          LOAD wLokalIniFil DIR wMappeLokalInifil NEW BASE-KEY "INI" NO-ERROR.
          IF ERROR-STATUS:ERROR THEN LEAVE SaveSettings.
       END.
           
       USE wLokalIniFil NO-ERROR.
       IF NOT ERROR-STATUS:ERROR THEN DO:
          PUT-KEY-VALUE SECTION wSection KEY wKey VALUE wValue  NO-ERROR.
          UNLOAD wLokalIniFil NO-ERROR.
       END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadIconfil dTables  _DB-REQUIRED
PROCEDURE LoadIconfil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wVindu   AS HANDLE NO-UNDO.
  DEF       VAR       wIconFil AS CHAR   NO-UNDO.

  /* Henter navn p† iconfil. */
  RUN IconFil IN THIS-PROCEDURE (OUTPUT wIconFil).

  /* Setter inn iconfil. */
  IF wIconFil <> "" THEN
    DO:
      IF wVindu:LOAD-ICON(wIconFil) THEN.
    END.

  /*
  ASSIGN /* THIS-PROCEDURE:PRIVATE-DATA = PROPATH */
         wVindu:NAME = "{&WindowName}".
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mappe dTables  _DB-REQUIRED
PROCEDURE Mappe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wMappe AS CHAR NO-UNDO.

  ASSIGN
      wMappe = RIGHT-TRIM(SESSION:TEMP-DIR,"\")
      .

/*   GET-KEY-VALUE SECTION "SYSPARA" KEY "Mappe" VALUE wMappe. */
/*   IF wMappe = ? then                                        */
/*     DO:                                                     */
/*       {syspara.i 1 1 6 wMappe}                              */
/*     END.                                                    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyFilLogg dTables  _DB-REQUIRED
PROCEDURE NyFilLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  Syntaks:  Sånn skal det bli
    publish "SystemLogg" (<LogType>, <LogId>, <Level>, <Tekst>).

------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER plFilId LIKE Filer.FilId NO-UNDO.
  DEF INPUT PARAMETER pcTekst AS   CHAR        NO-UNDO.

  DEF VAR piLinjeNr   AS INT NO-UNDO.
  DEF VAR piGradering AS INT NO-UNDO.

  DEF BUFFER bFilLogg  FOR FilLogg.
  DEF BUFFER b2FilLogg FOR FilLogg.

  ASSIGN
      piGradering = 0
      .
  IF NUM-ENTRIES(pcTekst,CHR(1)) >= 2 THEN
    ASSIGN
      piGradering = INT(ENTRY(2,pcTekst,CHR(1)))
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      piGradering = 0.

  FIND LAST b2FilLogg NO-LOCK WHERE
      b2FilLogg.FilId = plFilId NO-ERROR.
  IF AVAILABLE b2FilLogg THEN
      piLinjeNr = b2FilLogg.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.
  DO FOR bFilLogg TRANSACTION ON ERROR UNDO, RETRY:
      /* Skulle linjen finnes fra før, øker vi på litt. */
      IF RETRY THEN piLinjeNr = piLinjeNr + 1.
      CREATE bFilLogg.
      ASSIGN
          bFilLogg.FilId     = plFilId
          bFilLogg.LinjeNr   = piLinjeNr
          bFilLogg.Tekst     = ENTRY(1,pcTekst,CHR(1))
          bFilLogg.Gradering = piGradering
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
          IF AVAILABLE bFilLogg THEN
              DELETE bFillogg.
      END.
      IF AVAILABLE bFilLogg THEN
          RELEASE bFilLogg.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenExcelDocument dTables  _DB-REQUIRED
PROCEDURE OpenExcelDocument :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipFilnavn AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ipKatalog AS CHAR NO-UNDO.

  DEF VAR hInstance   AS INT NO-UNDO.


  RUN ShellExecute{&A} IN hpApi(0,
                                "open",
                                "Excel.exe",
                                ipFilnavn,
                                ipKatalog,
                                1,
                                OUTPUT hInstance).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenWeb dTables  _DB-REQUIRED
PROCEDURE OpenWeb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER document AS CHAR.

  DEF VAR executable AS CHAR.
  DEF VAR hInstance AS INTEGER.
  
  /* find the associated executable in registry */  
  Executable = FILL("x", 255). /* =allocate memory */
  RUN FindExecutable{&A} IN hpApi (document,
                                   "",
                                   INPUT-OUTPUT Executable,
                                   OUTPUT hInstance).

  /* if not found, show the OpenAs dialog from the Explorer */
  IF hInstance>=0 AND hInstance<=32 AND hInstance <> 2 THEN 
     RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "rundll32.exe",
                                  "shell32.dll,OpenAs_RunDLL " + document,
                                  "",
                                  1,
                                  OUTPUT hInstance).

  /* now open the document. If the user canceled the OpenAs dialog,
     this ShellExecute call will silently fail */
  RUN ShellExecute{&A} IN hpApi  (0,
                                  "open",
                                  document,
                                  "",
                                  "",
                                  1,
                                  OUTPUT hInstance).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadFromLokalIni dTables  _DB-REQUIRED
PROCEDURE ReadFromLokalIni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER wKey     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER wValue   AS CHAR NO-UNDO.

DEF VAR wSection AS CHAR NO-UNDO.
DEF VAR wMappe   AS CHAR NO-UNDO.
DEF VAR wIniFil  AS CHAR NO-UNDO.

ASSIGN
    wSection = "FILTERPARAMETRE".

/* Henter mappe og filnavn på den lokale inifilen */
RUN Mappe  (OUTPUT wMappe).
RUN IniFil (OUTPUT wIniFil).

/* Lagrer parametre */
IF wMappe <> ? THEN 
  DO:
    /* Laster den lokale ini filen. */
    LOAD wIniFil DIR wMappe BASE-KEY "INI" NO-ERROR.
    /* Finnes den ikke, opprette den. */
    IF ERROR-STATUS:ERROR THEN DO:
       LOAD wIniFil DIR wMappe NEW BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN "AVBRYT".
    END.           
    /* Stiller om til lokal ini-fil. */
    USE wIniFil NO-ERROR.
    /* Lagrer parameterverdien */
    IF NOT ERROR-STATUS:ERROR THEN
       GET-KEY-VALUE SECTION wSection KEY wKey VALUE wValue.
    /* Stiller tilbake til oppstarts ini fil. */
    UNLOAD wIniFil NO-ERROR.
  END.

/* Stripper ukjent verdi. */
ASSIGN
    wValue = IF wValue = ? THEN "" ELSE wValue.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RpbSkoDb dTables  _DB-REQUIRED
PROCEDURE RpbSkoDb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wDbListe AS CHAR NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.

  /* SkoTex databasen */
  GET-KEY-VALUE SECTION "SYSPARA" KEY "SkoDb" VALUE wTekst.
  IF wTekst = ? THEN
    DO:
      {syspara.i 1 1 10 wtekst}
    END.
  ASSIGN wDbListe = wTekst.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RpbWrDb dTables  _DB-REQUIRED
PROCEDURE RpbWrDb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wDbListe AS CHAR NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.

  /* RapportDatabasen */
  GET-KEY-VALUE SECTION "SYSPARA" KEY "WrDb" VALUE wTekst.
  IF wTekst = ? THEN
    DO:
      {syspara.i 1 1 11 wtekst}
    END.
  ASSIGN wDbListe = wTekst.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToLokalIni dTables  _DB-REQUIRED
PROCEDURE SaveToLokalIni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wKey     AS CHAR NO-UNDO.
DEF INPUT PARAMETER wValue   AS CHAR NO-UNDO.

DEF VAR wSection AS CHAR NO-UNDO.
DEF VAR wMappe   AS CHAR NO-UNDO.
DEF VAR wIniFil  AS CHAR NO-UNDO.

ASSIGN
    wSection = "FILTERPARAMETRE".

/* Henter mappe og filnavn på den lokale inifilen */
RUN Mappe  (OUTPUT wMappe).
RUN IniFil (OUTPUT wIniFil).

/* Lagrer parametre */
IF wMappe <> ? THEN 
  DO:
    /* Laster den lokale ini filen. */
    LOAD wIniFil DIR wMappe BASE-KEY "INI" NO-ERROR.
    /* Finnes den ikke, opprette den. */
    IF ERROR-STATUS:ERROR THEN DO:
       LOAD wIniFil DIR wMappe NEW BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN "AVBRYT".
    END.           
    /* Stiller om til lokal ini-fil. */
    USE wIniFil NO-ERROR.
    /* Lagrer parameterverdien */
    IF NOT ERROR-STATUS:ERROR THEN
       PUT-KEY-VALUE SECTION wSection KEY wKey VALUE wValue NO-ERROR.
    /* Stiller tilbake til oppstarts ini fil. */
    UNLOAD wIniFil NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLng dTables  _DB-REQUIRED
PROCEDURE SetLng :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcCurrLng   AS CHAR NO-UNDO.

  ASSIGN
      cCurrLng   = pcCurrLng
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetLngHandle dTables  _DB-REQUIRED
PROCEDURE SetLngHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER phLngHandle AS HANDLE.

  ASSIGN
      hLngHandle = phLngHandle
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkLapTop dTables  _DB-REQUIRED
PROCEDURE SjekkLapTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wSvar AS LOG NO-UNDO.

  DEF VAR wTekst AS CHAR NO-UNDO.


  GET-KEY-VALUE SECTION "SYSPARA" KEY "LapTopNr" VALUE wTekst.
  IF wTekst = ? THEN
    wSvar = FALSE.
  ELSE
    wSvar = TRUE.

  /* Sjekk mot LapTop tabellen.                             */
  /* Nummeret skal peke p† en gyldig post i denne tabellen. */
  IF wSvar = TRUE THEN
    DO:
      IF CAN-FIND(LapTop WHERE
                  LapTop.LapTopNr = INT(wTekst)) THEN
        wSvar = TRUE.
      ELSE
        wSvar = ?.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartNotePad dTables  _DB-REQUIRED
PROCEDURE StartNotePad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wFilNavn AS CHAR NO-UNDO.

  IF SEARCH(wFilNavn) = ? THEN
    DO:
      MESSAGE "Filen som skal åpnes finnes ikke" wFilNavn VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
  ELSE
      RUN ShellExecute{&A} IN hpApi(0,
                              "open",
                              "notepad.exe",
                              wFilNavn,
                              "",
                              1,
                              OUTPUT hInstance).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SystemNavn dTables  _DB-REQUIRED
PROCEDURE SystemNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER wSystemNavn AS CHAR NO-UNDO.

  ASSIGN
    wSystemNavn = "PRS".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EANprefixKonv dTables  _DB-REQUIRED
FUNCTION EANprefixKonv RETURNS CHARACTER
  ( INPUT cEANkode AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RUN bibl_chkean.p (INPUT-OUTPUT cEANkode).
  
  RETURN cEANkode.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EkspanderUPC dTables  _DB-REQUIRED
FUNCTION EkspanderUPC RETURNS CHARACTER
  ( INPUT cUPCtoExpand AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Expanderar en UPC-E kod till UPC-A
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cNewUpc AS CHARACTER  NO-UNDO.
  CASE SUBSTR(cUPCtoExpand,6,1):
      WHEN "0" OR WHEN "1" OR WHEN "2" THEN 
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,2) + 
                                  SUBSTR(cUPCtoExpand,6,1) +
                           "0000" + SUBSTR(cUPCtoExpand,3,3).
      WHEN "3" THEN           
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,3) +
                           "00000" + SUBSTR(cUPCtoExpand,4,2).
      WHEN "4" THEN 
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,4) +
                           "00000" + SUBSTR(cUPCtoExpand,5,1).
      OTHERWISE 
          ASSIGN cNewUpc = "00" + SUBSTR(cUPCtoExpand,1,5) +
                           "0000" + SUBSTR(cUPCtoExpand,6,1).
  END CASE.
  RETURN cNewUpc.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fixChkEAN dTables  _DB-REQUIRED
FUNCTION fixChkEAN RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  Räknar ut checksiffra för ean EAN-kod - parameter utan chksiffra
              i.e 12 lång
      Notes:  
  ------------------------------------------------------------------------------*/
      /* Input er 12 lang, så da legger vi på en 0. */
      cKode = cKode + '0'.
      RUN bibl_chkean.p (INPUT-OUTPUT cKode).
      RETURN cKode.

      /*
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.
      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
      */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

