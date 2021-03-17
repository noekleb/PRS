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

DEF VAR h_dfiler    AS HANDLE NO-UNDO.
DEF VAR h_Telleverk AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES Datasett

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  ButikkNr Dato FilId GruppeNr KasseNr SettNr SettStatus Tid DataSettId~
 FilType Behandlet AntallLinjer pfFlagg
&Scoped-define ENABLED-FIELDS-IN-Datasett ButikkNr Dato FilId GruppeNr ~
KasseNr SettNr SettStatus Tid DataSettId FilType Behandlet AntallLinjer ~
pfFlagg 
&Scoped-Define DATA-FIELDS  ButikkNr fuBehandletStatus Dato FilId fuKasseNavn GruppeNr KasseNr SettNr~
 fuButikkKortNavn SettStatus Tid DataSettId fuStatusTekst FilType Behandlet~
 AntallLinjer fuFilTypeTekst pfFlagg
&Scoped-define DATA-FIELDS-IN-Datasett ButikkNr Dato FilId GruppeNr KasseNr ~
SettNr SettStatus Tid DataSettId FilType Behandlet AntallLinjer pfFlagg 
&Scoped-Define MANDATORY-FIELDS  ButikkNr GruppeNr FilType
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "ddatasett.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Datasett NO-LOCK ~
    BY Datasett.DataSettId DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Datasett NO-LOCK ~
    BY Datasett.DataSettId DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Datasett
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Datasett


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD BehandletStatus dTables  _DB-REQUIRED
FUNCTION BehandletStatus RETURNS CHARACTER
  ( INPUT piBehandlet AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ButikkKortNavn dTables  _DB-REQUIRED
FUNCTION ButikkKortNavn RETURNS CHARACTER
  ( INPUT piButikkNr AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FilTypeTekst dTables  _DB-REQUIRED
FUNCTION FilTypeTekst RETURNS CHARACTER
  ( INPUT piFilType AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KasseNavn dTables  _DB-REQUIRED
FUNCTION KasseNavn RETURNS CHARACTER
  ( INPUT piButikkNr AS INT,
    INPUT piGruppeNr AS INT,
    INPUT piKasseNr  AS INT )  FORWARD.

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
      Datasett SCROLLING.
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
     _TblList          = "Data.Datasett"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Data.Datasett.DataSettId|no"
     _FldNameList[1]   > Data.Datasett.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[2]   > "_<CALC>"
"BehandletStatus(RowObject.Behandlet)" "fuBehandletStatus" "BehStatus" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no ?
     _FldNameList[3]   > Data.Datasett.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[4]   > Data.Datasett.FilId
"FilId" "FilId" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[5]   > "_<CALC>"
"KasseNavn(RowObject.ButikkNr, RowObject.GruppeNr, RowObject.KasseNr)" "fuKasseNavn" "Navn" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[6]   > Data.Datasett.GruppeNr
"GruppeNr" "GruppeNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 4.4 yes ""
     _FldNameList[7]   > Data.Datasett.KasseNr
"KasseNr" "KasseNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[8]   > Data.Datasett.SettNr
"SettNr" "SettNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[9]   > "_<CALC>"
"ButikkKortNavn(RowObject.ButikkNr)" "fuButikkKortNavn" "KortNavn" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[10]   > Data.Datasett.SettStatus
"SettStatus" "SettStatus" ? ? "integer" ? ? ? ? ? ? yes ? no 9.8 yes ""
     _FldNameList[11]   > Data.Datasett.Tid
"Tid" "Tid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[12]   > Data.Datasett.DataSettId
"DataSettId" "DataSettId" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.8 yes ""
     _FldNameList[13]   > "_<CALC>"
"StatusTekst(RowObject.SettStatus)" "fuStatusTekst" "StatusTekst" "x(12)" "character" ? ? ? ? ? ? no ? no 12 no ?
     _FldNameList[14]   > Data.Datasett.FilType
"FilType" "FilType" ? ? "integer" ? ? ? ? ? ? yes ? yes 6.8 yes ""
     _FldNameList[15]   > Data.Datasett.Behandlet
"Behandlet" "Behandlet" ? ? "integer" ? ? ? ? ? ? yes ? no 15.8 yes ""
     _FldNameList[16]   > Data.Datasett.AntallLinjer
"AntallLinjer" "AntallLinjer" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[17]   > "_<CALC>"
"FilTypeTekst(RowOBject.FilType)" "fuFilTypeTekst" "FilTypeNavn" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[18]   > Data.Datasett.pfFlagg
"pfFlagg" "pfFlagg" ? ? "integer" ? ? ? ? ? ? yes ? no 16.2 yes ""
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
         rowObject.fuBehandletStatus = (BehandletStatus(RowObject.Behandlet))
         rowObject.fuButikkKortNavn = (ButikkKortNavn(RowObject.ButikkNr))
         rowObject.fuFilTypeTekst = (FilTypeTekst(RowOBject.FilType))
         rowObject.fuKasseNavn = (KasseNavn(RowObject.ButikkNr, RowObject.GruppeNr, RowObject.KasseNr))
         rowObject.fuStatusTekst = (StatusTekst(RowObject.SettStatus))
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ddatasettDataAvailable dTables  _DB-REQUIRED
PROCEDURE ddatasettDataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).

  /* Code placed here will execute AFTER standard behavior.    */

  PUBLISH "ddatasettDataAvailable".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeOppdaterte dTables  _DB-REQUIRED
PROCEDURE GetAlleIkkeOppdaterte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  RUN GetAlleIkkeOppdaterte IN h_ddatasett (INPUT pbFilId, 
                                            INPUT 100, 
                                            OUTPUT pcValgteDatasett, 
                                            OUTPUT pbMore).

------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plFilId AS DEC  NO-UNDO.
  DEF INPUT  PARAMETER piAnt   AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore  AS LOG  NO-UNDO.

  ASSIGN
    pbMore = FALSE
    .

  LOOPEN:
  FOR EACH DataSett NO-LOCK WHERE
      DataSett.FilId  = plfilId AND
      DataSett.Behandlet <= 2:  
      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(DataSett.DataSettId).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOppdatert dTables  _DB-REQUIRED
PROCEDURE GetOppdatert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcDataSettId AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbOppdatert  AS LOG  NO-UNDO.

  FIND DataSett NO-LOCK WHERE
      DataSett.DataSettId = DEC(pcDataSettId) NO-ERROR.
  IF AVAILABLE DataSett THEN
  DO:
      IF DataSett.Behandlet <= 2 THEN
          pbOppdatert = FALSE.
      ELSE
          pbOppdatert = TRUE.

  END.
  /* Finnes ikke og skal ikke behandles av kallende rutine */
  ELSE
      pbOppdatert = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOverfort dTables  _DB-REQUIRED
PROCEDURE GetOverfort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcDataSettId AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbOverfort  AS LOG  NO-UNDO.

  FIND DataSett NO-LOCK WHERE
      DataSett.DataSettId = DEC(pcDataSettId) NO-ERROR.
  IF AVAILABLE DataSett THEN
  DO:
      IF DataSett.Behandlet = 5 THEN
          pbOverfort = TRUE.
      ELSE
          pbOverfort = FALSE.

  END.
  /* Finnes ikke og skal ikke behandles av kallende rutine */
  ELSE
      pbOverfort = TRUE.

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
     INPUT TRUE /* LOGICAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mottakskontroll dTables  _DB-REQUIRED
PROCEDURE Mottakskontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plDataSettId AS DEC NO-UNDO.

  FIND DataSett NO-LOCK WHERE
      DataSett.DataSettId = plDataSettId NO-ERROR.
  IF AVAILABLE DataSett AND DataSett.SettStatus > 1 THEN
  GURRE:
  DO TRANSACTION:
      FIND ApnSkjema EXCLUSIVE-LOCK WHERE
          ApnSkjema.ButikkNr = DataSett.ButikkNr AND
          ApnSkjema.Ar       = YEAR(DataSett.Dato) NO-ERROR.
      IF AVAILABLE ApnSkjema THEN
      DO:
          ENTRY(DataSett.Dato - Date(12,31,YEAR(DataSett.Dato) - 1),ApnSkjema.OpenClosed,",") = "4"
              .
      END.
  END. /* GURRE TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterDatasett dTables 
PROCEDURE OppdaterDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcDataSettIdListe AS  CHAR  NO-UNDO.
  DEF INPUT  PARAMETER plFilId           AS  DEC   NO-UNDO.
  DEF OUTPUT PARAMETER pbOk              AS  LOG   NO-UNDO.

  DEF VAR plDataSettId    AS  DEC     NO-UNDO.
  DEF VAR piLoop1         AS  INT     NO-UNDO.
  DEF VAR pcErrorListe    AS  CHAR    NO-UNDO.
  DEF VAR pcBehandling AS  CHAR    NO-UNDO.
  DEF VAR piAntLinjer     AS  INT     NO-UNDO.
  DEF VAR piStart         AS  INT     NO-UNDO.

  FIND Filer NO-LOCK WHERE
      Filer.FilId = plFilId NO-ERROR.

  OPPDATERDATASETT:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcDatasettIdListe) ON ERROR UNDO, LEAVE:
    ASSIGN
        plDatasettId = DEC(ENTRY(piLoop1,pcDataSettIdListe))
        piStart      = TIME
        .
    RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                   + " - Starter oppdatering av datasett (Datasett: " + 
                     STRING(plDataSettId,">>>>>>>>>>>>9") + ")." + CHR(1) + "9").

    FIND Datasett NO-LOCK WHERE
      Datasett.DataSettId = plDataSettId NO-ERROR.
    IF NOT AVAILABLE DataSett THEN
    DO:
      RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                     + " ** Anmodet om å oppdatere ukjent datasett (Datasett: " + 
                       STRING(plDataSettId,">>>>>>>>>>>>9") + ")." + CHR(1) + "2").
      ASSIGN
          pcErrorListe = 
            pcErrorListe +
            (IF pcErrorListe = ""
               THEN ""
               ELSE CHR(1)) + 
            STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                     + " ** Anmodet om å oppdatere ukjent datasett (Datasett: " + 
                       STRING(plDataSettId,">>>>>>>>>>>>9") + ")." + CHR(1) + "2"
          .
      NEXT OPPDATERDATASETT.
    END.

    /* Henter navn på oppdateringsrutine og starter denne */
    OPPDATER:
    DO:
        FIND Kasse NO-LOCK WHERE
            Kasse.ButikkNr = DataSett.ButikkNr AND
            Kasse.GruppeNr = DataSett.GruppeNr AND
            Kasse.KasseNr  = DataSett.KasseNr NO-ERROR.
        IF AVAILABLE Kasse THEN
            pcBehandling = (IF Filer.FilType = 1 
                              THEN Kasse.ElJournalBehandle
                            ELSE IF Filer.FilType = 2
                              THEN Kasse.KvitteringBehandle
                            ELSE IF Filer.FilType = 3 
                              THEN Kasse.UtskriftskopiBehandle
                            ELSE IF Filer.FilType = 4 
                              THEN Kasse.DagsOppgjBehandle
                            ELSE IF Filer.FilType = 5 
                              THEN Kasse.KassererOppgjBehandle
                            ELSE
                                "").
        ELSE DO:
            pcBehandling = "".
            RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                           " ** Datasettet er ikke koblet til kasse (Datasett: " + 
                           string(plDataSettId) + ")." + CHR(1) + "3").
            ASSIGN
                pcErrorListe = 
                  pcErrorListe +
                  (IF pcErrorListe = ""
                     THEN ""
                     ELSE "|") + 
                  STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                           + " ** Datasettet er ikke koblet til kasse (Datasett: " + 
                           string(plDataSettId) + ")." + CHR(1) + "2"
                .
        END.

        IF SEARCH(pcBehandling + ".r") = ? THEN
        DO:
          RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                          STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                         " ** Ukjent oppdateringsprogram: " + pcBehandling + ".r" + CHR(1) + "3").
          NEXT OPPDATERDATASETT.
        END.

        /* Kall til dummy skal ikke utføres. */
        IF pcBehandling = 'dummy' THEN
            NEXT OPPDATERDATASETT.

        RUN VALUE(pcBehandling + ".p") 
            (INPUT  plDataSettId,
             INPUT  h_Telleverk,
             INPUT  h_dfiler, /* Handel for logging */
             OUTPUT piAntLinjer
            ).
        IF RETURN-VALUE <> "" THEN
        DO:
            ASSIGN
                pcErrorListe = 
                  pcErrorListe +
                  (IF pcErrorListe = ""
                     THEN ""
                     ELSE "|") +  
                     RETURN-VALUE 
                .
        END.
        /* Tømmer feillisten */
        IF pcErrorListe <> "" THEN
        DO piLoop1 = 1 TO NUM-ENTRIES(pcErrorListe,"|"):
          RUN NyFilLogg IN h_dfiler (INPUT plFilId, ENTRY(piLoop1,pcErrorListe,"|")).
        END.
        ASSIGN
            pcErrorListe = ""
            .
    END. /* OPPDATER */

    ASSIGN
        pbOk = TRUE
        .
    /* Logger misslykket oppdatering */
    IF pbOk = FALSE THEN
    DO:
        RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                       + " ** Oppdatering av et eller flere av datasettene ble avbrutt." + CHR(1) + "2").
    END.
    /* Logger vellykket oppdatering. */
    ELSE  DO:
      RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Datasett oppdatert: " + 
                         string(plDatasettId) + 
                     " Tidsbruk: " + 
                     STRING(TIME - piStart,"HH:MM:SS") + 
                     " Antall poster: " +
                     STRING(piAntLinjer) + 
                     ".").
    END.

    /* Oppdaterer mottakskontrollstreng for butikken */
    RUN Mottakskontroll (plDatasettId).

    ASSIGN
        pbOk = FALSE
        .

  END. /* OPPDATERDATASETT */

  IF pcErrorListe <> "" THEN
  DO piLoop1 = 1 TO NUM-ENTRIES(pcErrorListe,"|"):
    RUN NyFilLogg IN h_dfiler (INPUT plFilId, ENTRY(piLoop1,pcErrorListe,"|")).
  END.

  /* Sender alltid OK tilbake */
  ASSIGN
      pbOk = TRUE
      .
  RETURN "Oppdatering av datasett ferdig.".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OverforDatasett dTables 
PROCEDURE OverforDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcDataSettIdListe AS  CHAR  NO-UNDO.
  DEF INPUT  PARAMETER plFilId           AS  DEC   NO-UNDO.
  DEF OUTPUT PARAMETER pbOk              AS  LOG   NO-UNDO.

  DEF VAR plDataSettId    AS  DEC     NO-UNDO.
  DEF VAR piLoop1         AS  INT     NO-UNDO.
  DEF VAR pcErrorListe    AS  CHAR    NO-UNDO.
  DEF VAR pcBehandling AS  CHAR    NO-UNDO.
  DEF VAR piAntLinjer     AS  INT     NO-UNDO.
  DEF VAR piStart         AS  INT     NO-UNDO.
  DEFINE VARIABLE pcAppSrv AS CHARACTER NO-UNDO.

  {syspara.i 200 1 8 pcAppSrv}

  /* Kjøres bongoppdatering via Appserver, skal ikke liste bygges her. */
  IF CAN-DO('2',pcAppSrv) THEN
  DO: 
    RUN NyFilLogg IN h_dfiler (INPUT plFilId, "ddatasett.w - OverforDatasett - Bongoppdatering kjører via AppServer (Syspara 200 1 8).").
    RETURN.
  END.

  FIND Filer NO-LOCK WHERE
      Filer.FilId = plFilId NO-ERROR.

  OPPDATERDATASETT:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcDatasettIdListe) ON ERROR UNDO, LEAVE:
    ASSIGN
        plDatasettId = DEC(ENTRY(piLoop1,pcDataSettIdListe))
        piStart      = TIME
        .
    RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                   + " - Starter overføring av datasett (Datasett: " + 
                     STRING(plDataSettId,">>>>>>>>>>>>9") + ")." + CHR(1) + "9").

    FIND Datasett NO-LOCK WHERE
      Datasett.DataSettId = plDataSettId NO-ERROR.
    IF NOT AVAILABLE DataSett THEN
    DO:
      RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                     + " ** Anmodet om å overføre ukjent datasett (Datasett: " + 
                       STRING(plDataSettId,">>>>>>>>>>>>9") + ")." + CHR(1) + "2").
      ASSIGN
          pcErrorListe = 
            pcErrorListe +
            (IF pcErrorListe = ""
               THEN ""
               ELSE CHR(1)) + 
            STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                     + " ** Anmodet om å overføre ukjent datasett (Datasett: " + 
                       STRING(plDataSettId,">>>>>>>>>>>>9") + ")." + CHR(1) + "2"
          .
      NEXT OPPDATERDATASETT.
    END.
    /* Vi ønsker bare å få med delhvis oppdaterte - 2 og oppdaterte - 3. */
    IF DataSett.Behandlet < 2 OR DataSett.Behandlet >= 5 THEN
    DO:
        NEXT OPPDATERDATASETT.
    END.

    /* Starter overføring av bonger. */
    RUN xoverforbong.p (plDataSettId, 
                        h_Telleverk, 
                        h_dfiler, 
                        OUTPUT piAntLinjer).
    IF RETURN-VALUE <> "" THEN
    DO:
        ASSIGN
            pcErrorListe = 
              pcErrorListe +
              (IF pcErrorListe = ""
                 THEN ""
                 ELSE "|") +  
                 RETURN-VALUE 
            .
    END.
    /* Tømmer feillisten */
    IF pcErrorListe <> "" THEN
    DO piLoop1 = 1 TO NUM-ENTRIES(pcErrorListe,"|"):
      RUN NyFilLogg IN h_dfiler (INPUT plFilId, ENTRY(piLoop1,pcErrorListe,"|")).
    END.
    ASSIGN
        pcErrorListe = ""
        .

    ASSIGN
        pbOk = TRUE
        .
    /* Logger misslykket oppdatering */
    IF pbOk = FALSE THEN
    DO:
        RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                       + " ** Overføring av et eller flere av datasettene ble avbrutt." + CHR(1) + "2").
    END.
    /* Logger vellykket overføring. */
    ELSE  DO:
      RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Datasett overført: " + 
                         string(plDatasettId) + 
                     " Tidsbruk: " + 
                     STRING(TIME - piStart,"HH:MM:SS") + 
                     " Antall poster: " +
                     STRING(piAntLinjer) + 
                     ".").
    END.
    ASSIGN
        pbOk = FALSE
        .

  END. /* OPPDATERDATASETT */

  IF pcErrorListe <> "" THEN
  DO piLoop1 = 1 TO NUM-ENTRIES(pcErrorListe,"|"):
    RUN NyFilLogg IN h_dfiler (INPUT plFilId, ENTRY(piLoop1,pcErrorListe,"|")).
  END.

  /* Sender alltid OK tilbake */
  ASSIGN
      pbOk = TRUE
      .
  RETURN "Overføring av datasett ferdig.".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDFiler dTables  _DB-REQUIRED
PROCEDURE SetDFiler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER h_Handle AS HANDLE NO-UNDO.

  ASSIGN
      h_dfiler = h_Handle
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetHandleTelleverk dTables  _DB-REQUIRED
PROCEDURE SetHandleTelleverk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER dh_Telleverk AS HANDLE NO-UNDO.

  ASSIGN
      h_Telleverk = dh_Telleverk
      .
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetMakulert dTables  _DB-REQUIRED
PROCEDURE SetMakulert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcDataSettId AS CHAR NO-UNDO.

  DO TRANSACTION:
      FIND DataSett EXCLUSIVE-LOCK WHERE
          DataSett.DataSettId = DEC(pcDataSettId) NO-ERROR.
      IF AVAILABLE DataSett THEN
          ASSIGN
          DataSett.Behandlet = 9
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettDatasett dTables  _DB-REQUIRED
PROCEDURE SlettDatasett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plDataSettId   AS  DEC     NO-UNDO.
  DEF OUTPUT PARAMETER pbOk           AS  LOG     NO-UNDO.

  DEF VAR plFilId        AS  DEC     NO-UNDO.

  SLETTDATASETT:
  DO TRANSACTION:
    FIND Datasett EXCLUSIVE-LOCK WHERE
      Datasett.DataSettId = plDataSettId NO-ERROR.
    IF NOT AVAILABLE DataSett THEN
      RETURN.

    /* Kun makulerte datasett kan slettes. */
    IF DataSett.Behandlet <> 9 THEN
        RETURN.

    ASSIGN
        plFilId = DataSett.FilId
        .

    /* Tar bort de tilhørende fillinjene. */
    FOR EACH FilLinjer OF DataSett EXCLUSIVE-LOCK:
        DELETE FilLinjer.
    END.
    /* Tar bort de tilhørende bongene */
    FOR EACH BongHode OF DataSett EXCLUSIVE-LOCK:
      FOR EACH BongLinje WHERE
          BongLinje.B_Id = BongHode.B_Id:
        DELETE BongLinje.
      END.
      DELETE BongHode.
    END.

    /* Tar bort datasettet */
    DELETE Datasett.

    ASSIGN
        pbOk = TRUE
        .
  END. /* TRANSACTION SLETTDATASETT */


  IF pbOk = FALSE THEN
  DO:
      RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                     + " ** Sletting av datasett avbrutt (Datasett: " + 
                       STRING(plDataSettId,">>>>>>>>>>>>9") + ").").
      RETURN "** Sletting av datasett avbrutt.".

  END.
  ELSE  DO:
    RUN NyFilLogg IN h_dfiler (INPUT plFilId, STRING(TODAY) + " " + 
                    STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                   " - Datasett slettet. (Datasett: " + 
                       STRING(plDataSettId,">>>>>>>>>>>>9") + ").").
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION BehandletStatus dTables  _DB-REQUIRED
FUNCTION BehandletStatus RETURNS CHARACTER
  ( INPUT piBehandlet AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RUN BehandletStatus IN h_dproclib (INPUT piBehandlet).

  RETURN RETURN-VALUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ButikkKortNavn dTables  _DB-REQUIRED
FUNCTION ButikkKortNavn RETURNS CHARACTER
  ( INPUT piButikkNr AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = piButikkNr NO-ERROR.
  IF AVAILABLE Butiker THEN
      RETURN Butiker.KortNavn.
  ELSE 
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FilTypeTekst dTables  _DB-REQUIRED
FUNCTION FilTypeTekst RETURNS CHARACTER
  ( INPUT piFilType AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RUN FilTypeTekst IN h_dproclib (INPUT piFilType).

  RETURN RETURN-VALUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KasseNavn dTables  _DB-REQUIRED
FUNCTION KasseNavn RETURNS CHARACTER
  ( INPUT piButikkNr AS INT,
    INPUT piGruppeNr AS INT,
    INPUT piKasseNr  AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  FIND Kasse NO-LOCK WHERE
      Kasse.ButikkNr = piButikkNr AND
      Kasse.GruppeNr = piGruppeNr AND
      Kasse.KasseNr  = piKasseNr NO-ERROR.
  IF AVAILABLE Kasse THEN
      RETURN Kasse.Navn.
  ELSE 
      RETURN "".   /* Function return value. */

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
    Notes:  "[Alle],0,Ventet,1,Ankommet,2,Ekstra,3,Ikke koblet,9"
------------------------------------------------------------------------------*/
  RUN FilMottakStatus IN h_dproclib (INPUT piStatus).

  RETURN RETURN-VALUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

