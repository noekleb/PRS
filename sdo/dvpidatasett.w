&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
&Scoped-define INTERNAL-TABLES VPIDatasett

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AntallArtikler AntallKoblet Beskrivelse BrukerID DatasettStatus EDato~
 EkstVPILevNr ETid FilId ImportDato ImportKl OppdatertDato OppdatertTid~
 RegistrertAv RegistrertDato RegistrertTid
&Scoped-define ENABLED-FIELDS-IN-VPIDatasett AntallArtikler AntallKoblet ~
Beskrivelse BrukerID DatasettStatus EDato EkstVPILevNr ETid FilId ~
ImportDato ImportKl OppdatertDato OppdatertTid RegistrertAv RegistrertDato ~
RegistrertTid 
&Scoped-Define DATA-FIELDS  AntallArtikler fuVpiLevKortNavn fuDatasettStatus fuOppdatertTid fuImportTid~
 fuEndretInfo AntallKoblet Beskrivelse BrukerID DatasettStatus EDato~
 EkstVPILevNr ETid FilId ImportDato ImportKl OppdatertDato OppdatertTid~
 RegistrertAv RegistrertDato RegistrertTid
&Scoped-define DATA-FIELDS-IN-VPIDatasett AntallArtikler AntallKoblet ~
Beskrivelse BrukerID DatasettStatus EDato EkstVPILevNr ETid FilId ~
ImportDato ImportKl OppdatertDato OppdatertTid RegistrertAv RegistrertDato ~
RegistrertTid 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dvpidatasett.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH VPIDatasett NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH VPIDatasett NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main VPIDatasett
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main VPIDatasett


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DatasettStatus dTables  _DB-REQUIRED
FUNCTION DatasettStatus RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImportTid dTables  _DB-REQUIRED
FUNCTION ImportTid RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OppdatertKl dTables  _DB-REQUIRED
FUNCTION OppdatertKl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VPIDatasettStatus dTables  _DB-REQUIRED
FUNCTION VPIDatasettStatus RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VpiLevKortNavn dTables  _DB-REQUIRED
FUNCTION VpiLevKortNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      VPIDatasett SCROLLING.
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
     _TblList          = "Vpi.VPIDatasett"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Vpi.VPIDatasett.AntallArtikler
"AntallArtikler" "AntallArtikler" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[2]   > "_<CALC>"
"VpiLevKortNavn()" "fuVpiLevKortNavn" "VpiLevKortNavn" "x(12)" "character" ? ? ? ? ? ? no ? no 12 no
     _FldNameList[3]   > "_<CALC>"
"VPIDatasettStatus()" "fuDatasettStatus" "Status" "x(12)" "character" ? ? ? ? ? ? no ? no 12 no
     _FldNameList[4]   > "_<CALC>"
"OppdatertKl()" "fuOppdatertTid" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no
     _FldNameList[5]   > "_<CALC>"
"ImportTid()" "fuImportTid" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no
     _FldNameList[6]   > "_<CALC>"
"EndretInfo()" "fuEndretInfo" "EndretInfo" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no
     _FldNameList[7]   > Vpi.VPIDatasett.AntallKoblet
"AntallKoblet" "AntallKoblet" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[8]   > Vpi.VPIDatasett.Beskrivelse
"Beskrivelse" "Beskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes
     _FldNameList[9]   > Vpi.VPIDatasett.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[10]   > Vpi.VPIDatasett.DatasettStatus
"DatasettStatus" "DatasettStatus" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes
     _FldNameList[11]   > Vpi.VPIDatasett.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[12]   > Vpi.VPIDatasett.EkstVPILevNr
"EkstVPILevNr" "EkstVPILevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes
     _FldNameList[13]   > Vpi.VPIDatasett.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[14]   > Vpi.VPIDatasett.FilId
"FilId" "FilId" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes
     _FldNameList[15]   > Vpi.VPIDatasett.ImportDato
"ImportDato" "ImportDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes
     _FldNameList[16]   > Vpi.VPIDatasett.ImportKl
"ImportKl" "ImportKl" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[17]   > Vpi.VPIDatasett.OppdatertDato
"OppdatertDato" "OppdatertDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.4 yes
     _FldNameList[18]   > Vpi.VPIDatasett.OppdatertTid
"OppdatertTid" "OppdatertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes
     _FldNameList[19]   > Vpi.VPIDatasett.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes
     _FldNameList[20]   > Vpi.VPIDatasett.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes
     _FldNameList[21]   > Vpi.VPIDatasett.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes
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
         rowObject.fuDatasettStatus = (VPIDatasettStatus())
         rowObject.fuEndretInfo = (EndretInfo())
         rowObject.fuImportTid = (ImportTid())
         rowObject.fuOppdatertTid = (OppdatertKl())
         rowObject.fuVpiLevKortNavn = (VpiLevKortNavn())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVPIDatasettStatusListe dTables  _DB-REQUIRED
PROCEDURE GetVPIDatasettStatusListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piModus AS INT  NO-UNDO. /* 1 - Om [ALLE] skal være med */
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  DEF VAR pcWork AS CHAR NO-UNDO.

  /* Statusliste */
  {syspara.i 1 12 3 pcTekst}

  IF piModus = 1 THEN
  DO:
      {syspara.i 1 100 1 pcWork}
      ASSIGN
          pcTekst = pcWork + ",0," + 
                    pcTekst
          .
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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
     INPUT TRUE /* LOGICAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettOppdatert dTables  _DB-REQUIRED
PROCEDURE SettOppdatert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    /* Setter datasettet til statu 5 - Oppdatert. */
  RUN SettOppdatert IN h_dvpidatasett (INPUT  piEkstVPILevNr, INPUT 5).

------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT NO-UNDO.
  DEF INPUT PARAMETER piStatus       AS INT NO-UNDO.

  DEF VAR pcReturn-Value AS CHAR NO-UNDO.

  DO TRANSACTION:

      FIND VPIDatasett EXCLUSIVE-LOCK WHERE
          VPIDatasett.EkstVPILevNr = piEkstVPILevNr NO-ERROR.
      IF AVAILABLE VPIDatasett THEN
      DO:
          ASSIGN
              VPIDatasett.DatasettStatus = piStatus
              .
          RELEASE VPIDatasett.
      END.
      ELSE
          ASSIGN
              pcReturn-Value = "* Ukjent datasett - " + STRING(piEkstVPILEvNr) + "."
              .

  END. /* TRANSACTION */
              
  RETURN pcReturn-Value.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkHeader dTables  _DB-REQUIRED
PROCEDURE SjekkHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  SJEKKHEADER:
  FOR EACH EkstVPILev NO-LOCK WHERE
      EkstVPILev.Aktiv = TRUE TRANSACTION:

      IF NOT CAN-FIND(FIRST VPIDatasett WHERE
                      VPIDatasett.EkstVPILevNr = EkstVPILev.EkstVPILevNr) THEN
      DO:
          CREATE VPIDatasett.
          ASSIGN
              VPIDAtaSett.EkstVPILevNr   = EkstVPILev.EkstVPILEvNR
              VPIDataSett.DataSettStatus = 1
              VPIDataSett.Beskrivelse    = "Opprettet fra SDO"
              VPIDataSett.ImportDato     = TODAY
              VPIDataSett.ImportKl       = TIME
              .
      END.

  END. /* SJEKKHEADER */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DatasettStatus dTables  _DB-REQUIRED
FUNCTION DatasettStatus RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 12 3 pcTekst}

  IF NUM-ENTRIES(pcTekst) >= 10 THEN
  DO:
      CASE RowObject.DatasettStatus:
          WHEN 1 THEN pcTekst = ENTRY(1,pcTekst).
          WHEN 2 THEN pcTekst = ENTRY(3,pcTekst).
          WHEN 3 THEN pcTekst = ENTRY(5,pcTekst).
          WHEN 4 THEN pcTekst = ENTRY(7,pcTekst).
          WHEN 5 THEN pcTekst = ENTRY(9,pcTekst).
      END CASE.
  END.
  ELSE
      pcTekst = "* Ukjent *".

  RETURN pcTekst.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImportTid dTables  _DB-REQUIRED
FUNCTION ImportTid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.ImportKl,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OppdatertKl dTables  _DB-REQUIRED
FUNCTION OppdatertKl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.OppdatertTid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VPIDatasettStatus dTables  _DB-REQUIRED
FUNCTION VPIDatasettStatus RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 12 3 pcTekst}

  IF NUM-ENTRIES(pcTekst) >= 10 THEN
  DO:
      CASE RowObject.DataSettStatus:
          WHEN 1 THEN pcTekst = ENTRY(1,pcTekst).
          WHEN 2 THEN pcTekst = ENTRY(3,pcTekst).
          WHEN 3 THEN pcTekst = ENTRY(5,pcTekst).
          WHEN 4 THEN pcTekst = ENTRY(7,pcTekst).
          WHEN 5 THEN pcTekst = ENTRY(9,pcTekst).
      END CASE.
  END.
  ELSE
      pcTekst = "* Ukjent *".

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VpiLevKortNavn dTables  _DB-REQUIRED
FUNCTION VpiLevKortNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND EkstVpiLev NO-LOCK WHERE
      EkstVpiLev.EkstVpiLevNr = RowObject.EkstVpiLevNr NO-ERROR.

  IF AVAILABLE EkstVpiLev THEN
      RETURN EkstVpiLev.KortNavn.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

