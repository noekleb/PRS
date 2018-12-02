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
DEFINE VARIABLE cOrdreStatList AS CHARACTER INIT "," NO-UNDO.

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
&Scoped-define INTERNAL-TABLES Ordre

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BrukerID EDato EkstId ETid LapTop LevAdresse1 LevAdresse2 LevKontakt~
 LevMerknad LevNr LevPostBoks LevPostNr LevTelefon Merknad Notat OrdreNr~
 OrdreStatus RegistrertAv RegistrertDato RegistrertTid SendtDato BekreftetAv~
 BekreftetDato BekreftetOrdre fraERP HkOrdre LeveringsDato VareBehNr CL~
 Hasteordre sendtButikkDato sendtButikkFlagg sendtButikkTid ULevNr~
 OrdreMottaker Opphav
&Scoped-define ENABLED-FIELDS-IN-Ordre BrukerID EDato EkstId ETid LapTop ~
LevAdresse1 LevAdresse2 LevKontakt LevMerknad LevNr LevPostBoks LevPostNr ~
LevTelefon Merknad Notat OrdreNr OrdreStatus RegistrertAv RegistrertDato ~
RegistrertTid SendtDato BekreftetAv BekreftetDato BekreftetOrdre fraERP ~
HkOrdre LeveringsDato VareBehNr CL Hasteordre sendtButikkDato ~
sendtButikkFlagg sendtButikkTid ULevNr OrdreMottaker Opphav 
&Scoped-Define DATA-FIELDS  BrukerID fuLevPostSted EDato EkstId ETid LapTop LevAdresse1 fStatusTxt~
 LevAdresse2 LevKontakt LevMerknad LevNr LevPostBoks LevPostNr LevTelefon~
 Merknad Notat OrdreNr OrdreStatus RegistrertAv RegistrertDato fLevNamn~
 RegistrertTid SendtDato BekreftetAv BekreftetDato BekreftetOrdre fraERP~
 HkOrdre LeveringsDato VareBehNr CL Hasteordre sendtButikkDato~
 sendtButikkFlagg sendtButikkTid ULevNr OrdreMottaker Opphav
&Scoped-define DATA-FIELDS-IN-Ordre BrukerID EDato EkstId ETid LapTop ~
LevAdresse1 LevAdresse2 LevKontakt LevMerknad LevNr LevPostBoks LevPostNr ~
LevTelefon Merknad Notat OrdreNr OrdreStatus RegistrertAv RegistrertDato ~
RegistrertTid SendtDato BekreftetAv BekreftetDato BekreftetOrdre fraERP ~
HkOrdre LeveringsDato VareBehNr CL Hasteordre sendtButikkDato ~
sendtButikkFlagg sendtButikkTid ULevNr OrdreMottaker Opphav 
&Scoped-Define MANDATORY-FIELDS  LevNr OrdreNr OrdreStatus
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dordre.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Ordre NO-LOCK ~
    BY Ordre.OrdreNr DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Ordre NO-LOCK ~
    BY Ordre.OrdreNr DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Ordre
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Ordre


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKobledeBest dTables  _DB-REQUIRED
FUNCTION getKobledeBest RETURNS CHARACTER
  ( INPUT iLevNr AS INTEGER, INPUT iOrdreNr AS INTEGER )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLevNavn2 dTables  _DB-REQUIRED
FUNCTION getLevNavn2 RETURNS CHARACTER
  ( INPUT iLevNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOrdreStatusTxt dTables  _DB-REQUIRED
FUNCTION getOrdreStatusTxt RETURNS CHARACTER
  ( INPUT iOrdreStatus AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetPostSted dTables  _DB-REQUIRED
FUNCTION GetPostSted RETURNS CHARACTER
  ( pcPostNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Ordre SCROLLING.
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
         HEIGHT             = 1.86
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{soksdo.i}
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
     _TblList          = "skotex.Ordre"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.Ordre.OrdreNr|no"
     _FldNameList[1]   > skotex.Ordre.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[2]   > "_<CALC>"
"GetPostSted(RowObject.LevPostNr)" "fuLevPostSted" "Poststed" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[3]   > skotex.Ordre.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[4]   > skotex.Ordre.EkstId
"EkstId" "EkstId" "Ekstern ref" ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[5]   > skotex.Ordre.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[6]   > skotex.Ordre.LapTop
"LapTop" "LapTop" "LT" ? "logical" ? ? ? ? ? ? yes ? no 19.8 yes ""
     _FldNameList[7]   > skotex.Ordre.LevAdresse1
"LevAdresse1" "LevAdresse1" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[8]   > "_<CALC>"
"getOrdreStatusTxt(OrdreStatus)" "fStatusTxt" "Statustekst" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[9]   > skotex.Ordre.LevAdresse2
"LevAdresse2" "LevAdresse2" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[10]   > skotex.Ordre.LevKontakt
"LevKontakt" "LevKontakt" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[11]   > skotex.Ordre.LevMerknad
"LevMerknad" "LevMerknad" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[12]   > skotex.Ordre.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 10.8 yes ""
     _FldNameList[13]   > skotex.Ordre.LevPostBoks
"LevPostBoks" "LevPostBoks" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[14]   > skotex.Ordre.LevPostNr
"LevPostNr" "LevPostNr" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[15]   > skotex.Ordre.LevTelefon
"LevTelefon" "LevTelefon" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[16]   > skotex.Ordre.Merknad
"Merknad" "Merknad" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[17]   > skotex.Ordre.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[18]   > skotex.Ordre.OrdreNr
"OrdreNr" "OrdreNr" ? "zzzzzzz9" "integer" ? ? ? ? ? ? yes ? yes 8.2 yes ""
     _FldNameList[19]   > skotex.Ordre.OrdreStatus
"OrdreStatus" "OrdreStatus" ? ? "integer" ? ? ? ? ? ? yes ? yes 6 yes ""
     _FldNameList[20]   > skotex.Ordre.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[21]   > skotex.Ordre.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[22]   > "_<CALC>"
"getLevNavn()" "fLevNamn" "Levnavn" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[23]   > skotex.Ordre.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[24]   > skotex.Ordre.SendtDato
"SendtDato" "SendtDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[25]   > skotex.Ordre.BekreftetAv
"BekreftetAv" "BekreftetAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[26]   > skotex.Ordre.BekreftetDato
"BekreftetDato" "BekreftetDato" ? ? "date" ? ? ? ? ? ? yes ? no 13.4 yes ""
     _FldNameList[27]   > skotex.Ordre.BekreftetOrdre
"BekreftetOrdre" "BekreftetOrdre" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[28]   > skotex.Ordre.fraERP
"fraERP" "fraERP" ? ? "logical" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[29]   > skotex.Ordre.HkOrdre
"HkOrdre" "HkOrdre" ? ? "logical" ? ? ? ? ? ? yes ? no 8.4 yes ""
     _FldNameList[30]   > skotex.Ordre.LeveringsDato
"LeveringsDato" "LeveringsDato" ? ? "date" ? ? ? ? ? ? yes ? no 13.4 yes ""
     _FldNameList[31]   > skotex.Ordre.VareBehNr
"VareBehNr" "VareBehNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[32]   > skotex.Ordre.CL
"CL" "CL" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[33]   > skotex.Ordre.Hasteordre
"Hasteordre" "Hasteordre" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[34]   > skotex.Ordre.sendtButikkDato
"sendtButikkDato" "sendtButikkDato" ? ? "date" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[35]   > skotex.Ordre.sendtButikkFlagg
"sendtButikkFlagg" "sendtButikkFlagg" ? ? "logical" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[36]   > skotex.Ordre.sendtButikkTid
"sendtButikkTid" "sendtButikkTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[37]   > skotex.Ordre.ULevNr
"ULevNr" "ULevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes ""
     _FldNameList[38]   > skotex.Ordre.OrdreMottaker
"OrdreMottaker" "OrdreMottaker" ? ? "character" ? ? ? ? ? ? yes ? no 13.6 yes ""
     _FldNameList[39]   > skotex.Ordre.Opphav
"Opphav" "Opphav" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
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
         rowObject.fLevNamn = (getLevNavn())
         rowObject.fStatusTxt = (getOrdreStatusTxt(OrdreStatus))
         rowObject.fuLevPostSted = (GetPostSted(RowObject.LevPostNr))
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
  DEFINE VARIABLE cOrdreStat AS CHARACTER  NO-UNDO.
  DEFINE VAR      piLoop     AS INT        NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  cOrdreStatList = "".
  DO piLoop = 1 TO 6:
      {syspara.i 5 3 piLoop cOrdreStat "string" }
      ASSIGN 
          cOrdreStatList = cOrdreStatList 
            + (IF cOrdreStatList <> "" THEN "," ELSE "")
            + (IF cOrdreStat = "" THEN "*Ukjent*" ELSE cOrdreStat).
  END. 

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DYNAMIC-FUNCTION('setRebuildOnRepos':U, INPUT TRUE /* LOGICAL */).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KobleBest dTables  _DB-REQUIRED
PROCEDURE KobleBest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER Slett-Liste AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER Nye-Liste   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER iOrdreNr    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE         iCount      AS INTEGER    NO-UNDO.
    IF Slett-Liste <> "" THEN DO iCount = 1 TO NUM-ENTRIES(Slett-Liste):
        FIND BestHode  WHERE
             BestHode.BestNr = INT(ENTRY(iCount,Slett-Liste)) NO-LOCK NO-ERROR.
        IF AVAIL BestHode THEN
            RUN bytbeststatus.p (RECID(BestHode),"-",0).
    END.
    IF Nye-Liste <> "" THEN DO iCount = 1 TO NUM-ENTRIES(Nye-Liste):
        FIND BestHode  WHERE
             BestHode.BestNr = INT(ENTRY(iCount,Nye-Liste)) NO-LOCK NO-ERROR.
        IF AVAIL BestHode THEN
            RUN bytbeststatus.p (RECID(BestHode),"+",iOrdreNr).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LevNrValidate dTables  _DB-REQUIRED
PROCEDURE LevNrValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER iLevNr LIKE LevBas.LevNr    NO-UNDO.
   IF iLevNr = 0 THEN
       RETURN "Registrer leverandør".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Poststed dTables  _DB-REQUIRED
PROCEDURE Poststed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcPostNr AS CHAR NO-UNDO.

  FIND Post NO-LOCK WHERE
    Post.PostNr = pcPostNr NO-ERROR.
  IF AVAILABLE Post THEN
    RETURN Post.Beskrivelse.
  ELSE
    RETURN "AVBRYT".   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bOrdre FOR Ordre.
    DEFINE VARIABLE iOrdreNr AS INTEGER    NO-UNDO.

    /*
    FIND LAST bOrdre NO-LOCK NO-ERROR.
    ASSIGN iOrdreNr = IF AVAIL bOrdre THEN bOrdre.OrdreNr  + 1 ELSE 1.
    */
    RUN trg/genordrenr (OUTPUT iOrdreNr).

    FOR EACH RowObjUpd WHERE CAN-DO("A,C",RowObjUpd.RowMod):
        IF NOT CAN-FIND(LevBas WHERE LevBas.LevNr = RowObjUpd.LevNr) THEN
            RETURN "Registrert leverandør finnes ikke.".
        /* TN 10/3-02 Tildeles nå i trigger
        ASSIGN RowObjUpd.OrdreNr = iOrdreNr
               iOrdreNr          = iOrdreNr + 1.
        */
    END.
    FOR EACH RowObjUpd WHERE CAN-DO("D",RowObjUpd.RowMod):
        IF CAN-FIND(FIRST BestHode WHERE BestHode.OrdreNr = RowObjUpd.OrdreNr) THEN
            RETURN "Det finnes bestillinger koblet til denne orderen. Kann ikke slettes.".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSendtStatus dTables  _DB-REQUIRED
PROCEDURE SetSendtStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cBestillinger AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iCount        AS INTEGER    NO-UNDO.
    DO iCount = 1 TO NUM-ENTRIES(cBestillinger):
        FIND BestHode WHERE BestHode.BestNr = INT(ENTRY(iCount,cBestillinger)) NO-LOCK NO-ERROR.
        IF AVAIL BestHode THEN
            RUN bytbeststatus.p (RECID(BestHode),"+",BestHode.OrdreNr).
    END. /* SET_RADER_SOM_SENDT */ 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKobledeBest dTables  _DB-REQUIRED
FUNCTION getKobledeBest RETURNS CHARACTER
  ( INPUT iLevNr AS INTEGER, INPUT iOrdreNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE IO-Liste AS CHARACTER  NO-UNDO.
   for each BestHode no-lock where
      BestHode.LevNr     = iLevNr and
      BestHode.OrdreNr   = iOrdreNr and
      BestHode.BestStat >= 2 and
      BestHode.BestStat <= 3:
      
      assign IO-Liste = IO-Liste + 
                        (if IO-Liste = "" then "" else ",") +
                        string(BestHode.BestNr).      
   end.
   RETURN IO-Liste.   /* Function return value. */

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
  FIND LevBas OF Ordre NO-LOCK NO-ERROR.
  RETURN IF AVAIL LevBas THEN LevBas.levnamn ELSE "Feil lev!!".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLevNavn2 dTables  _DB-REQUIRED
FUNCTION getLevNavn2 RETURNS CHARACTER
  ( INPUT iLevNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND LevBas WHERE LevBas.LevNr = iLevNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL LevBas THEN LevBas.levNamn ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOrdreStatusTxt dTables  _DB-REQUIRED
FUNCTION getOrdreStatusTxt RETURNS CHARACTER
  ( INPUT iOrdreStatus AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF CAN-DO("1,2,3,4,5,6",STRING(iOrdreStatus)) THEN
            ENTRY(iOrdreStatus,cOrdreStatList) ELSE "*Ukjent*".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetPostSted dTables  _DB-REQUIRED
FUNCTION GetPostSted RETURNS CHARACTER
  ( pcPostNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND Post NO-LOCK WHERE
      Post.PostNr = pcPostNr NO-ERROR.
  IF AVAILABLE Post THEN
      RETURN Post.Beskrivelse.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

