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
DEF VAR cBuntListe   AS CHAR NO-UNDO.
DEF VAR cOppdatertAv AS CHAR NO-UNDO.
{overforing.i
    &NEW    = "NEW"
    &SHARED = "SHARED"
}

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
&Scoped-define INTERNAL-TABLES OvBunt

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BrukerID BuntNr DatoOppdatert EDato ETid Merknad OppdatertAv RegistrertAv~
 RegistrertDato RegistrertTid TidOppdatert BatchNr BuntStatus opphav~
 Faktura_Id FakturertAv FakturertDato FakturertTid
&Scoped-define ENABLED-FIELDS-IN-OvBunt BrukerID BuntNr DatoOppdatert EDato ~
ETid Merknad OppdatertAv RegistrertAv RegistrertDato RegistrertTid ~
TidOppdatert BatchNr BuntStatus opphav Faktura_Id FakturertAv FakturertDato ~
FakturertTid 
&Scoped-Define DATA-FIELDS  BrukerID fuEKl BuntNr DatoOppdatert EDato fuKlOppdatert ETid Merknad~
 OppdatertAv fuKlRegistrert RegistrertAv RegistrertDato RegistrertTid~
 fuEndretInfo TidOppdatert BatchNr BuntStatus opphav Faktura_Id FakturertAv~
 FakturertDato FakturertTid fuBatchOppdatert fuFakturaNr
&Scoped-define DATA-FIELDS-IN-OvBunt BrukerID BuntNr DatoOppdatert EDato ~
ETid Merknad OppdatertAv RegistrertAv RegistrertDato RegistrertTid ~
TidOppdatert BatchNr BuntStatus opphav Faktura_Id FakturertAv FakturertDato ~
FakturertTid 
&Scoped-Define MANDATORY-FIELDS  Faktura_Id
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dovbunt.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH OvBunt NO-LOCK ~
    BY OvBunt.BuntNr DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH OvBunt NO-LOCK ~
    BY OvBunt.BuntNr DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main OvBunt
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main OvBunt


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuBatchOppdatert dTables  _DB-REQUIRED
FUNCTION fuBatchOppdatert RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuEKl dTables  _DB-REQUIRED
FUNCTION fuEKl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuEndretInfo dTables  _DB-REQUIRED
FUNCTION fuEndretInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuFakturaNr dTables  _DB-REQUIRED
FUNCTION fuFakturaNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuKlOppdatert dTables  _DB-REQUIRED
FUNCTION fuKlOppdatert RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuKlRegistrert dTables  _DB-REQUIRED
FUNCTION fuKlRegistrert RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      OvBunt SCROLLING.
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
     _TblList          = "skotex.OvBunt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.OvBunt.BuntNr|no"
     _FldNameList[1]   > skotex.OvBunt.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[2]   > "_<CALC>"
"fuEKl()" "fuEKl" "Endret kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[3]   > skotex.OvBunt.BuntNr
"BuntNr" "BuntNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[4]   > skotex.OvBunt.DatoOppdatert
"DatoOppdatert" "DatoOppdatert" ? ? "date" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[5]   > skotex.OvBunt.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[6]   > "_<CALC>"
"fuKlOppdatert()" "fuKlOppdatert" "Oppdatert kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[7]   > skotex.OvBunt.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[8]   > skotex.OvBunt.Merknad
"Merknad" "Merknad" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[9]   > skotex.OvBunt.OppdatertAv
"OppdatertAv" "OppdatertAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[10]   > "_<CALC>"
"fuKlRegistrert()" "fuKlRegistrert" "Reg.kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[11]   > skotex.OvBunt.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[12]   > skotex.OvBunt.RegistrertDato
"RegistrertDato" "RegistrertDato" "Reg.Dato" ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[13]   > skotex.OvBunt.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[14]   > "_<CALC>"
"fuEndretInfo()" "fuEndretInfo" ? "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[15]   > skotex.OvBunt.TidOppdatert
"TidOppdatert" "TidOppdatert" ? ? "integer" ? ? ? ? ? ? yes ? no 19 yes ""
     _FldNameList[16]   > skotex.OvBunt.BatchNr
"BatchNr" "BatchNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[17]   > skotex.OvBunt.BuntStatus
"BuntStatus" "BuntStatus" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[18]   > skotex.OvBunt.opphav
"opphav" "opphav" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[19]   > skotex.OvBunt.Faktura_Id
"Faktura_Id" "Faktura_Id" ? ? "decimal" ? ? ? ? ? ? yes ? yes 15.6 yes ""
     _FldNameList[20]   > skotex.OvBunt.FakturertAv
"FakturertAv" "FakturertAv" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[21]   > skotex.OvBunt.FakturertDato
"FakturertDato" "FakturertDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[22]   > skotex.OvBunt.FakturertTid
"FakturertTid" "FakturertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[23]   > "_<CALC>"
"fuBatchOppdatert()" "fuBatchOppdatert" "Batch oppdatert" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no ?
     _FldNameList[24]   > "_<CALC>"
"fuFakturaNr()" "fuFakturaNr" "Fakturanr" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AssignOppdatertAv dTables  _DB-REQUIRED
PROCEDURE AssignOppdatertAv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcOppdatertAv AS CHARACTER  NO-UNDO.
    ASSIGN cOppdatertAv = ipcOppdatertAv.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE BeginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Tar bort alle koblede overforingslinjer i OvBuffer. */
  FOR EACH RowObjUpd WHERE
      RowObjUpd.RowMod = "D":
    FOR EACH OvBuffer EXCLUSIVE-LOCK WHERE
        OvBuffer.BuntNr = RowObjUpd.BuntNr:
      DELETE OvBuffer.
    END.
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
         rowObject.fuBatchOppdatert = (fuBatchOppdatert())
         rowObject.fuEKl = (fuEKl())
         rowObject.fuEndretInfo = (fuEndretInfo())
         rowObject.fuFakturaNr = (fuFakturaNr())
         rowObject.fuKlOppdatert = (fuKlOppdatert())
         rowObject.fuKlRegistrert = (fuKlRegistrert())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndTransactionValidate dTables  _DB-REQUIRED
PROCEDURE EndTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.

  IF cBuntListe <> "" THEN
  DO piLoop = 1 TO NUM-ENTRIES(cBuntListe):
    RUN OppdaterTransLogg (INPUT int(ENTRY(piLoop,cBuntListe))).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getButikker dTables  _DB-REQUIRED
PROCEDURE getButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER cButFraListe AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER cButTilListe AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  cButTilListeTmp AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  cButFraTmp   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  cButTilTmp   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
   DEFINE        VARIABLE  iCount2      AS INTEGER    NO-UNDO.
   /* Bygger en lista med frabutiker */
   FOR EACH ovBuffer WHERE ovBuffer.BuntNr = RowObject.BuntNr NO-LOCK:
       IF NOT CAN-DO(cButFraTmp,STRING(OvBuffer.ButikkNrFra)) THEN
           ASSIGN cButFraTmp = cButFraTmp + 
                              (IF cButFraTmp <> "" THEN "," ELSE "") + 
                               STRING(OvBuffer.ButikkNrFra).
   END.
   DO iCount = 1 TO NUM-ENTRIES(cButFraTmp):
       FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCount,cButFraTmp)) NO-LOCK NO-ERROR.
       IF AVAIL Butiker THEN DO:
           ASSIGN cButFraListe = cButFraListe + 
                                 (IF cButFraListe <> "" THEN "," ELSE "") + 
                                 Butiker.butnamn + "," + STRING(Butiker.Butik)
                  cButTilTmp = "".
           FOR EACH ovBuffer WHERE ovBuffer.BuntNr = RowObject.BuntNr AND 
                                   OvBuffer.ButikkNrFra = INT(ENTRY(iCount,cButFraTmp)) NO-LOCK:
               IF NOT CAN-DO(cButTilTmp,STRING(OvBuffer.ButikkNrTil)) THEN
                   ASSIGN cButTilTmp = cButTilTmp + 
                                       (IF cButTilTmp <> "" THEN "," ELSE "") +
                                       STRING(OvBuffer.ButikkNrTil).
           END.
           ASSIGN cButTilListeTmp = "".
           IF cButTilTmp <> "" THEN DO iCount2 = 1 TO NUM-ENTRIES(cButTilTmp):
               FIND Butiker WHERE Butiker.Butik = INT(ENTRY(iCount2,cButTilTmp)) NO-LOCK NO-ERROR.
               IF AVAIL Butiker THEN
                   ASSIGN cButTilListeTmp = cButTilListeTmp + 
                                         (IF cButTilListeTmp <> "" THEN "," ELSE "") + 
                                         Butiker.butnamn + "," + STRING(Butiker.Butik).
           END.
           IF cButTilListeTmp <> "" THEN DO:
               ASSIGN cButTilListe = cButTilListe + (IF cButTilListe <> "" THEN ";" ELSE "") + cButTilListeTmp.
           END.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEtiketter dTables  _DB-REQUIRED
PROCEDURE getEtiketter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iButFra         AS INTEGER    NO-UNDO.
   DEFINE INPUT  PARAMETER iButTil         AS INTEGER    NO-UNDO.
   DEFINE OUTPUT PARAMETER cStrekKodeListe AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER cAntallListe    AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  ctmpStrekkode   AS CHARACTER  NO-UNDO.
   DEFINE        VARIABLE  cTmpStorl       AS CHARACTER  NO-UNDO.
   FOR EACH ovBuffer WHERE ovBuffer.BuntNr = RowObject.BuntNr AND 
                           OvBuffer.ButikkNrFra = iButFra     AND
                           OvBuffer.ButikkNrTil = iButTil     NO-LOCK:
  
       ASSIGN cTmpStorl = TRIM(TRIM(ovbuffer.tilstorl))
              cTmpStorl = IF (LENGTH(cTmpStorl) = 1 OR length(cTmpStorl) = 3) then " " + cTmpStorl
                         else cTmpStorl.

       FIND strkonv WHERE strkonv.storl = cTmpStorl NO-LOCK NO-ERROR.
       IF AVAIL strkonv THEN DO:
           ASSIGN ctmpStrekKode = "".
           FOR EACH StrekKode WHERE StrekKode.ArtikkelNr = ovBuffer.ArtikkelNr AND
                                    StrekKode.StrKode    = strKonv.StrKode NO-LOCK:
               ASSIGN ctmpStrekKode = IF ctmpStrekKode = "" THEN StrekKode.Kode ELSE
                   IF ctmpStrekKode BEGINS "02" THEN StrekKode.Kode ELSE ctmpStrekKode.
           END.
           IF ctmpStrekKode <> "" THEN
               ASSIGN cStrekKodeListe = cStrekKodeListe + (IF cStrekKodeListe <> "" THEN "," ELSE "") + ctmpStrekKode
                      cAntallListe    = cAntallListe    + (IF cAntallListe <> "" THEN "," ELSE "") + STRING(ovBuffer.antall).
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterTransLogg dTables  _DB-REQUIRED
PROCEDURE OppdaterTransLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piBuntNr AS INT NO-UNDO.
  
  DEF VAR piBatchNr AS INT    NO-UNDO.
  DEF VAR pcStatus  AS CHAR   NO-UNDO.
  DEF VAR ihbuffer  AS HANDLE NO-UNDO.
  DEF VAR cReturn   AS CHAR   NO-UNDO.
  DEF VAR bOk       AS LOG    NO-UNDO.

  /* Oppdaterer TransLogg med de registrerte transaksjonene */
  DO:
    FIND OvBunt NO-LOCK WHERE
        OvBunt.BuntNr = piBuntNr NO-ERROR.
    IF NOT AVAILABLE OvBunt THEN
        RETURN "Ingen bunt tilgjengelig (OppdaterTranslogg)".

    /* Bygger temp-tabell for de poster som skal overføres til TransLogg */
    FOR EACH OvBuffer OF OvBunt NO-LOCK
        BREAK BY Ovbuffer.BuntNr
              BY OvBuffer.ButikkNrFra
              BY Ovbuffer.ButikkNrTil:
        
        CREATE tmpOverfor.
        ASSIGN
            tmpOverfor.ArtikkelNr = OvBuffer.ArtikkelNr
            tmpOverfor.Vg         = OvBuffer.Vg
            tmpOverfor.LopNr      = OvBuffer.LopNr
            tmpOverfor.FraBut     = OvBuffer.ButikkNrFra
            tmpOverfor.TilBut     = OvBuffer.ButikkNrTil
            tmpOverfor.FraStorl   = OvBuffer.Storl
            tmpOverfor.TilStorl   = OvBuffer.TilStorl
            tmpOverfor.Antall     = OvBuffer.Antall
            tmpOverfor.BuntNr     = OvBuffer.BuntNr
            .

        /* Oppretter bong. pr. overføring */
        IF LAST-OF(Ovbuffer.ButikkNrTil) THEN
        DO:
            ihBuffer = BUFFER tmpOverfor:HANDLE.
            RUN ovbunt_overfor.p (STRING(Ovbuffer.ButikkNrFra) + '|' + STRING(Ovbuffer.ButikkNrTil),
                               ihBuffer,
                               '',
                               OUTPUT cReturn,
                               OUTPUT bOk
                              ).

            EMPTY TEMP-TABLE tmpOverfor.
        END.
    END.

    /* Starter oppdatering */
    IF bOk THEN
    DO:
      DO TRANSACTION:
          FIND CURRENT OvBunt EXCLUSIVE-LOCK.
          ASSIGN
              OvBunt.DatoOppdatert = TODAY
              OvBunt.TidOppdatert  = TIME
              OvBunt.BatchNr       = 0
              OvBunt.OppdatertAv   = USERID("SkoTex") /*cOppdatertAv*/
              OVbunt.Merknad       = /*'Oppd. via bong: ' +*/ Ovbunt.Merknad
              .
          FIND CURRENT OvBunt NO-LOCK.
      END. /* TRANSACTION */


        /* TN 11/5-17 Gammel kode --------------------------
        RUN opprettbatchoverfor.p (OUTPUT piBatchNr, OUTPUT pcStatus).
        /* Setter batchnummer på bunten. */
        IF piBatchNr <> 0 THEN DO TRANSACTION:
            FIND CURRENT OvBunt EXCLUSIVE-LOCK.
                    ASSIGN
              OvBunt.BatchNr    = piBatchNr
              OvBunt.OppdatertAv = userid("SkoTex") /*cOppdatertAv*/
              .
        END.
        FIND CURRENT OvBunt NO-LOCK.
        IF piBatchNr <> 0 THEN
            RUN opprettfakturaoverfor.p (OUTPUT piBatchNr, OUTPUT pcStatus).
        --------------------------------------------------- */
    END.
    ELSE
        pcStatus = "Ingen overføringer å postere.".

  END. /* TRANSACTION */

  RETURN pcStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postTransactionValidate dTables  _DB-REQUIRED
PROCEDURE postTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hBuffer AS HANDLE     NO-UNDO.
/*     IF AVAIL RowObjUpd THEN DO:                   */
/*         ASSIGN hBuffer = BUFFER RowObjUpd:HANDLE. */
/*         RUN refetchDBRow (hBuffer).               */
/*     END.                                          */
    ASSIGN cOppdatertAv = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreTransactionValidate dTables 
PROCEDURE PreTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     Setter unikt buntnummer på alle nye bunter.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piBuntNr LIKE OvBunt.BuntNr NO-UNDO.

  FOR EACH RowObjUpd WHERE
    CAN-DO("A,C",RowObjUpd.RowMod):

    FIND LAST OvBunt NO-LOCK WHERE 
      Ovbunt.buntNr < 1000000000 
      USE-INDEX BuntNr NO-ERROR.    
    IF AVAILABLE OvBunt THEN
        piBuntNr = OvBunt.BuntNr + 1.
    ELSE
        piBuntNr = 1.
   
    ASSIGN
        RowObjUpd.BuntNr = piBuntNr
        RowObjUpd.Opphav = 1
        .
  END.

  ASSIGN
      cBuntListe = ""
      .
  FOR EACH RowObject WHERE
      RowObject.RowMod = "U":
      IF RowObject.DatoOppdatert <> ? THEN
      DO:
        ASSIGN
            cBuntListe = cBuntListe +
                         (IF cBuntListe = ""
                            THEN ""
                            ELSE ",") +
                         STRING(RowObject.BuntNr).
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintOverfor dTables  _DB-REQUIRED
PROCEDURE PrintOverfor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iBuntnr LIKE ovBunt.BuntNr NO-UNDO.
    RUN printoverforX.p (iBuntNr) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlaaSammen dTables  _DB-REQUIRED
PROCEDURE SlaaSammen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cValgTeBuntNr AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  iLinjeNr     AS INTEGER    NO-UNDO.
    DEFINE BUFFER bFraOvBunt   FOR ovBunt.
    DEFINE BUFFER bFraOvBuffer FOR ovBuffer.
    FIND OvBunt WHERE OvBunt.BuntNr = INT(ENTRY(1,cValgteBuntNr)) NO-LOCK.
    FIND LAST OvBuffer OF OvBunt USE-INDEX BuntLinjeNr NO-LOCK NO-ERROR.
    ASSIGN iLinjeNr = IF AVAIL OvBuffer THEN OvBuffer.LinjeNr + 1 ELSE 1.
    RELEASE OvBuffer.
    DO iCount = 2 TO NUM-ENTRIES(cValgteBuntNr):
        FIND bFraOvBunt WHERE bFraOvBunt.BuntNr = INT(ENTRY(iCount,cValgteBuntNr)).
        FOR EACH bFraOvBuffer OF bFraOvBunt:
            BUFFER-COPY bFraOvBuffer EXCEPT bFraOvBuffer.BuntNr bFraOvBuffer.LinjeNr
                 TO OvBuffer
                ASSIGN OvBuffer.BuntNr = OvBunt.BuntNr
                       OvBuffer.Linjenr = iLinjeNr
                       iLinjeNr = iLinjeNr + 1.
            RELEASE OvBuffer.
            DELETE bFraOvBuffer.
        END.
        DELETE bFraOvBunt.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuBatchOppdatert dTables  _DB-REQUIRED
FUNCTION fuBatchOppdatert RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAILABLE RowOBject THEN
  DO:
    FIND BatchLogg NO-LOCK WHERE
        BatchLogg.BatchNr = RowObject.BatchNr NO-ERROR.
    IF AVAILABLE BatchLogg THEN
        RETURN "Stat: " + STRING(BatchLogg.OppdStatus) + " " +
               string(BatchLogg.StatusOppdatert) + " " + 
               string(BatchLogg.TidOppdatert,"HH:MM:SS") + " " +
               BatchLogg.OppdatertAv.
    ELSE
        RETURN "".
  END.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuEKl dTables  _DB-REQUIRED
FUNCTION fuEKl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF RowObject.ETid = 0 OR
     RowObject.ETid = ? THEN
  RETURN "".
  ELSE 
    RETURN STRING(RowObject.ETid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuEndretInfo dTables  _DB-REQUIRED
FUNCTION fuEndretInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcEndretInfo AS CHAR NO-UNDO.

  ASSIGN
      pcEndretInfo = "Opprettet: " + 
                      STRING(RowObject.RegistrertDato) + " " + 
                      fuKlRegistrert() + " " + 
                      RowObject.RegistrertAv + " " + 
                      "Endret: " + 
                      STRING(RowObject.EDato) + " " + 
                      fuEKl() + " " +
                      RowObject.BrukerId
      pcEndretInfo = IF pcEndretInfo = ?
                       THEN ""
                       ELSE pcEndretInfo
      .

  RETURN pcEndretInfo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuFakturaNr dTables  _DB-REQUIRED
FUNCTION fuFakturaNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAILABLE RowOBject THEN
  DO:
    FIND FakturaHode NO-LOCK WHERE
        FakturaHode.Faktura_Id = RowObject.Faktura_Id NO-ERROR.
    IF AVAILABLE FakturaHode THEN
        RETURN STRING(FakturaHode.FakturaNr).
    ELSE
        RETURN "".
  END.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuKlOppdatert dTables  _DB-REQUIRED
FUNCTION fuKlOppdatert RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF RowObject.TidOppdatert = 0 OR
     RowObject.TidOppdatert = ? THEN
  RETURN "".
  ELSE 
    RETURN STRING(RowObject.TidOppdatert,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuKlRegistrert dTables  _DB-REQUIRED
FUNCTION fuKlRegistrert RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF RowObject.RegistrertTid = 0 OR
     RowObject.RegistrertTid = ? THEN
  RETURN "".
  ELSE 
    RETURN STRING(RowObject.RegistrertTid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

