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
DEFINE VARIABLE iCentralLager   AS INTEGER NO-UNDO.
DEFINE VARIABLE iInitProfilNr   AS INTEGER NO-UNDO.
DEF VAR iProfilNr    AS INT NO-UNDO.
DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iGant AS INTEGER NO-UNDO.

DEFINE VARIABLE cRowId              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{proclib.i}
{syspara.i 210 100 8 iGant INT}

/*cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE) NO-ERROR.*/

ASSIGN 
/*  bTest = IF SEARCH('tnc.txt') <> ? THEN TRUE ELSE FALSE*/
  bTest = TRUE 
  cLogg = 'dKampanjeHode' + REPLACE(STRING(TODAY),'/','') 
  NO-ERROR.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

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
&Scoped-define INTERNAL-TABLES KampanjeHode

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Aktivert Beskrivelse KampanjeId Notat SluttDato StartDato RegistrertDato~
 RegistrertTid EDato ETid BrukerID RegistrertAv ProfilNr AktiveresTid~
 GyldigTilTid Komplett NormalPris Kamp% KampanjePris AvslagType setAnnonse~
 LeverandorKampanje KampId
&Scoped-define ENABLED-FIELDS-IN-KampanjeHode Aktivert Beskrivelse ~
KampanjeId Notat SluttDato StartDato RegistrertDato RegistrertTid EDato ~
ETid BrukerID RegistrertAv ProfilNr AktiveresTid GyldigTilTid Komplett ~
NormalPris Kamp% KampanjePris AvslagType setAnnonse LeverandorKampanje ~
KampId 
&Scoped-Define DATA-FIELDS  Aktivert Beskrivelse KampanjeId Notat SluttDato StartDato RegistrertDato~
 RegistrertTid EDato ETid BrukerID RegistrertAv KannAktiveres ProfilNr~
 AktiveresTid GyldigTilTid Komplett NormalPris Kamp% KampanjePris AvslagType~
 setAnnonse fAktiveresTid fGyldigTidTil LeverandorKampanje KampId
&Scoped-define DATA-FIELDS-IN-KampanjeHode Aktivert Beskrivelse KampanjeId ~
Notat SluttDato StartDato RegistrertDato RegistrertTid EDato ETid BrukerID ~
RegistrertAv ProfilNr AktiveresTid GyldigTilTid Komplett NormalPris Kamp% ~
KampanjePris AvslagType setAnnonse LeverandorKampanje KampId 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dKampanjeHode.i"
&Scoped-define SELF-NAME Query-Main
&Scoped-define QUERY-STRING-Query-Main FOR EACH KampanjeHode NO-LOCK   BY KampanjeHode.KampanjeId DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY {&SELF-NAME} FOR EACH KampanjeHode NO-LOCK   BY KampanjeHode.KampanjeId DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main KampanjeHode
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main KampanjeHode


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addRow dTables  _DB-REQUIRED
FUNCTION addRow RETURNS CHARACTER
  ( INPUT pcViewColList AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ByttElement dTables  _DB-REQUIRED
FUNCTION ByttElement RETURNS CHARACTER
  ( INPUT ipSkjerm AS CHAR,
    INPUT ipElement AS INT,
    INPUT ipNyttElement AS CHAR,
    INPUT ipDelimiter AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      KampanjeHode SCROLLING.
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
         WIDTH              = 59.2.
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
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH KampanjeHode NO-LOCK
  BY KampanjeHode.KampanjeId DESCENDING INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.KampanjeHode.KampanjeId|no"
     _FldNameList[1]   > skotex.KampanjeHode.Aktivert
"Aktivert" "Aktivert" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[2]   > skotex.KampanjeHode.Beskrivelse
"Beskrivelse" "Beskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[3]   > skotex.KampanjeHode.KampanjeId
"KampanjeId" "KampanjeId" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[4]   > skotex.KampanjeHode.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 256 yes ""
     _FldNameList[5]   > skotex.KampanjeHode.SluttDato
"SluttDato" "SluttDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[6]   > skotex.KampanjeHode.StartDato
"StartDato" "StartDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[7]   > skotex.KampanjeHode.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[8]   > skotex.KampanjeHode.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[9]   > skotex.KampanjeHode.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[10]   > skotex.KampanjeHode.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[11]   > skotex.KampanjeHode.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[12]   > skotex.KampanjeHode.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[13]   > "_<CALC>"
"string(RowObject.Komplett = false and can-find(first KampanjeLinje where KampanjeLinje.KampanjeId = RowObject.KampanjeId),""J/N"")" "KannAktiveres" ? "x(8)" "character" ? ? ? ? ? ? no ? no 1 no ?
     _FldNameList[14]   > skotex.KampanjeHode.ProfilNr
"ProfilNr" "ProfilNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[15]   > skotex.KampanjeHode.AktiveresTid
"AktiveresTid" "AktiveresTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[16]   > skotex.KampanjeHode.GyldigTilTid
"GyldigTilTid" "GyldigTilTid" ? ? "integer" ? ? ? ? ? ? yes ? no 17 yes ""
     _FldNameList[17]   > skotex.KampanjeHode.Komplett
"Komplett" "Komplett" ? ? "logical" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[18]   > skotex.KampanjeHode.NormalPris
"NormalPris" "NormalPris" ? ? "logical" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[19]   > skotex.KampanjeHode.Kamp%
"Kamp%" "Kamp%" ? ? "decimal" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[20]   > skotex.KampanjeHode.KampanjePris
"KampanjePris" "KampanjePris" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.6 yes ""
     _FldNameList[21]   > skotex.KampanjeHode.AvslagType
"AvslagType" "AvslagType" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[22]   > skotex.KampanjeHode.setAnnonse
"setAnnonse" "setAnnonse" ? ? "logical" ? ? ? ? ? ? yes ? no 17.2 yes ""
     _FldNameList[23]   > "_<CALC>"
"STRING (RowObject.AktiveresTid,""HH:MM"")" "fAktiveresTid" "Fra tid" "x(5)" "character" ? ? ? ? ? ? no ? no 5.8 no ?
     _FldNameList[24]   > "_<CALC>"
"STRING (RowObject.GyldigTilTid,""HH:MM"")" "fGyldigTidTil" "Til tid" "x(5)" "character" ? ? ? ? ? ? no ? no 5 no ?
     _FldNameList[25]   > skotex.KampanjeHode.LeverandorKampanje
"LeverandorKampanje" "LeverandorKampanje" ? ? "logical" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[26]   > skotex.KampanjeHode.KampId
"KampId" "KampId" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ?
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Query-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Aktiver dTables  _DB-REQUIRED
PROCEDURE Aktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piKampanjeId AS INT  NO-UNDO.

  DEF VAR pcFeilMsg   AS CHAR NO-UNDO.
  DEF VAR piAntallOk  AS INT  NO-UNDO.
  DEF VAR piAntallTot AS INT  NO-UNDO.

  IF bTest THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      ' ' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start Aktiver' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  FØR UtforAktiver' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Modus: 2' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  piKampanjeId: ' + STRING(piKampanjeId) 
      ).    
  END.  
  /* pcFeilMsg = OK                          - Alle linjer aktiver.                         */
  /* pcFeilMsg = <Ant.Aktivert>,<Ant.Totalt> - Antall linjer aktivert, Antall linjer totalt */
  
  RUN UtforAktiver (2, piKampanjeId, OUTPUT piAntallOk, OUTPUT piAntallTot).

  /* Setter feilmelding. */
  IF piAntallOk = piAntallTot THEN
      ASSIGN
        pcFeilMsg = "OK," + STRING(piKampanjeId) + "," + STRING(piAntallTot)
        .
  ELSE
      ASSIGN
        pcFeilMsg = "AVBRYT," + STRING(piKampanjeId) + "," + STRING(piAntallTot)
        .

  IF bTest THEN 
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ETTER UtforAktiver' 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  piAntallOk: ' + STRING(piAntallOk) 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  piAntallTot: ' + STRING(piAntallTot) 
      ).    
  END.  

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt Aktiver' 
      ).    
      
  RETURN pcFeilMsg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH RowObjUpd WHERE RowObjUpd.RowMod = "D":
        FOR EACH KampanjeLinje WHERE KampanjeLinje.KampanjeId = 
                                          RowObjUpd.KampanjeId EXCLUSIVE:
            DELETE KampanjeLinje.
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
         rowObject.fAktiveresTid = (STRING (RowObject.AktiveresTid,"HH:MM"))
         rowObject.fGyldigTidTil = (STRING (RowObject.GyldigTilTid,"HH:MM"))
         rowObject.KannAktiveres = (STRING(RowObject.Komplett = FALSE AND CAN-FIND(FIRST KampanjeLinje WHERE KampanjeLinje.KampanjeId = RowObject.KampanjeId),"J/N"))
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
  {syspara.i 5 1 1 iCentralLager INT}
  {syspar2.i 5 1 1 cOptProfilbutik}
  cOptProfilbutik = TRIM(cOptProfilbutik).        
  FIND Bruker NO-LOCK WHERE
      Bruker.BrukerId = USERID('SkoTex') NO-ERROR.
  IF AVAILABLE Bruker AND Bruker.BrukerType = 2 AND Bruker.ButikkNr > 0 THEN
  DO:
  END.
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = Bruker.ButikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN
        iProfilNr = Butiker.ProfilNr.
  ELSE 
      iProfilNr = 0.

  FIND Butiker WHERE Butiker.Butik = iCentralLager NO-LOCK NO-ERROR.
  IF AVAIL Butiker THEN
      ASSIGN iInitProfilNr = Butiker.ProfilNr.

  IF Bruker.BrukerType = 2 THEN
      DYNAMIC-FUNCTION('setQueryWhere':U,
        INPUT "KampanjeHode.ProfilNr = '" + STRING(iProfilNr) + "'" /* CHARACTER */).

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

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
    DEFINE VARIABLE piKampanjeId AS INTEGER    NO-UNDO.

    FIND LAST KampanjeHode NO-LOCK NO-ERROR.
    
    ASSIGN piKampanjeId = IF AVAIL KampanjeHode THEN KampanjeHode.KampanjeId + 1 ELSE 1.
    FOR EACH RowObjUpd WHERE CAN-DO("A,C",RowObjUpd.RowMod):
        IF RowObjUpd.Beskrivelse = "" THEN
            RETURN.
        ASSIGN RowObjUpd.KampanjeId = piKampanjeId
               piKampanjeId = piKampanjeId + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintKampanje dTables  _DB-REQUIRED
PROCEDURE PrintKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piKampanjeId AS INT NO-UNDO.
  DEF INPUT PARAMETER piType AS INT NO-UNDO.
  RUN printkampanje.p (piKampanjeId,piType) NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowObjectValidate dTables  _DB-REQUIRED
PROCEDURE rowObjectValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR cErrMessage AS CHARACTER NO-UNDO.

    IF RowObject.Beskrivelse = "" THEN
        ASSIGN cErrMessage = "Ange beskrivelse" + CHR(10).
    IF RowObject.StartDato < TODAY THEN
        ASSIGN cErrMessage = cErrMessage + "Startdato < dagens dato".
    IF RowObject.StartDato > RowObject.SluttDato THEN
        ASSIGN cErrMessage = cErrMessage + "Startdato > sluttdato".
    IF cErrMessage NE "" THEN
        RETURN cErrMessage.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}




{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SimulerAktiver dTables  _DB-REQUIRED
PROCEDURE SimulerAktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piKampanjeId AS INT  NO-UNDO.

  DEF VAR pcFeilMsg   AS CHAR NO-UNDO.
  DEF VAR piAntallOk  AS INT  NO-UNDO.
  DEF VAR piAntallTot AS INT  NO-UNDO.

  /* pcFeilMsg = OK                          - Alle linjer aktiver.                         */
  /* pcFeilMsg = <Ant.Aktivert>,<Ant.Totalt> - Antall linjer aktivert, Antall linjer totalt */
  
  RUN UtforAktiver (1, piKampanjeId, OUTPUT piAntallOk, OUTPUT piAntallTot).

  /* Setter feilmelding. */
  IF piAntallOk = piAntallTot THEN
      ASSIGN
        pcFeilMsg = "OK," + STRING(piAntallOk) + "," + STRING(piAntallTot)
        .
  ELSE
      ASSIGN
        pcFeilMsg = "AVBRYT," + STRING(piAntallOk) + "," + STRING(piAntallTot)
        .

  RETURN pcFeilMsg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtforAktiver dTables  _DB-REQUIRED
PROCEDURE UtforAktiver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piModus      AS INT  NO-UNDO.
  DEF INPUT  PARAMETER piKampanjeId AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER piAntallOk   AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER piAntallTot  AS INT  NO-UNDO.
  
  DEF VAR h_PrisKo AS HANDLE NO-UNDO.
  DEF VAR pcError  AS CHAR   NO-UNDO.
  DEF VAR pcSkjerm AS CHAR   NO-UNDO.
  
  DEFINE BUFFER LokPrisKo FOR PrisKo.
  DEFINE BUFFER bufArtPris FOR ArtPris.


  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start UtforAktiver' 
      ).    

  /* - Kontroller alle linjer og sett på feilmelding.         */
  /* - Hvis utfør, sett Behandlet = true på godkjente linjer. */
  /* - Ajourfør tellere kontinuerlig.                         */

  FIND KampanjeHode NO-LOCK WHERE
      KampanjeHode.KampanjeId = piKampanjeId NO-ERROR.

  IF bTest THEN 
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Modus: ' + STRING(piModus) 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  piKampanjeId: ' + STRING(piKampanjeId) 
      ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  KampanjeHode avail: ' + STRING(AVAILABLE KampanjeHode) 
      ).    
  END.              
      
  IF NOT AVAILABLE KampanjeHode THEN
      RETURN "AVBRYT".

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  BEHANDLE-LINJER:
  FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK WHERE
      KampanjeLinje.Behandlet = FALSE:

    /* Artikkler som ikke skal på kampanje. */
    IF iGant = 1 AND KampanjeHode.IgnorerNOS = FALSE THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE
            ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas AND ArtBas.Lagerkoder MATCHES '*NOS*' THEN
          NEXT.
    END.      
    
    /* Er artikkelen lagt inn i kampanjen, og den mangler pris på den aktuelle profilenen, */
    /* skal prisen hentes fra hk kalkylen.                                                 */
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.Artikkel = KampanjeLinje.ArtikkelNr AND 
      ArtPris.ProfilNr = KampanjeLinje.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
    DO FOR bufArtPris TRANSACTION:
      FIND FIRST ArtPris NO-LOCK WHERE 
        ArtPris.ArtikkelNr = KampanjeLinje.ArtikkelNr AND 
        ArtPris.ProfilNr   = 1 NO-ERROR.
      IF AVAILABLE ArtPris THEN 
      BUFFER-COPY ArtPris 
        EXCEPT ProfilNr 
        TO bufArtPris
        ASSIGN 
          bufArtPris.ProfilNr = KampanjeLinje.ProfilNr
          .
      IF AVAILABLE bufArtPris THEN RELEASE bufArtPris.
    END. /* TRANSACTION */
        
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  KampanjeLinje Id: ' + STRING(KampanjeLinje.KampanjeId) + ' Linje:' + 
                                 STRING(KampanjeLinje.KampanjeLinje) + ' ArtikkelNr:' + 
                                 STRING(KampanjeLinje.ArtikkelNr) + ' ProfilNr:' +  
                                 STRING(KampanjeLinje.ProfilNr) + ' NormalPris: ' +   
                                 STRING(KampanjeHode.NormalPris) + ' Lev.kampanje: ' +   
                                 STRING(KampanjeHode.LeverandorKampanje) + ' SettAnnonse: ' +   
                                 STRING(KampanjeHode.setAnnonse)   
        ).    

    /* Totalt antall linjer. */
    ASSIGN
        pcError     = ""
        piAntallTot = piAntallTot + 1
        .

    RUN SjekkNyPrisKo IN h_PrisKo 
                     (KampanjeLinje.ArtikkelNr, 
                      KampanjeLinje.ProfilNr,    
                      KampanjeHode.StartDato,        
                      KampanjeHode.AktiveresTid,         
                      KampanjeHode.SluttDato,       
                      KampanjeHode.gyldigTilTid,        
                      ?,  
                      (IF KampanjeHode.NormalPris
                         THEN FALSE /* Normalpris */
                         ELSE TRUE /* Tilbud */),      
                      (IF KampanjeHode.NormalPris
                         THEN 1 /* Normalpris */
                       ELSE IF KampanjeHode.LeverandorKampanje
                         THEN 5 /* Leverandørkampanje */
                       ELSE 2 /* Tilbud */),      
                      OUTPUT pcError).        

    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Etter SjekkNyPrisKo: ' + pcError 
        ).    

    /* Merker linjer med feilkode. */
    IF pcError <> "" THEN. /* Gjør ingenting. */
    ELSE /* Antall godkjente */
    OPPRETT:
    DO: /* Oppretter priskøpost. */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
      DO:
          pcError = "10" + CHR(1) + "(10) Ukjent artikkennummer!".
          IF bTest THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Error: ' + pcError 
              ).    
          LEAVE OPPRETT.  /* Ja ja ... */
      END.
      IF KampanjeHode.setAnnonse THEN
      DO TRANSACTION:
          FIND CURRENT ArtBas EXCLUSIVE-LOCK.
          ArtBas.AnonseArtikkel = TRUE.
          FIND CURRENT ArtBas NO-LOCK.
      END.

      FIND VarGr  OF ArtBas NO-LOCK NO-ERROR.
      FIND Moms   OF VarGr  NO-LOCK NO-ERROR.
      FIND Valuta OF ArtBas NO-LOCK NO-ERROR.

      /* Bygge skjerm streng ut fra aktiv kalkyle */
      RUN InitKalkyle IN h_PrisKo
            (RECID(ArtBas),
             KampanjeLinje.ProfilNr, 
             INPUT-OUTPUT pcSkjerm,     
             Moms.MomsProc,   
             Valuta.ValKurs,    
             18,     
             (IF AVAILABLE ArtPris
                THEN ArtPris.Tilbud
                ELSE FALSE)).     

      /* Oppdaterer strengen med den nye prisen. */
      pcSkjerm = ByttElement(INPUT pcSkjerm,
                      INPUT 18,
                      /* Det er kun 2 som benyttes 
                      input (IF KampanjeHode.NormalPris
                              THEN string(KampanjeLinje.Pris[1])
                              ELSE STRING(KampanjeLinje.Pris[2])) ,
                      */
                      INPUT STRING(KampanjeLinje.Pris[2]) ,
                      INPUT ";").

      /* Om vi har varekost i kampanjelinje */
      /* lagt till 31jan-08 */
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 1,
                          INPUT STRING(ROUND(KampanjeLinje.varekost / Valuta.ValKurs,2)) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 2,
                          INPUT STRING(KampanjeLinje.varekost) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 3,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 4,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 5,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 6,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 7,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 8,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 9,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 10,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 11,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 12,
                          INPUT STRING(0) ,
                          INPUT ";").
      END.
      IF kampanjelinje.varekost <> 0 THEN DO:
          pcSkjerm = ByttElement(INPUT pcSkjerm,
                          INPUT 13,
                          INPUT STRING(KampanjeLinje.varekost) ,
                          INPUT ";").
      END.
      /* END varekost */

      IF KampanjeHode.NormalPris = FALSE THEN
      TILB-OPPDAT-STRENG:
      DO:
        /* Tilbud fra */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                              INPUT 23,
                              INPUT STRING(KampanjeHode.StartDato),
                              INPUT ";").
        /* Tilbud fra tid */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                              INPUT 24,
                              INPUT STRING(KampanjeHode.AktiveresTid),
                              INPUT ";").
        /* Tilbud til */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                              INPUT 25,
                              INPUT STRING(KampanjeHode.SluttDato),
                              INPUT ";").
        /* Tilbud til tid */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                              INPUT 26,
                              INPUT STRING(KampanjeHode.GyldigTilTid),
                              INPUT ";").

      END. /* TILB-OPPDAT-STRENG */
      ELSE 
      NORM-OPPDAT-STRENG:
      DO:
        /* Fra */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                              INPUT 21,
                              INPUT STRING(KampanjeHode.StartDato),
                              INPUT ";").
        /* Til tid */
        pcSkjerm = ByttElement(INPUT pcSkjerm,
                              INPUT 22,
                              INPUT STRING(KampanjeHode.AktiveresTid),
                              INPUT ";").

      END. /* NORM-OPPDAT-STRENG */

      FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
          ArtPris.ProfilNr = KampanjeLinje.ProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN
        FIND FIRST ArtPris OF ArtBas NO-LOCK WHERE
            ArtPris.ProfilNr = iInitProfilNr NO-ERROR.
      IF NOT AVAILABLE ArtPris THEN
      DO:
          pcError = "11" + CHR(1) + "(11) Ingen artpris tilgjengelig på artikkelen!".
        IF bTest THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
            ' Error: ' + pcError  
            ).    
          LEAVE OPPRETT.  /* Ja ja ... */
      END.

      /* Oppdatering skal utføres. */
      IF piModus = 2 THEN
      DO:
        /* TN 19/7-20 Denne gjør ikke noe, og renger ikke kjøres.
        RUN Klargjor_kampanje_prisko.p (ROWID(ArtBas),KampanjeLinje.KampanjeId).
        */
        RUN NyPrisKo IN h_PrisKo
            (RECID(ArtBas),
             KampanjeLinje.ProfilNr,   
             INPUT-OUTPUT pcSkjerm,     
             (IF KampanjeHode.NormalPris
                         THEN FALSE /* Normalpris */
                         ELSE TRUE /* Tilbud */),
             (IF KampanjeHode.Normalpris 
                THEN 1
              ELSE IF KampanjeHode.LeverandorKampanje
                THEN 5 /* Leverandørkampanje */
              ELSE 2)). 
        IF NUM-ENTRIES(RETURN-VALUE,'|') > 1 THEN 
          cRowId = ENTRY(2,RETURN-VALUE,'|').
        ELSE
          cRowId = ''.
      END.

      ASSIGN
        piAntallOk = piAntallOk + 1
        .
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Etter Klargjor_kampanje_prisko.p og NyPrisKo piAntallOk: ' + STRING(piAntallOk) 
          ).    
        
    END. /* OPPRETT */
    
    /* TN 19/7-20 Aktiverer kampanje for ekstra prisprofiler. */
    IF cRowId <> '' THEN 
    EKSTRAPRISPROFIL:
    FOR EACH KampanjeProfil NO-LOCK WHERE 
      KampanjeProfil.KampanjeId = KampanjeLinje.KampanjeId:
      FIND LokPrisKo NO-LOCK WHERE 
        ROWID(LokPrisKo) = TO-ROWID(cRowId) NO-ERROR.
      IF AVAILABLE LokPrisko THEN 
      DO TRANSACTION:
        CREATE prisko.
        BUFFER-COPY LokPrisKo
          EXCEPT ProfilNr
          TO PrisKo
          ASSIGN 
            PrisKo.ProfilNr = KampanjeProfil.ProfilNr
            .
      END. /* TRANSACTION */  
    END. /* EKSTRAPRISPROFIL */    
  END. /* BEHANDLE-LINJER */
  
  IF piModus = 2 THEN
  DO TRANSACTION:
      FIND CURRENT KampanjeHode EXCLUSIVE-LOCK.
      IF (piAntallOk > 0) AND
         (piAntallOk = piAntallTot) THEN
           ASSIGN
             KampanjeHode.Komplett = TRUE.
      IF (piAntallOk > 0) AND
         (piAntallOk <= piAntallTot) THEN
           ASSIGN
             KampanjeHode.Aktivert = TRUE
             KampanjeHode.Notat = 'Aktivert: ' + STRING(NOW,"99/99/99 HH:MM:SS") + ' av ' + USERID('SkoTex') +
                                  (IF KampanjeHode.IgnorerNOS = TRUE THEN ' NOS flagg ignorert ved aktivering.' ELSE '') + 
                                  (IF KampanjeHode.Notat <> '' THEN CHR(10) ELSE '') + 
                                  KampanjeHode.Notat + '.'
             .
      FOR EACH KampanjeLinje OF KampanjeHode EXCLUSIVE-LOCK:
        ASSIGN
          KampanjeLinje.Behandlet = TRUE
          .
      END.
      IF bTest THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Stempler hode' 
          ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Komplett:' + STRING(KampanjeHode.Komplett) 
          ).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Aktivert:' + STRING(KampanjeHode.Aktivert) 
          ).
      END.    
      RELEASE KampanjeHode.
  END. /* TRANSACTION */

  IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Modus:' + STRING(piModus) 
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  piAntallTot:' + STRING(piAntallTot) 
      ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  piAntallOk:' + STRING(piAntallOk) 
      ).
  END.    

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.

  IF bTest THEN 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Ferdig UtforAktiver' 
      ).    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addRow dTables  _DB-REQUIRED
FUNCTION addRow RETURNS CHARACTER
  ( INPUT pcViewColList AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose: local SDO version of addRow
  Params:  pcViewColList AS CHARACTER – comma-separated list of
           the columns whose values should be returned.
  Notes:   This function first invokes the standard addRow and
           then sets the city field to ‘ Nashua’. If the City field
           is displayed by the caller, this value is inserted into
           that list as well.  
------------------------------------------------------------------------------*/
  DEF VAR cReturn AS CHARACTER.
  DEF VAR iPos AS INTEGER.
  /* First invoke the standard behavior and save the list of values which it returns. */
  ASSIGN cReturn               = SUPER( INPUT pcViewColList )
         iPos                 = LOOKUP('ProfilNr', pcViewColList)
         RowObject.ProfilNr   = iInitProfilNr
         .
         /* If ‘City’ was in the requested field list, insert our initial value
            (allowing for the RowIdent as the first entry in the list. */
  IF iPos NE 0 THEN
    ENTRY(iPos + 1, cReturn, CHR(1)) = STRING(iInitProfilNr).

  RETURN cReturn.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ByttElement dTables  _DB-REQUIRED
FUNCTION ByttElement RETURNS CHARACTER
  ( INPUT ipSkjerm AS CHAR,
    INPUT ipElement AS INT,
    INPUT ipNyttElement AS CHAR,
    INPUT ipDelimiter AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR ipLoop  AS INT NO-UNDO.
  DEF VAR ipTekst AS CHAR NO-UNDO.
  
  ipTekst = "".
  DO ipLoop = 1 TO NUM-ENTRIES(ipSkjerm,ipDelimiter):
    ASSIGN ipTekst = ipTekst + 
           (IF ipTekst = ""
              THEN ""
              ELSE ipDelimiter) +
           (IF ipLoop = ipElement 
              THEN ipNyttElement
              ELSE ENTRY(ipLoop,ipSkjerm,ipDelimiter)). 
  END.

  RETURN ipTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

