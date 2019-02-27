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
&Scoped-define INTERNAL-TABLES OvBuffer

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Antall ArtikkelNr BrukerID ButikkNrFra ButikkNrTil EDato ETid LinjeNr LopNr~
 Merknad RegistrertAv RegistrertDato RegistrertTid Storl TilStorl Vg BuntNr~
 Mva% MvaKr VareKost
&Scoped-define ENABLED-FIELDS-IN-OvBuffer Antall ArtikkelNr BrukerID ~
ButikkNrFra ButikkNrTil EDato ETid LinjeNr LopNr Merknad RegistrertAv ~
RegistrertDato RegistrertTid Storl TilStorl Vg BuntNr Mva% MvaKr VareKost 
&Scoped-Define DATA-FIELDS  Antall fVgBeskr ArtikkelNr BrukerID fBeskr ButikkNrFra ButikkNrTil~
 fFraButikk EDato ETid fTilbutikk LinjeNr LopNr fKlOpprettet Merknad~
 RegistrertAv fKlEndret RegistrertDato RegistrertTid Storl TilStorl Vg~
 BuntNr Mva% MvaKr VareKost fDatoOppdatert BildNr
&Scoped-define DATA-FIELDS-IN-OvBuffer Antall ArtikkelNr BrukerID ~
ButikkNrFra ButikkNrTil EDato ETid LinjeNr LopNr Merknad RegistrertAv ~
RegistrertDato RegistrertTid Storl TilStorl Vg BuntNr Mva% MvaKr VareKost 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dovbuffer.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main OvBuffer
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main OvBuffer


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBeskr dTables  _DB-REQUIRED
FUNCTION fBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFraButikk dTables  _DB-REQUIRED
FUNCTION fFraButikk RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fKlEndret dTables  _DB-REQUIRED
FUNCTION fKlEndret RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fKlOpprettet dTables  _DB-REQUIRED
FUNCTION fKlOpprettet RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLevKod dTables  _DB-REQUIRED
FUNCTION fLevKod RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOvBuntOppdat dTables  _DB-REQUIRED
FUNCTION fOvBuntOppdat RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOvBuntOppdatert dTables  _DB-REQUIRED
FUNCTION fOvBuntOppdatert RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTilButikk dTables  _DB-REQUIRED
FUNCTION fTilButikk RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVgBeskr dTables  _DB-REQUIRED
FUNCTION fVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtBasBildNr dTables  _DB-REQUIRED
FUNCTION getArtBasBildNr RETURNS INTEGER
  ( INPUT ArtikkelNr AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      OvBuffer SCROLLING.
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
     _TblList          = "skotex.OvBuffer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.OvBuffer.Antall
"Antall" "Antall" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[2]   > "_<CALC>"
"fVgBeskr()" "fVgBeskr" ? "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[3]   > skotex.OvBuffer.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[4]   > skotex.OvBuffer.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[5]   > "_<CALC>"
"fBeskr()" "fBeskr" "Beskrivelse" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[6]   > skotex.OvBuffer.ButikkNrFra
"ButikkNrFra" "ButikkNrFra" ? ? "integer" ? ? ? ? ? ? yes ? no 16.8 yes ""
     _FldNameList[7]   > skotex.OvBuffer.ButikkNrTil
"ButikkNrTil" "ButikkNrTil" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[8]   > "_<CALC>"
"fFraButikk()" "fFraButikk" "Fra butikk" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[9]   > skotex.OvBuffer.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[10]   > skotex.OvBuffer.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[11]   > "_<CALC>"
"fTilButikk()" "fTilbutikk" "Til butikk" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[12]   > skotex.OvBuffer.LinjeNr
"LinjeNr" "LinjeNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[13]   > skotex.OvBuffer.LopNr
"LopNr" "LopNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[14]   > "_<CALC>"
"fKlOpprettet()" "fKlOpprettet" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[15]   > skotex.OvBuffer.Merknad
"Merknad" "Merknad" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[16]   > skotex.OvBuffer.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[17]   > "_<CALC>"
"fKlEndret()" "fKlEndret" "Kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[18]   > skotex.OvBuffer.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[19]   > skotex.OvBuffer.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[20]   > skotex.OvBuffer.Storl
"Storl" "Storl" ? ? "character" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[21]   > skotex.OvBuffer.TilStorl
"TilStorl" "TilStorl" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[22]   > skotex.OvBuffer.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[23]   > skotex.OvBuffer.BuntNr
"BuntNr" "BuntNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[24]   > skotex.OvBuffer.Mva%
"Mva%" "Mva%" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[25]   > skotex.OvBuffer.MvaKr
"MvaKr" "MvaKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[26]   > skotex.OvBuffer.VareKost
"VareKost" "VareKost" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[27]   > "_<CALC>"
"fOvBuntOppdatert()" "fDatoOppdatert" ? "99/99/99" "Date" ? ? ? ? ? ? no ? no 9.2 no ?
     _FldNameList[28]   > "_<CALC>"
"getArtBasBildNr(INPUT ArtikkelNr)" "BildNr" ? "zzzzz9" "Integer" ? ? ? ? ? ? no ? no 6.2 no ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AntallValidate dTables 
PROCEDURE AntallValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piAntall AS INT NO-UNDO.

  IF piAntall = 0 THEN
      RETURN "Antall er ikke angitt.".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DATA.CALCULATE dTables  DATA.CALCULATE _DB-REQUIRED
PROCEDURE DATA.CALCULATE :
/*------------------------------------------------------------------------------
  Purpose:     Calculate all the Calculated Expressions found in the
               SmartDataObject.
  Parameters:  <none>
------------------------------------------------------------------------------*/
      ASSIGN 
         rowObject.BildNr = (getArtBasBildNr(INPUT ArtikkelNr))
         rowObject.fBeskr = (fBeskr())
         rowObject.fDatoOppdatert = (fOvBuntOppdatert())
         rowObject.fFraButikk = (fFraButikk())
         rowObject.fKlEndret = (fKlEndret())
         rowObject.fKlOpprettet = (fKlOpprettet())
         rowObject.fTilbutikk = (fTilButikk())
         rowObject.fVgBeskr = (fVgBeskr())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVarekost dTables  _DB-REQUIRED
PROCEDURE GetVarekost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER iVg         LIKE OvBuffer.Vg     NO-UNDO.
  DEFINE INPUT  PARAMETER iLopNr      LIKE OvBuffer.Lopnr  NO-UNDO.
  DEFINE INPUT  PARAMETER iFraButikk  LIKE OvBuffer.ButikkNrFra NO-UNDO.
  DEFINE OUTPUT PARAMETER fVarekost   AS DECIMAL         NO-UNDO.
  FIND ArtBas WHERE ArtBas.Vg    = iVg AND
                    Artbas.LopNr = iLopnr NO-LOCK.
  FIND Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                   Lager.Butik      = iFraButikk NO-LOCK NO-ERROR.
  ASSIGN fVareKost = IF AVAIL Lager THEN Lager.VVarekost ELSE 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LopNrValidate dTables  _DB-REQUIRED
PROCEDURE LopNrValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piLopNr AS INT NO-UNDO.

  IF piLopNr = ? THEN
      RETURN "Ugyldig løpenummer!".
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
  DEF VAR piLinjeNr LIKE OvBuffer.LinjeNr NO-UNDO.
  DEF VAR cStorl    AS CHARACTER  NO-UNDO.
  DEF VAR cTilStorl AS CHARACTER  NO-UNDO.
  FOR EACH RowObjUpd WHERE
      CAN-DO("A,C,U",RowObjUpd.RowMod):
      FIND Butiker NO-LOCK WHERE
          Butiker.Butik = RowObjUpd.ButikkNrFra NO-ERROR.
      IF NOT AVAILABLE Butiker THEN
          RETURN "Ukjent FRA butikk".

      FIND Butiker NO-LOCK WHERE
          Butiker.Butik = RowObjUpd.ButikkNrTil NO-ERROR.
      IF NOT AVAILABLE Butiker THEN
          RETURN "Ukjent TIL butikk".

      FIND VarGr NO-LOCK WHERE
          VarGr.Vg = RowObjUpd.Vg NO-ERROR.
      IF NOT AVAILABLE VarGr THEN
          RETURN "Ukjent varegruppe!".

      FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg    = RowObjUpd.Vg AND
          ArtBas.LopNr = RowObjUpd.LopNr NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          RETURN "Ukjent artikkel " + 
                 string(RowObjUpd.Vg) + "/" + 
                 string(RowObjUpd.LopNr).

      /* Linjenummer for nye rader. */
      IF CAN-DO("A,C",RowObjUpd.RowMod) THEN
      DO:
          FIND LAST OvBuffer WHERE OvBuffer.BuntNr = int(entry(1,DYNAMIC-FUNCTION('getForeignValues':U)))
              USE-INDEX BuntLinjeNr NO-LOCK NO-ERROR.
          IF AVAILABLE OvBuffer THEN
              ASSIGN
                piLinjeNr = Ovbuffer.LinjeNr + 1.
          ELSE
              ASSIGN
                piLinjeNr = 1.
      END.
      /* Setter Artikkelnummer m.m */
      ASSIGN
          cStorl = RIGHT-TRIM(TRIM(RowObjUpd.Storl),".")
          cStorl = (IF LENGTH(cStorl) = 1 OR LENGTH(cStorl) = 3 THEN " " ELSE "")
                     + cStorl
          cTilStorl = RIGHT-TRIM(TRIM(RowObjUpd.TilStorl),".")
          cTilStorl = (IF LENGTH(cTilStorl) = 1 OR LENGTH(cTilStorl) = 3 THEN " " ELSE "")
                     + cTilStorl
          RowObjUpd.ArtikkelNr = ArtBas.ArtikkelNr
          RowObjUpd.Storl      = cStorl
          RowObjUpd.TilStorl   = IF cTilStorl = ""
                                   THEN RowObjUpd.Storl
                                   ELSE cTilStorl
          RowObjUpd.BuntNr     = IF CAN-DO("A,C",RowObjUpd.RowMod) 
                                   THEN int(entry(1,DYNAMIC-FUNCTION('getForeignValues':U)))
                                   ELSE RowObjUpd.BuntNr
          RowObjUpd.LinjeNr    = IF CAN-DO("A,C",RowObjUpd.RowMod) 
                                   THEN piLinjeNr
                                   ELSE RowObjUpd.LinjeNr
          .

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowObjectValidate dTables  _DB-REQUIRED
PROCEDURE RowObjectValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH RowObject WHERE
      CAN-DO("A,C,U",RowObject.RowMod):

     IF RIGHT-TRIM(TRIM(RowObject.Storl),".") = "" THEN
         RETURN "Angi størrelse!".
     IF RowObject.ButikkNrFra = RowObject.ButikkNrTil AND 
        (RIGHT-TRIM(TRIM(RowObject.TilStorl),".") = "" OR 
         RIGHT-TRIM(TRIM(RowObject.Storl),".") = RIGHT-TRIM(TRIM(RowObject.TilStorl),".")) THEN
         RETURN "Fra og til butikk er like. Angi bytte av størrelse!".
     IF RowObject.Antall = 0 THEN
         RETURN "Antall = 0".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderArtikkel dTables  _DB-REQUIRED
PROCEDURE ValiderArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piVg    AS INT NO-UNDO.
  DEF INPUT PARAMETER piLopNr AS INT NO-UNDO.

  FIND ArtBas NO-LOCK WHERE
    ArtBas.Vg    = piVg AND
    ArtBas.LopNr = piLopNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
    RETURN ArtBas.Beskr.
  ELSE
    RETURN "FALSE".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderButikk dTables  _DB-REQUIRED
PROCEDURE ValiderButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piButikkNr AS INT NO-UNDO.

  FIND Butiker NO-LOCK WHERE
    Butiker.Butik = piButikkNr NO-ERROR.
  IF AVAILABLE Butiker THEN
    RETURN Butiker.ButNamn.
  ELSE
    RETURN "FALSE".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderVarGr dTables  _DB-REQUIRED
PROCEDURE ValiderVarGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piVg AS INT NO-UNDO.

  FIND VarGr NO-LOCK WHERE
    VarGr.Vg = piVg NO-ERROR.
  IF AVAILABLE VarGr THEN
    RETURN VarGr.VgBeskr.
  ELSE
    RETURN "FALSE".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBeskr dTables  _DB-REQUIRED
FUNCTION fBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
    RETURN ArtBas.Beskr.   /* Function return value. */
  ELSE 
    RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFraButikk dTables  _DB-REQUIRED
FUNCTION fFraButikk RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = RowObject.ButikkNrFra NO-ERROR.
  IF AVAILABLE Butiker THEN
      RETURN Butiker.KortNavn.
  ELSE 
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fKlEndret dTables  _DB-REQUIRED
FUNCTION fKlEndret RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.ETid,"HH:MM:SS").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fKlOpprettet dTables  _DB-REQUIRED
FUNCTION fKlOpprettet RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.RegistrertTid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLevKod dTables  _DB-REQUIRED
FUNCTION fLevKod RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
      RETURN ArtBas.LevKod.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOvBuntOppdat dTables  _DB-REQUIRED
FUNCTION fOvBuntOppdat RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ovBunt OF RowObject NO-LOCK NO-ERROR.
  RETURN IF AVAIL ovBunt THEN ovBunt.DatoOppdatert ELSE ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOvBuntOppdatert dTables  _DB-REQUIRED
FUNCTION fOvBuntOppdatert RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ovBunt OF RowObject NO-LOCK NO-ERROR.
  RETURN IF AVAIL ovBunt THEN ovBunt.DatoOppdatert ELSE ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTilButikk dTables  _DB-REQUIRED
FUNCTION fTilButikk RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = RowObject.ButikkNrTil NO-ERROR.
  IF AVAILABLE Butiker THEN DO:
      IF TRIM(Butiker.KortNavn) <> "" THEN
          RETURN Butiker.KortNavn.
      ELSE
          RETURN "But:" + STRING(Butiker.Butik).
  END.
  ELSE 
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVgBeskr dTables  _DB-REQUIRED
FUNCTION fVgBeskr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = RowObject.Vg NO-ERROR.
  IF AVAILABLE VarGr THEN
      RETURN VarGr.VgBeskr.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtBasBildNr dTables  _DB-REQUIRED
FUNCTION getArtBasBildNr RETURNS INTEGER
  ( INPUT ArtikkelNr AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE
      ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN ArtBas.BildNr ELSE 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

