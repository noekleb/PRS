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

&Global-define DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES KassererOppgj

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  ButikkNr Dato KassererNr z_nummer OpptaltInnVeksel OpptaltKontanter~
 OpptaltSjekk OpptaltValuta OpptaltReserve OpptaltGavekort~
 OpptaltGavekortAndre OpptaltGavekortUtlevert OpptaltTilgode~
 OpptaltTilgodeAndre OpptaltTilgodeUtlevert OpptaltBilag OpptaltVeksel~
 OpptaltLevertBank PoseNr RegistrertAv RegistrertDato RegistrertTid BrukerID~
 EDato ETid OpptaltKupong
&Scoped-define ENABLED-FIELDS-IN-KassererOppgj ButikkNr Dato KassererNr ~
z_nummer OpptaltInnVeksel OpptaltKontanter OpptaltSjekk OpptaltValuta ~
OpptaltReserve OpptaltGavekort OpptaltGavekortAndre OpptaltGavekortUtlevert ~
OpptaltTilgode OpptaltTilgodeAndre OpptaltTilgodeUtlevert OpptaltBilag ~
OpptaltVeksel OpptaltLevertBank PoseNr RegistrertAv RegistrertDato ~
RegistrertTid BrukerID EDato ETid OpptaltKupong 
&Scoped-Define DATA-FIELDS  ButikkNr Dato KassererNr z_nummer OpptaltInnVeksel OpptaltKontanter~
 OpptaltSjekk OpptaltValuta OpptaltReserve OpptaltGavekort~
 OpptaltGavekortAndre OpptaltGavekortUtlevert OpptaltTilgode~
 OpptaltTilgodeAndre OpptaltTilgodeUtlevert OpptaltBilag OpptaltVeksel~
 OpptaltLevertBank PoseNr fuEndretInfo RegistrertAv RegistrertDato~
 RegistrertTid BrukerID EDato ETid fuValorer fuBilag fuValuta OpptaltKupong
&Scoped-define DATA-FIELDS-IN-KassererOppgj ButikkNr Dato KassererNr ~
z_nummer OpptaltInnVeksel OpptaltKontanter OpptaltSjekk OpptaltValuta ~
OpptaltReserve OpptaltGavekort OpptaltGavekortAndre OpptaltGavekortUtlevert ~
OpptaltTilgode OpptaltTilgodeAndre OpptaltTilgodeUtlevert OpptaltBilag ~
OpptaltVeksel OpptaltLevertBank PoseNr RegistrertAv RegistrertDato ~
RegistrertTid BrukerID EDato ETid OpptaltKupong 
&Scoped-Define MANDATORY-FIELDS  ButikkNr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dkassereroppgj.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH KassererOppgj NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH KassererOppgj NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main KassererOppgj
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main KassererOppgj


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Bilag dTables  _DB-REQUIRED
FUNCTION Bilag RETURNS LOGICAL
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD KlTid dTables  _DB-REQUIRED
FUNCTION KlTid RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Valorer dTables  _DB-REQUIRED
FUNCTION Valorer RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Valuta dTables  _DB-REQUIRED
FUNCTION Valuta RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      KassererOppgj SCROLLING.
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
     _TblList          = "skotex.KassererOppgj"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.KassererOppgj.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[2]   > skotex.KassererOppgj.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[3]   > skotex.KassererOppgj.KassererNr
"KassererNr" "KassererNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[4]   > skotex.KassererOppgj.z_nummer
"z_nummer" "z_nummer" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[5]   > skotex.KassererOppgj.OpptaltInnVeksel
"OpptaltInnVeksel" "OpptaltInnVeksel" ? ? "decimal" ? ? ? ? ? ? yes ? no 25.8 yes ""
     _FldNameList[6]   > skotex.KassererOppgj.OpptaltKontanter
"OpptaltKontanter" "OpptaltKontanter" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.6 yes ""
     _FldNameList[7]   > skotex.KassererOppgj.OpptaltSjekk
"OpptaltSjekk" "OpptaltSjekk" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.4 yes ""
     _FldNameList[8]   > skotex.KassererOppgj.OpptaltValuta
"OpptaltValuta" "OpptaltValuta" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[9]   > skotex.KassererOppgj.OpptaltReserve
"OpptaltReserve" "OpptaltReserve" ? ? "decimal" ? ? ? ? ? ? yes ? no 21 yes ""
     _FldNameList[10]   > skotex.KassererOppgj.OpptaltGavekort
"OpptaltGavekort" "OpptaltGavekort" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.8 yes ""
     _FldNameList[11]   > skotex.KassererOppgj.OpptaltGavekortAndre
"OpptaltGavekortAndre" "OpptaltGavekortAndre" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.8 yes ""
     _FldNameList[12]   > skotex.KassererOppgj.OpptaltGavekortUtlevert
"OpptaltGavekortUtlevert" "OpptaltGavekortUtlevert" ? ? "decimal" ? ? ? ? ? ? yes ? no 23.4 yes ""
     _FldNameList[13]   > skotex.KassererOppgj.OpptaltTilgode
"OpptaltTilgode" "OpptaltTilgode" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes ""
     _FldNameList[14]   > skotex.KassererOppgj.OpptaltTilgodeAndre
"OpptaltTilgodeAndre" "OpptaltTilgodeAndre" ? ? "decimal" ? ? ? ? ? ? yes ? no 19.6 yes ""
     _FldNameList[15]   > skotex.KassererOppgj.OpptaltTilgodeUtlevert
"OpptaltTilgodeUtlevert" "OpptaltTilgodeUtlevert" ? ? "decimal" ? ? ? ? ? ? yes ? no 21.2 yes ""
     _FldNameList[16]   > skotex.KassererOppgj.OpptaltBilag
"OpptaltBilag" "OpptaltBilag" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[17]   > skotex.KassererOppgj.OpptaltVeksel
"OpptaltVeksel" "OpptaltVeksel" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes ""
     _FldNameList[18]   > skotex.KassererOppgj.OpptaltLevertBank
"OpptaltLevertBank" "OpptaltLevertBank" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[19]   > skotex.KassererOppgj.PoseNr
"PoseNr" "PoseNr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[20]   > "_<CALC>"
"EndretInfo()" "fuEndretInfo" "Opprettet/endret" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[21]   > skotex.KassererOppgj.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[22]   > skotex.KassererOppgj.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[23]   > skotex.KassererOppgj.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[24]   > skotex.KassererOppgj.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[25]   > skotex.KassererOppgj.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[26]   > skotex.KassererOppgj.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[27]   > "_<CALC>"
"Valorer()" "fuValorer" "Valører" "yes/no" "Logical" ? ? ? ? ? ? no ? no 6.6 no ?
     _FldNameList[28]   > "_<CALC>"
"Bilag()" "fuBilag" "Bilag" "yes/no" "Logical" ? ? ? ? ? ? no ? no 4.6 no ?
     _FldNameList[29]   > "_<CALC>"
"Valuta()" "fuValuta" "Valuta" "yes/no" "Logical" ? ? ? ? ? ? no ? no 6 no ?
     _FldNameList[30]   > skotex.KassererOppgj.OpptaltKupong
"OpptaltKupong" "OpptaltKupong" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.6 yes ?
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
         rowObject.fuBilag = (Bilag())
         rowObject.fuEndretInfo = (EndretInfo())
         rowObject.fuValorer = (Valorer())
         rowObject.fuValuta = (Valuta())
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

  /* Initierer nye poster */
  FOR EACH RowObject WHERE
      CAN-DO("A,C,U",RowObject.RowMod):

      /* Oppretter bilag */
      IF NOT CAN-FIND(FIRST KassererBilag WHERE
                  KassererBilag.ButikkNr     = RowObject.ButikkNr AND
                  KassererBilag.Dato         = RowObject.Dato AND
                  KassererBilag.KassererNr   = RowObject.KassererNr AND
                  KassererBilag.Z_Nummer     = RowObject.Z_Nummer) THEN
      DO piLoop = 1 TO 20:
          CREATE KassererBilag.
          ASSIGN
              KassererBilag.ButikkNr     = RowObject.ButikkNr 
              KassererBilag.Dato         = RowObject.Dato 
              KassererBilag.KassererNr   = RowObject.KassererNr 
              KassererBilag.Z_Nummer     = RowObject.Z_Nummer
              KassererBilag.BilagsNr     = piLoop
              .
      END.
      
      /* Oppretter valører */
      IF NOT CAN-FIND(FIRST KassererKontanter WHERE
                  KassererKontanter.ButikkNr     = RowObject.ButikkNr AND
                  KassererKontanter.Dato         = RowObject.Dato AND
                  KassererKontanter.KassererNr   = RowObject.KassererNr AND
                  KassererKontanter.Z_Nummer     = RowObject.Z_Nummer) THEN
      DO:
          CREATE KassererKontanter.
          ASSIGN
              KassererKontanter.ButikkNr     = RowObject.ButikkNr 
              KassererKontanter.Dato         = RowObject.Dato 
              KassererKontanter.KassererNr   = RowObject.KassererNr 
              KassererKontanter.Z_Nummer     = RowObject.Z_Nummer
              .
      END.

      /* Oppretter valuta */
      IF NOT CAN-FIND(FIRST KassererValuta WHERE
                  KassererValuta.ButikkNr     = RowObject.ButikkNr AND
                  KassererValuta.Dato         = RowObject.Dato AND
                  KassererValuta.KassererNr   = RowObject.KassererNr AND
                  KassererValuta.Z_Nummer     = RowObject.Z_Nummer) THEN
      DO:
          FOR EACH KasValuta NO-LOCK WHERE
              KasValuta.ValAktiv = TRUE AND
              KasValuta.EgenValuta = FALSE AND
              KasValuta.KasseValkurs <> 0:
              CREATE KassererValuta.
              ASSIGN
                  /* Nøkkel */
                  KassererValuta.ButikkNr     = RowObject.ButikkNr 
                  KassererValuta.Dato         = RowObject.Dato 
                  KassererValuta.KassererNr   = RowObject.KassererNr 
                  KassererValuta.Z_Nummer     = RowObject.Z_Nummer
                  KassererValuta.ValKod       = KasValuta.ValKod
                  /* Resten */
                  KassererValuta.KasseValkurs = KasValuta.KasseValkurs / KasValuta.Indeks
                  KassererValuta.KasseValkurs = IF KassererValuta.KasseValkurs = ?
                                                  THEN 0
                                                  ELSE KassererValuta.KasseValkurs
                  .
          END.
      END.
  END.

  /* Sletter relaterte poster */
  FOR EACH RowObject WHERE
    CAN-DO("D",RowObject.RowMod):

      FOR EACH KassererBilag EXCLUSIVE-LOCK WHERE
          KassererBilag.ButikkNr     = RowObject.ButikkNr AND
          KassererBilag.Dato         = RowObject.Dato AND
          KassererBilag.KassererNr   = RowObject.KassererNr AND
          KassererBilag.Z_Nummer     = RowObject.Z_Nummer:
          DELETE KassererBilag.
      END.
      FOR EACH KassererKontanter EXCLUSIVE-LOCK WHERE
          KassererKontanter.ButikkNr     = RowObject.ButikkNr AND
          KassererKontanter.Dato         = RowObject.Dato AND
          KassererKontanter.KassererNr   = RowObject.KassererNr AND
          KassererKontanter.Z_Nummer     = RowObject.Z_Nummer:
          DELETE KassererKontanter.
      END.
      FOR EACH KassererValuta EXCLUSIVE-LOCK WHERE
          KassererValuta.ButikkNr     = RowObject.ButikkNr AND
          KassererValuta.Dato         = RowObject.Dato AND
          KassererValuta.KassererNr   = RowObject.KassererNr AND
          KassererValuta.Z_Nummer     = RowObject.Z_Nummer:
          DELETE KassererValuta.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInngVeksel dTables  _DB-REQUIRED
PROCEDURE GetInngVeksel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piButikkNr   AS INT NO-UNDO.
  DEF OUTPUT PARAMETER plInngVeksel AS DEC NO-UNDO.

  FIND Butiker NO-LOCK WHERE
      Butiker.Butik = piButikkNr NO-ERROR.
  IF AVAILABLE Butiker THEN
      ASSIGN
      plInngVeksel = Butiker.StdVeksel
      .
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
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,INPUT TRUE).

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

      /* Finnes fra før? */
      IF CAN-FIND(KassererOppgj WHERE
                  KassererOppgj.ButikkNr     = RowObject.ButikkNr AND
                  KassererOppgj.Dato         = RowObject.Dato AND
                  KassererOppgj.KassererNr   = RowObject.KassererNr AND
                  KassererOppgj.Z_Nummer     = RowObject.Z_Nummer) THEN
          RETURN "Kassereroppgjør for kasserer, butikk og dato er allerede opprettet.".
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
  IF NOT CAN-FIND(Butiker WHERE 
                  Butiker.Butik = RowObject.ButikkNr) THEN
      RETURN "Ukjent butikknummer.".
  IF RowObject.KassererNr > 0 AND NOT CAN-FIND(Forsalj WHERE
                  Forsalj.ForsNr = RowObject.KassererNr) THEN
  DO:
      /* Dagsoppgjør istedenfor kassaoppgjør */
      IF NOT CAN-FIND(Kasse WHERE
                      Kasse.ButikkNr = RowObject.ButikkNr AND
                      Kasse.Gruppe = 1 AND
                      Kasse.KasseNr = RowObject.KassererNr) THEN
      RETURN "Ukjent kasserer.".
  END.
  IF RowObject.Dato = ? THEN
      RETURN "Dato er ikke angitt.".
  IF RowObject.Dato > TODAY THEN
      RETURN "Ugyldig datoangivelse.".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransactionValidate dTables  _DB-REQUIRED
PROCEDURE TransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Kontroll ved sletting av poster. */
  FOR EACH RowObjUpd WHERE
      CAN-DO("D",RowObjUpd.RowMod):

      /* Sletter valuta */
      FOR EACH KassererValuta EXCLUSIVE-LOCK WHERE
          KassererValuta.ButikkNr   = RowOBjUpd.ButikkNr AND
          KassererValuta.Dato       = RowObjUpd.Dato AND
          KassererValuta.KassererNr = RowObjUpd.KassererNr AND
          KassererValuta.Z_Nummer   = RowObjUpd.Z_Nummer:
          DELETE KassererValuta.
      END.
      /* Sletter Bilag */
      FOR EACH KassererValuta EXCLUSIVE-LOCK WHERE
          KassererBilag.ButikkNr   = RowOBjUpd.ButikkNr AND
          KassererBilag.Dato       = RowObjUpd.Dato AND
          KassererBilag.KassererNr = RowObjUpd.KassererNr AND
          KassererBilag.Z_Nummer   = RowObjUpd.Z_Nummer:
          DELETE KassererBilag.
      END.
      /* Sletter kontanter/valører */
      FOR EACH KassererKontanter EXCLUSIVE-LOCK WHERE
          KassererKontanter.ButikkNr   = RowOBjUpd.ButikkNr AND
          KassererKontanter.Dato       = RowObjUpd.Dato AND
          KassererKontanter.KassererNr = RowObjUpd.KassererNr AND
          KassererKontanter.Z_Nummer   = RowObjUpd.Z_Nummer:
          DELETE KassererKontanter.
      END.

  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Bilag dTables  _DB-REQUIRED
FUNCTION Bilag RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pbOk AS LOG NO-UNDO.

  IF CAN-FIND(FIRST KassererBilag WHERE
          KassererBilag.ButikkNr   = RowOBjUpd.ButikkNr AND
          KassererBilag.Dato       = RowObjUpd.Dato AND
          KassererBilag.KassererNr = RowObjUpd.KassererNr AND
          KassererBilag.Z_Nummer   = RowObjUpd.Z_Nummer) THEN
      pbOk = TRUE.
  ELSE
      pbOk = FALSE.

  RETURN pbOk.   /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION KlTid dTables  _DB-REQUIRED
FUNCTION KlTid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.ETid,"HH:MM:SS").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Valorer dTables  _DB-REQUIRED
FUNCTION Valorer RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pbOk AS LOG NO-UNDO.

  IF CAN-FIND(FIRST KassererKontanter WHERE
          KassererKontanter.ButikkNr   = RowOBjUpd.ButikkNr AND
          KassererKontanter.Dato       = RowObjUpd.Dato AND
          KassererKontanter.KassererNr = RowObjUpd.KassererNr AND
          KassererKontanter.Z_Nummer   = RowObjUpd.Z_Nummer) THEN
      pbOk = TRUE.
  ELSE
      pbOk = FALSE.

  RETURN pbOk.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Valuta dTables  _DB-REQUIRED
FUNCTION Valuta RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pbOk AS LOG NO-UNDO.

  IF CAN-FIND(FIRST KassererValuta WHERE
          KassererValuta.ButikkNr   = RowOBjUpd.ButikkNr AND
          KassererValuta.Dato       = RowObjUpd.Dato AND
          KassererValuta.KassererNr = RowObjUpd.KassererNr AND
          KassererValuta.Z_Nummer   = RowObjUpd.Z_Nummer) THEN
      pbOk = TRUE.
  ELSE
      pbOk = FALSE.

  RETURN pbOk.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

