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
&Scoped-define INTERNAL-TABLES EkstVPILev

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  EkstVPILevNr KortNavn Navn EDato ETid BrukerID RegistrertAv RegistrertDato~
 RegistrertTid AktivLev OppdatMaskeVPI PrioNr LevNr EDB-System
&Scoped-define ENABLED-FIELDS-IN-EkstVPILev EkstVPILevNr KortNavn Navn ~
EDato ETid BrukerID RegistrertAv RegistrertDato RegistrertTid AktivLev ~
OppdatMaskeVPI PrioNr LevNr EDB-System 
&Scoped-Define DATA-FIELDS  EkstVPILevNr KortNavn Navn EDato ETid BrukerID RegistrertAv RegistrertDato~
 RegistrertTid fuEndretInfo AktivLev OppdatMaskeVPI PrioNr LevNr EDB-System
&Scoped-define DATA-FIELDS-IN-EkstVPILev EkstVPILevNr KortNavn Navn EDato ~
ETid BrukerID RegistrertAv RegistrertDato RegistrertTid AktivLev ~
OppdatMaskeVPI PrioNr LevNr EDB-System 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dekstvpilev.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH EkstVPILev NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH EkstVPILev NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main EkstVPILev
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main EkstVPILev


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EndretInfo dTables  _DB-REQUIRED
FUNCTION EndretInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      EkstVPILev SCROLLING.
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
     _TblList          = "SkoTex.EkstVPILev"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > SkoTex.EkstVPILev.EkstVPILevNr
"EkstVPILevNr" "EkstVPILevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes ""
     _FldNameList[2]   > SkoTex.EkstVPILev.KortNavn
"KortNavn" "KortNavn" ? "X(15)" "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[3]   > SkoTex.EkstVPILev.Navn
"Navn" "Navn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[4]   > SkoTex.EkstVPILev.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[5]   > SkoTex.EkstVPILev.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[6]   > SkoTex.EkstVPILev.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[7]   > SkoTex.EkstVPILev.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[8]   > SkoTex.EkstVPILev.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[9]   > SkoTex.EkstVPILev.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[10]   > "_<CALC>"
"EndretInfo()" "fuEndretInfo" "EndretInfo" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[11]   > SkoTex.EkstVPILev.AktivLev
"AktivLev" "AktivLev" ? ? "logical" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[12]   > SkoTex.EkstVPILev.OppdatMaskeVPI
"OppdatMaskeVPI" "OppdatMaskeVPI" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[13]   > SkoTex.EkstVPILev.PrioNr
"PrioNr" "PrioNr" ? ? "integer" ? ? ? ? ? ? yes ? no 3.6 yes ""
     _FldNameList[14]   > SkoTex.EkstVPILev.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[15]   > SkoTex.EkstVPILev.EDB-System
"EDB-System" "EDB-System" ? ? "character" ? ? ? ? ? ? yes ? no 12 yes ?
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
         rowObject.fuEndretInfo = (EndretInfo())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetHarModell dTables  _DB-REQUIRED
PROCEDURE GetHarModell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pbT-Modell AS LOG NO-UNDO.

  DEF VAR pcolValues     AS CHAR NO-UNDO.
  DEF VAR pcTekst        AS CHAR NO-UNDO.
  DEF VAR piEkstVPILevNr AS INT NO-UNDO.

  ASSIGN
      piEkstVPILevNr = INT(entry(2,DYNAMIC-FUNCTION('colValues':U,
                       INPUT "EkstVPILevNr"),CHR(1)))
      .
  /* Leverandører som det ikke er lagt opp parameter for, har slik håndtering. */
  /* Lev. som har "nei, no, false , 0" har ikke.                               */
  {syspara.i 1 13 piEkstVPILevNr pcTekst}
  IF can-do("1,Nei,no,false,N", pcTekst) THEN
      pbT-Modell = FALSE.
  ELSE 
      pbT-Modell = TRUE.
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
      IF CAN-FIND(EkstVPILev WHERE
                  EkstVPILev.EkstVPILevNr = RowObject.EkstVPILevNr) THEN
          RETURN "Eksternt EDB system " + string(RowObject.EkstVPILevNr) +  " er allerede registrert.".
  END.
  /* Kontroll ved sletting av poster. */
  FOR EACH RowObjUpd WHERE
      CAN-DO("D",RowObjUpd.RowMod):

      /* Sjekker om det finnes betalingstransaksjoner */
      IF CAN-FIND(FIRST EkstVPIFil WHERE
                  EkstVPIFil.EkstVPILevNr = RowObjUpd.EkstVPILevNr) THEN
          RETURN "Det finnes fildefinisjoner registrert på dette EDB systemet. " +
                 "Posten kan ikke slettes.".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

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

