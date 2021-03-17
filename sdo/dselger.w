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
&Scoped-define INTERNAL-TABLES Selger

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  AnsattNr BrukerID EDato ETid Navn RegistrertAv RegistrertDato RegistrertTid~
 SelgerNr Adresse1 Adresse2 Mobiltelefon PersonNr PostNr Telefon NavnIKasse~
 ButikkNr AnsattDato ArbeidsProsent BrukeridPRS FastLonn FodtDato ForNavn~
 JobTittel LonnProfil SluttetDato TimeLonn deciPWD
&Scoped-define ENABLED-FIELDS-IN-Selger AnsattNr BrukerID EDato ETid Navn ~
RegistrertAv RegistrertDato RegistrertTid SelgerNr Adresse1 Adresse2 ~
Mobiltelefon PersonNr PostNr Telefon NavnIKasse ButikkNr AnsattDato ~
ArbeidsProsent BrukeridPRS FastLonn FodtDato ForNavn JobTittel LonnProfil ~
SluttetDato TimeLonn deciPWD 
&Scoped-Define DATA-FIELDS  AnsattNr fuDataObjekt BrukerID EDato fuPostSted ETid Navn fuEndretInfo~
 RegistrertAv RegistrertDato RegistrertTid SelgerNr Adresse1 Adresse2~
 Mobiltelefon PersonNr PostNr Telefon NavnIKasse ButikkNr AnsattDato~
 ArbeidsProsent BrukeridPRS FastLonn FodtDato ForNavn JobTittel LonnProfil~
 SluttetDato TimeLonn deciPWD
&Scoped-define DATA-FIELDS-IN-Selger AnsattNr BrukerID EDato ETid Navn ~
RegistrertAv RegistrertDato RegistrertTid SelgerNr Adresse1 Adresse2 ~
Mobiltelefon PersonNr PostNr Telefon NavnIKasse ButikkNr AnsattDato ~
ArbeidsProsent BrukeridPRS FastLonn FodtDato ForNavn JobTittel LonnProfil ~
SluttetDato TimeLonn deciPWD 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dselger.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Selger NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Selger NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Selger
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Selger


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DataObjekt dTables  _DB-REQUIRED
FUNCTION DataObjekt RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Poststed dTables  _DB-REQUIRED
FUNCTION Poststed RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UnikButik dTables  _DB-REQUIRED
FUNCTION UnikButik RETURNS LOGICAL
  ( OUTPUT iButik AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Selger SCROLLING.
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
     _TblList          = "skotex.Selger"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.Selger.AnsattNr
"AnsattNr" "AnsattNr" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[2]   > "_<CALC>"
"DataObjekt()" "fuDataObjekt" "DataObjekt" "x(13)" "character" ? ? ? ? ? ? no ? no 13 no ?
     _FldNameList[3]   > skotex.Selger.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[4]   > skotex.Selger.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[5]   > "_<CALC>"
"PostSted()" "fuPostSted" "Poststed" "x(30)" "character" ? ? ? ? ? ? no ? no 30 no ?
     _FldNameList[6]   > skotex.Selger.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[7]   > skotex.Selger.Navn
"Navn" "Navn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[8]   > "_<CALC>"
"EndretInfo()" "fuEndretInfo" "Endret info" "x(60)" "character" ? ? ? ? ? ? no ? no 60 no ?
     _FldNameList[9]   > skotex.Selger.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[10]   > skotex.Selger.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[11]   > skotex.Selger.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[12]   > skotex.Selger.SelgerNr
"SelgerNr" "SelgerNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[13]   > skotex.Selger.Adresse1
"Adresse1" "Adresse1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[14]   > skotex.Selger.Adresse2
"Adresse2" "Adresse2" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[15]   > skotex.Selger.Mobiltelefon
"Mobiltelefon" "Mobiltelefon" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[16]   > skotex.Selger.PersonNr
"PersonNr" "PersonNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[17]   > skotex.Selger.PostNr
"PostNr" "PostNr" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[18]   > skotex.Selger.Telefon
"Telefon" "Telefon" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[19]   > skotex.Selger.NavnIKasse
"NavnIKasse" "NavnIKasse" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[20]   > skotex.Selger.ButikkNr
"ButikkNr" "ButikkNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[21]   > skotex.Selger.AnsattDato
"AnsattDato" "AnsattDato" ? ? "date" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[22]   > skotex.Selger.ArbeidsProsent
"ArbeidsProsent" "ArbeidsProsent" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.2 yes ?
     _FldNameList[23]   > skotex.Selger.BrukeridPRS
"BrukeridPRS" "BrukeridPRS" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ?
     _FldNameList[24]   > skotex.Selger.FastLonn
"FastLonn" "FastLonn" ? ? "decimal" ? ? ? ? ? ? yes ? no 16 yes ?
     _FldNameList[25]   > skotex.Selger.FodtDato
"FodtDato" "FodtDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[26]   > skotex.Selger.ForNavn
"ForNavn" "ForNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[27]   > skotex.Selger.JobTittel
"JobTittel" "JobTittel" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[28]   > skotex.Selger.LonnProfil
"LonnProfil" "LonnProfil" ? ? "character" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[29]   > skotex.Selger.SluttetDato
"SluttetDato" "SluttetDato" ? ? "date" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[30]   > skotex.Selger.TimeLonn
"TimeLonn" "TimeLonn" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[31]   > skotex.Selger.deciPWD
"deciPWD" "deciPWD" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.8 yes ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BeginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE BeginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Sletter relaterte poster. */
  FOR EACH RowObjUpd WHERE
      CAN-DO("D",RowObjUpd.RowMod):

      /* Sletter statistikk */
      STAT:
      FOR EACH StLinje exclusive-lock where
        StLinje.StTypeId   = "SELGER" and
        StLinje.DataObjekt = string(TransLogg.SelgerNr,"9999999999999"):
        DELETE StLinje.
      END. /* STAT */

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
         rowObject.fuDataObjekt = (DataObjekt())
         rowObject.fuEndretInfo = (EndretInfo())
         rowObject.fuPostSted = (PostSted())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreTransactionValidate dTables  _DB-REQUIRED
PROCEDURE PreTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcDbId     AS CHAR NO-UNDO.
  DEF VAR pdSelgerNr AS DEC  NO-UNDO.

  {syspara.i 1 1 14 pcDbId}

  /*
  ASSIGN
      pdSelgerNr = DEC(pcDbId + "0000000")
      .
  */

  DEF BUFFER bSelger FOR Selger.

  /* Kontroller ved ny/kopier post */
  FOR EACH RowObject WHERE
      CAN-DO("A,C,U",RowObject.RowMod):

      IF NOT CAN-FIND(Post WHERE
                      Post.PostNr = RowObject.PostNr) THEN
          RETURN "Ukjent postnummer.".

  END.

  /* Kontroller ved ny/kopier post */
  FOR EACH RowObjUpd WHERE
      CAN-DO("A,C",RowObjUpd.RowMod):
      /*
      FIND LAST bSelger NO-LOCK WHERE 
          bSelger.SelgerNr >= pdSelgerNr NO-ERROR.
      IF AVAILABLE bSelger THEN
          RowObjUpd.SelgerNr = bSelger.SelgerNr + 1.
      ELSE
          RowObjUpd.SelgerNr = pdSelgerNr + 1.
     */
  END.
  /* Kontroll ved sletting av poster. */
  FOR EACH RowObjUpd WHERE
      CAN-DO("D",RowObjUpd.RowMod):
      /* Kan ikke slettes hvis koblet til kasse. */
      IF CAN-FIND(FIRST ButikkSelger where
                  ButikkSelger.SelgerNr = RowObjUpd.SelgerNr) THEN
          RETURN "Selgeren er koblet til en eller flere butikker. " +
                 "Selgere som er koblet, kan ikke slettes. Koblingene må tas bort først.".


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
      RETURN Post.Beskrivelse.
  ELSE
      RETURN "AVBRYT".   /* Function return value. */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DataObjekt dTables  _DB-REQUIRED
FUNCTION DataObjekt RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN string(RowObject.SelgerNr,"9999999999999"). /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Poststed dTables  _DB-REQUIRED
FUNCTION Poststed RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  FIND Post NO-LOCK WHERE
      Post.PostNr = RowObject.PostNr NO-ERROR.
  IF AVAILABLE Post 
      THEN pcTekst = Post.Beskrivelse.
  ELSE
      pcTekst = "".

  RETURN pcTekst.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UnikButik dTables  _DB-REQUIRED
FUNCTION UnikButik RETURNS LOGICAL
  ( OUTPUT iButik AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /* Om Butiker är AVAIL betyder det att vi bara har en butik i basen */
  FIND Butiker NO-LOCK NO-ERROR.
  IF AVAIL Butiker THEN
      ASSIGN iButik = Butiker.Butik.
  RETURN iButik > 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

