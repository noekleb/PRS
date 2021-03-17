&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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
DEFINE VARIABLE cKatalog AS CHARACTER  NO-UNDO.

{syspara.i 10 1 2 cKatalog}

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
&Scoped-define INTERNAL-TABLES VPIBilderegister

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BildNr BrukerID Bytes Dato DokumentNr EDato EksterntID EkstVPILevNr ETid~
 FilNavn LevArtNr LevNr Merknad Notat RegistrertAv RegistrertDato~
 RegistrertTid Sted Tekst Tid VareNr
&Scoped-define ENABLED-FIELDS-IN-VPIBilderegister BildNr BrukerID Bytes ~
Dato DokumentNr EDato EksterntID EkstVPILevNr ETid FilNavn LevArtNr LevNr ~
Merknad Notat RegistrertAv RegistrertDato RegistrertTid Sted Tekst Tid ~
VareNr 
&Scoped-Define DATA-FIELDS  BildNr BrukerID Bytes Dato DokumentNr EDato EksterntID EkstVPILevNr ETid~
 FilNavn LevArtNr LevNr Merknad Notat RegistrertAv RegistrertDato Bildefil~
 RegistrertTid Sted Tekst Tid VareNr
&Scoped-define DATA-FIELDS-IN-VPIBilderegister BildNr BrukerID Bytes Dato ~
DokumentNr EDato EksterntID EkstVPILevNr ETid FilNavn LevArtNr LevNr ~
Merknad Notat RegistrertAv RegistrertDato RegistrertTid Sted Tekst Tid ~
VareNr 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dvpibilderegister.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH VPIBilderegister NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH VPIBilderegister NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main VPIBilderegister
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main VPIBilderegister


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildeDiskNavn dTables  _DB-REQUIRED
FUNCTION getBildeDiskNavn RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildNr dTables  _DB-REQUIRED
FUNCTION getBildNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKatalog dTables  _DB-REQUIRED
FUNCTION getKatalog RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      VPIBilderegister SCROLLING.
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
     _TblList          = "vpi.VPIBilderegister"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > vpi.VPIBilderegister.BildNr
"BildNr" "BildNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8.4 yes ""
     _FldNameList[2]   > vpi.VPIBilderegister.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[3]   > vpi.VPIBilderegister.Bytes
"Bytes" "Bytes" ? ? "integer" ? ? ? ? ? ? yes ? no 6.8 yes ""
     _FldNameList[4]   > vpi.VPIBilderegister.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[5]   > vpi.VPIBilderegister.DokumentNr
"DokumentNr" "DokumentNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[6]   > vpi.VPIBilderegister.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[7]   > vpi.VPIBilderegister.EksterntID
"EksterntID" "EksterntID" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[8]   > vpi.VPIBilderegister.EkstVPILevNr
"EkstVPILevNr" "EkstVPILevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[9]   > vpi.VPIBilderegister.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[10]   > vpi.VPIBilderegister.FilNavn
"FilNavn" "FilNavn" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[11]   > vpi.VPIBilderegister.LevArtNr
"LevArtNr" "LevArtNr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[12]   > vpi.VPIBilderegister.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[13]   > vpi.VPIBilderegister.Merknad
"Merknad" "Merknad" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[14]   > vpi.VPIBilderegister.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[15]   > vpi.VPIBilderegister.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[16]   > vpi.VPIBilderegister.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[17]   > "_<CALC>"
"getBildeDiskNavn()" "Bildefil" ? "x(20)" "character" ? ? ? ? ? ? no ? no 20 no ?
     _FldNameList[18]   > vpi.VPIBilderegister.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[19]   > vpi.VPIBilderegister.Sted
"Sted" "Sted" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[20]   > vpi.VPIBilderegister.Tekst
"Tekst" "Tekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[21]   > vpi.VPIBilderegister.Tid
"Tid" "Tid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[22]   > vpi.VPIBilderegister.VareNr
"VareNr" "VareNr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
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
         rowObject.Bildefil = (getBildeDiskNavn())
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable dTables  _DB-REQUIRED
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcRelative AS CHARACTER NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN SUPER( INPUT pcRelative).
  
  /* Code placed here will execute AFTER standard behavior.    */

      PUBLISH "VisBilde" (ENTRY(2,DYNAMIC-FUNCTION('colValues':U,
          INPUT "Bildefil" /* CHARACTER */),CHR(1))).

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

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildeDiskNavn dTables  _DB-REQUIRED
FUNCTION getBildeDiskNavn RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF RowObject.BildNr = 0 THEN "" ELSE cKatalog + "\" + 
      (IF CAN-FIND(FIRST vpibildedata OF vpibilderegister WHERE vpibildedata.teller >= 200) THEN 
          "mini" ELSE "") + RowObject.FilNavn.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildNr dTables  _DB-REQUIRED
FUNCTION getBildNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN ENTRY(2,DYNAMIC-FUNCTION('colValues':U,
       INPUT "BildNr" /* CHARACTER */),CHR(1)).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKatalog dTables  _DB-REQUIRED
FUNCTION getKatalog RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN cKatalog.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

