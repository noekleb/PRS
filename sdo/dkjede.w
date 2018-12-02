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
&Scoped-define INTERNAL-TABLES Kjede

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BrukerID EDato ETid KjedeKNavn KjedeNavn KjedeNr RegistrertAv~
 RegistrertDato RegistrertTid
&Scoped-define ENABLED-FIELDS-IN-Kjede BrukerID EDato ETid KjedeKNavn ~
KjedeNavn KjedeNr RegistrertAv RegistrertDato RegistrertTid 
&Scoped-Define DATA-FIELDS  BrukerID EDato ETid KjedeKNavn KjedeNavn KjedeNr RegistrertAv~
 RegistrertDato RegistrertTid
&Scoped-define DATA-FIELDS-IN-Kjede BrukerID EDato ETid KjedeKNavn ~
KjedeNavn KjedeNr RegistrertAv RegistrertDato RegistrertTid 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dkjede.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Kjede NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Kjede NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Kjede
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Kjede


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Kjede SCROLLING.
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
     _TblList          = "skotex.Kjede"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.Kjede.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[2]   > skotex.Kjede.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[3]   > skotex.Kjede.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[4]   > skotex.Kjede.KjedeKNavn
"KjedeKNavn" "KjedeKNavn" ? ? "character" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[5]   > skotex.Kjede.KjedeNavn
"KjedeNavn" "KjedeNavn" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[6]   > skotex.Kjede.KjedeNr
"KjedeNr" "KjedeNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[7]   > skotex.Kjede.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[8]   > skotex.Kjede.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[9]   > skotex.Kjede.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bKjede FOR Kjede.
    DEFINE VARIABLE iKjedeNr AS INTEGER    NO-UNDO.
    FIND LAST bKjede NO-LOCK NO-ERROR.
    ASSIGN iKjedeNr = IF AVAIL bKjede THEN bKjede.KjedeNr  + 1 ELSE 1.
    FOR EACH RowObjUpd WHERE CAN-DO("A,C",RowObjUpd.RowMod):
        IF CAN-FIND(FIRST Kjede WHERE Kjede.KjedeKNavn = RowObjUpd.KjedeKNavn) THEN
            RETURN "Det finnes en kjede med kortnavn: " + RowObjUpd.KjedeKNavn.
        IF CAN-FIND(FIRST Kjede WHERE Kjede.KjedeNavn = RowObjUpd.KjedeNavn) THEN
            RETURN "Det finnes en kjede med navn: " + RowObjUpd.KjedeNavn.
        ASSIGN RowObjUpd.KjedeNr = iKjedeNr
               iKjedeNr          = iKjedeNr + 1.
    END.  
    FOR EACH RowObjUpd WHERE CAN-DO("U",RowObjUpd.RowMod):
        IF CAN-FIND(FIRST Kjede WHERE Kjede.KjedeNr <> RowObjUpd.Kjedenr AND Kjede.KjedeKNavn = RowObjUpd.KjedeKNavn) THEN
            RETURN "Det finnes en kjede med kortnavn: " + RowObjUpd.KjedeKNavn.
        IF CAN-FIND(FIRST Kjede WHERE Kjede.KjedeNr <> RowObjUpd.Kjedenr AND Kjede.KjedeNavn = RowObjUpd.KjedeNavn) THEN
            RETURN "Det finnes en kjede med navn: " + RowObjUpd.KjedeNavn.
    END.  
    FOR EACH RowObjUpd WHERE CAN-DO("D",RowObjUpd.RowMod):
        IF CAN-FIND(FIRST KjedeRegion WHERE KjedeRegion.KjedeNr = RowObjUpd.KjedeNr) THEN
            RETURN "Det finnes region(er) koblet til denne kjede. Kann ikke slettes.".
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

