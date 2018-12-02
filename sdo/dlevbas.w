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
&Scoped-define INTERNAL-TABLES LevBas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  betant BildNr BrukerID EDato ETid koadr koland kommentar1 kommentar2~
 kommentar3 kommentar4 kopadr koponr kotel kotelefax kotelex levadr levkon~
 levland levnamn levnr levpadr levponr levsal levtel Lng Notat RegistrertAv~
 RegistrertDato RegistrertTid telefax telex valkod VisDivInfo1 VisDivInfo2~
 VisDivInfo3 VisDivInfo4 VisDivInfo5 VisDivInfo6 VisDivInfo7 VisDivInfo8~
 VisDivInfo9 VisDivInfo10 VisDivInfo11 VisDivInfo12 VisDivInfo13~
 VisDivInfo14 VisDivInfo15 VisDivInfo16 VisDivInfo17 VisDivInfo18~
 VisDivInfo19 VisDivInfo20 E_MailKontakt E_MailLev KjedeAvtale ReklAdresse1~
 ReklAdresse2 ReklPostBoks ReklPostNr
&Scoped-define ENABLED-FIELDS-IN-LevBas betant BildNr BrukerID EDato ETid ~
koadr koland kommentar1 kommentar2 kommentar3 kommentar4 kopadr koponr ~
kotel kotelefax kotelex levadr levkon levland levnamn levnr levpadr levponr ~
levsal levtel Lng Notat RegistrertAv RegistrertDato RegistrertTid telefax ~
telex valkod VisDivInfo1 VisDivInfo2 VisDivInfo3 VisDivInfo4 VisDivInfo5 ~
VisDivInfo6 VisDivInfo7 VisDivInfo8 VisDivInfo9 VisDivInfo10 VisDivInfo11 ~
VisDivInfo12 VisDivInfo13 VisDivInfo14 VisDivInfo15 VisDivInfo16 ~
VisDivInfo17 VisDivInfo18 VisDivInfo19 VisDivInfo20 E_MailKontakt E_MailLev ~
KjedeAvtale ReklAdresse1 ReklAdresse2 ReklPostBoks ReklPostNr 
&Scoped-Define DATA-FIELDS  betant BildNr BrukerID EDato ETid koadr koland kommentar1 kommentar2~
 kommentar3 kommentar4 kopadr koponr kotel kotelefax kotelex levadr levkon~
 levland levnamn levnr levpadr levponr levsal levtel Lng Notat RegistrertAv~
 RegistrertDato RegistrertTid telefax telex valkod VisDivInfo1 VisDivInfo2~
 VisDivInfo3 VisDivInfo4 VisDivInfo5 VisDivInfo6 VisDivInfo7 VisDivInfo8~
 VisDivInfo9 VisDivInfo10 VisDivInfo11 VisDivInfo12 VisDivInfo13~
 VisDivInfo14 VisDivInfo15 VisDivInfo16 VisDivInfo17 VisDivInfo18~
 VisDivInfo19 VisDivInfo20 E_MailKontakt E_MailLev KjedeAvtale ReklAdresse1~
 ReklAdresse2 ReklPostBoks ReklPostNr
&Scoped-define DATA-FIELDS-IN-LevBas betant BildNr BrukerID EDato ETid ~
koadr koland kommentar1 kommentar2 kommentar3 kommentar4 kopadr koponr ~
kotel kotelefax kotelex levadr levkon levland levnamn levnr levpadr levponr ~
levsal levtel Lng Notat RegistrertAv RegistrertDato RegistrertTid telefax ~
telex valkod VisDivInfo1 VisDivInfo2 VisDivInfo3 VisDivInfo4 VisDivInfo5 ~
VisDivInfo6 VisDivInfo7 VisDivInfo8 VisDivInfo9 VisDivInfo10 VisDivInfo11 ~
VisDivInfo12 VisDivInfo13 VisDivInfo14 VisDivInfo15 VisDivInfo16 ~
VisDivInfo17 VisDivInfo18 VisDivInfo19 VisDivInfo20 E_MailKontakt E_MailLev ~
KjedeAvtale ReklAdresse1 ReklAdresse2 ReklPostBoks ReklPostNr 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.kommentar1 = LevBas.kommentar[1]~
  rowObject.kommentar2 = LevBas.kommentar[2]~
  rowObject.kommentar3 = LevBas.kommentar[3]~
  rowObject.kommentar4 = LevBas.kommentar[4]~
  rowObject.VisDivInfo1 = LevBas.VisDivInfo[1]~
  rowObject.VisDivInfo2 = LevBas.VisDivInfo[2]~
  rowObject.VisDivInfo3 = LevBas.VisDivInfo[3]~
  rowObject.VisDivInfo4 = LevBas.VisDivInfo[4]~
  rowObject.VisDivInfo5 = LevBas.VisDivInfo[5]~
  rowObject.VisDivInfo6 = LevBas.VisDivInfo[6]~
  rowObject.VisDivInfo7 = LevBas.VisDivInfo[7]~
  rowObject.VisDivInfo8 = LevBas.VisDivInfo[8]~
  rowObject.VisDivInfo9 = LevBas.VisDivInfo[9]~
  rowObject.VisDivInfo10 = LevBas.VisDivInfo[10]~
  rowObject.VisDivInfo11 = LevBas.VisDivInfo[11]~
  rowObject.VisDivInfo12 = LevBas.VisDivInfo[12]~
  rowObject.VisDivInfo13 = LevBas.VisDivInfo[13]~
  rowObject.VisDivInfo14 = LevBas.VisDivInfo[14]~
  rowObject.VisDivInfo15 = LevBas.VisDivInfo[15]~
  rowObject.VisDivInfo16 = LevBas.VisDivInfo[16]~
  rowObject.VisDivInfo17 = LevBas.VisDivInfo[17]~
  rowObject.VisDivInfo18 = LevBas.VisDivInfo[18]~
  rowObject.VisDivInfo19 = LevBas.VisDivInfo[19]~
  rowObject.VisDivInfo20 = LevBas.VisDivInfo[20]
&Scoped-Define DATA-FIELD-DEFS "dlevbas.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH LevBas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH LevBas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main LevBas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main LevBas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      LevBas SCROLLING.
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
     _TblList          = "skotex.LevBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.LevBas.betant
"betant" "betant" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[2]   > skotex.LevBas.BildNr
"BildNr" "BildNr" ? ? "integer" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[3]   > skotex.LevBas.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[4]   > skotex.LevBas.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[5]   > skotex.LevBas.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[6]   > skotex.LevBas.koadr
"koadr" "koadr" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[7]   > skotex.LevBas.koland
"koland" "koland" ? ? "character" ? ? ? ? ? ? yes ? no 15.2 yes ""
     _FldNameList[8]   > skotex.LevBas.kommentar[1]
"kommentar[1]" "kommentar1" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[9]   > skotex.LevBas.kommentar[2]
"kommentar[2]" "kommentar2" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[10]   > skotex.LevBas.kommentar[3]
"kommentar[3]" "kommentar3" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[11]   > skotex.LevBas.kommentar[4]
"kommentar[4]" "kommentar4" ? ? "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[12]   > skotex.LevBas.kopadr
"kopadr" "kopadr" ? ? "character" ? ? ? ? ? ? yes ? no 21.4 yes ""
     _FldNameList[13]   > skotex.LevBas.koponr
"koponr" "koponr" ? ? "character" ? ? ? ? ? ? yes ? no 22.6 yes ""
     _FldNameList[14]   > skotex.LevBas.kotel
"kotel" "kotel" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ""
     _FldNameList[15]   > skotex.LevBas.kotelefax
"kotelefax" "kotelefax" ? ? "character" ? ? ? ? ? ? yes ? no 17.4 yes ""
     _FldNameList[16]   > skotex.LevBas.kotelex
"kotelex" "kotelex" ? ? "character" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[17]   > skotex.LevBas.levadr
"levadr" "levadr" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[18]   > skotex.LevBas.levkon
"levkon" "levkon" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[19]   > skotex.LevBas.levland
"levland" "levland" ? ? "character" ? ? ? ? ? ? yes ? no 16.6 yes ""
     _FldNameList[20]   > skotex.LevBas.levnamn
"levnamn" "levnamn" ? ? "character" ? ? ? ? ? ? yes ? no 28.6 yes ""
     _FldNameList[21]   > skotex.LevBas.levnr
"levnr" "levnr" ? ">>>>>9" "integer" ? ? ? ? ? ? yes ? no 17 yes ""
     _FldNameList[22]   > skotex.LevBas.levpadr
"levpadr" "levpadr" ? ? "character" ? ? ? ? ? ? yes ? no 22.8 yes ""
     _FldNameList[23]   > skotex.LevBas.levponr
"levponr" "levponr" ? ? "character" ? ? ? ? ? ? yes ? no 24 yes ""
     _FldNameList[24]   > skotex.LevBas.levsal
"levsal" "levsal" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[25]   > skotex.LevBas.levtel
"levtel" "levtel" ? ? "character" ? ? ? ? ? ? yes ? no 26.4 yes ""
     _FldNameList[26]   > skotex.LevBas.Lng
"Lng" "Lng" ? ? "character" ? ? ? ? ? ? yes ? no 10.4 yes ""
     _FldNameList[27]   > skotex.LevBas.Notat
"Notat" "Notat" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[28]   > skotex.LevBas.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[29]   > skotex.LevBas.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[30]   > skotex.LevBas.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[31]   > skotex.LevBas.telefax
"telefax" "telefax" ? ? "character" ? ? ? ? ? ? yes ? no 18.8 yes ""
     _FldNameList[32]   > skotex.LevBas.telex
"telex" "telex" ? ? "character" ? ? ? ? ? ? yes ? no 17 yes ""
     _FldNameList[33]   > skotex.LevBas.valkod
"valkod" "valkod" ? ? "character" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[34]   > skotex.LevBas.VisDivInfo[1]
"VisDivInfo[1]" "VisDivInfo1" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[35]   > skotex.LevBas.VisDivInfo[2]
"VisDivInfo[2]" "VisDivInfo2" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[36]   > skotex.LevBas.VisDivInfo[3]
"VisDivInfo[3]" "VisDivInfo3" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[37]   > skotex.LevBas.VisDivInfo[4]
"VisDivInfo[4]" "VisDivInfo4" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[38]   > skotex.LevBas.VisDivInfo[5]
"VisDivInfo[5]" "VisDivInfo5" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[39]   > skotex.LevBas.VisDivInfo[6]
"VisDivInfo[6]" "VisDivInfo6" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[40]   > skotex.LevBas.VisDivInfo[7]
"VisDivInfo[7]" "VisDivInfo7" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[41]   > skotex.LevBas.VisDivInfo[8]
"VisDivInfo[8]" "VisDivInfo8" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[42]   > skotex.LevBas.VisDivInfo[9]
"VisDivInfo[9]" "VisDivInfo9" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[43]   > skotex.LevBas.VisDivInfo[10]
"VisDivInfo[10]" "VisDivInfo10" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[44]   > skotex.LevBas.VisDivInfo[11]
"VisDivInfo[11]" "VisDivInfo11" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[45]   > skotex.LevBas.VisDivInfo[12]
"VisDivInfo[12]" "VisDivInfo12" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[46]   > skotex.LevBas.VisDivInfo[13]
"VisDivInfo[13]" "VisDivInfo13" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[47]   > skotex.LevBas.VisDivInfo[14]
"VisDivInfo[14]" "VisDivInfo14" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[48]   > skotex.LevBas.VisDivInfo[15]
"VisDivInfo[15]" "VisDivInfo15" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[49]   > skotex.LevBas.VisDivInfo[16]
"VisDivInfo[16]" "VisDivInfo16" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[50]   > skotex.LevBas.VisDivInfo[17]
"VisDivInfo[17]" "VisDivInfo17" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[51]   > skotex.LevBas.VisDivInfo[18]
"VisDivInfo[18]" "VisDivInfo18" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[52]   > skotex.LevBas.VisDivInfo[19]
"VisDivInfo[19]" "VisDivInfo19" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[53]   > skotex.LevBas.VisDivInfo[20]
"VisDivInfo[20]" "VisDivInfo20" ? ? "logical" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[54]   > skotex.LevBas.E_MailKontakt
"E_MailKontakt" "E_MailKontakt" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[55]   > skotex.LevBas.E_MailLev
"E_MailLev" "E_MailLev" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[56]   > skotex.LevBas.KjedeAvtale
"KjedeAvtale" "KjedeAvtale" ? ? "logical" ? ? ? ? ? ? yes ? no 11.2 yes ""
     _FldNameList[57]   > skotex.LevBas.ReklAdresse1
"ReklAdresse1" "ReklAdresse1" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[58]   > skotex.LevBas.ReklAdresse2
"ReklAdresse2" "ReklAdresse2" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[59]   > skotex.LevBas.ReklPostBoks
"ReklPostBoks" "ReklPostBoks" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[60]   > skotex.LevBas.ReklPostNr
"ReklPostNr" "ReklPostNr" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
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
                    INPUT TRUE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

