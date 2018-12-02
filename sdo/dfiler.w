&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
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
DEF VAR h_Parent AS HANDLE NO-UNDO.

DEF STREAM InnFil.

DEFINE STREAM sLesKatalog.
DEFINE STREAM sLesFil.
DEFINE STREAM sAppendFil.

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
&Scoped-define INTERNAL-TABLES Filer

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Backup Dato Dobbel Feil FilId FilNavn Innlest InnlestAv InnlestDato~
 InnlestKl Katalog Kl Oppdatert OppdatertAv OppdatertDato OppdatertKl~
 Storrelse AntLinjer SlettetAv SlettetDato Slettet SlettetTid FilType~
 Overfort OverfortAv OverfortDato OverfortTid
&Scoped-define ENABLED-FIELDS-IN-Filer Backup Dato Dobbel Feil FilId ~
FilNavn Innlest InnlestAv InnlestDato InnlestKl Katalog Kl Oppdatert ~
OppdatertAv OppdatertDato OppdatertKl Storrelse AntLinjer SlettetAv ~
SlettetDato Slettet SlettetTid FilType Overfort OverfortAv OverfortDato ~
OverfortTid 
&Scoped-Define DATA-FIELDS  Backup fuFilTypeTekst Dato Dobbel fuInnlestInfo Feil FilId FilNavn Innlest~
 InnlestAv InnlestDato fuOppdatertInfo fuOverfortInfo InnlestKl Katalog Kl~
 Oppdatert OppdatertAv OppdatertDato fuOppdatertKl OppdatertKl Storrelse~
 AntLinjer SlettetAv SlettetDato Slettet fuInnlestKl SlettetTid FilType~
 Overfort fuSlettetInfo OverfortAv OverfortDato OverfortTid
&Scoped-define DATA-FIELDS-IN-Filer Backup Dato Dobbel Feil FilId FilNavn ~
Innlest InnlestAv InnlestDato InnlestKl Katalog Kl Oppdatert OppdatertAv ~
OppdatertDato OppdatertKl Storrelse AntLinjer SlettetAv SlettetDato Slettet ~
SlettetTid FilType Overfort OverfortAv OverfortDato OverfortTid 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dfiler.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH Filer NO-LOCK ~
    BY Filer.FilId DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH Filer NO-LOCK ~
    BY Filer.FilId DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main Filer
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main Filer


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FilTypeTekst dTables  _DB-REQUIRED
FUNCTION FilTypeTekst RETURNS CHARACTER
  ( INPUT piFiltype AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InnlestInfo dTables  _DB-REQUIRED
FUNCTION InnlestInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Kl dTables  _DB-REQUIRED
FUNCTION Kl RETURNS CHARACTER
  ( piTime AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OppdatertInfo dTables  _DB-REQUIRED
FUNCTION OppdatertInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD OverfortInfo dTables  _DB-REQUIRED
FUNCTION OverfortInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SlettetInfo dTables  _DB-REQUIRED
FUNCTION SlettetInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      Filer SCROLLING.
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
         HEIGHT             = 1.57
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}
{sdo/dproclibstart.i}
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
     _TblList          = "Data.Filer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Data.Filer.FilId|no"
     _FldNameList[1]   > Data.Filer.Backup
"Backup" "Backup" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 7.4 yes ""
     _FldNameList[2]   > "_<CALC>"
"FilTypeTekst(RowObject.FilType)" "fuFilTypeTekst" "FilType" "x(15)" "character" ? ? ? ? ? ? no ? no 15 no ?
     _FldNameList[3]   > Data.Filer.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[4]   > Data.Filer.Dobbel
"Dobbel" "Dobbel" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 6.8 yes ""
     _FldNameList[5]   > "_<CALC>"
"InnlestInfo()" "fuInnlestInfo" "Innlest" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[6]   > Data.Filer.Feil
"Feil" "Feil" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 3.2 yes ""
     _FldNameList[7]   > Data.Filer.FilId
"FilId" "FilId" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[8]   > Data.Filer.FilNavn
"FilNavn" "FilNavn" ? "X(40)" "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[9]   > Data.Filer.Innlest
"Innlest" "Innlest" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 6.2 yes ""
     _FldNameList[10]   > Data.Filer.InnlestAv
"InnlestAv" "InnlestAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[11]   > Data.Filer.InnlestDato
"InnlestDato" "InnlestDato" ? ? "date" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[12]   > "_<CALC>"
"OppdatertInfo()" "fuOppdatertInfo" "Oppdatert" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[13]   > "_<CALC>"
"OverfortInfo()" "fuOverfortInfo" "Overfort" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[14]   > Data.Filer.InnlestKl
"InnlestKl" "InnlestKl" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[15]   > Data.Filer.Katalog
"Katalog" "Katalog" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[16]   > Data.Filer.Kl
"Kl" "Kl" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[17]   > Data.Filer.Oppdatert
"Oppdatert" "Oppdatert" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[18]   > Data.Filer.OppdatertAv
"OppdatertAv" "OppdatertAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[19]   > Data.Filer.OppdatertDato
"OppdatertDato" "OppdatertDato" ? ? "date" ? ? ? ? ? ? yes ? no 14.2 yes ""
     _FldNameList[20]   > "_<CALC>"
"Kl(RowObject.OppdatertKl)" "fuOppdatertKl" "Oppdatert kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[21]   > Data.Filer.OppdatertKl
"OppdatertKl" "OppdatertKl" ? ? "integer" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[22]   > Data.Filer.Storrelse
"Storrelse" "Storrelse" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[23]   > Data.Filer.AntLinjer
"AntLinjer" "AntLinjer" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes ""
     _FldNameList[24]   > Data.Filer.SlettetAv
"SlettetAv" "SlettetAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[25]   > Data.Filer.SlettetDato
"SlettetDato" "SlettetDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[26]   > Data.Filer.Slettet
"Slettet" "Slettet" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[27]   > "_<CALC>"
"Kl(RowObject.InnlestKl)" "fuInnlestKl" "Innlest kl" "x(8)" "character" ? ? ? ? ? ? no ? no 8 no ?
     _FldNameList[28]   > Data.Filer.SlettetTid
"SlettetTid" "SlettetTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[29]   > Data.Filer.FilType
"FilType" "FilType" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[30]   > Data.Filer.Overfort
"Overfort" "Overfort" ? "*~~/" "logical" ? ? ? ? ? ? yes ? no 2.8 yes ""
     _FldNameList[31]   > "_<CALC>"
"SlettetInfo()" "fuSlettetInfo" "Slettet" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[32]   > Data.Filer.OverfortAv
"OverfortAv" "OverfortAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[33]   > Data.Filer.OverfortDato
"OverfortDato" "OverfortDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[34]   > Data.Filer.OverfortTid
"OverfortTid" "OverfortTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
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
         rowObject.fuFilTypeTekst = (FilTypeTekst(RowObject.FilType))
         rowObject.fuInnlestInfo = (InnlestInfo())
         rowObject.fuInnlestKl = (Kl(RowObject.InnlestKl))
         rowObject.fuOppdatertInfo = (OppdatertInfo())
         rowObject.fuOppdatertKl = (Kl(RowObject.OppdatertKl))
         rowObject.fuOverfortInfo = (OverfortInfo())
         rowObject.fuSlettetInfo = (SlettetInfo())
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
  PUBLISH "dfilerDataAvailable".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFil dTables  _DB-REQUIRED
PROCEDURE EksporterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plFilId     LIKE Filer.FilId NO-UNDO.
  DEF OUTPUT PARAMETER pbOk        AS   LOG         NO-UNDO.
  DEF OUTPUT PARAMETER piAntLinjer AS   INT         NO-UNDO.
  
  DEF VAR pcKvitteringId  AS CHAR NO-UNDO.
  DEF VAR pcError         AS CHAR NO-UNDO.
  DEF VAR pcInnKvittering AS CHAR NO-UNDO.
  DEF VAR pcLinje         AS CHAR NO-UNDO.

  DEF VAR piButikkNr      AS INT  NO-UNDO.
  DEF VAR piGruppeNr      AS INT  NO-UNDO.
  DEF VAR piKasseNr       AS INT  NO-UNDO.
  DEF VAR piLoop          AS INT  NO-UNDO.

  RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
           " - dfiler.w - Eksport av fil startet.").

  FIND Filer NO-LOCK WHERE
      Filer.FilId = plFilId NO-ERROR.
  IF NOT AVAILABLE Filer THEN
      RETURN "** Filer posten finnes ikke.".
  
  IF SEARCH(Filer.Katalog + "~\" + Filer.FilNavn) <> ? THEN
  DO:
      RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
               " -  Filen " + Filer.Katalog + "~\" + Filer.FilNavn + " finnes fra før. Eksport avbrutt." + 
               CHR(1) + "1").

      RETURN "** Filen " + Filer.Katalog + "~\" + Filer.FilNavn + " finnes fra før. Eskport avbrutt.".
  END.

  /* Legger ut filen */
  OUTPUT STREAM InnFil TO VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
  /*
  El-Journal,1,Kvittering,2,Utskriftskopi,3,Dagsoppgjør,4,Kassereroppgjør,5
  */
  CASE Filer.FilType:
      WHEN 1 THEN /* EL-Journal */
      DO:
          EKSPORT:
          FOR EACH FilLinjer OF Filer:
            PUT STREAM InnFil UNFORMATTED FilLinje.Tekst SKIP.
            ASSIGN
                piAntLinjer = piAntLinjer + 1
                .
          END. /* EKSPORT */
      END.
      WHEN 2 THEN /* Kvittering */
      DO:
          EKSPORT:
          FOR EACH FilLinjer OF Filer:
            PUT STREAM InnFil UNFORMATTED FilLinje.Tekst SKIP.
            ASSIGN
                piAntLinjer = piAntLinjer + 1
                .
          END. /* EKSPORT */
      END.
      WHEN 3 THEN /* Utskriftskopi */
      DO:
          EKSPORT:
          FOR EACH FilLinjer OF Filer:
            PUT STREAM InnFil UNFORMATTED FilLinje.StorTekst SKIP.
            ASSIGN
                piAntLinjer = piAntLinjer + 1
                .
          END. /* EKSPORT */
      END.
      WHEN 4 THEN /* Dagsoppgjør */
      DO:

      END.
      WHEN 5 THEN /* Kassereroppgjør */
      DO:

      END.
  END CASE.
  OUTPUT STREAM InnFil CLOSE.

  RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                  STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                  " - Fil eksport ferdig. (Eksportert antall linjer = " + STRING(piAntLinjer) + ") Fil: " + 
                 (IF SEARCH(Filer.Katalog + "~\" + Filer.FilNavn) <> ?
                   THEN SEARCH(Filer.Katalog + "~\" + Filer.FilNavn)
                   ELSE "?") + ".").
  RETURN "Fil eksport ferdig. (Eksportert antall linjer = " + STRING(piAntLinjer) + ").".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeInnleste dTables  _DB-REQUIRED
PROCEDURE GetAlleIkkeInnleste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      
 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntFiler AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pdFilId AS DECI NO-UNDO.
  DEF OUTPUT PARAMETER pcListe    AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore     AS LOG  NO-UNDO.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH Filer NO-LOCK WHERE
      Filer.Innlest   = FALSE AND
      Filer.Oppdatert = FALSE AND
      Filer.Overfort  = FALSE
      BY Filer.FilId:
      IF Filer.FilId <= pdFilId THEN
          NEXT.
      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(Filer.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntFiler THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeOppdaterte dTables  _DB-REQUIRED
PROCEDURE GetAlleIkkeOppdaterte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      

 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntFiler AS INT  NO-UNDO.
  DEF INPUT  PARAMETER pdFilId AS DECI NO-UNDO.
  DEF OUTPUT PARAMETER pcListe    AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore     AS LOG  NO-UNDO.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH Filer NO-LOCK WHERE
      Filer.Innlest   = TRUE AND
      Filer.Oppdatert = FALSE AND
      Filer.Overfort  = FALSE
      BY Filer.FilId:
      IF Filer.FilId <= pdFilId THEN
          NEXT.

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(Filer.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntFiler THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeOverforte dTables  _DB-REQUIRED
PROCEDURE GetAlleIkkeOverforte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      

 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntFiler AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe    AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore     AS LOG  NO-UNDO.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH Filer NO-LOCK WHERE
      Filer.Innlest   = TRUE AND
      Filer.Oppdatert = TRUE AND
      Filer.Overfort  = FALSE
      BY Filer.FilId:

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(Filer.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntFiler THEN
      DO:
          ASSIGN
              pbMore = TRUE
              .
          LEAVE LOOPEN.
      END.
  END. /* LOOPEN */

              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetDataSettForFil dTables  _DB-REQUIRED
PROCEDURE GetDataSettForFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  /* Behandler 10 og 10 datasett */
  RUN GetDataSettForFil IN h_dfiler (INPUT pcFilId, INPUT 2, 
                                     INPUT 10, 
                                     OUTPUT pcValgteDataSett,
                                     OUTPUT pbMore).
  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcFilId     AS CHAR NO-UNDO.
  DEF INPUT  PARAMETER piModus     AS INT  NO-UNDO.
  DEF INPUT  PARAMETER piAntSett   AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcValgListe AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore      AS LOG  NO-UNDO.

  DEF VAR pcColValues  AS CHAR NO-UNDO.
  DEF VAR piAntall     AS INT  NO-UNDO.

  ASSIGN
      pbMore   = FALSE
      piAntall = 1
      .

  BYGGDATASETT:
  DO:
      CASE piModus:
          /* Datasett for Oppdatering av bonger. */
          WHEN 2 THEN
              FOR EACH DataSett NO-LOCK WHERE
                  DataSett.FilId = DEC(pcFilId) AND
                  DataSett.SettStatus >= 2 AND 
                  DataSett.SettStatus <= 8 AND
                  DataSett.Behandlet  >= 1 AND
                  DataSett.Behandlet  <= 2
                  BY DataSett.FilId
                  BY DataSett.Dato
                  BY DataSett.Butikk
                  BY DataSett.GruppeNr
                  BY DataSett.KasseNr
                  BY DataSett.SettNr:
                  ASSIGN
                      piAntall    = piAntall + 1
                      pcValgListe = pcValgListe + 
                                    (IF pcValgListe = ""
                                       THEN ""
                                       ELSE CHR(1)) + 
                                    string(DataSett.DataSettId)
                      .
                 IF piAntall > piAntSett THEN
                 DO:
                     ASSIGN
                         pbMore = TRUE
                         .
                     LEAVE BYGGDATASETT.
                 END.
              END.
          /* Datasett for overføring av bonger. */
          WHEN 3 THEN
              FOR EACH DataSett NO-LOCK WHERE
                  DataSett.FilId = DEC(pcFilId) AND
                  DataSett.SettStatus >= 2 AND 
                  DataSett.SettStatus <= 8 AND
                  DataSett.Behandlet  >= 3 AND
                  DataSett.Behandlet  <= 4:
                  ASSIGN
                      piAntall    = piAntall + 1
                      pcValgListe = pcValgListe + 
                                    (IF pcValgListe = ""
                                       THEN ""
                                       ELSE CHR(1)) + 
                                    string(DataSett.DataSettId)
                  .
                  IF piAntall > piAntSett THEN
                  DO:
                      ASSIGN
                          pbMore = TRUE
                          .
                      LEAVE BYGGDATASETT.
                  END.
              END.
      END CASE.
  END. /* BYGGDATASETT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFilNavnListe dTables  _DB-REQUIRED
PROCEDURE GetFilNavnListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcFilNavn AS CHAR NO-UNDO.

  ASSIGN
    pcFilNavn = ""
    .
  FOR EACH Kasse NO-LOCK WHERE
      Kasse.Aktiv = TRUE:
      IF Kasse.ElJournalAktiv 
         AND NOT CAN-DO(pcFilNavn,Kasse.ElJournal[1] + "|" + Kasse.ElJournal[2] + "|" + string(Kasse.ElJournalOperand) + "|1") THEN
        ASSIGN
          pcFilNavn = pcFilNavn + 
                        (IF pcFilNavn = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.ElJournal[1] + "|" + Kasse.ElJournal[2] + "|" + string(Kasse.ElJournalOperand) + "|1" 
                        .
      IF Kasse.KvitteringAktiv 
         AND NOT CAN-DO(pcFilNavn,Kasse.Kvittering[1] + "|" + Kasse.Kvittering[2] + "|" + string(Kasse.KvitteringOperand) + "|2") THEN
        ASSIGN
          pcFilNavn = pcFilNavn + 
                        (IF pcFilNavn = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.Kvittering[1] + "|" + Kasse.Kvittering[2] + "|" + string(Kasse.KvitteringOperand) + "|2"
                        .
      IF Kasse.UtskriftskopiAktiv 
         AND NOT CAN-DO(pcFilNavn,Kasse.Utskriftskopi[1] + "|" + Kasse.Utskriftskopi[2] + "|" + STRING(UtskriftsKopiOperand) + "|3") THEN
        ASSIGN
          pcFilNavn = pcFilNavn + 
                        (IF pcFilNavn = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.Utskriftskopi[1] + "|" + Kasse.Utskriftskopi[2] + "|" + STRING(UtskriftsKopiOperand) + "|3"
                        .
      IF Kasse.KassererOppgjAktiv 
         AND NOT CAN-DO(pcFilNavn,Kasse.KassererOpgj[1] + "|" + Kasse.KassererOpgj[2] + "|" + string(Kasse.KassererOppgjOperand) + "|4") THEN
        ASSIGN
          pcFilNavn = pcFilNavn + 
                        (IF pcFilNavn = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.KassererOpgj[1] + "|" + Kasse.KassererOpgj[2] + "|" + string(Kasse.KassererOppgjOperand) + "|4"
                        .
      IF Kasse.DagsOppgjAktiv 
         AND NOT CAN-DO(pcFilNavn,Kasse.DagsOpgj[1] + "|" + Kasse.DagsOpgj[2] + "|" + STRING(Kasse.DagsOppgjOperand) + "|5") THEN
        ASSIGN
          pcFilNavn = pcFilNavn + 
                        (IF pcFilNavn = ""
                           THEN ""
                           ELSE ",") + 
                        Kasse.DagsOpgj[1] + "|" + Kasse.DagsOpgj[2] + "|" + STRING(Kasse.DagsOppgjOperand) + "|5"
                        .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetInnlest dTables  _DB-REQUIRED
PROCEDURE GetInnlest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdFilId     AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER pbInnlest   AS LOG NO-UNDO.

  FIND Filer NO-LOCK WHERE
      Filer.FilId = pdFilId NO-ERROR.
  IF AVAILABLE Filer THEN
      pbInnlest = Filer.Innlest.
  ELSE
      pbInnlest = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetKatalogListe dTables  _DB-REQUIRED
PROCEDURE GetKatalogListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcKataloger AS CHAR NO-UNDO.

  RUN bibl_getkataloglistekasse.p (OUTPUT pcKataloger).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOppdatert dTables  _DB-REQUIRED
PROCEDURE GetOppdatert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdFilId     AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER pbOppdatert AS LOG NO-UNDO.

  FIND Filer NO-LOCK WHERE
      Filer.FilId = pdFilId NO-ERROR.
  IF AVAILABLE Filer THEN
      pbOppdatert = Filer.Oppdatert.
  ELSE
      pbOppdatert = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetOverfort dTables  _DB-REQUIRED
PROCEDURE GetOverfort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdFilId     AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER pbOVerfort  AS LOG NO-UNDO.

  FIND Filer NO-LOCK WHERE
      Filer.FilId = pdFilId NO-ERROR.
  IF AVAILABLE Filer THEN
      pbOverfort = Filer.Overfort.
  ELSE
      pbOverfort = ?.

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
  DYNAMIC-FUNCTION('setRebuildOnRepos':U,
     INPUT TRUE /* LOGICAL */).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE JournalFilAdmin dTables  _DB-REQUIRED
PROCEDURE JournalFilAdmin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  RUN bibl_journalfiladmin.p.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil dTables  _DB-REQUIRED
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plFilId     LIKE Filer.FilId NO-UNDO.
  DEF OUTPUT PARAMETER pbOk        AS   LOG         NO-UNDO.
  DEF OUTPUT PARAMETER piAntLinjer AS   INT         NO-UNDO.
  
  DEF VAR pcKvitteringId  AS CHAR NO-UNDO.
  DEF VAR pcError         AS CHAR NO-UNDO.
  DEF VAR pcInnlesning AS CHAR NO-UNDO.
  DEF VAR pcLinje         AS CHAR NO-UNDO.

  DEF VAR piButikkNr      AS INT  NO-UNDO.
  DEF VAR piGruppeNr      AS INT  NO-UNDO.
  DEF VAR piKasseNr       AS INT  NO-UNDO.
  DEF VAR piTid           AS INT  NO-UNDO.
  DEF VAR pcReturn-Value  AS CHAR NO-UNDO.
  DEF VAR pcBkuFil        AS CHAR NO-UNDO.
  DEF VAR cLinje          AS CHAR NO-UNDO.
  DEF VAR cTekst          AS CHAR NO-UNDO.

  ASSIGN
      piTid          = TIME
      pcReturn-Value = ""
      .
  RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
           " - dfiler.w - LesInnFil startet." + CHR(1) + "9").

  FIND Filer NO-LOCK WHERE
      Filer.FilId = plFilId NO-ERROR.
  IF NOT AVAILABLE Filer THEN
      RETURN "** Filer posten finnes ikke.".
  
  IF SEARCH(Filer.Katalog + "~\" + Filer.FilNavn) = ? THEN
  DO:
      ASSIGN cTekst = "** Filen " + Filer.Katalog + "~\" + Filer.FilNavn + " er slettet fra katalogen.".
      /* Er filen borte fra katalogen, skal Filer posten bare slettes */
      /* Det skal ikke logges noe.                                    */
      /* Loggen vil bli slettet av databasetrigger.                   */
      DO TRANSACTION:
          FIND CURRENT Filer EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE Filer THEN
              DELETE Filer.
      END. /* TRANSACTION */
      RETURN cTekst.

/*       RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " +                                          */
/*                 STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") +                                 */
/*                " - Filen " + Filer.Katalog + "~\" + Filer.FilNavn + " er slettet fra katalogen." + */
/*                CHR(1) + "1").                                                                      */
/*       RETURN "** Filen " + Filer.Katalog + "~\" + Filer.FilNavn + " er slettet fra katalogen.". */
  END.

  IF Filer.Storrelse > 0 THEN
  FILINN:
  DO ON ERROR UNDO, LEAVE:
    /* Leser første linjen i filen. */
/*!!!*/ IF NOT ENTRY(NUM-ENTRIES(Filer.FilNavn,"."),Filer.FilNavn,".") = "xml" AND Filer.Storrelse > 0 THEN DO:
            INPUT STREAM InnFil FROM VALUE(Filer.Katalog + "~\" + Filer.FilNavn) NO-ECHO.
            IMPORT STREAM InnFil UNFORMATTED cLinje.
            INPUT STREAM InnFil CLOSE.
        END.
    /* Oppretter kasse automatisk hvis den ikke finnes fra før. */
    IF Filer.FilType = 1 /* InfoPOS POS */ THEN RUN SjekkKasseNrInfoPOS (INPUT cLinje).

    /* Kobler filen til butikk, gruppe og kasse.                      */
    /* NB: Filen kan inneholde data fra flere kasser. Vi kobler filen */
    /* til den første kassereferansen vi finner i filen.              */
    RUN koblekasse.p (INPUT  plFilId,
                      INPUT  THIS-PROCEDURE,
                      INPUT  0,
                      INPUT  Filer.FilType,
                      INPUT "", /* vid 0 ovan skall denna vara blank */
                      OUTPUT piButikkNr,
                      OUTPUT piGruppeNr,
                      OUTPUT piKasseNr
                     ).

    FIND Kasse NO-LOCK WHERE
        Kasse.ButikkNr = piButikkNr AND
        Kasse.GruppeNr = piGruppeNr AND
        Kasse.KasseNr  = piKasseNr NO-ERROR.

    /* Kan ikke bestemme innesningsprogram */
    IF NOT AVAILABLE Kasse THEN
        pcInnlesning = "".
    /* Innlesningsprogram for gjeldende filtype. */
    ELSE
        CASE Filer.FilType:
            WHEN 1 THEN pcInnlesning = Kasse.ElJournalInnles.
            WHEN 2 THEN pcInnlesning = Kasse.KvitteringInnles.
            WHEN 3 THEN pcInnlesning = Kasse.UtskriftskopiInnles.
            WHEN 4 THEN pcInnlesning = Kasse.DagsoppgjInnles.
            WHEN 5 THEN pcInnlesning = Kasse.KassererOppgjInnles.
        END CASE.
    /* 0 byte filer flyttes bare. */
    IF Filer.Storrelse > 0 THEN
    IKKE-NULL:
    DO:
        /* Sjekker at programmet finnes. */
        IF SEARCH(pcInnlesning + ".r") = ? THEN
        DO:
          IF pcInnlesning = "" THEN
          DO:
              RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                             " -  Det er ikke satt opp innlesningsprogram for denne filtypen." + 
                             CHR(1) + "3").
              RUN SetFilInnlest.
              RETURN "Det er ikke satt opp innlesningsprogram for denne filtypen.".
          END.
          ELSE DO:
              RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                              STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                             " -  Ukjent innlesningsprogram: " + pcInnlesning + ".r" + 
                             CHR(1) + "3").
              RUN SetFilInnlest.
              RETURN "Ukjent innlesningsprogram: " + pcInnlesning + ".r".
          END.
        END.
        RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                        " - Innlesning av fil starter (Innlesningsprogram: " + 
                        pcInnlesning + ".p).").

        RUN RensReturn.

        RUN VALUE(pcInnlesning + ".p") 
            (INPUT  plFilId,
             INPUT  THIS-PROCEDURE,
             INPUT  h_Parent,
             OUTPUT piAntLinjer
            ).

        IF RETURN-VALUE <> "" THEN
        DO:
            pcError = RETURN-VALUE.
            RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                            STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                           " -  Msg fra <" + pcInnlesning + ".p> " + pcError + 
                           " " + CHR(1) + "1").
        END.
        ELSE
          ASSIGN
              pbOk = TRUE
              .
    END. /* IKKE-NULL */
    ELSE DO:
        ASSIGN
            pbOk = TRUE
            .
    END.

  END. /* FILINN */
  ELSE
      pbOk = TRUE. /* 0-Byte filer skal bare ferdigstilles. */

  IF pbOk = FALSE AND Filer.Storrelse > 0 THEN
  DO:
      DO TRANSACTION:
          FIND CURRENT filer EXCLUSIVE-LOCK.
          ASSIGN
              Filer.Feil      = TRUE
              Filer.Innlest   = FALSE
              Filer.antLinjer = piantLinjer
              .
      END. /* TRANSACTION */
      FIND CURRENT filer NO-LOCK.

      RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " -  Det er oppdaget feil på filen." + CHR(1) + "1").
      RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Fil innlesning avbrutt. (Lest antall linjer = " + STRING(piAntLinjer) + ").").
      RETURN "** Det er oppdaget feil på filen ** " + CHR(13) + 
             "Fil innlesning avbrutt. (Lest antall linjer = " + STRING(piAntLinjer) + ").".
  END.
  ELSE DO:
      DO TRANSACTION:
          FIND CURRENT filer EXCLUSIVE-LOCK.
          ASSIGN
              Filer.Feil      = FALSE 
              Filer.Innlest   = TRUE
              Filer.antLinjer = piantLinjer
              .
          IF Filer.storrelse = 0 THEN
              ASSIGN
              Filer.Oppdatert = TRUE
              Filer.Overfort  = TRUE
              .
      END. /* TRANSACTION */
      FIND CURRENT filer NO-LOCK.

      IF (Filer.FilNavn BEGINS "Journal" OR Filer.FilNavn BEGINS 'PRSjournal') THEN
      DO:
          ASSIGN
              pcBkuFil = Filer.Katalog + "~\" + "oppd-" +
                         Filer.FilNavn
              .
      END.
      /* Bytter navn på fil hvis det er gammel JOURNAL fil. */
      ELSE IF NUM-ENTRIES(Filer.FilNavn,".") <= 2 THEN
          ASSIGN
              pcBkuFil = Filer.Katalog + "~\bku" + "\" + 
                         ENTRY(1,Filer.FilNavn,".") + 
                         "-bku" + 
                         STRING(YEAR(TODAY),"9999") +
                         STRING(MONTH(TODAY),"99") + 
                         STRING(DAY(TODAY),"99") + 
                         "-" + 
                         entry(1,STRING(TIME,"HH:MM:SS"),":") + 
                         entry(2,STRING(TIME,"HH:MM:SS"),":") + 
                         entry(3,STRING(TIME,"HH:MM:SS"),":") + 
                         "." + (IF NUM-ENTRIES(Filer.FilNavn,".") >= 2
                                THEN ENTRY(2,Filer.FilNavn,".")
                                ELSE "")
              .
      ELSE DO:
          ASSIGN
              pcBkuFil = Filer.Katalog + "~\bku" + "\" + 
                         Filer.FilNavn
              .
      END.

      /* Sikrer at backup katalog finnes. */
      OS-CREATE-DIR value(Filer.Katalog + "~\bku").
      /* Flytter filen til backup katalog. */
      IF SEARCH(Filer.Katalog + "~\" + Filer.FilNavn) <> ? THEN 
          OS-COPY value(Filer.Katalog + "~\" + Filer.FilNavn) VALUE(pcBkuFil).
      
      /* Renser bort fil */
      IF SEARCH(pcBkuFil) <> ? THEN
      DO:
          /* Filen tas bort fra katalogen. */
          IF SEARCH(Filer.Katalog + "~\" + Filer.FilNavn) <> ? THEN
              OS-DELETE VALUE(Filer.Katalog + "~\" + Filer.FilNavn).
          RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Filen " + Filer.FilNavn + " kopiert til backup katalog: " + Filer.Katalog + "~\bku" + 
                     CHR(1) + "0").
      END.
      ELSE
          RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                     " - Flytting av filen " + Filer.FilNavn + " til backup katalog misslykkes." + 
                     CHR(1) + "3").
     
      RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                      " - Fil innlesning ferdig. (Tid brukt: " + 
                      string(TIME - piTid,"HH:MM:SS") + " Lest antall linjer = " + STRING(piAntLinjer) + "). Filen er slettet fra katalogen.").
      RETURN "Fil innlesning ferdig. (" + 
             string(TIME - piTid,"HH:MM:SS") + " Lest antall linjer = " + STRING(piAntLinjer) + ").".           
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyFilLogg dTables  _DB-REQUIRED
PROCEDURE NyFilLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plFilId LIKE Filer.FilId NO-UNDO.
  DEF INPUT PARAMETER pcTekst AS   CHAR        NO-UNDO.
  
  DEF VAR piLinjeNr   AS INT NO-UNDO.
  DEF VAR piGradering AS INT NO-UNDO.

  ASSIGN
      piGradering = 0
      .
  IF NUM-ENTRIES(pcTekst,CHR(1)) >= 2 THEN
    ASSIGN
      piGradering = INT(ENTRY(2,pcTekst,CHR(1)))
      NO-ERROR.
  IF ERROR-STATUS:ERROR THEN
      piGradering = 0.

  DO TRANSACTION:
    FIND LAST FilLogg NO-LOCK WHERE
        FilLogg.FilId = plFilId NO-ERROR.
    IF AVAILABLE FilLogg THEN
        piLinjeNr = FilLogg.LinjeNr + 1.
    ELSE
        piLinjeNr = 1.
    CREATE FilLogg.
    ASSIGN
        FilLogg.FilId     = plFilId
        FilLogg.LinjeNr   = piLinjeNr
        FilLogg.Tekst     = ENTRY(1,pcTekst,CHR(1))
        FilLogg.Gradering = piGradering
        .
    RELEASE FilLogg.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPoster dTables  _DB-REQUIRED
PROCEDURE OpprettPoster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pcKataloger AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcFiler     AS CHAR NO-UNDO.

  DEF VAR piLoop1       AS INT  NO-UNDO.
  DEF VAR piLoop2       AS INT  NO-UNDO.
  DEF VAR pcKatalog     AS CHAR NO-UNDO.
  DEF VAR pcEkstent     AS CHAR NO-UNDO.
  DEF VAR pcKatFil      AS CHAR NO-UNDO.
  DEF VAR plFilId       AS DEC  NO-UNDO.
  DEF VAR pcBegins      AS CHAR NO-UNDO.
  DEF VAR piEntries     AS INT  NO-UNDO.
  DEF VAR piFilType     AS INT  NO-UNDO.

  DEF VAR pcFileName    AS CHAR FORMAT "x(100)" NO-UNDO.
  DEF VAR pcFilePath    AS CHAR FORMAT "x(200)" NO-UNDO.
  DEF VAR pcFileAttrib  AS CHAR FORMAT "x(15)" NO-UNDO.
  DEFINE VARIABLE cZipbkuDir AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cZipFilDir AS CHARACTER  NO-UNDO.


  /* Ingenting å sjekke */
  IF pcKataloger = "" THEN
    RETURN "Ingen kataloger som skal sjekkes.".
  /* Behandler alle kataloger. */
  DO piLoop1 = 1 TO NUM-ENTRIES(pcFiler):
    /*
      1. --> _ og _
      2. --> _ og *
      3. --> * og _
      4. --> * og *
    */
    ASSIGN
      pcBegins  = (IF (ENTRY(3,entry(piLoop1,pcFiler),"|") = "3" OR ENTRY(3,entry(piLoop1,pcFiler),"|") = "4")
                    THEN "*"
                    ELSE "") + 
                  ENTRY(1,
                          ENTRY(piLoop1,pcFiler),"|"
                       ) +
                  (IF (ENTRY(3,entry(piLoop1,pcFiler),"|") = "2" OR ENTRY(3,entry(piLoop1,pcFiler),"|") = "4")
                    THEN "*"
                    ELSE "")
      pcEkstent = ENTRY(2,
                          ENTRY(piLoop1,pcFiler),"|"
                       )
      piFilType = INT(ENTRY(4,
                          ENTRY(piLoop1,pcFiler),"|"
                       ))
      .

    /* For hver ekstent i katalog */
    DO piLoop2 = 1 TO NUM-ENTRIES(pcKataloger):
      ASSIGN
        pcKatFil  = ENTRY(piLoop2,pcKataloger)
        .
      OS-CREATE-DIR value(pcKatFil). /* Skaper katalogen hvis den ikke finnes */
      /* OM pcExtent = "dbf" finns det är dbf-filerna i formatet .zip och skall unzippas */
      IF pcEkstent = "dbf" THEN DO:
          ASSIGN cZipFilDir = RIGHT-TRIM(RIGHT-TRIM(pcKatFil,"\"),"/").
          ASSIGN cZipbkuDir = cZipFilDir + "\zipbku".
          OS-CREATE-DIR value(cZipbkuDir).
          OS-COMMAND SILENT VALUE("unzip -o -q " + cZipFilDir + "\*.zip -d " + cZipFilDir).
          OS-COMMAND SILENT VALUE("move /Y " +  cZipFilDir + "\*.zip " + cZipbkuDir).
      END.
/* MESSAGE pcKatFil                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      INPUT STREAM InnFil FROM OS-DIR (pcKatFil) NO-ECHO.
      FILINPUT:
      REPEAT:
        /*
        SET STREAM InnFil
          pcFileName  
          pcFilePath  
          pcFileAttrib
          WITH WIDTH 248.
        */
        IMPORT STREAM InnFil
            pcFileName  
            pcFilePath  
            pcFileAttrib
            .
        /* Bare filer skal opprettes */
        IF LOOKUP("F",pcFileAttrib) <> 0 THEN
        DO:
          /* Åpner for filinformasjonen */
          ASSIGN
            piEntries           = NUM-ENTRIES(pcFileName,".")
            FILE-INFO:FILE-NAME = pcFilePath
            . 
          /* Sletter tomme filer. */
          IF FILE-INFO:FILE-SIZE = 0 THEN DO:
              /*OS-DELETE VALUE(FILE-INFO:FILE-NAME).*/
              NEXT FILINPUT.
          END.
          /* Kun filer som oppfyller masken på filnavn skal inn. */
          IF pcFileName MATCHES pcBegins THEN. /* Gjør ingenting. */
          ELSE
            NEXT FILINPUT. /* Hopp over denne */

          /* Kun filer som oppfyller extent masken skal inn */
          IF piEntries > 1 THEN
          DO:
            IF ENTRY(piEntries,pcfileName,".") MATCHES pcEkstent THEN. /* gjør ingenting. */
            ELSE
              NEXT FILINPUT. /* Hopp over denne */
          END.

          /* Oppretter posten i filen. */
          IF NOT CAN-FIND(Filer WHERE
                          Filer.FilNavn   = pcFileName AND
                          Filer.Dato      = FILE-INFO:FILE-MOD-DATE AND
                          Filer.Kl        = string(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS") AND
                          Filer.Storrelse = FILE-INFO:FILE-SIZE AND
                          Filer.Katalog   = pcKatFil
                         ) THEN
          DO:
            /* Finner FilId */
            FIND LAST Filer NO-LOCK NO-ERROR.
            IF AVAILABLE Filer THEN
              plFilId = Filer.FilId + 1.
            ELSE
              plFilId = 1.
            CREATE Filer.
            ASSIGN
              Filer.FilId     = plFilId
              Filer.FilNavn   = pcFileName
              Filer.Katalog   = pcKatFil
              Filer.Dato      = FILE-INFO:FILE-MOD-DATE
              Filer.Kl        = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
              Filer.Storrelse = FILE-INFO:FILE-SIZE
              Filer.AntLinjer = 0
              Filer.FilType   = piFilType
              .
            RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                                  STRING(TIME,"HH:MM:SS")+ " " + USERID("skotex") + 
                                  " - Funnet på filkatalog ").

          END.
        END.

      END. /* FILINPUT */
      INPUT STREAM InnFil CLOSE.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensReturn dTables  _DB-REQUIRED
PROCEDURE RensReturn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RensTommePoster dTables  _DB-REQUIRED
PROCEDURE RensTommePoster :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFiler AS CHAR NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.
  DEF VAR pbOk   AS LOG NO-UNDO.

  /* Sendes blank liste inn, skal alle ikke innleste poster tas med. */
  /* Poster som er slettet, skal heller ikke tas med.                */
  IF pcFiler = "" THEN
  LESBLOKK:
  DO WHILE TRUE:

      FILBLOKK:
      FOR EACH Filer NO-LOCK WHERE
        Filer.Innlest = FALSE:
        
        /* Skal ikke ha med 'slettede' filer. */
        IF Filer.Slettet = TRUE THEN NEXT FILBLOKK.

        ASSIGN
          pcFiler = pcFiler + 
                    (IF pcFiler = ""
                       THEN ""
                       ELSE ",") + 
                    string(Filer.FilId,">>>>>>>>>>>>9")
                    .
          IF LENGTH(pcFiler) > 31000 THEN
              LEAVE.
      END. /* FILBLOKK */

      FILLOOP:
      DO piLoop = 1 TO NUM-ENTRIES(pcFiler):
        STATUS DEFAULT ENTRY(piLoop,pcFiler).

        RUN SlettFil (INPUT DEC(ENTRY(piLoop,pcFiler)), INPUT TRUE, OUTPUT pbOk).
      END. /* FILLOOP */
      STATUS DEFAULT "".

      /* Ferdig med jobben */
      IF pcFiler = "" THEN
          LEAVE LESBLOKK.
      ELSE
          pcFiler = "".

  END. /* LESBLOKK */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilInnlest dTables  _DB-REQUIRED
PROCEDURE SetFilInnlest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE Filer THEN
  DO TRANSACTION:
      FIND CURRENT Filer EXCLUSIVE-LOCK.
      ASSIGN
          Filer.Innlest = TRUE
          Filer.InnlestDato = TODAY
          Filer.InnlestKl   = TIME
          Filer.InnlestAv   = USERID("SkoTex")
          .
  END.
  FIND CURRENT Filer NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilOppdatert dTables  _DB-REQUIRED
PROCEDURE SetFilOppdatert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFilId AS CHAR NO-UNDO.

  DO TRANSACTION:
      FIND Filer EXCLUSIVE-LOCK WHERE
          Filer.FilId = DEC(pcFilId) NO-ERROR.
      IF AVAILABLE Filer THEN
          ASSIGN
          Filer.Oppdatert     = TRUE
          Filer.OppdatertDato = TODAY
          Filer.OppdatertKl   = TIME
          Filer.OppdatertAv   = USERID("SkoTex")
          .
      IF AVAILABLE Filer THEN
        RELEASE Filer.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFilOverfort dTables  _DB-REQUIRED
PROCEDURE SetFilOverfort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
            RUN SettFilOverfort IN h_dfiler (INPUT pcFilId).

------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFilId AS CHAR NO-UNDO.

  DO TRANSACTION:
      FIND Filer EXCLUSIVE-LOCK WHERE
          Filer.FilId = DEC(pcFilId) NO-ERROR.
      IF AVAILABLE Filer THEN
          ASSIGN
          Filer.Overfort     = TRUE
          Filer.OverfortDato = TODAY
          Filer.OverfortTid  = TIME
          Filer.OverfortAv   = USERID("SkoTex")
          .
      IF AVAILABLE Filer THEN
        RELEASE Filer.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetHandleTelleverk dTables  _DB-REQUIRED
PROCEDURE SetHandleTelleverk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ph_Parent AS HANDLE NO-UNDO.

  ASSIGN
      h_Parent = ph_Parent
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFilOppdatert dTables  _DB-REQUIRED
PROCEDURE SettFilOppdatert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plFilId AS DEC NO-UNDO.

  DO TRANSACTION:
      FIND Filer EXCLUSIVE-LOCK WHERE
          Filer.FilId = plFilId NO-ERROR.
      IF AVAILABLE Filer THEN
      DO:
          ASSIGN
          Filer.Oppdatert = TRUE
          Filer.OppdatertDAto = TODAY
          Filer.OppdatertKl   = TIME
          Filer.OppdatertAv   = USERID("dictdb")
          .
          /* Utskriftskopi skal også markeres som overført. */
          IF Filer.FilType = 3 THEN
              ASSIGN
              Filer.Overfort     = TRUE
              Filer.OverfortDato = TODAY
              Filer.OverfortTid  = TIME
              Filer.OverfortAv   = USERID("dictdb")
              .

          RELEASE Filer.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkKasseNrInfoPOS dTables  _DB-REQUIRED
PROCEDURE SjekkKasseNrInfoPOS :
/*------------------------------------------------------------------------------
  Purpose:     Oppretter kasserecord for de kassenummer som er ukjente.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pcLinje AS CHAR NO-UNDO.


DEF VAR piButikkNr AS INT  NO-UNDO.
DEF VAR piKasseNr  AS INT  NO-UNDO.

DEF BUFFER bKasse FOR Kasse.

ASSIGN
  piButikkNr = int(ENTRY(1,pcLinje,";"))
  piKasseNr  = int(ENTRY(2,pcLinje,";"))
  NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN.

/* Oppretter kassen hvis den mangler. */
IF NOT CAN-FIND(Kasse WHERE
                Kasse.ButikkNr = pibutikkNr AND
                Kasse.GruppeNr = 1  AND
                Kasse.KasseNr  = piKasseNr) THEN
  DO FOR bKasse TRANSACTION:
    /* Kasse 1 skal ALLTID være lagt opp på alle butikker. */
    FIND Kasse NO-LOCK WHERE
        Kasse.ButikkNr = piButikkNr AND
        Kasse.Gruppe   = 1 AND
        Kasse.KasseNr  = 1 NO-ERROR.
    IF AVAILABLE Kasse THEN
    DO:
        CREATE bKasse.
        BUFFER-COPY Kasse TO bKasse
            ASSIGN
            bKasse.KasseNr        = piKasseNr
            bKasse.Navn           = "Kasse " + string(piKasseNr) + " - Butikk " + string(piButikkNr)
            bKasse.Aktiv          = TRUE
            bKasse.ElJournal[2]   = STRING(pibutikkNr)
            bKasse.ElJournalId    = STRING(pibutikkNr) + ";" + string(piKasseNr) + ";"
            bKasse.ElJournalAktiv = TRUE
            .
    END.

    IF AVAILABLE bKasse THEN
        RELEASE bKasse.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettFil dTables  _DB-REQUIRED
PROCEDURE SlettFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plFilId   LIKE Filer.FilId NO-UNDO.
  DEF INPUT  PARAMETER pbUansett AS   LOG         NO-UNDO.
  DEF OUTPUT PARAMETER pbOk      AS   LOG         NO-UNDO.

  SLETTFIL:
  DO:
    FIND Filer NO-LOCK WHERE
      Filer.FilId = plFilId NO-ERROR.
    IF NOT AVAILABLE Filer THEN
      RETURN.

    /* Fjerner tilhørende datasett. */
    FOR EACH DataSett OF Filer EXCLUSIVE-LOCK:
        FOR EACH BongHode OF DataSett:
            FOR EACH BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id:
                DELETE BongLinje.
            END.
            DELETE BongHode.
        END.
        DELETE DataSett.
    END.

    /* Fjerner tilhørende linjer */
    FOR EACH FilLinjer WHERE
        Fillinjer.FilId = Filer.Filid EXCLUSIVE-LOCK:
      DELETE FilLinjer.
    END.

    /* Markerer hovedposten som slettet. */
    IF pbUansett = FALSE THEN
    DO TRANSACTION:
      FIND CURRENT Filer EXCLUSIVE-LOCK.
      ASSIGN
        Filer.Slettet     = TRUE
        Filer.SlettetDato = TODAY
        Filer.SlettetTid  = TIME
        Filer.SlettetAv   = USERID("SkoTex")
        .
    END.
    /* 4b. Sletter posten helt hvis dette kreves. */
    ELSE DO:
        /* 2. Fjerner tilhørende loggposter */
/*         FOR EACH FilLogg WHERE                          */
/*             FilLogg.FilId = Filer.FilId EXCLUSIVE-LOCK: */
/*           DELETE FilLogg.                               */
/*         END.                                            */
        /* Sletter posten */
        DO TRANSACTION:
            FIND CURRENT Filer EXCLUSIVE-LOCK.
            DELETE Filer.
        END.
    END.

    ASSIGN
        pbOk = TRUE
        .
  END. /* SLETTFIL */

  IF pbOk = FALSE THEN
  DO:
      RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + USERID("skotex")
                     + " -  Sletting av fil avbrutt." + CHR(1) + "1").
      RETURN "** Sletting av fil avbrutt.".

  END.
  ELSE  DO:
      IF pbUansett = FALSE THEN
      DO:
        RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + USERID("skotex") + 
                       " - Fil slettet.").
      END.
      RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Telleverk dTables  _DB-REQUIRED
PROCEDURE Telleverk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  PUBLISH "Telleverk" (INPUT pcTekst).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FilTypeTekst dTables  _DB-REQUIRED
FUNCTION FilTypeTekst RETURNS CHARACTER
  ( INPUT piFiltype AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RUN FilTypeTekst IN h_dproclib (INPUT piFilType).

  RETURN RETURN-VALUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InnlestInfo dTables  _DB-REQUIRED
FUNCTION InnlestInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcInfo AS CHAR NO-UNDO.

  ASSIGN
      pcInfo = STRING(RowObject.InnlestDato) + " " + 
               STRING(RowObject.InnlestKl,"HH:MM:SS") + " " +
               RowObject.InnlestAv
      pcInfo = IF pcInfo = ?
                 THEN ""
                 ELSE pcInfo
      .
  RETURN pcInfo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Kl dTables  _DB-REQUIRED
FUNCTION Kl RETURNS CHARACTER
  ( piTime AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcKl AS CHAR FORMAT "x(8)" NO-UNDO.

  ASSIGN
      pcKl = STRING(piTime,"HH:MM:SS")
      .
  

  RETURN pcKl.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OppdatertInfo dTables  _DB-REQUIRED
FUNCTION OppdatertInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcInfo AS CHAR NO-UNDO.

  ASSIGN
      pcInfo = STRING(RowObject.OppdatertDato) + " " + 
               STRING(RowObject.OppdatertKl,"HH:MM:SS") + " " +  
               RowObject.OppdatertAv
      pcInfo = IF pcInfo = ?
                 THEN ""
                 ELSE pcInfo
      .
  RETURN pcInfo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION OverfortInfo dTables  _DB-REQUIRED
FUNCTION OverfortInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcInfo AS CHAR NO-UNDO.

  ASSIGN
      pcInfo = STRING(RowObject.OverfortDato) + " " + 
               STRING(RowObject.OverfortTid,"HH:MM:SS") + " " +  
               RowObject.OverfortAv
      pcInfo = IF pcInfo = ?
                 THEN ""
                 ELSE pcInfo
      .
  RETURN pcInfo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SlettetInfo dTables  _DB-REQUIRED
FUNCTION SlettetInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcInfo AS CHAR NO-UNDO.

  ASSIGN
      pcInfo = STRING(RowObject.SlettetDato) + " " + 
               STRING(RowObject.SlettetTid,"HH:MM:SS") + " " +
               RowObject.SlettetAv
      pcInfo = IF pcInfo = ?
                 THEN ""
                 ELSE pcInfo
      .
  RETURN pcInfo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

