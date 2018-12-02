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

DEF VAR pcFilTypeTekst   AS CHAR NO-UNDO.
DEF VAR pcFilTypeListe   AS CHAR NO-UNDO.

DEF VAR pcFilStatusTekst AS CHAR NO-UNDO.
DEF VAR pcFilStatusListe AS CHAR NO-UNDO.

DEF VAR cTekst AS CHAR NO-UNDO.

DEF STREAM InnFil.

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
&Scoped-define INTERNAL-TABLES VPIFilHode

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  VPIFilStatus AntLinjer Dato FilId FilNavn Katalog Kl Storrelse VpiFilType~
 EAv EDato ETid OAv ODato OTid EkstVPILevNr
&Scoped-define ENABLED-FIELDS-IN-VPIFilHode VPIFilStatus AntLinjer Dato ~
FilId FilNavn Katalog Kl Storrelse VpiFilType EAv EDato ETid OAv ODato OTid ~
EkstVPILevNr 
&Scoped-Define DATA-FIELDS  VPIFilStatus fuStatusTekst fuDatoTid fuNavnEkstVPILev fuEDatoTid AntLinjer~
 Dato fuEndretInfo FilId FilNavn Katalog Kl Storrelse VpiFilType EAv EDato~
 ETid OAv ODato OTid EkstVPILevNr
&Scoped-define DATA-FIELDS-IN-VPIFilHode VPIFilStatus AntLinjer Dato FilId ~
FilNavn Katalog Kl Storrelse VpiFilType EAv EDato ETid OAv ODato OTid ~
EkstVPILevNr 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dvpifilhode.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH VPIFilHode NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH VPIFilHode NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main VPIFilHode
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main VPIFilHode


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DatoTid dTables  _DB-REQUIRED
FUNCTION DatoTid RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD EDatoTid dTables  _DB-REQUIRED
FUNCTION EDatoTid RETURNS CHARACTER
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FilType dTables  _DB-REQUIRED
FUNCTION FilType RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD NavnEkstVPILev dTables  _DB-REQUIRED
FUNCTION NavnEkstVPILev RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VPIFilStatus dTables  _DB-REQUIRED
FUNCTION VPIFilStatus RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      VPIFilHode SCROLLING.
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
         HEIGHT             = 1.43
         WIDTH              = 52.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}
{dproclibstart.i}
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
     _TblList          = "Data.VPIFilHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Data.VPIFilHode.VPIFilStatus
"VPIFilStatus" "VPIFilStatus" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[2]   > "_<CALC>"
"VPIFilStatus()" "fuStatusTekst" "StatusTekst" "x(12)" "character" ? ? ? ? ? ? no ? no 12 no ?
     _FldNameList[3]   > "_<CALC>"
"DatoTid()" "fuDatoTid" "Dato/Tid" "x(18)" "character" ? ? ? ? ? ? no ? no 18 no ?
     _FldNameList[4]   > "_<CALC>"
"NavnEkstVPILev()" "fuNavnEkstVPILev" "VPILev" "x(10)" "character" ? ? ? ? ? ? no ? no 10 no ?
     _FldNameList[5]   > "_<CALC>"
"EDatoTid()" "fuEDatoTid" "Innlest" "x(25)" "character" ? ? ? ? ? ? no ? no 25 no ?
     _FldNameList[6]   > Data.VPIFilHode.AntLinjer
"AntLinjer" "AntLinjer" ? ? "integer" ? ? ? ? ? ? yes ? no 12.6 yes ""
     _FldNameList[7]   > Data.VPIFilHode.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[8]   > "_<CALC>"
"EndretInfo()" "fuEndretInfo" "EndretInfo" "x(40)" "character" ? ? ? ? ? ? no ? no 40 no ?
     _FldNameList[9]   > Data.VPIFilHode.FilId
"FilId" "FilId" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[10]   > Data.VPIFilHode.FilNavn
"FilNavn" "FilNavn" ? "X(60)" "character" ? ? ? ? ? ? yes ? no 60 yes ""
     _FldNameList[11]   > Data.VPIFilHode.Katalog
"Katalog" "Katalog" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[12]   > Data.VPIFilHode.Kl
"Kl" "Kl" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[13]   > Data.VPIFilHode.Storrelse
"Storrelse" "Storrelse" ? ">>,>>>,>>>,>>9" "integer" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[14]   > Data.VPIFilHode.VpiFilType
"VpiFilType" "VpiFilType" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[15]   > Data.VPIFilHode.EAv
"EAv" "EAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[16]   > Data.VPIFilHode.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[17]   > Data.VPIFilHode.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[18]   > Data.VPIFilHode.OAv
"OAv" "OAv" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[19]   > Data.VPIFilHode.ODato
"ODato" "ODato" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[20]   > Data.VPIFilHode.OTid
"OTid" "OTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[21]   > Data.VPIFilHode.EkstVPILevNr
"EkstVPILevNr" "EkstVPILevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
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
         rowObject.fuDatoTid = (DatoTid())
         rowObject.fuEDatoTid = (EDatoTid())
         rowObject.fuEndretInfo = (EndretInfo())
         rowObject.fuNavnEkstVPILev = (NavnEkstVPILev())
         rowObject.fuStatusTekst = (VPIFilStatus())
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterFil dTables  _DB-REQUIRED
PROCEDURE EksporterFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcFilId AS  CHAR NO-UNDO.

  DEF VAR piAntLinjer AS INT NO-UNDO.

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = dec(pcFilId) NO-ERROR.
  IF NOT AVAILABLE VPIFilHode THEN
      RETURN.
  FIND EkstVPIFil NO-LOCK WHERE
      EkstVPIFil.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
      EkstVPIFil.VPIFilTypeNr = VPIFilHode.VPIFilType
      NO-ERROR.
  IF NOT AVAILABLE EkstVPIFil THEN
      RETURN.

  IF VPIFilHode.VPIFilStatus < 3 THEN
  DO:
      MESSAGE "Filen er ikke innlest. Kan ikke eksporteres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
  DO:
      RETURN "** Filen " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn + " finnes fra før. Eskport avbrutt.".
  END.

  EKSPORTER:
  DO ON ERROR UNDO, LEAVE:
      /* Legger ut filen */
      OUTPUT STREAM InnFil TO VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
      CASE EkstVPIFil.VPIFilType:
        WHEN 1 THEN /* VPI */
        DO:
            EKSPORT:
            FOR EACH VPIFilLinje OF VPIFilHode:
              PUT STREAM InnFil UNFORMATTED VPIFilLinje.StorTekst SKIP.
              ASSIGN
                  piAntLinjer = piAntLinjer + 1
                  .
              STATUS DEFAULT "Eksporterer linje " + 
                             STRING(piAntLinjer) + " av " + 
                             STRING(VPIFilHode.AntLinjer) + "."
                             .
            END. /* EKSPORT */
        END.
        WHEN 2 THEN /* Varegruppering */
        DO:
            EKSPORT:
            FOR EACH VPIFilLinje OF VPIFilHode:
              PUT STREAM InnFil UNFORMATTED VPIFilLinje.StorTekst SKIP.
              ASSIGN
                  piAntLinjer = piAntLinjer + 1
                  .
              STATUS DEFAULT "Eksporterer linje " + 
                             STRING(piAntLinjer) + " av " + 
                             STRING(VPIFilHode.AntLinjer) + "."
                             .
            END. /* EKSPORT */
        END.
        WHEN 3 THEN /*  */
        DO:
            EKSPORT:
            FOR EACH VPIFilLinje OF VPIFilHode:
              PUT STREAM InnFil UNFORMATTED VPIFilLinje.StorTekst SKIP.
              ASSIGN
                  piAntLinjer = piAntLinjer + 1
                  .
              STATUS DEFAULT "Eksporterer linje " + 
                             STRING(piAntLinjer) + " av " + 
                             STRING(VPIFilHode.AntLinjer) + "."
                             .
            END. /* EKSPORT */
        END.
        WHEN 4 THEN /*  */
        DO:
            EKSPORT:
            FOR EACH VPIFilLinje OF VPIFilHode:
              PUT STREAM InnFil UNFORMATTED VPIFilLinje.StorTekst SKIP.
              ASSIGN
                  piAntLinjer = piAntLinjer + 1
                  .
              STATUS DEFAULT "Eksporterer linje " + 
                             STRING(piAntLinjer) + " av " + 
                             STRING(VPIFilHode.AntLinjer) + "."
                             .
            END. /* EKSPORT */
        END.
      END CASE.
      OUTPUT STREAM InnFil CLOSE.

  END. /* EKSPORTER */

  STATUS DEFAULT "".

  RETURN "Eksport av fil ferdig.".

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
  DEF INPUT  PARAMETER piAntVPIFilHode AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe         AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore          AS LOG  NO-UNDO.

  DEF BUFFER bufVPIFilHode FOR VPIFilHode.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH bufVPIFilHode NO-LOCK WHERE
      bufVPIFilHode.VPIFilStatus   >= 1 AND
      bufVPIFilHode.VPIFilStatus   <= 2
      BY bufVPIFilHode.FilId:

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(bufVPIFilHode.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntVPIFilHode THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetAlleIkkeUtpakkede dTables  _DB-REQUIRED
PROCEDURE GetAlleIkkeUtpakkede :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      

 
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piAntVPIFilHode AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER pcListe         AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pbMore          AS LOG  NO-UNDO.

  ASSIGN
      pbMore = FALSE
      .

  LOOPEN:
  FOR EACH VPIFilHode NO-LOCK WHERE
      VPIFilHode.VPIFilStatus   >=3 AND
      VPIFilHode.VPIFilStatus   <=4
      BY VPIFilHode.FilId:

      ASSIGN
          pcListe = pcListe + 
                    (IF pcListe = ""
                       THEN ""
                       ELSE CHR(1)) + 
                    string(VPIFilHode.FilId).
      IF NUM-ENTRIES(pcListe,CHR(1)) > piAntVPIFilHode THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetEkstVpiLevListe dTables  _DB-REQUIRED
PROCEDURE GetEkstVpiLevListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcTekst   AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER piDefault AS INT  NO-UNDO.

  DEF VAR pcChar AS CHAR NO-UNDO.
  DEF VAR pcAlle AS CHAR NO-UNDO.

  {syspara.i 1 100 1 pcAlle}
  {syspara.i 1  12 1 piDefault INT} 

  pcTekst = pcAlle + ",0".
  FOR EACH EkstVpiLev NO-LOCK WHERE
      CAN-FIND(FIRST VPIFilHode WHERE 
               VPIFilHode.EkstVPILEvNr = EkstVPILev.EkstVPILevNr) 
      BREAK BY EkstVpiLev.EkstVPILevNr:
      ASSIGN
          pcChar  = REPLACE(EkstVpiLev.Navn,","," ")
          pcTekst = pcTekst + 
                    (IF pcTekst = ""
                       THEN "" ELSE ",") + 
                    STRING(EkstVpiLev.EkstVpiLevNr) + '/' + pcChar + "," + STRING(EkstVpiLev.EkstVpiLevNr)
          .
  END.

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

  DEFINE VARIABLE pcLst AS CHARACTER NO-UNDO.

  ASSIGN
    pcFilNavn = ""
    pcLst     = 'SBA,SPK,AE,APK'
    .
  FOR EACH EkstVPILev NO-LOCK WHERE
      EkstVPILev.AktivLev = TRUE:
      VPIFIL:
      FOR EACH EkstVPIFil OF EkstVPILev NO-LOCK WHERE
          EkstVPIFil.VPIFilAktiv = TRUE:

          /* VPI filer skal ikke lenger leses inn her. Det gjøres manuelt fra VPIMottakskontrollen */
          /* TN 28/7-08.                                                                           */
          /* VPI filer med filnavnprefix som ligger i denne listen, skal leses inn automatisk.     */
          /* TN 19/9-10 pcLst                                                                      */
          IF (EkstVPIFil.VPIFilType = 1  AND
              NOT CAN-DO(pcLst,EkstVPIFil.VPIFilNavn) AND 
              EkstVPIFil.VPIUtpakkingsrutin = "xsport1vpiutpakk") THEN
              NEXT VPIFIL.

          IF NOT CAN-DO(pcFilNavn,EkstVPIFil.VPIFilNavn + "|" + EkstVPIFil.VPIEkst + "|" + "1|1") THEN
          ASSIGN
            pcFilNavn = pcFilNavn + 
                          (IF pcFilNavn = ""
                             THEN ""
                             ELSE ",") + 
                          EkstVPIFil.VPIFilNavn + "|" + 
                          EkstVPIFil.VPIEkst + "|" + 
                          STRING(EkstVpiFil.VPIOperator) + 
                          "|" + STRING(EkstVpiFil.VPIFilTypeNr) + "|" + 
                          string(EkstVPIFil.EkstVPILevNr)
                          .
      END. /* VPIFIL */
  END.
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

  ASSIGN
    pcKataloger = ""
    .
  FOR EACH EkstVPILev NO-LOCK WHERE
      EkstVPILev.Aktiv = TRUE:
      VPIFIL:
      FOR EACH EkstVPIFil OF EkstVPILev NO-LOCK WHERE
          EkstVPIFil.VPIFilAktiv = TRUE:

          /* VPI filer skal ikke lenger leses inn her. Det gjøres manuelt fra VPIMottakskontrollen */
          /* TN 28/7-08.                                                                           */
          IF (EkstVPIFil.VPIFilType = 1  AND
              EkstVPIFil.VPIUtpakkingsrutin = "xsport1vpiutpakk") THEN
              NEXT VPIFIL.

          IF NOT CAN-DO(pcKataloger,EkstVPIFil.VPIKatalog) THEN
          ASSIGN
            pcKataloger = pcKataloger + 
                          (IF pcKataloger = ""
                             THEN ""
                             ELSE ",") + 
                          EkstVPIFil.VPIKatalog
                          .
      END. /* VPIFIL */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVPIFilStatus dTables 
PROCEDURE GetVPIFilStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pdFilId        AS DEC NO-UNDO.
  DEF OUTPUT PARAMETER pbVPIFilStatus AS INT NO-UNDO.

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = pdFilId NO-ERROR.
  IF AVAILABLE VPIFilHode THEN
      pbVPIFilStatus = VPIFilHode.VPIFilStatus.
  ELSE
      pbVPIFilStatus = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetVPIStatusListe dTables  _DB-REQUIRED
PROCEDURE GetVPIStatusListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER piModus AS INT  NO-UNDO. /* 1 - Om [ALLE] skal være med */
  DEF OUTPUT PARAMETER pcTekst AS CHAR NO-UNDO.

  DEF VAR pcWork AS CHAR NO-UNDO.

  /* Statusliste */
  {syspara.i 1 12 2 pcTekst}

  IF piModus = 1 THEN
  DO:
      {syspara.i 1 100 1 pcWork}
      ASSIGN
          pcTekst = pcWork + ",0," + 
                    pcTekst
          .
  END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil dTables  _DB-REQUIRED
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER plFilId     LIKE VPIFilHode.FilId NO-UNDO.
  DEF OUTPUT PARAMETER pbOk        AS   LOG         NO-UNDO.
  DEF OUTPUT PARAMETER piAntLinjer AS   INT         NO-UNDO.
  
  DEF VAR pcKvitteringId  AS CHAR NO-UNDO.
  DEF VAR pcError         AS CHAR NO-UNDO.
  DEF VAR pcInnlesning AS CHAR NO-UNDO.
  DEF VAR pcLinje         AS CHAR NO-UNDO.
  DEF VAR pcBkuFil        AS CHAR NO-UNDO.

  DEF VAR piButikkNr      AS INT  NO-UNDO.
  DEF VAR piGruppeNr      AS INT  NO-UNDO.
  DEF VAR piKasseNr       AS INT  NO-UNDO.
  DEF VAR piTid           AS INT  NO-UNDO.
  DEF VAR pcReturn-Value  AS CHAR NO-UNDO.
  DEF VAR piEkstVPILevNr  AS INT  NO-UNDO.

  ASSIGN
      piTid          = TIME
      pcReturn-Value = ""
      .

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = plFilId NO-ERROR.
  IF NOT AVAILABLE VPIFilHode THEN
      RETURN "** VPIFilHode posten finnes ikke.".
  IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) = ? THEN
  DO:
      ASSIGN
          cTekst = "* Finner ikke filen " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn + ". Innlesning avbrutt.".

      /* Er filen borte fra katalogen, skal Filer posten bare slettes */
      /* Det skal ikke logges noe.                                    */
      /* Loggen vil bli slettet av databasetrigger.                   */
      DO TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF AVAILABLE VPIFilhode THEN
              DELETE VPIFilHode.
      END. /* TRANSACTION */
      RETURN cTekst.
  END.
  ELSE DO:
      ASSIGN
          cTekst = "Leser inn filen " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn + ".".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
  END.

  IF VPIFilHode.Storrelse > 0 THEN
  FILINN:
  DO ON ERROR UNDO, LEAVE:
    STATUS DEFAULT "".

    /* Koble filen til Ekstern VPI leverandør og filtype for å finne */
    /* programnavn for innlesning og oppdatering av filen.            */
    ASSIGN
        pcInnlesning = "".
    RUN koblevpifil.p 
        (INPUT VPIFilHode.VPIFilType, 
         INPUT ENTRY(1,VPIFilHode.FilNavn,".") + '|' + STRING(VPIFilHode.EkstVPILevNr),
         1, /* Innlesningsprogram skal hentes */
         OUTPUT pcInnlesning,
         OUTPUT piEkstVPILevNr).

    /* Sjekker at programmet finnes. */
    IF SEARCH(pcInnlesning + ".r") = ? THEN
    DO:
      IF pcInnlesning = "" THEN
      DO:
          ASSIGN
              cTekst = "* Det er ikke satt opp innlesningsprogram for denne filtypen. Innlesning avbrutt.".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
          /*RUN SetFilInnlest.*/
          RETURN "Det er ikke satt opp innlesningsprogram for denne filtypen.".
      END.
      ELSE DO:
          ASSIGN
              cTekst = "* Ukjent innlesningsprogram: " + pcInnlesning + ".r. innlesning avbrutt.".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
          /*RUN SetFilInnlest. */
          RETURN "Ukjent innlesningsprogram: " + pcInnlesning + ".r".
      END.
    END.

    /* Blanker RETURN-VALUE */
    RUN RensReturn.

    /* Leser inn fillinjene */
    RUN VALUE(pcInnlesning + ".p") 
        (INPUT  plFilId,
         INPUT  THIS-PROCEDURE,
         OUTPUT piAntLinjer
        ).
    IF RETURN-VALUE <> "" AND RETURN-VALUE <> 'OK' THEN
    DO:
        /*
        MESSAGE 'RETURN-VALUE fra innlesningsrutine: ' + pcInnlesning + '.p'   RETURN-VALUE skip 
        VIEW-AS ALERT-BOX.
        */
        pcError = RETURN-VALUE.
    END.
    ELSE
      ASSIGN
          pbOk = TRUE
          .

    STATUS DEFAULT "".
  END. /* FILINN */
  ELSE
      pbOk = TRUE. /* NullByte filer skal ferdigmerkes. */

  IF pbOk = FALSE THEN
  DO:
      DO TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
          ASSIGN
              VPIFilHode.VPIFilStatus = 9 /* Misslykket innlesning */
              VPIFilHode.AntLinjer    = piantLinjer
              .
      END. /* TRANSACTION */
      FIND CURRENT VPIFilHode NO-LOCK.

      ASSIGN
          cTekst = "* Det er oppdaget feil på filen. " + 
                   "Fil innlesning avbrutt. (Lest antall linjer = " + STRING(piAntLinjer) + ").".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
      RETURN cTekst.
  END.
  ELSE DO:
      DO TRANSACTION:
          FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
          ASSIGN
              pcBkuFil = ENTRY(1,VPIFilHode.FilNavn,'.') + '-' + 
                         REPLACE(STRING(TODAY),'/','') + "-" + 
                         REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.' +
                         ENTRY(2,VPIFilHode.FilNavn,'.')
              VPIFilHode.AntLinjer    = piantLinjer
              VPIFilHode.EkstVPILevNr = piEkstVPILevNr.
          IF VPIFilHode.VPIFilStatus < 3 THEN    
              VPIFilHode.VPIFilStatus = (IF VPIFilHode.Storrelse > 0 
                                          THEN 3 /* Vellykket innlesning */
                                          ELSE 5). /* Ferdigstatus.        */
              
      END. /* TRANSACTION */
      FIND CURRENT VPIFilHode NO-LOCK.

      /* Sikrer at backup katalog finnes. */
      OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
      /* Flytter filen til backup katalog. */
      OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) value(VPIFilHode.Katalog + "~\bku~\" + pcBkuFil).

      /* Renser bort fil */
      IF SEARCH(VPIFilHode.Katalog + "~\bku~\" + pcBkuFil) <> ? THEN
      DO:
          /* Filen tas bort fra katalogen. */
          IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
              OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
      END.
      ELSE /* */.
     
      ASSIGN
          cTekst = "Fil innlesning ferdig. (Tid brukt: " + 
                    string(TIME - piTid,"HH:MM:SS") + " Lest antall linjer = " + STRING(piAntLinjer) + ").".           
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
      RETURN cTekst.
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

  DEF VAR piLoop1        AS INT  NO-UNDO.
  DEF VAR piLoop2        AS INT  NO-UNDO.
  DEF VAR pcKatalog      AS CHAR NO-UNDO.
  DEF VAR pcEkstent      AS CHAR NO-UNDO.
  DEF VAR pcKatFil       AS CHAR FORMAT "x(250)" NO-UNDO.
  DEF VAR plFilId        AS DEC  NO-UNDO.
  DEF VAR pcBegins       AS CHAR NO-UNDO.
  DEF VAR piEntries      AS INT  NO-UNDO.
  DEF VAR piFilType      AS INT  NO-UNDO.
  DEF VAR piEkstVPILevNr AS INT  NO-UNDO.

  DEF VAR pcFileName    AS CHAR FORMAT "x(130)" NO-UNDO.
  DEF VAR pcFilePath    AS CHAR FORMAT "x(100)" NO-UNDO.
  DEF VAR pcFileAttrib  AS CHAR FORMAT "x(15)" NO-UNDO.

  /* Ingenting å sjekke */
  IF pcKataloger = "" THEN
    RETURN "Ingen kataloger som skal sjekkes.".

  /* Behandler alle filer. */
  DO piLoop1 = 1 TO NUM-ENTRIES(pcFiler):
    /*
      1. --> _ og _
      2. --> _ og *
      3. --> * og _
      4. --> * og *
    */
    ASSIGN
      piEkstVPILevNr = INT(ENTRY(5,ENTRY(piLoop1,pcFiler),"|"))
      pcBegins  = /* Legger på stjerne først, hvis det er "Inneholder" */
                  (IF (ENTRY(3,entry(piLoop1,pcFiler),"|") = "3" OR ENTRY(3,entry(piLoop1,pcFiler),"|") = "4")
                    THEN "*"
                    ELSE "") + 
                  /* Legger på filnavnet */
                  ENTRY(1,
                          ENTRY(piLoop1,pcFiler),"|"
                       ) +
                  /* Legger på stjerne hvis det er starter med, eller inneholder */  
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

    /* For hver ekstent i katalog leses og behandles fillisten i katalogen. */
    DO piLoop2 = 1 TO NUM-ENTRIES(pcKataloger):
      ASSIGN
        pcKatFil  = ENTRY(piLoop2,pcKataloger)
        .
       /* Skaper katalogen hvis den ikke finnes */
      OS-CREATE-DIR value(pcKatFil).

      /* Leser fillisten fra katalogen. */
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
          IF ENTRY(1,pcFileName,".") MATCHES pcBegins THEN. /* Gjør ingenting. */
          ELSE
            NEXT FILINPUT. /* Hopp over denne */

          /* Kun filer som oppfyller extent masken skal inn */
          IF piEntries > 1 THEN
          DO:
            IF ENTRY(piEntries,pcfileName,".") MATCHES pcEkstent THEN. /* gjør ingenting. */
            ELSE
              NEXT FILINPUT. /* Hopp over denne */
          END.
          /* Filer som ikke har ekstent, skal ikke importeres. */
          ELSE IF pcEkstent <> '' THEN 
              NEXT FILINPUT. /* Hopp over denne */
          
          /* Oppretter posten i filen. */
          IF NOT CAN-FIND(FIRST 
                          VPIFilHode WHERE
                          VPIFilHode.FilNavn   = pcFileName AND
                          VPIFilHode.Dato      = FILE-INFO:FILE-MOD-DATE AND
                          VPIFilHode.Kl        = string(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS") AND
                          VPIFilHode.Storrelse = FILE-INFO:FILE-SIZE AND
                          VPIFilHode.Katalog   = pcKatFil
                         ) THEN
          DO:
            /* Finner FilId */
            FIND LAST VPIFilHode NO-LOCK NO-ERROR.
            IF AVAILABLE VPIFilHode THEN
              plFilId = VPIFilHode.FilId + 1.
            ELSE
              plFilId = 1.
            CREATE VPIFilHode.
            ASSIGN
              VPIFilHode.FilId        = plFilId
              VPIFilHode.FilNavn      = pcFileName
              VPIFilHode.Katalog      = pcKatFil
              VPIFilHode.Dato         = FILE-INFO:FILE-MOD-DATE
              VPIFilHode.Kl           = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
              VPIFilHode.Storrelse    = FILE-INFO:FILE-SIZE
              VPIFilHode.AntLinjer    = 0
              VPIFilHode.VPIFilType   = piFilType
              VPIFilHode.VPIFilStatus = 1
              VPIFilHode.EkstVPILevNr = piEkstVPILevNr
              .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PakkUtFil dTables  _DB-REQUIRED
PROCEDURE PakkUtFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER pcFilId AS  CHAR NO-UNDO.

  FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = dec(pcFilId) NO-ERROR.
    IF NOT AVAILABLE VPIFilHode THEN
    DO:
      PUBLISH "VPIFilLogg" ('Finner ikke VPIFilhode med VPIFilid: ' + STRING(pcFilId)).
      RETURN.
  END.
  
  FIND FIRST EkstVPIFil NO-LOCK WHERE
      EkstVPIFil.EkstVPILevNr = VPIFilHode.EkstVPILevNr AND
      EkstVPIFil.VPIFilTypeNr = VPIFilHode.VPIFilType
      NO-ERROR.
  IF NOT AVAILABLE EkstVPIFil THEN
  DO:
      PUBLISH "VPIFilLogg" ('Finner ikke EkstVPILev med  EkstVPILevNr: ' + STRING(VPIFilHode.EkstVPILevNr) + ' og ' + STRING(VPIFilHode.VPIFilType)).
      RETURN.
  END.

  PAKKUT:
  DO ON ERROR UNDO, LEAVE:
      
    IF SEARCH(EkstVPIFil.VPIUtpakkingsrutine + ".r") <> ? THEN
    DO:
        PUBLISH "VPIFilLogg" ('Starter utpakkingsrutine: ' + EkstVPIFil.VPIUtpakkingsrutine + ".r").
        
        RUN VALUE(EkstVPIFil.VPIUtpakkingsrutine)
            (INPUT VPIFilHode.FilId).
    END.
    ELSE 
      PUBLISH "VPIFilLogg" ('Finner ikke utpakkingsrutine: ' + EkstVPIFil.VPIUtpakkingsrutine + ".r").
        
  END. /* PAKKUT */

  RETURN "Utpakking av fil ferdig.".

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
  DEF VAR piLoop       AS INT NO-UNDO.
  DEF VAR pbOk         AS LOG NO-UNDO.
  DEF VAR pcVPIFilHode AS CHAR NO-UNDO.

  /* Sendes blank liste inn, skal alle ikke innleste poster tas med. */
  /* Poster som er slettet, skal heller ikke tas med.                */
  FOR EACH VPIFilHode NO-LOCK WHERE
    VPIFilHode.VPIFilStatus <= 2:
    RUN SlettFil (INPUT VPIFilHode.FilId, INPUT TRUE, OUTPUT pbOk).
  END.
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
  DEF INPUT  PARAMETER plFilId   LIKE VPIFilHode.FilId NO-UNDO.
  DEF INPUT  PARAMETER pbUansett AS   LOG         NO-UNDO.
  DEF OUTPUT PARAMETER pbOk      AS   LOG         NO-UNDO.

  DEF VAR piAntLinjer    AS INT NO-UNDO.
  DEF VAR piTotAntLinjer AS INT NO-UNDO.

  SLETTFIL:
  DO:
    FIND VPIFilHode NO-LOCK WHERE
      VPIFilHode.FilId = plFilId NO-ERROR.
    IF NOT AVAILABLE VPIFilHode THEN
      RETURN.

    ASSIGN
        piTotAntLinjer = VPIFilHode.AntLinjer
        .

    /* Fjerner tilhørende linjer */
    FOR EACH VPIFilLinje OF VPIFilHode EXCLUSIVE-LOCK:
      ASSIGN
          piAntLinjer = piAntLinjer + 1
          .
      STATUS DEFAULT "Sletter linje " + STRING(piAntLinjer) + 
                     " av " + STRING(piTotAntLinjer) + ".".
      DELETE VPIFilLinje.
    END.
    /* Fjerner tilhørende loggposter */
    piAntLinjer = 0.
    FOR EACH VPIFilLogg OF VPIFilHode EXCLUSIVE-LOCK:
      ASSIGN
          piAntLinjer = piAntLinjer + 1
          .
      STATUS DEFAULT "Sletter logglinje " + STRING(piAntLinjer) + ".".
      DELETE VPIFilLogg.
    END.
    DO TRANSACTION:
        FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
        /* Sletter posten */
        DELETE VPIFilHode.
    END.

    ASSIGN
        pbOk = TRUE
        .
  END. /* SLETTFIL */

  STATUS DEFAULT "".


  IF pbOk = FALSE THEN
  DO:
      /*
      RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                      STRING(TIME,"HH:MM:SS") + " " + userid("skotex")
                     + " -  Sletting av fil avbrutt." + CHR(1) + "1").
      */               
      RETURN "** Sletting av fil avbrutt.".

  END.
  ELSE  DO:
      IF pbUansett = FALSE THEN
      DO:
        /*
        RUN NyFilLogg (INPUT plFilId, STRING(TODAY) + " " + 
                        STRING(TIME,"HH:MM:SS") + " " + userid("skotex") + 
                       " - Fil slettet.").
        */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DatoTid dTables  _DB-REQUIRED
FUNCTION DatoTid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = STRING(RowObject.Dato) + " " + 
                STRING(RowObject.Kl)
      .
  RETURN pcTekst.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION EDatoTid dTables  _DB-REQUIRED
FUNCTION EDatoTid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = STRING(RowObject.EDato) + " " + 
                STRING(RowObject.ETid,"HH:MM") + " " +
                RowObject.EAv
      .
  RETURN pcTekst.   /* Function return value. */

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
                (IF RowObject.ODato <> ? 
                  THEN STRING(RowOBject.ODato)
                   ELSE "") + " " + 
                string(RowObject.OTid,"HH:MM:SS") + " " + 
                STRING(RowObject.OAv) + " " +
                "Endret: " + 
                (IF RowObject.EDato <> ? 
                   THEN STRING(RowOBject.EDato)
                   ELSE "") + " " + 
                STRING(RowObject.ETid,"HH:MM:SS") + " " +
                RowObject.EAv
      .

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FilType dTables  _DB-REQUIRED
FUNCTION FilType RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION NavnEkstVPILev dTables  _DB-REQUIRED
FUNCTION NavnEkstVPILev RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND EkstVPILev NO-LOCK WHERE
      EkstVPILev.EkstVPILevNr = RowOBject.EkstVPILevNr NO-ERROR.

  IF AVAILABLE EkstVPILev THEN
      RETURN EkstVPILev.KortNavn.
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VPIFilStatus dTables  _DB-REQUIRED
FUNCTION VPIFilStatus RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  {syspara.i 1 12 2 pcTekst}

  IF NUM-ENTRIES(pcTekst) >= 10 THEN
  DO:
      CASE RowObject.VPIFilStatus:
          WHEN 1 THEN pcTekst = ENTRY(1,pcTekst).
          WHEN 2 THEN pcTekst = ENTRY(3,pcTekst).
          WHEN 3 THEN pcTekst = ENTRY(5,pcTekst).
          WHEN 4 THEN pcTekst = ENTRY(7,pcTekst).
          WHEN 5 THEN pcTekst = ENTRY(9,pcTekst).
          WHEN 7 THEN pcTekst = "LESES INN".
          WHEN 8 THEN pcTekst = "INNLEST".
          WHEN 9 THEN pcTekst = "FEIL".
      END CASE.
  END.
  ELSE
      pcTekst = "* Ukjent *".

  RETURN pcTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

