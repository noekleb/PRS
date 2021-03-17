&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_BestHodeX NO-UNDO LIKE BestHode
       FIELD VgLopNr AS CHARACTER
       FIELD ArtBeskr AS CHARACTER
       FIELD Rest AS INTEGER
       FIELD LevNavn AS CHARACTER
       FIELD AvdelingNr LIKE Avdeling.AvdelingNr
       FIELD AvdBeskr AS CHARACTER
       FIELD Hg LIKE HuvGr.Hg
       FIELD HgBeskr AS CHAR
       FIELD Vg LIKE VarGr.Vg
       FIELD VgBeskr AS CHAR
       FIELD KategoriBeskr AS CHAR
       .



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

DEFINE TEMP-TABLE TT_BestHode NO-UNDO LIKE BestHode
       FIELD VgLopNr AS CHARACTER
       FIELD ArtBeskr AS CHARACTER
       FIELD Rest AS INTEGER
       FIELD LevNavn AS CHARACTER
       FIELD AvdelingNr LIKE Avdeling.AvdelingNr
       FIELD AvdBeskr AS CHARACTER
       FIELD Hg LIKE HuvGr.Hg
       FIELD HgBeskr AS CHAR
       FIELD Vg LIKE VarGr.Vg
       FIELD VgBeskr AS CHAR
       FIELD KategoriBeskr AS CHAR
       FIELD Sasong LIKE Sasong.sasong
       FIELD TotTBProc AS DECI
       FIELD NettoVerdi AS DECI
       INDEX BestCL IS PRIMARY BestNr CL.

DEFINE TEMP-TABLE TT_BestHodeTMP NO-UNDO LIKE TT_BestHode.

DEFINE TEMP-TABLE TT_Artiklar
    FIELD Artikkelnr AS DECI
    FIELD Vg         AS INTE
    FIELD Verdier    AS CHAR
    INDEX Artikkelnr Artikkelnr.

DEFINE TEMP-TABLE TT_Vg
    FIELD Vg           AS INTE
    FIELD Verdier    AS CHAR
    INDEX Vg Vg.

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
&Scoped-define INTERNAL-TABLES BestHode

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  BestNr AnonseArtikkel ArtikkelNr Beskrivelse BestillingsDato BestStat~
 BestType BrukerID DirekteLev EDato EkstId ETid LapTop LevDato LevFargKod~
 LevKod LevNr LevTid Merknad OrdreNr RegistrertAv RegistrertDato~
 RegistrertTid SendtAv SendtDato SendtTid StrTypeID TotAntPar TotDbKr~
 TotInnkjVerdi TotInnLev TotMakulert TotOverLev TotSalgsVerdi CL~
 BekreftetDato
&Scoped-define ENABLED-FIELDS-IN-BestHode BestNr AnonseArtikkel ArtikkelNr ~
Beskrivelse BestillingsDato BestStat BestType BrukerID DirekteLev EDato ~
EkstId ETid LapTop LevDato LevFargKod LevKod LevNr LevTid Merknad OrdreNr ~
RegistrertAv RegistrertDato RegistrertTid SendtAv SendtDato SendtTid ~
StrTypeID TotAntPar TotDbKr TotInnkjVerdi TotInnLev TotMakulert TotOverLev ~
TotSalgsVerdi CL BekreftetDato 
&Scoped-Define DATA-FIELDS  BestNr AnonseArtikkel ArtikkelNr Beskrivelse BestillingsDato BestStat~
 BestType BrukerID DirekteLev EDato EkstId ETid LapTop LevDato LevFargKod~
 LevKod LevNr LevTid Merknad OrdreNr RegistrertAv RegistrertDato~
 RegistrertTid SendtAv SendtDato SendtTid StrTypeID TotAntPar TotDbKr~
 TotInnkjVerdi TotInnLev TotMakulert TotOverLev TotSalgsVerdi CL~
 BekreftetDato
&Scoped-define DATA-FIELDS-IN-BestHode BestNr AnonseArtikkel ArtikkelNr ~
Beskrivelse BestillingsDato BestStat BestType BrukerID DirekteLev EDato ~
EkstId ETid LapTop LevDato LevFargKod LevKod LevNr LevTid Merknad OrdreNr ~
RegistrertAv RegistrertDato RegistrertTid SendtAv SendtDato SendtTid ~
StrTypeID TotAntPar TotDbKr TotInnkjVerdi TotInnLev TotMakulert TotOverLev ~
TotSalgsVerdi CL BekreftetDato 
&Scoped-Define MANDATORY-FIELDS  BestType LevKod LevNr SendtAv
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dbesthode.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH BestHode NO-LOCK ~
    BY BestHode.BestNr DESCENDING INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH BestHode NO-LOCK ~
    BY BestHode.BestNr DESCENDING INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main BestHode
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main BestHode


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HarBestillt dTables  _DB-REQUIRED
FUNCTION HarBestillt RETURNS LOGICAL
  ( INPUT iButnr AS INTEGER,INPUT iBestNr AS INTEGER,INPUT iBestStat AS INTEGER,OUTPUT cTotAntStr AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      BestHode SCROLLING.
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
   Temp-Tables and Buffers:
      TABLE: TT_BestHodeX T "?" NO-UNDO SkoTex BestHode
      ADDITIONAL-FIELDS:
          FIELD VgLopNr AS CHARACTER
          FIELD ArtBeskr AS CHARACTER
          FIELD Rest AS INTEGER
          FIELD LevNavn AS CHARACTER
          FIELD AvdelingNr LIKE Avdeling.AvdelingNr
          FIELD AvdBeskr AS CHARACTER
          FIELD Hg LIKE HuvGr.Hg
          FIELD HgBeskr AS CHAR
          FIELD Vg LIKE VarGr.Vg
          FIELD VgBeskr AS CHAR
          FIELD KategoriBeskr AS CHAR
          
      END-FIELDS.
   END-TABLES.
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
     _TblList          = "skotex.BestHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.BestHode.BestNr|no"
     _FldNameList[1]   > skotex.BestHode.BestNr
"BestNr" "BestNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[2]   > skotex.BestHode.AnonseArtikkel
"AnonseArtikkel" "AnonseArtikkel" ? ? "logical" ? ? ? ? ? ? yes ? no 2.8 yes ""
     _FldNameList[3]   > skotex.BestHode.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[4]   > skotex.BestHode.Beskrivelse
"Beskrivelse" "Beskrivelse" ? ? "character" ? ? ? ? ? ? yes ? no 50 yes ""
     _FldNameList[5]   > skotex.BestHode.BestillingsDato
"BestillingsDato" "BestillingsDato" ? "99/99/99" "date" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[6]   > skotex.BestHode.BestStat
"BestStat" "BestStat" ? ? "integer" ? ? ? ? ? ? yes ? no 2.8 yes ""
     _FldNameList[7]   > skotex.BestHode.BestType
"BestType" "BestType" ? ? "integer" ? ? ? ? ? ? yes ? yes 8.8 yes ""
     _FldNameList[8]   > skotex.BestHode.BrukerID
"BrukerID" "BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[9]   > skotex.BestHode.DirekteLev
"DirekteLev" "DirekteLev" ? ? "logical" ? ? ? ? ? ? yes ? no 3.2 yes ""
     _FldNameList[10]   > skotex.BestHode.EDato
"EDato" "EDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[11]   > skotex.BestHode.EkstId
"EkstId" "EkstId" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[12]   > skotex.BestHode.ETid
"ETid" "ETid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[13]   > skotex.BestHode.LapTop
"LapTop" "LapTop" ? ? "logical" ? ? ? ? ? ? yes ? no 19.8 yes ""
     _FldNameList[14]   > skotex.BestHode.LevDato
"LevDato" "LevDato" ? "99/99/99" "date" ? ? ? ? ? ? yes ? no 13.4 yes ""
     _FldNameList[15]   > skotex.BestHode.LevFargKod
"LevFargKod" "LevFargKod" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[16]   > skotex.BestHode.LevKod
"LevKod" "LevKod" ? ? "character" ? ? ? ? ? ? yes ? yes 30 yes ""
     _FldNameList[17]   > skotex.BestHode.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? yes 7.2 yes ""
     _FldNameList[18]   > skotex.BestHode.LevTid
"LevTid" "LevTid" ? ? "character" ? ? ? ? ? ? yes ? no 7.6 yes ""
     _FldNameList[19]   > skotex.BestHode.Merknad
"Merknad" "Merknad" ? ? "character" ? ? ? ? ? ? yes ? no 31 yes ""
     _FldNameList[20]   > skotex.BestHode.OrdreNr
"OrdreNr" "OrdreNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes ""
     _FldNameList[21]   > skotex.BestHode.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[22]   > skotex.BestHode.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[23]   > skotex.BestHode.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[24]   > skotex.BestHode.SendtAv
"SendtAv" "SendtAv" ? ? "character" ? ? ? ? ? ? yes ? yes 10 yes ""
     _FldNameList[25]   > skotex.BestHode.SendtDato
"SendtDato" "SendtDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[26]   > skotex.BestHode.SendtTid
"SendtTid" "SendtTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[27]   > skotex.BestHode.StrTypeID
"StrTypeID" "StrTypeID" ? ? "integer" ? ? ? ? ? ? yes ? no 7.4 yes ""
     _FldNameList[28]   > skotex.BestHode.TotAntPar
"TotAntPar" "TotAntPar" ? ? "decimal" ? ? ? ? ? ? yes ? no 12.4 yes ""
     _FldNameList[29]   > skotex.BestHode.TotDbKr
"TotDbKr" "TotDbKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[30]   > skotex.BestHode.TotInnkjVerdi
"TotInnkjVerdi" "TotInnkjVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.6 yes ""
     _FldNameList[31]   > skotex.BestHode.TotInnLev
"TotInnLev" "TotInnLev" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[32]   > skotex.BestHode.TotMakulert
"TotMakulert" "TotMakulert" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[33]   > skotex.BestHode.TotOverLev
"TotOverLev" "TotOverLev" ? ? "decimal" ? ? ? ? ? ? yes ? no 10.6 yes ""
     _FldNameList[34]   > skotex.BestHode.TotSalgsVerdi
"TotSalgsVerdi" "TotSalgsVerdi" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.8 yes ""
     _FldNameList[35]   > skotex.BestHode.CL
"CL" "CL" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[36]   > skotex.BestHode.BekreftetDato
"BekreftetDato" "BekreftetDato" ? ? "date" ? ? ? ? ? ? yes ? no 13.4 yes ?
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestHodeToTT dTables  _DB-REQUIRED
PROCEDURE BestHodeToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
/*   DEFINE OUTPUT PARAMETER TABLE-HANDLE TTH. */
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cQuery    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop     AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cTotAntStr      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.

  EMPTY TEMP-TABLE TT_BestHode.
  EMPTY TEMP-TABLE TT_BestHodeTMP.
  CREATE TT_BestHodeTMP. /* en temporär record för att kunna summera */
  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
      ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
             pcVerdier   = ENTRY(2,cXFilter,";").
      IF pcFeltListe <> pcVerdier THEN  /* */
          ASSIGN lUtvidetFilter = TRUE.
  END.
  /* Specialhantering av Vis/But                                       */
  /* Parameter cXParam används för att ge möjlighet att visa per butik */
  /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J          */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).
/*   ASSIGN cQuery = DYNAMIC-FUNCTION('getQueryString':U). */
  DYNAMIC-FUNCTION('closeQuery':U).
/*   DYNAMIC-FUNCTION('setQueryString':U,                                                               */
/*   INPUT REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ") /* CHARACTER */). */
  DYNAMIC-FUNCTION('openQuery':U).
  ASSIGN lFirst = FALSE
         rRowId = ?.
  REPEAT:
      IF lFirst = FALSE THEN DO:
          RUN fetchFirst.
          ASSIGN lFirst = TRUE.
      END.
      ELSE
          RUN fetchNext.
      IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
          LEAVE.
      ELSE
          ASSIGN rRowId = ROWID(RowObject).
      FIND ArtBas OF RowObject NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtBas THEN
          NEXT.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      IF NOT AVAIL LevBas THEN
          NEXT.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
      FIND VgKat WHERE VgKat.Vg = ArtBas.Vg AND VgKat.VgKat = ArtBas.VgKat NO-LOCK NO-ERROR.
      FIND Kategori OF VgKat NO-LOCK NO-ERROR.
      FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
      IF lUtvidetFilter THEN DO iCount = 1 TO NUM-ENTRIES(pcFeltListe,CHR(1)):
          ASSIGN lIkkeTreff = FALSE.
          CASE ENTRY(iCount,pcFeltListe,CHR(1)):
              WHEN "AvdelingNr" THEN DO:
                  IF NOT AVAIL Avdeling OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Avdeling.AvdelingNr)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "HuvGr" THEN DO:
                  IF NOT AVAIL HuvGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.Hg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "VarGr" THEN DO:
                  IF NOT AVAIL VarGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VarGr.Vg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Sasong" THEN DO:
                  IF NOT AVAIL Sasong OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Sasong.Sasong)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Aktivitet" THEN DO:
                  IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgAkt OF VarGr) THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                  END.
                  IF lIkkeTreff = FALSE THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                      FOR EACH VgAkt OF VarGr NO-LOCK:
                          IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgAkt.AktNr)) THEN DO:
                              ASSIGN lIkkeTreff = FALSE.
                              LEAVE.
                          END.
                      END.
                  END.
              END.
              WHEN "Kategori" THEN DO:
                  IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgKat OF VarGr) THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                  END.
                  IF lIkkeTreff = FALSE THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                      FOR EACH VgKat OF VarGr NO-LOCK:
                          IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgKat.KatNr)) THEN DO:
                              ASSIGN lIkkeTreff = FALSE.
                              LEAVE.
                          END.
                      END.
                  END.
              END.
          END CASE.
          IF lIkkeTreff THEN
              LEAVE.
      END.
      IF lIkkeTreff THEN
          NEXT.
      DO iCount = 1 TO NUM-ENTRIES(cButiker):
          IF NOT HarBestillt(INT(ENTRY(iCount,cButiker)),RowObject.BestNr,RowObject.BestStat,cTotAntStr) THEN NEXT.
          DO:
              BUFFER-COPY RowObject EXCEPT RowObject.CL RowObject.TotAntPar RowObject.TotDbKr RowObject.TotInnkjVerdi RowObject.TotInnLev RowObject.TotMakulert RowObject.TotOverLev RowObject.TotSalgsVerdi TO TT_BestHodeTMP.
              ASSIGN TT_BestHodeTMP.CL            = IF lVisPerBut THEN INT(ENTRY(iCount,cButiker)) ELSE ?
                     TT_BestHodeTMP.TotAntPar     = DECI(ENTRY(1,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotDbKr       = DECI(ENTRY(2,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotInnkjVerdi = DECI(ENTRY(3,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.NettoVerdi    = TT_BestHodeTMP.TotDbKr + TT_BestHodeTMP.TotInnkjVerdi
                     TT_BestHodeTMP.TotInnLev     = DECI(ENTRY(4,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotMakulert   = DECI(ENTRY(5,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotOverLev    = DECI(ENTRY(6,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotSalgsVerdi = DECI(ENTRY(7,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.Rest          = INT(RowObject.TotAntPar - ABS(RowObject.TotInnLev) - RowObject.TotMakulert + RowObject.TotOverLev)
                     
                     .
              IF NOT lVisPerBut THEN
                  FIND FIRST TT_BestHode WHERE TT_BestHode.BestNr = RowObject.BestNr NO-LOCK NO-ERROR.
              IF NOT AVAIL TT_BestHode THEN DO:
                  CREATE TT_BestHode.
                  BUFFER-COPY TT_BestHodeTMP TO TT_BestHode.
                  ASSIGN TT_BestHode.VgLopNr  = STRING(ArtBas.Vg) + "/" + STRING(ArtBas.LopNr)
                         TT_BestHode.ArtBeskr = ArtBas.Beskr
                         TT_BestHode.LevNavn  = LevBas.LevNamn
                         TT_BestHode.AvdelingNr = IF AVAIL Avdeling THEN Avdeling.AvdelingNr ELSE 0
                         TT_BestHode.AvdBeskr   = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE ""
                         TT_BestHode.Hg         = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 0
                         TT_BestHode.HgBeskr    = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE ""
                         TT_BestHode.Vg         = IF AVAIL VarGr THEN VarGr.Vg ELSE 0
                         TT_BestHode.VgBeskr    = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE ""
                         TT_BestHode.KategoriBeskr = IF AVAIL Kategori THEN Kategori.Beskrivelse ELSE ""
                         TT_BestHode.Sasong     = ArtBas.Sasong.
              END.
              ELSE DO:
                  ASSIGN TT_BestHode.TotAntPar     = TT_BestHode.TotAntPar      + TT_BestHodeTMP.TotAntPar    
                         TT_BestHode.TotDbKr       = TT_BestHode.TotDbKr        + TT_BestHodeTMP.TotDbKr      
                         TT_BestHode.TotInnkjVerdi = TT_BestHode.TotInnkjVerdi  + TT_BestHodeTMP.TotInnkjVerdi
                         TT_BestHode.NettoVerdi    = TT_BestHode.NettoVerdi     + TT_BestHodeTMP.NettoVerdi
                         TT_BestHode.TotInnLev     = TT_BestHode.TotInnLev      + TT_BestHodeTMP.TotInnLev    
                         TT_BestHode.TotMakulert   = TT_BestHode.TotMakulert    + TT_BestHodeTMP.TotMakulert  
                         TT_BestHode.TotOverLev    = TT_BestHode.TotOverLev     + TT_BestHodeTMP.TotOverLev   
                         TT_BestHode.TotSalgsVerdi = TT_BestHode.TotSalgsVerdi  + TT_BestHodeTMP.TotSalgsVerdi
                         TT_BestHode.Rest          = TT_BestHode.Rest           + TT_BestHodeTMP.Rest.        
              END.
          END.
          RELEASE TT_BestHode.
      END.
      RELEASE Avdeling.
      RELEASE HuvGr.
      RELEASE VarGr.
      RELEASE VgKat.
      RELEASE Kategori.
      
  END.
  DYNAMIC-FUNCTION('closeQuery':U).
  FOR EACH TT_BestHode:
      TT_BestHode.TotTBProc        = ROUND((TT_BestHode.TotDbKr / (TT_BestHode.TotInnkjVerdi + TT_BestHode.TotDbKr)) * 100,1) /* + 1 */ NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
          TT_BestHode.TotTBProc = 0.
  END.
  ASSIGN TTH = BUFFER TT_BestHode:HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BestHodeToTTAnalys dTables  _DB-REQUIRED
PROCEDURE BestHodeToTTAnalys :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
/*   DEFINE OUTPUT PARAMETER TABLE-HANDLE TTH. */
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cQuery    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop     AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cTotAntStr     AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cButikkVerdier AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cVerdier       AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iLookup        AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  iSasong        AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE cTmpEntry AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cSumEntry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.

  EMPTY TEMP-TABLE TT_BestHode.
  EMPTY TEMP-TABLE TT_BestHodeTMP.
  EMPTY TEMP-TABLE TT_Artiklar.
  EMPTY TEMP-TABLE TT_Vg.

  CREATE TT_BestHodeTMP. /* en temporär record för att kunna summera */
  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
      ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
             pcVerdier   = ENTRY(2,cXFilter,";").
      IF pcFeltListe <> pcVerdier THEN  /* */
          ASSIGN lUtvidetFilter = TRUE.
  END.
  /* cVerdier skall flagga om vi har artikeln har best på artikeln. cVedier kopieras in  */
  /* på TT_Artiklerrecorden. Vi initierar sista entryt med '1' = 'totalbutiken' */
  ASSIGN cButikkVerdier = cButiker + ",99999"
         cVerdier       = ";" + FILL(",;",NUM-ENTRIES(cButiker)).

  iSasong = 1. /* detta skall göras om och loopas run de två säsongbegreppen samt tidsperiode för respektive säsongbegrepp */

  /* Specialhantering av Vis/But                                       */
  /* Parameter cXParam används för att ge möjlighet att visa per butik */
  /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J          */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).
/*   ASSIGN cQuery = DYNAMIC-FUNCTION('getQueryString':U). */
  DYNAMIC-FUNCTION('closeQuery':U).
/*   DYNAMIC-FUNCTION('setQueryString':U,                                                               */
/*   INPUT REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ") /* CHARACTER */). */
  DYNAMIC-FUNCTION('openQuery':U).
  ASSIGN lFirst = FALSE
         rRowId = ?.
  REPEAT:
      IF lFirst = FALSE THEN DO:
          RUN fetchFirst.
          ASSIGN lFirst = TRUE.
      END.
      ELSE
          RUN fetchNext.
      IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
          LEAVE.
      ELSE
          ASSIGN rRowId = ROWID(RowObject).
      FIND ArtBas OF RowObject NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtBas THEN
          NEXT.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      IF NOT AVAIL LevBas THEN
          NEXT.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
      FIND VgKat WHERE VgKat.Vg = ArtBas.Vg AND VgKat.VgKat = ArtBas.VgKat NO-LOCK NO-ERROR.
      FIND Kategori OF VgKat NO-LOCK NO-ERROR.
      FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
      IF lUtvidetFilter THEN DO iCount = 1 TO NUM-ENTRIES(pcFeltListe,CHR(1)):
          ASSIGN lIkkeTreff = FALSE.
          CASE ENTRY(iCount,pcFeltListe,CHR(1)):
              WHEN "AvdelingNr" THEN DO:
                  IF NOT AVAIL Avdeling OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Avdeling.AvdelingNr)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "HuvGr" THEN DO:
                  IF NOT AVAIL HuvGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.Hg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "VarGr" THEN DO:
                  IF NOT AVAIL VarGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VarGr.Vg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Sasong" THEN DO:
                  IF NOT AVAIL Sasong OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Sasong.Sasong)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Aktivitet" THEN DO:
                  IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgAkt OF VarGr) THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                  END.
                  IF lIkkeTreff = FALSE THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                      FOR EACH VgAkt OF VarGr NO-LOCK:
                          IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgAkt.AktNr)) THEN DO:
                              ASSIGN lIkkeTreff = FALSE.
                              LEAVE.
                          END.
                      END.
                  END.
              END.
              WHEN "Kategori" THEN DO:
                  IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgKat OF VarGr) THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                  END.
                  IF lIkkeTreff = FALSE THEN DO:
                      ASSIGN lIkkeTreff = TRUE.
                      FOR EACH VgKat OF VarGr NO-LOCK:
                          IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgKat.KatNr)) THEN DO:
                              ASSIGN lIkkeTreff = FALSE.
                              LEAVE.
                          END.
                      END.
                  END.
              END.
          END CASE.
          IF lIkkeTreff THEN
              LEAVE.
      END.
      IF lIkkeTreff THEN
          NEXT.
      DO iCount = 1 TO NUM-ENTRIES(cButiker):
          IF NOT HarBestillt(INT(ENTRY(iCount,cButiker)),RowObject.BestNr,RowObject.BestStat,cTotAntStr) THEN NEXT.
          DO:
              /* Här har butiken beställt */
              FIND TT_Artiklar WHERE TT_Artiklar.Artikkelnr = ArtBas.Artikkelnr NO-ERROR.
              IF NOT AVAIL TT_Artiklar THEN DO:
                  CREATE TT_Artiklar.
                  ASSIGN TT_Artiklar.Artikkelnr = Artbas.Artikkelnr
                         TT_Artiklar.Vg         = ArtBas.Vg
                         TT_Artiklar.Verdier    = cVerdier.
                  cTmpEntry = ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier).
                  ENTRY(iSasong,cTmpEntry,";") = "1".
                  ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier) = cTmpEntry.
              END.
              iLookup = LOOKUP(ENTRY(iCount,cButiker),cButikkVerdier).
              IF iLookup > 0 THEN DO:
                  cTmpEntry = ENTRY(iLookup,TT_Artiklar.Verdier).
                  ENTRY(iSasong,cTmpEntry,";") = "1".
                  ENTRY(iLookup,TT_Artiklar.Verdier) = cTmpEntry.
              END.
              BUFFER-COPY RowObject EXCEPT RowObject.CL RowObject.TotAntPar RowObject.TotDbKr RowObject.TotInnkjVerdi RowObject.TotInnLev RowObject.TotMakulert RowObject.TotOverLev RowObject.TotSalgsVerdi TO TT_BestHodeTMP.
              ASSIGN TT_BestHodeTMP.CL            = IF lVisPerBut THEN INT(ENTRY(iCount,cButiker)) ELSE ?
                     TT_BestHodeTMP.TotAntPar     = DECI(ENTRY(1,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotDbKr       = DECI(ENTRY(2,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotInnkjVerdi = DECI(ENTRY(3,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotInnLev     = DECI(ENTRY(4,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotMakulert   = DECI(ENTRY(5,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotOverLev    = DECI(ENTRY(6,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.TotSalgsVerdi = DECI(ENTRY(7,cTotAntStr,CHR(1)))
                     TT_BestHodeTMP.Rest          = INT(RowObject.TotAntPar - ABS(RowObject.TotInnLev) - RowObject.TotMakulert + RowObject.TotOverLev)
                     .
              IF NOT lVisPerBut THEN
                  FIND FIRST TT_BestHode WHERE TT_BestHode.BestNr = RowObject.BestNr NO-LOCK NO-ERROR.
              IF NOT AVAIL TT_BestHode THEN DO:
                  CREATE TT_BestHode.
                  BUFFER-COPY TT_BestHodeTMP TO TT_BestHode.
                  ASSIGN TT_BestHode.VgLopNr  = STRING(ArtBas.Vg) + "/" + STRING(ArtBas.LopNr)
                         TT_BestHode.ArtBeskr = ArtBas.Beskr
                         TT_BestHode.LevNavn  = LevBas.LevNamn
                         TT_BestHode.AvdelingNr = IF AVAIL Avdeling THEN Avdeling.AvdelingNr ELSE 0
                         TT_BestHode.AvdBeskr   = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE ""
                         TT_BestHode.Hg         = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 0
                         TT_BestHode.HgBeskr    = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE ""
                         TT_BestHode.Vg         = IF AVAIL VarGr THEN VarGr.Vg ELSE 0
                         TT_BestHode.VgBeskr    = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE ""
                         TT_BestHode.KategoriBeskr = IF AVAIL Kategori THEN Kategori.Beskrivelse ELSE ""
                         TT_BestHode.Sasong     = ArtBas.Sasong.
              END.
              ELSE DO:
                  ASSIGN TT_BestHode.TotAntPar     = TT_BestHode.TotAntPar      + TT_BestHodeTMP.TotAntPar    
                         TT_BestHode.TotDbKr       = TT_BestHode.TotDbKr        + TT_BestHodeTMP.TotDbKr      
                         TT_BestHode.TotInnkjVerdi = TT_BestHode.TotInnkjVerdi  + TT_BestHodeTMP.TotInnkjVerdi
                         TT_BestHode.TotInnLev     = TT_BestHode.TotInnLev      + TT_BestHodeTMP.TotInnLev    
                         TT_BestHode.TotMakulert   = TT_BestHode.TotMakulert    + TT_BestHodeTMP.TotMakulert  
                         TT_BestHode.TotOverLev    = TT_BestHode.TotOverLev     + TT_BestHodeTMP.TotOverLev   
                         TT_BestHode.TotSalgsVerdi = TT_BestHode.TotSalgsVerdi  + TT_BestHodeTMP.TotSalgsVerdi
                         TT_BestHode.Rest          = TT_BestHode.Rest           + TT_BestHodeTMP.Rest         
                         .
              END.
          END.
          RELEASE TT_BestHode.
      END.
      RELEASE Avdeling.
      RELEASE HuvGr.
      RELEASE VarGr.
      RELEASE VgKat.
      RELEASE Kategori.
      
  END.
  OUTPUT TO "CLIPBOARD".
  FOR EACH TT_Artiklar.
      EXPORT TT_Artiklar.
      FIND TT_Vg WHERE TT_Vg.Vg = TT_Artiklar.Vg NO-ERROR.
      IF NOT AVAIL TT_Vg THEN DO:
          CREATE TT_Vg.
          ASSIGN TT_Vg.Vg = TT_Artiklar.Vg
                 TT_Vg.Verdier = cVerdier.
      END.
      DO iCount = 1 TO NUM-ENTRIES(TT_Artiklar.Verdier):
          
          cTmpEntry = ENTRY(iLookup,TT_Artiklar.Verdier).
          ENTRY(iSasong,cTmpEntry,";") = "1".
          ENTRY(iLookup,TT_Artiklar.Verdier) = cTmpEntry.
          
          cTmpEntry = ";".
          ENTRY(1,cTmpEntry,";") = STRING(INT(ENTRY(1,ENTRY(iCount,TT_Vg.Verdier),";")) + INT(ENTRY(1,ENTRY(iCount,TT_Artiklar.Verdier),";"))).
          ENTRY(2,cTmpEntry,";") = STRING(INT(ENTRY(2,ENTRY(iCount,TT_Vg.Verdier),";")) + INT(ENTRY(2,ENTRY(iCount,TT_Artiklar.Verdier),";"))).
          ENTRY(iCount,TT_Vg.Verdier) = cTmpEntry.
/*           ENTRY(1,ENTRY(iCount,TT_Vg.Verdier),";") = STRING(INT(ENTRY(iCount,TT_Vg.Verdier)) + INT(ENTRY(iCount,TT_Artiklar.Verdier))). */
      END.
  END.
  FOR EACH TT_Vg:
      EXPORT tt_vg.
      TT_Vg.verdier = REPLACE(TT_Vg.Verdier,",",CHR(2)).
      TT_Vg.verdier = REPLACE(TT_Vg.Verdier,";",CHR(3)).
  END.
  DYNAMIC-FUNCTION('closeQuery':U).
  ASSIGN TTH = BUFFER TT_Vg:HANDLE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OldToTT dTables  _DB-REQUIRED
PROCEDURE OldToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
/*   DEFINE OUTPUT PARAMETER TABLE-HANDLE TTH. */
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cQuery    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop     AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.

  EMPTY TEMP-TABLE TT_BestHode.
  EMPTY TEMP-TABLE TT_BestHodeTMP.
  CREATE TT_BestHodeTMP. /* en temporär record för att kunna summera */
/*   IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:       */
/*       ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")  */
/*              pcVerdier   = ENTRY(2,cXFilter,";"). */
/*       IF pcFeltListe <> pcVerdier THEN  /* */     */
/*           ASSIGN lUtvidetFilter = TRUE.           */
/*   END.                                            */
  /* Specialhantering av Vis/But                                       */
  /* Parameter cXParam används för att ge möjlighet att visa per butik */
  /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J          */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).
/*   ASSIGN cQuery = DYNAMIC-FUNCTION('getQueryString':U). */
  DYNAMIC-FUNCTION('closeQuery':U).
/*   DYNAMIC-FUNCTION('setQueryString':U,                                                               */
/*   INPUT REPLACE(cQuery,"SUBSTBUTIK","Butik = " + ENTRY(iButikLoop,cButiker) + " ") /* CHARACTER */). */
  DYNAMIC-FUNCTION('openQuery':U).
  ASSIGN lFirst = FALSE
         rRowId = ?.
  REPEAT:
      IF lFirst = FALSE THEN DO:
          RUN fetchFirst.
          ASSIGN lFirst = TRUE.
      END.
      ELSE
          RUN fetchNext.
      IF NOT AVAIL RowObject OR rRowId = ROWID(RowObject) THEN
          LEAVE.
      ELSE
          ASSIGN rRowId = ROWID(RowObject).
      FIND ArtBas OF RowObject NO-LOCK NO-ERROR.
      IF NOT AVAIL ArtBas THEN
          NEXT.
      FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
      IF NOT AVAIL LevBas THEN
          NEXT.
      FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
      FIND VgKat WHERE VgKat.Vg = ArtBas.Vg AND VgKat.VgKat = ArtBas.VgKat NO-LOCK NO-ERROR.
      FIND Kategori OF VgKat NO-LOCK NO-ERROR.
      BUFFER-COPY RowObject TO TT_BestHodeTMP.
      ASSIGN TT_BestHodeTMP.VgLopNr  = STRING(ArtBas.Vg) + "/" + STRING(ArtBas.LopNr)
             TT_BestHodeTMP.ArtBeskr = ArtBas.Beskr
             TT_BestHodeTMP.LevNavn  = LevBas.LevNamn
             TT_BestHodeTMP.Rest     = INT(RowObject.TotAntPar - ABS(RowObject.TotInnLev) - RowObject.TotMakulert + RowObject.TotOverLev)
             .
      CREATE TT_BestHode.
      BUFFER-COPY TT_BestHodeTMP TO TT_BestHode.
      ASSIGN TT_BestHode.AvdelingNr = IF AVAIL Avdeling THEN Avdeling.AvdelingNr ELSE 0
             TT_BestHode.AvdBeskr   = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE ""
             TT_BestHode.Hg         = IF AVAIL HuvGr THEN HuvGr.Hg ELSE 0
             TT_BestHode.HgBeskr    = IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE ""
             TT_BestHode.Vg         = IF AVAIL VarGr THEN VarGr.Vg ELSE 0
             TT_BestHode.VgBeskr    = IF AVAIL VarGr THEN VarGr.VgBeskr ELSE ""
             TT_BestHode.KategoriBeskr = IF AVAIL Kategori THEN Kategori.Beskrivelse ELSE "".
      RELEASE Avdeling.
      RELEASE HuvGr.
      RELEASE VarGr.
      RELEASE VgKat.
      RELEASE Kategori.
  END.
  DYNAMIC-FUNCTION('closeQuery':U).
  ASSIGN TTH = BUFFER TT_BestHode:HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HarBestillt dTables  _DB-REQUIRED
FUNCTION HarBestillt RETURNS LOGICAL
  ( INPUT iButnr AS INTEGER,INPUT iBestNr AS INTEGER,INPUT iBestStat AS INTEGER,OUTPUT cTotAntStr AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: OBS: rekkefölge i utstring = rekkefölge define 
------------------------------------------------------------------------------*/
  DEFINE VARIABLE  dTotAntPar     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotDbKr       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotInnkjVerdi AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotInnLev     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotMakulert   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotOverLev    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotSalgsVerdi AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  iKoeff         AS INTEGER    NO-UNDO.
  IF NOT CAN-FIND(FIRST Beststr WHERE Beststr.BestNr   = iBestNr AND
                                      Beststr.Butik    = iButNr  AND
                                      Beststr.BestStat = iBestStat AND
                                      BestStr.Bestilt  > 0) THEN
      ASSIGN cTotAntStr = "".
  ELSE DO:
        ASSIGN iKoeff = IF iBestStat = 7 THEN -1 ELSE 1.
        FIND BestPris WHERE BestPris.Bestnr = iBestNr AND BestPris.BestStat = iBestStat NO-LOCK NO-ERROR.
        FOR EACH Beststr WHERE Beststr.BestNr   = iBestNr   AND Beststr.Butik   = iButNr  AND
                               Beststr.BestStat = iBestStat AND BestStr.Bestilt > 0 NO-LOCK:
            ASSIGN dTotAntPar = dTotAntPar + BestStr.Bestilt.
        END.
        FOR EACH BestLevert WHERE BestLevert.BestNr = iBestNr AND BestLevert.Butik = iButNr NO-LOCK:
            ASSIGN dTotInnLev   = dTotInnLev + BestLevert.Levert
                   dTotMakulert = dTotMakulert + IF BestLevert.Levert = 0 THEN BestLevert.Rest ELSE 0.
        END.
        ASSIGN dTotInnkjVerdi = dTotAntPar * BestPris.VareKost
               dTotOverLev    = IF iBestStat <> 7 THEN dTotInnLev + dTotMakulert - dTotAntPar ELSE 0
               dTotOverLev    = IF dTotOverLev < 0 THEN 0 ELSE dTotOverLev
               dTotSalgsVerdi = dTotAntPar * BestPris.Pris
               dTotDbKr       = dTotSalgsVerdi - dTotAntPar * BestPris.MvaKr - dTotInnkjVerdi.
        ASSIGN cTotAntStr = STRING(iKoeff * dTotAntPar)     + CHR(1) +
                            STRING(iKoeff * dTotDbKr)       + CHR(1) +
                            STRING(iKoeff * dTotInnkjVerdi) + CHR(1) +
                            STRING(iKoeff * dTotInnLev)     + CHR(1) +
                            STRING(iKoeff * dTotMakulert)   + CHR(1) +
                            STRING(iKoeff * dTotOverLev)    + CHR(1) +
                            STRING(iKoeff * dTotSalgsVerdi).
  END.
  RETURN cTotAntStr <> "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

