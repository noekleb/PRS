&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_TransLogg NO-UNDO LIKE TransLogg
       FIELD Beskr LIKE ArtBas.Beskr
       FIELD Dbkr AS DECIMAL FORMAT ">>>>9.99"
       FIELD Db% AS DECIMAL FORMAT ">>9.99"
       FIELD Rab% AS DECIMAL FORMAT ">>9.9"
       FIELD NettoPris AS DECIMAL FORMAT ">>>>9.9"
       FIELD SumNetto AS DECIMAL FORMAT ">>>>>9.9"
       FIELD SumVk AS DECIMAL FORMAT ">>>>>9.9"
       FIELD SumDBKr AS DECIMAL FORMAT ">>>>>9.9"
       FIELD Lagervara AS INTEGER FORMAT "9"
       FIELD VgBeskr LIKE VarGr.VgBeskr
       FIELD Levnamn LIKE LevBas.LevNamn
       FIELD Kundenavn LIKE Kunde.Navn
       FIELD Selgernavn LIKE Selger.Navn
       FIELD Kasserernavn LIKE Forsalj.FoNamn
       FIELD Medlemnavn LIKE Medlem.Etternavn
       FIELD SolgtNegativt AS CHARACTER
       FIELD LevKod LIKE ArtBas.LevKod.



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
DEFINE VARIABLE ttTransloggh       AS HANDLE.
DEFINE VARIABLE bufTTh             AS HANDLE.
DEFINE VARIABLE buf-translogg-hndl   AS HANDLE.

&glob DATA-LOGIC-PROCEDURE .p

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
&Scoped-define INTERNAL-TABLES TransLogg

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Antall ArtikkelNr Ordreforslag BatchNr BestNr BongId BongLinjeNr Butik Dato~
 FeilKode ForsNr KalkylePris KassaNr Kode KortNr KortType KundNr LevNr~
 LinjeRab LopNr MedlemsNr Mva OvButik OvTransNr PersonalRab Plukket Postert~
 PostertDato PostertTid Pris ProfilNr RabKr RefNr RefTekst RegistrertAv~
 RegistrertDato RegistrertTid SattVVareKost SelgerNr SeqNr Storl SubtotalRab~
 TBId Tid TilStorl TransNr TTId Vg VVarekost BongTekst individnr NegLager~
 Mva%
&Scoped-define ENABLED-FIELDS-IN-TransLogg Antall ArtikkelNr Ordreforslag ~
BatchNr BestNr BongId BongLinjeNr Butik Dato FeilKode ForsNr KalkylePris ~
KassaNr Kode KortNr KortType KundNr LevNr LinjeRab LopNr MedlemsNr Mva ~
OvButik OvTransNr PersonalRab Plukket Postert PostertDato PostertTid Pris ~
ProfilNr RabKr RefNr RefTekst RegistrertAv RegistrertDato RegistrertTid ~
SattVVareKost SelgerNr SeqNr Storl SubtotalRab TBId Tid TilStorl TransNr ~
TTId Vg VVarekost BongTekst individnr NegLager Mva% 
&Scoped-Define DATA-FIELDS  Antall ArtikkelNr Ordreforslag BatchNr BestNr BongId BongLinjeNr Butik Dato~
 FeilKode ForsNr KalkylePris KassaNr Kode KortNr KortType KundNr LevNr~
 LinjeRab LopNr MedlemsNr Mva OvButik OvTransNr PersonalRab Plukket Postert~
 PostertDato PostertTid Pris ProfilNr RabKr RefNr RefTekst RegistrertAv~
 RegistrertDato RegistrertTid SattVVareKost SelgerNr SeqNr Storl SubtotalRab~
 TBId Tid TilStorl TransNr TTId Vg VVarekost BongTekst individnr NegLager~
 Mva%
&Scoped-define DATA-FIELDS-IN-TransLogg Antall ArtikkelNr Ordreforslag ~
BatchNr BestNr BongId BongLinjeNr Butik Dato FeilKode ForsNr KalkylePris ~
KassaNr Kode KortNr KortType KundNr LevNr LinjeRab LopNr MedlemsNr Mva ~
OvButik OvTransNr PersonalRab Plukket Postert PostertDato PostertTid Pris ~
ProfilNr RabKr RefNr RefTekst RegistrertAv RegistrertDato RegistrertTid ~
SattVVareKost SelgerNr SeqNr Storl SubtotalRab TBId Tid TilStorl TransNr ~
TTId Vg VVarekost BongTekst individnr NegLager Mva% 
&Scoped-Define MANDATORY-FIELDS  TTId individnr
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dtranslogg.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH TransLogg NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH TransLogg NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main TransLogg
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main TransLogg


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBongTekst dTables  _DB-REQUIRED
FUNCTION getBongTekst RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      TransLogg SCROLLING.
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
      TABLE: TT_TransLogg T "?" NO-UNDO SkoTex TransLogg
      ADDITIONAL-FIELDS:
          FIELD Beskr LIKE ArtBas.Beskr
          FIELD Dbkr AS DECIMAL FORMAT ">>>>9.99"
          FIELD Db% AS DECIMAL FORMAT ">>9.99"
          FIELD Rab% AS DECIMAL FORMAT ">>9.9"
          FIELD NettoPris AS DECIMAL FORMAT ">>>>9.9"
          FIELD SumNetto AS DECIMAL FORMAT ">>>>>9.9"
          FIELD SumVk AS DECIMAL FORMAT ">>>>>9.9"
          FIELD SumDBKr AS DECIMAL FORMAT ">>>>>9.9"
          FIELD Lagervara AS INTEGER FORMAT "9"
          FIELD VgBeskr LIKE VarGr.VgBeskr
          FIELD Levnamn LIKE LevBas.LevNamn
          FIELD Kundenavn LIKE Kunde.Navn
          FIELD Selgernavn LIKE Selger.Navn
          FIELD Kasserernavn LIKE Forsalj.FoNamn
          FIELD Medlemnavn LIKE Medlem.Etternavn
          FIELD SolgtNegativt AS CHARACTER
          FIELD LevKod LIKE ArtBas.LevKod
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
     _TblList          = "skotex.TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.TransLogg.Antall
"Antall" "Antall" ? ? "decimal" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[2]   > skotex.TransLogg.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[3]   > skotex.TransLogg.Ordreforslag
"Ordreforslag" "Ordreforslag" ? ? "logical" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[4]   > skotex.TransLogg.BatchNr
"BatchNr" "BatchNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.2 yes ""
     _FldNameList[5]   > skotex.TransLogg.BestNr
"BestNr" "BestNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[6]   > skotex.TransLogg.BongId
"BongId" "BongId" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[7]   > skotex.TransLogg.BongLinjeNr
"BongLinjeNr" "BongLinjeNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6.6 yes ""
     _FldNameList[8]   > skotex.TransLogg.Butik
"Butik" "Butik" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[9]   > skotex.TransLogg.Dato
"Dato" "Dato" ? ? "date" ? ? ? ? ? ? yes ? no 16.8 yes ""
     _FldNameList[10]   > skotex.TransLogg.FeilKode
"FeilKode" "FeilKode" ? ? "integer" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[11]   > skotex.TransLogg.ForsNr
"ForsNr" "ForsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[12]   > skotex.TransLogg.KalkylePris
"KalkylePris" "KalkylePris" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[13]   > skotex.TransLogg.KassaNr
"KassaNr" "KassaNr" ? ? "integer" ? ? ? ? ? ? yes ? no 8 yes ""
     _FldNameList[14]   > skotex.TransLogg.Kode
"Kode" "Kode" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[15]   > skotex.TransLogg.KortNr
"KortNr" "KortNr" ? ? "character" ? ? ? ? ? ? yes ? no 22 yes ""
     _FldNameList[16]   > skotex.TransLogg.KortType
"KortType" "KortType" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ""
     _FldNameList[17]   > skotex.TransLogg.KundNr
"KundNr" "KundNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[18]   > skotex.TransLogg.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.8 yes ""
     _FldNameList[19]   > skotex.TransLogg.LinjeRab
"LinjeRab" "LinjeRab" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[20]   > skotex.TransLogg.LopNr
"LopNr" "LopNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[21]   > skotex.TransLogg.MedlemsNr
"MedlemsNr" "MedlemsNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[22]   > skotex.TransLogg.Mva
"Mva" "Mva" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[23]   > skotex.TransLogg.OvButik
"OvButik" "OvButik" ? ? "integer" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[24]   > skotex.TransLogg.OvTransNr
"OvTransNr" "OvTransNr" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes ""
     _FldNameList[25]   > skotex.TransLogg.PersonalRab
"PersonalRab" "PersonalRab" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.6 yes ""
     _FldNameList[26]   > skotex.TransLogg.Plukket
"Plukket" "Plukket" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[27]   > skotex.TransLogg.Postert
"Postert" "Postert" ? ? "logical" ? ? ? ? ? ? yes ? no 6.6 yes ""
     _FldNameList[28]   > skotex.TransLogg.PostertDato
"PostertDato" "PostertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[29]   > skotex.TransLogg.PostertTid
"PostertTid" "PostertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[30]   > skotex.TransLogg.Pris
"Pris" "Pris" ? ? "decimal" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[31]   > skotex.TransLogg.ProfilNr
"ProfilNr" "ProfilNr" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ""
     _FldNameList[32]   > skotex.TransLogg.RabKr
"RabKr" "RabKr" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[33]   > skotex.TransLogg.RefNr
"RefNr" "RefNr" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[34]   > skotex.TransLogg.RefTekst
"RefTekst" "RefTekst" ? ? "character" ? ? ? ? ? ? yes ? no 40 yes ""
     _FldNameList[35]   > skotex.TransLogg.RegistrertAv
"RegistrertAv" "RegistrertAv" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ""
     _FldNameList[36]   > skotex.TransLogg.RegistrertDato
"RegistrertDato" "RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no 11.6 yes ""
     _FldNameList[37]   > skotex.TransLogg.RegistrertTid
"RegistrertTid" "RegistrertTid" ? ? "integer" ? ? ? ? ? ? yes ? no 20.8 yes ""
     _FldNameList[38]   > skotex.TransLogg.SattVVareKost
"SattVVareKost" "SattVVareKost" ? ? "logical" ? ? ? ? ? ? yes ? no 13.8 yes ""
     _FldNameList[39]   > skotex.TransLogg.SelgerNr
"SelgerNr" "SelgerNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.6 yes ""
     _FldNameList[40]   > skotex.TransLogg.SeqNr
"SeqNr" "SeqNr" ? ? "integer" ? ? ? ? ? ? yes ? no 6 yes ""
     _FldNameList[41]   > skotex.TransLogg.Storl
"Storl" "Storl" ? ? "character" ? ? ? ? ? ? yes ? no 8.2 yes ""
     _FldNameList[42]   > skotex.TransLogg.SubtotalRab
"SubtotalRab" "SubtotalRab" ? ? "decimal" ? ? ? ? ? ? yes ? no 13.2 yes ""
     _FldNameList[43]   > skotex.TransLogg.TBId
"TBId" "TBId" ? ? "integer" ? ? ? ? ? ? yes ? no 27.8 yes ""
     _FldNameList[44]   > skotex.TransLogg.Tid
"Tid" "Tid" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ""
     _FldNameList[45]   > skotex.TransLogg.TilStorl
"TilStorl" "TilStorl" ? ? "character" ? ? ? ? ? ? yes ? no 4.8 yes ""
     _FldNameList[46]   > skotex.TransLogg.TransNr
"TransNr" "TransNr" ? ? "integer" ? ? ? ? ? ? yes ? no 14.8 yes ""
     _FldNameList[47]   > skotex.TransLogg.TTId
"TTId" "TTId" ? ? "integer" ? ? ? ? ? ? yes ? yes 12 yes ""
     _FldNameList[48]   > skotex.TransLogg.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[49]   > skotex.TransLogg.VVarekost
"VVarekost" "VVarekost" ? ? "decimal" ? ? ? ? ? ? yes ? no 12 yes ""
     _FldNameList[50]   > skotex.TransLogg.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[51]   > skotex.TransLogg.individnr
"individnr" "individnr" ? ? "decimal" ? ? ? ? ? ? yes ? yes 14.4 yes ""
     _FldNameList[52]   > skotex.TransLogg.NegLager
"NegLager" "NegLager" ? ? "integer" ? ? ? ? ? ? yes ? no 9.4 yes ""
     _FldNameList[53]   > skotex.TransLogg.Mva%
"Mva%" "Mva%" ? ? "decimal" ? ? ? ? ? ? yes ? no 7.2 yes ""
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
/*   ASSIGN buf-translogg-hndl = BUFFER RowObject:HANDLE.             */
/*   CREATE TEMP-TABLE ttTransloggh.                                  */
/*   ttTransloggh:CREATE-LIKE(buf-translogg-hndl).                    */
/*   /*ttTransloggh:ADD-LIKE-FIELD("BongTekst","ArtBas.BongTekst").*/ */
/*   ttTransloggh:ADD-LIKE-FIELD("Beskr","ArtBas.Beskr").             */
/*   ttTransloggh:ADD-NEW-FIELD("Dbkr","DECIMAL",0,">>>>9.99",0).     */
/*   ttTransloggh:ADD-NEW-FIELD("Db%","DECI",0,">>9.99",0).           */
/*   ttTransloggh:ADD-NEW-FIELD("Rab%","DECIMAL",0,">>9.9",0).        */
/*   ttTransloggh:ADD-NEW-FIELD("NettoPris","DECIMAL",0,">>>>9.9",0). */
/*   ttTransloggh:ADD-NEW-FIELD("SumNetto","DECIMAL",0,">>>>>9.9",0). */
/*   ttTransloggh:ADD-NEW-FIELD("SumVk","DECIMAL",0,">>>>>9.9",0).    */
/*   ttTransloggh:ADD-NEW-FIELD("SumDBKr","DECIMAL",0,">>>>>9.9",0).  */
/*   ttTransloggh:ADD-NEW-FIELD("Lagervara","INTEGER",0,"9").         */
/*   ttTransloggh:TEMP-TABLE-PREPARE("dynttTranslogg").               */
/*   bufTTh = ttTransloggh:DEFAULT-BUFFER-HANDLE.                     */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokSdo dTables  _DB-REQUIRED
PROCEDURE SokSdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcFelt       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcValues     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcSort       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcOperators  AS CHAR NO-UNDO.
  DEF INPUT PARAMETER pcFeltListe  AS CHAR NO-UNDO.
  DEF VAR piLoop1      AS INT  NO-UNDO.
  DEF VAR pcRemoveFelt AS CHAR NO-UNDO.
  DEF VAR pcRemoveOper AS CHAR NO-UNDO.

  ASSIGN
      pcRemoveFelt = ""
      pcRemoveOper = "=,EQ,BEGINS,MATCHES,<,LE,<=,>,GE,>=,<>"
      .

  TA-BORT-KRITERIE:
  DO piLoop1 = 1 TO NUM-ENTRIES(pcFeltListe):
      /*IF NOT CAN-DO(pcFelt,ENTRY(piLoop1,pcFeltListe)) THEN*/
      DO:
          ASSIGN
              pcRemoveFelt = pcRemoveFelt + 
                             (IF pcRemoveFelt = ""
                              THEN ""
                              ELSE ",") +
                              ENTRY(piLoop1,pcFeltListe)
              .
      END.
  END. /* TA-BORT-KRITERIE */
  /* Tar bort kriterier.                                               */
  /* En gang pr. operatortype. Ikke mulig å "huske" hva som ble brukt. */
  IF pcRemoveFelt <> "" THEN
    DO piLoop1 = 1 TO NUM-ENTRIES(pcRemoveOper):
      DYNAMIC-FUNCTION('removeQuerySelection':U,
       INPUT pcRemoveFelt /* CHARACTER */,
        INPUT entry(piLoop1,pcRemoveOper) /* CHARACTER */).
    END.

  /* Legger på kriterier */
  IF pcFelt <> "" THEN
      DYNAMIC-FUNCTION('assignQuerySelection':U,
           INPUT pcFelt /* CHARACTER */,
           INPUT pcValues /* CHARACTER */,
           INPUT pcOperators /* CHARACTER */).

  /* Legger inn valgt sortering */
  IF pcSort <> "" THEN
    DYNAMIC-FUNCTION('setQuerySort':U,
       INPUT pcSort /* CHARACTER */).

  /* Åpner ikke query med ny filterverdi. */
/*   DYNAMIC-FUNCTION('openQuery':U). */
  /*
  MESSAGE 
    "Søkefelt:" pcFelt SKIP
    "Verdier:" pcValues SKIP
    "Operators:" pcOperators SKIP(2)
    "Slett kriterie:" pcRemoveFelt SKIP
    "               " pcRemoveOper SKIP
    "Alle felt:" pcFeltListe SKIP(2)
    "Aktiv query:" DYNAMIC-FUNCTION('getQueryWhere':U ) SKIP
    "BasisQuery:"  DYNAMIC-FUNCTION('getOpenQuery':U )
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransloggToTT dTables  _DB-REQUIRED
PROCEDURE TransloggToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
 DEFINE INPUT  PARAMETER pcFeltListe    AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER pcVerdier      AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER qh             AS HANDLE     NO-UNDO.
 DEFINE        VARIABLE  lFirst         AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  rRowId         AS ROWID      NO-UNDO.
 DEFINE        VARIABLE  dTotSum        AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
 DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  dPriskr        AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dRabkr         AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dMvakr         AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dVVarekostkr   AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dPosNegTall    AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  dAntall        AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dDBKr          AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  hBuffer       AS HANDLE     NO-UNDO.
 DEFINE        VARIABLE  lNeg          AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE qh2             AS HANDLE     NO-UNDO.
 IF VALID-HANDLE(qh) THEN DO:
     ASSIGN hBuffer = qh:GET-BUFFER-HANDLE(1).
/*      qh:QUERY-OPEN(). */
 END.
 DO iCount = 1 TO NUM-ENTRIES(pcVerdier,CHR(1)):
     IF ENTRY(iCount,pcVerdier,CHR(1)) <> "*" THEN DO:
         ASSIGN lUtvidetFilter = TRUE.
         LEAVE.
     END.
 END.
 DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
     IF ENTRY(iCount,pcFeltListe) = "Neglager" THEN DO:
         IF ENTRY(iCount,pcVerdier,CHR(1)) <> "*" THEN
             lNeg = TRUE. /* Vid lNeg skall inte ikke lagerstyrte tas med  */
     END.
 END.
 EMPTY TEMP-TABLE TT_TransLogg.
 DYNAMIC-FUNCTION('openQuery':U).
/* qh2 = DYNAMIC-FUNCTION('getQueryHandle':U).  */
/* MESSAGE PROGRAM-NAME(1) SKIP                 */
/* DYNAMIC-FUNCTION('getQuerySort':U) SKIP      */
/* DYNAMIC-FUNCTION('getQueryString':U) SKIP    */
/* DYNAMIC-FUNCTION('getQueryWhere':U)  SKIP    */
/* qh2:INDEX-INFORMATION(1)                     */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.       */
 REPEAT:
     IF lFirst = FALSE THEN DO:
         RUN fetchFirst.
         ASSIGN lFirst = TRUE.
     END.
     ELSE
         RUN fetchNext.
     IF rRowId = ROWID(RowObject) THEN
         LEAVE.
     ELSE
         ASSIGN rRowId = ROWID(RowObject).
     ASSIGN lIkkeTreff = FALSE.
     IF VALID-HANDLE(qh) THEN DO:
         ASSIGN lIkkeTreff = NOT hBuffer:FIND-UNIQUE("WHERE ArtikkelNr = " + STRING(RowObject.ArtikkelNr)) NO-ERROR.
     END.
     IF lIkkeTreff = FALSE AND lUtvidetFilter = TRUE THEN DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
       IF ENTRY(iCount,pcVerdier,CHR(1)) = "*" THEN
             NEXT.
         CASE ENTRY(iCount,pcFeltListe):
           WHEN "LevNr" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.LevNr)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "Avdeling" OR WHEN "Hg" OR WHEN "Vg" THEN DO:
               CASE ENTRY(iCount,pcFeltListe):
                   WHEN "Vg" THEN DO:
                       IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.Vg)) THEN
                           ASSIGN lIkkeTreff = TRUE.
                   END.
                   WHEN "Avdeling" OR WHEN "Hg" THEN DO:
                       FIND VarGr WHERE VarGr.Vg = RowObject.Vg NO-LOCK NO-ERROR.
                       IF NOT AVAIL VarGr THEN
                           ASSIGN lIkkeTreff = TRUE.
                       ELSE DO:
                           IF ENTRY(iCount,pcFeltListe) = "Hg" THEN DO: 
                               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VarGr.Hg)) THEN
                                  ASSIGN lIkkeTreff = TRUE.
                           END.
                           ELSE DO:
                               FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                               IF NOT AVAIL HuvGr OR
                                       NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr)) THEN
                                   ASSIGN lIkkeTreff = TRUE.
                           END.
                       END.
                   END.
               END CASE.
           END.
           WHEN "KasseNr" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.KassaNr)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "ForsNr" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.ForsNr)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "SelgerNr" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.SelgerNr)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "Neglager" THEN DO: /* värde av Neglager i pcVerdier är antingen '1' eller '*' vid '*' kommer vi inte hit */
               IF RowObject.NegLager = 0 THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "Kortnr" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.KortNr)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "Butik" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.Butik)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
           WHEN "TTId" THEN DO:
               IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.TTId)) THEN
                   ASSIGN lIkkeTreff = TRUE.
           END.
         END CASE.
         IF lIkkeTreff = TRUE THEN
             LEAVE.
       END.
       IF lIkkeTreff THEN
           NEXT.
     FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(RowObject.ArtikkelNr) NO-LOCK NO-ERROR.
     IF lNeg = TRUE AND AVAIL ArtBas AND ArtBas.Lager = FALSE THEN
         NEXT.
     CREATE TT_TransLogg.
     BUFFER-COPY RowObject TO TT_TransLogg NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
         DELETE TT_TransLogg.
         NEXT.
     END.
     FIND VarGr  WHERE VarGr.Vg = TT_TransLogg.Vg NO-LOCK NO-ERROR.
     FIND LevBas WHERE LevBas.LevNr = TT_TransLogg.LevNr NO-LOCK NO-ERROR.
     FIND Kunde  WHERE Kunde.KundeNr = TT_TransLogg.KundNr NO-LOCK NO-ERROR.
     FIND Medlem WHERE Medlem.MedlemsNr = TT_TransLogg.MedlemsNr NO-LOCK NO-ERROR.
     FIND Selger WHERE Selger.SelgerNr = TT_TransLogg.SelgerNr NO-LOCK NO-ERROR.
     FIND Forsalj WHERE Forsalj.ForsNr = INT(TT_TransLogg.ForsNr) NO-LOCK NO-ERROR.
     ASSIGN dPosNegTall  = IF RowObject.Antall < 0 THEN -1 ELSE 1
            dAntall      = RowObject.Antall
            dPriskr      = RowObject.Pris
            dVVarekostkr = RowObject.VVarekost
            dRabKr       = RowObject.RabKr
            dMvaKr       = RowObject.Mva
            dDBKr        = dPosNegTall * (dPrisKr - dRabKr - dMvaKr - dVVarekostKr)
            TT_TransLogg.BongTekst = IF TT_TransLogg.BongTekst <> "" THEN TT_TransLogg.BongTekst ELSE
                                       IF AVAIL ArtBas THEN ArtBas.Bongtekst ELSE "Ukjent"
            TT_TransLogg.Beskr     = IF AVAIL ArtBas THEN
                                        ArtBas.Beskr ELSE "Ukjent"
            TT_TransLogg.DbKr      = dDBKr
            TT_TransLogg.Db%       =  IF dPrisKr = 0 THEN 0 ELSE
                    ROUND((dPrisKr - dRabKr - dMvaKr - dVVarekostKr) / (dPrisKr - dRabKr - dMvaKr) * 100,1)
            TT_TransLogg.Rab% = IF dPrisKr = 0 THEN 0 ELSE
                    ROUND(dRabKr / dPrisKr * 100,1)
            TT_TransLogg.NettoPris = dPrisKr - dRabKr
            TT_TransLogg.SumNetto  = dAntall * (dPrisKr - dRabKr)
            TT_TransLogg.SumVk     = dAntall * dVVarekostkr
            TT_TransLogg.SumDBKr   = dAntall * dPosNegTall * dDBKr
            TT_Translogg.Mva       = dAntall * TT_Translogg.Mva
            TT_TransLogg.Lagervara = IF AVAIL ArtBas AND 
                                      ArtBas.OPris = FALSE AND /* Ikke PLU artikkler. */
                                      ArtBas.LAger = TRUE THEN 1 ELSE 0     /* Ikke artikkler som ikke er lagerstyrt. */
            TT_TransLogg.VgBeskr      = IF AVAIL VarGr  THEN  VarGr.VgBeskr ELSE ""  
            TT_TransLogg.Levnamn      = IF AVAIL LevBas THEN  LevBas.Levnamn ELSE ""
            TT_TransLogg.Kundenavn    = IF AVAIL Kunde  THEN  Kunde.Navn ELSE "" 
            TT_TransLogg.Selgernavn   = IF AVAIL Selger THEN  Selger.Navn ELSE "" 
            TT_TransLogg.Kasserernavn = IF AVAIL Forsalj THEN  Forsalj.Navn ELSE "" 
            TT_TransLogg.Medlemnavn   = IF AVAIL Medlem THEN Medlem.Etternavn ELSE ""
            TT_Translogg.SolgtNegativt = STRING(TT_Translogg.NegLager = 1,"J/")
            TT_Translogg.LevKod        = IF AVAIL ArtBas THEN ArtBas.LevKod ELSE ""
            .
 END.
 DYNAMIC-FUNCTION('closeQuery':U).
/*  IF VALID-HANDLE(hBuffer) THEN   */
/*      hBuffer:EMPTY-TEMP-TABLE(). */
 ASSIGN TTH = BUFFER TT_Translogg:HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TransloggToTTNY dTables  _DB-REQUIRED
PROCEDURE TransloggToTTNY :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
 DEFINE INPUT  PARAMETER pcFeltListe    AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER pcVerdier      AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER qh             AS HANDLE     NO-UNDO.
 DEFINE INPUT  PARAMETER cButiker       AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER cTTId          AS CHARACTER  NO-UNDO.
 DEFINE INPUT  PARAMETER dFraDato       AS DATE       NO-UNDO.
 DEFINE INPUT  PARAMETER dTilDato       AS DATE       NO-UNDO.
 DEFINE        VARIABLE  lFirst         AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  rRowId         AS ROWID      NO-UNDO.
 DEFINE        VARIABLE  dTotSum        AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
 DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  dPriskr        AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dRabkr         AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dMvakr         AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dVVarekostkr   AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dPosNegTall    AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  dAntall        AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  dDBKr          AS DECIMAL    NO-UNDO.
 DEFINE        VARIABLE  hBuffer       AS HANDLE     NO-UNDO.
 DEFINE        VARIABLE  lNeg          AS LOGICAL    NO-UNDO.
 DEFINE        VARIABLE  dDatoLoop AS DATE       NO-UNDO.
 DEFINE        VARIABLE  iButLoop AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  iTTIdLoop AS INTEGER    NO-UNDO.
 DEFINE        VARIABLE  cQry AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE hQuery             AS HANDLE     NO-UNDO.

 ASSIGN cButiker = REPLACE(cButiker,"|",",").

 IF dTilDato = ? THEN
     ASSIGN dTilDato = dFradato.
/*  MESSAGE pcFeltListe SKIP               */
/*          pcVerdier  SKIP                */
/*          cButiker   SKIP                */
/*          cTTId      SKIP                */
/*          dFraDato   SKIP                */
/*          dTilDato                       */
/*                                         */
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                         */


 IF VALID-HANDLE(qh) THEN DO:
     ASSIGN hBuffer = qh:GET-BUFFER-HANDLE(1).
/*      qh:QUERY-OPEN(). */
 END.
 DO iCount = 1 TO NUM-ENTRIES(pcVerdier,CHR(1)):
     IF ENTRY(iCount,pcVerdier,CHR(1)) <> "*" THEN DO:
         ASSIGN lUtvidetFilter = TRUE.
         LEAVE.
     END.
 END.
 DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
     IF ENTRY(iCount,pcFeltListe) = "Neglager" THEN DO:
         IF ENTRY(iCount,pcVerdier,CHR(1)) <> "*" THEN
             lNeg = TRUE. /* Vid lNeg skall inte ikke lagerstyrte tas med  */
     END.
 END.
 EMPTY TEMP-TABLE TT_TransLogg.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(BUFFER Translogg:HANDLE).
DO dDatoLoop = dFraDato TO dTilDato:
    DO iButLoop = 1 TO NUM-ENTRIES(cButiker):
        DO iTTIdLoop = 1 TO NUM-ENTRIES(cTTId):
             cQry = "FOR EACH Translogg WHERE Translogg.Dato = '" + STRING(dDatoLoop) + "' AND " +
                    "Translogg.TTId = '" + ENTRY(iTTIdLoop,cTTId) + "' AND " +
                    "Translogg.Butik = '" + ENTRY(iButLoop,cButiker) + "' NO-LOCK INDEXED-REPOSITION".
/*              MESSAGE cQry                           */
/*                  VIEW-AS ALERT-BOX INFO BUTTONS OK. */
             hQuery:QUERY-PREPARE(cQry).
             hQuery:QUERY-OPEN().
/*              hQuery:INDEX-INFORMATION(1)            */
/*                  VIEW-AS ALERT-BOX INFO BUTTONS OK. */
             hQuery:GET-FIRST().
             REPEAT WHILE NOT hQuery:QUERY-OFF-END:
/*                  MESSAGE cQry                           */
/*                      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                 ASSIGN lIkkeTreff = FALSE.
                 IF VALID-HANDLE(qh) THEN DO:
                     ASSIGN lIkkeTreff = NOT hBuffer:FIND-UNIQUE("WHERE ArtikkelNr = " + STRING(Translogg.ArtikkelNr)) NO-ERROR.
                 END.
                 IF lIkkeTreff = FALSE AND lUtvidetFilter = TRUE THEN DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
                     IF ENTRY(iCount,pcVerdier,CHR(1)) = "*" THEN
                         NEXT.
                     CASE ENTRY(iCount,pcFeltListe):
                       WHEN "LevNr" THEN DO:
                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.LevNr)) THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
                       WHEN "Avdeling" OR WHEN "Hg" OR WHEN "Vg" THEN DO:
                           CASE ENTRY(iCount,pcFeltListe):
                               WHEN "Vg" THEN DO:
                                   IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.Vg)) THEN
                                       ASSIGN lIkkeTreff = TRUE.
                               END.
                               WHEN "Avdeling" OR WHEN "Hg" THEN DO:
                                   FIND VarGr WHERE VarGr.Vg = Translogg.Vg NO-LOCK NO-ERROR.
                                   IF NOT AVAIL VarGr THEN
                                       ASSIGN lIkkeTreff = TRUE.
                                   ELSE DO:
                                       IF ENTRY(iCount,pcFeltListe) = "Hg" THEN DO: 
                                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VarGr.Hg)) THEN
                                              ASSIGN lIkkeTreff = TRUE.
                                       END.
                                       ELSE DO:
                                           FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
                                           IF NOT AVAIL HuvGr OR
                                                   NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr)) THEN
                                               ASSIGN lIkkeTreff = TRUE.
                                       END.
                                   END.
                               END.
                           END CASE.
                       END.
                       WHEN "KasseNr" THEN DO:
                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.KassaNr)) THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
                       WHEN "ForsNr" THEN DO:
                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.ForsNr)) THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
                       WHEN "SelgerNr" THEN DO:
                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.SelgerNr)) THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
                       WHEN "KundNr" THEN DO:
                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.KundNr)) THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
                       WHEN "Neglager" THEN DO: /* värde av Neglager i pcVerdier är antingen '1' eller '*' vid '*' kommer vi inte hit */
                           IF Translogg.NegLager = 0 THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
                       WHEN "Kortnr" THEN DO:
                           IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.KortNr)) THEN
                               ASSIGN lIkkeTreff = TRUE.
                       END.
            /*            WHEN "Butik" THEN DO:                                                          */
            /*                IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.Butik)) THEN */
            /*                    ASSIGN lIkkeTreff = TRUE.                                              */
            /*            END.                                                                           */
            /*            WHEN "TTId" THEN DO:                                                           */
            /*                IF NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Translogg.TTId)) THEN  */
            /*                    ASSIGN lIkkeTreff = TRUE.                                              */
            /*            END.                                                                           */
                     END CASE.
                     IF lIkkeTreff = TRUE THEN
                         LEAVE.
                   END.
                   IF lIkkeTreff = FALSE THEN SKAPA: DO:
                       FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(Translogg.ArtikkelNr) NO-LOCK NO-ERROR.
                       IF lNeg = TRUE AND AVAIL ArtBas AND ArtBas.Lager = FALSE THEN
                           LEAVE SKAPA.
                       CREATE TT_TransLogg.
                       BUFFER-COPY Translogg TO TT_TransLogg NO-ERROR.
                       IF ERROR-STATUS:ERROR THEN DO:
                           DELETE TT_TransLogg.
                           LEAVE SKAPA.
                       END.
                       FIND VarGr  WHERE VarGr.Vg = TT_TransLogg.Vg NO-LOCK NO-ERROR.
                       FIND LevBas WHERE LevBas.LevNr = TT_TransLogg.LevNr NO-LOCK NO-ERROR.
                       FIND Kunde  WHERE Kunde.KundeNr = TT_TransLogg.KundNr NO-LOCK NO-ERROR.
                       FIND Medlem WHERE Medlem.MedlemsNr = TT_TransLogg.MedlemsNr NO-LOCK NO-ERROR.
                       FIND Selger WHERE Selger.SelgerNr = TT_TransLogg.SelgerNr NO-LOCK NO-ERROR.
                       FIND Forsalj WHERE Forsalj.ForsNr = INT(TT_TransLogg.ForsNr) NO-LOCK NO-ERROR.
                       ASSIGN dPosNegTall  = IF Translogg.Antall < 0 THEN -1 ELSE 1
                              dAntall      = Translogg.Antall
                              dPriskr      = Translogg.Pris
                              dVVarekostkr = Translogg.VVarekost
                              dRabKr       = Translogg.RabKr
                              dMvaKr       = Translogg.Mva
                              dDBKr        = dPosNegTall * (dPrisKr - dRabKr - dMvaKr - dVVarekostKr)
                              TT_TransLogg.BongTekst = IF TT_TransLogg.BongTekst <> "" THEN TT_TransLogg.BongTekst ELSE
                                                         IF AVAIL ArtBas THEN ArtBas.Bongtekst ELSE "Ukjent"
                              TT_TransLogg.Beskr     = IF AVAIL ArtBas THEN
                                                          ArtBas.Beskr ELSE "Ukjent"
                              TT_TransLogg.DbKr      = dDBKr
                              TT_TransLogg.Db%       =  IF dPrisKr = 0 THEN 0 ELSE
                                      ROUND((dPrisKr - dRabKr - dMvaKr - dVVarekostKr) / (dPrisKr - dRabKr - dMvaKr) * 100,1)
                              TT_TransLogg.Rab% = IF dPrisKr = 0 THEN 0 ELSE
                                      ROUND(dRabKr / dPrisKr * 100,1)
                              TT_TransLogg.NettoPris = dPrisKr - dRabKr
                              TT_TransLogg.SumNetto  = dAntall * (dPrisKr - dRabKr)
                              TT_TransLogg.SumVk     = dAntall * dVVarekostkr
                              TT_TransLogg.SumDBKr   = dAntall * dPosNegTall * dDBKr
                              TT_Translogg.Mva       = dAntall * TT_Translogg.Mva
                              TT_TransLogg.Lagervara = IF AVAIL ArtBas AND 
                                                        ArtBas.OPris = FALSE AND /* Ikke PLU artikkler. */
                                                        ArtBas.LAger = TRUE THEN 1 ELSE 0     /* Ikke artikkler som ikke er lagerstyrt. */
                              TT_TransLogg.VgBeskr      = IF AVAIL VarGr  THEN  VarGr.VgBeskr ELSE ""  
                              TT_TransLogg.Levnamn      = IF AVAIL LevBas THEN  LevBas.Levnamn ELSE ""
                              TT_TransLogg.Kundenavn    = IF AVAIL Kunde  THEN  Kunde.Navn ELSE "" 
                              TT_TransLogg.Selgernavn   = IF AVAIL Selger THEN  Selger.Navn ELSE "" 
                              TT_TransLogg.Kasserernavn = IF AVAIL Forsalj THEN  Forsalj.Navn ELSE "" 
                              TT_TransLogg.Medlemnavn   = IF AVAIL Medlem THEN Medlem.Etternavn ELSE ""
                              TT_Translogg.SolgtNegativt = STRING(TT_Translogg.NegLager = 1,"J/")
                              TT_Translogg.LevKod        = IF AVAIL ArtBas THEN ArtBas.LevKod ELSE ""
                              .
                   END.
                 hQuery:GET-NEXT().
             END. /* OFF-END-LOOP */
             hQuery:QUERY-CLOSE().
        END. /* TTIdLoop */
    END. /* Butikkloop */
END. /* Datoloop */
/*  IF VALID-HANDLE(hBuffer) THEN   */
/*      hBuffer:EMPTY-TEMP-TABLE(). */
 ASSIGN TTH = BUFFER TT_Translogg:HANDLE.
 DELETE OBJECT hQuery.
 hQuery = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBongTekst dTables  _DB-REQUIRED
FUNCTION getBongTekst RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-LOCK NO-ERROR.

  RETURN IF AVAIL ArtBas THEN ArtBas.BongTekst ELSE "Ukjent".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

