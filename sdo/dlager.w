&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_Lager NO-UNDO LIKE Lager
       FIELD UtSolgt% AS DECI
       FIELD Vg LIKE VarGr.Vg
       FIELD VgBeskr LIKE VarGr.VgBeskr
       FIELD HgBeskr LIKE HuvGr.HgBeskr
       FIELD AvdelingNr LIKE Avdeling.AvdelingNr
       FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
       FIELD LevNr LIKE LevBas.LevNr
       FIELD LevNamn LIKE LevBas.levnamn
       FIElD ForsNr LIKE Forsalj.ForsNr
       FIELD FoNamn LIKE Forsalj.FoNamn
       FIELD SelgerNr LIKE Selger.SelgerNr
       FIELD SelgerNavn LIKE Selger.Navn
       FIELD Beskrivelse AS CHARACTER
       FIELD CharButik AS CHARACTER
       FIELD Sasong LIKE ArtBas.Sasong
       FIELD SasBeskr LIKE Sasong.SasBeskr
       FIELD Farg LIKE ArtBas.Farg
       FIELD FarBeskr LIKE Farg.FarBeskr
       FIELD DbKr LIKE StLinje.DbKr
       FIELD Db% LIKE StLinje.Db%
       FIELD LagerVerdi LIKE Lager.VVarekost
       FIELD Hg LIKE HuvGr.Hg
       FIELD VgLopNr AS CHARACTER
       FIELD T_db% AS DECIMAL FORMAT "->>,>>9.99"
       FIELD Pris  AS DECIMAL FORMAT "->>,>>9.99"
       FIELD T_LagerVerdi  AS DECIMAL FORMAT "->>>,>>>,>>9.99".



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

&glob DATA-LOGIC-PROCEDURE .p

DEFINE VARIABLE ttLagerh         AS HANDLE.
DEFINE VARIABLE bufTTh             AS HANDLE.
DEFINE VARIABLE buf-Lager-hndl   AS HANDLE.
DEFINE VARIABLE buf-TTLager-hndl AS HANDLE.
DEFINE TEMP-TABLE TT_LagerTMP LIKE TT_Lager.

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
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  Aktivert ArtikkelNr Beskr BongTekst Farg foder-id Hg Klack KundeRabatt~
 lager last-id LevFargKod LevKod LevNr LopNr MatKod OPris Pakke ProdNr~
 SaSong Vg VgKat VMId
&Scoped-define ENABLED-FIELDS-IN-ArtBas Aktivert ArtikkelNr Beskr BongTekst ~
Farg foder-id Hg Klack KundeRabatt lager last-id LevFargKod LevKod LevNr ~
LopNr MatKod OPris Pakke ProdNr SaSong Vg VgKat VMId 
&Scoped-Define DATA-FIELDS  Aktivert ArtikkelNr Beskr BongTekst Farg foder-id Hg Klack KundeRabatt~
 lager last-id LevFargKod LevKod LevNr LopNr MatKod OPris Pakke ProdNr~
 SaSong Vg VgKat VMId
&Scoped-define DATA-FIELDS-IN-ArtBas Aktivert ArtikkelNr Beskr BongTekst ~
Farg foder-id Hg Klack KundeRabatt lager last-id LevFargKod LevKod LevNr ~
LopNr MatKod OPris Pakke ProdNr SaSong Vg VgKat VMId 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dlager.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH ArtBas NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH ArtBas NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main ArtBas


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBeskr dTables  _DB-REQUIRED
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}


/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      ArtBas SCROLLING.
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
      TABLE: TT_Lager T "?" NO-UNDO skotex Lager
      ADDITIONAL-FIELDS:
          FIELD UtSolgt% AS DECI
          FIELD Vg LIKE VarGr.Vg
          FIELD VgBeskr LIKE VarGr.VgBeskr
          FIELD HgBeskr LIKE HuvGr.HgBeskr
          FIELD AvdelingNr LIKE Avdeling.AvdelingNr
          FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
          FIELD LevNr LIKE LevBas.LevNr
          FIELD LevNamn LIKE LevBas.levnamn
          FIElD ForsNr LIKE Forsalj.ForsNr
          FIELD FoNamn LIKE Forsalj.FoNamn
          FIELD SelgerNr LIKE Selger.SelgerNr
          FIELD SelgerNavn LIKE Selger.Navn
          FIELD Beskrivelse AS CHARACTER
          FIELD CharButik AS CHARACTER
          FIELD Sasong LIKE ArtBas.Sasong
          FIELD SasBeskr LIKE Sasong.SasBeskr
          FIELD Farg LIKE ArtBas.Farg
          FIELD FarBeskr LIKE Farg.FarBeskr
          FIELD DbKr LIKE StLinje.DbKr
          FIELD Db% LIKE StLinje.Db%
          FIELD LagerVerdi LIKE Lager.VVarekost
          FIELD Hg LIKE HuvGr.Hg
          FIELD VgLopNr AS CHARACTER
          FIELD T_db% AS DECIMAL FORMAT "->>,>>9.99"
          FIELD Pris  AS DECIMAL FORMAT "->>,>>9.99"
          FIELD T_LagerVerdi  AS DECIMAL FORMAT "->>>,>>>,>>9.99"
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
     _TblList          = "skotex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > skotex.ArtBas.Aktivert
"Aktivert" "Aktivert" ? ? "logical" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[2]   > skotex.ArtBas.ArtikkelNr
"ArtikkelNr" "ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes ? no 14.4 yes ""
     _FldNameList[3]   > skotex.ArtBas.Beskr
"Beskr" "Beskr" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[4]   > skotex.ArtBas.BongTekst
"BongTekst" "BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ""
     _FldNameList[5]   > skotex.ArtBas.Farg
"Farg" "Farg" ? ? "integer" ? ? ? ? ? ? yes ? no 4.2 yes ""
     _FldNameList[6]   > skotex.ArtBas.foder-id
"foder-id" "foder-id" ? ? "integer" ? ? ? ? ? ? yes ? no 7 yes ""
     _FldNameList[7]   > skotex.ArtBas.Hg
"Hg" "Hg" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ""
     _FldNameList[8]   > skotex.ArtBas.Klack
"Klack" "Klack" ? ? "integer" ? ? ? ? ? ? yes ? no 4 yes ""
     _FldNameList[9]   > skotex.ArtBas.KundeRabatt
"KundeRabatt" "KundeRabatt" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[10]   > skotex.ArtBas.lager
"lager" "lager" ? ? "logical" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[11]   > skotex.ArtBas.last-id
"last-id" "last-id" ? ? "integer" ? ? ? ? ? ? yes ? no 5.4 yes ""
     _FldNameList[12]   > skotex.ArtBas.LevFargKod
"LevFargKod" "LevFargKod" ? ? "character" ? ? ? ? ? ? yes ? no 15 yes ""
     _FldNameList[13]   > skotex.ArtBas.LevKod
"LevKod" "LevKod" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ""
     _FldNameList[14]   > skotex.ArtBas.LevNr
"LevNr" "LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no 18.2 yes ""
     _FldNameList[15]   > skotex.ArtBas.LopNr
"LopNr" "LopNr" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[16]   > skotex.ArtBas.MatKod
"MatKod" "MatKod" ? ? "integer" ? ? ? ? ? ? yes ? no 12.2 yes ""
     _FldNameList[17]   > skotex.ArtBas.OPris
"OPris" "OPris" ? ? "logical" ? ? ? ? ? ? yes ? no 5 yes ""
     _FldNameList[18]   > skotex.ArtBas.Pakke
"Pakke" "Pakke" ? ? "logical" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[19]   > skotex.ArtBas.ProdNr
"ProdNr" "ProdNr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ""
     _FldNameList[20]   > skotex.ArtBas.SaSong
"SaSong" "SaSong" ? ? "integer" ? ? ? ? ? ? yes ? no 7.2 yes ""
     _FldNameList[21]   > skotex.ArtBas.Vg
"Vg" "Vg" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ""
     _FldNameList[22]   > skotex.ArtBas.VgKat
"VgKat" "VgKat" ? ? "integer" ? ? ? ? ? ? yes ? no 5.8 yes ""
     _FldNameList[23]   > skotex.ArtBas.VMId
"VMId" "VMId" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ""
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
/*   ASSIGN buf-Lager-hndl = BUFFER Lager:HANDLE. */
  ASSIGN buf-TTLager-hndl = BUFFER TT_Lager:HANDLE.
  CREATE TEMP-TABLE ttLagerh.
  ttLagerh:CREATE-LIKE(buf-TTLager-hndl).
/*   ttLagerh:ADD-LIKE-FIELD("LevNr","LevBas.LevNr").     */
/*   ttLagerh:ADD-LIKE-FIELD("Levnamn","LevBas.levnamn"). */
/*   ttLagerh:ADD-LIKE-FIELD("AvdelingNr","Avdeling.AvdelingNr").     */
/*   ttLagerh:ADD-LIKE-FIELD("AvdelingNavn","Avdeling.AvdelingNavn"). */
/*   ttLagerh:ADD-LIKE-FIELD("HgBeskr","HuvGr.HgBeskr"). */
/*   ttLagerh:ADD-LIKE-FIELD("Vg","VarGr.Vg").           */
/*   ttLagerh:ADD-LIKE-FIELD("VgBeskr","VarGr.VgBeskr"). */
/*   ttLagerh:ADD-LIKE-FIELD("Sasong","SaSong.Sasong").     */
/*   ttLagerh:ADD-LIKE-FIELD("SasBeskr","SaSong.SasBeskr"). */
/*   ttLagerh:ADD-LIKE-FIELD("Farg","Farg.Farg").         */
/*   ttLagerh:ADD-LIKE-FIELD("FarBeskr","Farg.FarBeskr"). */
  ttLagerh:ADD-LIKE-FIELD("MatKod","Material.MatKod").
  ttLagerh:ADD-LIKE-FIELD("MatBeskr","Material.MatBeskr").
  ttLagerh:ADD-LIKE-FIELD("VMId","Varemerke.VMId").
  ttLagerh:ADD-LIKE-FIELD("VMBeskr","Varemerke.Beskrivelse").
  ttLagerh:TEMP-TABLE-PREPARE("dynttStLager").
  bufTTh = ttLagerh:DEFAULT-BUFFER-HANDLE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerToTT dTables  _DB-REQUIRED
PROCEDURE LagerToTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE-HANDLE TTH.
    DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lVisBut   AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
    DEFINE        VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  cLagAnt        AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lOK            AS LOGICAL    NO-UNDO.
    bufTTh:EMPTY-TEMP-TABLE().
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    CREATE TT_LagerTMP. /* en temporär record för att kunna summera */
    IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
        ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
               pcVerdier   = ENTRY(2,cXFilter,";").
        IF pcFeltListe <> pcVerdier THEN  /* */
            ASSIGN lUtvidetFilter = TRUE.
    END.
    DO iCount = 1 TO NUM-ENTRIES(cXParam,";"):
        IF ENTRY(1,ENTRY(iCount,cXParam,";"),":") = "LagAnt" THEN DO:
            ASSIGN cLagAnt = TRIM(ENTRY(2,ENTRY(iCount,cXParam,";"),":")).
            LEAVE.
        END.
    END.
    IF cLagAnt = "" THEN
        ASSIGN cLagAnt = "*".
    DYNAMIC-FUNCTION('openQuery':U).
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
        
/*         FIND ArtBas WHERE ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR. */
        IF RowObject.Lager = FALSE OR RowObject.OPris = TRUE THEN
            NEXT.
        FIND VarGr WHERE VarGr.Vg = RowObject.Vg NO-LOCK NO-ERROR.
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        IF cStTypeId = "ARTIKKEL" AND lUtvidetFilter = TRUE THEN DO:
          ASSIGN lIkkeTreff = FALSE.
          DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
            CASE ENTRY(iCount,pcFeltListe):
                WHEN "AvdelingNr" THEN DO:
                    IF NOT AVAIL HuvGr THEN
                        ASSIGN lIkkeTreff = TRUE.
                    ELSE IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr)) THEN
                        ASSIGN lIkkeTreff = TRUE.
                END.
              WHEN "LevNr" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.LevNr)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Vg" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.Vg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Sasong" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.Sasong)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Farg" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.Farg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "MatKod" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(RowObject.MatKod)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
            END CASE.
            IF lIkkeTreff = TRUE THEN
                LEAVE.
          END.
          IF lIkkeTreff THEN
              NEXT.
        END.
        DO iCount = 1 TO NUM-ENTRIES(cButiker):
            FOR EACH Lager WHERE Lager.Butik = INT(ENTRY(iCount,cButiker)) AND
                Lager.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK:
                IF cLagAnt <> "*" THEN DO:
                    ASSIGN lOK = FALSE.
                    CASE cLagAnt:
                        WHEN "<" THEN 
                            IF Lager.Lagant < 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "<=" THEN 
                            IF Lager.Lagant <= 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "=" THEN 
                            IF Lager.Lagant = 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN ">=" THEN 
                            IF Lager.Lagant >= 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN ">" THEN 
                            IF Lager.Lagant > 0 THEN 
                                ASSIGN lOK = TRUE.
                        WHEN "<>" THEN 
                            IF Lager.Lagant <> 0 THEN 
                                ASSIGN lOK = TRUE.
                    END CASE.
                    IF lOK = FALSE THEN
                        NEXT.
                END.
                BUFFER-COPY Lager TO TT_LagerTMP.
                IF AVAIL HuvGr THEN
                    ASSIGN TT_LagerTMP.AvdelingNr = HuvGr.AvdelingNr
                           TT_LagerTMP.Hg         = HuvGr.Hg.
                FIND CURRENT TT_LagerTMP.

                IF NOT lVisBut THEN
                    FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr NO-ERROR.
                IF AVAIL TT_Lager THEN DO:
                    RUN SummeraTT.
                END.
                ELSE DO:
                    CREATE TT_Lager.
                    BUFFER-COPY TT_LagerTMP TO TT_Lager.
                    ASSIGN TT_Lager.Beskrivelse  = RowObject.Beskr
                           TT_Lager.AvdelingNavn = getBeskr("AVDELING",STRING(TT_Lager.AvdelingNr))
                           TT_Lager.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_Lager.Hg))
                           TT_Lager.Vg           = RowObject.Vg
                           TT_Lager.VgBeskr      = getBeskr("VAREGR",STRING(TT_Lager.Vg))
        /*                    TT_Lager.VgLopNr       = STRING(RowObject.Vg) + "/" + REPLACE(STRING(RowObject.LopNr,"z999"),"0","  ") */
                           TT_Lager.VgLopNr       = STRING(RowObject.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(RowObject.LopNr))) + STRING(RowObject.LopNr)
                           TT_Lager.LevNr       = RowObject.LevNr
                           TT_Lager.LevNamn     = getBeskr("LEVERAN",STRING(TT_Lager.LevNr))
                           TT_Lager.Sasong      = RowObject.Sasong
                           TT_Lager.SasBeskr    = getBeskr("SASONG",STRING(TT_Lager.Sasong))
                           TT_Lager.Farg        = RowObject.Farg
                           TT_Lager.FarBeskr    = getBeskr("FARG",STRING(TT_Lager.Farg))
                           TT_Lager.CharButik   = IF lVisBut THEN STRING(TT_Lager.Butik) ELSE ""
                           TT_Lager.DbKr        = TT_Lager.VerdiSolgt - (TT_Lager.AntSolgt * TT_Lager.VVarekost)
                          /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                          /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                          /* som ger vvarekost/st */
                           TT_Lager.LagerVerdi     = TT_Lager.LagAnt * TT_Lager.VVarekost.
                    RELEASE TT_Lager.
                END.
                RELEASE TT_Lager.
            END.
        END.
    END.
    FOR EACH TT_Lager:
        ASSIGN TT_Lager.UtSolgt% = IF TT_Lager.KjopAnt > 0 THEN ROUND(TT_Lager.AntSolgt / TT_Lager.KjopAnt * 100,1)
                                      ELSE 0
               TT_Lager.Db%      = IF TT_Lager.VerdiSolgt <> 0 THEN ROUND(TT_Lager.DbKr / TT_Lager.VerdiSolgt * 100,1)
                                      ELSE 0.
/*                TT_Lager.VVarekost = IF TT_Lager.LagAnt <> 0 THEN ROUND(TT_Lager.LagerVerdi / TT_Lager.LagAnt,2) ELSE 0. */
        FIND ArtPris WHERE ArtPris.ArtikkelNr = TT_Lager.ArtikkelNr AND
                           ArtPris.Profil     = 1 NO-LOCK NO-ERROR.
        IF AVAIL ArtPris THEN DO:
            ASSIGN TT_Lager.T_db% = ArtPris.db%[IF NOT ArtPris.Tilbud THEN 1 ELSE 2]
                   TT_Lager.Pris  = ArtPris.Pris[IF NOT ArtPris.Tilbud THEN 1 ELSE 2].
                   TT_Lager.T_Lagerverdi = TT_Lager.LagAnt * TT_Lager.Pris.
        END.
        bufTTh:BUFFER-CREATE.
        bufTTh:BUFFER-COPY(buf-TTLager-hndl) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            LEAVE.
    END.
    DYNAMIC-FUNCTION('closeQuery':U).
    bufTTh:BUFFER-RELEASE().
    ASSIGN TTH = ttLagerh.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerToTTOld dTables  _DB-REQUIRED
PROCEDURE LagerToTTOld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
 /*   
    DEFINE OUTPUT PARAMETER TABLE-HANDLE TTH.
    DEFINE INPUT  PARAMETER cStTypeId AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cButiker  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER lVisBut   AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER cXFilter  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER cXParam   AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
    DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
    DEFINE        VARIABLE  hBufferField AS HANDLE     NO-UNDO.
    DEFINE        VARIABLE  pcFeltListe  AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  pcVerdier    AS CHARACTER  NO-UNDO.
    DEFINE        VARIABLE  iCount       AS INTEGER    NO-UNDO.
    DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
    DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
    bufTTh:EMPTY-TEMP-TABLE().
    EMPTY TEMP-TABLE TT_Lager.
    EMPTY TEMP-TABLE TT_LagerTMP.
    CREATE TT_LagerTMP. /* en temporär record för att kunna summera */
    IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
        ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
               pcVerdier   = ENTRY(2,cXFilter,";").
        IF pcFeltListe <> pcVerdier THEN  /* */
            ASSIGN lUtvidetFilter = TRUE.
    END.
    DYNAMIC-FUNCTION('openQuery':U).
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
        IF NOT CAN-DO(cButiker,STRING(RowObject.Butik)) THEN
            NEXT.
        
        FIND ArtBas WHERE ArtBas.ArtikkelNr = RowObject.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas OR ArtBas.Lager = FALSE OR ArtBas.OPris = TRUE THEN
            NEXT.
        FIND VarGr WHERE VarGr.Vg = ArtBas.Vg NO-LOCK NO-ERROR.
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        IF cStTypeId = "ARTIKKEL" AND lUtvidetFilter = TRUE THEN DO:
          ASSIGN lIkkeTreff = FALSE.
          DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
            CASE ENTRY(iCount,pcFeltListe):
                WHEN "AvdelingNr" THEN DO:
                    IF NOT AVAIL HuvGr THEN
                        ASSIGN lIkkeTreff = TRUE.
                    ELSE IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.AvdelingNr)) THEN
                        ASSIGN lIkkeTreff = TRUE.
                END.
              WHEN "LevNr" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.LevNr)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Vg" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.Vg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Sasong" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.Sasong)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "Farg" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.Farg)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
              WHEN "MatKod" THEN DO:
                  IF ENTRY(iCount,pcVerdier,CHR(1)) <> "" AND NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.MatKod)) THEN
                      ASSIGN lIkkeTreff = TRUE.
              END.
            END CASE.
            IF lIkkeTreff = TRUE THEN
                LEAVE.
          END.
          IF lIkkeTreff THEN
              NEXT.
        END.
        BUFFER-COPY RowObject TO TT_LagerTMP.
        IF AVAIL HuvGr THEN
            ASSIGN TT_LagerTMP.AvdelingNr = HuvGr.AvdelingNr
                   TT_LagerTMP.Hg         = HuvGr.Hg.
        FIND CURRENT TT_LagerTMP.

        IF NOT lVisBut THEN
            FIND TT_Lager WHERE TT_Lager.ArtikkelNr = TT_LagerTMP.ArtikkelNr NO-ERROR.
        IF AVAIL TT_Lager THEN DO:
            RUN SummeraTT.
        END.
        ELSE DO:
            CREATE TT_Lager.
            BUFFER-COPY TT_LagerTMP TO TT_Lager.
            ASSIGN TT_Lager.Beskrivelse  = ArtBas.Beskr
                   TT_Lager.AvdelingNavn = getBeskr("AVDELING",STRING(TT_Lager.AvdelingNr))
                   TT_Lager.HgBeskr      = getBeskr("HOVEDGR",STRING(TT_Lager.Hg))
                   TT_Lager.Vg           = ArtBas.Vg
                   TT_Lager.VgBeskr      = getBeskr("VAREGR",STRING(TT_Lager.Vg))
/*                    TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + REPLACE(STRING(ArtBas.LopNr,"z999"),"0","  ") */
                   TT_Lager.VgLopNr       = STRING(ArtBas.Vg) + "/" + FILL("  ",4 - LENGTH(STRING(ArtBas.LopNr))) + STRING(ArtBas.LopNr)
                   TT_Lager.LevNr       = ArtBas.LevNr
                   TT_Lager.LevNamn     = getBeskr("LEVERAN",STRING(TT_Lager.LevNr))
                   TT_Lager.Sasong      = ArtBas.Sasong
                   TT_Lager.SasBeskr    = getBeskr("SASONG",STRING(TT_Lager.Sasong))
                   TT_Lager.Farg        = ArtBas.Farg
                   TT_Lager.FarBeskr    = getBeskr("FARG",STRING(TT_Lager.Farg))
                   TT_Lager.CharButik   = IF lVisBut THEN STRING(TT_Lager.Butik) ELSE ""
                   TT_Lager.DbKr        = TT_Lager.VerdiSolgt - (TT_Lager.AntSolgt * TT_Lager.VVarekost)
                  /* Här sätter vi den totala lagervärdet utifrån LagAnt och VVarekost */
                  /* vid flera butiker summeras den och längre ner delar vi det totala lagervärdet med totala LagAnt */
                  /* som ger vvarekost/st */
                   TT_Lager.LagerVerdi     = TT_Lager.LagAnt * TT_Lager.VVarekost.
            RELEASE TT_Lager.
        END.
        RELEASE TT_Lager.
    END.
    FOR EACH TT_Lager:
        ASSIGN TT_Lager.UtSolgt% = IF TT_Lager.KjopAnt > 0 THEN ROUND(TT_Lager.AntSolgt / TT_Lager.KjopAnt * 100,1)
                                      ELSE 0
               TT_Lager.Db%      = IF TT_Lager.VerdiSolgt <> 0 THEN ROUND(TT_Lager.DbKr / TT_Lager.VerdiSolgt * 100,1)
                                      ELSE 0.
/*                TT_Lager.VVarekost = IF TT_Lager.LagAnt <> 0 THEN ROUND(TT_Lager.LagerVerdi / TT_Lager.LagAnt,2) ELSE 0. */
        FIND ArtPris WHERE ArtPris.ArtikkelNr = TT_Lager.ArtikkelNr AND
                           ArtPris.Profil     = 1 NO-LOCK NO-ERROR.
        IF AVAIL ArtPris THEN DO:
            ASSIGN TT_Lager.T_db% = ArtPris.db%[IF NOT ArtPris.Tilbud THEN 1 ELSE 2]
                   TT_Lager.Pris  = ArtPris.Pris[IF NOT ArtPris.Tilbud THEN 1 ELSE 2].
                   TT_Lager.T_Lagerverdi = TT_Lager.LagAnt * TT_Lager.Pris.
        END.
        bufTTh:BUFFER-CREATE.
        bufTTh:BUFFER-COPY(buf-TTLager-hndl) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            LEAVE.
    END.
    DYNAMIC-FUNCTION('closeQuery':U).
    bufTTh:BUFFER-RELEASE().
    ASSIGN TTH = ttLagerh.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummeraTT dTables  _DB-REQUIRED
PROCEDURE SummeraTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN TT_Lager.LagAnt        = TT_Lager.LagAnt        + TT_LagerTMP.LagAnt       
       TT_Lager.AntSolgt      = TT_Lager.AntSolgt      + TT_LagerTMP.AntSolgt     
       TT_Lager.VerdiSolgt    = TT_Lager.VerdiSolgt    + TT_LagerTMP.VerdiSolgt   
       TT_Lager.AntRab        = TT_Lager.AntRab        + TT_LagerTMP.AntRab       
/**/   TT_Lager.DbKr          = TT_Lager.DbKr          + TT_LagerTMP.VerdiSolgt - (TT_LagerTMP.AntSolgt * TT_LagerTMP.VVarekost)
       TT_Lager.VerdiRabatt   = TT_Lager.VerdiRabatt   + TT_LagerTMP.VerdiRabatt  
    /* Här summerar vi varekosten för att få totala lagervärdet, */
    /* Innan vi kör visning delas den med LagAnt och ger VVarekost */
/**/   TT_Lager.LagerVerdi    = TT_Lager.LagerVerdi    + (TT_LagerTMP.LagAnt * TT_LagerTMP.VVarekost)    
       TT_Lager.ReklAnt       = TT_Lager.ReklAnt       + TT_LagerTMP.ReklAnt      
       TT_Lager.ReklVerdi     = TT_Lager.ReklVerdi     + TT_LagerTMP.ReklVerdi    
       TT_Lager.ReklLAnt      = TT_Lager.ReklLAnt      + TT_LagerTMP.ReklLAnt     
       TT_Lager.ReklLVerdi    = TT_Lager.ReklLVerdi    + TT_LagerTMP.ReklLVerdi   
       TT_Lager.RetLAnt       = TT_Lager.RetLAnt       + TT_LagerTMP.RetLAnt      
       TT_Lager.SvinnAnt      = TT_Lager.SvinnAnt      + TT_LagerTMP.SvinnAnt     
       TT_Lager.SvinnVerdi    = TT_Lager.SvinnVerdi    + TT_LagerTMP.SvinnVerdi   
       TT_Lager.GjenkjopAnt   = TT_Lager.GjenkjopAnt   + TT_LagerTMP.GjenkjopAnt  
       TT_Lager.GjenkjopVerdi = TT_Lager.GjenkjopVerdi + TT_LagerTMP.GjenkjopVerdi
       TT_Lager.KjopAnt       = TT_Lager.KjopAnt       + TT_LagerTMP.KjopAnt      
       TT_Lager.KjopVerdi     = TT_Lager.KjopVerdi     + TT_LagerTMP.KjopVerdi    
       TT_Lager.BrekkAnt      = TT_Lager.BrekkAnt      + TT_LagerTMP.BrekkAnt     
       TT_Lager.BrekkVerdi    = TT_Lager.BrekkVerdi    + TT_LagerTMP.BrekkVerdi   
       TT_Lager.IntAnt        = TT_Lager.IntAnt        + TT_LagerTMP.IntAnt       
       TT_Lager.IntVerdi      = TT_Lager.IntVerdi      + TT_LagerTMP.IntVerdi     
       TT_Lager.JustAnt       = TT_Lager.JustAnt       + TT_LagerTMP.JustAnt      
       TT_Lager.JustVerdi     = TT_Lager.JustVerdi     + TT_LagerTMP.JustVerdi    
       TT_Lager.NedAnt        = TT_Lager.NedAnt        + TT_LagerTMP.NedAnt       
       TT_Lager.NedVerdi      = TT_Lager.NedVerdi      + TT_LagerTMP.NedVerdi     
       TT_Lager.OvAnt         = TT_Lager.OvAnt         + TT_LagerTMP.OvAnt        
       TT_Lager.OvVerdi       = TT_Lager.OvVerdi       + TT_LagerTMP.OvVerdi.      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

/* ************************  Function Implementations ***************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBeskr dTables  _DB-REQUIRED
FUNCTION getBeskr RETURNS CHARACTER
  ( INPUT cStTypeId AS CHARACTER, INPUT cObjekt AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBeskrivelse AS CHARACTER  NO-UNDO.
  CASE cStTypeId:
      WHEN "ARTIKKEL" THEN DO:
          FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL ArtBas THEN ArtBas.Bongtekst ELSE "Ukjent".
      END.
      WHEN "AVDELING" THEN DO:
          FIND Avdeling WHERE Avdeling.AvdelingNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "Ukjent".
      END.
      WHEN "FARG" THEN DO:
          FIND Farg WHERE Farg.Farg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Farg THEN Farg.FarBeskr ELSE "Ukjent".
      END.
      WHEN "SASONG" THEN DO:
          FIND Sasong WHERE Sasong.Sasong = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Sasong THEN Sasong.SasBeskr ELSE "Ukjent".
      END.
      WHEN "HOVEDGR" THEN DO:
          FIND HuvGr WHERE HuvGr.Hg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL HuvGr THEN HuvGr.HgBeskr ELSE "Ukjent".
      END.
      WHEN "BUTSTAT" THEN DO:
          FIND Butiker WHERE Butiker.Butik = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".
      END.
      WHEN "VAREGR" THEN DO:
          FIND VarGr WHERE VarGr.Vg = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL VarGr THEN VarGr.VgBeskr ELSE "Ukjent".
      END.
      WHEN "LEVERAN" THEN DO:
          FIND LevBas WHERE LevBas.LevNr = INT(cObjekt) NO-LOCK NO-ERROR.
          RETURN IF AVAIL Levbas THEN LevBas.levnamn ELSE "Ukjent".
      END.
/*       WHEN "ARTIKKEL" THEN DO:                                             */
/*           FIND ArtBas WHERE ArtBas.ForsNr = INT(cObjekt) NO-LOCK NO-ERROR. */
/*           RETURN IF AVAIL Forsalj THEN Forsalj.FoNamn ELSE "Ukjent".       */
/*       END.                                                                 */
      OTHERWISE RETURN "".
  END CASE.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

