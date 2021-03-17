&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER cEksportDir AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iButikkNr   AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER dDato       AS DATE       NO-UNDO.
DEFINE INPUT  PARAMETER lSendFtp    AS LOGICAL    NO-UNDO.
DEFINE INPUT  PARAMETER cFtpHost    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpBruker  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER cFtpPassord AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cFileName    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilePrefix  AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN Exportera.

/* IF lSendFtp THEN */
/*     RUN DoFtp.   */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AnalysResult) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnalysResult Procedure 
PROCEDURE AnalysResult :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cc    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ccOld AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE ccFil AS CHARACTER  NO-UNDO.
    OS-CREATE-DIR VALUE(cEksportdir + "bku").
    IF SEARCH(cEksportdir + "Result.ftp") <> ? THEN DO:
        INPUT FROM VALUE(cEksportdir + "Result.ftp").
        REPEAT:
            IMPORT UNFORMATTED cc.
            IF TRIM(cc) = "" THEN DO:
                ccOld = "".
                NEXT.
            END.
            IF cc BEGINS "150" THEN
                ccOld = cc.
            ELSE IF cc BEGINS "226" THEN DO:
                /* här skall filnamnet i ccOld hämtas */
                ccFil = TRIM(ENTRY(NUM-ENTRIES(ccOld," "),ccOld," "),".").
                IF SEARCH(cEksportdir + ccFil) <> ? THEN
                    OS-RENAME VALUE(cEksportdir + ccFil) VALUE(cEksportdir + "bku\" + ccFil).
                ccOld = "".
                ccFil = "".
            END.
            ELSE
                ccOld = "".
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DoFtp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DoFtp Procedure 
PROCEDURE DoFtp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OUTPUT to VALUE(cEksportDir + "tgftp.tg").
    Put unformatted
        "open " cFtpHost    SKIP
                cFtpBruker  SKIP
                cFtpPassord SKIP
        "binary"           SKIP
        "prompt"           SKIP
        "mput " + cFilePrefix + "*.*" SKIP
          "quit".
    Output close.
    OUTPUT to VALUE(cEksportDir + "tgftp.cmd").
    PUT UNFORMATTED "cd " + RIGHT-TRIM(cEksportdir,"\") SKIP
                    "ftp -s:tgftp.tg" SKIP.
    OUTPUT CLOSE.
    OS-COMMAND SILENT VALUE(cEksportdir + "tgftp.cmd" + " > " + cEksportdir + "Result.ftp").

    RUN AnalysResult.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Exportera) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exportera Procedure 
PROCEDURE Exportera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAarPerLinNr AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cOutputFilename AS CHARACTER  NO-UNDO.
    FIND butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
    ASSIGN cFilePrefix = "StTimeGrip."
           cEksportDir = RIGHT-TRIM(cEksportDir,"\") + "\"
           iAarPerLinnr = YEAR(dDato) * 1000 + (ddato - DATE(12,31,YEAR(dDato) - 1)).
           cFileName   = cEksportDir + cFilePrefix + STRING(iButikkNr).
           cOutputFilename = cFileName + ".TMP".
/*            cFileName   = cEksportDir + "StTimeGrip." + STRING(iButikkNr). */
    IF SEARCH(cFileName) <> ? THEN
        OS-RENAME VALUE(cFileName) VALUE(cOutputFilename).
    IF CAN-FIND(FIRST StLinje WHERE Stlinje.Butik = iButikkNr AND
                                    StLinje.SttypeId = "ARTIKKEL" AND
                                    StLinje.PerId = "DAG" AND
                                    StLinje.AarPerlinnr = iAarPerlinnr) THEN DO:
        OUTPUT TO VALUE(cOutputFilename) APPEND.
        DEFINE VARIABLE dArtikkelNR  AS DECIMAL    NO-UNDO.
        DEFINE VARIABLE dDbKr        AS DECIMAL    NO-UNDO.
        /* Här måste vi ta hänsayn tillårsskifte. */

        PUT UNFORMATTED "SE-Artikkelnr" ";" 
                        "SE-Artbeskr"   ";"
                        "Dag"           ";"
                        "Butikk"        ";"
                        "Butikknavn"    ";"
                        "AntSolgt"      ";"
                        "Brutto solgt"  ";"
                        "Mva"           ";"
                        "Dbkr"          ";"
                        "Rabatt antall" ";"
                        "Rabatt verdi"  ";"
                        "Varekost"      ";"
                        "Kjøpt antall"  ";"
                        "Kjøpt verdi"   ";"
                        "Levnr"         ";"
                        "Navn"          ";"
                        "Lev artnr"     ";"
                        "Levfarg"       ";"
                        "SE-farg"       ";"
                        "SE-fargbeskr"  ";"
                        "Sesongkode"    ";"
                        "Sesong"        ";"
                        "Varegr"        ";"
                        "Vargr-beskr"   ";"
                        "Hovegr"        ";"
                        "Hovegr-beskr"  ";"
                        "Avdeling"      ";"
                        "Avdeling-beskr" ";" 
                        "VMId"           ";"
                         "VMBeskrivelse" ";" SKIP.
        FOR EACH stlinje WHERE stlinje.butik      = iButikkNr AND
                               stlinje.sttypeid   = "ARTIKKEL" AND
                               stlinje.perid      = "DAG" AND
                               stlinje.AarPerLinNr = iAarperlinnr NO-LOCK.
            ASSIGN dArtikkelNR = DECI(stlinje.dataobjekt) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            FIND artbas WHERE artbas.artikkelnr = dArtikkelNR NO-LOCK NO-ERROR.
            IF NOT AVAIL Artbas THEN
                NEXT.
            FIND vargr OF artbas NO-LOCK NO-ERROR.
            FIND huvgr OF artbas NO-LOCK NO-ERROR.
            IF AVAIL huvgr THEN
                FIND avdeling OF huvgr NO-LOCK NO-ERROR.
            ELSE
                RELEASE avdeling.
            FIND farg  OF artbas NO-LOCK NO-ERROR.
            FIND levbas OF artbas NO-LOCK NO-ERROR.
            FIND sasong OF artbas NO-LOCK NO-ERROR.
            FIND lager WHERE lager.artikkelnr = artbas.artikkelnr AND lager.butik = butiker.butik NO-LOCK NO-ERROR.
            FIND varemerke OF artbas NO-LOCK NO-ERROR.
            ASSIGN dDbKr = StLinje.VerdiSolgt - StLinje.VVarekost NO-ERROR.
            ASSIGN dDbKr = IF dDbKr = ? THEN 0 ELSE dDbKr.
            PUT UNFORMATTED Artbas.artikkelnr ";"
                            REPLACE(Artbas.beskr,";"," ") ";"
                            STRING(dDato)     ";"
                            stlinje.butik     ";"
                            REPLACE(butiker.butnamn,";"," ")   ";"
                            StLinje.AntSolgt  ";"
                            StLinje.VerdiSolgt ";"
                            StLinje.MvaVerdi   ";"
                            dDbKr              ";"
                            StLinje.AntRabatt  ";"
                            StLinje.VerdiRabatt ";"
                            StLinje.VVarekost   ";"
                            StLinje.KjopAnt     ";"
                            StLinje.KjopVerdi   ";"
                            Artbas.levnr        ";"
                            (IF AVAIL Levbas THEN REPLACE(Levbas.levnamn,";"," ") ELSE "") ";"
                            REPLACE(Artbas.levkod,";"," ")       ";"
                            REPLACE(Artbas.levfargkod,";"," ")   ";"
                            Artbas.farg                          ";"
                            (IF AVAIL farg THEN REPLACE(farg.farbeskr,";"," ") ELSE "")     ";"
                            Artbas.sasong                        ";"
                            (IF AVAIL sasong THEN REPLACE(sasong.sasbeskr,";"," ") ELSE "") ";"
                            Artbas.vg                            ";"
                            (IF AVAIL vargr THEN REPLACE(vargr.vgbeskr,";","") ELSE "")     ";"
                            Artbas.hg                            ";"
                            (IF AVAIL huvgr THEN REPLACE(huvgr.hgbeskr,";","") ELSE "")     ";"
                            (IF AVAIL avdeling THEN avdeling.avdelingnr ELSE 0)             ";"
                            (IF AVAIL avdeling THEN REPLACE(Avdeling.AvdelingNavn,";","") ELSE "") ";" 
                             ArtBas.VMId      ";"
                            (IF AVAIL varemerke THEN REPLACE(varemerke.beskrivelse,";","") ELSE "") ";" 
                             SKIP.
        END.
        OUTPUT CLOSE.
        OS-RENAME VALUE(cOutputFileName) VALUE(cFilename).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExporteraOLD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExporteraOLD Procedure 
PROCEDURE ExporteraOLD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAarPerLinNr AS INTEGER    NO-UNDO.
    FIND butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
    ASSIGN cFilePrefix = "StTimeGrip_"
           cEksportDir = RIGHT-TRIM(cEksportDir,"\") + "\"
           iAarPerLinnr = YEAR(dDato) * 1000 + (ddato - DATE(12,31,YEAR(dDato) - 1)).
           cFileName   = cEksportDir + cFilePrefix + STRING(iAarPerLinnr) + "." + STRING(iButikkNr).
/*            cFileName   = cEksportDir + "StTimeGrip." + STRING(iButikkNr). */
    IF CAN-FIND(FIRST StLinje WHERE Stlinje.Butik = iButikkNr AND
                                    StLinje.SttypeId = "ARTIKKEL" AND
                                    StLinje.PerId = "DAG" AND
                                    StLinje.AarPerlinnr = iAarPerlinnr) THEN DO:
        OUTPUT TO VALUE(cFilename).
        DEFINE VARIABLE dArtikkelNR  AS DECIMAL    NO-UNDO.
        DEFINE VARIABLE dDbKr        AS DECIMAL    NO-UNDO.
        /* Här måste vi ta hänsayn tillårsskifte. */

        PUT UNFORMATTED "SE-Artikkelnr" ";"
                        "SE-Artbeskr"   ";"
                        "Dag"            ";"
                        "Butikk"        ";"
                        "Butikknavn"    ";"
                        "AntSolgt"      ";"
                        "Brutto solgt"  ";"
                        "Mva"           ";"
                        "Dbkr"          ";"
                        "Rabatt antall" ";"
                        "Rabatt verdi"  ";"
                        "Varekost"      ";"
                        "Kjøpt antall"  ";"
                        "Kjøpt verdi"   ";"
                        "Levnr"         ";"
                        "Navn"          ";"
                        "Lev artnr"     ";"
                        "Levfarg"       ";"
                        "SE-farg"       ";"
                        "SE-fargbeskr"  ";"
                        "Sesongkode"    ";"
                        "Sesong"        ";"
                        "Varegr"        ";"
                        "Vargr-beskr"   ";"
                        "Hovegr"        ";"
                        "Hovegr-beskr"  ";"
                        "Avdeling"       ";"
                        "Avdeling-beskr" ";" SKIP.
        FOR EACH stlinje WHERE stlinje.butik      = iButikkNr AND
                               stlinje.sttypeid   = "ARTIKKEL" AND
                               stlinje.perid      = "DAG" AND
                               stlinje.AarPerLinNr = iAarperlinnr NO-LOCK.
            ASSIGN dArtikkelNR = DECI(stlinje.dataobjekt) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                NEXT.
            FIND artbas WHERE artbas.artikkelnr = dArtikkelNR NO-LOCK NO-ERROR.
            IF NOT AVAIL Artbas THEN
                NEXT.
            FIND vargr OF artbas NO-LOCK NO-ERROR.
            FIND huvgr OF artbas NO-LOCK NO-ERROR.
            IF AVAIL huvgr THEN
                FIND avdeling OF huvgr NO-LOCK NO-ERROR.
            ELSE
                RELEASE avdeling.
            FIND farg  OF artbas NO-LOCK NO-ERROR.
            FIND levbas OF artbas NO-LOCK NO-ERROR.
            FIND sasong OF artbas NO-LOCK NO-ERROR.
            FIND lager WHERE lager.artikkelnr = artbas.artikkelnr AND lager.butik = butiker.butik NO-LOCK NO-ERROR.
            ASSIGN dDbKr = StLinje.VerdiSolgt - StLinje.MvaVerdi - StLinje.VVarekost NO-ERROR.
            ASSIGN dDbKr = IF dDbKr = ? THEN 0 ELSE dDbKr.
            PUT UNFORMATTED Artbas.artikkelnr ";"
                            REPLACE(Artbas.beskr,";"," ") ";"
                            STRING(dDato)     ";"
                            stlinje.butik     ";"
                            REPLACE(butiker.butnamn,";"," ")   ";"
                            StLinje.AntSolgt  ";"
                            StLinje.VerdiSolgt ";"
                            StLinje.MvaVerdi   ";"
                            dDbKr              ";"
                            StLinje.AntRabatt  ";"
                            StLinje.VerdiRabatt ";"
                            StLinje.VVarekost   ";"
                            StLinje.KjopAnt     ";"
                            StLinje.KjopVerdi   ";"
                            Artbas.levnr        ";"
                            (IF AVAIL Levbas THEN REPLACE(Levbas.levnamn,";"," ") ELSE "") ";"
                            REPLACE(Artbas.levkod,";"," ")       ";"
                            REPLACE(Artbas.levfargkod,";"," ")   ";"
                            Artbas.farg                          ";"
                            (IF AVAIL farg THEN REPLACE(farg.farbeskr,";"," ") ELSE "")     ";"
                            Artbas.sasong                        ";"
                            (IF AVAIL sasong THEN REPLACE(sasong.sasbeskr,";"," ") ELSE "") ";"
                            Artbas.vg                            ";"
                            (IF AVAIL vargr THEN REPLACE(vargr.vgbeskr,";","") ELSE "")     ";"
                            Artbas.hg                            ";"
                            (IF AVAIL huvgr THEN REPLACE(huvgr.hgbeskr,";","") ELSE "")     ";"
                            (IF AVAIL avdeling THEN avdeling.avdelingnr ELSE 0)             ";"
                            (IF AVAIL avdeling THEN REPLACE(Avdeling.AvdelingNavn,";","") ELSE "") ";" SKIP.
        END.
        OUTPUT CLOSE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

