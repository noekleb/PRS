/* strAnalyse.p */
DEFINE INPUT PARAMETER cButLst    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER dFraDato   AS DATE NO-UNDO.
DEFINE INPUT PARAMETER dTilDato   AS DATE NO-UNDO.
DEFINE INPUT PARAMETER cAvdLst    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cHgLst     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cVgLst     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cSesLst    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cVmLst     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cLevLst    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cToggelLst AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER cUtFilNavn AS CHAR NO-UNDO.

DEFINE VARIABLE iLoop      AS INTEGER   NO-UNDO.

DEF TEMP-TABLE StrAnalyse 
    FIELD Butik         AS INT FORMAT ">>>>>9"
    FIELD Storl         AS CHAR FORMAT "x(10)"
    FIELD AntallSolgt   AS INT FORMAT "->>,>>>,>>9"
    FIELD AntallKjopt   AS INT FORMAT "->>,>>>,>>9"
    FIELD AntallRetur   AS INT FORMAT "->>,>>>,>>9"
    FIELD VerdiSolgt    AS DEC FORMAT "->>>,>>>,>>9"
    FIELD VerdiKjopt    AS DEC FORMAT "->>>,>>>,>>9"
    FIELD VerdiRetur    AS DEC FORMAT "->>>,>>>,>>9"
    FIELD AvdelingNr    AS INT FORMAT ">>>9"
    FIELD AvdelingNavn  AS CHAR FORMAT "x(40)"
    FIELD Hg            AS INT FORMAT ">>>>>9"
    FIELD HgBeskr       AS CHAR FORMAT "x(40)"
    FIELD Vg            AS INT FORMAT ">>>>>9"
    FIELD VgBeskr       AS CHAR FORMAT "x(40)"
    FIELD SaSong        AS INTEGER FORMAT ">>>>>9"
    FIELD SasBeskr      AS CHARACTER FORMAT "x(40)"
    FIELD VmId          AS INTEGER FORMAT ">>>>>9"
    FIELD VmBeskrivelse AS CHARACTER FORMAT "x(40)"
    FIELD LevNr         AS INTEGER FORMAT ">>>>>9"
    FIELD LevNamn       AS CHARACTER FORMAT "x(40)"
    INDEX Storrelse Butik AvdelingNr Hg Vg Storl Sasong VmId LevNr
    .
    
DEF STREAM Ut.

ASSIGN
    cUtFilNavn = 'c:\appdir\strAnalyse.csv'
    .
    
/* MESSAGE                                */
/*     cButLst     SKIP                   */
/*     dFraDato     SKIP                  */
/*     dTilDato        SKIP               */
/*     cAvdLst     SKIP                   */
/*     cHgLst     SKIP                    */
/*     cVgLst     SKIP                    */
/*     cSesLst     SKIP                   */
/*     cVmLst     SKIP                    */
/*     cLevLst     SKIP                   */
/*     cToggelLst     SKIP                */
/*     cUtFilNavn                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* RETURN.                                */

RUN uttrekkStorl.
    
IF CAN-FIND(FIRST StrAnalyse) then
DO:
    RUN eksporterStorl.    
    RUN strAnalyse_til_excel.
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE eksporterStorl:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    OUTPUT STREAM Ut TO VALUE(cUtFilNavn) NO-ECHO.
    PUT STREAM Ut UNFORMATTED
    'Butik;'       
    'AvdelingNr;'  
    'AvdelingNavn;' 
    'Hg;'          
    'HgBeskr;'     
    'Vg;'          
    'VgBeskr;'     
    'Storl;'       
    'Solgt antall;'
    'Solgt verdi;'
    'Kjopt antall;'
    'Kjopt verdi;'
    'Retur antall;'
    'Retur verdi;'
    'Sasong;'
    'SasBeskr;'
    'VmId;'
    'VmBeskrivelse;'
    'Levnr;'
    'LevNavn'
        SKIP.
  
    FOR EACH StrAnalyse:
        PUT STREAM Ut UNFORMATTED
        StrAnalyse.Butik ';'      
        StrAnalyse.AvdelingNr ';'  
        StrAnalyse.AvdelingNavn ';' 
        StrAnalyse.Hg ';'          
        StrAnalyse.HgBeskr ';'     
        StrAnalyse.Vg ';'          
        StrAnalyse.VgBeskr ';'     
        StrAnalyse.Storl ';'       
        StrAnalyse.AntallSolgt ';'
        StrAnalyse.VerdiSolgt ';'
        StrAnalyse.AntallKjopt ';'
        StrAnalyse.VerdiKjopt ';'
        StrAnalyse.AntallRetur ';'
        StrAnalyse.VerdiRetur ';'
        StrAnalyse.Sasong ';'
        StrAnalyse.SasBeskr ';'
        StrAnalyse.VmId  ';'
        StrAnalyse.VmBeskrivelse  ';'
        StrAnalyse.LevNr  ';'
        StrAnalyse.LevNamn
        SKIP.
    END.
    OUTPUT STREAM Ut CLOSE.

END PROCEDURE.

PROCEDURE uttrekkStorl:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* Looper de valgte butikkene */
    BUTLOOP:
    DO iLoop = 1 TO NUM-ENTRIES(cButLst):
        FOR EACH TransLogg NO-LOCK WHERE
            TransLogg.Butik = INT(ENTRY(iLoop,cButLst)) AND 
            /*Translogg.TTId = 1 */
            TransLogg.TTId >=  1 AND
            TransLogg.TTId <= 10:
                
            /* Salg, kjøp og retur. */
            IF NOT CAN-DO('1,5,10',STRING(TransLogg.TTId))then NEXT.

            FIND ArtBAs NO-LOCK WHERE
                ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
            IF NOT AVAILABLE ArtBas THEN
                NEXT.
            FIND Varemerke OF ArtBas    NO-LOCK NO-ERROR.
            FIND SaSong    OF ArtBas    NO-LOCK NO-ERROR.

            FIND VarGr     OF Translogg NO-LOCK NO-ERROR.
            FIND Huvgr     OF VarGr     NO-LOCK NO-ERROR.
            FIND Avdeling  OF HuvGr     NO-LOCK NO-ERROR.
            FIND LevBas    OF Translogg NO-LOCK NO-ERROR.

            FILTER:
            DO:
                IF cAvdLst <> '' AND AVAILABLE Avdeling THEN 
                    IF NOT CAN-DO(cAvdLst,STRING(Avdeling.AvdelingNr)) THEN NEXT.
                IF cHgLst <> '' AND AVAILABLE HuvGr THEN 
                    IF NOT CAN-DO(cHgLst,STRING(HuvGr.Hg)) THEN NEXT.
                IF cVgLst <> '' AND AVAILABLE VarGr THEN 
                    IF NOT CAN-DO(cVgLst,STRING(VarGr.Hg)) THEN NEXT.
                IF cSesLst <> '' AND AVAILABLE Sasong THEN 
                    IF NOT CAN-DO(cSesLst,STRING(SaSong.Sasong)) THEN NEXT.
                IF cVmLst <> '' AND AVAILABLE Varemerke THEN 
                    IF NOT CAN-DO(cVmLst,STRING(Varemerke.VmId)) THEN NEXT.
                IF cLevLst <> '' AND AVAILABLE LevBas THEN 
                    IF NOT CAN-DO(cLevLst,STRING(LevBas.LevNr)) THEN NEXT.
            END. /* FILTER */

            FIND FIRST StrAnalyse WHERE 
                StrAnalyse.Butik      = (IF ENTRY(1,cToggelLst) = 'YES' THEN TransLogg.Butik ELSE 0) AND
                StrAnalyse.AvdelingNr = (IF ENTRY(2,cToggelLst) = 'YES' THEN Avdeling.AvdelingNr ELSE 0) AND
                StrAnalyse.Hg         = (IF ENTRY(3,cToggelLst) = 'YES' THEN HuvGr.Hg ELSE 0) AND
                StrAnalyse.Vg         = (IF ENTRY(4,cToggelLst) = 'YES' THEN VarGr.Vg ELSE 0) AND
                StrAnalyse.Storl      = TransLogg.Stor AND 
                StrAnalyse.Sasong     = (IF ENTRY(5,cToggelLst) = 'YES' THEN ArtBas.Sasong ELSE 0) AND
                StrAnalyse.VmId       = (IF ENTRY(6,cToggelLst) = 'YES' THEN ArtBas.VmId ELSE 0) AND
                StrAnalyse.LevNr      = (IF ENTRY(7,cToggelLst) = 'YES' THEN Artbas.LevNr ELSE 0)
                NO-ERROR.
            IF NOT AVAILABLE StrAnalyse THEN
            DO:
                CREATE StrAnalyse.
                ASSIGN
                    StrAnalyse.Butik         = (IF ENTRY(1,cToggelLst) = 'YES' THEN TransLogg.Butik ELSE 0) 
                    StrAnalyse.AvdelingNr    = IF AVAILABLE Avdeling THEN (IF ENTRY(2,cToggelLst) = 'YES' THEN Avdeling.AvdelingNr ELSE 0) ELSE 0
                    StrAnalyse.Hg            = IF AVAILABLE VarGr THEN (IF ENTRY(3,cToggelLst) = 'YES' THEN VarGr.Hg ELSE 0) ELSE 0 
                    StrAnalyse.Vg            = (IF ENTRY(4,cToggelLst) = 'YES' THEN VarGr.Vg ELSE 0) 
                    StrAnalyse.Storl         = TransLogg.Stor 
                    StrAnalyse.AvdelingNavn  = IF AVAILABLE Avdeling THEN (IF ENTRY(2,cToggelLst) = 'YES' THEN Avdeling.AvdelingNavn ELSE '') ELSE ''
                    StrAnalyse.HgBeskr       = IF AVAILABLE HuvGr THEN (IF ENTRY(3,cToggelLst) = 'YES' THEN Huvgr.HgBeskr ELSE '') ELSE ''
                    StrAnalyse.VgBeskr       = IF AVAILABLE VarGr THEN (IF ENTRY(3,cToggelLst) = 'YES' THEN VarGr.VgBeskr ELSE '') ELSE ''
                    StrAnalyse.Sasong        = IF AVAILABLE ArtBas THEN (IF ENTRY(5,cToggelLst) = 'YES' THEN ArtBas.Sasong ELSE 0) ELSE 0
                    StrAnalyse.SasBeskr      = IF AVAILABLE SaSong THEN (IF ENTRY(5,cToggelLst) = 'YES' THEN SaSong.SasBeskr ELSE '') ELSE ''
                    StrAnalyse.VmId          = IF AVAILABLE ArtBas THEN (IF ENTRY(6,cToggelLst) = 'YES' THEN ArtBas.VmId ELSE 0) ELSE 0
                    StrAnalyse.VmBeskrivelse = IF AVAILABLE Varemerke THEN (IF ENTRY(6,cToggelLst) = 'YES' THEN Varemerke.Beskrivelse ELSE '') ELSE ''
                    StrAnalyse.LevNr         = IF AVAILABLE ArtBas THEN (IF ENTRY(7,cToggelLst) = 'YES' THEN Artbas.LevNr ELSE 0) ELSE 0
                    StrAnalyse.LevNamn       = IF AVAILABLE LevBas THEN (IF ENTRY(7,cToggelLst) = 'YES' THEN LevBas.LevNamn ELSE '') ELSE ''
                    .
            END.
            IF TransLogg.TTId = 1 THEN 
                ASSIGN
                    StrAnalyse.AntallSolgt = StrAnalyse.AntallSolgt + TransLogg.Antall
                    StrAnalyse.VerdiSolgt  = StrAnalyse.VerdiSolgt  + (TransLogg.Antall * (IF Translogg.VVareKost <> ? THEN TransLogg.VVareKost ELSE 0))
                    .
            ELSE IF TransLogg.TTId = 5 THEN 
                ASSIGN
                    StrAnalyse.AntallKjopt = StrAnalyse.AntallKjopt + TransLogg.Antall
                    StrAnalyse.VerdiKjop   = StrAnalyse.VerdiKjop  + (TransLogg.Antall * (IF Translogg.VVareKost <> ? THEN TransLogg.VVareKost ELSE 0))
                    .
            ELSE IF TransLogg.TTId = 10 THEN 
                ASSIGN
                    StrAnalyse.AntallRetur = StrAnalyse.AntallRetur + TransLogg.Antall
                    StrAnalyse.VerdiRetur  = StrAnalyse.VerdiRetur + (TransLogg.Antall * (IF Translogg.VVareKost <> ? THEN TransLogg.VVareKost ELSE 0))
                    .
        END.
    
    END. /* BUTLOOP */    


END PROCEDURE.

PROCEDURE ExcelSheetParams:

    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    PUBLISH "ExcelSheetParams" (ihObject,chExcelInstance,chWorkbook,chWorksheet,iCount - 1).
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipObject        AS HANDLE     NO-UNDO.
    DEF INPUT PARAMETER ipExcelInstance AS CHAR       NO-UNDO.
    DEF INPUT PARAMETER ipWorkbook      AS COM-HANDLE NO-UNDO.
    DEF INPUT PARAMETER ipWorksheet     AS COM-HANDLE NO-UNDO.
    DEF INPUT PARAMETER ipI             AS INT        NO-UNDO.

      ipWorkSheet:Range("D:F"):NumberFormat = "@".
      ipWorkSheet:Range("P:P"):NumberFormat = "@".
END PROCEDURE.

PROCEDURE strAnalyse_til_excel:

  IF SEARCH(cUtFilNavn) <> ? THEN
      RUN strAnalyse_til_excel.p (cUtFilNavn).

END PROCEDURE.
