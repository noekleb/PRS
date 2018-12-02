CURRENT-WINDOW:WIDTH = 132.

DEF VAR lArtikkelNr AS DEC NO-UNDO. 
DEF VAR cArtikkelnr AS CHAR NO-UNDO.
DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.


DEF BUFFER bTransLogg FOR TransLogg.
  DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iButikknr AS INTEGER     NO-UNDO.

  DEFINE TEMP-TABLE tt_fsdag NO-UNDO
    FIELD butikknr AS INTE
    FIELD dato AS DATE
    INDEX bd IS PRIMARY UNIQUE butikknr dato.

  INPUT FROM "c:\tmp\prfs_ejstat.d".
  REPEAT:
    IMPORT UNFORMATTED cc.
    cc = TRIM(cc).
    IF cc = "" THEN
        NEXT.
    iButikknr = INT(ENTRY(1,cc,";")).
    DO:
        CREATE tt_fsdag.
        ASSIGN tt_fsdag.butikknr = iButikknr
               tt_fsdag.dato     = DATE(ENTRY(2,cc,";")).
    END.
   
  END.
  INPUT CLOSE.
FOR EACH tt_fsdag:
    FOR EACH kasse WHERE kasse.butikknr = tt_fsdag.butikknr AND 
                       kasse.gruppenr = 1 NO-LOCK:
        FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
           BongLinje.ButikkNr = tt_fsdag.butikknr AND 
           BongLinje.GruppeNr = 1 AND 
           BongLinje.KasseNr  = kasse.KasseNr AND 
           BongLinje.Dato     = tt_fsdag.Dato :
    
          IF CAN-DO( "1,2,70,71,72,73,74,75,76,77,78,79,80,81,82,84,85,86,87,88,89",STRING(BongLinje.HovedGr)) THEN
          KORRIGER_BONGLINJE: 
          DO:
              cArtikkelNr = "9000" + string(BongLinje.HovedGr,"99").
              IF bonglinje.artikkelnr = cArtikkelnr THEN
                LEAVE KORRIGER_BONGLINJE.

              lArtikkelnr = DECI(cArtikkelnr).
              FIND ArtBas NO-LOCK WHERE 
                  ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
              IF AVAILABLE ArtBas THEN
                  ASSIGN
                 BongLinje.OriginalData = "KORR=" + BongLinje.ArtikkelNr + '|' + STRING(BongLinje.VareGr) + '|' + BongLinje.Strekkode
                  BongLinje.ArtikkelNr   = STRING(ArtBas.ArtikkelNr)
                  BongLinje.VareGr       = ArtBas.Vg
                  Bonglinje.lopenr        = ArtBas.lopnr
                  BongLinje.Strekkode    = STRING(ArtBas.ArtikkelNr).
          END.
        END.
    END.
    OUTPUT TO "c:\tmp\prfs_ejstat_run.d" APPEND.
    EXPORT tt_fsdag.
    OUTPUT CLOSE.
END.
