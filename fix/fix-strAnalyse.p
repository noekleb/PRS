/* fix-strAnalyse.p */
DEF VAR cUtFilNavn AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 300.

DEF TEMP-TABLE StrAnalyse 
    FIELD Butik        AS INT FORMAT ">>>>>9"
    FIELD Storl        AS CHAR FORMAT "x(10)"
    FIELD Antall       AS INT FORMAT "->>,>>>,>>9"
    FIELD Verdi        AS DEC FORMAT "->>>,>>>,>>9"
    FIELD AvdelingNr   AS INT FORMAT ">>>9"
    FIELD AvdelingNavn AS CHAR FORMAT "x(40)"
    FIELD Hg           AS INT FORMAT ">>>>>9"
    FIELD HgBeskr      AS CHAR FORMAT "x(40)"
    FIELD Vg           AS INT FORMAT ">>>>>9"
    FIELD VgBeskr      AS CHAR FORMAT "x(40)"
    INDEX Storrelse Butik AvdelingNr Hg Vg Storl
    .
    
DEF STREAM Ut.

ASSIGN
    cUtFilNAvn = 'c:\appdir\strAnalyse.csv'.
FOR EACH TransLogg NO-LOCK WHERE
    TransLogg.Butik = 15 AND 
    TransLogg.TTId = 1:

    FIND ArtBAs NO-LOCK WHERE
        ArtBas.ArtikkelNr = TransLogg.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT.
    FIND Huvgr OF ArtBas NO-LOCK NO-ERROR.
    FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.



    FIND FIRST StrAnalyse WHERE 
        StrAnalyse.Butik      = TransLogg.Butik AND
        StrAnalyse.AvdelingNr = Avdeling.AvdelingNr AND
        StrAnalyse.Hg         = HuvGr.Hg AND
        StrAnalyse.Vg         = VarGr.Vg AND
        StrAnalyse.Storl      = TransLogg.Stor NO-ERROR.
    IF NOT AVAILABLE StrAnalyse THEN
    DO:
        CREATE StrAnalyse.
        ASSIGN
            StrAnalyse.Butik      = TransLogg.Butik 
            StrAnalyse.AvdelingNr = Avdeling.AvdelingNr
            StrAnalyse.Hg         = HuvGr.Hg 
            StrAnalyse.Vg         = VarGr.Vg 
            StrAnalyse.Storl      = TransLogg.Stor 
            StrAnalyse.AvdelingNavn = Avdeling.AvdelingNavn
            StrAnalyse.HgBeskr    = Huvgr.HgBeskr
            StrAnalyse.VgBeskr    = VarGr.VgBeskr
            .
    END.
    ASSIGN
        StrAnalyse.Antall = StrAnalyse.Antall + TransLogg.Antall
        StrAnalyse.Verdi  = StrAnalyse.Verdi  + (TransLogg.Antall * (IF Translogg.VVareKost <> ? THEN TransLogg.VVareKost ELSE 0))
        .
END.

OUTPUT STREAM Ut TO VALUE(cUtFilNavn) NO-ECHO.
      PUT STREAM Ut UNFORMATTED
          'Butik;'       
          'AvdelingNr;'  
          'Hg;'          
          'Vg;'          
          'Storl;'       
          'AvdelingNavn;' 
          'HgBeskr;'     
          'VgBeskr;'     
          'Antall;'
          'Verdi'
      SKIP.
  
  FOR EACH StrAnalyse:
      PUT STREAM Ut UNFORMATTED
          StrAnalyse.Butik ';'      
          StrAnalyse.AvdelingNr ';'  
          StrAnalyse.Hg ';'          
          StrAnalyse.Vg ';'          
          StrAnalyse.Storl ';'       
          StrAnalyse.AvdelingNavn ';' 
          StrAnalyse.HgBeskr ';'     
          StrAnalyse.VgBeskr ';'     
          StrAnalyse.Antall ';'
          StrAnalyse.Verdi
          SKIP.
  END.
OUTPUT STREAM Ut CLOSE.
