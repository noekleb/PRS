    
FOR EACH ArtBAs NO-LOCK WHERE 
    NOT CAN-FIND(VarGr OF ArtBas):

    RUN OpprettVg.
END.
              
PROCEDURE OpprettVg:
    FIND VarGr WHERE
        VarGr.Vg = ArtBAs.Vg NO-LOCK NO-WAIT NO-ERROR.
    IF NOT AVAILABLE VarGr AND NOT LOCKED VarGr THEN 
      DO:
          CREATE VarGr.
          ASSIGN
/*              VarGr.Vg         = tmpvArticle_NO.nArtGroup*/
              VarGr.Vg         = ArtBas.Vg
              VarGr.VgBeskr    = '* Automatisk opprettet'
              VarGr.Hg         = ArtBas.Hg
              VarGr.MomsKod    = 1
              VarGr.Kost_Proc  = 65
              NO-ERROR.
          FOR EACH Kategori NO-LOCK WHERE
              Kategori.KatNr <= 4:
              IF NOT CAN-FIND(FIRST VgKat WHERE
                              VgKat.Vg    = VarGr.Vg AND
                              VgKat.VgKat = Kategori.KatNr) THEN
              DO:
                  CREATE VgKat.
                  ASSIGN
                  VgKat.Vg    = VarGr.Vg
                  VgKat.VgKat = Kategori.KatNr
                  VgKat.KatNr = Kategori.KatNr
                  NO-ERROR.
              END.
          END.
      END.
END PROCEDURE.
