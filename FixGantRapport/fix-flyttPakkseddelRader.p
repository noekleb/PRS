DEF VAR dOldArtikkelNr AS DEC NO-UNDO.
DEF VAR dNyArtikkelNr AS DEC NO-UNDO.

ASSIGN 
    dOldArtikkelNr = 9853172
    dNyArtikkelNr  = 9851647
    .

FOR EACH PksdlHode NO-LOCK WHERE 
    PkSdlHode.PksdlStatus = 10,
    EACH PkSdlLinje OF PkSdlHode EXCLUSIVE-LOCK WHERE 
        PkSdlLinje.ArtikkelNr = dOldArtikkelnr:
            
    ASSIGN 
        PkSdlLinje.ArtikkelNr = dNyArtikkelnr NO-ERROR.
END.
            
FOR EACH PksdlHode NO-LOCK WHERE 
    PkSdlHode.PksdlStatus = 10,
    EACH PkSdlPris OF PkSdlHode EXCLUSIVE-LOCK WHERE 
        PkSdlPris.ArtikkelNr = dOldArtikkelnr:
            
    ASSIGN 
        PkSdlPris.ArtikkelNr = dNyArtikkelnr NO-ERROR.
END.            
