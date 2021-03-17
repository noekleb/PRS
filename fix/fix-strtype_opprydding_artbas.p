CURRENT-WINDOW:WIDTH = 250.

DEF VAR iAnt AS INT NO-UNDO.

DEF VAR iStrTypeId AS INT NO-UNDO.

FIX-BLOKKEN:
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
    ArtBas.StrTypeId >= 1000 
    BY ArtBas.StrTypeId:
    iAnt = iAnt + 1.

    iStrTypeId = 0.
    RUN setStorrelsestype.p (ArtBas.ArtikkelNr, 0, FALSE, OUTPUT iStrTypeId).
        
    find StrType of ArtBAs.
    if available StrType and 
        not can-find(first STRTStr of STRType) then 
        do:
            for each StrTSTr of StrType:
                delete STRTstr.
            end.
            delete STrType.
        end.
        
     IF iStrTypeId > 1 THEN
        ArtBas.StrTypeID = iStrTypeId.
     else 
        ArtBAs.StrTypeId = 2.
       
        
    
END. /* FIX-BLOKKEN */

MESSAGE 'Fikset:' iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
