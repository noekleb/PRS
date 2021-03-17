DEF VAR iStrTypeID AS INT NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

CURRENT-WINDOW:WIDTH = 350. 
FOR EACH StrType EXCLUSIVE-LOCK WHERE 
    (
     StrType.Intervall = '' OR 
     Strtype.Fordeling = '' OR
     StrType.AlfaFordeling = '' OR
     StrType.Intervall = ? OR
     Strtype.fordeling = ? OR
     StrType.Alfafordeling = ?
     ) /*AND 
    NOT CAN-FIND(FIRST ArtBas WHERE 
                 ArtBas.StrTypeId = StrType.StrtypeId)*/:
    DISPLAY
        StrType.StrTypeId
        StrType.Beskrivelse
        StrType.KortNavn
        StrType.Intervall
        Strtype.Fordeling
        StrType.AlfaFordeling
        StrType.Hg
        StrType.AvdelingNr
        StrType.RegistrertDato
    WITH WIDTH 350.


END.

