DEF VAR lcJason AS LONGCHAR NO-UNDO.                   

CURRENT-WINDOW:WIDTH = 350.
FOR EACH BongCRMLogg EXCLUSIVE-LOCK WHERE 
    BongCRMLogg.B_Id = 2004290000100100269017:
    DISPLAY
        BongCRMLogg.B_Id FORMAT ">>>>>>>>>>>>>>>>>>>>>>>>>9"
        BongCRMLogg.ButikkNr
        BongCRMLogg.GruppeNr
        BongCRMLogg.KasseNr
        BongCRMLogg.Dato
        BongCRMLogg.BongNr
        BongCRMLogg.RegistrertDatoTid
        BongCRMLogg.SendtCRM
        STRING(BongCRMLogg.JsonPayLoad) > '' COLUMN-LABEL 'Payload'

    WITH WIDTH 359.
    IF BongCRMLogg.JsonPayLoad <> ? THEN
    DO:
        lcJason = STRING(BongCRMLogg.JsonPayLoad).
        OUTPUT TO VALUE('konv\CRMBong' + STRING(BongCRMLogg.B_Id) + '.json').
        PUT UNFORMATTED 
            STRING(lcJason) 
            SKIP.
        OUTPUT CLOSE.       
        MESSAGE 'konv\CRMBong' + STRING(BongCRMLogg.B_Id) + '.json' 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        
    END.

    /*DELETE BongCRMLogg.*/ 
END.
