DEF VAR iDo AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    iDo = 0  /* 4056734006579 */
    .

FOR EACH KORdreLinje WHERE 
    KOrdreLinje.KOrdre_Id = 1190000005:

    DISPLAY
        KOrdreLinje.KOrdre_Id
        KOrdreLinje.KOrdreLinjeNr
        KOrdreLinje.KopiKOrdreLinjeNr
        KOrdreLinje.VareNr
        KOrdreLinje.Aktiv
        KOrdreLinje.Returnert        
    WITH WIDTH 350.
    IF iDo = 1 THEN 
        KOrdreLinje.Returnert = FALSE.
END.
