DEF VAR iant AS INT NO-UNDO.
CURRENT-WINDOW:WIDTH = 350.
DEF VAR bFlagg AS LOG NO-UNDO.

FOR EACH Butiker:
FOR EACH DataSett EXCLUSIVE-LOCK WHERE 
    DataSett.ButikkNr = Butiker.butik AND 
    DataSett.Dato >= 08/07/2020 AND 
    DataSett.Behandlet >= 3 AND 
    DataSett.Behandlet <= 4:

    bFlagg = TRUE.
    FOR EACH bONGhODE OF dATAsETT:
        IF bONGhODE.BongStatus <> 7 THEN
            bFlagg = FALSE.
    END.
    
    iant = iant + 1.
    
    IF bFlagg = TRUE THEN
    DO:
        DataSett.Behandlet = 5.
    END.
    /*       
    DISPLAY
      bFlagg
      DataSett.ButikkNr FORMAT ">>>>>9" LABEL "Butikknummer"
      DataSett.GruppeNr FORMAT ">9" LABEL "Gruppenummer"
      DataSett.KasseNr FORMAT ">>9" LABEL "KasseNr"
      DataSett.DataSettId FORMAT ">>>>>>>>>>>>>9" LABEL "DatasettId"
      DataSett.Dato FORMAT "99/99/99" LABEL "Dato"
      STRING(dATAsETT.Tid,"HH:MM:SS")
      DataSett.SettNr FORMAT ">>>9"
      DataSett.SettStatus FORMAT "9" LABEL "SettStatus"
      DataSett.FilId FORMAT ">>>>>>>>>>>>9" LABEL "FilId"
      DataSett.FilType FORMAT ">9" LABEL "FilType"
      DataSett.Behandlet FORMAT ">9" LABEL "Behandlet status"
      DataSett.AntallLinjer FORMAT "->,>>>,>>9" LABEL "Antall linjer"
      DataSett.pfFlagg FORMAT ">9" LABEL "Status ProfitBase"
    WITH WIDTH 350.
    */
END.
END.


MESSAGE iant
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
