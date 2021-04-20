DEF VAR dDato AS DATE NO-UNDO.
DEF VAR iTTId AS INT NO-UNDO.
DEF VAR ibutNr AS INT NO-UNDO.

ASSIGN
    dDato = 05/28/2020
    iTTId = 108 /* 92 EOD, 108 Veksel */
    ibutNr = 11
    .
CURRENT-WINDOW:WIDTH = 350.
FOR EACH Kasse NO-LOCK WHERE 
    Kasse.ButikkNr = ibutNr AND 
    Kasse.GruppeNr = 1 AND 
    Kasse.KasseNr <= 90,
    EACH bongHode NO-LOCK WHERE 
        BongHode.ButikkNr = Kasse.butikkNr AND 
        BongHode.GruppeNr = Kasse.GruppeNr AND 
        BongHode.KasseNr = Kasse.KasseNr AND 
        BongHode.Dato = dDato AND 
        CAN-FIND(FIRST BongLinje WHERE 
                 BongLinje.B_Id = BongHode.B_Id AND 
                 BongLinje.TTId = iTTId),
    FIRST BongLinje NO-LOCK WHERE 
        BongLinje.ButikkNr = Kasse.ButikkNr AND 
        BongLinje.GruppeNr = Kasse.GruppeNr AND 
        BongLinje.KasseNr = Kasse.KasseNr AND
        BongLinje.Dato = BongHode.Dato AND 
        BongLinje.TTId = iTTId:
        
        
    DISPLAY
    BongLinje.butikkNr
    BongLinje.KasseNr
    BongHode.BongNr
    BongLinje.TTId
    BongLinje.TBId
    BongLinje.LinjeSum
    BongHode.Dato
    STRING(BongHode.Tid,"HH:MM:SS")
    BongHode.SelgerNr
    BongHode.SelgerNavn
    WITH WIDTH 350.
END.
/*
FOR EACH BongLinje NO-LOCK WHERE 
    BongLinje.ButikkNr = 2 AND 
    BongLinje.GruppeNr = 1 AND 
    BongLinje.KasseNr >= 1 AND 
    BongLinje.Dato = 04/28/2020 AND 
    BongLinje.TTId = 108 AND 
    BongLinje.BongNr >= 0 AND 
    BongLinje.LinjeNr >= 0,
    FIRST BongHode NO-LOCK WHERE 
        BongHode.B_Id = BongLinje.B_Id
    :

    FIND FIRST Kas_Rap WHERE 
         Kas_Rap.Dato = BongLinje.Dato AND 
         Kas_Rap.Butik = BongLinje.butik AND 
         Kas_Rap.Kasse = BongLinje.KasseNr AND 
         kas_rap.KassererNr = INT(BongHode.KassererNr) NO-ERROR.

    FIND FIRST KassererOppgj NO-LOCK WHERE 
        KassererOppgj.ButikkNr = BongLinje.Butik AND 
        KassererOppgj.Dato = bongLinje.Dato AND 
        KassererOppgj.KassererNr = 0 NO-ERROR.

    DISPLAY
        BongLinje.ButikkNr
        BongLinje.GruppeNr
        BongLinje.KasseNr
        BongLinje.Dato
        BongLinje.TTId
        KassererOppgj.OpptaltVeksel WHEN AVAILABLE KassererOppgj
        KassererOppgj.OpptaltInnVeksel WHEN AVAILABLE KassererOppgj       
        '|'
        Kas_Rap.VekselBeholdning WHEN AVAILABLE Kas_Rap
    WITH WIDTH 350.

END.
*/
