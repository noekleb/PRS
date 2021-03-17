/* fix-visFakturaNavision.p */

DEF VAR cButLst AS CHAR NO-UNDO. 

ASSIGN 
    cButLst = '1,20,10,40,10100,10110,10120,10130,10140'
    .
                     
CURRENT-WINDOW:WIDTH = 350.
CURRENT-WINDOW:HEIGHT = 40.

FOR EACH FakturaHode EXCLUSIVE-LOCK WHERE 
    FakturaHode.PkSdlNr = '237367'
    /*
    FakturaHode.FakturertDato = 01/28/2021
    */
    :

    FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
    /*
    IF NOT CAN-DO(cButLst,STRING(Kunde.ButikkNr)) THEN
        NEXT.
    */
    DISPLAY
        FakturaHode.ButikkNr
        FakturaHode.FakturaNr
        FakturaHode.PkSdlNr
        FakturaHode.Dato
        FakturaHode.KundeNr
        Kunde.butikkNr
        FakturaHode.EksportertDato
        FakturaHode.SendingsNr


        /*
        FakturaEksport.SendingsNr       = iSeqNr
        FakturaEksport.Opphav           = 1 /* Dette feltet brukes ikke */
        FakturaEksport.EksporterDatoTid = NOW
        FakturaEksport.EksportertAv     = USERID('SkoTex') 
        FakturaEksport.Merknad          = 'Navision eksport ' + STRING(NOW,"99/99/99 HH:MM:SS")
        */

    WITH WIDTH 350.
    /*
    ASSIGN
        FakturaHode.SendingsNr = ''
        FakturaHode.EksportertDato = ?
        FakturaHode.EksportertAv = ''
        .
    */
END.
