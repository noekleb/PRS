/*
Denne fix rutinen skal kjøres ved oppdatering til ny versjon av SE, vhor gavekort 
tas ibruk (Versjon 4.0).
IdNr på gavekort og tilgodelapper tildeles her fortløpende.
Det er ikke mulig å matche inn og utgående, slik at disse blir liggende som separate poster.

*/

DEF VAR cTTId  AS CHAR NO-UNDO.
DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR iAntall AS INT NO-UNDO.

DEF VAR cIdentNr AS CHAR NO-UNDO.

PUBLISH 'infoDisp' ("Oppretter tilgodelapper..").

ASSIGN
    /* 66-Tilgode inn, 69-Tilgode ut*/
    cttid = "66,69"
    .

DO piLoop = 1 TO NUM-ENTRIES(cTTId):
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.TTId = INT(ENTRY(piLoop,cTtid)):
    FIND BongHode NO-LOCK WHERE
        BongHode.B_Id = BongLinje.B_Id NO-ERROR.
    IF BongHode.KundeNr > 0 THEN
        FIND Kunde OF BongHode NO-LOCK NO-ERROR.
    IF BongHode.MedlemsNr > 0 THEN
        FIND Medlem OF BongHode NO-ERROR.

    FIND LAST Tilgode NO-LOCK WHERE
        Tilgode.Butnr = BongLinje.ButikkNr USE-INDEX tilgodeidx1 NO-ERROR.
    IF AVAILABLE Tilgode THEN
    DO ON ERROR UNDO, LEAVE:
        ASSIGN
        cIdentNr = string(INT(Tilgode.IdentNr) + 1,"99999")
        NO-ERROR.
    END.
    ELSE 
        cIdentNr = "00001".

    ASSIGN
        BongLinje.Strekkode = cIdentNr
        .

    /* Oppretter eller skriver over */
    FIND Tilgode EXCLUSIVE-LOCK WHERE
        Tilgode.ButNr = BongLinje.butikkNr AND
        Tilgode.IdentNr = cIdentNr NO-ERROR.
    IF NOT AVAILABLE Tilgode THEN
        CREATE Tilgode.
    ASSIGN
        iAntall                = iAntall + 1
        Tilgode.ButNr          = BongLinje.ButikkNr
        Tilgode.IdentNr        = cIdentNr
        Tilgode.identtype      = 1
        Tilgode.modus          = Tilgode.modus
        Tilgode.dato           = BongLinje.TransDato
        Tilgode.tid            = BongLinje.TransTid
        Tilgode.kassenr        = BongLinje.KasseNr
        Tilgode.kassnr         = BongHode.KassererNr
        Tilgode.bongnr         = BongLinje.BongNr
        Tilgode.gyldigdato     = TODAY + 90
        Tilgode.belop          = BongLinje.LinjeSum
        Tilgode.FraB_Id        = BongLinje.B_Id
        Tilgode.KundeNr        = BongHode.KundeNr
        Tilgode.MedlemsNr      = BongHode.MedlemsNr
        .
    /* Tilgode inn */
    IF BongLinje.TTId = 66 THEN
        ASSIGN
        Tilgode.bruktdato      = BongLinje.TransDato
        Tilgode.brukttid       = BongLinje.TransTid
        Tilgode.BruktB_Id      = BongLinje.B_Id
        Tilgode.BruktButNr     = BongLinje.ButikkNr
        Tilgode.BruktBongNr    = BongLinje.BongNr
        .
    IF AVAILABLE Medlem THEN
        ASSIGN
        Tilgode.MForNavn       = Medlem.ForNavn
        Tilgode.MAdresse1      = Medlem.Adresse1
        Tilgode.MPostNr        = Medlem.PostNr
        Tilgode.MTelefon       = Medlem.Telefon
        Tilgode.MEtterNavn     = Medlem.EtterNavn
        .
    IF AVAILABLE Kunde THEN
        ASSIGN
        Tilgode.KNavn          = Kunde.Navn
        Tilgode.KAdresse1      = Kunde.Adresse1
        Tilgode.KPostNr        = Kunde.PostNr
        Tilgode.KTelefon       = Kunde.Telefon
        .

    /*
    IF iAntall MODULO 200 = 0 THEN
    DO:
        PAUSE 0.
        DISPLAY
        "Tilgode:" iAntall
        WITH FRAME g.
    END.
    */

  END.
END.

PUBLISH 'infoDisp' ("Oppretter gavekort..").
ASSIGN
    /* 53 Gavekort inn, 134-Gavekort ut. */
    cttid = "53,134"
    .

DO piLoop = 1 TO NUM-ENTRIES(cTTId):
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
    BongLinje.TTId = INT(ENTRY(piLoop,cTtid)):
    FIND BongHode NO-LOCK WHERE
        BongHode.B_Id = BongLinje.B_Id NO-ERROR.
    IF BongHode.KundeNr > 0 THEN
        FIND Kunde OF BongHode NO-LOCK NO-ERROR.
    IF BongHode.MedlemsNr > 0 THEN
        FIND Medlem OF BongHode NO-ERROR.

    FIND LAST Gavekort NO-LOCK WHERE
        Gavekort.Butnr = BongLinje.ButikkNr USE-INDEX Gavekortidx1 NO-ERROR.
    IF AVAILABLE Gavekort THEN
    DO ON ERROR UNDO, LEAVE:
        ASSIGN
        cIdentNr = string(INT(Gavekort.IdentNr) + 1,"99999")
        NO-ERROR.
    END.
    ELSE 
        cIdentNr = "00001".

    ASSIGN
        BongLinje.Strekkode = cIdentNr
        .

    /* Oppretter eller skriver over */
    FIND Gavekort EXCLUSIVE-LOCK WHERE
        Gavekort.ButNr = BongLinje.butikkNr AND
        Gavekort.IdentNr = cIdentNr NO-ERROR.
    IF NOT AVAILABLE Gavekort THEN
        CREATE Gavekort.
    ASSIGN
        iAntall                = iAntall + 1
        Gavekort.ButNr          = BongLinje.ButikkNr
        Gavekort.IdentNr        = cIdentNr
        Gavekort.identtype      = 1
        Gavekort.modus          = Gavekort.modus
        Gavekort.dato           = BongLinje.TransDato
        Gavekort.tid            = BongLinje.TransTid
        Gavekort.kassenr        = BongLinje.KasseNr
        Gavekort.kassnr         = BongHode.KassererNr
        Gavekort.bongnr         = BongLinje.BongNr
        Gavekort.gyldigdato     = TODAY + 90
        Gavekort.belop          = BongLinje.LinjeSum
        Gavekort.FraB_Id        = BongLinje.B_Id
        Gavekort.KundeNr        = BongHode.KundeNr
        Gavekort.MedlemsNr      = BongHode.MedlemsNr
        .
    /* Gavekort inn */
    IF BongLinje.TTId = 53 THEN
        ASSIGN
        Gavekort.bruktdato      = BongLinje.TransDato
        Gavekort.brukttid       = BongLinje.TransTid
        Gavekort.BruktB_Id      = BongLinje.B_Id
        Gavekort.BruktButNr     = BongLinje.ButikkNr
        Gavekort.BruktBongNr    = BongLinje.BongNr
        .
    IF AVAILABLE Medlem THEN
        ASSIGN
        Gavekort.MForNavn       = Medlem.ForNavn
        Gavekort.MAdresse1      = Medlem.Adresse1
        Gavekort.MPostNr        = Medlem.PostNr
        Gavekort.MTelefon       = Medlem.Telefon
        Gavekort.MEtterNavn     = Medlem.EtterNavn
        .
    IF AVAILABLE Kunde THEN
        ASSIGN
        Gavekort.KNavn          = Kunde.Navn
        Gavekort.KAdresse1      = Kunde.Adresse1
        Gavekort.KPostNr        = Kunde.PostNr
        Gavekort.KTelefon       = Kunde.Telefon
        .

    /*
    IF iAntall MODULO 200 = 0 THEN
    DO:
        PAUSE 0.
        DISPLAY
        "Gavekort:" iAntall
        WITH FRAME gg.
    END.
    */

  END.
END.

