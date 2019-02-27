
RUN x-klargjorprisko.w (?).

OUTPUT TO ".\kom\ut\posdump.txt".

DEFINE VARIABLE cString        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTilbudFraDato AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTilbudTilDato AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTilbudFraTid  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTilbudTilTid  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cBestNr AS CHARACTER   NO-UNDO.
DEFINE BUFFER bufElogg FOR Elogg.

FOR EACH moms NO-LOCK.
    cString = "MOMS" + ";" + STRING(Moms.Momskod) + ";" + 
                    REPLACE(REPLACE(Moms.Beskrivelse,";",""),'"'," ") + ";" + 
                             STRING(Moms.momsproc) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    PUT UNFORMATTED cString SKIP.
END.
FOR EACH VARGR NO-LOCK.
    cString = "VARGR" + ";" + STRING(VarGr.Vg) + ";" + 
                     REPLACE(REPLACE(VarGr.vgbeskr,";",""),'"'," ") + ";" + 
                              STRING(VarGr.Momskod) + ";" + 
                              STRING(Vargr.kost_proc) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    PUT UNFORMATTED cString SKIP.
END.
FOR EACH ArtBas WHERE artbas.edato > TODAY - 6 NO-LOCK:
    IF Artbas.LopNr = ? THEN
        NEXT.
    IF NOT CAN-FIND(FIRST artpris OF Artbas) THEN
        NEXT.
    FIND Bilderegister OF artbas NO-LOCK NO-ERROR.
    cString = "ARTBAS" + ";" + STRING(ArtBas.Artikkelnr) + ";" + 
                               STRING(ArtBas.Vg)         + ";" + 
                               STRING(ArtBas.Lopnr)      + ";" + 
                      REPLACE(REPLACE(ArtBas.Bongtekst,";",""),'"'," ") + ";" + 
                               STRING(ArtBas.Opris,"J/N") + ";" +
                  IF AVAIL Bilderegister THEN "mini" + bilderegister.filnavn ELSE "" NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    PUT UNFORMATTED cString SKIP.
    FIND FIRST artpris OF Artbas WHERE Artpris.profilnr = 1 NO-LOCK.
    IF ArtPris.TilBud THEN DO:
        IF ArtPris.TilbudFraDato = ? THEN
            ASSIGN cTilbudFraDato = ""
                   iTilbudFraTid  = 0.
        ELSE
            ASSIGN cTilbudFraDato = STRING(YEAR(ArtPris.TilbudFraDato),"9999") +
                                    STRING(MONTH(ArtPris.TilbudFraDato),"99")  +
                                    STRING(DAY(ArtPris.TilbudFraDato),"99")
                   iTilbudFraTid  = ArtPris.TilbudFraTid.
        IF ArtPris.TilbudTilDato = ? THEN
            ASSIGN cTilbudTilDato = ""
                   iTilbudTilTid  = 0.
        ELSE
            ASSIGN cTilbudTilDato = STRING(YEAR(ArtPris.TilbudTilDato),"9999") +
                                    STRING(MONTH(ArtPris.TilbudTilDato),"99")  +
                                    STRING(DAY(ArtPris.TilbudTilDato),"99")
                   iTilbudTilTid  = ArtPris.TilbudTilTid.
    END.
    ELSE
        ASSIGN cTilbudFraDato = ""
               cTilbudTilDato = ""
               iTilbudFraTid  = 0
               iTilbudTilTid  = 0.

    cString = "ARTPRIS" + ";" + STRING(ArtPris.Artikkelnr)   + ";" + 
                                STRING(Artpris.profilnr)     + ";" + 
                                STRING(Artpris.tilbud,"J/N") + ";" + 
                                       cTilbudFraDato        + ";" + 
                                STRING(iTilbudFraTid)        + ";" + 
                                       cTilbudTilDato        + ";" + 
                                STRING(iTilbudTilTid)        + ";" + 
                                STRING(Artpris.pris[1])      + ";" + 
                                STRING(Artpris.pris[2])      + ";" + 
                                STRING(Artpris.VareKost[1])  + ";" + 
                                STRING(Artpris.VareKost[2]).
    PUT UNFORMATTED cString SKIP.
    FOR EACH StrekKode OF ArtBas NO-LOCK:
        IF LENGTH(StrekKode.Bestillingsnummer) = 12 THEN
           cBestNr = "0" + SUBSTR(StrekKode.Bestillingsnummer,1,7) + SUBSTR(StrekKode.Bestillingsnummer,9).
        ELSE
            cBestNr = StrekKode.Bestillingsnummer.
        FIND strkonv OF strekkode NO-LOCK NO-ERROR.
        cString = "STREKKODE" + ";" + STRING(StrekKode.Artikkelnr)   + ";" + 
                                      cBestNr                        + ";" + 
                                      STRING(StrekKode.IKasse,"J/N") + ";" + 
                                      StrekKode.Kode                 + ";" + 
                                      STRING(StrekKode.KodeType)     + ";" + 
                                      STRING(StrekKode.StrKode)      + ";" + 
                                      (IF AVAIL strkonv THEN TRIM(strkonv.storl) ELSE "") + ";" +
                                      STRING(StrekKode.VareId).
        PUT UNFORMATTED cString SKIP.
    END.
END.
/* IF CAN-FIND(FIRST SysPara WHERE SysPara.SysHId = 20 AND SysPara.SysGr = 3) THEN DO:                                                     */
/*     FOR FIRST SysHode WHERE SysHode.SysHId = 20 NO-LOCK:                                                                                */
/*         FOR FIRST SysGruppe WHERE SysGruppe.SysHId = 20 AND SysGruppe.SysGr = 3 NO-LOCK:                                                */
/*             cString = "SYSHODE" + ";" + STRING(Syshode.SysHid)   + ";" + SysHode.Beskrivelse.                                           */
/*             PUT UNFORMATTED cString SKIP.                                                                                               */
/*             cString = "SYSGRUPPE" + ";" + STRING(SysGruppe.SysHId)   + ";" + STRING(SysGruppe.SysGr) + ";" + SysGruppe.Beskrivelse.     */
/*             PUT UNFORMATTED cString SKIP.                                                                                               */
/*             FOR EACH SysPara WHERE SysPara.SysHId = 20 AND SysPara.SysGr = 3 NO-LOCK:                                                   */
/*                 cString = "SYSPARA" + ";" + STRING(SysPara.SysHId) + ";" + STRING(SysPara.SysGr) + ";" + STRING(SysPara.ParaNr) + ";" + */
/*                                             SysPara.Beskrivelse + ";" + SysPara.Parameter1 + ";" + SysPara.Parameter2.                  */
/*                 PUT UNFORMATTED cString SKIP.                                                                                           */
/*             END.                                                                                                                        */
/*         END.                                                                                                                            */
/*     END.                                                                                                                                */
/* END.                                                                                                                                    */
FOR EACH Elogg WHERE ELogg.TabellNavn = "Selger" AND ELogg.EksterntSystem = "POS" NO-LOCK.
    FIND Selger WHERE Selger.SelgerNr = INT(ELogg.verdier) NO-LOCK NO-ERROR.
    IF AVAIL Selger AND Selger.selgernr < 99999999 AND (TRIM(Selger.NavnIKasse) <> "" OR TRIM(Selger.Navn) <> "") THEN DO:
        IF CAN-FIND(butiker WHERE butiker.butik = Selger.butikknr) THEN DO:
            cString = "SELGER" + ";" + STRING(selger.selgernr) + ";" +
                                       (IF TRIM(Selger.NavnIKasse) <> "" THEN Selger.NavnIKasse ELSE ENTRY(1,TRIM(Selger.Navn)," ")) + ";" +
                                      STRING(Selger.Butikknr).
            PUT UNFORMATTED cString SKIP.
        END.
    END.
END.
OUTPUT CLOSE.
FOR EACH Elogg WHERE ELogg.TabellNavn = "Selger" AND ELogg.EksterntSystem = "POS" NO-LOCK:
    FIND bufElogg WHERE ROWID(bufElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT.
    IF AVAIL bufElogg THEN
        DELETE bufElogg.
END.
