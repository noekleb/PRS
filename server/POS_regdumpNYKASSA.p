
RUN x-klargjorprisko.w (?).

OUTPUT TO ".\kom\ut\posdumpNYKASSA.txt".

DEFINE VARIABLE cString        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTilbudFraDato AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTilbudTilDato AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTilbudFraTid  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTilbudTilTid  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cVVAreKost     AS CHARACTER NO-UNDO.

DEFINE BUFFER bufElogg FOR Elogg.

FOR EACH moms NO-LOCK.
/*     cString = "MOMS" + ";" + STRING(Moms.Momskod) + ";" +                     */
/*                     REPLACE(REPLACE(Moms.Beskrivelse,";",""),'"'," ") + ";" + */
/*                              STRING(Moms.momsproc) NO-ERROR.                  */
    cString = "MOMS" + ";" +
               STRING(1) + ";" +
               STRING(Moms.MomsKod) + ";" + 
               STRING(Moms.MomsProc) + ";" + 
               Moms.Beskrivelse
               NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    PUT UNFORMATTED cString SKIP.
END.
FOR EACH VARGR NO-LOCK.
/*     cString = "VARGR" + ";" + STRING(VarGr.Vg) + ";" +                      */
/*                      REPLACE(REPLACE(VarGr.vgbeskr,";",""),'"'," ") + ";" + */
/*                               STRING(VarGr.Momskod) + ";" +                 */
/*                               STRING(Vargr.kost_proc) NO-ERROR.             */
    cString = "VARGR;" + 
              '1' + ";" +
              STRING(VarGr.Vg) + ";" + 
              REPLACE(REPLACE(VarGr.vgbeskr,";",""),'"'," ") + ";" + 
              STRING(VarGr.Momskod) + ";" + 
              STRING(Vargr.kost_proc)  + ";;;;;;;;;"
              NO-ERROR.

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
/*     cString = "ARTBAS" + ";" + STRING(ArtBas.Artikkelnr) + ";" +                             */
/*                                STRING(ArtBas.Vg)         + ";" +                             */
/*                                STRING(ArtBas.Lopnr)      + ";" +                             */
/*                       REPLACE(REPLACE(ArtBas.Bongtekst,";",""),'"'," ") + ";" +              */
/*                                STRING(ArtBas.Opris,"J/N") + ";" +                            */
/*                   IF AVAIL Bilderegister THEN "mini" + bilderegister.filnavn ELSE "" + ";" + */
/*                                STRING(Artbas.vekt,"J/N") + ";" +                             */
/*                                STRING(ArtBas.Non_Sale,"J/N") + ";" +                         */
/*                                STRING(ArtBas.Negvare,"J/N")  + ";" +                         */
/*                                STRING(ArtBas.ManRabIKas,"J/N")                               */
/*                       NO-ERROR.                                                              */

/* 01 */  cString = "ARTBAS" + ";" +
/* 02 */             "1" + ";" +
/* 03 */             STRING(ArtBas.Artikkelnr) + ";" + 
/* 04 */             STRING(ArtBas.Vg)         + ";" + 
/* 05 */             STRING(ArtBas.Lopnr)      + ";" + 
/* 06 */    REPLACE(REPLACE(ArtBas.Bongtekst,";",""),'"'," ") + ";" + 
/* 07 */             STRING(ArtBas.Opris,"J/N") + ";" +
/* 08 */    (IF AVAIL Bilderegister THEN "mini" + bilderegister.filnavn ELSE "") + ";" +
/* 09 */             STRING(Artbas.vekt,"J/N") + ";" +
/* 10 */             STRING(ArtBas.Non_Sale,"J/N") + ";" +
/* 11 */             STRING(ArtBas.Negvare,"J/N")  + ";" +
/* 12 */             STRING(ArtBas.ManRabIKas,"J/N") + ";" +
/* 13 */             STRING(ArtBas.ArtSlag) + ";" +
/* 14 */        STRING(ArtBas.Hg) + ";" +
/* 15 */        STRING(ArtBas.Farg) + ";" +
/* 16 */        REPLACE(REPLACE(ArtBas.Beskr,";",""),'"'," ") + ";" +
/* 17 */        STRING(ArtBas.LevNr) + ";" +
/* 18 */        REPLACE(REPLACE(ArtBas.LevKod,";",""),'"'," ") + ";" +
/* 19 */        STRING(ArtBas.RabKod) + ";" +
/* 20 */        REPLACE(REPLACE(ArtBas.LevFargKod,";",""),'"'," ") + ";" +
/* 21 */        STRING(ArtBas.BildeIKasse,"J/N") + ";" +
/* 22 */        STRING(ArtBas.Pakke,"J/N") + ";" +
/* 23 */        STRING(ArtBas.Alder) + ";" +
/* 24 */        STRING(ArtBas.KundeRabatt,"J/N") + ";" +
/* 25 */        REPLACE(REPLACE(ArtBas.SalgsEnhet,";",""),'"'," ") + ";" +
/* 26 */        STRING(ArtBas.LinkVareNr) + ";" +
/* 27 */        STRING(ArtBas.Mengde) + ";" +
/* 28 */        STRING(ArtBas.IndividType) + ";" +
/* 29 */        STRING(ArtBas.Pant,"J/N") + ";" +
/* 30 */        STRING(ArtBas.GarantiKl) + ";" +
/* 31 */        STRING(ArtBas.AntIPakn) + ";" +
/* 32 */        REPLACE(REPLACE(REPLACE(ArtBas.VareFakta,";",""),'"'," "),CHR(13),CHR(1)) + ";" +
/* 33 */        STRING(ArtBas.Depositum) + ";" +
/* 34 */        STRING(ArtBas.HoyLavMva,"J/N") + ";" +
/* 35 */        STRING(ArtBas.VareType) + ";" +
/* 36 */        STRING(ArtBas.Telefonkort,"J/N") + ";"  +
/* 37 */        STRING(ArtBas.MengdeRabatt,"J/N") + ";"  +
/* 38 */        STRING(ArtBas.Kjokkenskriver)
                NO-ERROR.

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

/*     cString = "ARTPRIS" + ";" + STRING(ArtPris.Artikkelnr)   + ";" + */
/*                                 STRING(Artpris.profilnr)     + ";" + */
/*                                 STRING(Artpris.tilbud,"J/N") + ";" + */
/*                                        cTilbudFraDato        + ";" + */
/*                                 STRING(iTilbudFraTid)        + ";" + */
/*                                        cTilbudTilDato        + ";" + */
/*                                 STRING(iTilbudTilTid)        + ";" + */
/*                                 STRING(Artpris.pris[1])      + ";" + */
/*                                 STRING(Artpris.pris[2])      + ";" + */
/*                                 STRING(Artpris.VareKost[1])  + ";" + */
/*                                 STRING(Artpris.VareKost[2]).         */

    cString = "ARTPRIS;1" + ";" + STRING(ArtPris.Artikkelnr)  + ";" + 
                                STRING(Artpris.profilnr)      + ";" + 
                                STRING(Artpris.tilbud,"J/N")  + ";" + 
                                       cTilbudFraDato         + ";" + 
                                STRING(iTilbudFraTid)         + ";" + 
                                       cTilbudTilDato         + ";" + 
                                STRING(iTilbudTilTid)         + ";" + 
                                STRING(Artpris.pris[1])       + ";" + 
                                STRING(Artpris.pris[2])       + ";" + 
                                STRING(Artpris.VareKost[1])   + ";" + 
                                STRING(Artpris.VareKost[2])   + ";" +
                                STRING(Artpris.VareKost[1])   + ";" +  /* cVVAreKost */
                                STRING(Artpris.MengdeRabAnt)  + ";" +
                                STRING(Artpris.MengdeRabPris) + ";" +
                                STRING(Artpris.Mva%[1])       + ";" +
                                STRING(Artpris.MvaKr[1])      + ";" +
                                STRING(Artpris.MomsKod[1])    + ";" +
                                STRING(Artpris.DbKr[1])       + ";" +
                                STRING(Artpris.Db%[1])        + ";" +
                                STRING(Artpris.Mva%[2])       + ";" +
                                STRING(Artpris.MvaKr[2])      + ";" +
                                STRING(Artpris.MomsKod[2])    + ";" +
                                STRING(Artpris.DbKr[2])       + ";" +
                                STRING(Artpris.Db%[2])        
                                .


    PUT UNFORMATTED cString SKIP.
    FOR EACH StrekKode OF ArtBas NO-LOCK:
        FIND strkonv OF strekkode NO-LOCK NO-ERROR.
        cString = "STREKKODE" + ";" + STRING(StrekKode.Artikkelnr)   + ";" + 
                                      StrekKode.Bestillingsnummer    + ";" + 
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
FOR EACH selger NO-LOCK.
    IF Selger.selgernr < 99999999 AND (TRIM(Selger.NavnIKasse) <> "" OR TRIM(Selger.Navn) <> "") THEN DO:
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
