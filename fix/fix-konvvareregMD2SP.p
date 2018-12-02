/* 
Konvertering av vareregister fra MegaDisk varestruktur til 
StorePoint varestruktur.
*/

DEF VAR iLoop AS INT NO-UNDO.

                 
ARTBAS:
FOR EACH ArtBas EXCLUSIVE-LOCK:
    ASSIGN
        iLoop = iLoop + 1
        .
    PAUSE 0.
    DISPLAY 
        ArtBas.ArtikkelNr
        ArtBas.Beskr
        ArtBas.Vg FORMAT ">>>>>9"
        iLoop
        .


    STREKKODE:
    FOR EACH Strekkode OF ArtBas EXCLUSIVE-LOCK:
        /* Konverterer strekkoden */
        FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
             ImpKonv.Tabell     = "Strekkode" AND
             ImpKonv.EksterntID = TRIM(string(Strekkode.Kode)) NO-LOCK NO-ERROR.
        IF AVAILABLE ImpKonv THEN
        DO:
            /* Utføres bare hvis koden ikke finnes fra før. */
            ASSIGN
                Strekkode.Kode = ImpKonv.InterntId
                NO-ERROR.

        END.
    END. /* STREKKODE */

    /* Konverterer varegruppen og oppdaterer hovedgruppekobling. */
    /* Konverterer varegruppen TN 8/10-03 */
    FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND
         ImpKonv.Tabell     = "VarGr"    AND
         ImpKonv.EksterntID = TRIM(string(ArtBas.Vg)) NO-LOCK NO-ERROR.
    IF AVAILABLE ImpKonv THEN
    DO:
        FIND VarGr NO-LOCK WHERE
            VarGr.Vg = int(ImpKonv.InterntId) NO-ERROR.
        IF AVAILABLE VarGr THEN
            FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
        ASSIGN
            ArtBas.Vg         = int(ImpKonv.InterntId)
            ArtBas.LopNr      = ?
            ArtBas.Hg         = IF AVAILABLE VarGr
                                   THEN VarGr.Hg
                                   ELSE ArtBas.Hg
            .
    END.

/*     /* Konverterer leverandør */                                            */
/*     FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND                  */
/*          ImpKonv.Tabell     = "LevBas" AND                                  */
/*          ImpKonv.EksterntID = TRIM(string(ArtBas.LevNr)) NO-LOCK NO-ERROR.  */
/*     IF AVAILABLE ImpKonv THEN                                               */
/*         ASSIGN                                                              */
/*             ArtBas.LevNr  = int(ImpKonv.InterntId)                          */
/*             .                                                               */
/*                                                                             */
/*     /* Konverterer produsent */                                             */
/*     FIND ImpKonv WHERE ImpKonv.EDB-System = "MegaDisk" AND                  */
/*          ImpKonv.Tabell     = "Produsent" AND                               */
/*          ImpKonv.EksterntID = TRIM(string(ArtBas.ProdNr)) NO-LOCK NO-ERROR. */
/*     IF AVAILABLE ImpKonv THEN                                               */
/*         ASSIGN                                                              */
/*             ArtBas.ProdNr  = int(ImpKonv.InterntId)                         */
            .
END. /* ARTBAS */
