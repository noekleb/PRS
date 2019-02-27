/*------------------------------------------------------------------------------
  Purpose:     oppdVPIArtBas.p
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iHKVareId AS INT  NO-UNDO.
DEF VAR cTekst    AS CHAR NO-UNDO.
DEF VAR bHK       AS LOG  NO-UNDO.
DEF VAR iLopNr    AS INT  NO-UNDO.
DEF VAR bHKDirekteOppd    AS LOG NO-UNDO.
DEF VAR iUkjentEkstVPILev AS INT NO-UNDO.
DEF VAR ocReturn    AS CHAR NO-UNDO.
DEF VAR obOK        AS LOG NO-UNDO.
  
{tmptt_VPIArtPris.i &SHARED=SHARED}

DEF BUFFER tmpArtPris FOR ArtPris.

DEF VAR ihBuffer       AS HANDLE NO-UNDO.
DEF VAR bKorr          AS LOG    NO-UNDO.
DEF VAR bOppdatDirekte AS LOG    NO-UNDO.
DEF VAR bOppdatUtpris  AS LOG    NO-UNDO.

IF NOT CAN-FIND(FIRST TT_VPIArtPris) THEN
  RETURN.

/* Kobler ut logging til kassen. */
ON CREATE OF VPIArtPris OVERRIDE 
DO:  
END.
ON WRITE OF VPIArtPris OVERRIDE 
DO:  
END.
ON DELETE OF VPIArtPris OVERRIDE 
DO:  
END.

/* HK installasjon. */
{syspara.i 1 1 18 cTekst}
IF CAN-DO("yes,1,ja,true",cTekst) THEN
    bHk = TRUE.
  ELSE
    bHk = FALSE.

/* Behandler sletteposter */
SLETTEPOSTER:
FOR EACH TT_VPIArtPris WHERE
    TT_VPIArtPris.RecType = 3:
    FIND VPIArtPris EXCLUSIVE-LOCK WHERE
      VPIArtPris.EkstVPILevNr = TT_VPIArtPris.EkstVPILevNr AND
      VPIArtPris.VareNr       = TT_VPIArtPris.VareNr AND
      VPIArtPris.ProfilNr     = TT_VPIArtPris.ProfilNr NO-ERROR.
    IF AVAILABLE VPIArtPris THEN
    DO:
      DELETE VPIArtPris NO-ERROR.
      DELETE TT_VPIArtPris.
    END.
END. /* SLETTEPOSTER */

/* Behandler Ny/endre poster */
NY-ENDRE:
FOR EACH TT_VPIArtPris WHERE
    TT_VPIArtPris.RecType = 1:
    FIND VPIArtPris EXCLUSIVE-LOCK WHERE
      VPIArtPris.EkstVPILevNr = TT_VPIArtPris.EkstVPILevNr AND
      VPIArtPris.VareNr       = TT_VPIArtPris.VareNr AND
      VPIArtPris.ProfilNr     = TT_VPIArtPris.ProfilNr NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIArtPris THEN
      CREATE VPIArtPris.
    BUFFER-COPY TT_VPIArtPris TO VPIArtPris.

    /* Logger prisendringer i VPI Mottak */
    IF bHk = FALSE AND
      CAN-FIND(ArtBas NO-LOCK WHERE
                ArtBas.ArtikkelNr = VPIArtPris.ArtikkelNr) THEN 
    SJEKK_PRIS_ENDRING:
    DO:
        FIND tmpArtPris NO-LOCK WHERE
            tmpArtPris.ArtikkelNr = VPIArtPris.ArtikkelNr AND
            tmpArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
        IF AVAILABLE tmpArtPris THEN
        SJEKK:
        DO:
            IF (tmpArtPris.Pris[1]         <> vpiArtPris.Pris[1]         OR
                tmpartPris.Db%[1]          <> VPIArtPris.DB%[1]          OR
                tmpArtPris.Innkjopspris[1] <> vpiArtPris.Innkjopspris[1] OR
                tmpArtPris.Varekost[1]     <> vpiArtPris.Varekost[1]     OR
                tmpArtPris.Rab1%[1]        <> vpiArtPris.Rab1%[1]        OR
                tmpArtPris.Mva%[1]         <> vpiArtPris.Mva%[1]) THEN
            OPPRETT:
            DO:
                /* Sjekker om prispost skal oppdateres inn i ArtPris direkte. */
                /* Dette skal gjøres hvis innpris er endre. Utpris skal ikke  */
                /* kopieres inn i dette tilfelle.                             */
                IF (tmpArtPris.Innkjopspris[1] <> vpiArtPris.Innkjopspris[1] OR
                    tmpArtPris.Varekost[1]     <> vpiArtPris.Varekost[1]     OR
                    tmpArtPris.Rab1%[1]        <> vpiArtPris.Rab1%[1]) THEN
                    bOppdatDirekte = TRUE.

                /* Sjekker om prispost skal oppdateres inn i ArtPris direkte.  */
                /* Dette skal gjøres hvis innpris er endret. Utpris skal ikke  */
                /* kopieres inn i dette tilfelle.                              */
                IF (tmpArtPris.Pris[1] <> vpiArtPris.Pris[1]) THEN
                    bOppdatUtpris = TRUE.

                /* Henter artikkel om varegrupp (Moms) */
                FIND ArtBas NO-LOCK WHERE
                    ArtBas.ArtikkelNr = VPIArtPRis.ArtikkelNr NO-ERROR.
                FIND VarGr OF ArtBas NO-ERROR.

                /* Opphav - for bestemmelse av type. */
                FIND VPIArtBas NO-LOCK WHERE
                    VPIArtBas.EkstVPILevNr = VPIArtPris.EkstVPILevNr AND
                    VPIArtBas.VareNr       = VPIArtPris.VareNr NO-ERROR.
                IF AVAILABLE VPIArtBas THEN
                DO:
                    IF VPIArtBas.KorrArtikkelNr > 0.0 THEN
                        bKorr = TRUE.
                    ELSE
                        bKorr = FALSE.
                END.
                ELSE bKorr = FALSE.

            END. /* OPPRETT */
            ELSE bKorr = FALSE.

            /* Oppretter KORREKSJONpost VPI mottak - Logger prisendring fra hovedkontor. */
            /* Denne posten har både endring i inn og utpris.                            */
            IF bKorr THEN
            KORR_VPIMOTTAK:
            DO:
                /* Oppstandelsen */
                CREATE VPIMottak.
                ASSIGN
                    VPIMottak.BehStatus        = 1
                    VPIMottak.VPIType          = 2
                    VPIMottak.ArtikkelNr       = ArtBas.ArtikkelNr
                    VPIMottak.LevKod           = ArtBAs.LevKod
                    VPIMottak.Beskr            = ArtBAs.Beskr
                    VPIMottak.LevFargKod       = ArtBas.LevFargKod
                    VPIMottak.InnkjopsPris     = VPIArtPris.InnkjopsPris[1]
                    VPIMottak.Varekost         = VPIArtPris.Varekost[1]
                    VPIMottak.Rab1%            = VPIArtPris.Rab1%[1]
                    VPIMottak.Db%              = VPIArtPris.Db%[1]
                    VPIMottak.OrgDb%           = VPIArtPris.Db%[1]
                    VPIMottak.Mva%             = VPIArtPris.Mva%[1]
                    VPIMottak.AktiveresDato    = IF VPIArtPris.AktivFraDato < TODAY THEN TODAY ELSE VPIArtPris.AktivFraDato
                    VPIMottak.OrgAktiveresDato = VPIArtPris.AktivFraDato
                    VPIMottak.GyldigTilDato    = ?
                    VPIMottak.OrgGyldigTilDato = ?
                    VPIMottak.Pris             = VPIArtPris.Pris[1]
                    VPIMottak.OrgPris          = VPIArtPris.Pris[1]
                    VPIMottak.ProfilNr         = VPIArtPris.ProfilNr
                    VPIMottak.MomsKod          = IF AVAILABLE VarGr
                                                   THEN VarGr.MomsKod
                                                   ELSE 0
                    VPIMottak.Vg               = ArtBas.Vg
                    VPIMottak.Sasong           = ArtBas.Sasong
                    VPIMottak.LevNr            = ArtBas.LevNr
                    bKorr                      = FALSE
                    .
            END. /* KORR_VPIMOTTAK */

            /* Oppretter VPI mottak - Endring i INNPRIS/VAREKOST skal oppdateres direkte. */
            /* Systemparameter 50 16 1 styrer dette.                                      */
            /* Utpris skal ikke overskrives.                                              */
            IF bOppdatDirekte THEN
            VPIMOTTAK2:
            DO:
                FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = VPIArtPris.ArtikkelNr AND
                    ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
                IF AVAILABLE ArtPris THEN
                DO:
                    /* Oppstandelsen */
                    CREATE VPIMottak.
                    ASSIGN
                        VPIMottak.BehStatus        = 1
                        VPIMottak.VPIType          = 1
                        VPIMottak.ArtikkelNr       = ArtBas.ArtikkelNr
                        VPIMottak.LevKod           = ArtBAs.LevKod
                        VPIMottak.Beskr            = ArtBAs.Beskr
                        VPIMottak.LevFargKod       = ArtBas.LevFargKod
                        VPIMottak.InnkjopsPris     = VPIArtPris.InnkjopsPris[1]
                        VPIMottak.Varekost         = VPIArtPris.Varekost[1]
                        VPIMottak.Rab1%            = VPIArtPris.Rab1%[1]
                        VPIMottak.Mva%             = VPIArtPris.Mva%[1]
                        
                        /* Regnes om i forhold til gammel utpris som fortsatt gjelder. */
                        VPIMottak.Db%              = VPIArtPris.Db%[1]
                        VPIMottak.OrgDb%           = VPIMottak.Db%
                        
                        /* Endringer i innpris skal aktiveres direkte. */
                        VPIMottak.AktiveresDato    = TODAY 
                        VPIMottak.OrgAktiveresDato = TODAY 
                        VPIMottak.GyldigTilDato    = ?
                        VPIMottak.OrgGyldigTilDato = ?

                        /* Den gamle utprisen skal gjelde. */
                        VPIMottak.Pris             = ArtPris.Pris[1]
                        VPIMottak.OrgPris          = ArtPris.Pris[1]
                        VPIMottak.ProfilNr         = ArtPris.ProfilNr

                        VPIMottak.MomsKod          = IF AVAILABLE VarGr
                                                       THEN VarGr.MomsKod
                                                       ELSE 0
                        VPIMottak.Vg               = ArtBas.Vg
                        VPIMottak.Sasong           = ArtBas.Sasong
                        VPIMottak.LevNr            = ArtBas.LevNr
                        .

                    /* Oppdaterer VPIMottak posten */
                    IF bHKDirekteOppd THEN DO:
                        CREATE BUFFER ihBuffer FOR TABLE "VPIMottak".
                        ihBuffer = BUFFER VPIMottak:HANDLE.
                        RUN vpimottak_pris.p ('',ihBuffer,'',OUTPUT ocReturn,OUTPUT obOk).
                        IF obOk THEN
                            ASSIGN 
                              VPImottak.behStatus = 90. /*Behandlet*/
                    END.
                END.
                ASSIGN
                    bOppdatDirekte = FALSE
                    .
            END. /* VPIMOTTAK2 */
            ELSE bOppdatDirekte = FALSE.

            /* Oppretter VPI mottak - Endring i utpris skal logges for oppdatering.             */
            /* I denne posten logges både inn og utprisendring.                                 */
            /* Loggposten blir ikke oppdatert, men må oppdateres manuelt av ansvarlig i butikk. */
            /* Utpris skal ikke overskrives.                                                    */
            IF bOppdatUtpris THEN
            VPIMOTTAK3:
            DO:
                /* Oppstandelsen */
                CREATE VPIMottak.
                ASSIGN
                    VPIMottak.BehStatus        = 1
                    VPIMottak.VPIType          = 1
                    VPIMottak.ArtikkelNr       = ArtBas.ArtikkelNr
                    VPIMottak.LevKod           = ArtBAs.LevKod
                    VPIMottak.Beskr            = ArtBAs.Beskr
                    VPIMottak.LevFargKod       = ArtBas.LevFargKod
                    VPIMottak.InnkjopsPris     = VPIArtPris.InnkjopsPris[1]
                    VPIMottak.Varekost         = VPIArtPris.Varekost[1]
                    VPIMottak.Rab1%            = VPIArtPris.Rab1%[1]
                    VPIMottak.Mva%             = VPIArtPris.Mva%[1]

                    /* Regnes om i forhold til gammel utpris som fortsatt gjelder. */
                    VPIMottak.Db%              = VPIArtPris.Db%[1]
                    VPIMottak.OrgDb%           = VPIMottak.Db%

                    /* Endringer i innpris skal aktiveres direkte. */
                    VPIMottak.AktiveresDato    = IF VPIArtPris.AktivFraDato < TODAY THEN TODAY ELSE VPIArtPris.AktivFraDato
                    VPIMottak.OrgAktiveresDato = VPIArtPris.AktivFraDato
                    VPIMottak.GyldigTilDato    = ?
                    VPIMottak.OrgGyldigTilDato = ?

                    /* Den gamle utprisen skal gjelde. */
                    VPIMottak.Pris             = VPIArtPris.Pris[1]
                    VPIMottak.OrgPris          = VPIArtPris.Pris[1]
                    VPIMottak.ProfilNr         = VPIArtPris.ProfilNr

                    VPIMottak.MomsKod          = IF AVAILABLE VarGr
                                                   THEN VarGr.MomsKod
                                                   ELSE 0
                    VPIMottak.Vg               = ArtBas.Vg
                    VPIMottak.Sasong           = ArtBas.Sasong
                    VPIMottak.LevNr            = ArtBas.LevNr
                    bOppdatUtpris = FALSE
                    .
            END. /* VPIMOTTAK3 */
        END. /* SJEKK */
    END. /* SJEKK_PRIS_ENDRING */

    RELEASE VPIArtPris.
    DELETE TT_VPIArtPris.
END. /* NY-ENDRE */

