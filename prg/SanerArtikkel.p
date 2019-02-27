/* SanerArtikkel.p 
   run SanerArtikkel.p (<Artikkel som skal motta>,<Artikkel som skal saneres>).
   
*/

DEFINE INPUT  PARAMETER cArtNrSaneres AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER cArtikkelNr   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER bOk           AS LOG       NO-UNDO.

DEFINE VARIABLE iCl          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLevNamn     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBeskrivelse AS CHARACTER NO-UNDO.
DEFINE VARIABLE lArtikkelNr  AS DECIMAL   NO-UNDO.

DEFINE BUFFER sanArtBas FOR ArtBas.

{syspara.i 5 1 1 iCL INT}

FIND sanArtBas NO-LOCK WHERE
  sanArtBas.ArtikkelNr = DECIMAL(cArtNrSaneres) NO-ERROR.
FIND ArtBas NO-LOCK WHERE
  ArtBas.ArtikkelNr = DECIMAL(cArtikkelNr) NO-ERROR.
IF AVAILABLE sanArtBas THEN FIND FIRST ArtPris OF sanArtBas NO-LOCK NO-ERROR.
IF NOT AVAILABLE ArtBas OR NOT AVAILABLE sanArtBas OR NOT AVAILABLE ArtPris THEN 
    DO:
      bOk = FALSE.
      RETURN.
    END.  
FIND LevBas OF sanArtBas NO-LOCK NO-ERROR.
IF AVAILABLE LevBas THEN cLevNamn = LevBas.levnamn.
FIND StrType OF sanArtBas NO-LOCK NO-ERROR.
IF AVAILABLE StrType THEN cBeskrivelse = StrType.Beskrivelse.
ASSIGN
    lArtikkelNr = ArtBas.ArtikkelNr.
    
RUN d-bekreftSanera.w (sanArtbas.artikkelnr,
                       sanArtbas.Beskr,
                       sanArtbas.VG,
                       sanArtbas.LevNr,
                       cLevNamn,
                       sanArtbas.Lager,
                       sanArtBas.StrTypeID,
                       cBeskrivelse,
                       ArtPris.Pris[1],
                       ArtPris.Pris[2],
                       ArtPris.TilbudFraDato,
                       ArtPris.TilbudTilDato,
                       iCl,
                       INPUT-OUTPUT lArtikkelNr,OUTPUT bOk).
IF lArtikkelNr > 0 AND bOk THEN 
DO:
    MESSAGE "Skal artikkelen saneres til ny artikkel ?" UPDATE
        lBekreft AS LOGICAL
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO.
    IF lBekreft THEN 
    DO:
        RUN vpikorrutfor.p (lArtikkelNr,sanArtbas.artikkelnr).
        /*
        DO TRANSACTION:
            IF NOT CAN-FIND(ELogg WHERE 
                            ELogg.EksterntSystem = "KORRHK" AND 
                            ELogg.TabellNavn     = "VPIArtBas" AND 
                            ELogg.Verdier        = "0|" + STRING(lArtikkelNr) + "|"
                                                 + STRING(sanArtbas.artikkelnr)) THEN 
            DO:
                CREATE Elogg.
                ASSIGN ELogg.EksterntSystem  = "KORRHK"
                       ELogg.TabellNavn      = "VPIArtBas"
                       ELogg.Verdier         = "0|" + STRING(lArtikkelNr) + "|"
                                                    + STRING(sanArtbas.artikkelnr)
                       ELogg.EndringsType    = 1 
                       ELogg.Behandlet       = FALSE.
                IF AVAILABLE Elogg THEN RELEASE Elogg.
            END.
            FIND CURRENT sanArtBas EXCLUSIVE-LOCK.
            ASSIGN sanArtbas.IKasse     = FALSE
                   sanArtBas.Sanertdato = TODAY
                   sanArtBas.Beskr      = 'KORR: ' + sanArtBas.Beskr.
            FIND CURRENT sanArtBas NO-LOCK.
        END. /* TRANSACTION */
        RUN vpikorreksjon.w.
        */
        bOk = TRUE.
    END.
END.
