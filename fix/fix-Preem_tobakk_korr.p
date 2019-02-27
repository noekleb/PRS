/* Korreksjon av tobakk */
CURRENT-WINDOW:WIDTH = 300.

DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cLinje      AS CHAR FORMAT "x(80)"          NO-UNDO.
DEF VAR lArtikkelNr AS DEC  FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEF VAR lPris       AS DEC  FORMAT "->>>>>>9.99"    NO-UNDO.
DEF VAR lVarekost   AS DEC  FORMAT "->>>>>>9.99"    NO-UNDO.
DEF VAR iLevNr      AS INT  FORMAT ">>>>>9"         NO-UNDO.
DEF VAR cLevKod     AS CHAR FORMAT "x(20)"          NO-UNDO.

DEFINE VARIABLE iBatchNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeqNr   AS INTEGER NO-UNDO.

DEFINE BUFFER bArtBas    FOR ArtBas.
DEFINE BUFFER bArtPris   FOR ArtPris.    
DEFINE BUFFER bStrekkode FOR STrekkode.
DEFINE BUFFER bTransLogg FOR TransLogg.

DEF STREAM Inn.
DEF STREAM Ut.

ASSIGN
    /*cFilNavn = 'C:\Appdir\PreemKorr\Underlag till GJ all tobak säker osäker alla säker glass 110907 (2).csv'*/
    cFilNavn = 'C:\Polygon\test.txt'
    .
INPUT STREAM Inn FROM VALUE(cFilNavn).
LESFIL:
REPEAT:
    ASSIGN
        lArtikkelNr = 0
        lPris       = 0
        lVareKost   = 0
        iLevNr      = 0
        cLevKod     = ''
        .
   IMPORT STREAM Inn UNFORMATTED cLinje.
   ASSIGN
       lArtikkelNr = DEC(ENTRY(1,clinje,';')) 
       lPris       = DEC(ENTRY(3,clinje,';'))
       lVareKost   = DEC(ENTRY(4,clinje,';'))
       iLevNr      = INT(ENTRY(5,clinje,';'))
       cLevKod     = TRIM(ENTRY(7,clinje,';'),'')
       NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
       NEXT LESFIL.

   /* Oppretter batch som skal balangsere PLU'er og ikke lagerstyrte varer */
   IF iBatchNr = 0 THEN 
      RUN batchlogg.p (PROGRAM-NAME(1),
               "Varekost korr tobakk " +
               string(TODAY) +
               " " +
               string(TIME,"HH:MM") +
               " " +
               USERID("dictdb"),
               OUTPUT iBatchNr).

   FIND ArtBas NO-LOCK WHERE
       ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
   IF AVAILABLE ArtBas THEN
       FIND ArtPris NO-LOCK WHERE
         ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
         ArtPris.ProfilNr   = 1 NO-ERROR.
   FIND FIRST Strekkode NO-LOCK WHERE
       Strekkode.Kode = STRING(lArtikkelNr) NO-ERROR.
   IF NOT AVAILABLE STrekkode THEN
       FIND FIRST Strekkode NO-LOCK WHERE
         Strekkode.Kode = FILL('0',13 - LENGTH(STRING(lArtikkelNr))) + STRING(lArtikkelNr) NO-ERROR.
   /* Korriger Artikkelinformasjon. 
      - Bytte leverandørnr på artikkel.
      - Bytte levkod på artikkel.
      - Bytte bestillingsnummer på strekkoden
   */
   IF AVAILABLE ArtBas  THEN 
   DO TRANSACTION:
       FIND bArtBas EXCLUSIVE-LOCK WHERE
           RECID(bArtBas) = RECID(ArtBas).
       IF (bArtBas.LevNr <> iLevNr AND iLevNr > 0) THEN 
           bArtBas.LevNr = iLevNr.

       IF (cLevKod <> '0' AND cLevKod <> '') THEN
       DO:
           bArtBas.LevKod = cLevKod.
           FIND bStrekkode EXCLUSIVE-LOCK WHERE 
               RECID(bStrekkode) = RECID(Strekkode).
           IF AVAILABLE bStrekkode THEN DO:
               bStrekkode.Bestillingsnummer = cLevKod.
               RELEASE bStrekkode.
           END.

       END.
       IF AVAILABLE bArtBas THEN
           RELEASE bArtBas.
   END. /* TRANSACTION */

   /* Korriger kalkyle (Nb: Pris er angitt inkl. mva) 
      - Rette innpris og utpris på hk profilen.
      - Rette innpris og utpris på profilene
   */
   IF AVAILABLE ArtBas AND AVAILABLE ArtPris THEN
   KALKYLEFIX:
   DO:
       IF (ArtPris.Varekost[1] <> lVarekost OR ArtPris.Pris[1] <> lPris) THEN 
       DO:
           FOR EACH bArtPris EXCLUSIVE-LOCK WHERE
               bArtPris.ArtikkelNr = ArtBas.ArtikkelNr:
               ASSIGN
                 bArtPris.ValPris[1]      = lVareKost
                 bArtPris.InnkjopsPris[1] = lVareKost
                 bArtPris.Pris[1]         = lPris
                 bArtPris.Rab1Kr[1]       = 0
                 bArtPris.Rab1%[1]        = 0
                 bArtPris.Rab2Kr[1]       = 0
                 bArtPris.Rab2%[1]        = 0
                 bArtPris.Rab3Kr[1]       = 0
                 bArtPris.Rab3%[1]        = 0
                 bArtPris.VareKost[1]     = lVareKost
                 bArtPris.MvaKr[1]        =  bArtPris.Pris[1] - (bArtPris.Pris[1] / (1 + bArtPris.Mva%[1] / 100))
                 bArtPris.DbKr[1]         = (bArtPris.Pris[1] - bArtPris.MvaKr[1] - bArtPris.VareKost[1])
                 bArtPris.Db%[1]          = (((bArtPris.Pris[1] - bArtPris.MvaKr[1]) - bArtPris.VareKost[1]) * 100) / (bArtPris.Pris[1] - bArtPris.MvaKr[1])
                 .
           END.
       END.
       ELSE LEAVE KALKYLEFIX.   
   END. /* KALKYLEFIX */

   /* Korriger translogg og Bonglinje for innpris
      (Korr gjøres med motpostering og postering. Samle alle korr. i en batch med status 0. )
      - Rette Translogg.VVarekost hvs denne avviker fra innpris
      - Sette inn BongLinje.VVareKost på tilhørende bonglinje.
   */
   TRANSLOGG:
   FOR EACH TransLogg NO-LOCK WHERE
     TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
     
     IF (CAN-DO('1,3,10',STRING(TransLogg.TTId)) AND 
        TransLogg.VVareKost <> lVareKost) THEN 
     KORR_TRANSLOGG:
     DO:
        /* Motposterer */
        LOOPEN1:
        DO iSeqNr = 2 TO 99:
            FIND FIRST bTransLogg NO-LOCK WHERE
                       bTransLogg.Butik   = TransLogg.Butik  AND
                       bTransLogg.TransNr = TransLogg.TransNr       AND
                       bTransLogg.SeqNr   = iSeqNr               
                       NO-ERROR.
            IF AVAILABLE bTransLogg THEN 
              NEXT LOOPEN1.
            ELSE DO TRANSACTION:
                CREATE bTransLogg.
                BUFFER-COPY TransLogg
                    EXCEPT SeqNr BatchNr
                    TO bTranslogg
                    ASSIGN
                        bTransLogg.BatchNr     = iBatchNr
                        bTransLogg.Antall      = bTransLogg.Antall * -1
                        bTransLogg.Postert     = FALSE
                        bTransLogg.PostertDato =?
                        bTransLogg.PostertTid  = 0
                        .
                RELEASE bTransLogg.
                LEAVE LOOPEN1.
            END.        
        END. /* LOOPEN1 */
        
        /* Posterer korrigert transaksjon */
        LOOPEN2:
        DO iSeqNr = 2 TO 99:
            FIND FIRST bTransLogg NO-LOCK WHERE
                       bTransLogg.Butik   = TransLogg.Butik  AND
                       bTransLogg.TransNr = TransLogg.TransNr       AND
                       bTransLogg.SeqNr   = iSeqNr               
                       NO-ERROR.
            IF AVAILABLE bTransLogg THEN 
              NEXT LOOPEN2.
            ELSE DO TRANSACTION:
                CREATE bTransLogg.
                BUFFER-COPY TransLogg
                    EXCEPT SeqNr BatchNr
                    TO bTranslogg
                    ASSIGN
                        bTransLogg.BatchNr     = iBatchNr
                        bTransLogg.Postert     = FALSE
                        bTransLogg.PostertDato = ? 
                        bTransLogg.PostertTid  = 0
                        bTransLogg.VVareKost   = lVareKost
                        .
                RELEASE bTransLogg.
                LEAVE LOOPEN2.
            END.        
        END. /* LOOPEN2 */
     END. /* KORR_TRANSLOGG */
     
     KORR_BONGLINJE:
     DO TRANSACTION:
         FIND BongLinje EXCLUSIVE-LOCK WHERE
             BongLinje.ButikkNr = TransLogg.Butik AND 
             BongLinje.GruppeNr = 1 AND 
             BongLinje.KasseNr  = TransLogg.KassaNr AND 
             BongLinje.Dato     = TransLogg.Dato AND 
             BongLinje.BongNr   = TransLogg.BongId AND
             BongLinje.LinjeNr  = TransLogg.BongLinjeNr NO-ERROR.
         IF AVAILABLE BongLinje THEN DO: 
             BongLinje.VVareKost = lVareKost.
             RELEASE BongLinje.
         END.
     END. /* KORR_BONGLINJE */
   END. /* TRANSLOGG */

   DISPLAY
       lArtikkelNr
       ArtBas.Beskr WHEN AVAILABLE ArtBas
       lPris
       lVareKost
       iLevNr
       cLevKod
       '|'
       Artpris.Pris[1] WHEN AVAILABLE ArtPris
       '*' WHEN Artpris.Pris[1] <> lPris
       ArtPris.VareKost[1] WHEN AVAILABLE ArtPris
       '*' WHEN ArtPris.VareKost[1] <> lVarekost
       ArtBas.LevNr WHEN AVAILABLE ArtBas
       '*' WHEN ArtBas.LevNr <> iLevNr
       ArtBas.LevKod WHEN AVAILABLE ArtBas
       '*' WHEN ArtBas.LevKod <> cLevKod
       Strekkode.Bestillingsnummer WHEN AVAILABLE Strekkode
       AVAILABLE Strekkode
   WITH WIDTH 300.

END. /* LESFIL */
INPUT STREAM Inn CLOSE.
