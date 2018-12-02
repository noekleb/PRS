&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER ipButik    LIKE Butiker.Butik    NO-UNDO.
DEFINE INPUT  PARAMETER ipb_id     LIKE BongHode.b_id    NO-UNDO.
DEFINE INPUT  PARAMETER ipProfilnr LIKE Butiker.ProfilNr NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND Butiker WHERE Butiker.butik = ipButik NO-LOCK NO-ERROR.
RUN AvviksLogg IN THIS-PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AvviksLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvviksLogg Procedure 
PROCEDURE AvviksLogg :
DEFINE VARIABLE iFeilKode AS INTEGER    NO-UNDO. /* 1=varekost,2=båda,3=Pris */
DEFINE VARIABLE dLinjeSum AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iPrisExt  AS INTEGER    NO-UNDO.
    TRANSRAD:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id     = ipb_id AND
        BongLinje.Makulert = FALSE AND
    /*     CAN-DO("001,003,010,012",STRING(BongLinje.TTId,"999")): */
        CAN-DO("001",STRING(BongLinje.TTId,"999")) AND Bonglinje.Antall > 0:
        FIND Strekkode WHERE StrekKode.Kode = TRIM(BongLinje.StrekKode) NO-LOCK NO-ERROR.
        IF AVAIL StrekKode THEN
            FIND ArtBas WHERE ArtBas.Artikkelnr = StrekKode.ArtikkelNr NO-LOCK NO-ERROR.
        ELSE
            FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(TRIM(BongLinje.ArtikkelNr)) NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtBas THEN
            NEXT.
        ASSIGN dLinjeSum = Bonglinje.Linjesum / Bonglinje.Antall.
        FIND ArtPris WHERE ArtPris.ProfilNr   = Butiker.ProfilNr AND
                           ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris AND ipProfilNr <> ? THEN 
            FIND ArtPris WHERE ArtPris.ProfilNr   = ipProfilNr AND
                               ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
        IF NOT AVAIL ArtPris THEN 
            FIND FIRST ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
        IF AVAIL ArtPris THEN DO:
            ASSIGN iPrisExt = IF ArtPris.Tilbud THEN 2 ELSE 1.
            IF ArtPris.Pris[iPrisExt] = 0 OR ArtPris.VareKost[iPrisExt] = 0 THEN
                NEXT.
        END.
        ELSE
            NEXT.
        IF (ArtPris.Pris[iPrisExt] <> dLinjeSum) OR
           (ArtPris.VareKost[iPrisExt] <> BongLinje.VVarekost) THEN DO:
            ASSIGN iFeilKode = IF ArtPris.Pris[iPrisExt] <> dLinjeSum THEN 3 ELSE 1
                   iFeilKode = IF iFeilKode = 1 THEN 1 ELSE IF ArtPris.VareKost[iPrisExt] <> BongLinje.VVarekost
                                     THEN 2 ELSE iFeilKode.

            CREATE priskontroll.
            ASSIGN priskontroll.ButikkNr      = BongLinje.ButikkNr
                   priskontroll.GruppeNr      = BongLinje.GruppeNr
                   priskontroll.KasseNr       = BongLinje.KasseNr 
                   priskontroll.Dato          = BongLinje.Dato    
                   priskontroll.Feilkode      = iFeilKode
                   priskontroll.b_id          = BongLinje.b_id
                   priskontroll.ArtikkelNr    = DECI(BongLinje.ArtikkelNr)
                   priskontroll.BongTekst     = BongLinje.BongTekst
                   priskontroll.SEVarekost    = ArtPris.VareKost[iPrisExt]
                   priskontroll.KasseVarekost = BongLinje.VVarekost
                   priskontroll.SEPris        = ArtPris.Pris[iPrisExt]
                   priskontroll.BongPris      = dLinjeSum
                   priskontroll.Antall        = BongLinje.Antall
                   priskontroll.StrekKode     = BongLinje.StrekKode NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                DELETE Priskontroll.
        END.
    END. /* TRANSRAD */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

