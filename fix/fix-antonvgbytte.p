CURRENT-WINDOW:WIDTH = 200.

DEF VAR cFil      AS CHAR                NO-UNDO.
DEF VAR cFilLinje AS CHAR FORMAT "x(80)" NO-UNDO.
DEF VAR cKode     AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR iVg       AS INT  FORMAT ">>>>9" NO-UNDO.
DEF VAR iAnt      AS INT                 NO-UNDO.

ASSIGN
    cFil = "AntonVgBytte.csv"
    .

IF NOT SEARCH(cFil) <> ? THEN
DO:
    MESSAGE "Ukjent fil" cFil
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

DEF STREAM Inn.
DEF STREAM Ut.

INPUT STREAM Inn FROM VALUE(cFil) NO-ECHO.
OUTPUT STREAM Ut TO VALUE("AntonByttVg.Log") NO-ECHO.

MAIN-LOOP:
REPEAT:
    IMPORT STREAM Inn UNFORMATTED
        cFilLinje
        .

    ASSIGN
        cKode = trim(ENTRY(4,cFilLinje,";"))
        iVg   = int(ENTRY(23,cFilLinje,";"))
        iAnt  = iAnt + 1
        .

    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = cKode NO-ERROR.
    IF AVAILABLE Strekkode THEN
        FIND ArtBas OF Strekkode NO-ERROR.

    PAUSE 0 BEFORE-HIDE.
    DISPLAY
        iAnt
        (IF NUM-ENTRIES(cFilLinje,";") <> 35 THEN
            "*ERRFORMAT*"
            ELSE "") FORMAT "x(11)" COLUMN-LABEL "FormatErr"
        (IF AVAILABLE ArtBas
                THEN string(ArtBas.ArtikkelNr)
                ELSE "") COLUMN-LABEL "ArtikkelNr" FORMAT "x(13)"
        cKode
        iVg  COLUMN-LABEL "ImpVg"
        (IF AVAILABLE ArtBas
                THEN string(ArtBas.Vg)
                ELSE "") COLUMN-LABEL "DbVg"
        (IF AVAILABLE ArtBas AND
                    ArtBas.Vg <> iVg THEN
            "BYTTET"
            ELSE "")
        cFilLinje
        WITH WIDTH 198.

    IF AVAILABLE ArtBas AND
        ArtBas.Vg <> iVg THEN
    BYTT-VG:
    DO:
       IF NOT CAN-FIND(VarGr WHERE
                       VarGr.Vg = iVg) THEN
           LEAVE BYTT-VG.

       PUT STREAM Ut UNFORMATTED
           cKode ";"
           ArtBas.ArtikkelNr ";"
           ArtBas.Vg FORMAT ">>>>>9" ";"
           ArtBas.LopNr FORMAT ">>>>9" ";"
           iVg FORMAT ">>>>>9" SKIP
           .

       RUN ByttVg.
    END. /* BYTT-VG */
END. /* MAIN-LOOP */

OUTPUT STREAM Ut CLOSE.
INPUT STREAM Inn CLOSE.

RETURN.

PROCEDURE ByttVg:
    RETURN.

/*     DEF VAR piLopNr AS INT NO-UNDO.                                   */
/*                                                                       */
/*     DEF BUFFER bufArtBas FOR ArtBas.                                  */
/*                                                                       */
/*     ASSIGN                                                            */
/*         piLopNr = ArtBas.LopNr                                        */
/*         .                                                             */
/*                                                                       */
/*     /* Sjekker om artikkelen må tildeles nytt løpenummer */           */
/*     IF CAN-FIND(bufArtBas WHERE                                       */
/*                 bufArtBas.Vg    = iVg AND                             */
/*                 bufArtBas.LopNr = ArtBas.LopNr) THEN                  */
/*     DO:                                                               */
/*         FIND LAST bufArtBas NO-LOCK WHERE                             */
/*             bufArtBas.Vg = iVg USE-INDEX ArtIn.                       */
/*         ASSIGN                                                        */
/*             piLopNr = bufArtBas.LopNr + 1                             */
/*             .                                                         */
/*                                                                       */
/*         IF piLopNr > 9999 THEN                                        */
/*         DO:                                                           */
/*             MESSAGE "Kan ikke bytte varegruppe. Nummerserie er full." */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                    */
/*             RETURN.                                                   */
/*         END.                                                          */
/*     END.                                                              */
/*                                                                       */
/*     FIND VarGr NO-LOCK WHERE                                          */
/*         VarGr.Vg = iVg NO-ERROR.                                      */
/*     IF NOT AVAILABLE VarGr THEN                                       */
/*     DO:                                                               */
/*         MESSAGE "Ukjent varegruppe " iVg                              */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*         RETURN.                                                       */
/*     END.                                                              */
/*                                                                       */
/*     DO TRANSACTION:                                                   */
/*       FIND bufArtBas EXCLUSIVE-LOCK WHERE                             */
/*           bufArtBas.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.          */
/*       IF NOT AVAILABLE bufArtBas THEN                                 */
/*       DO:                                                             */
/*         MESSAGE "Artikkelen oppdateres fra en annen terminal."        */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                        */
/*         RETURN NO-APPLY "AVBRYT".                                     */
/*       END.                                                            */
/*       ASSIGN                                                          */
/*           bufArtBas.Vg        = iVg                                   */
/*           bufArtBas.LopNr     = piLopNr                               */
/*           bufArtBas.Hg        = VarGr.Hg                              */
/*           .                                                           */
/*                                                                       */
/*       /* Translogg */                                                 */
/*       FOR EACH TransLogg EXCLUSIVE-LOCK WHERE                         */
/*           TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:                   */
/*           ASSIGN                                                      */
/*               TransLogg.Vg    = iVg                                   */
/*               TransLogg.LopNr = piLopNr                               */
/*               .                                                       */
/*       END.                                                            */
/*                                                                       */
/*       /* KundeTrans */                                                */
/*       FOR EACH Kunde NO-LOCK,                                         */
/*           EACH KundeTrans OF Kunde EXCLUSIVE-LOCK WHERE               */
/*           KundeTrans.ArtikkelNr = ArtBas.ArtikkelNr:                  */
/*           ASSIGN                                                      */
/*               KundeTrans.Vg    = iVg                                  */
/*               KundeTrans.LopNr = piLopNr                              */
/*               .                                                       */
/*       END.                                                            */
/*                                                                       */
/*       /* Medlemstrans */                                              */
/*       FOR EACH Medlem NO-LOCK,                                        */
/*           EACH MedTrans OF Medlem EXCLUSIVE-LOCK WHERE                */
/*           MedTrans.ArtikkelNr = ArtBas.ArtikkelNr:                    */
/*           ASSIGN                                                      */
/*               MedTrans.Vg    = iVg                                    */
/*               MedTrans.LopNr = piLopNr                                */
/*               .                                                       */
/*       END.                                                            */
/*                                                                       */
/*       /* Artlag */                                                    */
/*       FOR EACH ArtLAg EXCLUSIVE-LOCK WHERE                            */
/*           ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:                      */
/*           ASSIGN                                                      */
/*               ArtLag.Vg    = iVg                                      */
/*               ArtLag.LopNr = piLopNr                                  */
/*               .                                                       */
/*       END.                                                            */
/*                                                                       */
/*       RELEASE bufArtBas.                                              */
/*     END.                                                              */
END PROCEDURE. /* BYTT-VG */


