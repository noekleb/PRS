TRIGGER PROCEDURE FOR WRITE OF StrekKode OLD BUFFER oldStrekKode.

DEFINE BUFFER trgEANNrListe FOR EANNrListe.
DEFINE BUFFER trgArtBas FOR ArtBas.

DEFINE VARIABLE bUndertrykk AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cGenInterleave AS CHARACTER NO-UNDO.
{syspara.i 2 4 17 cGenInterleave}

{syspara.i 2 4 43 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) 
  THEN bUndertrykk = TRUE.
  ELSE bUndertrykk = FALSE.

/* Denne må erstattes av at det opprettes ArtBas ELogg direkte.
   Dette gir postlåsning på ELogg for datamottaksserver. REf. problemer hos Anton HK.
FIND trgArtBas EXCLUSIVE-LOCK WHERE 
  trgArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
IF AVAILABLE trgArtBas THEN 
  trgArtBas.ETid = TIME.
*/

ASSIGN
  StrekKode.EDato    = TODAY
  StrekKode.ETid     = TIME
  StrekKode.BrukerId = USERID("skotex").
  
  /* Initierer ERPNr hvis det er blankt. */
  /* TN 16/9-10 Dette skal ikke gjøres. ERPNr vil bli innlest direkte fra ERP via en egen import  
  IF (Strekkode.StrKode > 0 AND 
      Strekkode.ArtikkelNr > 0 AND 
      Strekkode.ERPNr = '') THEN
    ASSIGN Strekkode.ERPNr = STRING(STrekkode.ArtikkelNr) + trim(STRING(Strekkode.StrKode,">>999")).
  */


/* IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = StrekKode.ArtikkelNr AND ArtBas.KjedeVare = TRUE) THEN */
/* ERP:                                                                                                */
/* DO:                                                                                                 */
/*     FIND ELogg WHERE                                                                                */
/*          ELogg.TabellNavn     = "ArtBas" AND                                                        */
/*          ELogg.EksterntSystem = "ERP"    AND                                                        */
/*          ELogg.Verdier        = STRING(Strekkode.ArtikkelNr)  + CHR(1) + Strekkode.Kode NO-ERROR.   */
/*     IF NOT AVAIL Elogg THEN DO:                                                                     */
/*         CREATE Elogg.                                                                               */
/*         ASSIGN ELogg.TabellNavn     = "ArtBas"                                                      */
/*                ELogg.EksterntSystem = "ERP"                                                         */
/*                ELogg.Verdier        = STRING(Strekkode.ArtikkelNr)  + CHR(1) + Strekkode.Kode.      */
/*     END.                                                                                            */
/*     ASSIGN ELogg.EndringsType = 1                                                                   */
/*            ELogg.Behandlet    = FALSE.                                                              */
/*     RELEASE ELogg.                                                                                  */
/* END. /* ERP */                                                                                      */

/* Assigner feltet for utvidet søk. */
/* Dette går i loop med trigger i w_artbas.p 
UTVIDETSOK:
DO:
  RUN init_utvidetsok.p (Strekkode.ArtikkelNr).
END. /* UTVIDETSOK */
*/


/* Flagger at koden er brukt hvis den finnes i en liste. */
DO FOR trgEANNrListe:
    IF CAN-FIND(trgEANNrListe WHERE
                trgEANNrListe.EANKode = Strekkode.Kode) THEN
    DO:
      FIND trgEANNrListe EXCLUSIVE-LOCK WHERE
                trgEANNrListe.EANKode = Strekkode.Kode NO-ERROR NO-WAIT.
      IF AVAILABLE trgEANNrListe THEN
      DO:
          trgEANNrListe.ArtikkelNr = Strekkode.ArtikkelNr.
          FIND CURRENT trgEANNrListe NO-LOCK.
      END.
    END.
    IF AVAILABLE trgEANNrListe THEN RELEASE trgEANNrListe.
END.

IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = Strekkode.ArtikkelNr AND
                             ArtBas.iKasse = TRUE) THEN
    RETURN.
IF StrekKode.iKasse = FALSE AND oldStrekKode.iKasse = FALSE THEN
    RETURN. /* ingen ändring mot kassa så därför gör vi ingen loggning */

/* Logger utlegg for de profiler det gjelder. */
IF bUndertrykk = FALSE AND 
   CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = Strekkode.ArtikkelNr AND
                         ArtBas.iKasse     = TRUE) THEN
DO FOR ELogg:
    FOR EACH ArtPris NO-LOCK WHERE ArtPris.ArtikkelNr = Strekkode.ArtikkelNr:
      FIND ELogg EXCLUSIVE-LOCK WHERE 
           ELogg.TabellNavn     = "ArtPris" AND
           ELogg.EksterntSystem = "POS"    AND
           ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) NO-ERROR NO-WAIT.
      IF LOCKED ELOGG THEN NEXT.
      ELSE DO:
        IF NOT AVAIL Elogg THEN DO:
          CREATE Elogg.
          ASSIGN ELogg.TabellNavn     = "ArtPris"
                 ELogg.EksterntSystem = "POS"   
                 ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr)
                 ELogg.EndringsType = 1
                 ELogg.Behandlet    = FALSE NO-ERROR.
        END.
        ELSE ASSIGN ELogg.EndringsType = 1
                    ELogg.Behandlet    = FALSE NO-ERROR.
      END.
    END.     
    IF AVAILABLE ELogg THEN RELEASE ELogg.
END.

/* Utlegg til ferskvarevekt */
DO FOR trgArtBas,ELogg:
    FIND trgArtBas NO-LOCK WHERE 
      trgArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.
    IF AVAILABLE trgArtBas AND 
       CAN-FIND (FIRST SysPara WHERE SysPara.SysHId = 23 AND SysPara.SysGr = 1 AND SysPara.Parameter1 > '0') THEN 
    DO:
      IF trgArtBas.ArtSlag = 1 AND LENGTH(Strekkode.Kode) = 13 THEN
      BLOKKEN: 
      DO:
        FIND ELogg EXCLUSIVE-LOCK WHERE 
             ELogg.TabellNavn     = "ArtBas" AND
             ELogg.EksterntSystem = "FVEKT"    AND
             ELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) + CHR(1) + STRING(Strekkode.Kode) NO-ERROR NO-WAIT.
        IF LOCKED ELogg THEN LEAVE BLOKKEN.
        ELSE DO:
          IF NOT AVAIL Elogg THEN DO:      
              CREATE Elogg.
              ASSIGN ELogg.TabellNavn     = "ArtBas"
                     ELogg.EksterntSystem = "FVEKT"   
                     ELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) + CHR(1) + STRING(Strekkode.Kode)
                     ELogg.EndringsType   = IF trgArtBas.ArtSlag = 1 THEN 1 ELSE 3 
                     ELogg.Behandlet      = FALSE
                     NO-ERROR.
          END.
          ELSE ASSIGN ELogg.EndringsType = IF trgArtBas.ArtSlag = 1 THEN 1 ELSE 3 
                      ELogg.Behandlet    = FALSE NO-ERROR.
          IF AVAILABLE ELogg THEN RELEASE ELogg.
        END.
      END.
    END.
    IF AVAILABLE ELogg THEN RELEASE ELogg.


    IF cGenInterleave = '1' THEN
    GENINTERLEAVE: 
    DO:
      IF trgArtBas.Vg <= 999 AND trgArtBas.LopNr <= 9999 AND Strekkode.Bestillingsnummer = '' THEN
      DO:
        FIND StrKonv WHERE 
          StrKonv.StrKode = Strekkode.StrKode USE-INDEX StrKode NO-LOCK NO-ERROR.
        IF NOT AVAIL StrKonv THEN
              LEAVE GENINTERLEAVE.
          ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 
                           THEN TRIM(REPLACE(StrKonv.Storl,".","")) 
                           ELSE TRIM(StrKonv.Storl) + "0"
                 cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                 cKode = STRING(trgArtBas.Vg,"999")     +
                         STRING(trgArtBas.LopNr,"9999") +
                         "0" +
                         cStrl NO-ERROR.
          IF ERROR-STATUS:ERROR = FALSE THEN 
              Strekkode.Bestillingsnummer = cKode.
      END.
    END. /* GENINTERLEAVE */

    IF AVAILABLE trgArtBas THEN 
      RELEASE trgArtBas.
END.


