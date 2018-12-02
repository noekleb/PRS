TRIGGER PROCEDURE FOR WRITE OF VPIArtBas OLD BUFFER oldVPIArtBas.

DEF VAR ctrgTekst AS CHAR NO-UNDO.
DEF VAR btrgHk    AS LOG  NO-UNDO.
DEFINE VARIABLE bInitVPIBildekode AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

{syspara.i 1 1 18 ctrgTekst}
IF CAN-DO("ja,1,true,yes",ctrgTekst) THEN
    btrgHK = TRUE.
ELSE
    btrgHK = FALSE.

{syspara.i 2 4 200 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) 
  THEN bInitVPIBildekode = TRUE.
  ELSE bInitVPIBildekode = FALSE.

{trg\c_w_trg.i &Fil=VpiArtBas &Type="W"}

/* Initiering av bildekodefeltet. */
IF bInitVPIBildekode = FALSE THEN DO:
  IF VPIArtBas.VPIBildeKode = '' 
    THEN VPIArtBas.VPIBildeKode = RIGHT-TRIM(REPLACE(REPLACE(STRING(VPIArtBas.LevNr) + '_' + VPIArtBas.LevKod + '_' + VPIArtBas.LevFargKod,'/','_'),' ','_'),'_') + '.jpg'.
END.

/* Initierer artikkelnr */
IF VPIArtBas.ArtikkelNr = 0 THEN
    VPIArtBas.ArtikkelNr = DEC(VPIArtBas.VareNr).

IF btrgHK = TRUE THEN
DO:
/*     FIND ELogg WHERE ELogg.TabellNavn = "vpiArtBas" AND                                                      */
/*                  ELogg.EksterntSystem = "POS"    AND                                                         */
/*                  ELogg.Verdier        = STRING(vpiArtBas.EkstVPILevNr) + CHR(1) + vpiArtBas.VareNr NO-ERROR. */
/*     IF NOT AVAIL Elogg THEN DO:                                                                              */
/*         CREATE Elogg.                                                                                        */
/*         ASSIGN ELogg.TabellNavn = "vpiArtBas"                                                                */
/*                  ELogg.EksterntSystem = "POS"                                                                */
/*                  ELogg.Verdier        = STRING(vpiArtBas.EkstVPILevNr) + CHR(1) + vpiArtBas.VareNr NO-ERROR. */
/*     END.                                                                                                     */
/*     ASSIGN ELogg.EndringsType = 1                                                                            */
/*            ELogg.Behandlet    = FALSE.                                                                       */
/*     RELEASE ELogg.                                                                                           */
END.


UTVIDETSOK:
DO:
  RUN init_utvidetsok_vpiartbas.p (VPIArtBAs.ARtikkelNr).
END. /* UTVIDETSOK */
