/*
   Opprettet: 29.09.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEF VAR fReklamVerdi    AS DEC NO-UNDO.
DEF VAR fReklamTotal    AS DEC NO-UNDO.
DEF VAR fReklamUtgift   AS DEC NO-UNDO.
DEF VAR iReklamasjonsNr AS INT NO-UNDO.
DEF VAR iAnt            AS INT NO-UNDO.

iReklamasjonsNr = INT(ENTRY(1,icParam,';')).
DO TRANSACTION:
 FIND FIRST Reklamasjonslogg WHERE Reklamasjonslogg.ReklamasjonsNr = iReklamasjonsNr
                             EXCLUSIVE-LOCK NO-ERROR.

 IF AVAIL reklamasjonslogg THEN
 DO:
   FOR EACH Reklamasjonslinje WHERE 
       Reklamasjonslinje.Reklamasjonsnr = iReklamasjonsnr NO-LOCK
       BY ReklamasjonsLinje.LinjeNr:

     ASSIGN
         iAnt          = iAnt + 1
         fReklamVerdi  = fReklamVerdi  + (IF Reklamasjonslinje.ReklamUtgift > 0 THEN Reklamasjonslinje.ReklamUtgift ELSE (Reklamasjonslinje.Antall * ( Reklamasjonslinje.pris - Reklamasjonslinje.RabKr)))
         fReklamTotal  = fReklamTotal  + (IF Reklamasjonslinje.ReklamUtgift > 0 THEN Reklamasjonslinje.ReklamUtgift ELSE (Reklamasjonslinje.Antall * Reklamasjonslinje.VVarekost)) 
         fReklamUtgift = fReklamUtgift + Reklamasjonslinje.ReklamUtgift
/*          fReklamVerdi  = fReklamVerdi  + Reklamasjonslinje.ReklamUtgift + (Reklamasjonslinje.Antall * ( Reklamasjonslinje.pris - Reklamasjonslinje.RabKr)) */
/*          fReklamTotal  = fReklamTotal  + (Reklamasjonslinje.Antall * Reklamasjonslinje.VVarekost) + Reklamasjonslinje.ReklamUtgift                         */
/*          fReklamUtgift = fReklamUtgift + Reklamasjonslinje.ReklamUtgift                                                                                    */
         .

     /* Setter på leverandørnummer hvis det ikke er satt.*/
     IF Reklamasjonslogg.LevNr = 0 AND Reklamasjonslinje.ArtikkelNr > 0 THEN
     DO:
         FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = Reklamasjonslinje.ArtikkelNr NO-ERROR.
         IF AVAILABLE ArtBas THEN
             ASSIGN
             Reklamasjonslogg.LevNr = ArtBas.LevNr.
     END.
   END.                              
   
   ASSIGN 
     Reklamasjonslogg.ReklamVerdi    = fReklamVerdi
     Reklamasjonslogg.ReklamUtgifter = fReklamUtgift
     Reklamasjonslogg.ReklamTotal    = fReklamTotal
     ocReturn                        = ''
     obOk                            = TRUE
     .
   /* Ta bort referanse til reklamasjonslinje. */
   IF iAnt = 0 THEN
       ASSIGN
           Reklamasjonslogg.ArtikkelNr = 0
           Reklamasjonslogg.LevKod     = ""
           Reklamasjonslogg.Besk       = "".
 END.


END. /*Transaction*/
IF AVAIL Reklamasjonslogg THEN RELEASE Reklamasjonslogg.
  
