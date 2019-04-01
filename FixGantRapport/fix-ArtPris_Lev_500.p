
DEF BUFFER bufArtPris FOR ArtPris.

CURRENT-WINDOW:WIDTH = 350.

FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.LEvNr = 500,
    FIRST ArtPris OF ArtBas WHERE
        ArtPris.ProfilNr = 1:
    
    FIND bufArtPris OF ArtBas WHERE 
             bufArtPris.ProfilNr = 2 NO-ERROR.

    IF NOT AVAILABLE bufArtPris THEN
    DO:
        CREATE bufArtPris.
        BUFFER-COPY ArtPris
            EXCEPT ProfilNr
            TO bufArtPris
            ASSIGN
                bufArtPris.ProfilNr = 2.
    END.

/*     DISPLAY                                 */
/*         ArtBAs.ArtikkelNr                   */
/*         ArtPris.Innkjopspris[1]             */
/*         ArtPris.Rab1%[1]                    */
/*         ArtPris.VareKost[1]                 */
/*         Artpris.Pris[1]                     */
/*         '|'                                 */
/*         CAN-FIND(bufArtPris OF ArtBas WHERE */
/*                  bufArtPris.ProfilNr = 2)   */
/*     WITH WIDTH 350.                         */

    IF AVAILABLE bufArtPris THEN
    DO:
        ASSIGN
              bufArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
              bufArtPris.ValPris[1]      = ArtPris.InnkjopsPris[1]
              bufArtPris.Rab1%[1]        = 45
              bufArtPris.Rab1Kr[1]       = ROUND((bufArtPris.InnkjopsPris[1] * bufArtPris.Rab1%[1])/ 100,2) 
              bufArtPris.Rab1Kr[1]       = if bufArtPris.Rab1Kr[1] = ? then 0 else bufArtPris.Rab1Kr[1] 
              bufArtPris.VareKost[1]     = bufArtPris.InnkjopsPris[1] - bufArtPris.Rab1Kr[1]
              bufArtPris.Pris[1]         = ROUND(ArtPris.Pris[1] - ((ArtPris.Pris[1] * 30)/ 100),2) 
              bufArtPris.MvaKr[1]        = bufArtPris.Pris[1] - (bufArtPris.Pris[1] / (1 + (bufArtPris.Mva%[1] / 100)))
              bufArtPris.DbKr[1]         = bufArtPris.Pris[1] - bufArtPris.MvaKr[1] - bufArtPris.VareKost[1]
              bufArtPris.DB%[1]          = ROUND((bufArtPris.DbKr[1] * 100) / (bufArtPris.VareKost[1] + bufArtPris.DbKr[1]),2)
              bufArtPris.DB%[1]          = IF bufArtPris.DB%[1] = ? THEN 0 ELSE bufArtPris.DB%[1]
          .

/*         DISPLAY                                   */
/*             bufArtPris.Innkjopspris[1]            */
/*             bufArtPris.Rab1%[1]                   */
/*             bufArtPris.VareKost[1]                */
/*             bufArtpris.Pris[1]                    */
/*             Artpris.Pris[1] <> bufArtpris.Pris[1] */
/*         WITH WIDTH 350.                           */
    END.
END.
