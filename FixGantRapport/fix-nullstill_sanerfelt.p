
FIND ArtBAs WHERE artikkelnr = 9853172. /* til 9851647 */

ASSIGN Artbas.IKasse   = TRUE
       ArtBas.Sanertdato = ?
       ArtBas.Beskr = IF NUM-ENTRIES(ArtBas.Beskr,':') > 1 THEN ENTRY(2,ArtBas.Beskr,':') ELSE ArtBas.Beskr
       ArtBas.BongTekst = ArtBas.Beskr
    .
DISPLAY
    Artbas.IKasse
    ArtBas.Sanertdato
    ArtBas.Beskr
    ArtBas.BongTekst
    ArtBas.Beskr
    .
