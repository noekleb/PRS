/*

Denne rutinen kan benyttes for å opprette en telleliste som skal brukes
til å nullstille lagerantall på artikler.

Forutsetter at både ny og gammel tellestestehode er opprettet.
Den gamle listen kan godt være tom.

Det sjekkes ikke på lagerstyrt, fordi ikke lagerstyrte varer alltid skal
ha lagerantall = 0. Har de et antall forskjellig fra 0, skal de 
nullstilles.

Når den nye lsiten er opprettet, oppdateres den uten at det registreres
noe antall talt. Hele hensikten med listen er å få nullstillt lager
på de artikler som er logget i listen.

*/

current-window:width = 100.
def var ii as inte no-undo.
def var ilagant as inte no-undo.
def var deVerdi as decimal.


DEF VAR iButNr      AS INT NO-UNDO.
DEF VAR iTelleNrGml AS INT NO-UNDO.
DEF VAR iTelleNrNy  AS INT NO-UNDO.
DEF VAR wCl         AS INT NO-UNDO.
DEF VAR wVVAreKost  AS DEC NO-UNDO.

def buffer clButiker for Butiker.

{syspara.i 5 1 1 wCl INT}
find clButiker no-lock where
  clButiker.Butik = wCl.

ASSIGN
    iButNR = 1 /* 279 */
    iTelleNrGml = 1
    iTelleNrNy  = 2
    .

IF NOT CAN-FIND(TelleHode WHERE
                TelleHode.TelleNR = iTelleNrGml) THEN
    RETURN.
IF NOT CAN-FIND(TelleHode WHERE
                TelleHode.TelleNR = iTelleNrNy) THEN
    RETURN.
find Butiker no-lock where
    Butiker.Butik = iButNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN
    RETURN.

LAGER:
for each lager where lager.butik = iButNr:
    FIND ArtBas OF Lager NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
        NEXT LAGER.
    IF ArtBas.OPris THEN
        NEXT LAGER.

    /* Sjekker om den ligger i den gamle tellelisten */
    if not can-find(first tellelinje where 
                   tellelinje.tellenr    = iTelleNrGml and   
                   TelleLinje.ArtikkelNr = Lager.ArtikkelNr) and 
                   lager.lagant <> 0 then 
    TELLELINJE:
    do:
        assign 
            deVerdi = deverdi + (lagant * vvarekost)
            .
        FOR EACH ArtLag NO-LOCK where
            ArtLAg.ArtikkelNr = ArtBAs.ArtikkelNr AND
            ArtLAg.LagAnt <> 0:

            find ArtPris no-lock where
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
              ArtPris.ProfilNr   = Butiker.ProfilNr no-error.

            /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
            if not available ArtPris then
              do:
                find ArtPris no-lock where
                  ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
                  ArtPris.ProfilNr   = clButiker.ProfilNr no-error.
              end.
            if available ArtPris then
              wVVAreKost = ArtPris.Pris[if ArtPris.Tilbud then 2 else 1].

            /* Teller lager */
            ilagant = iLagant + Artlag.lagant.


            find TelleLinje exclusive-lock where
              TelleLinje.TelleNr    = iTelleNrNy and
              TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr and
              TelleLinje.Butik      = iButNr and
              TelleLinje.Storl      = ArtLag.Storl no-error.
            IF NOT AVAILABLE TelleLinje THEN
            /* NYPOST */
            DO:
                CREATE TelleLinje.
                ASSIGN
                    TelleLinje.TelleNr    = iTelleNrNy
                    TelleLinje.ArtikkelNr = ArtBas.ArtikkelNr
                    TelleLinje.Butik      = iButNr 
                    TelleLinje.Storl      = ArtLag.Storl 
                    .

                assign
                  Tellelinje.Beskr      = ArtBas.Beskr
                  TelleLinje.Vg         = ArtBas.Vg
                  TelleLinje.LopNr      = ArtBas.LopNr
                  TelleLinje.Kode       = ""
                  TelleLinje.LevKod     = ArtBas.LevKod
                  TelleLinje.AntallPar  = ArtLAg.LagAnt
                  TelleLinje.AntallTalt = 0
                  TelleLinje.VVareKost  = wVVAreKost
                  TelleLinje.NedSkrevet = wVVareKost
                  TelleLinje.OpprVerdi  = TelleLinje.AntallPar  * wVVareKost
                  TelleLinje.OpptVerdi  = TelleLinje.AntallTalt * TelleLinje.NedSkrevet
                  TelleLinje.LevNr      = ArtBas.LevNr
                  TelleLinje.Sasong     = ArtBas.SaSong
                  TelleLinje.Farg       = ArtBas.Farg
                  TelleLinje.MatKod     = ArtBas.MatKod
                  TelleLinje.VgLopNr    = trim(string(ArtBas.Vg,">>>9")) + "/" + trim(string(ArtBas.LopNr,">>>9"))
                  .

            END. /* NYPOST */
        END.
    end. /* TELLELINJE */
/* disp lager.artikkelnr. */
end. /* LAGER */

message ii skip ilagant skip deVerdi view-as alert-box.
