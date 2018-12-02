/************************************************************
    Program:  artikkelgridmeny.p
    Created:  TN   18 Aug 99
Description:  Initierer og utfører menyvalg fra bildegrid.

Last change:  TN   24 Sep 99   12:04 pm
************************************************************/
DEF INPUT PARAMETER  wMenuAction as CHAR NO-UNDO.
DEF INPUT PARAMETER  wOptions    as CHAR NO-UNDO.
DEF OUTPUT PARAMETER wMenuList   as CHAR NO-UNDO.

CASE wMenuAction:
  WHEN "INIT"   then run InitMeny.
  WHEN "ZOOM"   THEN run ZoomBilde.
  WHEN "ARTKOR" THEN run ArtikkelKort.
  WHEN "BYTTVG" THEN RUN ByttVg.
  WHEN "SETLOP" THEN RUN SettLopenummer.
  WHEN "WE"     THEN MESSAGE "Action WE " wOptions VIEW-AS ALERT-BOX.
  OTHERWISE
    MESSAGE "Ukjent Action-Kode mottatt av artikkelgridmeny.p. (" + wMenuAction + ").".
END CASE.

PROCEDURE InitMeny:
  /* Oppsett av meny.                                   */
  /*----------------------------------------------------*/
  /* Hvert menypunkt består av 2 ledd adskillt av ";".  */
  /*    Ledd 1 - Menytekst                              */
  /*         2 - Action code                            */
  wMenuList =  "&Ta bort;DE" +                          /* Ta bort et bilde fra griden.
                                                           NB: Håndteres i w-bildegrid.w*/
               ",;" +                                   /* Skillelinje */
               ",&Vis bilde;ZOOM" +                     /* Viser forstørret bilde. */
               ",&Artikkelkort;ARTKOR" +                /* Starter artikkelkortet. */
               ",&Sett løpenummer;SETLOP" +             /* Setter artikkelens løpenummer */
               ",&Bytt varegruppe;BYTTVG".              /* Bytte av varegruppe på artikkel */
END PROCEDURE.

/* Setter artikkelens løpenummer */
PROCEDURE SettLopenummer:
  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.

  /* Sjekker om løpenummer allerede er tildelt */
  IF ArtBas.LopNr <> ? AND ArtBas.LopNr <> 0 then
    DO:
      MESSAGE "Artikkelen er allerede tildelt varegruppe/løpenummer: " +
              STRING(ArtBas.Vg) + "/" + STRING(ArtBas.LopNr) + "!"
              VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
      RETURN.
    END.
  else run d-vtildelopnr.w (input recid(ArtBas)).
END PROCEDURE.

/* Viser et forstørret bilde av artikkelen. */
PROCEDURE ZoomBilde:

  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.

  if available ArtBAs then
    do:
      find BildeRegister of ArtBas no-error.
      if available BildeRegister then
        run d-visbil.w (input recid(BildeRegister)).
    end.
END.

/* Starter artikkelkortet. */
PROCEDURE ArtikkelKort:

  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.

  if available ArtBas then
    run w-vartkor (input recid(ArtBas), "ENDRE").

END PROCEDURE.

/* Bytte av varegruppe på artikkel. */
PROCEDURE ByttVg:
  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.

  IF NOT AVAILABLE ArtBas THEN
      RETURN NO-APPLY.

  RUN d-byttvg (ArtBas.ArtikkelNr).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
END PROCEDURE.


