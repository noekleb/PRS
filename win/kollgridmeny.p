/************************************************************
    Program:  kollgridmeny.p
    Created:  TN   18 Aug 99
Description:  Initierer og utfører menyvalg fra bildegrid.

Last change:  TN    3 May 101    0:27 am
************************************************************/
DEF INPUT PARAMETER  wMenuAction as CHAR NO-UNDO.
DEF INPUT PARAMETER  wOptions    as CHAR NO-UNDO.
DEF OUTPUT PARAMETER wMenuList   as CHAR NO-UNDO.

CASE wMenuAction:
  WHEN "INIT"   then run InitMeny.
  WHEN "ZOOM"   THEN run ZoomBilde.
  WHEN "ARTKOR" THEN run ArtikkelKort.
  WHEN "SETLOP" THEN RUN SettLopenummer.
  WHEN "ENDRE"  THEN RUN EndreBestilling.
  WHEN "KALKYL" THEN RUN BestillingsKalkyle.
  WHEN "INLEV"  THEN RUN Innleveranse.
  WHEN "BYTTVG" THEN RUN ByttVg.
  WHEN "WE"     THEN MESSAGE "Action WE " wOptions VIEW-AS ALERT-BOX.
  OTHERWISE
    MESSAGE "Ukjent Action-Kode mottatt av kollgridmeny.p. (" + wMenuAction + ").".
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
               ",;" +                                   /* Skillelinje */
               ",&Endre bestilling;ENDRE" +             /* Endre bestillingen */
               ",&Endre kalkyle på bestilling;KALKYL" + /* Endre bestillingens kalkyle */
               ",&Innleveranse av bestilling;INLEV" +   /* innleveranse av bestilling. */
               ",&Bytt varegruppe;BYTTVG".              /* Bytte av varegruppe på artikkel */
END PROCEDURE.

PROCEDURE Innleveranse:
  def var wBestHodeRecid as recid no-undo.
  
  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.
  if not available ArtBas then
    return no-apply.

  FIND BestHode NO-LOCK where
    BestHode.BestNr = INT(ENTRY(2,wOptions)) NO-ERROR.
  if not available BestHode then
    return no-apply.
    
  if ArtBas.LopNr = 0 or ArtBas.LopNr = ? then
    do:
      message "Artikkelen må tildeles løpenummer før innleveranse kan gjøres!"
        view-as alert-box MESSAGE title "Melding".
      return no-apply.
    end.
    
  if not available BestHode then
    return no-apply.

  if BestHode.BestStat < 4  then
    do:
      message "Bestilling med denne status kan ikke innleveres!"
        view-as alert-box title "Melding".
      return no-apply.
    end.  
  
  assign
    wBestHodeRecid = recid(BestHode).
  run w-gridinnlev.w (input recid(ArtBas), input-output wBestHodeRecid, "INLEV").

END PROCEDURE.

/* Endring av bestillingingen. */
PROCEDURE EndreBestilling:
  def var wBestHodeRecid as recid no-undo.
  
  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.
  if not available ArtBas then
    return no-apply.

  FIND BestHode NO-LOCK where
    BestHode.BestNr = INT(ENTRY(2,wOptions)) NO-ERROR.
  if not available BestHode then
    return no-apply.
    
  assign
    wBestHodeRecid = recid(BestHode).
  run w-gridord.w (input recid(ArtBas), input-output wBestHodeRecid, "ENDRE").

END PROCEDURE.

/* Endre bestillingens kalkyle */
PROCEDURE BestillingsKalkyle:
  def var wBestHodeRecid as recid no-undo.
  
  FIND ArtBas NO-LOCK where
    ArtBas.ArtikkelNr = INT(ENTRY(1,wOptions)) NO-ERROR.
  if not available ArtBas then
    return no-apply.

  FIND BestHode NO-LOCK where
    BestHode.BestNr = INT(ENTRY(2,wOptions)) NO-ERROR.
  if not available BestHode then
    return no-apply.
    
  assign
    wBestHodeRecid = recid(BestHode).
  run d-vbestkalkyle.w (input recid(ArtBas), input wBestHodeRecid).

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

