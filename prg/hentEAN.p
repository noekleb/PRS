/*
  RUN hentEAN.p (piEANType,piLandKode,OUTPUT cEAN). 
  
*/
  DEFINE INPUT PARAMETER  piEANType  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER  piLandKode AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER cEAN       AS CHARACTER NO-UNDO.
  
  DEF VAR iInt AS INT  NO-UNDO.

  DEF BUFFER bufEANNrListe FOR EANNrListe.

  /* Default landkode er satt i systemparameter. */
  {syspar2.i 1 90 3 piLandKode INT}

  IF piEANType  = 0 THEN piEANType  = 1.
  IF piLandKode = 0 THEN piLandkode = 2. /* Bedriftsinterne koder. */

  CASE piEANType:
      WHEN  1 THEN ASSIGN piEANType  = 13 piLandkode = 2.
      WHEN 10 THEN ASSIGN piEANType  = 13.
      WHEN 11 THEN ASSIGN piEANType  =  8 piLandkode = 2.
      WHEN 12 THEN ASSIGN piEANType  =  8.
  END CASE.

  LOOPEN:
  DO iInt = 1 TO 2:
    HENT_EAN:
    FOR EACH EANNrSerie NO-LOCK WHERE
      EANNrSerie.EANSerieAktiv = TRUE AND
      EANNrSerie.EANType       = piEANType AND
      (IF piLandKode > 2
       THEN EANNrSerie.EANLandKode > 2
       ELSE EANNrSerie.EANLandKode > 0)
      BREAK BY EANNrSerie.EANSerieAktiv
            BY EANNrSerie.EANLandKode
            BY EANNrSerie.EANSerieId:
      LISTE:
      FOR EACH EANNrListe OF EANNrSerie NO-LOCK WHERE
          EANNrListe.ArtikkelNr = 0:
          ASSIGN
              cEAN = EANNrListe.EANKode.
          /* Skulle det ligge en luring, settes den her. */
          IF CAN-FIND(FIRST Strekkode WHERE Strekkode.Kode = cEAN) THEN
          DO:
              FIND Strekkode NO-LOCK WHERE
                  Strekkode.Kode = cEAN.
              FIND bufEANNrListe EXCLUSIVE-LOCK WHERE
                  RECID(bufEANNrListe) = RECID(EANNrListe).
              ASSIGN
                  bufEANNrListe.ArtikkelNr = Strekkode.ArtikkelNr.
              NEXT LISTE.
          END.
          ELSE DO:
              /* Flagger at den er brukt */
              FIND bufEANNrListe EXCLUSIVE-LOCK WHERE
                  RECID(bufEANNrListe) = RECID(EANNrListe).
              ASSIGN
                  bufEANNrListe.ArtikkelNr = 1.
          END.
          LEAVE HENT_EAN.
      END. /* LISTE */
    END. /* HENT_EAN */ 
    /* Feilet det tar vi en runde til. */ 
    IF cEAN = '' AND piLandKode > 0 THEN
      DO:
        piLandKode = 2.
        NEXT.
      END.
    ELSE
      LEAVE LOOPEN.
  END. /* LOOPEN */
  
  IF cEAN = '' THEN
  DO:
      MESSAGE "EAN nr. serie er full, eller mangler." SKIP
              "Det må legges opp en ny EAN nr. serie. Kontakt systemansvarlig."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

