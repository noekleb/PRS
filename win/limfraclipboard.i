/************************************************************
    Program:  limfraclipboard.i
    Created:  TN    4 Nov 99
Description:

Last change:  TN    8 Dec 99    3:53 pm
************************************************************/
  def var wBekreftNyttNr as log initial false.
  DEF VAR wParameter1    as CHAR NO-UNDO.
  DEF VAR cClipTekst     as CHAR NO-UNDO.
  def var wLoop        as int  no-undo.
  def var wNyFil       as char no-undo.
  def var wKatalog     as char no-undo.
  def var wReturnValue as char no-undo.
  def buffer bufArtBas for ArtBas.
  IF SELF:PRIVATE-DATA = "" OR SELF:PRIVATE-DATA = ? THEN DO:
     assign
        SELF:PRIVATE-DATA = ""
        cClipTekst = CLIPBOARD:value
        no-error.
     IF cClipTekst <> ? then
     DO:
          MESSAGE "Clipboard inneholder ugyldige data (1)."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.

      /* Nullstiller error flagg for ocx. */
      chIMAGE-Sko:Picbuf:errornumber = 0.

      /* Legger bilde inn i buffer fra ClipBoard. */
      chIMAGE-Sko:Picbuf:PasteClipboard no-error.

      IF chIMAGE-Sko:Picbuf:errornumber <> 0 then
      DO:
          MESSAGE "Cliboard inneholder ugyldige data (2)."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
             
  assign
    wBildNr = &IF "{&BildNr}" <> ""
                &THEN {&BildNr}
                &ELSE ArtBas.BildNr
              &ENDIF.

  /* Sjekker om det ligger noe i ClipBoard. */
  if SELF:PRIVATE-DATA = "" AND ClipBoard:num-formats > 0 then
    do:
      message skip(1)
              "       ClipBoard er tomt!" skip
              "              eller" skip
              "det inneholder ugyldige data" skip(1)
              view-as alert-box title "Melding".
      return no-apply.
    end.

  if not valid-handle(wLibHandle) then
    do:
      message "Prosedyrebiblioteket er ikke startet!" view-as alert-box Title "Melding".
      return no-apply.
    end.

  /* Er bildenummer <> 0 skal bruker spørres om det skal tildeles nytt løpenummer eller */
  /* om det nye bilde skal skrive over det gamle.                                       */
  /* Bekreftelse kan sl†s av/p† via systemparameter. */
  {syspara.i 10 1 10 wParameter1}
  if wParameter1 = "" THEN wParameter1 = "2". /* Default er at det skal bekreftes. */
  if wBildNr <> 0 and wParameter1 <> "1" then
    do:
      message "Artikkelen er allerede tildelt et bildenummer." skip
              "Ja     - Nytt bilde legges på det eksisterende bildenummer." skip
              "Nei    - Nytt bildenummer tildeles."
              view-as alert-box question buttons YES-NO-CANCEL title "Bekreft"
              update wBekreftNyttNr.
      if wBekreftNyttNr = ? then
        return no-apply.
    end.
  else do:
    assign
      wBekreftNyttNr = false. /* Alltid nytt nr når nr = 0. */
  end.

  BILDENUMMER:
  do:
    /* Henter nytt nummer */
    if wBekreftNyttNr = false then
      run BildeNummer in wLibHandle (input "B", output wBildNr, output wFilNavn, output wKatalog).
    /* Benytter gammelt nummer. */
    else 
      run GmlBildeNummer in wLibHandle (input "B", input-output wBildNr, output wFilNavn, output wKatalog).
 
    if return-value = "AVBRYT" then
      do:
        message "Klarte ikke å skape et filnavn!" view-as alert-box 
          title "Melding".
        return no-apply.
      end.
    find BildeRegister no-lock where
       BildeRegister.BildNr = wBildNr no-error.
  
    &IF "{&Skjerm}" <> ""
      &THEN
        {&Skjerm}
      &ELSE
        /* Setter bildenummer i ArtBas */
        do for bufArtBas TRANSACTION:
          find bufArtBas exclusive-lock where
            recid(bufArtBAs) = recid(ArtBas) no-error.
          if available bufArtBas then
            do:
              assign
                bufArtBas.BildNr = wBildNr.
              release bufArtBas.
            end.
        end.
    &ENDIF.
  end. /* BILDENUMMER */

  /* Legger bilde inn i buffer fra ClipBoard. */
  IF SELF:PRIVATE-DATA = "" THEN
      chIMAGE-Sko:Picbuf:PasteClipboard no-error.
  ELSE DO:
      chIMAGE-Sko:PicBuf:CLEAR(2).
      ASSIGN chIMAGE-Sko:PicBuf:FILENAME = SELF:PRIVATE-DATA.
      chIMAGE-Sko:PicBuf:LOAD().
  END.

  /* Tildeler filnavn */
  chIMAGE-Sko:Picbuf:FileName = wKatalog + "\" + wFilNavn.
  
  /* Lagrer bilde på hd. ------------------------------------------------ */
  If chIMAGE-Sko:Picbuf:WriteCompression = 0 Then  /* Filen skal komprimeres.    */
    chIMAGE-Sko:Picbuf:WriteCompression = 65.
  chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
  If chIMAGE-Sko:Picbuf:WriteCompression <> 0 Then /* Filen skal komprimeres.    */
    chIMAGE-Sko:Picbuf:WriteCompression = 0.
    
  /* Leser inn filen. */
  assign wReturnValue = "AVBRYT".
  if search(wKatalog + "\" + wFilNavn) <> ? then
    do:
      if valid-handle(wLibHandle) then
        run LesInnBilde in wLibHandle (BildeRegister.BildNr, wKatalog + "\" + wFilNavn, output wReturnValue).
    end.
  if wReturnValue = "AVBRYT" then
    do:
      message "Feil ved lasting av bilde " BildeRegister.BildNr
        view-as alert-box error title "Feil".
    end.
  ELSE
      RUN w-Forminsk.w (Bilderegister.Bildnr).
