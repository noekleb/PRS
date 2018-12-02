/*
   Opprettet: 29.09.2007 Geir Otto Olsen
 -----------------------------------------------------------------------------------*/
 DEF INPUT  PARAM icRowid             AS CHAR NO-UNDO.
 DEF INPUT  PARAM icFields            AS CHAR NO-UNDO.
 DEF INPUT  PARAM icValues            AS CHAR NO-UNDO.
 DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
 DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.  

 DEF VAR fReklamVerdi  AS DEC NO-UNDO.
 DEF VAR fReklamTotal  AS DEC NO-UNDO.
 DEF VAR fReklamUtgift AS DEC NO-UNDO.

  &SCOPED-DEFINE useTable Reklamasjonslogg
  FIND {&useTable} WHERE ROWID({&useTable}) = TO-ROWID(icRowId) EXCLUSIVE-LOCK NO-ERROR.
  
  IF NOT AVAIL {&useTable} THEN
  DO:
   ocReturn = 'Posten for {&useTable} ble ikke funnet'.
   RETURN.
  END.
/*   MESSAGE                                                                                     */
/*     DEC(ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|")) <> Reklamasjonslogg.KampTilbBelop */
/*     DEC(ENTRY(LOOKUP('KampTilbBelop',icFields),icValues,"|"))                                 */
/*                                                                                               */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                        */
  IF Reklamasjonslogg.RegistrertDato = ? THEN
    Reklamasjonslogg.RegistrertDato = TODAY.

  IF LOGICAL(ENTRY(LOOKUP('SluttfortBesluttet',icFields),icValues,"|")) <> Reklamasjonslogg.SluttfortBesluttet THEN
  DO:
    IF LOGICAL(ENTRY(LOOKUP('SluttfortBesluttet',icFields),icValues,"|")) THEN
    DO:
      ASSIGN 
        Reklamasjonslogg.sluttfortAv   = DYNAMIC-FUNCTION('getASUserId')
        Reklamasjonslogg.sluttfortDato = TODAY
      .
    END.
    ELSE
    DO:
      ASSIGN 
        Reklamasjonslogg.sluttfortAv   = ''
        Reklamasjonslogg.sluttfortDato = ?
      .
    END.
  END.
  IF LOGICAL(ENTRY(LOOKUP('AkseptertBesluttet',icFields),icValues,"|")) <> Reklamasjonslogg.AkseptertBesluttet THEN
  DO:
    IF LOGICAL(ENTRY(LOOKUP('AkseptertBesluttet',icFields),icValues,"|")) THEN
    DO:
      ASSIGN 
        Reklamasjonslogg.AkseptertAv   = DYNAMIC-FUNCTION('getASUserId')
        Reklamasjonslogg.AkseptertDato = TODAY
      .
    END.
    ELSE
    DO:
      ASSIGN 
        Reklamasjonslogg.AkseptertAv   = ''
        Reklamasjonslogg.AkseptertDato = ?
      .
    END.
  END.
  IF LOGICAL(ENTRY(LOOKUP('BetalesAvgjort',icFields),icValues,"|")) <> Reklamasjonslogg.BetalesAvgjort THEN
  DO:
    IF LOGICAL(ENTRY(LOOKUP('BetalesAvgjort',icFields),icValues,"|")) THEN
    DO:
      ASSIGN 
        Reklamasjonslogg.BetalesBruker = DYNAMIC-FUNCTION('getASUserId')
        Reklamasjonslogg.BetalesDato   = TODAY
      .
    END.
    ELSE
    DO:
      ASSIGN 
        Reklamasjonslogg.BetalesBruker = ''
        Reklamasjonslogg.BetalesDato   = ?
      .
    END.
  END.
  
  /*
  FOR EACH reklamasjonslinje OF Reklamasjonslogg NO-LOCK:
    fReklamVerdi  = fReklamVerdi + (Reklamasjonslinje.Antall * ( reklamasjonslinje.pris - Reklamasjonslinje.RabKr)).
    fReklamUtgift = fReklamUtgift + reklamasjonslinje.ReklamUtgift.
  END.
  fReklamTotal = fReklamVerdi - fReklamUtgift.
  ASSIGN 
    reklamasjonslogg.ReklamVerdi    = fReklamVerdi
    Reklamasjonslogg.ReklamUtgifter = fReklamUtgift
    Reklamasjonslogg.ReklamTotal    = fReklamTotal
  .
  */

