DEF VAR cFilNavn AS CHAR NO-UNDO.  
DEF VAR lplListeId AS DEC NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.
DEF VAR iLevType AS INT NO-UNDO.
    
ASSIGN 
    lPlListeId = 279
    iButNr = 2
    iLevType = 3
    .
RUN skrivplukkliste.p (lPlListeId, iButNr, iLevType, OUTPUT cfilnavn).
IF iLevType = 1 AND SEARCH(cfilnavn) <> ? THEN 
  RUN browse2pdf\viewxmldialog.w (cFilNavn,"Rapport").    


