/************************************************************
    Program:  bilder.i
    Created:  TN   12 Nov 98
Description:  Rutiner for h†ndtering av bilder.

        4/7-99  TN      Lagt inn inn/utlesning av bilder fra databasen.

Last change:  TN   14 Mar 100    6:49 pm
************************************************************/

/* Benytter det gamle bildenummeret. */
procedure GmlBildeNummer:
  DEF INPUT        PARAMETER wPrefix  as CHAR NO-UNDO.
  DEF input-OUTPUT PARAMETER wBildNr  as INT NO-UNDO.
  DEF       OUTPUT PARAMETER wFilNavn as CHAR NO-UNDO.
  DEF       OUTPUT PARAMETER wWorkDir as CHAR initial ".\bilder" NO-UNDO.

  DEF VAR wLoop    as INT                        NO-UNDO.
  DEF VAR wReturn-Value as CHAR INITIAL "AVBRYT" NO-UNDO.
  DEF VAR wFilExt  as CHAR      INITIAL "jpg"    NO-UNDO.

  DEF BUFFER bufBildeRegister FOR BildeRegister.
  DEF BUFFER bufBildeData     FOR BildeData.

  /* Ekstent på bilder fra ClipBoard*/
  {syspara.i 10 2 1 wFilExt}

  /* Henter Katalognavn for billedarkivet fra ini filen */
  RUN HentBildeKatalog (output wWorkDir).
  IF CAN-DO("\,/",SUBSTRING(wWorkDir,LENGTH(wWorkDir),1)) then
    wWorkDir = SUBSTRING(wWorkDir,1,LENGTH(wWorkDir) - 1).

  /* Tar bort det gamle bildet. */
  do FOR bufBildeRegister transaction:
    FIND bufBildeRegister EXCLUSIVE-LOCK where
      bufBildeRegister.BildNR = wBildNr NO-ERROR.
    IF AVAILABLE bufBildeRegister then
      DO:
        FOR EACH bufBildeData OF bufBildeRegister:
          DELETE bufBildeData.
        END.
        DELETE bufBildeRegister.
      END.
  END. /* TRANSACTION */

  /* Finner et unikt filnavn. */
  do for bufBildeRegister TRANSACTION:
    create bufBildeRegister.
    assign
      bufBildeRegister.BildNr     = wBildNr.
    assign
      bufBildeRegister.DokumentNr = 0 /* Ingen kobling til arkivpost. */
      bufBildeRegister.FilNavn    = wPrefix +  /* Alle bilder som er pastet fra ClipBoard får B. */
                                    string(wBildNr,"999999") + "." +
                                    wFilExt  /* Lagres normalt som JPG fil. */
      bufBildeRegister.Dato       = today
      bufBildeRegister.Tekst      = if wPrefix = "I"
                                      then "exellence PRO " + STRING(TODAY,"99/99/9999")
                                      else "ClipBoard " + STRING(TODAY,"99/99/9999")
      wReturn-Value               = ""
      wFilNavn                    = bufBildeRegister.FilNavn.

    release bufBildeRegister.
  end. /* TRANSACTION */


END PROCEDURE.

/* Henter første ledige bildenummer og bygger et unikt filnavn. */
PROCEDURE BildeNummer:
  DEF INPUT PARAMETER  wPrefix  as CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wBildNr  as INT NO-UNDO.
  DEF OUTPUT PARAMETER wFilNavn as CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wWorkDir as CHAR initial ".\bilder" NO-UNDO.

  DEF VAR wForste  as INT                        NO-UNDO.
  DEF VAR wLoop    as INT                        NO-UNDO.
  DEF VAR wReturn-Value as CHAR INITIAL "AVBRYT" NO-UNDO.
  DEF VAR wFilExt  as CHAR      INITIAL "jpg"    NO-UNDO.

  DEF BUFFER bufBildeRegister FOR BildeRegister.
  DEF BUFFER bufSysPAra FOR SysPara.

  /* Ekstent på bilder fra ClipBoard*/
  {syspara.i 10 2 1 wFilExt}

  assign
   wForste = ?.

  /* Henter Katalognavn for billedarkivet fra ini filen */
  RUN HentBildeKatalog (output wWorkDir).
  IF CAN-DO("\,/",SUBSTRING(wWorkDir,LENGTH(wWorkDir),1)) then
    wWorkDir = SUBSTRING(wWorkDir,1,LENGTH(wWorkDir) - 1).

  /* Finner neste ledige bildenummer */
  FIND last BildeRegister NO-LOCK NO-ERROR.
  assign
    wBildNr = if AVAILABLE BildeRegister
                THEN BildeRegister.BildNr
                ELSE 0.

  /* Sjekker om nummerserien er slutt. */
  if wBildNr >= 999999 then
    DO:
      {syspara.i 10 1 4 wForste INT}
      FIND FIRST BildeRegister NO-LOCK NO-ERROR.
      assign
        wBildNr = if AVAILABLE BildeRegister
                    THEN BildeRegister.BildNr
                    ELSE wForste.
    END.

  wLoop = wBildNr.
  {sww.i}
  do WHILE AVAILABLE BildeRegister:
    FIND BildeRegister NO-LOCK where
      BildeRegister.BildNr = wLoop NO-ERROR.
    if available BildeRegister then
      assign
        wForste = 0 /* Forskjellig fra ? */
        wLoop   = wLoop + 1.
  END.
  assign
    wBildNr = wLoop.
  {swn.i}

  /* Finner et unikt filnavn. */
  do for bufBildeRegister TRANSACTION:
    create bufBildeRegister.
    assign
      bufBildeRegister.BildNr     = wBildNr.
    assign
      bufBildeRegister.DokumentNr = 0 /* Ingen kobling til arkivpost. */
      bufBildeRegister.FilNavn    = wPrefix +  /* Alle bilder som er pastet fra ClipBoard får B. */
                                    string(wBildNr,"999999") + "." +
                                    wFilExt  /* Lagres normalt som JPG fil. */
      bufBildeRegister.Dato       = today
      bufBildeRegister.Tekst      = if wPrefix = "I"
                                      then "exellence PRO " + STRING(TODAY,"99/99/9999")
                                      else "ClipBoard " + STRING(TODAY,"99/99/9999")
      wReturn-Value               = ""
      wFilNavn                    = bufBildeRegister.FilNavn.

    release bufBildeRegister.
  end. /* TRANSACTION */

  if wForste <> ? then
  SET-SYS-PARA:
  do FOR bufSysPara TRANSACTION:
    {setsyspara.i 10 1 4 wBildNr " " "buf"}
  END. /* TRANSACTION */

END PROCEDURE.

procedure HentBildeKatalog:
  DEF OUTPUT PARAMETER wKatalog as CHAR NO-UNDO.

  /* Katalognavn kan overstyres i ini-fil.              */
  GET-KEY-VALUE SECTION "SYSPARA" KEY "BilledRegister" VALUE wKatalog.
  if wKatalog = ? then
    DO:
      /* Henter katalog for bilderegisteret fra systemparameter. */
      {syspara.i 10 1 2 wKatalog}
    END.
  if can-do("\,/",substring(wKatalog,length(wKatalog),1)) THEN.
  else
    wKatalog = wKatalog + (if opsys = "unix"
                             then "/"
                             else "\").
  /* Sikrer at katalogen er opprettet. */
  OS-CREATE-DIR VALUE(RIGHT-TRIM(wKatalog,'\')).
END PROCEDURE.

procedure HentBildeArkiv:
  DEF OUTPUT PARAMETER wKatalog as CHAR NO-UNDO.

  /* KAtalognavn kan overstyres i ini-fil.              */
  GET-KEY-VALUE SECTION "SYSPARA" KEY "BilledArkiv" VALUE wKatalog.
  if wKatalog = ? then
    DO:
      /* Henter katalog for bildearkivet fra systemparameter. */
      {syspara.i 10 1 3 wKatalog}
    END.
  if can-do("\,/",substring(wKatalog,length(wKatalog),1)) THEN.
  else
    wKatalog = wKatalog + (if opsys = "unix"
                             then "/"
                             else "\").
END PROCEDURE.

PROCEDURE HentBildePeker:
 DEF INPUT  PARAMETER ipBildNr     as INT  NO-UNDO.
 DEF INPUT  PARAMETER ipMode       as INT  NO-UNDO.
 DEF INPUT PARAMETER  ipBilde      as CHAR NO-UNDO.
 DEF OUTPUT PARAMETER ipBildePeker as CHAR NO-UNDO.

 DEF BUFFER bufBildeData     FOR BildeData.
 DEF BUFFER bufBildeRegister FOR BildeRegister.

 DEF VAR ipBlanktBilde  as CHAR NO-UNDO.
 DEF VAR ipBildeKatalog as CHAR NO-UNDO.
 DEF VAR iTeller        AS INTE NO-UNDO.

 /* Katalog */
 RUN HentBildeKatalog (OUTPUT ipBildeKatalog).

 /*Blankt bilde */
 {syspara.i 10 1 1 ipBlanktBilde}

 IF ipMode = 11 THEN DO: /* Vi sletter bilde på disk, pga ny bilde på artikkel */
     ipMode = 1.
     IF SEARCH(ipBildeKatalog + "mini" + ipBilde) <> ? THEN
         OS-DELETE VALUE(ipBildeKatalog + "mini" + ipBilde).
     IF SEARCH(ipBildeKatalog + ipBilde) <> ? THEN
         OS-DELETE VALUE(ipBildeKatalog + ipBilde).
 END.

 if ipBilde <> "" then
 LESUT:
 DO:
   /* Finnes ikke bildet på disk, legges det ut fra DB hvis det finnes der. */
   if SEARCH(ipBildeKatalog + (IF ipMode = 1 THEN "mini" ELSE "") + ipBilde) = ? then
     LES_UT:
     DO:
       FIND bufBildeRegister EXCLUSIVE-LOCK where
         bufBildeRegister.BildNr = ipBildNr NO-ERROR.
       if NOT AVAILABLE bufBildeRegister then
         LEAVE LES_UT.
       IF ipMode = 1 THEN DO:
           if NOT CAN-FIND(FIRST bufBildeData OF bufBildeRegister WHERE bufBildeData.Teller = 200) then
               ASSIGN ipMode = 3.
           ELSE 
               ASSIGN ipBilde = "mini" + ipBilde
                      iTeller = 200.
       
       END.
       if ipMode = 3 AND NOT CAN-FIND(FIRST bufBildeData OF bufBildeRegister) then
         LEAVE LES_UT.

        OUTPUT STREAM Stream1 TO VALUE(ipBildeKatalog + ipBilde) NO-MAP NO-CONVERT.
        FOR EACH bufBildeData FIELDS(RawData) OF bufBildeRegister WHERE bufBildeData.Teller >= iTeller NO-LOCK:
          PUT STREAM Stream1 CONTROL bufBildeData.RawData.
        END.
        OUTPUT STREAM Stream1 CLOSE.
     END. /* LES_UT */
     ELSE ASSIGN ipBilde = (IF ipMode = 1 THEN "mini" ELSE "") + ipBilde.
 END. /* LESUT */
 ELSE ipMode = 2.
 /* Henter bilde.                            */
 /*Er bilde ukjent, skal blankt bilde vises. */
 if ipMode = 1 OR ipMode = 3 then
   DO:
     /*
     if NUM-ENTRIES(ipBilde,".") <> 2 then
       ipBildePeker = ipBildeKatalog + ipBlanktBilde.
     ELSE if not CAN-DO("bmp,jpg",ENTRY(2,ipBilde,".")) then
       ipBildePeker = ipBildeKatalog + ipBlanktBilde.
     */

     if SEARCH(ipBildeKatalog + ipBilde) = ? then
       ipBildePeker = ipBildeKatalog + ipBlanktBilde.
     else
       ipBildePeker = ipBildeKatalog + ipBilde.

   END.
 /*Blankt bilde skal hentes. */
 else
   ipBildePeker = ipBildeKatalog + ipBlanktBilde.
END PROCEDURE.

/* Rutine som leser inn et bilde fra disk og lagrer dette i databasen. */
PROCEDURE LesInnBilde:
  DEF INPUT  PARAMETER wBildNr  as INT  NO-UNDO.
  DEF INPUT  PARAMETER wFilNavn as CHAR NO-UNDO.
  DEF OUTPUT PARAMETER wOk      as CHAR NO-UNDO.

  DEF VAR wTeller  as INT NO-UNDO.
  DEF VAR wRawData as raw NO-UNDO.

  DEF BUFFER bufBildeData     FOR BildeData.
  DEF BUFFER bufBildeRegister FOR BildeRegister.

  LAST_BILDE:
  DO TRANSACTION:
    FIND bufBildeRegister EXCLUSIVE-LOCK where
      bufBildeRegister.BildNr = wBildNr NO-ERROR.
    if NOT available bufBildeRegister then
      LEAVE LAST_BILDE.

    /* Tar bort gamle data, hvis det finnes ny fil å lese inn. */
    if NOT ENTRY(NUM-ENTRIES(wFilNavn,"\"),wFilNavn,"\") BEGINS "mini" AND search(wFilNavn) <> ? then
      for each bufBildeData of bufBildeRegister:
        delete bufBildeData.
      end.

    INPUT FROM VALUE(wFilNavn) BINARY NO-MAP NO-CONVERT.
    ASSIGN
      wOk              = "AVBRYT"
      wTeller          = IF ENTRY(NUM-ENTRIES(wFilNavn,"\"),wFilNavn,"\") BEGINS "mini" THEN 200 ELSE 0
      LENGTH(wRawData) = 30000.
    REPEAT:
      IMPORT UNFORMATTED wRawData.
      CREATE bufBildeData.
      ASSIGN
        bufBildeData.BildNr    = wBildNr
        bufBildeData.Teller    = wTeller
        bufBildeData.RawData   = wRawData
        bufBildeRegister.Bytes = bufBildeRegister.Bytes  + LENGTH(bufBildeData.RawData,"RAW")
        wTeller                = wTeller + 1.
    END.
    INPUT CLOSE.

    assign
      wOk = "OK".
  END. /* LAST_BILDE TRANSACTION */

END PROCEDURE.

/* Procedure som leser bilde ut på disken. */
PROCEDURE LesUtBilde:
  DEF INPUT  PARAMETER wBildNr  as INT  NO-UNDO.
  DEF INPUT  PARAMETER wFilNavn as CHAR NO-UNDO.
  DEF output parameter wOk      as CHAR NO-UNDO.

  DEF VAR wBytesLagret as INT NO-UNDO.

  define BUFFER bufBildeData     FOR BildeData.
  DEFINE BUFFER bufBildeRegister FOR BildeRegister.

  FIND bufBildeRegister EXCLUSIVE-LOCK where
    bufBildeRegister.BildNr = wBildNr NO-ERROR.
  if available bufBildeRegister then
    DO:
      OUTPUT STREAM Stream1 TO VALUE(wFilNavn) NO-MAP NO-CONVERT.
      FOR EACH bufBildeData FIELDS(RawData) OF bufBildeRegister NO-LOCK:
        PUT STREAM Stream1 CONTROL bufBildeData.RawData.
      END.
      OUTPUT STREAM Stream1 CLOSE.
    END.
END PROCEDURE.

/* Viser bilde p† skjermen */
PROCEDURE VisBilde:


END PROCEDURE.
