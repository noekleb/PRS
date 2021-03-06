TRIGGER PROCEDURE FOR WRITE OF Bokforingsbilag OLD BUFFER oldBokforingsbilag.
    
DEFINE VARIABLE bStatTilHK  AS LOG       NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE piBokforingsNr AS INTEGER NO-UNDO.

DEFINE BUFFER trgBokforingsbilag FOR BokforingsBilag.

/* Skal det sendes artikkelstatistikk? */
{syspara.i 3 4 1 cTekst}
IF (cTekst = "1" OR cTekst = "") THEN
    bStatTilHK = TRUE.
ELSE
    bStatTilHK = FALSE.

{trg\c_w_trg.i &Fil=SkoTex.Bokforingsbilag &TYPE=W}

IF Bokforingsbilag.BokforingsID = ? AND BokforingsBilag.RegistrertDato <> ? THEN
DO:
  ASSIGN 
    Bokforingsbilag.BokforingsID = DEC(
                                       STRING(BokforingsBilag.ButikkNr,">>>>>9") + 
                                       STRING(BokforingsBilag.Aar,"9999") + 
                                       STRING(BokforingsBilag.BokforingsNr,"999999") 
                                      ) 
    .
END. 

IF oldBokforingsbilag.GodkjentFlagg = FALSE AND
    Bokforingsbilag.GodkjentFlagg = TRUE THEN
    ASSIGN
    Bokforingsbilag.GodkjentDato = TODAY
    Bokforingsbilag.godkjentTid  = TIME
    Bokforingsbilag.GodkjentAv   = USERID("SkoTex")
    .
ELSE
    ASSIGN
        Bokforingsbilag.GodkjentDato = ?
        Bokforingsbilag.GodkjentTid  = 0
        Bokforingsbilag.GodkjentAv   = ""
        .

IF oldBokforingsbilag.SendtRegnskap = FALSE AND
    Bokforingsbilag.Sendtregnskap = TRUE THEN
    ASSIGN
    Bokforingsbilag.SendtDato = TODAY
    Bokforingsbilag.SendtTid  = TIME
    Bokforingsbilag.SendAv    = USERID("SkoTex")
    .
ELSE
    ASSIGN
        Bokforingsbilag.SendtDato = ?
        Bokforingsbilag.SendtTid  = 0
        Bokforingsbilag.SendAv    = ""
        .

IF Bokforingsbilag.EODMottatt = TRUE AND 
   Bokforingsbilag.EODDato    = ? 
   THEN  
      Bokforingsbilag.EODDato = TODAY.

IF bStatTilHK THEN 
LOGGSTAT:
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Bokforingsbilag" AND
         ELogg.EksterntSystem = "HK"    AND
         ELogg.Verdier        = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Bokforingsbilag"
               ELogg.EksterntSystem = "HK"   
               ELogg.Verdier        = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.

/* ERP system */
IF oldBokforingsbilag.GodkjentFlagg = FALSE AND
   Bokforingsbilag.GodkjentFlagg = TRUE  THEN
DO:
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "Bokforingsbilag" AND
         ELogg.EksterntSystem = "ERP"    AND
         ELogg.Verdier        = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "Bokforingsbilag"
               ELogg.EksterntSystem = "ERP"   
               ELogg.Verdier        = STRING(BokforingsBilag.ButikkNr) + CHR(1) + STRING(BokforingsBilag.Aar) + CHR(1) + STRING(BokforingsBilag.BokforingsNr).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
END.



