TRIGGER PROCEDURE FOR WRITE OF BongHode.

DEFINE VARIABLE trgLoggBong     AS LOG     NO-UNDO.
DEFINE VARIABLE trgCRMTilkoblet AS INTEGER NO-UNDO.

{syspara.i 14 200 1 trgCRMTilkoblet INT}

ASSIGN
  trgLoggBong         = FALSE
  Data.BongHode.EDato = TODAY
  Data.BongHode.ETid  = TIME
  Data.BongHode.EAv   = USERID("Skotex").

IF trgCRMTilkoblet = 1 THEN
CRMBLOKK: 
DO:
  /* Logger bonger for sending til webCRM. */
  LESSYSPARA:
  FOR EACH SysPara NO-LOCK WHERE 
    SysPara.SysHId = 14 AND 
    SysPara.SysGr  = 200:
    IF SysPara.Parameter1 = '1' THEN 
    DO:
      trgLoggBong = TRUE.
      LEAVE LESSYSPARA.
    END.   
  END. /* LESSYSPARA */  

  IF trgLoggBong THEN 
  DO:
    /* Logger for sending av fil til Webside for initiering */
    BONG_TIL_WEB:
    DO:
      FIND ELogg WHERE 
        ELogg.TabellNavn     = "BongHode" AND
        ELogg.EksterntSystem = "WEBINIT"    AND
        ELogg.Verdier        = STRING(BongHode.B_Id) NO-ERROR.
      IF NOT AVAILABLE Elogg THEN 
      DO:
        CREATE Elogg.
        ASSIGN 
          ELogg.TabellNavn     = "Bonghode"
          ELogg.EksterntSystem = "WEBINIT"   
          ELogg.Verdier        = STRING(BongHode.B_Id).
      END.
      ASSIGN 
        ELogg.EndringsType = 1
        ELogg.Behandlet    = FALSE.
    END. /* BONG_TIL_WEB */
  END. 
END. /* CRMBLOKK */
