
DEF VAR lPkSdlId AS DEC NO-UNDO.
DEF VAR iFrabut AS INT NO-UNDO.
DEF VAR iBuntNr AS INT NO-UNDO.
DEF VAR iDummy AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR piLinjeNr AS INT NO-UNDO.

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

{overforing.i &NEW = "NEW" &SHARED = "Shared"}
      
SUBSCRIBE 'getPkSdlId' ANYWHERE.

ASSIGN 
    lPkSdlId = 100003 /*163960*/
    iFrabut  = 20
    iBuntNr  = 0
    .
      
FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.

piLinjeNr = 1.
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  
  FIND StrKonv NO-LOCK WHERE 
      StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
  FIND PkSdlPris OF PkSdlHode NO-LOCK WHERE 
       PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  /* Logger overføringstransaksjonen */
  CREATE TT_OvBuffer.
  ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
         TT_OvBuffer.LinjeNr     = piLinjeNr
         TT_OvBuffer.ArtikkelNr  = PkSdlLinje.ArtikkelNr
         TT_OvBuffer.Vg          = ArtBas.Vg   
         TT_OvBuffer.LopNr       = (IF ArtBas.LopNr = ? THEN 0 ELSE ArtBas.LopNr)
         TT_OvBuffer.Antall      = PkSdlLinje.AntLevert
         TT_OvBuffer.Merknad     = "Varemottak pakkseddel"
         TT_OvBuffer.Storl       = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
         TT_OvBuffer.TilStorl    = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
         TT_OvBuffer.Varekost    = PkSdlPris.NyVarekost
         piLinjeNr               = piLinjeNr + 1
         /* Setter datoinfo i registrert dato og tid. */
         TT_OvBuffer.RegistrertDato = TODAY 
         TT_OvBuffer.RegistrertTid  = TIME
         TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
         TT_OvBuffer.ButikkNrFra = iFrabut
         TT_OvBuffer.ButikkNrTil = PkSdlLinje.ButikkNr        
         .
END.

ASSIGN iBuntNr = -2. /* -2 = En overføringsordre pr. bong. Og de markeres som oppdatert. */
RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                   0,
                   "N" + CHR(1) + "Varemottak outlet " + STRING(TODAY) + STRING(TIME,"HH:MM") + CHR(1) + "N",
                   '',
                   '',
                   7).

OPPRETT_TMP:
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
    FIND StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
  
    CREATE tmpOverfor.
    ASSIGN
      tmpOverfor.ArtikkelNr = DEC(PkSdlLinje.ArtikkelNr)
      tmpOverfor.Vg         = ArtBas.Vg
      tmpOverfor.LopNr      = ArtBas.LopNr
      tmpOverfor.FraBut     = iFrabut
      tmpOverfor.TilBut     = PkSdlLinje.ButikkNr
      tmpOverfor.FraStorl   = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
      tmpOverfor.TilStorl   = tmpOverfor.FraStorl 
      tmpOverfor.Antall     = PkSdlLinje.AntLevert
      tmpOverfor.BuntNr     = iBuntNr
      tmpOverfor.OrdreNr    = ''
      tmpOverFor.Rab%       = 0
      tmpOverfor.Kode       = PkSdlLinje.Kode 
       .
END. /* OPPRETT_TMP */

RUN opprettfakturaoverfor.p (OUTPUT iDummy, OUTPUT cTekst).
FIND LAST FakturaHode WHERE 
    FakturaHode.PkSdlNr = PkSdlHode.PkSdlNr AND 
    FakturaHode.Dato = TODAY NO-ERROR.
FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
IF AVAILABLE FakturaHode THEN
    PkSdlHode.FakturaNr = FakturaHode.FakturaNr.
    .
MESSAGE iDummy SKIP
    cTekst SKIP
    PkSdlHode.PkSdlNr SKIP
    PkSdlHode.FakturaNr
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

PROCEDURE getPkSdlId:
    DEF OUTPUT PARAMETER olPkSdlId AS DEC.
    ASSIGN 
        olPkSdlId = lPkSdlId
        .
END PROCEDURE.
