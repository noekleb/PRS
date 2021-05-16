DEF VAR cPkSdlIdLst AS CHAR NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR lPkSdlId AS DEC FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEF VAR lFaktura_Id LIKE FakturaHode.Faktura_id NO-UNDO.
DEF VAR iDummy AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.
DEF VAR iFrabut AS INT NO-UNDO.
DEF VAR iBuntNr AS INT NO-UNDO.
DEF VAR cFakturafilNavn AS CHAR NO-UNDO.

DEFINE BUFFER bufArtBas  FOR ArtBas.
DEFINE BUFFER bufArtPris FOR ArtPris.
DEFINE BUFFER bufButiker FOR Butiker.
DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.

CURRENT-WINDOW:WIDTH = 350.
FORM 
WITH FRAME A DOWN.
FORM
WITH FRAME B DOWN.

{overforing.i &NEW=NEW &SHARED="Shared"}

SUBSCRIBE TO "getPkSdlId" ANYWHERE.
SUBSCRIBE TO "putFakturaId" ANYWHERE.
SUBSCRIBE TO "getPkSdlNr" ANYWHERE.
SUBSCRIBE TO 'fakturaFilNavn' ANYWHERE.

ASSIGN 
    iBuntNr = 0
    iFrabut = 20
    /*cPkSdlIdLst = '164046,164049,156306,156307,155530,169323,169352,169320,169329,169351,169349,169321,168271,164048'*/
    cPkSdlIdLst = '164046'
    /* tn 20/4-21 rEGENERERT FAKTURA for Illums. */
    cPkSdlIdLst = 
       '79927,' +
       '79710,' +
       '78318,' +
       '78314,' +
       '78320,' +
       '78939,' +
       '78930,' +
       '79002,' +
       '79653,' +
       '79651,' +
       '79652,' +
       '79703,' +
       '80045,' +
       '81083,' +
       '78316,' +
       '78311,' +
       '81087,' +
       '81086' 
    .

DO  iLoop = 1 TO NUM-ENTRIES(cPkSdlIdLst):
    lPkSdlId = DEC(ENTRY(iLoop,cPkSdlIdLst)).

    FIND PkSdlHode EXCLUSIVE-LOCK WHERE 
        PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
    RUN BareFakturer.

    DISPLAY
        lPkSdlId
        PkSdlHode.PkSdlId
        PkSdlHode.PkSdlNr
        PkSdlHode.FakturaNr
        PkSdlHode.PkSdlOpphav
        PkSdlHode.OrdreType
    WITH FRAME A WIDTH 350 DOWN.
    DOWN WITH FRAME A.
END.

PROCEDURE BareFakturer:
  /* for å kunne opprette faktura. */
  RUN oppretttmpOverfor.
  
  lFaktura_Id = 0.
  /* Her opprettes faktura, og den sendes på eMail hvis det er satt opp eMail sending. */
  RUN opprettfakturaoverfor.p (OUTPUT iDummy, OUTPUT cTekst).
  /* Skriver ut faktura i butikk som mottar varene. Fikk faktura_id via putFakturaId subscribe. */
  IF lFaktura_Id > 0 AND
    CAN-FIND(FakturaHode NO-LOCK WHERE
    FakturaHode.Faktura_Id = lFaktura_Id) THEN
  DO:

    RUN faktura_fakturaskriver.p (STRING(PkSdlHode.ButikkNr) + "|1|",
      ?,
      "",
      OUTPUT cTekst,
      OUTPUT obOk).

    IF NUM-ENTRIES(cTekst,'|') >= 5 THEN
    DO:
      RUN skrivfaktura.p (STRING(lFaktura_Id) + "|",ENTRY(1,cTekst,"|"),ENTRY(2,cTekst,"|"),ENTRY(3,cTekst,"|"),ENTRY(4,cTekst,"|"),ENTRY(5,cTekst,"|")).
    END.
  END.

END PROCEDURE.

PROCEDURE oppretttmpOverfor:
  EMPTY TEMP-TABLE tmpOverfor.
  
  /* Legger pakkseddel linjene opp i en temp-tabell. */
  OPPRETT_TMP:
  FOR EACH bufPkSdlLinje OF PkSdlHode NO-LOCK:
    FIND PkSdlPris OF PkSdlHode NO-LOCK WHERE 
      PkSdlPris.ArtikkelNr = bufPkSdlLinje.ArtikkelNr NO-ERROR.
    
    FIND StrKonv NO-LOCK WHERE 
      StrKonv.StrKode = bufPkSdlLinje.StrKode NO-ERROR.
      
    FIND bufArtBas NO-LOCK WHERE 
      bufArtBas.ArtikkelNr = bufPkSdlLinje.ArtikkelNr NO-ERROR.
        
    /* For å kunne opprette faktura. */    
    CREATE tmpOverfor.
    ASSIGN
      tmpOverfor.ArtikkelNr = DEC(bufPkSdlLinje.ArtikkelNr)
      tmpOverfor.Vg         = bufArtBas.Vg
      tmpOverfor.LopNr      = bufArtBas.LopNr
      tmpOverfor.FraBut     = iFrabut
      tmpOverfor.TilBut     = bufPkSdlLinje.ButikkNr
      tmpOverfor.FraStorl   = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
      tmpOverfor.TilStorl   = tmpOverfor.FraStorl 
      tmpOverfor.Antall     = bufPkSdlLinje.AntLevert
      tmpOverfor.BuntNr     = iBuntNr
      tmpOverfor.OrdreNr    = ''
      tmpOverFor.Rab%       = 0 /* Skal alltid være 0. Prisen hentes senere fra varekost - etter rabatter. */
      tmpOverfor.Kode       = bufPkSdlLinje.Kode 
      .
  END. /* OPPRETT_TMP */

END PROCEDURE.

PROCEDURE getPkSdlId:
    DEF OUTPUT PARAMETER plPkSdlId AS DEC NO-UNDO.

    plPkSdlId = PkSdlHode.PkSdlId.
END.

PROCEDURE putFakturaId:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER plFaktura_Id LIKE FakturaHode.Faktura_Id NO-UNDO.
    
  ASSIGN 
    lFaktura_Id = plFaktura_Id
    .

END PROCEDURE.

PROCEDURE getPkSdlNr:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER cPkSdlNr AS CHARACTER NO-UNDO.

  IF AVAILABLE PkSdlHode THEN 
    cPkSdlNr = PkSdlHode.PkSdlNr.
        
END PROCEDURE.

PROCEDURE FakturafilNavn:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcFakturafilNavn AS CHARACTER NO-UNDO.

  ASSIGN 
    cFakturafilNavn = pcFakturafilNavn
    .

END PROCEDURE.
