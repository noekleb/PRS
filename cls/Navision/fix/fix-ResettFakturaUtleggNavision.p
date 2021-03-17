                                                                                                                        DEFINE VARIABLE cButNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButLstTillat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButLstKommisjon AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutletListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEksterneLev AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcLevNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INT NO-UNDO.
DEFINE VARIABLE cFakturaLst AS CHAR NO-UNDO.
DEFINE VARIABLE i2Loop AS INT NO-UNDO.
DEF VAR lFakturaNr AS DEC NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
  
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( ).

{syspara.i 50 55 14 cButLstTillat}
{syspara.i 22  5  2 cOutletListe}
{syspara.i 50 55 15 cEksterneLev} /*'99,501,502,504'*/

/* Henter liste med kommisjonsbutikker. */    
rStandardFunksjoner:getKommisjonsButLst(OUTPUT cButLstKommisjon).    

FORM 
  FakturaHode.ButikkNr
  FakturaHode.FakturaNr
  FakturaHode.FakturertDato
  FakturaHode.SendingsNr     
  FakturaHode.EksportertDato 
  FakturaHode.EksportertAv   
  Kunde.KundeNr
  Kunde.Navn
  Kunde.ButikkNr
WITH FRAME A WIDTH 350 DOWN.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    cButNrLst = cButLstTillat + ',' + 
                cOutletListe + ',' + 
                cButLstKommisjon 
/*    cFakturaLst = '16105565,' +*/
/*                  '16103755,' +*/
/*                  '16104500,' +*/
/*                  '16104747,' +*/
/*                  '16105399,' +*/
/*                  '16105402,' +*/
/*                  '16105475,' +*/
/*                  '16106005,' +*/
/*                  '2100496,' + */
/*                  '2100497,' + */
/*                  '2100498'    */
/*    cFakturaLst =    */
/*        '2100433,' + */
/*        '9100020,' + */
/*        '9100021,' + */
/*        '9100018,' + */
/*        '9100019,' + */
/*        '2100434,' + */
/*        '9100022,' + */
/*        '16101868,' +*/
/*        '16101848,' +*/
/*        '9100023,' + */
/*        '16101863,' +*/
/*        '3100210,' + */
/*        '2100437,' + */
/*        '15102272,' +*/
/*        '2100436,' + */
/*        '3100211,' + */
/*        '13100017,' +*/
/*        '3100209,' + */
/*        '19100008,' +*/
/*        '16102031,' +*/
/*        '9100024,' + */
/*        '9100025,' + */
/*        '6100399,' + */
/*        '15102410,' +*/
/*        '13100018,' +*/
/*        '15102454,' +*/
/*        '15102409,' +*/
/*        '15102455,' +*/
/*        '9100026,' + */
/*        '2100451,' + */
/*        '2100444,' + */
/*        '17100009,' +*/
/*        '2100448,' + */
/*        '2100445,' + */
/*        '2100442,' + */
/*        '2100450,' + */
/*        '2100440,' + */
/*        '2100441,' + */
/*        '2100443,' + */
/*        '2100446,' + */
/*        '2100447,' + */
/*        '2100449,' + */
/*        '2100462,' + */
/*        '9100027,' + */
/*        '2100461,' + */
/*        '2100459,' + */
/*        '2100456,' + */
/*        '12100011,' +*/
/*        '2100458,' + */
/*        '2100455,' + */
/*        '16102342,' +*/
/*        '2100452,' + */
/*        '2100457,' + */
/*        '2100454,' + */
/*        '2100453,' + */
/*        '2100463,' + */
/*        '2100464,' + */
/*        '13100019,' +*/
/*        '13100020,' +*/
/*        '2100465,' + */
/*        '2100472,' + */
/*        '16102779,' +*/
/*        '13100021,' +*/
/*        '2100470,' + */
/*        '2100467,' + */
/*        '3100212,' + */
/*        '2100466,' + */
/*        '3100213,' + */
/*        '2100471,' + */
/*        '2100469,' + */
/*        '2100468,' + */
/*        '16102719,' +*/
/*        '9100029,' + */
/*        '2100473,' + */
/*        '15103479,' +*/
/*        '2100474,' + */
/*        '15103645,' +*/
/*        '6100405,' + */
/*        '6100402,' + */
/*        '6100404,' + */
/*        '6100401,' + */
/*        '15103714,' +*/
/*        '15103716,' +*/
/*        '6100406,' + */
/*        '6100403,' + */
/*        '6100408,' + */
/*        '15104064,' +*/
/*        '17100013,' +*/
/*        '2100476,' + */
/*        '6100409,' + */
/*        '6100407,' + */
/*        '6100413,' + */
/*        '15103894,' +*/
/*        '3100215,' + */
/*        '15104232,' +*/
/*        '2100478,' + */
/*        '15104205,' +*/
/*        '13100024,' +*/
/*        '3100214,' + */
/*        '15104548,' +*/
/*        '3100216,' + */
/*        '15104512,' +*/
/*        '16104098,' +*/
/*        '6100416,' + */
/*        '6100418,' + */
/*        '16104213,' +*/
/*        '6100417,' + */
/*        '6100424,' + */
/*        '6100419,' + */
/*        '6100423,' + */
/*        '6100420,' + */
/*        '9100032,' + */
/*        '15105340,' +*/
/*        '15105481,' +*/
/*        '6100426,' + */
/*        '15105556,' +*/
/*        '15105554,' +*/
/*        '15105555,' +*/
/*        '11100029,' +*/
/*        '11100036,' +*/
/*        '11100027,' +*/
/*        '11100031,' +*/
/*        '11100028,' +*/
/*        '11100030,' +*/
/*        '11100033,' +*/
/*        '11100032,' +*/
/*        '11100035,' +*/
/*        '11100034,' +*/
/*        '11100038,' +*/
/*        '18100007,' +*/
/*        '15106016,' +*/
/*        '9100033,' + */
/*        '11100037,' +*/
/*        '9100034,' + */
/*        '15106356,' +*/
/*        '15106357,' +*/
/*        '11100040,' +*/
/*        '15106355,' +*/
/*        '11100041,' +*/
/*        '11100039,' +*/
/*        '15106539,' +*/
/*        '2100492,' + */
/*        '18100011,' +*/
/*        '15106540,' +*/
/*        '18100010,' +*/
/*        '15106554,' +*/
/*        '2100494,' + */
/*        '2100493,' + */
/*        '18100009,' +*/
/*        '15106557,' +*/
/*        '15106537,' +*/
/*        '18100008,' +*/
/*        '15106538,' +*/
/*        '2100501,' + */
/*        '15106631,' +*/
/*        '3100227,' + */
/*        '15106630,' +*/
/*        '2100500,' + */
/*        '15106773,' +*/
/*        '17100016,' +*/
/*        '18100012,' +*/
/*        '13100026'   */
/*    .                */
    cFakturaLst = ''. /* Alle faktura. */

cButLst = ''.
FOR EACH Butiker NO-LOCK WHERE 
  Butiker.harButikksystem = TRUE AND 
  Butiker.NedlagtDato = ?:

  /* Ikke ta med +/- butikkene. */
  IF CAN-DO('848,849',STRING(Butiker.Butik)) THEN 
    NEXT.
  
  IF NOT CAN-DO(cButNrLst,STRING(Butiker.Butik)) THEN 
    cButLst = cButLst + 
              (IF cButLst <> '' THEN ',' ELSE '') + 
              STRING(Butiker.Butik).  
END.
IF cButLst <> '' THEN 
  cButNrLst = cButNrLst + ',' + cButLst. 

MESSAGE 'cButLst:' cButLst SKIP
'cButNrLst:' cButNrLst 
VIEW-AS ALERT-BOX.

MESSAGE 'cFakturaLst:' cFakturaLst 
VIEW-AS ALERT-BOX.


LOOPBLOKK:
DO i2Loop = 1 TO (IF NUM-ENTRIES(cFakturaLst) > 1 THEN NUM-ENTRIES(cFakturaLst) ELSE 1):
    lFakturaNr = DEC(ENTRY(i2Loop,cFakturaLst)).
    FAKTURALOOP:
    FOR EACH FakturaHode EXCLUSIVE-LOCK WHERE 
      (IF lFakturaNr = 0 THEN TRUE ELSE FakturaHode.FakturaNr = lFakturaNr)
      /*
      FakturaHode.FakturertDato >= 02/19/2021 AND
      FakturaHode.FakturertDato <= 02/21/2021 AND
      FakturaHode.EksportertDato <> ?
      */
      :
    
      FIND kunde OF FakturaHode NO-LOCK NO-ERROR.
    
      /* Bare faktura utstedt til butikker i en av listene skal eksporteres. */
      IF NOT CAN-DO(cButNrLst,STRING(Kunde.butikkNr)) THEN 
        NEXT.
    
      /* Outlet faktura skal bare med hvis de inneholder varer fra eksterne leverandører. */
      IF CAN-DO(cOutletListe,STRING(Kunde.ButikkNr)) THEN
        EKSTERNLEV: 
        DO:
          /* Lister opp de leverandørene som har levert varene på fakturaen. */
          FOR EACH FakturaLinje OF FakturaHode NO-LOCK, 
            FIRST ArtBas NO-LOCK WHERE ArtBas.ArtikkelNr = FakturaLinje.ArtikkelNr:
    
            IF NOT CAN-DO(pcLevNrLst,STRING(ArtBas.LevNr)) THEN 
              pcLevNrLst = pcLevNrLst + 
                           (IF pcLevNrLst = '' THEN '' ELSE ',') + 
                           STRING(ArtBas.LevNr).
          END. 
          
          /* Skipper faktura som ikke har varer fra eksterne leverandører. */
          IF pcLevNrLst <> '' THEN
          DO:
            LEVSJEKK: 
            DO iLoop = 1 TO NUM-ENTRIES(cEksterneLev):
              IF LOOKUP(ENTRY(iLoop,cEksterneLev),pcLevNrLst) > 0 THEN 
                LEAVE EKSTERNLEV. /* Denne skal vi ha med. */
            END. /* LEVSJEKK */
            NEXT FAKTURALOOP. /* Faktura skal ikke med. */
          END.
          ELSE 
            NEXT FAKTURALOOP. /* Faktura skal ikke med. */
        END. /* EKSTERNLEV */

      DISPLAY
          FakturaHode.ButikkNr
          FakturaHode.FakturaNr
          FakturaHode.FakturertDato
          FakturaHode.SendingsNr     
          FakturaHode.EksportertDato 
          FakturaHode.EksportertAv   
          Kunde.KundeNr
          Kunde.Navn
          Kunde.ButikkNr
      WITH FRAME A WIDTH 350 DOWN.
      DOWN 1 WITH FRAME A.
      PAUSE 0 BEFORE-HIDE.
      
    
        
      ASSIGN 
        FakturaHode.SendingsNr     = ''
        FakturaHode.EksportertDato = ?
        FakturaHode.EksportertAv   = ''
        .
        
    END. /* FAKTURALOOP */
END. /* LOOPBLOKK */


