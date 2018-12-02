/* Avstemmingsrapport, messeordre mot ordre
   Parametere: Input: Varebehnr
               Output (ocReturn): Filnavn
   
   Opprettet: 01.09.06 av BHa               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fVarebehNr      AS DEC  NO-UNDO.
DEF VAR cFileName       AS CHAR NO-UNDO.
def var isumforh        as int  no-undo.
def var isumbest        as int  no-undo.
def var edatosort       as date no-undo.
def var iantsort        as int  no-undo.
DEF VAR cOrdreList      AS CHAR NO-UNDO.
def var iBestStat       as int  no-undo.
DEF VAR dForsteLev      AS DATE NO-UNDO.
DEF VAR cButikkListe    AS CHAR NO-UNDO.

DEF BUFFER bBeststr FOR BestStr.

ASSIGN fVarebehNr   = DEC(ENTRY(1,icParam))
       cButikkListe = REPLACE(ENTRY(2,icParam),"|",",")
       NO-ERROR.


FIND VarebehHode NO-LOCK
     WHERE VarebehHode.VarebehNr = fVarebehNr
     NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Finner ikke messebok " + icParam.
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + "Avstem_" + STRING(VarebehHode.VarebehNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED VarebehHode.VareBehBeskrivelse + "~t~t~tAvstemmingsrapport mot ordre"
                SKIP.
PUT UNFORMATTED 
                "Artikkelnr"             + "~t" 
                "Lev.nr"                 + "~t"
                "Kjedevare"              + "~t"
                "Butikknr"               + "~t"
                "Ant forh"               + "~t"
                "Ant best"               + "~t"
                "Best.stat"              + "~t"
                "Forh oppr"              + "~t"
                "Forh endr"              + "~t" 
                "Endr av"                + "~t" 
                "Første lev"             + "~t"
                "Ordrenr"                        
                SKIP.

for each varebehlinjetrans no-lock
    where varebehlinjetrans.varebehnr = fVarebehNr
      AND CAN-DO(cButikkListe,STRING(varebehlinjetrans.butikknr)) 
      and varebehlinjetrans.godkjent  = yes
      and (bestilt1 > 0 or bestilt2 > 0 or bestilt3 > 0 or bestilt4 > 0)
    ,first varebehlinje of varebehlinjetrans no-lock
    ,FIRST varebehlinjeThode OF varebehlinjeTRans NO-LOCK
           WHERE VarebehLinjeTHode.Godkjent = YES
    ,first artbas of varebehlinje no-lock
           WHERE artbas.Kjedevare OR artbas.Gjennomfaktureres
     break by varebehlinjetrans.artikkelnr
           by varebehlinjetrans.butikknr
      :
   
  assign iantsort = 0
         edatosort = 01/01/1900.
  
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
      ,FIRST LevSort OF ArtSort NO-LOCK:
  
    FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
      iantsort = iantsort + levsant.soant.
      if levsant.edato > edatosort then
        edatosort = levsant.edato.
    END.
  END.
  
  isumforh = isumforh + (bestilt1 + bestilt2 + bestilt3 + bestilt4) * max(1,iantsort).
      
  if last-of(varebehlinjetrans.butikknr) then do:
    ASSIGN isumbest     = 0
           cOrdreList   = ""
           iBestStat    = 0
           dForsteLev   = 01/01/2100
           .
  
  FOR EACH besthode NO-LOCK
       WHERE besthode.varebehnr  = fvarebehnr
         AND besthode.artikkelnr = varebehlinjetrans.artikkelnr
       ,each beststr no-lock
             OF besthode
             where beststr.butik = varebehlinjetrans.butikknr
            :
  
 
    if not can-find(first bbeststr no-lock
                    where bbeststr.bestnr = beststr.bestnr
                      and bbeststr.butik  = beststr.butik
                      and bbeststr.storl  = beststr.storl
                      and bbeststr.beststat > beststr.beststat) then 
      isumbest = isumbest + beststr.bestilt.
 

    IF BestHode.BestStat > iBestStat THEN iBestStat = BestHode.BestStat.

    IF BestHode.LevDato < dForsteLev THEN dForsteLev = BestHode.LevDato.
     
    IF NOT CAN-DO(cOrdreList,STRING(besthode.ordrenr)) THEN
      cOrdreList = cOrdreList + STRING(besthode.ordrenr) + ",".
  END.
  if isumforh ne isumbest then  
    export delimiter "~t" 
           varebehlinjetrans.artikkelnr
           varebehlinje.levnr
           artbas.kjedevare
           varebehlinjetrans.butikknr
           isumforh
           isumbest
           iBestStat
           varebehlinjetrans.registrertdato
           varebehlinjetrans.edato
           varebehlinjetrans.brukerid
           dForsteLev
           cOrdreList 
          .

   isumforh = 0.
 end.   
end.   

OUTPUT CLOSE.
         
IF ocReturn = "" THEN
  ASSIGN obOk = YES
         ocReturn = cFileName.
