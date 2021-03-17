/************************************************************
    Program:  x-korrbestilling.p
    Created:  TN   20 Jul 99
Description:

Last change:  TN   20 Jul 99    8:39 am
************************************************************/

def input parameter wBestNr like BestHode.BestNr no-undo.

def var wStrListe as char no-undo.
def var wFordeling as char no-undo.
def var wStorl as char no-undo.
def var wLoop as int no-undo.

def temp-table tStr
  field Storl  as char
  field Antall as int.  

find BestHode no-lock where
  BestHode.BestNr = wBestNr no-error.

/* Tømmer temp-file. */
for each tStr:
  delete tStr.
end.

if available BestHode then  
do TRANSACTION:  
  /* Summerer opp bestillingene pr. størrelse. */
  for each BestStr of BestHode no-lock where
    BestStr.BestStat = 4:
  
    find first tStr where tstr.storl = trim(BestStr.Storl) no-error.
    if not available tStr then
      do:
        create tStr.
        assign
          tStr.Storl = trim(BestSTr.Storl).
      end.
    assign
      tStr.Antall = tStr.Antall + BestStr.Bestilt.
  end.  

  /* Setter SkoTex standard for korrekt sortering. */
  for each tStr:
    run FiksStorl(input-output tStr.Storl).
  end.

  /* Bygger ny størrelsesliste og fodelingsliste. */
  assign 
    wStrListe  = ""
    wFordeling = "".
  for each tStr by tStr.Storl:
    if not can-do(wStrListe,tStr.Storl) then
      assign
        wStrListe = wStrListe +
                    (if wStrListe = "" 
                       then ""
                       else " ") +
                    trim(tStr.Storl)                
        wFordeling = wFordeling +
                    (if wFordeling = "" 
                       then ""
                       else " ") +
                    trim(string(tStr.Antall)).
  end.

  /* Henter pasienten. */
  FIND FIRST BestSort OF BestHode WHERE 
    BestSort.Fri = YES exclusive-LOCK NO-ERROR.
  
  /* Retter opp størrelseslisten. */  
  assign
    BestSort.Fordeling  = wFordeling
    BestSort.Storrelser = wStrListe.
  
  release BestSort.
  
end. /* TRANSACTION */

PROCEDURE fiksstorl:
  def input-output parameter wStorl as char.

 assign
    wStorl = trim(wStorl)
    wStorl = caps(wStorl)
    wStorl = if (length(wStorl) = 1 or 
                 length(wStorl) = 3
                 ) 
                then " " + wStorl
                else wStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.
