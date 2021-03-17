
/*------------------------------------------------------------------------
    File        : asListeNettbutikkordre.p
    Purpose     : Varsle i butikk at ordre er på vei, smat minne dem på å kvittere og varsle kunde omordren.

    Syntax      :

    Description : Lister opp ordre som er sendt fra eCom siste 3 døg til Pick&Collect butikk  

    Author(s)   : Tom Nøkleby
    Created     : Sat Oct 03 08:43:41 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEFINE VARIABLE iButNr AS INTEGER INITIAL 2 NO-UNDO.
  DEFINE VARIABLE iAntHK AS INTEGER NO-UNDO.
  DEFINE VARIABLE iAntBut AS INTEGER NO-UNDO.
&ELSE
  DEFINE INPUT  PARAMETER iButNr AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntHK AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntBut AS INTEGER NO-UNDO.
&ENDIF

{asListeNettbutikkordre.i}

DEFINE OUTPUT PARAMETER TABLE FOR ttKOrdreHode.

DEFINE VARIABLE iAntDager AS INTEGER NO-UNDO.
DEFINE VARIABLE cJSonFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
EMPTY TEMP-TABLE ttKOrdreHode.

ASSIGN 
  iAntDager = 3
  cJSonFil  = 'konv\casListeNettbutikkordre' + STRING(iButNr) + '_' + REPLACE(STRING(TODAY),'/','') + '.JSon'
  bTest     = TRUE 
  .

LESORDRE:
FOR EACH KOrdreHode NO-LOCK WHERE 
    KOrdreHode.Butik = iButNr AND   
    KOrdreHode.LevStatus <= '50' AND 
    DATE(KOrdreHode.DatoTidOpprettet) >= TODAY - iAntDager:
      
  IF KOrdreHode.LevStatus <= '50' THEN 
    iantHK = iAntHK + 1.
  ELSE 
    iAntbut = iAntBut + 1.
    
  CREATE ttKOrdreHode.
  ASSIGN     
    ttKOrdreHode.KOrdre_Id        = KOrdreHode.KOrdre_Id          
    ttKOrdreHode.Butik            = KOrdreHode.Butik           
    ttKOrdreHode.LevFNr           = KOrdreHode.LevFNr          
    ttKOrdreHode.LevStatus        = KOrdreHode.LevStatus       
    ttKOrdreHode.EkstOrdreNr      = KOrdreHode.EkstOrdreNr     
    ttKOrdreHode.Navn             = KOrdreHode.Navn            
    ttKOrdreHode.MobilTlf         = KOrdreHode.MobilTlf        
    ttKOrdreHode.ePostAdresse     = KOrdreHode.ePostAdresse    
    ttKOrdreHode.DatoTidOpprettet = KOrdreHode.DatoTidOpprettet
    ttKOrdreHode.ShipmentSendt    = KOrdreHode.ShipmentSendt   
    .
END. /* LESORDRE */

IF bTest THEN 
  TEMP-TABLE ttKOrdrEHode:WRITE-JSON('file', cJSonFil, TRUE).
  
RETURN.  
  
  


    