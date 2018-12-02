

DEF VAR cKortNr AS CHAR NO-UNDO. 

/*
FOR EACH medlem WHERE EksterntMedlemsNr NE ?  NO-LOCK : 
    DISP medlem.medlemsnr. 

    cKortNr = LEFT-TRIM(REPLACE(TRIM(EksterntMedlemsNr),'M-',''),'0'). 

    FIND FIRST  medlemskort WHERE 
             MedlemsKort.KortNr = cKortNr 
             NO-LOCK .


END.
*/

DEF VAR icnt2 AS INT. 
DEF VAR icnt AS INT NO-UNDO. 
DEF BUFFER bfmedlem FOR medlem. 
DEF VAR newMedlemsnr AS DEC. 

DEF BUFFER bfMedlemskort FOR medlemskort.      

DISABLE TRIGGERS FOR DUMP OF medlem. 
DISABLE TRIGGERS FOR LOAD OF medlem. 
DISABLE TRIGGERS FOR LOAD OF medlemskort. 
DISABLE TRIGGERS FOR DUMP OF medlemskort. 


FOR EACH medlem WHERE etternavn BEGINS "but/kasse:" : 
   icnt = icnt + 1. 

   FIND FIRST  medlemskort WHERE 
               MedlemsKort.medlemsnr = medlem.medlemsnr NO-LOCK NO-ERROR.

   IF AVAIL medlemskort THEN medlem.kilde = "M-000" + MedlemsKort.kortnr.
END.


FOR EACH medlem WHERE etternavn BEGINS "but/kasse:" AND medlem.medlemsnr LE 1000000: 
    icnt = icnt + 1. 

    FIND FIRST medlemskort WHERE medlemskort.medlemsnr = medlem.medlemsnr NO-LOCK NO-ERROR. 
    IF NOT AVAIL medlemskort THEN NEXT. 

    FIND FIRST bfmedlem WHERE bfmedlem.EksterntMedlemsNr = medlem.kilde AND 
        ROWID(bfmedlem) NE ROWID(medlem) NO-ERROR.

    IF AVAIL bfMedlem THEN
    DO:
        newMedlemsNr       = medlem.medlemsnr.
        medlem.medlemsnr   = medlem.medlemsnr * 1000. 
        bfMedlem.medlemsnr = newMedlemsnr. 
        bfMedlem.butik     = Medlem.butik. 
    END. 
END. 


FOR EACH medlemskort : 
    FIND FIRST medlem WHERE medlemskort.medlemsnr = medlem.medlemsnr NO-LOCK NO-ERROR. 
    IF NOT AVAIL medlem THEN
        DELETE medlemskort. 
END.


FOR EACH medlem WHERE etternavn BEGINS "but/kasse:" AND medlem.medlemsnr GT 10000000: 
    icnt = icnt + 1. 

    FIND FIRST medlemskort WHERE medlemskort.medlemsnr = medlem.medlemsnr NO-LOCK NO-ERROR. 
    IF NOT AVAIL medlemskort THEN 
    DO:
        DELETE medlem. 
    END.
END. 


FOR EACH medlem : 
    icnt = icnt + 1. 

    FIND FIRST medlemskort WHERE medlemskort.medlemsnr = medlem.medlemsnr 
          AND medlemskort.kortnr = ?  NO-ERROR. 
    IF  AVAIL medlemskort THEN 
    DO: 
        IF medlem.mobiltlf NE "" AND mobiltlf NE ? THEN
        medlemskort.kortnr = medlem.mobiltlf. 
        ELSE medlemskort.kortnr = STRING( medlemskort.medlemsnr).
    END.
END. 


FOR EACH medlem WHERE EksterntMedlemsNr BEGINS "M-" : 
    icnt = icnt + 1. 

    FIND FIRST medlemskort WHERE medlemskort.medlemsnr = medlem.medlemsnr 
          AND medlemskort.kortnr = LEFT-TRIM(REPLACE(TRIM(EksterntMedlemsNr),'M-',''),'0')  NO-ERROR. 

    IF  AVAIL medlemskort THEN 
    DO: 
        IF medlem.mobiltlf NE "" AND mobiltlf NE ? THEN
        medlemskort.kortnr = medlem.mobiltlf. 
        ELSE medlemskort.kortnr = STRING( medlemskort.medlemsnr).
    END.
END. 


FOR EACH medlem WHERE EksterntMedlemsNr BEGINS "M-" AND butikknr = 0:
    medlem.butikknr = 1. 
    icnt = icnt + 1. 
END. 


DISABLE TRIGGERS FOR DUMP OF skotex.nets. 
DISABLE TRIGGERS FOR LOAD OF skotex.nets. 
FOR EACH skotex.nets : 
     DELETE nets. 
END.
















/*
 FOR EACH medlem WHERE etternavn BEGINS "but/kasse:" : 
    icnt = icnt + 1. 
    
    FIND FIRST  medlemskort WHERE 
                MedlemsKort.medlemsnr = medlem.medlemsnr 
              .

    FIND FIRST bfmedlem WHERE bfmedlem.EksterntMedlemsNr = "m-000" + kortnr  AND 
        rowid(bfmedlem) NE ROWID(medlem) NO-ERROR.
    IF AVAIL bfMedlem THEN
        icnt2 = icnt2 + 1. 

    IF AVAIL bfMedlem THEN
    DO:
    
        newMedlemsNr = medlem.medlemsnr.
        medlem.medlemsnr = medlem.medlemsnr * 1000. 
        FOR EACH  bfmedlemskort WHERE 
             bfMedlemsKort.medlemsnr = bfmedlem.medlemsnr :
              
           DELETE bfMedlemskort. 
        END.

        /*medlemskort.medlemsnr = bfMedlem.medlemsnr.*/ 

        bfMedlem.medlemsnr = newMedlemsnr. 
        DISP bfMedlem.medlemsnr medlem.medlemsnr.  
       
    END. 
    
END.
MESSAGE icnt icnt2 VIEW-AS ALERT-BOX.    */
