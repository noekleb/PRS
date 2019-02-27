

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
MESSAGE icnt icnt2 VIEW-AS ALERT-BOX. 
