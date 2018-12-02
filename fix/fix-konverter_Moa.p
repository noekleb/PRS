DEF VAR lDec AS DEC FORMAT ">>>>9" NO-UNDO.
DEF VAR cObject AS CHAR FORMAT "x(20)" NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

ASSIGN 
    ibutNr = 12
    .

RUN slettTgForbutikk.
/*RUN korrigerStLinje.*/
/*RUN korrigerBongLinje.*/


/* **********************  Internal Procedures  *********************** */

PROCEDURE slettTgForbutikk:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FOR EACH TGEmp WHERE 
        TGEmp.TGStore_Id = iButNr:
        DELETE TGEmp.
    END.
    FOR EACH TGExport WHERE 
        TGExport.TGStore_Id = iButNr:
        DELETE TGExport.
    END.
    FOR EACH TGSales WHERE 
        TGSales.TGStore_Id = iButNr:
        DELETE TGSales.
    END.
    FOR EACH TGSales_Ext WHERE 
        TGSales_Ext.TGStore_Id = iButNr:
        DELETE TGSales_Ext.
    END.
    FOR EACH TGTimeStamp WHERE 
        TGTimeStamp.TGStore_Id = iButNr:
        DELETE TGTimeStamp.
    END.

END PROCEDURE.


PROCEDURE korrigerBongLinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    FOR EACH BongHode EXCLUSIVE-LOCK WHERE 
        BongHode.ButikkNr = ibutNr AND 
        BongHode.SelgerNr > 0 AND 
        BongHode.SelgerNr <= 999:
         
        FIND LAST ButikkSelger NO-LOCK WHERE 
            ButikkSelger.ButikkNr = BongHode.ButikkNr AND 
            ButikkSelger.SelgerId = BongHode.SelgerNr NO-ERROR.        
        IF AVAILABLE ButikkSelger THEN 
            BongHode.SelgerNr = ButikkSelger.SelgerNr.
/*        DISPLAY                                              */
/*            BongHode.butikkNr                                */
/*            BongHode.Dato                                    */
/*            BongHode.SelgerNr                                */
/*            ButikkSelger.SelgerNr WHEN AVAILABLE ButikkSelger*/
/*            .                                                */
    END.

END PROCEDURE.

PROCEDURE korrigerStLinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    FOR EACH StLinje EXCLUSIVE-LOCK WHERE 
        StLinje.butik = ibutNr AND 
        StLinje.StTypeId BEGINS 'SELGER':
         
        ASSIGN 
            lDec = DEC(LEFT-TRIM(StLinje.DataObjekt)) NO-ERROR 
            .
        IF ERROR-STATUS:ERROR OR lDec > 999 OR lDec = 0 THEN
            NEXT.
    
        cObject = ''.
        FIND LAST ButikkSelger NO-LOCK WHERE 
            ButikkSelger.ButikkNr = StLinje.butik AND
            ButikkSelger.SelgerId = lDec NO-ERROR.
        IF AVAILABLE ButikkSelger THEN
        DO:
            cObject = FILL('0',13 - LENGTH(STRING(ButikkSelger.SelgerNr))) + STRING(ButikkSelger.SelgerNr).
            StLinje.DataObjekt = cObject.
        END.
    
    /*    DISPLAY                                              */
    /*        StLinje.butik                                    */
    /*        StLinje.StTypeId                                 */
    /*        StLinje.PerId                                    */
    /*        StLinje.AarPerlinNr                              */
    /*        StLinje.DataObjekt FORMAT "x(20)"                */
    /*        LENGTH(StLinje.DataObjekt)                       */
    /*        lDec                                             */
    /*        ButikkSelger.SelgerNr WHEN AVAILABLE ButikkSelger*/
    /*        ButikkSelger.SelgerId WHEN AVAILABLE ButikkSelger*/
    /*        cObject                                          */
    /*        LENGTH(cObject)                                  */
    /*                                                         */
    /*    WITH WIDTH 350.                                      */
    END.
END PROCEDURE.

