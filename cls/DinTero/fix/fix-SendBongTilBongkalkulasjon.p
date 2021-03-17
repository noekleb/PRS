DEF VAR bOk AS LOG NO-UNDO.
DEF VAR ocReturn AS CHAR NO-UNDO.
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEF VAR lB_Id LIKE BongHode.B_Id NO-UNDO.

{cls\dintero\ttPOSBong.i}
{cls\dintero\dsPOSBong.i}

/* B_Id benyttes når vi henter fra bong. Nulles denne, hentes fra fil. */
ASSIGN 
    lB_iD = 2004290000110100106167
    lB_Id = 2004290000110100106168
    lB_Id = 2004290000100100269017 /* Henter fra Bong db. */
    lb_Id = 0 /* Henter fra Fil. */
    .

FOR EACH BongCRMLogg WHERE  
    BongCRMLogg.B_i = lB_Id:
    DELETE BongCRMLogg.
END.

/* Fyller datasettet fra en JSon fil fra disk. */
IF lB_Id = 0 THEN
DO:
    /*DATASET dsPOSBong:READ-JSON('file','cls\DinTero\Test\dsPOSBong000009_00075899.JSon','EMPTY').*/
    /*DATASET dsPOSBong:READ-JSON('file','cls\DinTero\Test\dsPOSBong2102150009990100076027.json','EMPTY').*/
    DATASET dsPOSBong:READ-JSON('file','cls\DinTero\Test\dsPOSBong2102160009990100076039.json','EMPTY').
    
END.
/* Her er kode for å fylle opp dsBong fra BongHode. */
ELSE DO:
    FIND BongHode WHERE
        BongHode.B_Id = Lb_iD NO-ERROR.
    
    CREATE ttPOSBongHode.
    BUFFER-COPY BongHode
        TO ttPOSBongHode.
    FOR EACH BongLinje WHERE 
        BongLinje.B_Id = BongHode.B_Id AND 
        BongLinje.TTId <= 11:
        CREATE ttPOSBongLinje.
        BUFFER-COPY BongLinje
            TO ttPOSBongLinje.
    END.
END.

/* Sender bongen til dintero for kalkulasjon.                               */
/* Ferdig kalkulert bong legges i BongCRMLogg.cPayload som en Json melding. */
RUN cls\dintero\asBongkalkulasjonDintero.p ( INPUT-OUTPUT DATASET dsPOSBong BY-REFERENCE,
                                        OUTPUT bOk, 
                                        OUTPUT ocReturn
                                       ).

/* Viser responskoder fra Dintero. */
MESSAGE bOk ocReturn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                                         
