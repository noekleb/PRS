CURRENT-WINDOW:WIDTH = 350.

FOR EACH sendtowebtrans NO-LOCK WHERE 
    /*sendtowebtrans.batchnr > 344000 AND*/
    sendtowebtrans.batchtype = "RETURN" AND 
    sendtowebtrans.extraparam = '500075164' 
    BREAK 
    BY sendtowebtrans.extraparam
    BY sendtowebtrans.sent:
    

    IF LAST-OF(sendtowebtrans.sent) THEN
    DO:
        display
            sendtowebtrans.batchnr 
            sendtowebtrans.batchtype
            sendtowebtrans.Transferstatus
            sendtowebtrans.received  FORMAT "99/99/9999 HH:MM:SS"
            /*sendtowebtrans.payload*/
            /*sendtowebtrans.updated*/
            sendtowebtrans.StatusMessage
            sendtowebtrans.checkcompleted
            sendtowebtrans.retryTrans
            sendtowebtrans.extraparam 
            sendtowebtrans.currentstatus FORMAT "x(30)"
            sendtowebtrans.sent FORMAT "99/99/9999 HH:MM:SS"
            /*ENTRY(1,STRING(sendtowebtrans.sent)) FORMAT "x(40)"*/
        WITH WIDTH 350.
    END.

END.


    /*
    OUTPUT TO c:\tmp\returnerror.txt.
    FOR EACH sendtowebtrans WHERE sendtowebtrans.batchnr > 344000 AND
    sendtowebtrans.batchtype = "RETURN" NO-LOCK.

    EXPORT DELIMITER ";" sendtowebtrans.batchnr sendtowebtrans.extraparam sendtowebtrans.currentstatus ENTRY(1,STRING(sendtowebtrans.sent)).

    END.
    OUTPUT CLOSE.
    */

