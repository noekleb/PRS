DEFINE INPUT PARAMETER iMemberNo AS INT NO-UNDO. 
DEFINE INPUT PARAMETER lChecked  AS LOGICAL NO-UNDO. 
DEFINE OUTPUT PARAMETER lok AS LOGICAL INIT FALSE NO-UNDO. 

FIND FIRST medlem WHERE medlem.medlemsnr = iMemberno NO-ERROR. 

IF AVAIL medlem THEN
DO :
    medlem.Bonus_Berettiget = lChecked. 
    lok = TRUE. 
END.


