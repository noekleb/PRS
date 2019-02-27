/*
FIND LAST jboxusermenu.
CURRENT-VALUE(seqjboxusermenuid) = jboxusermenu.ijboxusermenuid.
DISP CURRENT-VALUE(seqjboxusermenuid) jboxusermenu.ijboxusermenuid
*/
DEF VAR ilast AS INT NO-UNDO.
FOR EACH jboxusermenu BY ijboxusermenuid:
    DISP jboxusermenu.ijboxusermenuid.
    ilast = ijboxusermenuid.
END.
CURRENT-VALUE(seqjboxusermenuid) = ilast.
DISP CURRENT-VALUE(seqjboxusermenuid) ilast

