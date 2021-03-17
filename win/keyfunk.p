/************************************************************
    Program:  keyfunk.p
    Created:  TN   15 Jun 98
Description:  Redefinering av tangentbordet.

Last change:  TN   20 Oct 100   11:09 am
************************************************************/

/* Definerer funksjonstaster for Utvikling */
ON ctrl-F1  HELP.
ON ctrl-F2  HELP.
ON Ctrl-F3  HELP.
ON Ctrl-F4  HELP.
ON Ctrl-F5  HELP.
ON Ctrl-F6  HELP.
ON Ctrl-F7  HELP.
ON Ctrl-F8  HELP.
ON Ctrl-F9  HELP.
ON Ctrl-F10 HELP.
ON Ctrl-F11 HELP.
ON Ctrl-F12 HELP.
/*ON Ctrl-P   HELP.*/
DO:
    RETURN.
END.

/* Redefinerer taster for at gamle SkoTex programmer skal virke */
/*
on F1       GO.
on f2       HELP.
on pf2      help.
*/
ON F4       END-ERROR.
ON ESC      END-ERROR.
ON F7       RECALL.
ON F8       CLEAR.
ON F9       NEW-LINE.
/*on F10      DELETE-LINE.*/
ON F12      help.
ON F11      help.

