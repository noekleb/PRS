/************************************************************
    Program:  keyfunk.p
    Created:  TN   15 Jun 98
Description:  Redefinering av tangentbordet.

Last change:  TN   20 Oct 100   11:09 am
************************************************************/

/* Definerer funksjonstaster for Utvikling */
on ctrl-F1  HELP.
on ctrl-F2  HELP.
on Ctrl-F3  HELP.
on Ctrl-F4  HELP.
on Ctrl-F5  HELP.
on Ctrl-F6  HELP.
on Ctrl-F7  HELP.
on Ctrl-F8  HELP.
on Ctrl-F9  HELP.
on Ctrl-F10 HELP.
on Ctrl-F11 HELP.
on Ctrl-F12 HELP.
ON Ctrl-P   HELP. 
DO:
    RETURN.
END.

/* Redefinerer taster for at gamle SkoTex programmer skal virke */
/*
on F1       GO.
on f2       HELP.
on pf2      help.
*/
on F4       END-ERROR.
on ESC      END-ERROR.
on F7       RECALL.
on F8       CLEAR.
on F9       NEW-LINE.
/*on F10      DELETE-LINE.*/
on F12      help.
on F11      help.

