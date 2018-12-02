/*------------------------------------------------------------------------
  Forfatter:   Sturla Johnsen, 27.09.99
  System:      BA
  Beskrivelse: Henter, setter, lagrer ... innstillinger for en browse
  Endringer:
	Last change:  SJ   30 Sep 1999    3:05 pm
------------------------------------------------------------------------*/

DEF VAR wBrowse_StdSettings{2}  AS CHAR   NO-UNDO.

/* Vanllig initiering.  */
&IF ("{2}" = "")
&THEN
   DEF VAR wBrowse_firstproc    AS HANDLE NO-UNDO.
   DEF VAR wBrowse_Count        AS INTE   NO-UNDO.
   ASSIGN wBrowse_firstproc = /* SESSION:FIRST-PROCEDURE. */ wLibHandle.
   PROCEDURE SaveBrowseSettings:
      DEF VAR wCount AS INTE NO-UNDO.
      &IF NOT DEFINED(UIB_IS_RUNNING) <> 0 &THEN
         DO wCount = 0 TO 6:
            RUN VALUE("SaveBrowseOnExit0" + TRIM(STRING(wCount,"zzz"))) IN THIS-PROCEDURE NO-ERROR.
         END.
      &ENDIF
   END PROCEDURE.
&ENDIF

&IF ("{3}" <> "NOTINIT")
&THEN
   RUN InitBrowseSettings IN wBrowse_firstproc (BROWSE {1}:HANDLE,THIS-PROCEDURE,0{2}).
   ASSIGN wBrowse_StdSettings{2} = RETURN-VALUE.
&ENDIF

PROCEDURE StdBrowseSettings0{2}:
  RUN StdBrowseSettings IN wBrowse_firstproc (BROWSE {1}:HANDLE,wBrowse_StdSettings{2},THIS-PROCEDURE).
END PROCEDURE.
PROCEDURE SaveBrowseOnExit0{2}:
   RUN SaveBrowseSettings IN wBrowse_firstproc (BROWSE {1}:HANDLE,wBrowse_StdSettings{2},THIS-PROCEDURE).
END.

