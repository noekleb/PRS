/************************************************************
    Program:  genlib.i.I
    Created:  TN   21 Jun 98
Description:  Felles håndtering av
              - Oppstart av prosedurebiblotek
              - Innmelding i programliste
              - Utmelding av programliste ved stopp av program
              - Stopp av prosedurebibliotek
              - Hvis prodedyrebiblioteket må startes i
                definisjonsblokken, send med
                    &NoLibCall  = "No"
                Da trekkes ikke funlib.i inn her.


Last change:  TN    5 Nov 99   11:12 am
************************************************************/
/* Starter procedurebibloteket. */
&IF "{&NoLibCall}" = "" &THEN {runlib.i} &ELSE &ENDIF

{inutmld.i &Modus = "Opprett"} /* Melder fra at programmet har startet. */
{winpos.i &WindowName = "{&WindowName} "} /* Space kreves av kompilator. */

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
    {&PreIClose}
    {inutmld.i &Modus = "Slett"} /* Melder fra at programmet har stopper. */
    {savepos.i &WindowName = "{&WindowName} "}  /* Lagrer winduwposisjonen     */
    {stopplib.i} /* Stopper prosedurebibloteket */
    {&PostIClose}
    RUN disable_UI.
    {&PostDisable_ui}

    PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  end.

