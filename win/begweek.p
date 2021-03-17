/************************************************************************************
	PROCEDURE: begweek.p

	PURPOSE:   Calculates first day of the week 

	SYNTAX:    RUN samples/begweek.p (INPUT in, OUTPUT out).

	REMARKS:   This code calculates the date of the FIRST day for a 
		   given week.

	PARAMETERS:
            INPUT:  A week-number (For example 199042)
            OUTPUT: The date of day 1 in input week (10/15/90 using 199042 as input)

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/

/* Assumptions:                                                     */
/* 1. Weeks start on MONDAYS                                        */
/* 2. If January 1st falls on Friday, Saturday, Sunday or Monday    */
/*    then week 1 for this year will start on the first Monday      */
/*    the same year. If not, week 1 will start on the last Monday   */
/*    previous year.                                                */
/*    (In other words: At least 4 of the seven days of week 1 for   */
/*     a given year must fall into this year)                       */


DEFINE INPUT  PARAMETER inweek AS INT.   /* Input week , eg 199042   */
DEFINE OUTPUT PARAMETER fdate  AS DATE.  /* Output date, eg 10/15/90 */

DEFINE VARIABLE yr   AS INT.  /* Year of inweek, eg 1990      */
DEFINE VARIABLE week AS INT.  /* Week of inweek, eg 42        */
DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
			      /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
DEFINE VARIABLE cweek as INT. /* Used to check if week 53 is valid */

ASSIGN
  yr     = INTEGER(SUBSTRING(STRING(inweek , "+999999") , 1 , 5))
  week   = INTEGER(SUBSTRING(STRING(inweek , "+999999") , 6 , 2)).
IF week > 53 THEN DO:
   MESSAGE "Maximum week number is 53".
   RETURN.
END.

IF week = 53 THEN DO:
    RUN weeknum.p ( DATE(12 , 31 , yr) , OUTPUT cweek ).
    IF inweek NE cweek THEN DO:
      MESSAGE "There is no week 53 in year" yr.
      RETURN.
    END.
END.

ASSIGN
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
			    DATE(1, 10, yr) - d1 )
  fdate = dat1 + 7 * 
          (INTEGER(SUBSTRING(STRING(inweek , "999999") , 5 , 2)) - 1).





