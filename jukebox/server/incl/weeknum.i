
PROCEDURE Weeknum:

/************************************************************************************
	PROCEDURE: weeknum.p

	PURPOSE:   Calculates the week-number for a given date

	SYNTAX:    RUN samples/weeknum.p (INPUT in, OUTPUT out).

	REMARKS:   This code calculates the week-number for the date given.
		   The format is YYYYWW

	PARAMETERS:
            INPUT:  date
            OUTPUT: week number

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


DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
DEFINE OUTPUT PARAMETER yyyyww   AS INT.   /* Output week, eg 9042     */

DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
			      /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */

ASSIGN
  yr   = YEAR(indate)
  d1   = WEEKDAY(DATE( 1 , 1 , yr))
  dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
			  DATE(1, 10, yr) - d1 )
  wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
ASSIGN
  yr     = yr - 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
 			    DATE(1, 10, yr) - d1 )
  wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
ASSIGN
  yr     = yr + 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
	      THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.

END PROCEDURE.
