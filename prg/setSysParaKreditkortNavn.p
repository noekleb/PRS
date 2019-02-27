/* setSysParaKreditkortNavn.p*/

  DEF VAR piLoop    AS INT NO-UNDO.
  DEF VAR cKortNavn AS CHAR NO-UNDO.

  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.
  DEF BUFFER bSysHode   FOR SysHode.

  IF cKortNavn = "" THEN
      ASSIGN
      cKortNavn = 
/* 01 */ "Bankkort (BankAxept)" + "," +
/* 02 */ "Postbanken (BankAxept)" + "," +
/* 03 */ "Visa" + "," +
/* 04 */ "MasterCard" + "," +
/* 05 */ "AmEx" + "," +
/* 06 */ "Diners" + "," +
/* 07 */ "Ukjent kort 07" + "," +
/* 08 */ "Ukjent kort 08" + "," +
/* 09 */ "GE Capital (BankAxept)" + "," +
/* 10 */ "Ukjent kort 10" + "," +
/* 11 */ "JCB" + "," +
/* 12 */ "trumf" + "," +
/* 13 */ "Ukjent kort 13" + "," +
/* 14 */ "Maestro" + "," +
/* 15 */ "Lindex" + "," +
/* 16 */ "Ikano" + "," +
/* 17 */ "Ukjent kort 17" + "," +
/* 18 */ "Proton" + "," +
/* 19 */ "Ukjent kort 19" + "," +
/* 20 */ "Coop (BankAxept)" + "," +
/* 21 */ "Ukjent kort 21" + "," +
/* 22 */ "Gavekort Senter" + "," +
/* 23 */ "Gavekort Kjede" + "," +
/* 24 */ "Esso Mastercard" + "," +
/* 25 */ "Sentrumskortet" + "," +
/* 26 */ "Mastercard Purchase" + "," +
/* 27 */ "XPonCard kortsystem" + "," +
/* 28 */ "Multicard" + "," +
/* 29 */ "Universal Presentkort" + "," +
/* 30 */ "BAX Smartkort" + "," +
/* 31 */ "Ukjent kort 31" + "," +
/* 32 */ "Resurs Bank" + "," +
/* 34 */ "NG Bedriftskort" + "," +
/* 35 */ "BBS Sentergavekort" + "," +
/* 36 */ "BBS Kjedegavekort" + "," +
/* 37 */ "EMV e-Purse" + "," +
/* 38 */ "NETS ( Visa/MC;Bankkort, Swedbank credit/debit selection)" + "," +
/* 39 */ "NETS ( Visa/MC:Bankkort, All except swedbank/nordea)" + "," +
/* 40 */ "NETS ( Visa/MCNordea:MC, Visa)" + "," +
/* 41 */ "NETS ( Visa/MC)" + "," +
/* 42 */ "Nokaskortet" + "," +
/* 43 */ "S&S Medlemskort" + "," +
/* 44 */ "Posten-kortet" + "," +
/* 45 */ "Nordea Finans (Profilerad)" + "," +
/* 46 */ "Handelsbanken Finans (Profilerad)" + "," +
/* 47 */ "Swedbank (Profilerad)" + "," +
/* 48 */ "SEB (Profilerad)" + "," +
/* 49 */ "Resursbank (Profilerad)" + "," +
/* 50 */ "Dankort" + "," +
/* 51 */ "COOP Visa" + "," +
/* 52 */ "Payex Gavekort" + "," +
/* 53 */ "VØT Lojalitetskort" + "," +
/* 54 */ "Cresco / MC" + "," +
/* 55 */ "Trumf Visa" + "," +
/* 56 */ "Gavekort 1" + "," +
/* 57 */ "Visa Danmark" + "," +
/* 58 */ "Mastercard Danmark" + "," +
/* 59 */ "Maestro Danmark" + "," +
/* 60 */ "Diners Danmark" + "," +
/* 61 */ "Amex Danmark" + "," +
/* 62 */ "CashcomsPresentkort" + "," +
/* 63 */ "Storcash Bedriftkort" + "," +
/* 64 */ "Gavekort – PBS" + "," +
/* 65 */ "Forbrugsforeningen DK" + "," +
/* 66 */ "Ikano Finans DK" + "," +
/* 67 */ "Oberthur Gavekort DK" + "," +
/* 68 */ "Ukjent kort 68" + "," +
/* 69 */ "Ukjent kort 69" + "," +
/* 70 */ "Ukjent kort 70" + "," +
/* 71 */ "Ukjent kort 71" + "," +
/* 72 */ "Ukjent kort 72" + "," +
/* 73 */ "Ukjent kort 73" + "," +
/* 74 */ "Ukjent kort 74" + "," +
/* 75 */ "Ukjent kort 75" + "," +
/* 76 */ "Ukjent kort 76" + "," +
/* 77 */ "Ukjent kort 77" + "," +
/* 78 */ "Ukjent kort 78" + "," +
/* 79 */ "Ukjent kort 79" + "," +
/* 80 */ "Ukjent kort 80" + "," +
/* 81 */ "Ukjent kort 81" + "," +
/* 82 */ "Ukjent kort 82" + "," +
/* 83 */ "Ukjent kort 83" + "," +
/* 84 */ "Ukjent kort 84" + "," +
/* 85 */ "Ukjent kort 85" + "," +
/* 86 */ "Ukjent kort 86" + "," +
/* 87 */ "Ukjent kort 87" + "," +
/* 88 */ "Ukjent kort 88" + "," +
/* 89 */ "Ukjent kort 89" + "," +
/* 90 */ "Ukjent kort 90" + "," +
/* 91 */ "Ukjent kort 91" + "," +
/* 92 */ "Ukjent kort 92" + "," +
/* 93 */ "Ukjent kort 93" + "," +
/* 94 */ "Ukjent kort 94" + "," +
/* 95 */ "Ukjent kort 95" + "," +
/* 96 */ "Ukjent kort 96" + "," +
/* 97 */ "Ukjent kort 97" + "," +
/* 98 */ "Ukjent kort 98" + "," +
/* 99 */ "Ukjent kort 99"
.                 

  IF NOT CAN-FIND(SysHode WHERE
                  SysHode.SysHId = 20) THEN
  DO TRANSACTION:
      CREATE bSysHode.
      ASSIGN
          bSysHode.SysHId      = 20
          bSysHode.Beskrivelse = "Oppsett av bokføringsbilag"
          .
      RELEASE bSysHode.
  END.

  DO piLoop = 3 TO 3:
      IF NOT CAN-FIND(SysGruppe WHERE
          SysGruppe.SysHId = 20 AND
          SysGruppe.SysGr  = piLoop) THEN
      DO FOR bSysGruppe TRANSACTION:
          CREATE bSysGruppe.
          ASSIGN
              bSysGruppe.SysHId = 20
              bSysGruppe.SysGr  = piLoop
              Beskrivelse       = "Bank og kreditkort"
              .
          RELEASE bSysGruppe.
      END. /* bSysGruppe TRANSACTION */
  END.

  DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 99:
        FIND bSysPara WHERE
            bSysPara.SysHId = 20 AND
            bSysPara.SysGr  = 3 AND
            bSysPara.ParaNr = piLoop NO-ERROR.
        IF NOT AVAILABLE bSysPara THEN
        OPPRETT:
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 20 
                bSysPara.SysGr        = 3 
                bSysPara.ParaNr       = piLoop
                .
        END. /* OPPRETT */

        ASSIGN
            bSysPara.Parameter1   = ENTRY(piLoop,cKortNavn)
            bSysPara.Beskrivelse  = ENTRY(piLoop,cKortNavn) 
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END. /* LOOP */
  END. /* bSysPara TRANSACTION*/

