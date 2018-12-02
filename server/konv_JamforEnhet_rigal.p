/*

*/

DEFINE INPUT-OUTPUT PARAMETER cJamforEnhet AS CHARACTER NO-UNDO.

CASE cJamforEnhet:
    WHEN  '1' THEN cJamforEnhet = 'stk'.
    WHEN  '2' THEN cJamforEnhet = 'kg'.
    WHEN  '3' THEN cJamforEnhet = 'l'.
    WHEN  '4' THEN cJamforEnhet = 'm'.
    WHEN  '5' THEN cJamforEnhet = 'm3'.
    WHEN  '6' THEN cJamforEnhet = 'm2'.
    WHEN  '7' THEN cJamforEnhet = '100m'.
    WHEN  '8' THEN cJamforEnhet = 'dos'.
    WHEN  '9' THEN cJamforEnhet = 'pors'.
    WHEN '10' THEN cJamforEnhet = 'tabl'.
    WHEN '11' THEN cJamforEnhet = 'beh'.
    WHEN '12' THEN cJamforEnhet = 'vask'.
    WHEN '13' THEN cJamforEnhet = 'par'.
    WHEN '14' THEN cJamforEnhet = 'hg'.
    OTHERWISE      cJamforEnhet = 'stk'.
END CASE.
