       IDENTIFICATION DIVISION.                                         00034300
       PROGRAM-ID. CW12DEMO.                                            00034409
      ******************************************************************00035000
      * PROGETTO        : SIP - DBSTART                                *00040000
      * OGGETTO         : ROUTINE CONTROLLO VALIDITA' CODICE FISCALE   *00050000
      * DATA CREAZIONE  : 13/01/1993                                   *00060000
      * ULTIMA MODIFICA : ../../....                                   *00070000
      ******************************************************************00080000
      *                                                                 00090000
      *                                                                 00100000
      *================================================================*00110000
      * ACCETTA IN INPUT IL CODICE FISCALE SUL QUALE ESEGUE I CONTROLLI*00120000
      * DI VALIDITA' E RESTITUISCE IL CODICE DI RITORNO CON L' ESITO   *00130000
      * DELL' OPERAZIONE.                                              *00140000
      *================================================================*00150000
      *                                                                *00160000
      * COPY DI LAVORO ===> IETC012                                   * 00170004
      *                                                                *00180000
      * AREA DI TRANSITO 'AREA-IET012CT' COSTITUITA DA:                *00190003
      *                                                                *00200000
      * - LL-IET012CT  = LUNGHEZZA AREA DI TRANSITO (FISSA)            *00210003
      *                                                                *00220000
      * - CF-IET012CT  = CODICE FISCALE.                               *00230003
      *                                                                *00240000
      * - RC-IET012CT  = CODICE DI RITORNO.                            *00250003
      *----------------------------------------------------------------*00260000
      * DECODIFICA DI RC-IET012CT:                                     *00270003
      *                                                                *00280000
      * - SPACES       = OPERAZIONE CORRETTAMENTE ESEGUITA             *00290000
      * - 'E1'         = CODICE FISCALE NON SIGNIFICATIVO              *00300000
      * - 'E2'         = CODICE FISCALE ERRATO                         *00310000
      *================================================================*00320000
       ENVIRONMENT DIVISION.                                            00330000
       CONFIGURATION SECTION.                                           00340000
       DATA DIVISION.                                                   00350000
      ******************************************************************00360000
       WORKING-STORAGE SECTION.                                         00370000
      ******************************************************************00380000
      *                                                                 00390000
       COPY  CWC012.                                                    00400012
      *                                                                 00410000
       01  CAMPO-COMODO            PIC 9(3).                            00420000
                                                                        00430000
       01  TAB-CODFIS.                                                  00440000
           02 EL-CODFIS            PIC X  OCCURS 16.                    00450000
                                                                        00460000
       01  TAB-CODFIS-NUM.                                              00470000
           02 EL-CODFIS-NUM        PIC 9  OCCURS 11.                    00480000
                                                                        00490000
       01  CAMPO-36.                                                    00500000
           02 FILLER               PIC X(36)                            00510000
              VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'.             00520000
       01  TAB-36 REDEFINES CAMPO-36.                                   00530000
           02 EL-CARATTERE         PIC X OCCURS 36.                     00540000
                                                                        00550000
       01  CAMPO-36-02.                                                 00560000
           02 FILLER               PIC X(32)                            00570000
              VALUE '00010203040506070809101112131415'.                 00580000
           02 FILLER               PIC X(32)                            00590000
              VALUE '16171819202122232425000102030405'.                 00600000
           02 FILLER               PIC X(8)                             00610000
              VALUE '06070809'.                                         00620000
       01  TAB-36-02 REDEFINES CAMPO-36-02.                             00630000
           02 EL-NUMERO-PAR        PIC 99    OCCURS 36.                 00640000
                                                                        00650000
       01  CAMPO-36-03.                                                 00660000
           02 FILLER               PIC X(32)                            00670000
              VALUE '01000507091315171921020418201103'.                 00680000
           02 FILLER               PIC X(32)                            00690000
              VALUE '06081214161022252423010005070913'.                 00700000
           02 FILLER               PIC X(8)                             00710000
              VALUE '15171921'.                                         00720000
       01  TAB-36-03 REDEFINES CAMPO-36-03.                             00730000
           02 EL-NUMERO-DIS        PIC 99    OCCURS 36.                 00740000
                                                                        00750000
       01  COMODO-CD               PIC X        VALUE SPACES.           00760000
       01  IND-CF                  PIC S9(4) COMP VALUE ZERO.           00770000
       01  IND-CAR                 PIC S9(4) COMP VALUE ZERO.           00780000
       01  CTR1                    PIC S9(4) COMP VALUE ZERO.           00790000
       01  RESTO-CF                PIC 99  VALUE ZERO.                  00800000
       01  NUMERO-CONTROLLO        PIC 9  VALUE ZERO.                   00810000
       01  CONT                    PIC 99.                              00820000
       01  DECINE-UNITA  REDEFINES  CONT.                               00830000
           02 DECINE               PIC 9.                               00840000
           02 UNITA                PIC 9.                               00850000
      *                                                                 00860000
       01   ALFA                         PIC X(26)                      00870000
            VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.                         00880000
       01   TAB-ALFA   REDEFINES ALFA.                                  00890000
            02  ELE-ALFA    OCCURS 26    PIC X.                         00900000
      *                                                                 00910000
       01  INDICI.                                                      00920000
            05 IND-ALFA                PIC 9(02) VALUE ZERO.            00930000
            05 IND-ALFA2               PIC 9(02) VALUE ZERO.            00940000
      *                                                                 00950000
       01   FLAG-ALFA                  PIC 9     VALUE ZERO.            00960000
      *                                                                 00970000
       01   CODFI.                                                      00980000
           02  CODFI1.                                                  00990000
               03  CODFI11.                                             01000000
                   04  ELE-CODFI11               PIC X OCCURS 6.        01010000
               03  CODFI12                       PIC X(02).             01020000
               03  CODFI13                       PIC X.                 01030000
               03  CODFI14                       PIC X(02).             01040000
               03  NUM-CODFI14 REDEFINES CODFI14 PIC 99.                01050000
           02  CODFI1-R REDEFINES CODFI1.                               01060000
               03  CODFI1-RA                     PIC 9(10).             01070000
               03  CODFI1-RB                     PIC 9(01).             01080000
           02  CODFI2.                                                  01090000
               03  CODFI21                       PIC X.                 01100000
               03  CODFI22                       PIC X(03).             01110000
               03  CODFI23                       PIC X.                 01120000
           02  CODFI2-R REDEFINES CODFI2         PIC 9(05).             01130000
       01   CODFI-R REDEFINES CODFI.                                    01140000
           02  CODFI-R1                          PIC 9(05).             01150000
           02  CODFI-R2                          PIC 9(11).             01160000
           02  CODFI-R20 REDEFINES CODFI-R2.                            01170000
               03  CODFI-R21                     PIC 9(10).             01180000
               03  CODFI-R22                     PIC 9(01).             01190000
       01  CODFI111 REDEFINES CODFI.                                    01200000
           02  CODFI111-A                        PIC X(05).             01210000
           02  CODFI111-B                        PIC X(11).             01220000
       01  APPO.                                                        01230000
           02  APPO-1                            PIC X(05).             01240000
           02  APPO-2                            PIC X(11).             01250000
      *----------------*                                                01260000
      ******************************************************************01270000
       LINKAGE SECTION.                                                 01280000
      ******************************************************************01290000
      *                                                                 01300000
CICS  *01  DFHCOMMAREA               PIC X(20).                         01310000
      *                                                                 01311000
CICS   01  DFHCOMMAREA.                                                 01312000
CICS       05  FILLER  PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.   01313000
      *                                                                 01314000
      *                                                                 01320000
       01  BLLCELLS.                                                    01330000
           03  F                     PIC S9(8) COMP.                    01340000
      *                                                                 01350000
      ******************************************************************01360000
       PROCEDURE DIVISION.                                              01370000
      ******************************************************************01380000
      *----------------------------------------------------------------*01390000
      *----------------------------------------------------------------*01400000
      *                                                                 01410000
       INIZIO-IET012CT.                                                 01420003
      *                                                                 01430000
              MOVE  DFHCOMMAREA    TO  AREA-IET012CT.                   01440003
      *                                                                 01450000
              MOVE  SPACES         TO  RC-IET012CT                      01461005
                                       COMODO-CD.                       01470000
              MOVE ZERO            TO  IND-CF                           01480000
                                       IND-CAR                          01490000
                                       CTR1                             01500000
                                       RESTO-CF                         01510000
                                       NUMERO-CONTROLLO                 01520000
                                       CONT.                            01530000
      *                                                                 01540000
              IF  CF-IET012CT  =  SPACES OR LOW-VALUE                   01550003
                  MOVE  'E1'       TO    RC-IET012CT                    01560003
                  GO TO FINE-IET012CT.                                  01570003
      *                                                                 01580000
              MOVE CF-IET012CT     TO CODFI.                            01590003
      *                                                                 01600000
      *-------------------------------------------------------*         01610000
      * CONTROLLO SE CODICE FISCALE NUMERICO (PERSONE GIURID. *         01620000
      * O ALFANUMERICO (PERSONE FISICHE O PERSONE GIURIDICHE  *         01630000
      * RECUPERABILI)                                         *         01640000
      *-------------------------------------------------------*         01650000
                                                                        01660000
            IF CODFI IS NUMERIC                                         01670000
               PERFORM CODFI-NUM THRU EX-CODFI-NUM                      01680000
            ELSE                                                        01690000
               PERFORM CODFI-ALF THRU EX-CODFI-ALF.                     01700000
      *                                                                 01710000
       FINE-IET012CT.                                                   01720003
      *                                                                 01730000
              MOVE  AREA-IET012CT    TO    DFHCOMMAREA.                 01740003
      *                                                                 01750000
              EXEC  CICS  RETURN                                        01760005
                          END-EXEC.                                     01762005
      *                                                                 01770000
      *----------------------------------------------------------------*01780000
      *----------------------------------------------------------------*01790000
                                                                        01800000
      *----------*                                                      01810000
       CODFI-NUM.                                                       01820000
                                                                        01830000
      *-------------------------------------------*                     01840000
      * CONTROLLO SE CODICE FISCALE SIGNIFICATIVO *                     01850000
      *-------------------------------------------*                     01860000
                                                                        01870000
            IF CODFI = '0000000000000000'                               01880000
               MOVE 'E1' TO RC-IET012CT                                 01890003
               GO TO EX-CODFI-NUM                                       01900000
            ELSE                                                        01910000
               NEXT SENTENCE.                                           01920000
                                                                        01930000
      *------------------------------------------------------*          01940000
      * CONTROLLO PER CODICI FISCALI PERSONE GIURIDICHE O.K. *          01950000
      *------------------------------------------------------*          01960000
                                                                        01970000
            IF CODFI-R1  = 0  AND                                       01980000
               CODFI-R21 > 0                                            01990000
               PERFORM CHECK-PG THRU EX-CHECK-PG                        02000000
                  GO TO EX-CODFI-NUM                                    02010000
            ELSE                                                        02020000
               NEXT SENTENCE.                                           02030000
                                                                        02040000
      *--------------------------------------------------*              02050000
      * CONTROLLO PER CODICI FISCALI PERSONE GIURIDICHE  *              02060000
      * RECUPERABILI (CODICI CON CINQUE ZERI CONSECUTIVI *              02070000
      * IN CODA)                                         *              02080000
      *--------------------------------------------------*              02090000
                                                                        02100000
            IF CODFI1-RA > 0  AND                                       02110000
               CODFI2-R  = 0                                            02120000
               MOVE CODFI1-R TO APPO-2                                  02130000
               MOVE ZEROES   TO APPO-1                                  02140000
               MOVE APPO     TO CODFI                                   02150000
               PERFORM ELAB-RECUP THRU EX-ELAB-RECUP                    02160000
            ELSE                                                        02170000
               MOVE 'E2' TO RC-IET012CT.                                02180003
                                                                        02190000
       EX-CODFI-NUM.                                                    02200000
           EXIT.                                                        02210000
                                                                        02220000
      *----------*                                                      02230000
       CODFI-ALF.                                                       02240000
                                                                        02250000
      *---------------------------------------------------*             02260000
      * CONTROLLI PER CODICI FISCALI PERSONE GIURIDICHE   *             02270000
      * RECUPERABILI (CODICI CON CINQUE SPAZI CONSECUTIVI *             02280000
      * IN TESTA O IN CODA)                               *             02290000
      *---------------------------------------------------*             02300000
                                                                        02310000
            IF CODFI1 IS NOT NUMERIC                                    02320000
               NEXT SENTENCE                                            02330000
            ELSE                                                        02340000
               IF CODFI1-RA > 0  AND                                    02350000
                  CODFI2    = SPACES                                    02360000
                  MOVE CODFI1-R TO APPO-2                               02370000
                  MOVE ZEROES   TO APPO-1                               02380000
                  MOVE APPO     TO CODFI                                02390000
                  PERFORM ELAB-RECUP THRU EX-ELAB-RECUP                 02400000
                  GO TO EX-CODFI-ALF.                                   02410000
      *                                                                 02420000
            IF CODFI111-B IS NOT NUMERIC                                02430000
               NEXT SENTENCE                                            02440000
            ELSE                                                        02450000
               IF CODFI111-A = SPACES AND                               02460000
                  CODFI-R21  > 0                                        02470000
                  MOVE ZEROES   TO CODFI-R1                             02480000
                  PERFORM ELAB-RECUP THRU EX-ELAB-RECUP                 02490000
                  GO TO EX-CODFI-ALF.                                   02500000
                                                                        02510000
      *--------------------------------------------------*              02520000
      * CONTROLLO PER CODICI FISCALI PERSONE FISICHE     *              02530000
      *--------------------------------------------------*              02540000
                                                                        02550000
            IF CODFI2 NOT = SPACES AND                                  02560000
               CODFI11 IS ALPHABETIC AND                                02570000
               CODFI11 NOT = SPACES                                     02580000
               PERFORM ELAB-FISICO THRU EX-ELAB-FISICO                  02590000
            ELSE                                                        02600000
               MOVE 'E2' TO RC-IET012CT.                                02610003
                                                                        02620000
       EX-CODFI-ALF.                                                    02630000
           EXIT.                                                        02640000
                                                                        02650000
      *------------*                                                    02660000
       ELAB-FISICO.                                                     02670000
                                                                        02680000
      *--------------------------------------------------*              02690000
      * CONTROLLI FORMALI SULLE SINGOLE PARTI COMPONENTI *              02700000
      * UN C.FISC. PER PERSONE FISICHE PRIMA DEL C.DIGIT *              02710000
      *--------------------------------------------------*              02720000
                                                                        02730000
           MOVE 1            TO IND-ALFA.                               02740000
           MOVE 0            TO IND-ALFA2.                              02750000
           MOVE 0            TO FLAG-ALFA.                              02760000
           MOVE SPACES TO RC-IET012CT.                                  02770003
                                                                        02780000
           PERFORM CONTR-ALFA  THRU EX-CONTR-ALFA                       02790000
                UNTIL IND-ALFA > 6.                                     02800000
                                                                        02810000
           IF FLAG-ALFA  = 1                                            02820000
           OR CODFI12 IS NOT NUMERIC                                    02830000
           OR CODFI13 IS NOT ALPHABETIC                                 02840000
           OR CODFI13 = SPACES                                          02850000
              MOVE 'E2' TO RC-IET012CT                                  02860003
              GO TO EX-ELAB-FISICO.                                     02870000
                                                                        02880000
           IF CODFI13 NOT = 'A' AND                                     02890000
              CODFI13 NOT = 'B' AND                                     02900000
              CODFI13 NOT = 'C' AND                                     02910000
              CODFI13 NOT = 'D' AND                                     02920000
              CODFI13 NOT = 'E' AND                                     02930000
              CODFI13 NOT = 'H' AND                                     02940000
              CODFI13 NOT = 'L' AND                                     02950000
              CODFI13 NOT = 'M' AND                                     02960000
              CODFI13 NOT = 'P' AND                                     02970000
              CODFI13 NOT = 'R' AND                                     02980000
              CODFI13 NOT = 'S' AND                                     02990000
              CODFI13 NOT = 'T'                                         03000000
              MOVE 'E2' TO RC-IET012CT                                  03010003
              GO TO EX-ELAB-FISICO.                                     03020000
                                                                        03030000
           IF CODFI14 IS NOT NUMERIC                                    03040000
              MOVE 'E2' TO RC-IET012CT                                  03050003
              GO TO EX-ELAB-FISICO.                                     03060000
                                                                        03070000
           IF NUM-CODFI14  > 71  OR                                     03080000
              NUM-CODFI14  = 0                                          03090000
              MOVE 'E2' TO RC-IET012CT                                  03100003
              GO TO EX-ELAB-FISICO.                                     03110000
                                                                        03120000
           IF CODFI21 IS NOT ALPHABETIC                                 03130000
           OR CODFI21 = SPACES                                          03140000
           OR CODFI22 IS NOT NUMERIC                                    03150000
           OR CODFI22 = '000'                                           03160000
           OR CODFI23 IS NOT ALPHABETIC                                 03170000
           OR CODFI23 = SPACES                                          03180000
              MOVE 'E2' TO RC-IET012CT                                  03190003
              GO TO EX-ELAB-FISICO.                                     03200000
                                                                        03210000
           PERFORM CHECK-PF     THRU EX-CHECK-PF.                       03220000
                                                                        03230000
       EX-ELAB-FISICO.                                                  03240000
            EXIT.                                                       03250000
                                                                        03260000
      *-----------*                                                     03270000
       CONTR-ALFA.                                                      03280000
                                                                        03290000
      *----------------------------------------------------*            03300000
      * ROUTINE PER CONTROLLO ALFABETICITA' PRIMI 6 CAR.   *            03310000
      *----------------------------------------------------*            03320000
                                                                        03330000
           ADD    1           TO    IND-ALFA2.                          03340000
           IF IND-ALFA2 > 26                                            03350000
              MOVE 1     TO  FLAG-ALFA                                  03360000
              MOVE 7     TO  IND-ALFA                                   03370000
              GO TO EX-CONTR-ALFA.                                      03380000
           IF ELE-CODFI11(IND-ALFA) =  ELE-ALFA(IND-ALFA2)              03390000
              ADD   1    TO  IND-ALFA                                   03400000
              MOVE  0    TO  IND-ALFA2.                                 03410000
                                                                        03420000
       EX-CONTR-ALFA.                                                   03430000
            EXIT.                                                       03440000
                                                                        03450000
      *-----------*                                                     03460000
       ELAB-RECUP.                                                      03470000
                                                                        03480000
      *----------------------------------------------------*            03490000
      * ROUTINE PER VERIFICA RECUPERABILITA' C.F.GIURIDICO *            03500000
      *----------------------------------------------------*            03510000
                                                                        03520000
           PERFORM CHECK-PG THRU EX-CHECK-PG.                           03530000
                                                                        03540000
       EX-ELAB-RECUP.                                                   03550000
            EXIT.                                                       03560000
                                                                        03570000
      *---------*                                                       03580000
       CHECK-PF.                                                        03590000
                                                                        03600000
      *----------------------------------------------------*            03610000
      * ROUTINE PER CONTROLLO CHECK DIGIT PERSONE FISICHE  *            03620000
      *----------------------------------------------------*            03630000
                                                                        03640000
           MOVE ZEROES TO CTR1.                                         03650000
           MOVE CODFI          TO TAB-CODFIS.                           03660000
           MOVE 1 TO IND-CF.                                            03670000
           PERFORM CONTROLLO-PF THRU EX-CONTROLLO-PF                    03680000
                   UNTIL IND-CF > 15.                                   03690000
      *                                                                 03700000
           IF RC-IET012CT NOT = SPACES                                  03710003
              GO TO EX-CHECK-PF.                                        03720000
      *                                                                 03730000
           DIVIDE CTR1 BY 26 GIVING CAMPO-COMODO                        03740000
           REMAINDER RESTO-CF.                                          03750000
           ADD 1 TO RESTO-CF.                                           03760000
           IF EL-CODFIS(16) = EL-CARATTERE(RESTO-CF)                    03770000
              MOVE SPACES TO RC-IET012CT                                03780003
           ELSE                                                         03790000
              MOVE EL-CARATTERE(RESTO-CF) TO COMODO-CD                  03800000
              MOVE 'E2' TO RC-IET012CT.                                 03810003
                                                                        03820000
       EX-CHECK-PF.                                                     03830000
           EXIT.                                                        03840000
                                                                        03850000
      *-------------*                                                   03860000
       CONTROLLO-PF.                                                    03870000
                                                                        03880000
           PERFORM GIRO-IND-CAR THRU EX-GIRO-IND-CAR                    03890000
                   VARYING IND-CAR FROM 1 BY 1                          03900000
                   UNTIL IND-CAR > 36         OR                        03910000
                   EL-CODFIS(IND-CF) = EL-CARATTERE(IND-CAR).           03920000
                                                                        03930000
           IF IND-CAR > 36                                              03940000
              MOVE 'E2' TO RC-IET012CT                                  03950003
              MOVE 16 TO IND-CF                                         03960000
              GO TO EX-CONTROLLO-PF.                                    03970000
                                                                        03980000
           DIVIDE IND-CF BY 2 GIVING CAMPO-COMODO                       03990000
           REMAINDER RESTO-CF.                                          04000000
           IF RESTO-CF = ZERO                                           04010000
              ADD EL-NUMERO-PAR(IND-CAR) TO CTR1                        04020000
           ELSE                                                         04030000
              ADD EL-NUMERO-DIS(IND-CAR) TO CTR1.                       04040000
                                                                        04050000
           ADD 1 TO IND-CF.                                             04060000
                                                                        04070000
       EX-CONTROLLO-PF.                                                 04080000
           EXIT.                                                        04090000
                                                                        04100000
       GIRO-IND-CAR.                                                    04110000
                                                                        04120000
      *---------------*                                                 04130000
      * ROUTINE VUOTA *                                                 04140000
      *---------------*                                                 04150000
                                                                        04160000
       EX-GIRO-IND-CAR.                                                 04170000
           EXIT.                                                        04180000
                                                                        04190000
      *---------*                                                       04200000
       CHECK-PG.                                                        04210000
                                                                        04220000
      *------------------------------------------*                      04230000
      * CONTROLLO CHECK DIGIT PERSONE GIURIDICHE *                      04240000
      *------------------------------------------*                      04250000
                                                                        04260000
           MOVE CODFI-R2       TO TAB-CODFIS-NUM.                       04270000
           MOVE 1  TO  IND-CF.                                          04280000
           PERFORM GIRO-IND-CF    THRU EX-GIRO-IND-CF                   04290000
                   UNTIL IND-CF   >  10.                                04300000
      *                                                                 04310000
           MOVE CTR1 TO  CONT.                                          04320000
           COMPUTE CONT  =  10 - UNITA.                                 04330000
           MOVE UNITA TO NUMERO-CONTROLLO.                              04340000
           IF NUMERO-CONTROLLO = EL-CODFIS-NUM(IND-CF)                  04350000
              MOVE SPACES      TO  RC-IET012CT                          04360003
           ELSE                                                         04370000
              MOVE NUMERO-CONTROLLO  TO COMODO-CD                       04380000
              MOVE 'E2'        TO  RC-IET012CT.                         04390003
                                                                        04400000
       EX-CHECK-PG.                                                     04410000
           EXIT.                                                        04420000
                                                                        04430000
      *------------*                                                    04440000
       GIRO-IND-CF.                                                     04450000
                                                                        04460000
           DIVIDE IND-CF BY 2 GIVING CAMPO-COMODO                       04470000
           REMAINDER RESTO-CF.                                          04480000
           IF RESTO-CF NOT = ZERO                                       04490000
              ADD EL-CODFIS-NUM(IND-CF) TO CTR1                         04500000
           ELSE                                                         04510000
              COMPUTE CONT = EL-CODFIS-NUM(IND-CF) * 2                  04520000
              COMPUTE CTR1 = CTR1 + DECINE + UNITA.                     04530000
           ADD 1 TO IND-CF.                                             04540000
                                                                        04550000
       EX-GIRO-IND-CF.                                                  04560000
           EXIT.                                                        04570000
                                                                        04580000
