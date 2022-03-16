       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW05DEMO.                                            00020000
      ******************************************************************00040000
      * PROGETTO        : CPS                                          *00050000
      * ID. TRANSAZIONE : RR05                                         *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :   - LISTING                                  *00070043
      * OGGETTO         : LISTA  DIPENDENTI                            *00080043
      * TIPO OPERAZIONE : SELEZIONE                                    *00100043
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 04/03/1999                                   *00130043
      * ULTIMA MODIFICA :   /  /                                       *00140043
      ******************************************************************00150000
       ENVIRONMENT DIVISION.                                            00170000
       CONFIGURATION SECTION.                                           00180000
       SPECIAL-NAMES.                                                   00190000
           DECIMAL-POINT IS COMMA.                                      00200000
       DATA DIVISION.                                                   00210000
       WORKING-STORAGE SECTION.                                         00220000
      * -------------------------------------------------------------- *00310043
      *      DEFINIZIONE VARIABILI HOST                                *00320043
      * -------------------------------------------------------------- *00330043
       01  COD-MSG-HOST                    PIC X(03)   VALUE SPACE.     00340043
       01  W-TSCODE                        PIC 9(01) VALUE 0.           00350043
           88  W-ITEMERR             VALUE 1.                           00360043
           88  W-QIDERR              VALUE 2.                           00370043
       01  W-SQLCODE                       PIC S9(03) COMP VALUE +0.    00380043
           88  W-SQLCODE-OK          VALUE +0.                          00390043
           88  W-SQLCODE-NOT-FOUND   VALUE +100.                        00400043
      *                                                                 00410043
      * -------------------------------------------------------------- *00310043
      *      DEFINIZIONE INDICATORI                                    *00320043
      * -------------------------------------------------------------- *00330043
      *                                                                 00410043
       01  W-NULL-COGNOME                  PIC S9(04) COMP   VALUE +0.  06650043
       01  W-NULL-NOME                     PIC S9(04) COMP   VALUE +0.  06650043
       01  W-NULL-DATA-NASCITA             PIC S9(04) COMP   VALUE +0.  06650043
       01  W-NULL-QUALIFICA-INTERNA        PIC S9(04) COMP   VALUE +0.  06650043
       01  W-NULL-CODICE-FISCALE           PIC S9(04) COMP   VALUE +0.  06650043
       01  W-NULL-RUN-DATE                 PIC S9(04) COMP   VALUE +0.  06650043
      *                                                                 00410043
      * -------------------------------------------------------------- *00310043
      *      DEFINIZIONE CAMPI DI COMODO                               *00320043
      * -------------------------------------------------------------- *00330043
      *                                                                 00410043
       01  W-ITEM-MAP                      PIC 9(04)   VALUE 10.        00420043
       01  W-IND1                          PIC 9(04)   VALUE 0.         00430043
       01  W-FLAG-TROVATO                  PIC 9       VALUE 0.         00430043
       01  W-RIGA                          PIC 9(02)   VALUE ZERO.              
       01  W-COUNT                         PIC 9(04)   VALUE 0.         00440043
      *                                                                 00410043
DEMORR 01  ANNI-BISESTILI        PIC 9(4)   VALUE ZERO.                         
DEMORR 01  ANNI-NON-BISESTILI    PIC 9(4)   VALUE ZERO.                 00440043
      *                                                                 00410043
       01  W-COD-FISC                      PIC X(16).                   00440043
       01  W-COMODO                        PIC X(30).                   00440043
       01  W-COMODO-R  REDEFINES  W-COMODO.                             00440043
          05  W-ELEMENTO   OCCURS  30.                                  00440043
              10  W-ELEM        PIC X.                                  00440043
       01  W-IND2                          PIC 99     VALUE 0.          00440043
                                                                        00440043
       01  W-MATRICOLA-N                   PIC 9(05).                   00710043
       01  W-MATRICOLA-X REDEFINES W-MATRICOLA-N  PIC X(05).            00710043
      *   05  ELEMENTO   OCCURS  30.                                    00440043
      *                                                                 00440043
       01  W-DATA-NASC.                                                 00410043
          05  W-DATA-NASC-GG     PIC  XX.                               00410043
          05  FILLER             PIC  X   VALUE '/'.                    00410043
          05  W-DATA-NASC-MM     PIC  XX.                               00410043
          05  FILLER             PIC  X   VALUE '/'.                    00410043
          05  W-DATA-NASC-AAAA   PIC  XXXX.                             00410043
      *                                                                 00440043
       01  W-DATA-NUMERICA.                                             00410043
          05  W-DATA-NUM-AAAA   PIC  9999.                              00410043
          05  FILLER            PIC  X.                                 00410043
          05  W-DATA-NUM-MM     PIC  99.                                00410043
          05  FILLER            PIC  X.                                 00410043
          05  W-DATA-NUM-GG     PIC  99.                                00410043
      *                                                                 00410043
       01  W-DATA-APP.                                                  00410043
          05  W-DATA-APP-AAAA    PIC  XXXX.                             00410043
          05  FILLER             PIC  X   VALUE '/'.                    00410043
          05  W-DATA-APP-MM      PIC  XX.                               00410043
          05  FILLER             PIC  X   VALUE '/'.                    00410043
          05  W-DATA-APP-GG      PIC  XX.                               00410043
      *                                                                 00410043
DEMORR 01  W-RUN-DATE.                                                          
DEMORR     02  RUN-DATE-AA                   PIC 9(2).                          
DEMORR     02  FILLER                        PIC X VALUE '-'.                   
DEMORR     02  RUN-DATE-MM                   PIC 9(2).                          
DEMORR     02  FILLER                        PIC X VALUE '-'.                   
DEMORR     02  RUN-DATE-GG                   PIC 9(2).                          
      *                                                                 00410043
      *                                                                 00410043
      * -------------------------------------------------------------- *00470043
      *    DEFINIZIONE CAMPI STANDARD DELLA TRANSAZIONE                *00480043
      * -------------------------------------------------------------- *00490043
      *                                                                 00410043
       01  SW-SQL                          PIC  9      VALUE 0.                 
       01  W-MSG                           PIC  9(3)   VALUE 0.         00520000
       01  W-CTL-END                       PIC  X(4)   VALUE 'LOOP'.    00530000
       01  W-NOME-PGM                      PIC  X(8)   VALUE 'CW05DEMO'.00540000
       01  W-PGM-LOGON                     PIC  X(8)   VALUE 'CW01DEMO'.00540000
       01  W-PGM-MENU-GEN                  PIC  X(8)   VALUE 'CW02DEMO'.00540000
       01  W-PGM-MENU-RAMO                 PIC  X(8)   VALUE 'CW03DEMO'.00540000
       01  W-PGM-PREC                      PIC  X(8)   VALUE 'CW06DEMO'.00540000
       01  W-NOME-MAPPA                    PIC  X(7)   VALUE 'MF05MAP'. 00540000
       01  W-NOME-MAPSET                   PIC  X(7)   VALUE 'MF05MAP'. 00540000
       01  W-TRS-ID                        PIC  X(4)   VALUE SPACES.    00550000
GABRY  01  W-TRS-STAMPA                    PIC  X(4)   VALUE 'RR11'.    00550000
       01  W-XCTL-PGM                      PIC  X(8).                   00560000
       01  W-COD-PROGETTO                  PIC  9(6)   VALUE 471000.    00550043
       01  ULT-LABEL                       PIC  X(15)  VALUE SPACES.    00570000
       01  W-LEN                           PIC S9(4)   COMP VALUE +250. 00570043
       01  W-STAMPANTE-START               PIC  X(4)   VALUE 'BR67'.    00570043
      *                                                                 00410043
      * -------------------------------------------------------------- *00590043
      *  DEFINIZIONE CAMPI STANDARD PER CODA DI TEMPORARY STORAGE :    *00600043
      *  MEMORIZZAZIONE DIPENDENTI                                     *00610043
      * -------------------------------------------------------------- *00620043
      * LUNGHEZZA CODA +850                                            *00620043
      *                                                                 00620043
       01  W-NOME-CODA.                                                 00630043
           02  FILLER                        PIC XXX    VALUE 'CPS'.    00640043
           02  W-TERM-CODA                   PIC X(4)   VALUE SPACES.   00650043
           02  W-NUM-CODA                    PIC X      VALUE '1'.      00660043
       01  W-LEN-CODA                        PIC S9(4) COMP VALUE +850. 00670043
       01  W-ITEM-CODA                       PIC S9(4) COMP VALUE +0.   00680043
      *                                                                 00680043
       01  W-CODA-AREA-DATI.                                            00690043
           02  W-CODA-TABELLA OCCURS 10.                                00700043
               05  CPS-SEL                    PIC X(01).                00750043
               05  CPS-MATR                   PIC 9(05).                00710043
               05  CPS-COGN                   PIC X(30).                        
               05  CPS-NOME                   PIC X(15).                00720043
               05  CPS-COD-FISC               PIC X(16).                00740043
               05  CPS-DT-NAT                 PIC X(10).                00730043
               05  CPS-QU-IN                  PIC X(08).                00740043
      * -------------------------------------------------------------- *00760043
      *    DEFINIZIONE  CAMPI  PER DATA                                *00770043
      * -------------------------------------------------------------- *00780043
       01  DATA-CUR.                                                    00790043
           02  MM-CUR                      PIC XX.                      00800043
           02  FILLER                      PIC X.                       00810043
           02  GG-CUR                      PIC XX.                      00820043
           02  FILLER                      PIC X.                       00830043
           02  AA-CUR                      PIC XX.                      00840043
      * -------------------------------------------------------------- *00850043
      *    DEFINIZIONE  MESSAGGIO ABEND SQL                            *00750000
      * -------------------------------------------------------------- *00760000
       01  ERR-SQL.                                                     00770000
           02  FILLER           PIC X(11)  VALUE 'ERRORE SQL '.         00780000
           02  SQL-CODICE       PIC ----.                               00800000
           02  FILLER           PIC X(12)  VALUE ' ALLA LABEL '.        00810000
           02  ULT-LABEL-SQL    PIC X(15).                              00830000
           02  FILLER           PIC X(10)  VALUE ' TRANSID: '.          00840000
           02  TRS-ID-SQL       PIC X(4).                               00860000
           02  FILLER           PIC X(06)  VALUE ' PGM: '.              00870000
           02  NOME-PGM-SQL     PIC X(8).                               00890000
           02  FILLER           PIC X(4).                               00900000
      * -------------------------------------------------------------- *00930000
      *    DEFINIZIONE  MESSAGGIO ABEND CICS                           *00940000
      * -------------------------------------------------------------- *00950000
       01  APPOGGIO-CICS        PIC X(79)  VALUE SPACES.                00960000
       01  ERR-CICS.                                                    00970000
           02  FILLER           PIC X(12)  VALUE 'ERRORE CICS '.        00980000
           02  COD-ERR          PIC X(4).                               01000000
           02  FILLER           PIC X(12)  VALUE ' ALLA LABEL '.        01010000
           02  ULT-LABEL-CICS   PIC X(15).                              01030000
           02  FILLER           PIC X(10)  VALUE ' TRANSID: '.          01040000
           02  TRS-ID-CICS      PIC X(4).                               01060000
           02  FILLER           PIC X(06)  VALUE ' PGM: '.              01070000
           02  NOME-PGM-CICS    PIC X(8).                               01090000
           02  FILLER           PIC X(8).                               01100000
      * -------------------------------------------------------------- *01130000
      *    DEFINIZIONE SQLCA E TABELLE                                 *01140000
      * -------------------------------------------------------------- *01150000
           EXEC SQL  INCLUDE SQLCA          END-EXEC.                   01230043
           EXEC SQL  INCLUDE CWDIPE         END-EXEC.                   01240043
      * -------------------------------------------------------------- *01200000
      *    DEFINIZIONE DELLA MAPPA                                     *01260043
      * -------------------------------------------------------------- *01270043
           COPY MF05MAP.                                                01280043
      * -------------------------------------------------------------- *01290043
      *    DEFINIZIONE DELLE COPY COMUNI                               *01300043
      * -------------------------------------------------------------- *01310043
           COPY CWMESS.                                                 01320043
           COPY CWATTRIB.                                               01340043
           COPY DFHAID.                                                 01350043
      *                                                                 01360043
      * -------------------------------------------------------------- *01360043
      *                                                                *01370043
      *                      DECLARE CUR1                              *01370043
      *                                                                *01370043
      * -------------------------------------------------------------- *01380043
           EXEC SQL DECLARE CUR1 CURSOR FOR                             01390043
                    SELECT  COD_MATRICOLA_DIP,                          01400043
                            COGNOME,                                    01400043
                            NOME,                                               
                            DATA_NASCITA,                               01410043
                            QUALIFICA_INTERNA,                          01430043
                            COD_FISC,                                   01430043
                            RUN_DATE                                    01430043
                    FROM    CPS04.CWDIPENDENTI                          01440043
                    WHERE   COGNOME LIKE :W-COMODO                      01440043
                    ORDER BY  COD_MATRICOLA_DIP                         01450043
           END-EXEC.                                                    01470043
      *                                                                 01360043
      *                                                                 01360043
      *                                                                *01370043
      *                      DECLARE CUR2                              *01370043
      *                                                                *01370043
      * -------------------------------------------------------------- *01380043
           EXEC SQL DECLARE CUR2 CURSOR FOR                             01390043
                    SELECT  COD_MATRICOLA_DIP,                          01400043
                            COGNOME,                                    01400043
                            NOME,                                               
                            DATA_NASCITA,                               01410043
                            QUALIFICA_INTERNA,                          01430043
                            COD_FISC,                                   01430043
                            RUN_DATE                                    01430043
                    FROM    CPS04.CWDIPENDENTI                          01440043
                    WHERE   COGNOME LIKE :W-COMODO OR                   01440043
                            COD_FISC =   :W-COD-FISC                    01440043
                    ORDER BY  COD_MATRICOLA_DIP                         01450043
           END-EXEC.                                                    01470043
      *                                                                 01360043
      * -------------------------------------------------------------- *01480043
      *                      DECLARE CUR3                              *01370043
      *                                                                *01370043
      * -------------------------------------------------------------- *01380043
           EXEC SQL DECLARE CUR3 CURSOR FOR                             01390043
                    SELECT  COD_MATRICOLA_DIP,                          01400043
                            COGNOME,                                    01400043
                            NOME,                                               
                            DATA_NASCITA,                               01410043
                            QUALIFICA_INTERNA,                          01430043
                            COD_FISC,                                   01430043
                            RUN_DATE                                    01430043
                    FROM    CPS04.CWDIPENDENTI                          01440043
                    WHERE   COGNOME LIKE :W-COMODO AND                  01440043
                            COD_FISC =   :W-COD-FISC                    01440043
                    ORDER BY  COD_MATRICOLA_DIP                         01450043
           END-EXEC.                                                    01470043
      *                                                                 01360043
      *                    DEFINIZIONE COMMAREA                        *01490043
      * -------------------------------------------------------------- *01500043
       01  W-COMMAREA.                                                  01510043
           COPY CWCOMMA.                                                01520043
                                                                        01770043
      *----------------                                                 01800043
       LINKAGE SECTION.                                                 01780043
CICS   01  DFHCOMMAREA.                                                         
CICS       05  FILLER  PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.           
      *                                                                         
      * ---                                                             01800043
       PROCEDURE DIVISION.                                              02300000
      *---                                                              02310000
       0000-MAIN SECTION.                                               02320000
      *---------*                                                       02330000
           PERFORM 1000-INIZIO-ELAB.                                    02340000
           PERFORM 2000-CORPO-ELAB.                                     02350000
           PERFORM 3000-FINE-ELAB.                                      02360000
           GOBACK.                                                      02370000
                                                                        02330000
                                                                                
                                                                                
      *---------*                                                       02330000
       1000-INIZIO-ELAB SECTION.                                        02420000
      *----------------*                                                02430000
           MOVE EIBTRNID TO W-TRS-ID.                                   02440000
           MOVE EIBTRMID TO W-TERM-CODA.                                01960043
           PERFORM 1100-COND-ANOMAL.                                    02450000
           PERFORM 1300-TESTA-RIEN.                                     01980043
           PERFORM 1200-TASTI-FUNZ.                                     02460000
                                                                        02480000
       1000-EX. EXIT.                                                   02490000
                                                                        02500000
                                                                        02510000
                                                                        02520000
                                                                        02530000
       1100-COND-ANOMAL SECTION.                                        02540000
      *---------------*                                                 02550000
      *    EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        02560000
      *                                                    END-EXEC.    02570000
           EXEC CICS HANDLE CONDITION  MAPFAIL (1120-COND-MFAIL)        02580000
                                                           END-EXEC.    02590000
           EXEC CICS IGNORE CONDITION QIDERR END-EXEC.                  02120043
           EXEC SQL WHENEVER SQLERROR GO TO 2998-DBERROR   END-EXEC.    02610000
                                                                        02620000
       1100-EX. EXIT.                                                   02630000
                                                                        02640000
                                                                        02650000
                                                                        02660000
                                                                        02670000
       1110-ABEND-CICS SECTION.                                         02680000
      *---------------*                                                 02690000
           MOVE ULT-LABEL     TO  ULT-LABEL-CICS.                       02220043
           MOVE W-TRS-ID      TO  TRS-ID-CICS.                          02230043
           MOVE W-NOME-PGM    TO  NOME-PGM-CICS.                        02240043
           EXEC CICS ASSIGN ABCODE (COD-ERR) END-EXEC.                  02730000
           EXEC CICS HANDLE ABEND CANCEL     END-EXEC.                  02260043
           EXEC CICS SYNCPOINT ROLLBACK      END-EXEC.                  02270043
           MOVE ERR-CICS      TO  M-MSG-2O.                             02280043
           PERFORM 3000-FINE-ELAB.                                      02770000
                                                                        02780000
       1110-EX. EXIT.                                                   02790000
                                                                        02800000
                                                                        02820000
                                                                        02830000
       1120-COND-MFAIL SECTION.                                         02840000
      *---------------*                                                 02850000
           MOVE '** ERRORE DI MPFAIL **' TO ERR-CICS.                           
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)                      
                          ERASE WAIT END-EXEC.                                  
           EXEC CICS RETURN END-EXEC.                                           
                                                                        02390043
       1120-EX. EXIT.                                                   02400043
                                                                        02890000
                                                                        02950000
                                                                        02570043
       1200-TASTI-FUNZ SECTION.                                         02960000
      *---------------*                                                 02970000
                                                                        02600043
           EXEC CICS HANDLE AID                                         02980000
                                PA1    (1210-TASTO-PA1)                 02650043
                                PF11   (1211-TASTO-PF11)                02660043
                                CLEAR  (1220-TASTO-CLEAR)               02990000
                                PF3    (1230-TASTO-PF3)                 02650043
                                PF7    (1237-TASTO-PF7)                 02650043
                                PF8    (1238-TASTO-PF8)                 02660043
                                PF4    (1240-TASTO-PF4)                 02660043
                                ANYKEY (1250-TASTO-ANYKEY)              02680043
           END-EXEC.                                                    02690043
                                                                        02700043
       1200-EX. EXIT.                                                   03070000
                                                                                
                                                                                
                                                                        03110000
       1210-TASTO-PA1 SECTION.                                          03120000
      *--------------*                                * RITORNO CICS *  02770043
                                                                        02800043
           MOVE '** RITORNO CICS **' TO ERR-CICS.                       02770043
           EXEC CICS SEND TEXT FROM   (ERR-CICS)                        03170000
                               LENGTH (78)                              03180000
                               ERASE WAIT END-EXEC.                     03190000
           EXEC CICS RETURN END-EXEC.                                   03210000
                                                                        03220000
       1210-EX. EXIT.                                                   03230000
                                                                                
                                                                                
                                                                        03270000
       1211-TASTO-PF11 SECTION.                                         03060043
      *--------------*                                                  03070043
                                                                        03080043
           PERFORM 2120-NORMALIZZA.                                     03440043
                                                                        03160043
      *    EXEC CICS                                                            
      *         HANDLE CONDITION ENQBUSY(1212-CODA-OCCUPATA)                    
      *         QUIDERR(1213-CODA-ASSENTE)                                      
      *    END-EXEC                                                             
                                                                                
      *    EXEC CICS ENQ                                                        
      *         RESOURCE(W-STAMPANTE-START)                                     
      *         LENGHT(+4)                                                      
      *         NOSUSPEND                                                       
      *    END-EXEC                                                             
                                                                                
           EXEC CICS                                                            
GABRY *         START TRANSID('RR11')                                           
GABRY           START TRANSID(W-TRS-STAMPA)                                     
                FROM(W-COMMAREA)                                                
                LENGTH(W-LEN)                                                 
           END-EXEC.                                                            
                                                                                
GABRY      MOVE 'STAMPA INOLTRATA' TO M-MSG-2O.                                 
GABRY      MOVE -1        TO M-SELL (1).                                05890043
GABRY      MOVE 'LOOP' TO W-CTL-END.                                            
GABRY      PERFORM 3000-FINE-ELAB.                                              
      *                                                                         
       1211-EX. EXIT.                                                   03490000
                                                                        03500000
                                                                                
                                                                                
                                                                                
      *1212-CODA-OCCUPATA SECTION.                                      04140000
      *                                                                         
      *      MOVE 'STAMPANTE OCCUPATA.' TO M-MSG-2O                        04220
      *            MOVE -1        TO M-SELL (1)                         05890043
      *      PERFORM 3000-FINE-ELAB.                                            
      *                                                                         
      *1212-EX. EXIT.                                                           
                                                                        04430000
                                                                                
                                                                        03500000
      *1213-CODA-ASSENTE SECTION.                                       04140000
      *                                                                         
      *      MOVE 'NON ESISTE STAMPANTE.' TO M-MSG-2O                      04220
      *            MOVE -1        TO M-SELL (1)                         05890043
      *      PERFORM 3000-FINE-ELAB.                                            
      *                                                                         
      *1213-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
       1220-TASTO-CLEAR SECTION.                                        03280000
      *--------------*                    * RITORNO AL MENU' GENERALE * 02920043
           PERFORM 2120-NORMALIZZA.                                     03440043
           PERFORM 2180-CTRL-SCELTA.                                    03810043
           PERFORM 2929-DELETE-TS.                                      02930043
           MOVE +250              TO  W-LEN.                            02940043
           MOVE SPACES            TO  COD-MSG-HOST.                     03120043
           MOVE 'END'             TO  W-CTL-END.                        08370000
           MOVE W-PGM-MENU-GEN    TO  W-XCTL-PGM.                       08370000
           PERFORM 3000-FINE-ELAB.                                      08140043
                                                                        03350000
       1220-EX. EXIT.                                                   03360000
                                                                        03240043
                                                                        03610000
                                                                                
       1230-TASTO-PF3 SECTION.                                          03230043
      *--------------*            * RITORNO PROGRAMMA PRECEDENTE *      03240043
                                                                        03610000
           PERFORM 2120-NORMALIZZA.                                     03440043
           PERFORM 2929-DELETE-TS.                                      03260043
           PERFORM 2180-CTRL-SCELTA.                                    03810043
           MOVE +250      TO W-LEN.                                             
           EXEC CICS XCTL PROGRAM  ('CW03DEMO')                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04010000
       1230-EX. EXIT.                                                   04020000
                                                                        03370000
                                                                        03480000
                                                                                
                                                                        04430000
       1237-TASTO-PF7 SECTION.                                          03670043
      *--------------*                                                  04450000
           MOVE '1237-TASTO-PF7' TO ULT-LABEL.                          05170000
      * -------                                                         04730043
           MOVE SPACE     TO M-MSG-2O.                                  03690043
           MOVE -1        TO M-SELL (1).                                90043   
           PERFORM 2120-NORMALIZZA.                                     05140043
           PERFORM 2180-CTRL-SCELTA.                                    03810043
                                                                        03830043
           IF COM-PAG > 1                                               03840043
            THEN                                                        03850043
              SUBTRACT 1    FROM  COM-PAG                               03860043
              MOVE COM-PAG   TO   W-ITEM-CODA                           03880043
              PERFORM 2970-PREPARA-MAP                                  03900043
              PERFORM 3000-FINE-ELAB                                    04690000
            ELSE                                                                
              IF  COM-PAG = 1                                                   
                  THEN                                                          
      *             '018 - PRIMA PAGINA'                                03920043
                    MOVE '018' TO COD-MSG-HOST                          03920043
                    PERFORM 2999-CERCA-ERR                              04630000
              END-IF                                                            
           END-IF.                                                              
                                                                        04700000
       1237-EX. EXIT.                                                   04710000
                                                                        04720000
                                                                        04750000
       1238-TASTO-PF8 SECTION.                                          04030043
      *--------------*                                                  04450000
           MOVE '1238-TASTO-PF8' TO ULT-LABEL.                          05170000
      * -------                                                         04730043
           MOVE SPACE   TO M-MSG-2O.                                    04050043
           MOVE -1      TO M-SELL (1).                                  50043   
           PERFORM 2120-NORMALIZZA.                                     04170043
           PERFORM 2180-CTRL-SCELTA.                                    03810043
                                                                        04330043
           IF COM-PAG < COM-TOT-PAG                                     04200043
              THEN                                                      04210043
                ADD 1          TO  COM-PAG                              04250043
                MOVE COM-PAG   TO  W-ITEM-CODA                          04270043
                PERFORM 2970-PREPARA-MAP                                04290043
                PERFORM 3000-FINE-ELAB                                  04320043
              ELSE                                                      04290043
                IF COM-PAG = COM-TOT-PAG                                04200043
                   THEN                                                       04
      *              '019 - ULTIMA PAGINA.'                                   04
                     MOVE '019'   TO COD-MSG-HOST                             04
                     PERFORM 2999-CERCA-ERR                                   04
                END-IF                                                  04300043
           END-IF.                                                      04300043
                                                                        04300043
       1238-EX. EXIT.                                                   04830000
                                                                        04840000
                                                                                
                                                                                
       1240-TASTO-PF4 SECTION.                                          03060043
      *--------------*               * RITORNO MENU' DI RAMO *          03070043
                                                                        03080043
           PERFORM 2120-NORMALIZZA.                                     03440043
           PERFORM 2180-CTRL-SCELTA.                                    03810043
           PERFORM 2929-DELETE-TS.                                      03090043
           MOVE +250      TO W-LEN.                                             
           EXEC CICS XCTL PROGRAM  ('CW03DEMO')                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        03160043
       1240-EX. EXIT.                                                   03490000
                                                                        03500000
                                                                        03480000
                                                                                
                                                                                
       1250-TASTO-ANYKEY SECTION.                                       03410043
      *-----------------*                                               03420043
                                                                        03430043
           PERFORM 2120-NORMALIZZA.                                     03440043
                                                                        03460043
           MOVE COM-PAG          TO M-PAGO.                             03450043
           INSPECT  M-PAGO       REPLACING LEADING ZEROES BY SPACE.             
           MOVE '006'            TO COD-MSG-HOST.                       03470043
           MOVE -1               TO M-SELL (1).                         70043   
                                                                        04190000
           PERFORM 2999-CERCA-ERR.                                      03600043
                                                                        04380000
       1250-EX. EXIT.                                                   03620043
                                                                                
                                                                                
                                                                        04870000
       1300-TESTA-RIEN SECTION.                                         04880000
      *---------------*                                                 04890000
           IF EIBCALEN = ZERO                                           04410043
            THEN                                                        04910000
              PERFORM 1310-TRANS-DIS                                    04920000
            ELSE                                                        04440043
              MOVE DFHCOMMAREA TO W-COMMAREA                            04450043
           END-IF.                                                      04930000
                                                                        04940000
       1300-EX. EXIT.                                                   04970000
                                                                                
                                                                                
                                                                        05010000
       1310-TRANS-DIS SECTION.                                          05020000
      *--------------*                                                  05030000
           MOVE '*** TRANSAZIONE NON PERMESSA ***' TO ERR-CICS.         05040000
           EXEC CICS SEND TEXT FROM   (ERR-CICS)                        05050000
                               LENGTH (78)                              05060000
                               ERASE WAIT END-EXEC.                     05070000
           EXEC CICS RETURN END-EXEC.                                   05080000
                                                                        05090000
       1310-EX. EXIT.                                                   05100000
                                                                        05110000
                                                                                
                                                                                
                                                                        04690043
       2000-CORPO-ELAB SECTION.                                         05150000
      *---------------*                                                 05160000
           MOVE '2000-CORPO-ELAB' TO ULT-LABEL.                         05170000
      * -------                                                         04730043
           MOVE SPACE TO M-MSG-2O.                                      04740043
                                                                        04750043
           IF W-NOME-PGM = COM-NOME-PGM                                 05190000
            THEN                                                        05200000
              PERFORM 2100-RECEIVE                                      05210000
              PERFORM 2200-CONTROLLI                                    04790043
            ELSE                                                        04800043
              PERFORM 2900-RIEMP-MASK                                   05230000
           END-IF.                                                      05240000
                                                                        05250000
       2000-EX. EXIT.                                                   05260000
                                                                                
                                                                                
                                                                        04890043
       2100-RECEIVE SECTION.                                            05300000
      *------------*                                                    05310000
           MOVE '2100-RECEIVE' TO ULT-LABEL.                            05320000
      * ---                                                             05330000
           PERFORM 2110-REC-MAPPA.                                      04940043
           PERFORM 2120-NORMALIZZA.                                     04950043
                                                                        05360000
       2100-EX. EXIT.                                                   05370000
                                                                                
                                                                                
                                                                        05410000
       2110-REC-MAPPA SECTION.                                          05020043
      *--------------*                                                  05030043
           MOVE '2110-REC-MAPPA' TO ULT-LABEL.                          05040043
      * ---                                                             05050043
           EXEC CICS RECEIVE MAP    (W-NOME-MAPPA)                      09850043
                             MAPSET (W-NOME-MAPSET)                     09860043
                             INTO   (MF05MAPI) END-EXEC.                09860043
                                                                        05420000
       2110-EX. EXIT.                                                   05090043
                                                                        05580000
                                                                        05630000
       2180-CTRL-SCELTA SECTION.                                        05140043
      *---------------*                                                 05150043
           MOVE '2180-CTRL-SCELTA' TO ULT-LABEL.                        05160043
      * ---                                                             05170043
           PERFORM VARYING W-IND1 FROM 1 BY 1                           05180043
                   UNTIL   W-IND1 > W-ITEM-MAP                          05190043
             IF M-SELI(W-IND1)  NOT =  SPACE                            05200043
                THEN                                                       04210
      *            '012 - PER USCIRE, TOGLIERE LA SELEZIONE.'              04220
                   MOVE '012'     TO COD-MSG-HOST                          04220
                   MOVE FSET-BRT  TO M-SELA (W-IND1)                    05880043
                   MOVE -1        TO M-SELL (W-IND1)                    05890043
                   PERFORM 2999-CERCA-ERR                                  04230
             END-IF                                                             
           END-PERFORM.                                                 05220043
                                                                        05830000
       2180-EX. EXIT.                                                   05240043
                                                                                
                                                                                
                                                                                
       2120-NORMALIZZA SECTION.                                         05140043
      *---------------*                                                 05150043
           MOVE '2120-NORMALIZZA' TO ULT-LABEL.                         05160043
      * ---                                                             05170043
           PERFORM VARYING W-IND1 FROM 1 BY 1                           05180043
              UNTIL   W-IND1 > W-ITEM-MAP                               05190043
              INSPECT M-SELO(W-IND1) REPLACING ALL LOW-VALUE BY ' '        05200
              INSPECT M-SELO(W-IND1) REPLACING ALL '_'   BY ' '            05200
           END-PERFORM.                                                         
                                                                        05830000
       2120-EX. EXIT.                                                   05240043
                                                                        05920000
                                                                                
                                                                        06080000
       2200-CONTROLLI SECTION.                                          05670043
      *--------------*                                                  05680043
           MOVE '2200-CONTROLLI' TO ULT-LABEL.                          05690043
      *----                                                             05700043
           MOVE ZEROES  TO  W-COUNT.                                    05710043
           MOVE SPACES  TO  COD-MSG-HOST                                05720043
                            M-MSG-2O.                                   05730043
                                                                        06090000
                                                                                
           PERFORM 2205-CONTROLLI-LOGICI                                05810043
              VARYING W-IND1   FROM 1 BY 1                              05810043
              UNTIL W-IND1          >    W-ITEM-MAP                     05820043
              OR  COD-MSG-HOST  NOT =  SPACES                           05830043
              OR  W-COUNT         >      1.                             05840043
                                                                        06460000
           IF COD-MSG-HOST  NOT =  SPACES                               06020043
            THEN                                                        06030043
              PERFORM 2999-CERCA-ERR                                    06040043
           END-IF.                                                      06470000
                                                                        06480000
           IF W-COUNT  =  1                                             06130043
            THEN                                                        06140043
              PERFORM 2700-CHIAMA-PGM                                   06150043
            ELSE                                                                
              IF W-COUNT = 0                                                    
                MOVE '021'     TO COD-MSG-HOST                          05870043
                MOVE FSET-BRT  TO M-SELA (1)                            05880043
                MOVE -1        TO M-SELL (1)                            05890043
                PERFORM 2999-CERCA-ERR                                  06040043
              END-IF                                                    05900043
           END-IF.                                                      06680000
                                                                        06690000
       2200-EX. EXIT.                                                   06180043
                                                                                
                                                                                
                                                                        06080000
       2205-CONTROLLI-LOGICI SECTION.                                   05670043
      *---------------------*                                           05680043
           MOVE '2205-CONTROLLI-LOGICI' TO ULT-LABEL.                   05690043
      *----                                                             05700043
           IF M-SELI (W-IND1)  NOT =  'S'                               05850043
              IF M-SELI (W-IND1)  NOT =  ' '                            05850043
                 IF M-SELI (W-IND1)  NOT =  '_'                         05850043
                 THEN                                                   05860043
                   MOVE '021'     TO COD-MSG-HOST                       05870043
                   MOVE FSET-BRT  TO M-SELA (W-IND1)                    05880043
                   MOVE -1        TO M-SELL (W-IND1).                   05890043
                                                                        05900043
                                                                        05900043
           IF M-SELI (W-IND1)  =  'S'                                   05910043
              THEN                                                      05920043
                ADD   1        TO W-COUNT                               05930043
                IF W-COUNT = 2                                          05940043
                   THEN                                                 05950043
                      MOVE '022'     TO COD-MSG-HOST                    06090043
                      MOVE -1        TO M-SELL (W-IND1)                 05960043
                END-IF                                                  05970043
           END-IF.                                                      05990043
                                                                        06000043
       2205-EX. EXIT.                                                   06180043
                                                                        06240043
                                                                                
                                                                        06240043
       2700-CHIAMA-PGM  SECTION.                                        07860043
      *---------------*                                                 07870043
           MOVE '2700-CHIAMA-PGM     ' TO ULT-LABEL.                    07880043
      *---                                                              07890043
           PERFORM VARYING W-IND1   FROM 1 BY 1                         07900043
                     UNTIL M-SELO (W-IND1)  =  'S'                      07910043
           END-PERFORM.                                                 07940043
                                                                                
           MOVE M-MATRO   (W-IND1) TO COM-MATRICOLA.                    40043   
           INSPECT  COM-MATRICOLA REPLACING LEADING SPACE BY ZERO.              
                                                                        40043   
           MOVE M-COGNO   (W-IND1) TO COM-COGNOME.                      40043   
           MOVE M-NOMEO   (W-IND1) TO COM-NOME.                         40043   
           MOVE M-DT-NATO (W-IND1) TO COM-DATA-NASCITA.                 40043   
           MOVE M-QU-INO  (W-IND1) TO COM-QUALIFICA-INTERNA.            40043   
                                                                        08100043
           MOVE COM-PAG           TO W-ITEM-CODA                        08100043
           EXEC CICS READQ  TS QUEUE(W-NOME-CODA)                       07540043
                               INTO(W-CODA-AREA-DATI)                   07550043
                               LENGTH(W-LEN-CODA)                       07560043
                               ITEM(W-ITEM-CODA)                        07570043
           END-EXEC.                                                    07580043
           MOVE M-MATRO(W-IND1) TO W-MATRICOLA-N.                               
           MOVE 1 TO W-RIGA.                                                    
           PERFORM 2710-CARICA-COD-FISC                                 08100043
                   UNTIL W-RIGA > 10 OR W-FLAG-TROVATO = 1.             08100043
           MOVE CPS-COD-FISC(W-RIGA) TO COM-COD-FI.                             
                                                                                
           MOVE 'END'             TO W-CTL-END.                         08110043
           MOVE 'CW06DEMO'        TO W-XCTL-PGM.                        08120043
                                                                        08130043
           PERFORM 3200-PASSA-CTL.                                      08140043
                                                                        08150043
       2700-EX. EXIT.                                                   08160043
                                                                                
                                                                                
                                                                                
       2710-CARICA-COD-FISC SECTION.                                    08180043
      *---------------*                                                 08230043
           MOVE '2710-CARICA-COD-FISC' TO ULT-LABEL.                    07510043
      *---------------*                                                 08230043
                                                                        07530043
           IF W-FLAG-TROVATO NOT EQUAL 1                                08190043
              IF W-MATRICOLA-N = CPS-MATR(W-RIGA)                               
                 MOVE 1 TO W-FLAG-TROVATO                               08200043
              ELSE                                                              
                 ADD 1 TO W-RIGA.                                               
                                                                        08210043
       2710-EX. EXIT.                                                   08180043
                                                                                
                                                                                
                                                                                
                                                                                
       2900-RIEMP-MASK SECTION.                                         08220043
      *---------------*                                                 08230043
           MOVE '2900-RIEMP-MASK' TO ULT-LABEL.                         08240043
      *---                                                              08250043
           MOVE LOW-VALUE        TO MF05MAPO.                           08260043
           PERFORM 2901-CONTR-CUR.                                      08410043
           MOVE 1 TO W-ITEM-CODA                                        08430043
                     COM-PAG.                                                   
           PERFORM 2970-PREPARA-MAP.                                    08420043
                                                                        08450043
           MOVE SPACE            TO COM-MESSAGGIO                       08460043
                                    M-MSG-2O.                           08470043
      *    '020 - INIZIO VISUALIZZAZIONE TABELLA'                               
           MOVE '020' TO COD-MSG-HOST.                                          
           PERFORM 2999-CERCA-ERR.                                              
                                                                                
       2900-EX. EXIT.                                                   08480043
                                                                                
                                                                                
                                                                                
       2901-CONTR-CUR  SECTION.                                         06230043
      *------------*                                                    06240043
           MOVE '2901-CONTR-CUR ' TO ULT-LABEL.                         06250043
      * ---                                                             06260043
           MOVE EIBTRMID TO W-TERM-CODA.                                06270043
           PERFORM 2929-DELETE-TS.                                      06280043
                                                                        06760000
           MOVE 0             TO  W-ITEM-CODA.                          06300043
           MOVE  COM-COGNOME  TO  W-COMODO.                             06300043
                                                                        06300043
           PERFORM VARYING W-IND2 FROM 30 BY -1                         05180043
                   UNTIL   W-ELEM(W-IND2) NOT = SPACE                   05190043
                           OR   W-IND2      = 0                         05190043
                   MOVE  '%'   TO  W-ELEM(W-IND2)                       05200043
           END-PERFORM.                                                 05220043
           MOVE  COM-COD-FI   TO  W-COD-FISC.                           06300043
                                                                        05830000
           IF W-COMODO = SPACES AND W-COD-FISC = SPACES                         
           PERFORM  2902-PRIMO-CURSORE.                                 06300043
                                                                        06260043
           IF (W-COMODO NOT EQUAL SPACES) AND                                   
              (W-COD-FISC = SPACES)                                             
           PERFORM  2903-SECONDO-CURSORE.                               06300043
                                                                        06260043
           IF (W-COMODO = SPACES) AND                                           
              (W-COD-FISC NOT EQUAL SPACES)                                     
           PERFORM  2903-SECONDO-CURSORE.                               06300043
                                                                        06260043
           IF (W-COMODO NOT EQUAL SPACES) AND                                   
              (W-COD-FISC NOT EQUAL SPACES)                                     
           PERFORM  2904-TERZO-CURSORE.                                 06300043
                                                                                
           MOVE 1 TO COM-PAG.                                           06510043
           MOVE 1 TO W-ITEM-CODA.                                       06520043
                                                                        07150000
       2901-EX. EXIT.                                                   06540043
      *----                                                             06620043
                                                                        06890000
                                                                                
                                                                        07100043
       2902-PRIMO-CURSORE   SECTION.                                    06590043
      *----                                                             06620043
           MOVE '2902-PRIMO-CURSORE' TO ULT-LABEL.                      06610043
      *----                                                             06620043
           EXEC SQL OPEN CUR1 END-EXEC.                                 06310043
                                                                        06890000
           PERFORM 2912-FETCH-CUR1                                      06330043
            UNTIL (W-SQLCODE-NOT-FOUND OR NOT W-SQLCODE-OK).            06340043
                                                                                
           IF W-SQLCODE-NOT-FOUND                                               
            THEN                                                        06890000
              IF SW-SQL = 0                                                     
                THEN                                                            
                  MOVE SPACES TO COM-COGNOME                                    
                                 COM-COD-FI                                     
                  MOVE 'NON ESISTONO DATI DA VISUALIZZARE' TO                   
                                              COM-MESSAGGIO                     
                  MOVE 'END'             TO W-CTL-END                   08110043
                  MOVE 'CW06DEMO'        TO W-XCTL-PGM                  08120043
                  PERFORM 3200-PASSA-CTL                                08140043
                ELSE                                                            
                  MOVE 0 TO SW-SQL                                              
              END-IF                                                            
           END-IF.                                                              
                                                                                
           EXEC SQL CLOSE CUR1  END-EXEC.                               06380043
                                                                        06890000
           IF W-RIGA > 0                                                        
              PERFORM 2922-WRITE-TS                                     06350043
           END-IF.                                                              
                                                                        06890000
       2902-EX. EXIT.                                                           
                                                                        07160000
                                                                                
                                                                                
                                                                                
       2903-SECONDO-CURSORE   SECTION.                                  06590043
      *----                                                             06620043
           MOVE '2903-SECONDO-CURSORE' TO ULT-LABEL.                    06610043
      *----                                                             06620043
           EXEC SQL OPEN CUR2 END-EXEC.                                 06310043
                                                                        06890000
           PERFORM 2913-FETCH-CUR2                                      06330043
            UNTIL (W-SQLCODE-NOT-FOUND OR NOT W-SQLCODE-OK).            06340043
                                                                        06890000
           IF W-SQLCODE-NOT-FOUND                                               
            THEN                                                        06890000
              IF SW-SQL = 0                                                     
                THEN                                                            
                  MOVE SPACES TO COM-COGNOME                                    
                                 COM-COD-FI                                     
                  MOVE 'NON ESISTONO DATI DA VISUALIZZARE' TO                   
                                              COM-MESSAGGIO                     
                  MOVE 'END'             TO W-CTL-END                   08110043
                  MOVE 'CW06DEMO'        TO W-XCTL-PGM                  08120043
                  PERFORM 3200-PASSA-CTL                                08140043
                ELSE                                                            
                  MOVE 0 TO SW-SQL                                              
              END-IF                                                            
           END-IF.                                                              
                                                                                
           EXEC SQL CLOSE CUR2  END-EXEC.                               06380043
                                                                        06890000
           IF W-RIGA > 0                                                        
              PERFORM 2922-WRITE-TS                                     06350043
           END-IF.                                                              
                                                                        06890000
       2903-EX. EXIT.                                                           
                                                                        07160000
                                                                                
                                                                                
                                                                                
       2904-TERZO-CURSORE   SECTION.                                    06590043
      *----                                                             06620043
           MOVE '2904-TERZO-CURSORE' TO ULT-LABEL.                      06610043
      *----                                                             06620043
           EXEC SQL OPEN CUR3 END-EXEC.                                 06310043
                                                                        06890000
           PERFORM 2914-FETCH-CUR3                                      06330043
            UNTIL  W-SQLCODE-NOT-FOUND OR NOT W-SQLCODE-OK  OR          06340043
                         COM-COD-FI = COD-FISC OF DCL-CPSDIP.           06890000
                                                                                
           IF W-SQLCODE-NOT-FOUND                                               
            THEN                                                        06890000
              IF SW-SQL = 0                                                     
                THEN                                                            
                  MOVE SPACES TO COM-COGNOME                                    
                                 COM-COD-FI                                     
                  MOVE 'NON ESISTONO DATI DA VISUALIZZARE' TO                   
                                              COM-MESSAGGIO                     
                  MOVE 'END'             TO W-CTL-END                   08110043
                  MOVE 'CW06DEMO'        TO W-XCTL-PGM                  08120043
                  PERFORM 3200-PASSA-CTL                                08140043
                ELSE                                                            
                  MOVE 0 TO SW-SQL                                              
              END-IF                                                            
           END-IF.                                                              
           EXEC SQL CLOSE CUR3  END-EXEC.                               06380043
                                                                        06890000
           IF W-RIGA > 0                                                        
              PERFORM 2922-WRITE-TS                                     06350043
           END-IF.                                                              
                                                                        06890000
       2904-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
       2912-FETCH-CUR1  SECTION.                                        06590043
      *------------*                                                    06600043
           MOVE '2912-FETCH-CUR1' TO ULT-LABEL.                         06610043
      *----                                                             06620043
           EXEC SQL FETCH CUR1                                          06630043
                    INTO :DCL-CPSDIP.COD-MATRICOLA-DIP,                 06640043
                         :DCL-CPSDIP.COGNOME                            06650043
                                :W-NULL-COGNOME,                        06650043
                         :DCL-CPSDIP.NOME                                       
                                :W-NULL-NOME,                           06650043
                         :DCL-CPSDIP.DATA-NASCITA                       06670043
                                :W-NULL-DATA-NASCITA,                   06650043
                         :DCL-CPSDIP.QUALIFICA-INTERNA                  06680043
                                :W-NULL-QUALIFICA-INTERNA,              06650043
                         :DCL-CPSDIP.COD-FISC                           06680043
                                :W-NULL-CODICE-FISCALE,                 06650043
                         :DCL-CPSDIP.RUN-DATE                           06680043
                                :W-NULL-RUN-DATE                        06650043
           END-EXEC.                                                    06690043
           MOVE SQLCODE TO W-SQLCODE.                                           
                                                                        07210000
           IF W-SQLCODE-OK                                                      
             THEN                                                               
                MOVE    1    TO     SW-SQL                                      
                PERFORM 2920-CARICA-CODA                                        
           END-IF.                                                              
                                                                        07210000
       2912-EX. EXIT.                                                   06590043
                                                                        07180000
                                                                        06620043
                                                                                
       2913-FETCH-CUR2  SECTION.                                        06590043
      *------------*                                                    06600043
           MOVE '2913-FETCH-CUR2' TO ULT-LABEL.                         06610043
      *----                                                             06620043
           EXEC SQL FETCH CUR2                                          06630043
                    INTO :DCL-CPSDIP.COD-MATRICOLA-DIP,                 06640043
                         :DCL-CPSDIP.COGNOME                            06650043
                                :W-NULL-COGNOME,                        06650043
                         :DCL-CPSDIP.NOME                                       
                                :W-NULL-NOME,                           06650043
                         :DCL-CPSDIP.DATA-NASCITA                       06670043
                                :W-NULL-DATA-NASCITA,                   06650043
                         :DCL-CPSDIP.QUALIFICA-INTERNA                  06680043
                                :W-NULL-QUALIFICA-INTERNA,              06650043
                         :DCL-CPSDIP.COD-FISC                           06680043
                                :W-NULL-CODICE-FISCALE,                 06650043
                         :DCL-CPSDIP.RUN-DATE                           06680043
                                :W-NULL-RUN-DATE                        06650043
           END-EXEC.                                                    06690043
                                                                        07210000
           MOVE SQLCODE TO W-SQLCODE.                                           
           IF W-SQLCODE-OK                                                      
             THEN                                                               
                MOVE    1    TO     SW-SQL                                      
                PERFORM 2920-CARICA-CODA                                        
           END-IF.                                                              
                                                                        07210000
                                                                        07210000
       2913-EX. EXIT.                                                   06590043
                                                                        07200000
                                                                        06620043
                                                                        07160000
       2914-FETCH-CUR3  SECTION.                                        06590043
      *------------*                                                    06600043
           MOVE '2914-FETCH-CUR3' TO ULT-LABEL.                         06610043
      *----                                                             06620043
           EXEC SQL FETCH CUR3                                          06630043
                    INTO :DCL-CPSDIP.COD-MATRICOLA-DIP,                 06640043
                         :DCL-CPSDIP.COGNOME                            06650043
                                :W-NULL-COGNOME,                        06650043
                         :DCL-CPSDIP.NOME                                       
                                :W-NULL-NOME,                           06650043
                         :DCL-CPSDIP.DATA-NASCITA                       06670043
                                :W-NULL-DATA-NASCITA,                   06650043
                         :DCL-CPSDIP.QUALIFICA-INTERNA                  06680043
                                :W-NULL-QUALIFICA-INTERNA,              06650043
                         :DCL-CPSDIP.COD-FISC                           06680043
                                :W-NULL-CODICE-FISCALE,                 06650043
                         :DCL-CPSDIP.RUN-DATE                           06680043
                                :W-NULL-RUN-DATE                        06650043
           END-EXEC.                                                    06690043
                                                                                
           MOVE SQLCODE TO W-SQLCODE.                                           
           IF W-SQLCODE-OK                                                      
             THEN                                                               
                MOVE    1    TO     SW-SQL                                      
                PERFORM 2920-CARICA-CODA                                        
           END-IF.                                                              
                                                                                
       2914-EX. EXIT.                                                   06590043
      *----------------                                                 06620043
                                                                                
                                                                                
       2920-CARICA-CODA SECTION.                                                
      *----------------                                                 06620043
           MOVE '2920-CARICA-CODA' TO ULT-LABEL.                        06610043
      *----------------                                                 06620043
           MOVE SQLCODE    TO   W-SQLCODE.                              06710043
                                                                        06710043
           IF W-SQLCODE-OK                                              06720043
              ADD 1 TO W-RIGA                                                   
              MOVE COD-MATRICOLA-DIP  OF  DCL-CPSDIP                    06740043
                                      TO CPS-MATR(W-RIGA)               06750043
              IF  W-NULL-COGNOME = -1                                   06680043
                  MOVE SPACES              TO CPS-COGN(W-RIGA)                  
              ELSE                                                              
                  MOVE COGNOME             OF DCL-CPSDIP                        
                                           TO CPS-COGN(W-RIGA)                  
              END-IF                                                            
                                                                                
              IF  W-NULL-NOME = -1                                      06680043
                  MOVE SPACES              TO CPS-NOME(W-RIGA)                  
              ELSE                                                              
                  MOVE NOME                OF DCL-CPSDIP                        
                                           TO CPS-NOME(W-RIGA)                  
              END-IF                                                            
                                                                                
              IF  W-NULL-DATA-NASCITA = -1                              06680043
                  MOVE SPACES              TO CPS-DT-NAT(W-RIGA)                
              ELSE                                                              
                  MOVE DATA-NASCITA        OF DCL-CPSDIP                        
                                           TO CPS-DT-NAT(W-RIGA)                
              END-IF                                                            
                                                                                
              IF  W-NULL-QUALIFICA-INTERNA = -1                         06680043
                  MOVE SPACES              TO CPS-QU-IN(W-RIGA)                 
              ELSE                                                              
                  MOVE QUALIFICA-INTERNA   OF DCL-CPSDIP                        
                                           TO CPS-QU-IN(W-RIGA)                 
              END-IF                                                            
                                                                                
              IF  W-NULL-CODICE-FISCALE    = -1                         06680043
                  MOVE SPACES              TO CPS-COD-FISC(W-RIGA)              
              ELSE                                                              
                  MOVE COD-FISC            OF DCL-CPSDIP                        
                                           TO CPS-COD-FISC(W-RIGA)              
              END-IF                                                            
DEMORR        IF  W-NULL-RUN-DATE      NOT = -1                         06680043
DEMORR            MOVE RUN-DATE OF DCL-CPSDIP TO W-RUN-DATE                     
DEMORR            MOVE DATA-NASCITA OF DCL-CPSDIP TO W-DATA-NUMERICA            
DEMORR            DIVIDE W-DATA-NUM-AAAA BY RUN-DATE-AA                         
DEMORR                   GIVING ANNI-BISESTILI                                  
DEMORR                   REMAINDER  ANNI-NON-BISESTILI                          
              END-IF                                                            
            END-IF.                                                             
                                                                                
           IF W-RIGA >  9                                                       
              PERFORM 2922-WRITE-TS                                             
              ADD  1          TO  W-ITEM-CODA                              06980
              MOVE 0          TO  W-RIGA                                        
              MOVE SPACE      TO  W-CODA-AREA-DATI                         00   
           END-IF.                                                              
                                                                                
       2920-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                        06930043
       2922-WRITE-TS SECTION.                                           06940043
      *------------*                                                    06950043
           MOVE '2922-WRITE-TS ' TO ULT-LABEL.                          06960043
      *----                                                             06970043
                                                                        06990043
           EXEC CICS WRITEQ TS QUEUE(W-NOME-CODA)                       07000043
                               FROM(W-CODA-AREA-DATI)                   07010043
                               LENGTH(W-LEN-CODA)                       07020043
                               ITEM(W-ITEM-CODA)                        07030043
           END-EXEC.                                                    07040043
                                                                        07050043
           MOVE W-ITEM-CODA TO COM-TOT-PAG.                             06980043
SILVIA     MOVE W-NOME-CODA TO COM-NOME-CODA.                           06980043
                                                                        07050043
       2922-EX. EXIT.                                                   07060043
                                                                        07080043
                                                                                
                                                                        07480043
       2925-LEGGI-TS SECTION.                                           07490043
      *------------*                                                    07500043
           MOVE '2925-LEGGI-TS ' TO ULT-LABEL.                          07510043
      * ---                                                             07520043
                                                                        07530043
           EXEC CICS READQ  TS QUEUE(W-NOME-CODA)                       07540043
                               INTO(W-CODA-AREA-DATI)                   07550043
                               LENGTH(W-LEN-CODA)                       07560043
                               ITEM(W-ITEM-CODA)                        07570043
           END-EXEC.                                                    07580043
                                                                        07590043
                                                                        03220000
       2925-EX. EXIT.                                                   07600043
                                                                        07610043
                                                                                
                                                                                
       2929-DELETE-TS SECTION.                                          09040043
      * -------------*                                                  09050043
           MOVE '2929-DELETE-TS' TO ULT-LABEL.                          09060043
      * ---                                                             09070043
           MOVE EIBTRMID TO W-TERM-CODA.                                09080043
           EXEC CICS HANDLE CONDITION QIDERR(2929-EX) END-EXEC.         09090043
           EXEC CICS DELETEQ TS QUEUE(W-NOME-CODA) END-EXEC.            09100043
                                                                        09110043
       2929-EX. EXIT.                                                   09120043
                                                                        09130043
                                                                                
                                                                                
                                                                        09160043
       2970-PREPARA-MAP SECTION.                                        07110043
      *------------*                                                    07120043
           MOVE '2970-PREPARA-MAP ' TO ULT-LABEL.                       07130043
      * ---                                                             07140043
           PERFORM 2925-LEGGI-TS.                                       07190043
           PERFORM 2980-CARICA-MAPPA                                            
                   VARYING W-RIGA FROM 1 BY 1                                   
                   UNTIL W-RIGA > W-ITEM-MAP.                                   
                                                                        07400043
           MOVE COM-PAG TO M-PAGO.                                      07410043
           INSPECT  M-PAGO       REPLACING LEADING ZEROES BY SPACE.             
                                                                        07420043
                                                                        07430043
       2970-EX. EXIT.                                                   07440043
                                                                        07450043
                                                                        06620043
                                                                        06920043
                                                                        07090043
       2980-CARICA-MAPPA SECTION.                                               
      *------------*                                                    07120043
           MOVE '2980-CARICA-MAPPA ' TO ULT-LABEL.                      07130043
      * ---                                                             07140043
                                                                                
           MOVE CPS-SEL(W-RIGA)            TO M-SELO(W-RIGA).           07210   
                                                                        00440043
           IF   CPS-MATR(W-RIGA)  NOT =  SPACE                                  
                MOVE CPS-MATR(W-RIGA)           TO W-MATRICOLA-N                
                INSPECT  W-MATRICOLA-X REPLACING LEADING ZEROES BY SPACE        
                MOVE W-MATRICOLA-X              TO M-MATRO(W-RIGA)              
           ELSE                                                                 
                MOVE SPACES                     TO M-MATRO(W-RIGA)              
           END-IF.                                                              
                                                                        00440043
           MOVE CPS-COGN(W-RIGA)           TO M-COGNO(W-RIGA).          07220   
           MOVE CPS-NOME(W-RIGA)           TO M-NOMEO(W-RIGA).          07240   
                                                                        00440043
           IF   CPS-DT-NAT(W-RIGA) NOT = SPACE                          07240   
                MOVE CPS-DT-NAT(W-RIGA)         TO W-DATA-APP           07240   
                MOVE W-DATA-APP-GG              TO W-DATA-NASC-GG       07240   
                MOVE W-DATA-APP-MM              TO W-DATA-NASC-MM       07240   
                MOVE W-DATA-APP-AAAA            TO W-DATA-NASC-AAAA     07240   
                MOVE W-DATA-NASC                TO M-DT-NATO(W-RIGA)    07240   
           ELSE                                                         07240   
                MOVE SPACES                     TO M-DT-NATO(W-RIGA)    07240   
           END-IF.                                                      07240   
                                                                        00440043
           MOVE CPS-QU-IN(W-RIGA)          TO M-QU-INO(W-RIGA).         07240   
                                                                                
                                                                        07050043
           IF M-MATRO(W-RIGA) > SPACES                                          
              THEN                                                              
                MOVE FSET          TO M-SELA(W-RIGA)                    460043  
                MOVE '_'           TO M-SELO(W-RIGA)                       07200
              ELSE                                                              
                MOVE PROT-DRK      TO M-SELA(W-RIGA)                    460043  
                MOVE SPACES        TO M-SELO(W-RIGA)                       07200
           END-IF.                                                              
                                                                                
           MOVE -1               TO M-SELL (1).                         70043   
                                                                        05980043
       2980-EX. EXIT.                                                           
      *------------*                                                    07500043
                                                                        07460043
                                                                                
                                                                        08510043
                                                                        08520043
       2998-DBERROR  SECTION.                                           07220000
      *------------*                                                    07230000
           MOVE ULT-LABEL  TO ULT-LABEL-SQL.                            07240000
           MOVE W-TRS-ID   TO TRS-ID-SQL.                               07250000
           MOVE W-NOME-PGM TO NOME-PGM-SQL.                             07260000
           MOVE SQLCODE    TO SQL-CODICE                                08580043
           MOVE ERR-SQL    TO M-MSG-2O OF MF05MAPO.                     07280000
           EXEC CICS DUMP TRANSACTION DUMPCODE(SQL-CODICE)                      
                                         END-EXEC.                              
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       07290000
           PERFORM 3000-FINE-ELAB.                                      07300000
                                                                        07310000
       2998-EX. EXIT.                                                   07320000
                                                                        07330000
                                                                        07340000
                                                                        07350000
                                                                        07360000
       2999-CERCA-ERR SECTION.                                          07370000
      *--------------*                                                  07380000
           MOVE '2999-CERCA-ERR' TO ULT-LABEL.                          07390000
      * ---                                                             08710043
           SET IND-TAB TO 1.                                            07410000
                                                                        07420000
           SEARCH ELEM-TAB-MSG AT END                                   07430000
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'            07440000
                    TO M-MSG-2O                                         07450000
                  WHEN COD-MSG-HOST  =  ELEM-COD-MSG(IND-TAB)           08770043
                       MOVE ELEM-DESC-MSG(IND-TAB)  TO M-MSG-2O         08780043
           END-SEARCH.                                                  07490000
                                                                        07500000
           PERFORM 3000-FINE-ELAB.                                      07510000
                                                                        07520000
       2999-EX. EXIT.                                                   07530000
                                                                        07540000
                                                                        07550000
                                                                        07560000
                                                                        07570000
       3000-FINE-ELAB SECTION.                                          07590000
      * -------------*                                                  07600000
           MOVE '3000-FINE-ELAB' TO ULT-LABEL.                          07610000
      * ---                                                             07620000
           IF W-CTL-END = 'LOOP'                                        07630000
              PERFORM 3100-RIENTRO                                      07640000
           ELSE                                                         07650000
      *       PERFORM 2929-DELETE-TS                                    08950043
              PERFORM 3200-PASSA-CTL                                    07670000
           END-IF.                                                      07680000
                                                                        07690000
       3000-EX. EXIT.                                                   07700000
                                                                        07710000
                                                                        07740000
                                                                                
       3100-RIENTRO SECTION.                                            07750000
      *------------*                                                    07760000
           MOVE '3100-RIENTRO' TO ULT-LABEL.                            07770000
      *---                                                              07780000
           MOVE COM-DATA-SISTEMA TO M-DATA-SO.                          07790000
           PERFORM 3130-FORMATTA-MAPPA.                                 09220043
           PERFORM 3140-INVIO-MAPPA.                                    07800000
                                                                        07810000
       3100-EX. EXIT.                                                   07820000
                                                                        07990000
                                                                                
                                                                        09420043
       3130-FORMATTA-MAPPA SECTION.                                     09430043
      *-------------------*                                             09440043
           MOVE '3130-FORMATTA-MAPPA' TO ULT-LABEL.                     09450043
      *---                                                              09460043
           PERFORM VARYING W-IND1 FROM 1 BY 1                           09470043
                     UNTIL W-IND1 > W-ITEM-MAP                          09480043
                                                                                
               IF M-MATRO (W-IND1) > SPACES                                     
                  MOVE FSET      TO M-SELA(W-IND1)                              
                  MOVE '_'       TO M-SELO(W-IND1)                              
               ELSE                                                             
                  MOVE PROT-DRK  TO M-SELA(W-IND1)                              
               END-IF                                                           
           END-PERFORM.                                                 09710043
                                                                        09720043
       3130-EX. EXIT.                                                   09730043
                                                                        09740043
                                                                        09750043
                                                                        09760043
                                                                        09770043
       3140-INVIO-MAPPA SECTION.                                        08000000
      *----------------*                                                08010000
           MOVE '3140-INVIO-MAPPA' TO ULT-LABEL.                        08020000
      *---                                                              08030000
           IF W-NOME-PGM = COM-NOME-PGM                                 08040000
            THEN                                                        09830043
              EXEC CICS SEND                                            09840043
                        MAP    (W-NOME-MAPPA)                           09850043
                        MAPSET (W-NOME-MAPSET)                          09860043
                        FROM   (MF05MAPO)                               09860043
                        CURSOR                                          09870043
                        DATAONLY
                        FREEKB
              END-EXEC                                                  09880043
            ELSE                                                        09890043
              MOVE W-NOME-PGM  TO COM-NOME-PGM                          09900043
              EXEC CICS SEND                                            09910043
                        MAP    (W-NOME-MAPPA)                           09920043
                        MAPSET (W-NOME-MAPSET)                          09930043
                        FROM   (MF05MAPO)                               09930043
                        CURSOR                                          09940043
                        ERASE
                        FREEKB                                          09940043
              END-EXEC                                                  09950043
           END-IF.                                                      08200000
                                                                        08210000
           EXEC CICS RETURN                                             08220000
                     TRANSID  ('RR05')                                  08230000
                     COMMAREA (W-COMMAREA)                              08240000
                     LENGTH   (W-LEN)                                   08250000
                     END-EXEC.                                          10020043
                                                                        08270000
       3140-EX. EXIT.                                                   08280000
                                                                        08290000
                                                                        08300000
                                                                        08310000
       3200-PASSA-CTL SECTION.                                          08320000
      *--------------*                                                  08330000
           MOVE '3200-PASSA-CTL' TO ULT-LABEL.                          08340000
                                                                        10160043
           MOVE W-NOME-PGM TO COM-NOME-PGM.                             10170043
                                                                        10180043
           EXEC CICS XCTL                                               08360000
                     PROGRAM  (W-XCTL-PGM)                              08370000
                     COMMAREA (W-COMMAREA)                              08380000
                     LENGTH   (W-LEN)                                   08390000
           END-EXEC.                                                    08400000
                                                                        08410000
       3200-EX. EXIT.                                                   08420000
