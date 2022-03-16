       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW07DEMO.                                            00020000
      * ---                                                             00030000
      ******************************************************************00040000
      * PROGETTO        :                                              *00050000
      * ID. TRANSAZIONE :                                              *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :                                              *00080000
      * OGGETTO         :                                              *        
      * TIPO OPERAZIONE : AGGIORNAMENTO                                *00100000
      * RIFERIMENTO P.E.:                                              *        
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 05/03/1999                                   *        
      * ULTIMA MODIFICA : 05/03/1999                                 *          
      ******************************************************************00150000
      * ---                                                             00160000
       ENVIRONMENT DIVISION.                                            00170000
       CONFIGURATION SECTION.                                           00180000
       DATA DIVISION.                                                   00190000
       WORKING-STORAGE SECTION.                                         00200000
       01  FILLER      PIC X(14) VALUE 'INIZIO WORKING'.                00210000
      * -------------------------------------------------------------- *00220000
      *      DEFINIZIONE VARIABILI HOST                               * 00230000
      * -------------------------------------------------------------- *00240000
       01  W-MSG-HOST                      PIC  X(3)  VALUE SPACE.      00250000
       01  W-SQLCODE                       PIC S9(3)  COMP VALUE +0.    00260000
           88  W-SQLCODE-OK          VALUE +0.                          00270000
           88  W-SQLCODE-NOT-FOUND   VALUE +100.                        00280000
       01  W-COUNT                         PIC S9(4)  COMP-3 VALUE +0.  00290000
       01  W-COGN                          PIC X(30) VALUE SPACES.              
       01  W-NOME                          PIC X(15) VALUE SPACES.              
       01  W-A-NASC-NUM                    PIC 9(4)  VALUE ZEROES.              
       01  W-A-NASC                        PIC X(4)  VALUE SPACES.              
       01  W-M-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-G-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-QU-IN                         PIC X(08) VALUE SPACES.              
       01  W-COD-FISC                      PIC X(16) VALUE SPACES.              
       01  W-MATR                          PIC 9(5)  VALUE ZEROES.              
       01  W-MATRICOLA                    PIC S9(5)V USAGE COMP-3.              
       01  W-CONF-OP                       PIC X(2) VALUE SPACES.               
       01  W-DATA-NASCITA.                                                      
         02  W-AA-NASC                     PIC X(4)  VALUE SPACES.              
         02  FILLER                        PIC X     VALUE '-'.                 
         02  W-MM-NASC                     PIC X(2)  VALUE SPACES.              
         02  FILLER                        PIC X     VALUE '-'.                 
         02  W-GG-NASC                     PIC X(2)  VALUE SPACES.              
       01 FILLER                           PIC X(10) VALUE SPACES.      00380000
                                                                        00390000
      * -------------------------------------------------------------- *01300000
      *    W.S. PER ROUTINE DI CONTROLLO DATA                          *01310000
      * -------------------------------------------------------------- *01320000
                                                                        01400000
       01  W-APPO-DATA.                                                 01330000
           02  APPO-GIORNO                   PIC X(2).                  01340000
           02  FILLER                        PIC X VALUE SPACES.        01340000
           02  APPO-MESE                     PIC X(2).                  01350000
           02  FILLER                        PIC X VALUE SPACES.        01350000
           02  APPO-ANNO                     PIC X(4).                  01360000
                                                                        01370000
       01  APPO-DATA.                                                   01330000
           02  APPO-AA                     PIC 99.                      01340000
           02  APPO-MM                     PIC 99.                      01350000
           02  APPO-GG                     PIC 99.                      01360000
                                                                        01370000
       01  QUOZIENTE                       PIC 99.                      01380000
       01  RESTO                           PIC 99.                      01390000
       01  RISULTATO                       PIC 99.                      01390000
      *                                                                         
       01  TABELLA.                                                     01410000
           03  FILLER                      PIC XX     VALUE '31'.       01420000
           03  FILLER                      PIC XX     VALUE '28'.       01430000
           03  FILLER                      PIC XX     VALUE '31'.       01440000
           03  FILLER                      PIC XX     VALUE '30'.       01450000
           03  FILLER                      PIC XX     VALUE '31'.       01460000
           03  FILLER                      PIC XX     VALUE '30'.       01470000
           03  FILLER                      PIC XX     VALUE '31'.       01480000
           03  FILLER                      PIC XX     VALUE '31'.       01490000
           03  FILLER                      PIC XX     VALUE '30'.       01500000
           03  FILLER                      PIC XX     VALUE '31'.       01510000
           03  FILLER                      PIC XX     VALUE '30'.       01520000
           03  FILLER                      PIC XX     VALUE '31'.       01530000
                                                                        01540000
       01  TAB-GG REDEFINES TABELLA.                                    01550000
           03  GG-TAB  OCCURS  12          PIC 99.                      01560000
                                                                        01570000
                                                                        01580000
      * -------------------------------------------------------------- *01590000
      *    DEFINIZIONE CAMPI STANDART DELLA TRANSAZIONE                *01600000
      * -------------------------------------------------------------- *01610000
       01  W-CTL-END                       PIC  X(4)   VALUE 'LOOP'.    01630000
       01  W-NOME-PGM                     PIC  X(8)   VALUE 'CW07DEMO'. 01640000
       01  W-PGM-LOGON                    PIC  X(8)   VALUE 'CW01DEMO'. 01640000
       01  W-PGM-MENU-GEN                 PIC  X(8)   VALUE 'CW02DEMO'. 01640000
       01  W-PGM-MENU-RAMO                PIC  X(8)   VALUE 'CW03DEMO'. 01640000
       01  W-TRS-ID                        PIC  X(4)   VALUE SPACE.     01650000
       01  W-XCTL-PGM                      PIC  X(8)   VALUE SPACE.     01660000
       01  W-ULT-LABEL                     PIC  X(15)  VALUE SPACES.    01670000
       01  W-LEN                           PIC S9(4)   COMP VALUE +250.         
       01  W-TERMID                        PIC  X(4)   VALUE SPACE.     01700000
                                                                        01720000
       01  CPSCODA.                                                     00630043
           02  FILLER                        PIC XXX    VALUE 'CPS'.    00640043
           02  CPS-TERM                      PIC X(4)   VALUE SPACES.   00650043
           02  CPS-NUM-CODA                  PIC X      VALUE '1'.      00660043
       01  CPS-LEN-CODA                      PIC S9(4) COMP VALUE +690. 00670043
       01  CPS-ITEM-CODA                     PIC S9(4) COMP VALUE +0.   00680043
      *                                                                 00680043
       01  CPS-DENOM.                                                   00690043
           05  CPS-MATR                   PIC 9(05).                    00710043
           05  CPS-COD-FISC               PIC X(16).                            
           05  CPS-COGN                   PIC X(30).                            
           05  CPS-NOME                   PIC X(15).                    00720043
           05  CPS-AA-NASC                PIC X(04).                    00730043
           05  CPS-MM-NASC                PIC X(02).                    00730043
           05  CPS-GG-NASC                PIC X(02).                    00730043
           05  CPS-QU-IN                  PIC X(08).                    00740043
       01 W-ITEM-MAP                          PIC 9(04) VALUE ZEROES.           
       01 W-IND1                              PIC S9(04) COMP VALUE 0.          
       01 W-RIGA                              PIC 9(02) VALUE ZEROES.           
                                                                        01730000
      * -------------------------------------------------------------- *01980000
      *    DEFINIZIONE  MESSAGGIO ABEND SQL                            *01990000
      * -------------------------------------------------------------- *02000000
       01  ERR-SQL.                                                     02010000
           02  FILLER                      PIC X(11)                    02020000
                                           VALUE 'ERRORE SQL '.         02030000
           02  SQL-CODICE                  PIC ----.                    02040000
           02  FILLER                      PIC X(12)                    02050000
                                           VALUE ' ALLA LABEL '.        02060000
           02  W-ULT-LABEL-SQL             PIC X(15).                   02070000
           02  FILLER                      PIC X(10)                    02080000
                                           VALUE ' TRANSID: '.          02090000
           02  TRS-ID-SQL                  PIC X(4).                    02100000
           02  FILLER                      PIC X(06)                    02110000
                                           VALUE ' PGM: '.              02120000
           02  NOME-PGM-SQL                PIC X(8).                    02130000
           02  FILLER                      PIC X(4).                    02140000
                                                                        02150000
                                                                        02160000
      * -------------------------------------------------------------- *02170000
      *    DEFINIZIONE  MESSAGGIO ABEND CICS                           *02180000
      * -------------------------------------------------------------- *02190000
       01  APPOGGIO-CICS                   PIC X(79)  VALUE SPACES.     02200000
       01  ERR-CICS.                                                    02210000
           02  FILLER                      PIC X(12)                    02220000
                                           VALUE 'ERRORE CICS '.        02230000
           02  COD-ERR                     PIC X(4).                    02240000
           02  FILLER                      PIC X(12)                    02250000
                                           VALUE ' ALLA LABEL '.        02260000
           02  W-ULT-LABEL-CICS            PIC X(15).                   02270000
           02  FILLER                      PIC X(10)                    02280000
                                           VALUE ' TRANSID: '.          02290000
           02  TRS-ID-CICS                 PIC X(4).                    02300000
           02  FILLER                      PIC X(06)                    02310000
                                           VALUE ' PGM: '.              02320000
           02  NOME-PGM-CICS               PIC X(8).                    02330000
           02  FILLER                      PIC X(8).                    02340000
                                                                        02350000
                                                                        02360000
      * -------------------------------------------------------------- *02370000
      *    DEFINIZIONE SQLA E TABELLE                                  *02380000
      * -------------------------------------------------------------- *02390000
           EXEC SQL  INCLUDE SQLCA  END-EXEC.                           02400000
      * ---                                                             02410000
           EXEC SQL  INCLUDE CWDIPE         END-EXEC.                           
           EXEC SQL  DECLARE CURDIP CURSOR FOR                          02470000
                     SELECT COD_MATRICOLA_DIP,                                  
                            COGNOME,                                            
                            NOME,                                               
                            DATA_NASCITA,                                       
                            QUALIFICA_INTERNA,                                  
                            COD_FISC                                            
                     FROM CPS04.CWDIPENDENTI                                    
                     WHERE COGNOME LIKE :W-COGN                                 
                     ORDER BY COGNOME                                           
           END-EXEC.                                                            
      * -------------------------------------------------------------- *02500000
      *    DEFINIZIONE DELLA MAPPA                                     *02510000
      * -------------------------------------------------------------- *02520000
           COPY MF07MAP.                                                02530000
                                                                        02540000
                                                                        02550000
                                                                        02560000
      * -------------------------------------------------------------- *02570000
      *    DEFINIZIONE DELLE COPY COMUNI                               *02580000
      * -------------------------------------------------------------- *02590000
           COPY CWATTRIB.                                               02600000
           COPY CWMESS.                                                 02610000
           COPY DFHAID.                                                 02620000
                                                                        02630000
                                                                        02640000
      * -------------------------------------------------------------- *02650000
      * DEFINIZIONE AREA DI COMUN. PER LINK IET010CT (ALLINEAMENTO)    *02660000
      * -------------------------------------------------------------- *02670000
           COPY CWC010.                                                 02680000
                                                                        02690000
           COPY CWC012.                                                 02700000
                                                                        02710000
      * -------------------------------------------------------------- *02720000
      *    DEFINIZIONE COMMAREA.                                       *        
      * -------------------------------------------------------------- *02760000
       01  FILLER      PIC X(15) VALUE 'INIZIO COMMAREA'.               00210000
       01  W-COMMAREA.                                                  02770000
           COPY CWCOMMA.                                                02780000
                                                                        02846000
                                                                        02847000
       LINKAGE SECTION.                                                 02848000
      * ---                                                             02849000
       01  DFHCOMMAREA.                                                         
           05  FILLER  PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.           
      *                                                                         
                                                                        02860000
       PROCEDURE DIVISION.                                              02870000
      *---                                                              02880000
       0000-MAIN SECTION.                                               02890000
      *---------*                                                       02900000
           PERFORM 1000-INIZIO-ELAB.                                    02910000
           PERFORM 2000-CORPO-ELAB.                                     02920000
           PERFORM 3000-FINE-ELAB.                                      02930000
           GOBACK.                                                      02940000
                                                                        02950000
                                                                        02980000
       1000-INIZIO-ELAB SECTION.                                        02990000
      *----------------*                                                03000000
           MOVE EIBTRNID TO W-TRS-ID.                                   03010000
           MOVE EIBTRMID TO CPS-TERM.                                           
           PERFORM 1100-COND-ANOMAL.                                    03020000
           PERFORM 1300-TESTA-RIEN.                                     03030000
                                                                        03040000
           IF COM-GIRO = '1'                                            03050000
           THEN                                                                 
               PERFORM 2900-RIEMP-MASK                                  03070000
           END-IF.                                                      03100000
      *                                                                         
           IF COM-GIRO = '2'                                            03050000
              PERFORM 1200-TASTI-FUNZ                                   03070000
           END-IF.                                                      03100000
                                                                                
           IF COM-GIRO = '3'                                                    
             PERFORM CONTROLLI2                                                 
             PERFORM  1400-TASTI-LAST                                   04670000
           END-IF.                                                      03100000
                                                                        03110000
       1000-EX. EXIT.                                                   03120000
                                                                        03160000
      *---------------*                                                 03180000
       1100-COND-ANOMAL SECTION.                                        03170000
      *---------------*                                                 03180000
           EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        03190000
                                                           END-EXEC.    03200000
           EXEC CICS HANDLE CONDITION  MAPFAIL (1120-COND-MFAIL)        03210000
                                                           END-EXEC.    03230000
                                                                        03240000
           EXEC SQL WHENEVER SQLERROR GO TO 2998-DBERROR   END-EXEC.    03250000
                                                                        03260000
       1100-EX. EXIT.                                                   03270000
                                                                        03280000
                                                                        03290000
                                                                        03300000
                                                                        03310000
       1110-ABEND-CICS SECTION.                                         03320000
      *---------------*                                                 03330000
           MOVE W-ULT-LABEL TO W-ULT-LABEL-CICS.                        03340000
           MOVE W-TRS-ID   TO TRS-ID-CICS.                              03350000
           MOVE W-NOME-PGM TO NOME-PGM-CICS.                            03360000
           EXEC CICS ASSIGN ABCODE (COD-ERR) END-EXEC.                  03370000
           EXEC CICS HANDLE ABEND CANCEL END-EXEC.                      03380000
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       03390000
           MOVE ERR-CICS   TO  M-MSG-1O.                                03400000
           PERFORM 3000-FINE-ELAB.                                      03410000
                                                                        03420000
       1110-EX. EXIT.                                                   03430000
                                                                        03440000
                                                                        03450000
                                                                        03460000
                                                                        03470000
       1120-COND-MFAIL SECTION.                                         03480000
      *---------------*                                                 03490000
           MOVE '1120-COND-MFAIL'   TO    W-ULT-LABEL.                          
      *-------                                                                  
           MOVE '** ERRORE DI MPFAIL **' TO ERR-CICS.                   02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
                                                                        03510000
       1120-EX. EXIT.                                                   03520000
                                                                        03670000
       1200-TASTI-FUNZ SECTION.                                         03680000
      *---------------*                                                 03690000
           EXEC CICS HANDLE AID                                         03700000
                                PA1    (1210-TASTO-PA1)                         
                                CLEAR  (1220-TASTO-CLEAR)               03720000
                                PF3    (1230-TASTO-PF3)                 03730000
                                PF4    (1240-TASTO-PF4)                 03730000
                                ENTER  (1239-TASTO-ENTER)               03740000
                                ANYKEY (1250-TASTO-ANYK)                03750000
                                END-EXEC.                               03760000
       1200-EX. EXIT.                                                   03770000
      *                                                                         
       1210-TASTO-PA1  SECTION.                                                 
      *----                                                                     
           MOVE '1210-TASTO-PA1'  TO W-ULT-LABEL.                               
      *-----                                                                    
           MOVE '** RITORNO AL CICS  **' TO ERR-CICS.                   02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
      *                                                                         
       1210-EX. EXIT.                                                           
      *                                                                         
                                                                        03780000
       1220-TASTO-CLEAR SECTION.                                        03950000
      *--------------*                    * RITORNO MENU' PRINCIPALE *  03960000
           MOVE +250      TO W-LEN.                                             
           PERFORM 2120-NORMALIZZA.                                             
           PERFORM 3050-DELETE-TS.                                              
           MOVE SPACES TO W-MSG-HOST.                                           
           EXEC CICS XCTL PROGRAM  ('CW03DEMO')                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04020000
       1220-EX. EXIT.                                                   04030000
      *                                                                         
       1230-TASTO-PF3 SECTION.                                          04080000
      *--------------*                     - RITORNO MENU' GENERALE   - 04090000
           MOVE '1230-TASTO-PF3' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           PERFORM 2120-NORMALIZZA.                                             
           PERFORM 3050-DELETE-TS.                                              
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE 'CW02DEMO'       TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1230-EX. EXIT.                                                   04170000
                                                                        04180000
      *                                                                         
       1240-TASTO-PF4     SECTION.                                      04080000
      *--------------*                     - RITORNO MENU' GENERALE   - 04090000
           MOVE '1240-TASTO-PF4' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           PERFORM 2120-NORMALIZZA.                                             
           PERFORM 3050-DELETE-TS.                                              
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE 'CW03DEMO'       TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1240-EX. EXIT.                                                   04170000
                                                                                
       1239-TASTO-ENTER  SECTION.                                       04040000
      *----                                                                     
           MOVE '1239-TASTO-ENTER' TO W-ULT-LABEL.                              
      *-----                                                                    
           MOVE 'CW07DEMO' TO W-XCTL-PGM.                                       
           MOVE SPACES TO M-MSG-1O M-MSG-2O.                                    
           MOVE SPACES TO W-MSG-HOST.                                           
           MOVE -1  TO M-COGNL.                                                 
           PERFORM 2200-CONTROLLI.                                              
       EX-1239. EXIT.                                                   04060000
      *                                                                 04070000
                                                                                
       1250-TASTO-ANYK SECTION.                                         04220000
      *--------------*                                                  04230000
                                                                        04240000
           IF COM-GIRO = '2'                                            04250000
            THEN                                                        04260000
              MOVE '013' TO W-MSG-HOST                                  04270000
            ELSE                                                        04280000
              MOVE '006' TO W-MSG-HOST                                  04290000
           END-IF.                                                      04300000
                                                                        04310000
           MOVE -1    TO M-COGNL.                                               
           PERFORM 2999-CERCA-ERR.                                      04330000
                                                                        04340000
       1250-EX. EXIT.                                                   04350000
                                                                        04360000
                                                                        04370000
                                                                        04380000
                                                                        04390000
       1300-TESTA-RIEN SECTION.                                         04400000
      *---------------*                                                 04410000
                                                                                
           IF EIBCALEN = ZERO                                           04420000
            THEN                                                        04430000
              PERFORM 1310-TRANS-DIS                                    04440000
           END-IF.                                                      04450000
                                                                        04460000
           MOVE DFHCOMMAREA TO W-COMMAREA.                              04470000
                                                                        04480000
       1300-EX. EXIT.                                                   04490000
                                                                        04500000
                                                                        04510000
                                                                        04520000
                                                                        04530000
       1310-TRANS-DIS SECTION.                                          04540000
      *--------------*                                                  04550000
           MOVE '*** TRANSAZIONE NON PERMESSA ***' TO ERR-CICS.         04560000
           EXEC CICS SEND TEXT FROM   (ERR-CICS)                        04570000
                               LENGTH (78)                              04580000
                               ERASE WAIT END-EXEC.                     04590000
           EXEC CICS RETURN END-EXEC.                                   04600000
                                                                        04610000
       1310-EX. EXIT.                                                   04620000
                                                                        04630000
                                                                        04640000
                                                                        04650000
                                                                        04660000
       1400-TASTI-LAST SECTION.                                         04670000
      *---------------*                                                 04680000
           EXEC CICS HANDLE AID                                         04690000
                                CLEAR  (1220-TASTO-CLEAR)               04710000
                                PF3    (1230-TASTO-PF3)                 04710000
                                PF4    (1240-TASTO-PF4)                 04710000
                                ENTER  (1425-CONTROLLA-CONF)            04740000
                                PF7    (1237-TASTO-PF7)                 04740000
                                PF8    (1238-TASTO-PF8)                 04740000
                                ANYKEY (1250-TASTO-ANYK)                04750000
                                END-EXEC.                               04760000
       1400-EX. EXIT.                                                   04770000
                                                                        05140000
       1425-CONTROLLA-CONF SECTION.                                     05310000
      *-----------------*                                               05320000
           MOVE '1425-CONTROLLA-CONF' TO W-ULT-LABEL.                   05330000
      * ---                                                             05340000
           MOVE SPACES TO M-MSG-1O M-MSG-2O RC-IET012CT.                        
      *                                                                         
           PERFORM 2500-CONFERMA.                                               
           MOVE    M-CONF-OPI TO W-CONF-OP.                                     
                                                                                
                                                                                
           IF W-CONF-OP  NOT EQUAL 'SI' AND                                     
              W-CONF-OP  NOT EQUAL 'NO'                                         
              MOVE -1    TO M-CONF-OPL                                          
              MOVE '016'  TO W-MSG-HOST                                         
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                              
      *                                                                         
                                                                                
           IF W-CONF-OP  = 'SI'                                                 
              PERFORM 1426-AGGIORNAMENTO                                        
              PERFORM 3050-DELETE-TS                                            
              MOVE '1' TO COM-GIRO                                              
              MOVE W-COMMAREA  TO DFHCOMMAREA                                   
              PERFORM 1000-INIZIO-ELAB                                          
           END-IF.                                                              
      *                                                                         
           IF W-CONF-OP  = 'NO'                                                 
           MOVE -1 TO M-COGNL                                                   
           MOVE SPACE   TO M-TEST-CONFO                                         
                           M-CONF-OPO                                           
                           M-TES-FISSOO                                         
                           M-MSG-1O                                             
                           M-MSG-2O                                             
           MOVE PROT    TO M-CONF-OPA                                           
           MOVE     UNPROT  TO M-COGNA                                          
                               M-NOMEA                                          
                               M-A-NASCA                                        
                               M-M-NASCA                                        
                               M-G-NASCA                                        
                               M-QU-INA                                         
                               M-MATRA                                          
                               M-COD-FISCA                                      
                                                                                
           END-IF.                                                              
                                                                                
           MOVE SPACE   TO M-TEST-CONFO                                         
                           M-CONF-OPO                                           
                           M-TES-FISSOO.                                        
           MOVE PROT    TO M-CONF-OPA.                                          
                                                                                
           PERFORM 2130-NORM-ATTR.                                      05370000
           PERFORM 3000-FINE-ELAB.                                      05380000
                                                                        05390000
                                                                        05400000
       1425-EX. EXIT.                                                   05410000
                                                                        05420000
       1426-AGGIORNAMENTO SECTION.                                      05450000
      *                                                                         
           PERFORM 2120-NORMALIZZA.                                             
      *---------------*                                                 05460000
           MOVE '1426-AGGIORNAMENTO' TO W-ULT-LABEL.                    05470000
      *----                                                             05480000
           PERFORM TESTA-DATA.                                                  
           MOVE W-A-NASC          TO W-AA-NASC.                                 
           MOVE W-M-NASC          TO W-MM-NASC.                                 
           MOVE W-G-NASC          TO W-GG-NASC.                                 
           MOVE W-COGN            TO COGNOME  OF DCL-CPSDIP.                    
           MOVE W-NOME            TO NOME     OF DCL-CPSDIP.                    
           MOVE W-DATA-NASCITA    TO DATA-NASCITA  OF DCL-CPSDIP.               
           MOVE W-QU-IN           TO QUALIFICA-INTERNA   OF DCL-CPSDIP.         
           MOVE W-MATRICOLA       TO COD-MATRICOLA-DIP   OF DCL-CPSDIP.         
           MOVE SPACES TO RC-IET012CT.                                          
      *                                                                         
           IF W-COD-FISC NOT EQUAL SPACES                                       
               MOVE W-COD-FISC TO CF-IET012CT                                   
               MOVE 20         TO LL-IET012CT                                   
               PERFORM LINK THRU LINK-EX                                        
               PERFORM TEST-RITORNO THRU TEST-RITORNO-EX                        
           END-IF.                                                              
           MOVE W-COD-FISC        TO COD-FISC   OF DCL-CPSDIP.                  
                                                                                
           EXEC SQL UPDATE  CPS04.CWDIPENDENTI                                  
                                                                                
                    SET COGNOME = :DCL-CPSDIP.COGNOME,                          
                        NOME    = :DCL-CPSDIP.NOME,                             
                        DATA_NASCITA = :DCL-CPSDIP.DATA-NASCITA,                
                        QUALIFICA_INTERNA =                                     
                                       :DCL-CPSDIP.QUALIFICA-INTERNA,           
                        COD_FISC = :DCL-CPSDIP.COD-FISC                         
                    WHERE COD_MATRICOLA_DIP =                                   
                                    :DCL-CPSDIP.COD-MATRICOLA-DIP               
           END-EXEC.                                                    05010000
      *                                                                         
                                                                                
       1426-EX. EXIT.                                                           
      *                                                                         
       1237-TASTO-PF7   SECTION.                                        07480000
      *--------------------*                                            07490000
           MOVE '1237-TASTO-PF7' TO W-ULT-LABEL.                        07500000
      * ---                                                             07510000
      * ---                                                                     
           IF COM-ITEM > 1                                                      
           THEN                                                                 
               SUBTRACT 1 FROM COM-ITEM                                         
               MOVE COM-ITEM TO W-IND1                                          
               MOVE -1 TO M-COGNL                                               
               PERFORM   2130-NORM-ATTR                                         
               MOVE SPACES TO M-MSG-1O M-MSG-2O RC-IET012CT                     
               MOVE SPACES TO W-MSG-HOST                                        
               PERFORM PREPARA-MAP                                              
               PERFORM 3000-FINE-ELAB                                           
           ELSE                                                                 
               IF COM-ITEM = 1                                                  
                 THEN                                                           
                 MOVE -1 TO M-COGNL                                             
                 MOVE '018' TO W-MSG-HOST                                       
                 PERFORM 2999-CERCA-ERR                                         
               END-IF                                                           
           END-IF.                                                              
                                                                                
       1237-EX. EXIT.                                                   07710000
                                                                                
       1238-TASTO-PF8   SECTION.                                        07480000
      *--------------------*                                            07490000
           MOVE '1527-GEST-AVAN' TO W-ULT-LABEL.                        07500000
      * ---                                                             07510000
      * ---                                                                     
           IF COM-ITEM < COM-TOT-PAG                                            
              ADD 1 TO COM-ITEM                                                 
              MOVE COM-ITEM TO  W-IND1                                          
              MOVE -1 TO M-COGNL                                                
              PERFORM   2130-NORM-ATTR                                          
              MOVE SPACES TO M-MSG-1O M-MSG-2O RC-IET012CT                      
              MOVE SPACES TO W-MSG-HOST                                         
              PERFORM PREPARA-MAP                                               
             ELSE                                                               
              IF COM-ITEM = COM-TOT-PAG                                         
                THEN                                                            
                   MOVE -1    TO M-COGNL                                        
                   MOVE '019' TO W-MSG-HOST                                     
                   PERFORM 2999-CERCA-ERR                                       
              END-IF                                                            
           END-IF.                                                              
      *                                                                         
      *                                                                         
                                                                                
       1238-EX. EXIT.                                                   07710000
      *----                                                                     
       2000-CORPO-ELAB SECTION.                                         05450000
      *---------------*                                                 05460000
           MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       05470000
      *----                                                             05480000
           IF W-NOME-PGM = COM-NOME-PGM                                 05490000
            THEN                                                        05500000
              PERFORM 2100-RECEIVE                                      05510000
              PERFORM 2200-CONTROLLI                                    05520000
            ELSE                                                        05540000
              PERFORM 2900-RIEMP-MASK                                   05550000
           END-IF.                                                      05560000
                                                                        05570000
       2000-EX. EXIT.                                                   05580000
                                                                        05590000
                                                                        05600000
       2100-RECEIVE SECTION.                                            05630000
      *------------*                                                    05640000
           MOVE '2100-RECEIVE' TO W-ULT-LABEL.                          05650000
      * ---                                                             05660000
           PERFORM 2110-REC-MAPPA.                                      05670000
           PERFORM 2120-NORMALIZZA.                                     05680000
           PERFORM 2130-NORM-ATTR.                                      05690000
                                                                        05700000
       2100-EX. EXIT.                                                   05710000
      *                                                                 05720000
       2110-REC-MAPPA SECTION.                                          05760000
      *--------------*                                                  05770000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        05780000
      * ---                                                             05790000
           EXEC CICS RECEIVE MAP    ('MF07MAP')                         05800000
                             MAPSET ('MF07MAP') END-EXEC.               05810000
                                                                        05820000
       2110-EX. EXIT.                                                   05830000
                                                                        05840000
                                                                        05850000
                                                                        05860000
                                                                        05870000
       2120-NORMALIZZA SECTION.                                         05880000
      *---------------*                                                 05890000
           MOVE '2120-NORMALIZZA' TO W-ULT-LABEL.                       05900000
      * ---                                                             05910000
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY SPACES.                   
           INSPECT M-COGNI REPLACING ALL '_' BY SPACES.                         
           MOVE    M-COGNI TO W-COGN.                                           
                                                                        05950000
           INSPECT M-NOMEI  REPLACING ALL LOW-VALUE BY ' '.                     
           INSPECT M-NOMEI  REPLACING ALL '_' BY ' '.                           
           MOVE    M-NOMEI  TO W-NOME.                                          
                                                                        05950000
           INSPECT M-A-NASCI  REPLACING ALL LOW-VALUE BY ' '.                   
           INSPECT M-A-NASCI  REPLACING ALL '_' BY ' '.                         
           MOVE    M-A-NASCI  TO W-A-NASC.                                      
                                                                        05950000
           INSPECT M-M-NASCI  REPLACING ALL LOW-VALUE BY ' '.                   
           INSPECT M-M-NASCI  REPLACING ALL '_' BY ' '.                         
           MOVE    M-M-NASCI  TO W-M-NASC.                                      
                                                                        05950000
           INSPECT M-G-NASCI  REPLACING ALL LOW-VALUE BY ' '.                   
           INSPECT M-G-NASCI  REPLACING ALL '_' BY ' '.                         
           MOVE    M-G-NASCI  TO W-G-NASC.                                      
                                                                        05981001
           INSPECT M-QU-INI    REPLACING ALL LOW-VALUE BY ' '.                  
           INSPECT M-QU-INI    REPLACING ALL '_' BY ' '.                        
           MOVE    M-QU-INI    TO W-QU-IN.                                      
                                                                        05990000
           INSPECT M-MATRI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-MATRI REPLACING ALL '_' BY ' '.                            
           MOVE    M-MATRI TO W-MATR.                                           
           MOVE    W-MATR  TO W-MATRICOLA.                                      
      *                                                                         
           INSPECT M-COD-FISCI REPLACING ALL LOW-VALUE BY ' '.                  
           INSPECT M-COD-FISCI REPLACING ALL '_' BY ' '.                        
           MOVE    M-COD-FISCI TO W-COD-FISC.                                   
      *                                                                         
                                                                        06020000
           MOVE    M-CONF-OPI TO  W-CONF-OP.                                    
                                                                                
       2120-EX. EXIT.                                                   06110000
                                                                        06120000
                                                                        06130000
                                                                        06140000
                                                                        06150000
       2130-NORM-ATTR SECTION.                                          06160000
      *--------------*                                                  06170000
           MOVE '2130-NORM-ATTR' TO W-ULT-LABEL.                        06180000
      * ---                                                             06190000
           MOVE        FSET TO M-COGNA                                          
                               M-NOMEA                                          
                               M-A-NASCA                                        
                               M-M-NASCA                                        
                               M-G-NASCA                                        
                               M-QU-INA                                         
                               M-MATRA                                          
                               M-COD-FISCA.                                     
       2130-EX. EXIT.                                                   06220000
                                                                        06230000
                                                                        06240000
                                                                        06250000
                                                                        06260000
       2200-CONTROLLI SECTION.                                          06270000
      *--------------*                                                  06280000
           MOVE '2200-CONTROLLI' TO W-ULT-LABEL.                        06290000
      * ---                                                             06300000
                                                                        06370000
           IF COM-GIRO = '2'                                                    
           THEN                                                                 
               PERFORM CONTROLLI1                                               
           ELSE                                                                 
               IF COM-GIRO = '3'                                                
                  PERFORM CONTROLLI2                                            
               END-IF                                                           
           END-IF.                                                              
      *                                                                         
       2200-EX. EXIT.                                                   07180000
      *                                                                         
       CONTROLLI1 SECTION.                                                      
      *---                                                                      
           MOVE 'CONTROLLI1'  TO W-ULT-LABEL.                                   
      *------                                                                   
           MOVE SPACE TO M-MSG-1O                                       06380000
                         M-MSG-2O                                       06380000
                         RC-IET012CT                                            
                         M-TEST-CONFO                                   06380000
      *                  M-CONF-OPO                                     06380000
                         M-TES-FISSOO.                                  06380000
           INSPECT M-COGNO REPLACING ALL '_' BY SPACES.                 06440000
      *                                                                         
           INSPECT M-COGNO REPLACING ALL LOW-VALUES BY SPACES.                  
      *                                                                         
           MOVE M-COGNO  TO W-COGN.                                             
      *                                                                         
           IF W-COGN = SPACE OR LOW-VALUE                                       
              MOVE ALL '%'      TO W-COGN                                       
              PERFORM 4902-ACCESSO-SQL                                          
              GO TO 2200-EX                                                     
           END-IF.                                                      06430000
                                                                                
           IF W-COGN NOT EQUAL TO SPACES AND LOW-VALUE                          
              INSPECT W-COGN REPLACING ALL SPACES  BY '%'                       
              INSPECT W-COGN REPLACING ALL LOW-VALUES  BY '%'                   
              PERFORM 4902-ACCESSO-SQL                                          
              GO TO 2200-EX                                                     
      *                                                                 06440000
           END-IF.                                                      06430000
      *                                                                         
       CONTROLLI1-EX. EXIT.                                                     
      **---                                                                     
       CONTROLLI2  SECTION.                                                     
      *---                                                                      
           MOVE 'CONTROLLI2'  TO W-ULT-LABEL.                                   
      *------                                                                   
           IF (M-COGNO    EQUAL  CPS-COGN)    AND                               
              (M-NOMEO    EQUAL  CPS-NOME)    AND                               
              (M-A-NASCO  EQUAL  CPS-AA-NASC) AND                               
              (M-M-NASCO  EQUAL  CPS-MM-NASC) AND                               
              (M-G-NASCO  EQUAL  CPS-GG-NASC) AND                               
              (M-QU-INO   EQUAL  CPS-QU-IN)   AND                               
              (M-MATRO    EQUAL  CPS-MATR)    AND                               
              (M-COD-FISCO EQUAL CPS-COD-FISC)                                  
           THEN                                                                 
              NEXT SENTENCE                                                     
      *    ELSE                                                                 
      *       INSPECT M-CONF-OPI REPLACING ALL LOW-VALUE BY '_'                 
      *       INSPECT M-CONF-OPI REPLACING ALL SPACES BY '_'                    
      *       PERFORM 2500-CONFERMA                                             
           END-IF.                                                              
      *                                                                         
       CONTROLLI2-EX. EXIT.                                             07190000
                                                                        07200000
       TESTA-DATA         SECTION.                                              
      *----------------*                                                07240000
           MOVE 'TESTA-DATA' TO W-ULT-LABEL.                                    
      * ---                                                             07260000
           IF (M-M-NASCO  > '12') OR                                            
              (M-M-NASCO  < '01')                                               
              MOVE FSET-BRT TO M-M-NASCA                                        
              MOVE SPACES   TO M-TES-FISSOO                                     
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
              MOVE PROT     TO M-CONF-OPA                                       
              MOVE -1       TO M-M-NASCL                                        
              MOVE '026'    TO W-MSG-HOST                               06480000
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                      06490000
      *                                                                         
           IF (M-M-NASCI NOT EQUAL SPACES) OR                                   
             (M-M-NASCI NOT EQUAL LOW-VALUE)                                    
              MOVE M-M-NASCI      TO STRINGA                                    
              MOVE 2              TO LL-STRINGA                                 
              MOVE 'D'            TO ALLINEAMENTO                               
              PERFORM 2211-PASSA-ROUTINE                                        
              PERFORM 2215-TEST-RITORNO                                         
              MOVE STRINGA  TO W-M-NASC                                         
              INSPECT W-M-NASC REPLACING ALL LOW-VALUE BY ZEROES                
              INSPECT W-M-NASC REPLACING ALL SPACE BY ZEROES                    
              MOVE W-M-NASC TO M-M-NASCO                                        
           END-IF.                                                              
                                                                        06440000
      *                                                                         
           IF (M-G-NASCI NOT EQUAL SPACES) OR                                   
             (M-G-NASCI NOT EQUAL LOW-VALUE)                                    
              MOVE M-G-NASCI      TO STRINGA                                    
              MOVE 2              TO LL-STRINGA                                 
              MOVE 'D'            TO ALLINEAMENTO                               
              PERFORM 2211-PASSA-ROUTINE                                        
              PERFORM 2216-TEST-RITORNO                                         
              MOVE STRINGA  TO W-G-NASC                                         
              INSPECT W-G-NASC REPLACING ALL LOW-VALUE BY ZEROES                
              INSPECT W-G-NASC REPLACING ALL SPACE BY ZEROES                    
              MOVE W-G-NASC TO M-G-NASCO                                        
           END-IF.                                                              
      *                                                                         
           IF (M-A-NASCI NOT EQUAL SPACES) OR                                   
             (M-A-NASCI NOT EQUAL LOW-VALUE)                                    
              MOVE M-A-NASCI      TO STRINGA                                    
              MOVE 4              TO LL-STRINGA                                 
              MOVE 'D'            TO ALLINEAMENTO                               
              PERFORM 2211-PASSA-ROUTINE                                        
              PERFORM 2217-TEST-RITORNO                                         
              MOVE STRINGA  TO W-A-NASC                                         
              INSPECT W-A-NASC REPLACING ALL LOW-VALUE BY ZEROES                
              INSPECT W-A-NASC REPLACING ALL SPACE BY ZEROES                    
              MOVE W-A-NASC TO M-A-NASCO                                        
           END-IF.                                                              
      *                                                                         
           IF M-M-NASCO = '04' OR '06' OR '09' OR '11'                          
             IF M-G-NASCO > '30' OR < '01'                                      
                MOVE -1     TO M-G-NASCL                                        
                MOVE FSET-BRT TO M-G-NASCA                                      
                MOVE SPACES   TO M-TES-FISSOO                                   
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
                MOVE PROT  TO M-CONF-OPA                                        
                MOVE '026'  TO W-MSG-HOST                                       
                PERFORM 2999-CERCA-ERR                                          
             END-IF                                                             
           ELSE                                                                 
             IF M-M-NASCO NOT = '02'                                            
               IF M-G-NASCO > '31' OR < '01'                                    
                 MOVE -1     TO M-G-NASCL                                       
                 MOVE FSET-BRT TO M-G-NASCA                                     
                 MOVE SPACES   TO M-TES-FISSOO                                  
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
                 MOVE PROT   TO M-CONF-OPA                                      
                 MOVE '026'  TO W-MSG-HOST                                      
                 PERFORM 2999-CERCA-ERR                                         
               END-IF                                                           
             ELSE                                                               
               PERFORM 2221-ANNO-BISESTILE                                      
             END-IF                                                             
           END-IF.                                                              
                                                                                
      *                                                                         
           IF M-A-NASCO < '1000'                                                
              MOVE SPACES   TO M-TES-FISSOO                                     
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
              MOVE PROT    TO  M-CONF-OPA                                       
              MOVE -1                TO M-A-NASCL                               
              MOVE FSET-BRT    TO M-A-NASCA                                     
              MOVE '026'             TO W-MSG-HOST                              
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                              
      *                                                                         
           MOVE COM-DATA-SISTEMA     TO W-APPO-DATA.                            
           IF (M-A-NASCI > APPO-ANNO)                                           
              MOVE -1                TO M-A-NASCL                               
              MOVE FSET-BRT TO  M-A-NASCA                                       
              MOVE SPACES   TO  M-TES-FISSOO                                    
                                M-CONF-OPO                                      
                                M-TEST-CONFO                                    
              MOVE PROT     TO  M-CONF-OPA                                      
              MOVE '029'             TO W-MSG-HOST                              
              PERFORM 2999-CERCA-ERR                                            
           ELSE                                                                 
              IF (M-A-NASCI = APPO-ANNO)                                        
                 IF M-M-NASCI > APPO-MESE                                       
                  MOVE -1           TO M-M-NASCL                                
                  MOVE FSET-BRT TO  M-M-NASCA                                   
                  MOVE SPACES   TO  M-TES-FISSOO                                
                                M-CONF-OPO                                      
                                M-TEST-CONFO                                    
                  MOVE PROT     TO  M-CONF-OPA                                  
                  MOVE '029'             TO W-MSG-HOST                          
                  PERFORM 2999-CERCA-ERR                                        
              ELSE                                                              
                  IF M-M-NASCI = APPO-MESE                                      
                    IF M-G-NASCI > APPO-GIORNO                                  
                     MOVE -1           TO M-G-NASCL                             
                     MOVE FSET-BRT TO  M-G-NASCA                                
                     MOVE SPACES   TO  M-TES-FISSOO                             
                                M-CONF-OPO                                      
                                M-TEST-CONFO                                    
                     MOVE PROT     TO  M-CONF-OPA                               
                     MOVE '029'             TO W-MSG-HOST                       
                     PERFORM 2999-CERCA-ERR                                     
                  ELSE                                                          
                      MOVE UNPROT TO M-CONF-OPA                                 
                      MOVE -1     TO M-CONF-OPL                                 
                    END-IF                                                      
                  END-IF                                                        
              END-IF                                                            
           END-IF.                                                              
      *                                                                         
       2211-PASSA-ROUTINE SECTION.                                              
      *----------------*                                                07240000
           MOVE '2211-PASSA-ROUTINE' TO W-ULT-LABEL.                            
      * ---                                                             07260000
           EXEC CICS LINK  PROGRAM('CW10DEMO')                          07290000
                           COMMAREA(AREA-IET010CT)                      07300000
                           LENGTH(LL-IET010CT)    END-EXEC.             07310000
                                                                        07340000
      *---                                                                      
       2211-EX. EXIT.                                                   07430000
                                                                        07440000
       2215-TEST-RITORNO SECTION.                                               
      *----------------*                                                07240000
           MOVE '2215-TEST-RITORNO' TO W-ULT-LABEL.                             
      * ---                                                             07260000
           IF RC-IET010CT = 'E1'                                        07290000
              MOVE -1      TO M-M-NASCL                                 07300000
              MOVE '001'   TO W-MSG-HOST                                07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
             IF RC-IET010CT = 'E2'                                      07290000
                MOVE -1      TO M-M-NASCL                               07300000
                MOVE '014'   TO W-MSG-HOST                              07300000
                PERFORM 2999-CERCA-ERR                                  07310000
             ELSE                                                               
                IF RC-IET010CT = 'E3'                                   07290000
                   MOVE -1      TO M-M-NASCL                            07300000
                   MOVE '023'   TO W-MSG-HOST                           07300000
                   PERFORM 2999-CERCA-ERR                               07310000
                END-IF                                                          
             END-IF                                                             
           END-IF.                                                              
                                                                        07340000
      *---                                                                      
       2215-EX. EXIT.                                                   07430000
                                                                        07340000
       2216-TEST-RITORNO SECTION.                                               
      *----------------*                                                07240000
           MOVE '2216-TEST-RITORNO' TO W-ULT-LABEL.                             
      * ---                                                             07260000
           IF RC-IET010CT = 'E1'                                        07290000
              MOVE -1      TO M-G-NASCL                                 07300000
              MOVE '001'   TO W-MSG-HOST                                07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
             IF RC-IET010CT = 'E2'                                      07290000
                MOVE -1      TO M-G-NASCL                               07300000
                MOVE '014'   TO W-MSG-HOST                              07300000
                PERFORM 2999-CERCA-ERR                                  07310000
             ELSE                                                               
                IF RC-IET010CT = 'E3'                                   07290000
                   MOVE -1      TO M-G-NASCL                            07300000
                   MOVE '023'   TO W-MSG-HOST                           07300000
                   PERFORM 2999-CERCA-ERR                               07310000
                END-IF                                                          
             END-IF                                                             
           END-IF.                                                              
                                                                        07340000
      *---                                                                      
       2216-EX. EXIT.                                                   07430000
                                                                        07440000
       2217-TEST-RITORNO SECTION.                                               
      *----------------*                                                07240000
           MOVE '2217-TEST-RITORNO' TO W-ULT-LABEL.                             
      * ---                                                             07260000
           IF RC-IET010CT = 'E1'                                        07290000
              MOVE -1      TO M-A-NASCL                                 07300000
              MOVE '001'   TO W-MSG-HOST                                07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
             IF RC-IET010CT = 'E2'                                      07290000
                MOVE -1      TO M-A-NASCL                               07300000
                MOVE '014'   TO W-MSG-HOST                              07300000
                PERFORM 2999-CERCA-ERR                                  07310000
             ELSE                                                               
                IF RC-IET010CT = 'E3'                                   07290000
                   MOVE -1      TO M-A-NASCL                            07300000
                   MOVE '027'   TO W-MSG-HOST                           07300000
                   PERFORM 2999-CERCA-ERR                               07310000
                END-IF                                                          
             END-IF                                                             
           END-IF.                                                              
                                                                        07340000
      *---                                                                      
       2217-EX. EXIT.                                                   07430000
                                                                        07340000
       2221-ANNO-BISESTILE SECTION.                                     08360000
      *------------*                                                    08370000
           MOVE '2221-ANNO-BISESTILE' TO W-ULT-LABEL.                   08380000
      * ---                                                             08390000
           MOVE M-A-NASCO   TO W-A-NASC-NUM.                                    
           DIVIDE 4 INTO W-A-NASC-NUM GIVING RISULTATO                          
                                    REMAINDER RESTO.                            
           IF RESTO = 0                                                         
             IF M-G-NASCO > '29' OR < '01'                                      
                MOVE SPACES TO M-TES-FISSOO                                     
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
                MOVE PROT   TO M-CONF-OPA                                       
                MOVE FSET-BRT  TO M-G-NASCA                                     
                MOVE -1     TO M-G-NASCL                                        
                MOVE '026'  TO W-MSG-HOST                                       
                PERFORM 2999-CERCA-ERR                                          
             END-IF                                                             
           ELSE                                                                 
             IF M-G-NASCO > '28' OR < '01'                                      
                MOVE SPACES TO M-TES-FISSOO                                     
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
                MOVE PROT   TO M-CONF-OPA                                       
                MOVE FSET-BRT  TO M-G-NASCA                                     
                MOVE -1     TO M-G-NASCL                                        
                MOVE '026'  TO W-MSG-HOST                                       
                PERFORM 2999-CERCA-ERR                                          
             END-IF                                                             
           END-IF.                                                              
                                                                                
       2221-EX. EXIT.                                                   07710000
                                                                        07210000
        TESTA-EX. EXIT.                                                 07220000
      *                                                                         
       2210-CARICA-TS     SECTION.                                              
      *----------------*                                                07240000
           MOVE '2210-CARICA-TS ' TO W-ULT-LABEL.                               
      * ---                                                             07260000
           MOVE EIBTRMID    TO  CPS-TERM.                                       
           PERFORM  3050-DELETE-TS.                                             
           MOVE 0   TO CPS-ITEM-CODA.                                   07340000
           MOVE ALL '%'      TO W-COGN.                                         
           PERFORM 4902-ACCESSO-SQL.                                            
      *---                                                                      
           MOVE 1  TO COM-PAG.                                                  
           MOVE 1 TO CPS-ITEM-CODA.                                             
       2210-EX. EXIT.                                                   07430000
                                                                        07440000
                                                                        07450000
                                                                        07460000
                                                                        07470000
       4902-ACCESSO-SQL SECTION.                                        07480000
      *--------------------*                                            07490000
           MOVE '4902-ACCESSO-SQL' TO W-ULT-LABEL.                      07500000
      * ---                                                             07510000
                                                                                
           EXEC SQL OPEN CURDIP  END-EXEC.                                      
           MOVE 1 TO W-IND1.                                                    
      *                                                                         
           PERFORM FETCH-CURDIP UNTIL W-SQLCODE-NOT-FOUND.                      
                                                                                
           IF W-RIGA  = 0                                                       
                PERFORM 3130-FORMATTA-MAPPA                                     
                MOVE -1     TO M-COGNL                                          
                MOVE '011'  TO W-MSG-HOST                                       
                EXEC SQL CLOSE CURDIP  END-EXEC                                 
                PERFORM 2999-CERCA-ERR                                          
           END-IF.                                                              
      *                                                                         
           EXEC SQL CLOSE CURDIP  END-EXEC.                                     
      *                                                                         
           MOVE 1  TO W-IND1    COM-ITEM.                                       
           PERFORM  2225-LEGGI-TS.                                              
           PERFORM  CARICA-MAPPA THRU CARICA-MAPPA-EX.                          
                                                                                
       4902-EX. EXIT.                                                   07710000
      *                                                                 07720000
       FETCH-CURDIP SECTION.                                                    
      *----                                                                     
           MOVE ' FETCH-CURDIP'      TO W-ULT-LABEL.                            
      *------                                                                   
           EXEC SQL FETCH CURDIP                                                
                       INTO :DCL-CPSDIP.COD-MATRICOLA-DIP,                      
                            :DCL-CPSDIP.COGNOME,                                
                            :DCL-CPSDIP.NOME,                                   
                            :DCL-CPSDIP.DATA-NASCITA,                           
                            :DCL-CPSDIP.QUALIFICA-INTERNA,                      
                            :DCL-CPSDIP.COD-FISC                                
           END-EXEC.                                                            
      *                                                                         
           MOVE SQLCODE TO  W-SQLCODE.                                          
           IF W-SQLCODE-OK                                                      
           THEN                                                                 
               PERFORM CARICA-CODA.                                             
      *                                                                         
       FETCH-CURDIP-EX. EXIT.                                                   
      **------                                                                  
       CARICA-CODA   SECTION.                                                   
      *--------                                                                 
           MOVE 'CARICA-CODA'  TO W-ULT-LABEL.                                  
      *--------                                                                 
                                                                                
                MOVE -1       TO M-COGNL.                                       
                MOVE SPACES   TO W-MSG-HOST.                                    
                MOVE SPACES   TO M-MSG-1O.                                      
                MOVE SPACES   TO M-MSG-2O.                                      
                MOVE COGNOME  OF DCL-CPSDIP  TO CPS-COGN.                       
                MOVE NOME     OF DCL-CPSDIP  TO CPS-NOME.                       
                MOVE DATA-NASCITA OF DCL-CPSDIP  TO W-DATA-NASCITA.             
                MOVE W-AA-NASC               TO CPS-AA-NASC.                    
                MOVE W-MM-NASC               TO CPS-MM-NASC.                    
                MOVE W-GG-NASC               TO CPS-GG-NASC.                    
                MOVE QUALIFICA-INTERNA  OF DCL-CPSDIP                           
                                        TO CPS-QU-IN.                           
                MOVE COD-MATRICOLA-DIP  OF DCL-CPSDIP                           
                                        TO CPS-MATR.                            
                MOVE COD-FISC OF DCL-CPSDIP  TO CPS-COD-FISC.                   
      *                                                                         
              PERFORM 2212-WRITE-TS.                                            
              ADD 1 TO W-IND1.                                                  
              MOVE SPACES TO CPS-DENOM.                                         
      *---                                                                      
       CARICA-CODA-EX. EXIT.                                                    
      *-----                                                                    
       2212-WRITE-TS   SECTION.                                                 
      **------                                                                  
           MOVE '2212-WRITE-TS'  TO  W-ULT-LABEL.                               
      *-------------                                                            
           EXEC CICS WRITEQ TS QUEUE(CPSCODA)                                   
                               FROM (CPS-DENOM)                                 
                               LENGTH(CPS-LEN-CODA)                             
                               ITEM(W-IND1)                                     
           END-EXEC.                                                            
           MOVE W-IND1          TO COM-TOT-PAG.                                 
           ADD 1 TO W-RIGA.                                                     
      *------                                                                   
       EX-2212. EXIT.                                                           
      *-------                                                                  
       PREPARA-MAP  SECTION.                                                    
      *--------------------*                                            07490000
           MOVE 'PREPARA-MAP' TO W-ULT-LABEL.                           07500000
      *----                                                             07510000
           PERFORM  2225-LEGGI-TS.                                              
           PERFORM  CARICA-MAPPA THRU CARICA-MAPPA-EX.                          
                                                                                
       PREPARA-MAP-EX. EXIT.                                                    
      *------                                                                   
       2225-LEGGI-TS SECTION.                                                   
      **------                                                                  
           MOVE '2225-LEGGI-TS'  TO  W-ULT-LABEL.                               
      *-------------                                                            
           EXEC CICS READQ TS QUEUE(CPSCODA)                                    
                               INTO (CPS-DENOM)                                 
                               LENGTH(CPS-LEN-CODA)                             
                               ITEM(W-IND1)                                     
           END-EXEC.                                                            
       2225-EX. EXIT.                                                           
      *----------                                                               
                                                                        05400000
                                                                        07750000
                                                                        08650000
                                                                        09000000
       2500-CONFERMA SECTION.                                           09010000
      *-------------*                                                   09020000
           MOVE '2500-CONFERMA' TO W-ULT-LABEL.                         09030000
      *                                                                         
           IF COM-GIRO = '3' AND                                                
             (M-CONF-OPO     EQUAL    SPACES OR LOW-VALUE)                      
           THEN                                                                 
      *                                                                         
               PERFORM 2120-NORMALIZZA                                          
               MOVE SPACES  TO M-MSG-1O                                         
                            M-MSG-2O                                            
                            RC-IET012CT                                         
               MOVE PROT-FSET TO M-COGNA                                        
                             M-NOMEA                                            
                             M-A-NASCA                                          
                             M-M-NASCA                                          
                             M-G-NASCA                                          
                             M-QU-INA                                           
                             M-MATRA                                            
                             M-COD-FISCA                                05400000
      * ---                                                             05340000
               MOVE 'CONFERMA OPERAZIONE:'    TO M-TEST-CONFO                   
               MOVE PROT-BRT  TO M-TEST-CONFA                                   
               MOVE ALL '_'   TO M-CONF-OPO                                     
               MOVE -1        TO M-CONF-OPL                                     
               MOVE UNPROT    TO M-CONF-OPA                                     
               MOVE '(SI/NO)' TO M-TES-FISSOO                                   
               MOVE PROT-BRT  TO M-TES-FISSOA                                   
               PERFORM 3140-INVIO-MAPPA                                         
           END-IF.                                                              
                                                                        09210000
       2500-EX. EXIT.                                                   09220000
                                                                        09230000
                                                                        09240000
                                                                        09250000
                                                                        09260000
       2900-RIEMP-MASK SECTION.                                         09270000
      *---------------*                                                 09280000
           MOVE '2900-RIEMP-MASK' TO W-ULT-LABEL.                       09290000
      *---                                                              09300000
           MOVE LOW-VALUE TO MF07MAPO.                                  09310000
      *--- PERFORM   2210-CARICA-TS.                                            
           MOVE 1 TO CPS-ITEM-CODA COM-PAG.                                     
           PERFORM   2130-NORM-ATTR.                                            
           MOVE -1        TO M-COGNL.                                           
           MOVE ALL '_'   TO M-COGNO                                            
                             M-NOMEO                                            
                             M-A-NASCO                                          
                             M-M-NASCO                                          
                             M-G-NASCO                                          
                             M-QU-INO                                           
                             M-MATRO                                            
                             M-COD-FISCO.                                       
           MOVE SPACE   TO M-TEST-CONFO                                         
                           M-CONF-OPO                                           
                           M-TES-FISSOO.                                        
       2900-EX. EXIT.                                                   09370000
      *                                                                         
       CARICA-MAPPA      SECTION.                                       09380000
      *----                                                                     
           MOVE 'CARICA-MAPPA' TO W-ULT-LABEL.                                  
      *------                                                                   
           MOVE CPS-COGN         TO M-COGNO.                                    
           MOVE CPS-NOME         TO M-NOMEO.                                    
           MOVE CPS-AA-NASC         TO M-A-NASCO.                               
           MOVE CPS-MM-NASC         TO M-M-NASCO.                               
           MOVE CPS-GG-NASC         TO M-G-NASCO.                               
           MOVE CPS-QU-IN         TO M-QU-INO.                                  
           MOVE CPS-MATR         TO M-MATRO.                                    
           MOVE CPS-COD-FISC     TO M-COD-FISCO.                                
           MOVE 3 TO COM-GIRO.                                                  
           PERFORM 3140-INVIO-MAPPA.                                            
      *                                                                 09390000
       CARICA-MAPPA-EX. EXIT.                                           09400000
                                                                        09410000
       2998-DBERROR  SECTION.                                           09420000
      *------------*                                                    09430000
           MOVE W-ULT-LABEL TO W-ULT-LABEL-SQL.                         09440000
           MOVE W-TRS-ID   TO TRS-ID-SQL.                               09450000
           MOVE W-NOME-PGM TO NOME-PGM-SQL.                             09460000
           MOVE SQLCODE    TO SQL-CODICE.                               09470000
           MOVE ERR-SQL    TO M-MSG-1O.                                 09480000
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       09490000
           PERFORM 3000-FINE-ELAB.                                      09500000
                                                                        09510000
       2998-EX. EXIT.                                                   09520000
      *                                                                 09560000
       2999-CERCA-ERR SECTION.                                          09570000
      *--------------*                                                  09580000
           MOVE '2999-CERCA-ERR' TO W-ULT-LABEL.                        09590000
      * ---                                                             09600000
           SET IND-TAB TO 1.                                            09610000
                                                                        09620000
           SEARCH ELEM-TAB-MSG AT END                                   09630000
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'            09640000
                    TO M-MSG-1O                                         09650000
                  WHEN W-MSG-HOST    =  ELEM-COD-MSG(IND-TAB)           09660000
                       MOVE ELEM-DESC-MSG(IND-TAB)  TO M-MSG-1O         09670000
           END-SEARCH.                                                  09680000
                                                                        09690000
           PERFORM 3140-INVIO-MAPPA.                                    09700000
                                                                        09710000
       2999-EX. EXIT.                                                   09720000
      *                                                                 09730000
       LINK SECTION.                                                    09740000
                                                                        09750000
           EXEC CICS LINK  PROGRAM('CW12DEMO')                          07290000
                           COMMAREA(AREA-IET012CT)                      07300000
                           LENGTH(LL-IET012CT)    END-EXEC.             07310000
                                                                        07340000
      *                                                                         
       LINK-EX. EXIT.                                                           
      *                                                                         
       TEST-RITORNO SECTION.                                                    
      **----                                                                    
           MOVE 'TEST-RITORNO'   TO W-ULT-LABEL.                                
      *----                                                                     
           IF RC-IET012CT EQUAL SPACES                                          
               MOVE CF-IET012CT TO W-COD-FISC                                   
                                  M-COD-FISCO                                   
               MOVE PROT-FSET   TO M-COD-FISCA                                  
               GO TO TEST-RITORNO-EX                                            
           ELSE                                                                 
             IF RC-IET012CT NOT EQUAL SPACES                                    
               MOVE SPACES   TO M-TES-FISSOO                                    
                               M-CONF-OPO                                       
                               M-TEST-CONFO                                     
               MOVE PROT     TO M-CONF-OPA                                      
               MOVE FSET-BRT  TO M-COD-FISCA                                    
               MOVE -1        TO M-COD-FISCL                                    
               MOVE '028'     TO W-MSG-HOST                                     
               PERFORM 2999-CERCA-ERR                                           
             END-IF                                                             
           END-IF.                                                              
       TEST-RITORNO-EX. EXIT.                                                   
                                                                                
       3000-FINE-ELAB SECTION.                                          09770000
      * -------------*                                                  09780000
           MOVE '3000-FINE-ELAB' TO W-ULT-LABEL.                        09790000
      * ---                                                             09800000
           IF W-CTL-END = 'LOOP'                                        09810000
              PERFORM 3100-RIENTRO                                      09820000
           ELSE                                                         09830000
              PERFORM 3200-PASSA-CTL                                    09840000
           END-IF.                                                      09850000
                                                                        09860000
       3000-EX. EXIT.                                                   09870000
      *                                                                         
       3050-DELETE-TS        SECTION.                                   09880000
      ****                                                                      
           MOVE '3050-DELETE-TS'  TO W-ULT-LABEL.                               
      *****                                                                     
           MOVE EIBTRMID  TO CPS-TERM.                                          
           EXEC CICS  HANDLE CONDITION QIDERR(3050-EX) END-EXEC.                
           EXEC CICS  DELETEQ TS                                                
                      QUEUE (CPSCODA)                                           
           END-EXEC.                                                    09890000
      *                                                                         
       3050-EX. EXIT.                                                           
      *--                                                                       
       3100-RIENTRO SECTION.                                            09900000
      *------------*                                                    09910000
           MOVE '3100-RIENTRO' TO W-ULT-LABEL.                          09920000
      *---                                                              09930000
           MOVE COM-DATA-SISTEMA   TO M-DATA-SO.                        10070000
           MOVE PROT               TO M-DATA-SA.                                
           PERFORM 3130-FORMATTA-MAPPA.                                 09950000
           PERFORM 3140-INVIO-MAPPA.                                    09960000
                                                                        09970000
       3100-EX. EXIT.                                                   09980000
                                                                        10150000
       3130-FORMATTA-MAPPA SECTION.                                     10160000
      *-------------------*                                             10170000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   10180000
      *---                                                              10190000
           IF M-COGNA = PROT-FSET                                               
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
            ELSE                                                        10230000
              INSPECT M-COGNO REPLACING ALL LOW-VALUE BY '_'                    
              INSPECT M-COGNO REPLACING ALL SPACE BY '_'                        
           END-IF.                                                      10260000
                                                                        10270000
           IF M-NOMEA = PROT-FSET                                               
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
            ELSE                                                        10230000
              INSPECT M-NOMEO REPLACING ALL LOW-VALUE BY '_'                    
              INSPECT M-NOMEO REPLACING ALL SPACE BY '_'                        
           END-IF.                                                      10260000
                                                                        10270000
           IF M-A-NASCA = PROT-FSET                                             
            THEN                                                        10290000
              NEXT SENTENCE                                             10300000
            ELSE                                                        10310000
              INSPECT M-A-NASCO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-A-NASCO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10340000
                                                                                
           IF M-M-NASCA = PROT-FSET                                             
            THEN                                                        10290000
              NEXT SENTENCE                                             10300000
            ELSE                                                        10310000
              INSPECT M-M-NASCO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-M-NASCO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10340000
                                                                                
           IF M-G-NASCA = PROT-FSET                                             
            THEN                                                        10290000
              NEXT SENTENCE                                             10300000
            ELSE                                                        10310000
              INSPECT M-G-NASCO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-G-NASCO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10340000
                                                                        10340000
           IF M-QU-INA = PROT-FSET                                              
            THEN                                                        10342001
              NEXT SENTENCE                                             10343001
            ELSE                                                        10344001
              INSPECT M-QU-INO REPLACING ALL LOW-VALUE BY '_'                   
              INSPECT M-QU-INO REPLACING ALL SPACE BY '_'                       
           END-IF.                                                      10347001
                                                                        10350000
           IF M-MATRA = PROT-FSET                                               
            THEN                                                        10370000
              NEXT SENTENCE                                             10380000
            ELSE                                                        10390000
              INSPECT M-MATRO REPLACING ALL LOW-VALUE BY '_'                    
              INSPECT M-MATRO REPLACING ALL SPACE BY '_'                        
           END-IF.                                                      10420000
                                                                        10430000
           IF M-COD-FISCA = PROT-FSET                                           
            THEN                                                        10370000
              NEXT SENTENCE                                             10380000
            ELSE                                                        10390000
              INSPECT M-COD-FISCO REPLACING ALL LOW-VALUE BY '_'                
              INSPECT M-COD-FISCO REPLACING ALL SPACE BY '_'                    
           END-IF.                                                      10420000
                                                                        10510000
       3130-EX. EXIT.                                                   10680000
      *----                                                             10720000
       3140-INVIO-MAPPA SECTION.                                        10730000
      *----------------*                                                10740000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      10750000
      *---                                                              10760000
           IF W-NOME-PGM = COM-NOME-PGM                                 10770000
            THEN                                                        10780000
              IF COM-GIRO = '1'                                                 
                 MOVE '2' TO COM-GIRO                                           
                 MOVE '017'   TO W-MSG-HOST                                     
                 SET IND-TAB TO 1                                       09610000
                                                                        09620000
                 SEARCH ELEM-TAB-MSG AT END                             09630000
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'            09640000
                    TO M-MSG-1O                                         09650000
                  WHEN W-MSG-HOST    =  ELEM-COD-MSG(IND-TAB)           09660000
                       MOVE ELEM-DESC-MSG(IND-TAB)  TO M-MSG-1O         09670000
                  END-SEARCH                                            09680000
              END-IF                                                            
              EXEC CICS SEND                                            10790000
                        MAP    ('MF07MAP')                              10800000
                        MAPSET ('MF07MAP')                              10810000
                        CURSOR                                          10820000
                        DATAONLY
                        FREEKB
              END-EXEC                                                  10830000
            ELSE                                                        10840000
              MOVE W-NOME-PGM  TO COM-NOME-PGM                          10850000
              MOVE '2' TO COM-GIRO                                              
              EXEC CICS SEND                                            10860000
                        MAP    ('MF07MAP')                              10870000
                        MAPSET ('MF07MAP')                              10880000
                        CURSOR                                          10890000
                        ERASE
                        FREEKB
              END-EXEC                                                  10900000
           END-IF.                                                      10910000
                                                                        10920000
      *                                                                         
           EXEC CICS RETURN                                             10930000
                     TRANSID  ('RR07')                                  10940000
                     COMMAREA (W-COMMAREA)                              10950000
                     LENGTH   (W-LEN)                                   10960000
                     END-EXEC.                                          10970000
                                                                        10980000
       3140-EX. EXIT.                                                   10990000
                                                                        11000000
                                                                        11010000
      *                                                                 11020000
       3200-PASSA-CTL SECTION.                                          11030000
      *--------------*                                                  11040000
           MOVE '3200-PASSA-CTL' TO W-ULT-LABEL.                        11050000
      *---                                                              11060000
           EXEC CICS XCTL                                               11070000
                     PROGRAM  (W-XCTL-PGM)                              11080000
                     COMMAREA (W-COMMAREA)                              11090000
                     LENGTH   (W-LEN)                                   11100000
           END-EXEC.                                                    11110000
                                                                        11120000
       3200-EX. EXIT.                                                   11130000
