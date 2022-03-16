       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW06DEMO.                                            00020000
      * ---                                                             00030000
      ******************************************************************00040000
      * PROGETTO        :  CENTRO PRODUZIONE SOFTWARE                  *00050000
      * ID. TRANSAZIONE :  RR06                                        *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :                                              *00080000
      * OGGETTO         :                                              *        
      * TIPO OPERAZIONE : MENU                                         *00100000
      * RIFERIMENTO P.E.:                                              *        
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 05/03/1999                                   *        
      * ULTIMA MODIFICA : 05/03/1999                                   *        
      ******************************************************************00150000
      * ---                                                             00160000
       ENVIRONMENT DIVISION.                                            00170000
       CONFIGURATION SECTION.                                           00180000
       DATA DIVISION.                                                   00190000
       WORKING-STORAGE SECTION.                                         00200000
                                                                        00210000
      * -------------------------------------------------------------- *00220000
      *      DEFINIZIONE VARIABILI HOST                               * 00230000
      * -------------------------------------------------------------- *00240000
       01  W-COD-MSG-HOST                  PIC  X(3)  VALUE SPACE.      00250000
       01  W-SQLCODE                       PIC S9(3)  COMP VALUE +0.    00260000
           88  W-SQLCODE-OK          VALUE +0.                          00270000
           88  W-SQLCODE-NOT-FOUND   VALUE +100.                        00280000
       01  W-COGN                          PIC X(30) VALUE SPACES.              
       01  W-NOME                          PIC X(15) VALUE SPACES.              
       01  W-COD-FI                        PIC X(16) VALUE SPACES.              
       01  W-A-NASC-NUM                    PIC 9(4)  VALUE ZEROES.              
       01  W-A-NASC                        PIC X(4)  VALUE SPACES.              
       01  W-M-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-G-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-ANNI                          PIC 9(3)  COMP-3 VALUE 0.            
       01  COM-ANNI                        PIC 9(4)  VALUE ZERO.                
       01  W-QU-IN                         PIC X(08) VALUE SPACES.              
       01  W-MATR                          PIC 9(5)  VALUE ZEROES.              
       01  W-MATRICOLA                     PIC S9(5)V USAGE COMP-3.             
       01 DATA-NASCITA.                                                         
         02  DT-GIORNO                     PIC X(2)  VALUE SPACES.              
         02  FILLER                        PIC X(1)  VALUE '/'.                 
         02  DT-MESE                       PIC X(2)  VALUE SPACES.              
         02  FILLER                        PIC X(1)  VALUE '/'.                 
         02  DT-ANNO                       PIC X(4)  VALUE SPACES.              
      * -------------------------------------------------------------- *01590000
      *    DEFINIZIONE CAMPI STANDART DELLA TRANSAZIONE                *01600000
      * -------------------------------------------------------------- *01610000
       01  W-CTL-END                       PIC  X(4)  VALUE 'LOOP'.     01630000
       01  W-NOME-PGM                      PIC  X(8)  VALUE 'CW06DEMO'. 01640000
       01  W-TRS-ID                        PIC  X(4)  VALUE SPACE.      01650000
       01  W-XCTL-PGM                      PIC  X(8)  VALUE 'CW05DEMO'. 01660000
       01  W-ULT-LABEL                     PIC  X(15) VALUE SPACES.     01670000
       01  W-LEN                           PIC S9(4)  COMP VALUE +250.          
       01  W-TERMID                        PIC  X(4)  VALUE SPACE.      01700000
                                                                        01720000
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
      * -------------------------------------------------------------- *02500000
      *    DEFINIZIONE DELLA MAPPA                                     *02510000
      * -------------------------------------------------------------- *02520000
           COPY MF06MAP.                                                02530000
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
      * DEFINIZIONE AREA DI COMUN. PER LINK IET012CT (CODICE FISCALE)  *02660000
      * -------------------------------------------------------------- *02670000
           COPY CWC012.                                                 02680000
                                                                        02690000
      * -------------------------------------------------------------- *02650000
      * DEFINIZIONE AREA DI COMUN. PER LA ROUTINE IETCONTD            * 02660000
      * -------------------------------------------------------------- *02670000
           COPY CWWDATA.                                                02680000
      ****************                                                  02690000
           COPY CWWRANGE.                                               02700000
      ****************                                                  02710000
      * -------------------------------------------------------------- *02720000
      *    DEFINIZIONE COMMAREA.                                       *        
      * -------------------------------------------------------------- *02760000
       01  W-COMMAREA.                                                  02770000
           COPY CWCOMMA.                                                02780000
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
           PERFORM 1100-COND-ANOMAL.                                    03020000
           PERFORM 1300-TESTA-RIEN.                                     03030000
           PERFORM 1200-TASTI-FUNZ.                                     03090000
                                                                        03110000
       1000-EX. EXIT.                                                   03120000
                                                                        03160000
      *---------------*                                                 03180000
       1100-COND-ANOMAL SECTION.                                        03170000
      *---------------*                                                 03180000
      *    EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        03190000
      *                                                    END-EXEC.    03200000
           EXEC CICS HANDLE CONDITION  MAPFAIL (1120-COND-MFAIL)        03210000
                                                           END-EXEC.    03230000
                                                                        03240000
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
                                                                        03470000
       1120-COND-MFAIL SECTION.                                         03480000
      *---------------*                                                 03490000
           MOVE '*** ERRORE DI MPFAIL ***' TO ERR-CICS.                 04560000
           EXEC CICS SEND TEXT FROM   (ERR-CICS)                        04570000
                               LENGTH (78)                              04580000
                               ERASE WAIT END-EXEC.                     04590000
           EXEC CICS RETURN END-EXEC.                                   04600000
                                                                        04610000
       1120-EX. EXIT.                                                   03520000
                                                                        03670000
       1200-TASTI-FUNZ SECTION.                                         03680000
      *---------------*                                                 03690000
           EXEC CICS HANDLE AID                                         03700000
                                PA1    (1210-TASTO-PA1)                 03730000
                                CLEAR  (1220-TASTO-CLEAR)               03720000
                                PF3    (1230-TASTO-PF3)                 03730000
                                PF4    (1240-TASTO-PF4)                 03730000
                                ENTER  ()                               03740000
                                ANYKEY (1250-TASTO-ANYK)                03750000
                                END-EXEC.                               03760000
       1200-EX. EXIT.                                                   03770000
                                                                        03670000
       1210-TASTO-PA1  SECTION.                                         03480000
      *---------------*                                                 03490000
           MOVE '*** USCITA DAL CICS ***' TO ERR-CICS.                  04560000
           EXEC CICS SEND TEXT FROM   (ERR-CICS)                        04570000
                               LENGTH (78)                              04580000
                               ERASE WAIT END-EXEC.                     04590000
           EXEC CICS RETURN END-EXEC.                                   04600000
                                                                        04610000
       1210-EX. EXIT.                                                   03520000
                                                                        03780000
       1220-TASTO-CLEAR SECTION.                                        03950000
      *--------------*                    * RITORNO MENU' GENERALE   *  03960000
           MOVE +250      TO W-LEN.                                             
           EXEC CICS XCTL PROGRAM  ('CW03DEMO')                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04020000
       1220-EX. EXIT.                                                   04030000
                                                                        04040000
                                                                        04050000
                                                                        04060000
                                                                        04070000
       1230-TASTO-PF3 SECTION.                                          04080000
      *--------------*                     - RITORNO PGM PRECEDENTE   - 04090000
           MOVE '1230-TASTO-PF3' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE 'CW03DEMO'       TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1230-EX. EXIT.                                                   04170000
                                                                                
                                                                        04070000
       1240-TASTO-PF4 SECTION.                                          04080000
      *--------------*                     - RITORNO MENU DI RAMO     - 04090000
           MOVE '1240-TASTO-PF4' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE 'CW02DEMO'       TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1240-EX. EXIT.                                                   04170000
                                                                                
       1250-TASTO-ANYK SECTION.                                         04220000
      *--------------*                                                  04230000
                                                                        04240000
           MOVE '006' TO W-COD-MSG-HOST.                                04310000
           MOVE -1    TO M-COGNL.                                               
           PERFORM 2999-CERCA-ERR.                                      04330000
                                                                        04340000
       1250-EX. EXIT.                                                   04350000
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
                                                                        05620000
       2100-RECEIVE SECTION.                                            05630000
      *------------*                                                    05640000
           MOVE '2100-RECEIVE' TO W-ULT-LABEL.                          05650000
      * ---                                                             05660000
           PERFORM 2110-REC-MAPPA.                                      05670000
           PERFORM 2120-NORMALIZZA.                                     05680000
           PERFORM 2130-NORM-ATTR.                                      05690000
                                                                        05700000
       2100-EX. EXIT.                                                   05710000
                                                                        05750000
       2110-REC-MAPPA SECTION.                                          05760000
      *--------------*                                                  05770000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        05780000
      * ---                                                             05790000
           EXEC CICS RECEIVE MAP    ('MF06MAP')                         05800000
                             MAPSET ('MF06MAP') END-EXEC.               05810000
                                                                        05820000
       2110-EX. EXIT.                                                   05830000
                                                                        05870000
       2120-NORMALIZZA SECTION.                                         05880000
      *---------------*                                                 05890000
           MOVE '2120-NORMALIZZA' TO W-ULT-LABEL.                       05900000
      * ---                                                             05910000
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-COGNI REPLACING ALL '_' BY ' '.                            
           MOVE    M-COGNI TO W-COGN.                                           
                                                                        05950000
           INSPECT M-COD-FI REPLACING ALL LOW-VALUE BY ' '.                     
           INSPECT M-COD-FI REPLACING ALL '_' BY ' '.                           
           MOVE    M-COD-FI TO W-COD-FI.                                        
                                                                        06020000
       2120-EX. EXIT.                                                   06110000
                                                                        06150000
                                                                        06150000
       2130-NORM-ATTR SECTION.                                          06160000
      *--------------*                                                  06170000
           MOVE '2130-NORM-ATTR' TO W-ULT-LABEL.                        06180000
      * ---                                                             06190000
           MOVE        FSET TO M-COGNA                                          
                               M-COD-FA.                                        
       2130-EX. EXIT.                                                   06220000
                                                                        06230000
                                                                        06230000
       2200-CONTROLLI SECTION.                                          06270000
      *--------------*                                                  06280000
           MOVE '2200-CONTROLLI' TO W-ULT-LABEL.                        06290000
      * ---                                                             06300000
           INSPECT M-COD-FI REPLACING ALL LOW-VALUE BY ' '.                     
           INSPECT M-COD-FI REPLACING ALL '_' BY ' '.                           
           MOVE    M-COD-FI TO W-COD-FI.                                        
      *                                                                         
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-COGNI REPLACING ALL '_' BY ' '.                            
           MOVE    M-COGNI  TO W-COGN.                                          
      *                                                                         
           MOVE SPACES       TO RC-IET012CT.                                    
      *                                                                         
           IF W-COD-FI NOT EQUAL SPACES                                         
                 MOVE W-COD-FI       TO CF-IET012CT                             
                 MOVE 20             TO LL-IET012CT                             
                 MOVE W-COGN         TO COM-COGNOME                             
                 PERFORM 2210-PASSA-ROUTINE                                     
                 PERFORM 2215-TEST-RITORNO                                      
           END-IF.                                                              
      *                                                                         
           IF (W-COGN = SPACES) OR                                              
              (W-COGN = LOW-VALUE)                                              
             IF (W-COD-FI = SPACES) OR                                          
                (W-COD-FI = LOW-VALUE)                                          
                MOVE W-COD-FI          TO COM-COD-FI                    06440000
                MOVE W-COGN            TO COM-COGNOME                   06440000
             END-IF                                                             
           END-IF.                                                              
      *                                                                         
           IF (W-COGN NOT EQUAL SPACES) OR                                      
              (W-COGN NOT EQUAL LOW-VALUE)                                      
             IF (W-COD-FI = SPACES) OR                                          
                (W-COD-FI = LOW-VALUE)                                          
                MOVE W-COD-FI          TO COM-COD-FI                    06440000
                MOVE W-COGN            TO COM-COGNOME                   06440000
             END-IF                                                             
           END-IF.                                                              
      *                                                                         
           MOVE 'END'             TO W-CTL-END.                                 
      *                                                                         
       2200-EX. EXIT.                                                   07180000
                                                                        06020000
       2210-PASSA-ROUTINE SECTION.                                              
      *----------------*                                                07240000
           MOVE '2210-PASSA-ROUTINE' TO W-ULT-LABEL.                            
      * ---                                                             07260000
           EXEC CICS LINK  PROGRAM('CW12DEMO')                          07290000
                           COMMAREA(AREA-IET012CT)                      07300000
                           LENGTH(LL-IET012CT)     END-EXEC.            07310000
                                                                        07340000
      *---                                                                      
       2210-EX. EXIT.                                                   07430000
                                                                        07220000
                                                                        07440000
       2215-TEST-RITORNO SECTION.                                               
      *----------------*                                                07240000
           MOVE '2215-TEST-RITORNO' TO W-ULT-LABEL.                             
      * ---                                                             07260000
           IF RC-IET012CT = SPACES                                      07290000
              MOVE CF-IET012CT   TO M-COD-FO                                    
                                    COM-COD-FI                                  
              MOVE PROT-FSET     TO M-COD-FA                                    
           ELSE                                                                 
              PERFORM 2216-CODICE-MESSAGGIO                                     
           END-IF.                                                              
                                                                        07340000
      *---                                                                      
       2215-EX. EXIT.                                                   07430000
                                                                        09250000
                                                                        09260000
       2216-CODICE-MESSAGGIO SECTION.                                   09270000
      *---------------*                                                 09280000
           MOVE '2216-CODICE-MESSAGGIO' TO W-ULT-LABEL.                 09290000
      *---                                                              09300000
           IF RC-IET012CT = 'E1'                                        07290000
              MOVE FSET-BRT     TO M-COD-FA                                     
              MOVE -1           TO M-COD-FL                             07300000
              MOVE '001'        TO W-COD-MSG-HOST                       07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
              MOVE FSET-BRT     TO M-COD-FA                                     
              MOVE -1           TO M-COD-FL                             07300000
              MOVE '028'        TO W-COD-MSG-HOST                       07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           END-IF.                                                              
      *---                                                                      
       2216-EX. EXIT.                                                   07430000
                                                                        09250000
                                                                                
       2900-RIEMP-MASK SECTION.                                         09270000
      *---------------*                                                 09280000
           MOVE '2900-RIEMP-MASK' TO W-ULT-LABEL.                       09290000
      *---                                                              09300000
           IF COM-NOME-PGM = 'CW05DEMO'                                         
              PERFORM 2910-RIENTRO                                              
           ELSE                                                                 
              MOVE LOW-VALUE        TO MF06MAPO                         09310000
              MOVE COM-DATA-SISTEMA TO DATAT                            09310000
              PERFORM 1202-CONV-IN-GIUL                                 09310000
              MOVE JUL-DATA         TO M-DT-GIUO                        09310000
              MOVE M-DT-GIUO        TO COM-DT-GIU                       09310000
              MOVE -1               TO M-COGNL                                  
              MOVE COM-DATA-SISTEMA TO M-DATA-SO                                
              MOVE FSET             TO M-COD-FA                                 
           END-IF.                                                              
      *                                                                         
       2900-EX. EXIT.                                                   09370000
                                                                        09410000
                                                                        09410000
       2910-RIENTRO SECTION.                                            09570000
      *--------------*                                                  09580000
           MOVE '2910-RIENTRO' TO W-ULT-LABEL.                          09590000
      * ---                                                             09600000
           IF COM-MESSAGGIO = SPACES OR LOW-VALUE                               
              NEXT SENTENCE                                                     
           ELSE                                                                 
              MOVE COM-MESSAGGIO    TO M-MSG-1O                                 
           END-IF.                                                              
      *                                                                         
           IF COM-COGNOME = SPACES OR LOW-VALUE                                 
      *       MOVE LOW-VALUE        TO CW06MAPO                         09310000
              MOVE -1               TO M-COGNL                                  
              MOVE COM-DATA-SISTEMA TO M-DATA-SO                                
              MOVE COM-DT-GIU       TO M-DT-GIUO                                
              MOVE SPACES           TO M-COD-FO                                 
              MOVE FSET             TO M-COD-FA                                 
           ELSE                                                                 
              MOVE -1         TO M-COGNL                                        
              MOVE PROT-FSET  TO M-COGNA                                        
                                 M-NOMEA                                        
                                 M-A-NASCA                                      
                                 M-M-NASCA                                      
                                 M-G-NASCA                                      
                                 M-MATRA                                        
                                 M-QU-INA                                       
                                 M-COD-FA                                       
              MOVE COM-DATA-SISTEMA        TO M-DATA-SO                         
              MOVE COM-DT-GIU              TO M-DT-GIUO                         
              MOVE COM-COGNOME             TO M-COGNO                           
              MOVE COM-COD-FI              TO M-COD-FO                          
              MOVE COM-NOME                TO M-NOMEO                           
              MOVE COM-DATA-NASCITA        TO DATA-NASCITA                      
      ************************************************************              
              MOVE DATA-NASCITA            TO DATAT                     09310000
              PERFORM 1202-CONV-IN-GIUL                                 09310000
              MOVE JUL-DATA                TO WK-DAT-INIZIO-RANGE       09310000
              MOVE COM-DT-GIU              TO WK-DAT-FINE-RANGE                 
              MOVE  10                     TO WK-ORA-INIZIO-RANGE               
              MOVE  12                     TO WK-ORA-FINE-RANGE                 
              MOVE  30                     TO WK-MIN-INIZIO-RANGE               
              MOVE  20                     TO WK-MIN-FINE-RANGE                 
              PERFORM 1201-CALCOLA-DURATA                               09310000
              MOVE WK-RISULT-IN-ORE        TO M-OREO                    09310000
              MOVE WK-RISULT-IN-MIN        TO M-MINO                    09310000
      ************************************************************              
              MOVE DT-ANNO                 TO M-A-NASCO                         
              MOVE DT-MESE                 TO M-M-NASCO                         
              MOVE DT-GIORNO               TO M-G-NASCO                         
              MOVE COM-MATRICOLA           TO M-MATRO                           
              MOVE DT-ANNO                 TO COM-ANNI                          
              COMPUTE W-ANNI = COM-DATA-SISTEMA-AAAA - COM-ANNI                 
              MOVE W-ANNI                  TO M-ANNIO                           
              MOVE COM-QUALIFICA-INTERNA   TO M-QU-INO                          
      *       MOVE COM-COD-FI              TO M-COD-FO                          
           END-IF.                                                              
                                                                        09710000
       2910-EX. EXIT.                                                   09720000
                                                                        09750000
       2999-CERCA-ERR SECTION.                                          09570000
      *--------------*                                                  09580000
           MOVE '2999-CERCA-ERR' TO W-ULT-LABEL.                        09590000
      * ---                                                             09600000
           SET IND-TAB TO 1.                                            09610000
                                                                        09620000
           SEARCH ELEM-TAB-MSG AT END                                   09630000
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'            09640000
                    TO M-MSG-1O                                         09650000
                  WHEN W-COD-MSG-HOST = ELEM-COD-MSG(IND-TAB)           09660000
                       MOVE ELEM-DESC-MSG(IND-TAB)  TO M-MSG-1O         09670000
           END-SEARCH.                                                  09680000
                                                                        09690000
           PERFORM 3000-FINE-ELAB.                                      09700000
                                                                        09710000
       2999-EX. EXIT.                                                   09720000
                                                                        09750000
                                                                        09760000
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
                                                                        09880000
                                                                        09890000
       3100-RIENTRO SECTION.                                            09900000
      *------------*                                                    09910000
           MOVE '3100-RIENTRO' TO W-ULT-LABEL.                          09920000
      *---                                                              09930000
           PERFORM 3130-FORMATTA-MAPPA.                                 09950000
           PERFORM 3140-INVIO-MAPPA.                                    09960000
                                                                        09970000
       3100-EX. EXIT.                                                   09980000
                                                                        10150000
                                                                        10150000
       3130-FORMATTA-MAPPA SECTION.                                     10160000
      *-------------------*                                             10170000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   10180000
      *---                                                              10190000
           IF M-COGNA = PROT-FSET                                               
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
           ELSE                                                         10230000
              INSPECT M-COGNO REPLACING ALL LOW-VALUE BY '_'                    
              INSPECT M-COGNO REPLACING ALL SPACE BY '_'                        
           END-IF.                                                      10260000
                                                                        10270000
           IF M-COD-FA = PROT-FSET                                              
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
           ELSE                                                         10230000
             INSPECT M-COD-FO REPLACING ALL LOW-VALUE BY '_'                    
             INSPECT M-COD-FO REPLACING ALL SPACE BY '_'                        
           END-IF.                                                      10260000
                                                                        10270000
       3130-EX. EXIT.                                                   10680000
                                                                        10710000
                                                                        10720000
       3140-INVIO-MAPPA SECTION.                                        10730000
      *----------------*                                                10740000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      10750000
      *---                                                              10760000
           IF W-NOME-PGM = COM-NOME-PGM                                 10770000
            THEN                                                        10780000
              EXEC CICS SEND                                            10790000
                        MAP    ('MF06MAP')                              10800000
                        MAPSET ('MF06MAP')                              10810000
                        CURSOR                                          10820000
                        DATAONLY
                        FREEKB
              END-EXEC                                                  10830000
            ELSE                                                        10840000
              MOVE W-NOME-PGM  TO COM-NOME-PGM                          10850000
              EXEC CICS SEND                                            10860000
                        MAP    ('MF06MAP')                              10870000
                        MAPSET ('MF06MAP')                              10880000
                        CURSOR                                          10890000
                        ERASE
                        FREEKB
              END-EXEC                                                  10900000
           END-IF.                                                      10910000
                                                                        10920000
           EXEC CICS RETURN                                             10930000
                     TRANSID  ('RR06')                                  10940000
                     COMMAREA (W-COMMAREA)                              10950000
                     LENGTH   (W-LEN)                                   10960000
                     END-EXEC.                                          10970000
                                                                        10980000
       3140-EX. EXIT.                                                   10990000
                                                                        11010000
                                                                        11020000
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
      *                                                                         
           EXEC SQL INCLUDE CWCONTD END-EXEC.                                   
      ******************************************************                    
           EXEC SQL INCLUDE CWRANGE END-EXEC.                                   
      ******************************************************                    
