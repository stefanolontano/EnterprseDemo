       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW04DEMO.                                            00020000
      * ---                                                             00030000
      ******************************************************************00040000
      * PROGETTO        : CENTRO PRODUZIONE SOFTWARE - I&T             *00050000
      * ID. TRANSAZIONE : RR04                                         *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :                                              *00080000
      * OGGETTO         :                                              *        
      * TIPO OPERAZIONE : INSERIMENTO                                  *00100000
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
      *      DEFINIZIONE VARIABILI HOST PER ISTRUZIONI SQL             *00230000
      * -------------------------------------------------------------- *00240000
       01  W-SQLCODE                       PIC S9(3)  COMP VALUE +0.    00260000
           88  W-SQLCODE-OK          VALUE +0.                          00270000
           88  W-SQLCODE-NOT-FOUND   VALUE +100.                        00280000
       01  W-COUNT                         PIC S9(4)  COMP-3 VALUE +0.  00290000
       01  W-MATRICOLA-T.                                                       
           02  W-MATRICOLA                 PIC S9(5)V USAGE COMP-3.             
                                                                        00210000
      * -------------------------------------------------------------- *00220000
      *      DEFINIZIONE DI CAMPI DI APPOGGIO PER ELABORAZIONE         *00230000
      * -------------------------------------------------------------- *00240000
       01  W-FLAG                          PIC  9(1)  VALUE ZEROES.     00250000
       01  W-COD-MSG-HOST                  PIC  X(3)  VALUE SPACE.      00250000
       01  W-COGN                          PIC X(30) VALUE SPACES.              
       01  W-NOME                          PIC X(15) VALUE SPACES.              
       01  W-A-NASC-NUM                    PIC 9(4)  VALUE ZEROES.              
       01  W-A-NASC                        PIC X(4)  VALUE SPACES.              
       01  W-M-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-G-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-QU-IN                         PIC X(08) VALUE SPACES.              
       01  W-MATR                          PIC 9(5)  VALUE ZERO.                
       01  W-MATR-NUM                      PIC 9(3)  COMP-3 VALUE 0.            
       01  W-COD-FI                        PIC X(16) VALUE SPACES.              
       01  W-CONF-OP                       PIC X(2) VALUE SPACES.               
      *                                                                         
       01  W-DATA-NASCITA.                                                      
           02  W-AA-NASC                     PIC X(4)  VALUE SPACES.            
           02  FILLER                        PIC X     VALUE '-'.               
           02  W-MM-NASC                     PIC X(2)  VALUE SPACES.            
           02  FILLER                        PIC X     VALUE '-'.               
           02  W-GG-NASC                     PIC X(2)  VALUE SPACES.            
      *                                                                         
       01  W-APPO-DATA.                                                 01330000
           02  APPO-ANNO                     PIC X(4).                  01340000
           02  FILLER                        PIC X VALUE SPACES.        01340000
           02  APPO-MESE                     PIC X(2).                  01350000
           02  FILLER                        PIC X VALUE SPACES.        01350000
           02  APPO-GIORNO                   PIC X(2).                  01360000
      *                                                                         
       01  W-DATA-APP1.                                                 01330000
           02  APP-GG1                       PIC X(2).                  01340000
           02  FILLER                        PIC X VALUE SPACES.        01340000
           02  APP-MM1                       PIC X(2).                  01350000
           02  FILLER                        PIC X VALUE SPACES.        01350000
           02  APP-AA1.                                                 01360000
               05 APP-SECOLO                 PIC XX.                            
               05 APP-ANNO                   PIC XX.                            
      *                                                                         
       01  W-DATA-APP2.                                                 01330000
           02  APP-AA2                       PIC X(4).                  01340000
           02  FILLER                        PIC X VALUE SPACES.        01340000
           02  APP-MM2                       PIC X(2).                  01350000
           02  FILLER                        PIC X VALUE SPACES.        01350000
           02  APP-GG2                       PIC X(2).                  01360000
      *                                                                 01370000
DEMORR 01  W-RUN-DATE.                                                  01330000
DEMORR     02  RUN-DATE-AA                   PIC 9(2).                  01340000
DEMORR     02  FILLER                        PIC X VALUE '-'.           01340000
DEMORR     02  RUN-DATE-MM                   PIC 9(2).                  01350000
DEMORR     02  FILLER                        PIC X VALUE '-'.           01340000
DEMORR     02  RUN-DATE-GG                   PIC 9(2).                  01350000
      *                                                                 01370000
       01  W-RESTO                         PIC 99.                      01390000
       01  W-RISULTATO                     PIC 99.                      01390000
                                                                        00390000
      * -------------------------------------------------------------- *00510000
      *    DEFINIZIONE CAMPI PER ROUTINE DI ALLINEAMENTO               *00520000
      * -------------------------------------------------------------- *00530000
       01  Z-APPO                          PIC X(05)  JUSTIFIED RIGHT.  00540000
                                                                        00550000
       01  Z-MATR-OK-05  REDEFINES Z-APPO   PIC 9(05).                  00560000
                                                                        00570000
       01  FILLER        REDEFINES Z-APPO.                              00580000
           02 FILLER                       PIC X(01).                   00590000
           02 Z-MATR-OK-04                 PIC 9(04).                   00600000
                                                                        00610000
       01  FILLER        REDEFINES Z-APPO.                              00620000
           02 FILLER                       PIC X(02).                   00630000
           02 Z-MATR-OK-03                 PIC 9(03).                   00640000
                                                                        00650000
       01  FILLER        REDEFINES Z-APPO.                              00660000
           02 FILLER                       PIC X(03).                   00670000
           02 Z-MATR-OK-02                 PIC 9(02).                   00680000
                                                                        00690000
       01  FILLER        REDEFINES Z-APPO.                              00700000
           02 FILLER                       PIC X(04).                   00710000
           02 Z-MATR-OK-01                 PIC 9(01).                   00720000
                                                                        00730000
      * ---                                                             01250000
       01  Z-MATR                          PIC X(05).                   01260000
       01  Z-SW                            PIC 9      VALUE 0.          01270000
                                                                        01580000
      * -------------------------------------------------------------- *01590000
      *    DEFINIZIONE CAMPI STANDART DELLA TRANSAZIONE                *01600000
      * -------------------------------------------------------------- *01610000
       01  W-PGM-LOGON                     PIC  X(8)  VALUE 'CW01DEMO'.         
       01  W-PGM-MENU-GEN                  PIC  X(8)  VALUE 'CW02DEMO'.         
       01  W-PGM-MENU-RAMO                 PIC  X(8)  VALUE 'CW03DEMO'.         
       01  W-CTL-END                       PIC  X(4)  VALUE 'LOOP'.     01630000
       01  W-NOME-PGM                      PIC  X(8)  VALUE 'CW04DEMO'. 01640000
       01  W-TRS-ID                        PIC  X(4)  VALUE SPACE.      01650000
       01  W-XCTL-PGM                      PIC  X(8)  VALUE SPACE.      01660000
       01  W-ULT-LABEL                     PIC  X(15) VALUE SPACES.     01670000
       01  W-LEN                           PIC S9(4)  COMP VALUE +250.          
       01  W-TERMID                        PIC  X(4)  VALUE SPACE.      01700000
      *01  W-NOME-MAPSET                   PIC  X(7)  VALUE 'CW04MAP'.  01720000
      *01  W-NOME-MAP                      PIC  X(7)  VALUE 'CW04MAP'.  01730000
DUMPDB 01  W-DUMP-T.                                                            
DUMPDB     05 W-DUMP                       PIC 9(3) COMP-3.                     
DUMPDB 01  W-PACKED                        PIC 9(3) COMP-3.                     
      *                                                                 01730000
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
           EXEC SQL  INCLUDE CWDIPE END-EXEC.                                   
                                                                        02480000
      * -------------------------------------------------------------- *02500000
      *    DEFINIZIONE DELLA MAPPA                                     *02510000
      * -------------------------------------------------------------- *02520000
           COPY MF04MAP.                                                02530000
                                                                        02560000
      * -------------------------------------------------------------- *02570000
      *    DEFINIZIONE DELLA COPY DEGLI ATTRIBUTI                      *02580000
      * -------------------------------------------------------------- *02590000
           COPY CWATTRIB.                                               02600000
                                                                        02560000
      * -------------------------------------------------------------- *02570000
      *    DEFINIZIONE DELLA COPY DEI MESSAGGI E DEI TASTI FUNZIONALI  *02580000
      * -------------------------------------------------------------- *02590000
           COPY CWMESS.                                                 02610000
           COPY DFHAID.                                                 02620000
                                                                        02640000
      * -------------------------------------------------------------- *02650000
      * DEFINIZIONE AREA DI COMUNICAZ. PER LINK IET010CT (ALLINEAMENTO)*02660000
      * -------------------------------------------------------------- *02670000
           COPY CWC010.                                                 02680000
                                                                        02710000
      * -------------------------------------------------------------- *02650000
      * DEFINIZIONE AREA DI COMUNICAZ. PER LINK IET012CT (CODICE FISC) *02660000
      * -------------------------------------------------------------- *02670000
           COPY CWC012.                                                 02680000
                                                                        02710000
      * -------------------------------------------------------------- *02720000
      *    DEFINIZIONE COMMAREA.                                       *        
      * -------------------------------------------------------------- *02760000
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
           PERFORM 1100-COND-ANOMAL.                                    03020000
           PERFORM 1300-TESTA-RIEN.                                     03030000
      *                                                                         
      * GESTIONE DEI TASTI FUNZIONE A SECONDA DEL VALORE DATO                   
      * AL CAMPO DI COMMAREA COM-GIRO                                           
      *                                                                         
           IF COM-GIRO = '2'                                            03050000
              PERFORM 1400-TASTI-LAST                                   03070000
           ELSE                                                         03100000
              PERFORM 1200-TASTI-FUNZ                                   03090000
           END-IF.                                                      03100000
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
      *                                                                 03490000
           MOVE '** ERRORE DI MPFAIL **' TO ERR-CICS.                           
                                                                        03490000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)                      
                        ERASE WAIT END-EXEC.                                    
           EXEC CICS RETURN END-EXEC.                                           
      *                                                                 03510000
       1120-EX. EXIT.                                                   03520000
                                                                        03670000
       1200-TASTI-FUNZ SECTION.                                         03680000
      *---------------*                                                 03690000
      *                                                                 03510000
           EXEC CICS HANDLE AID                                         03700000
                                PA1    (1210-TASTO-PA1)                         
                                CLEAR  (1220-TASTO-CLEAR)               03720000
                                PF3    (1230-TASTO-PF3)                 03730000
                                ENTER  ()                               03740000
                                PF4    (1240-TASTO-PF4)                         
                                ANYKEY (1250-TASTO-ANYK)                03750000
           END-EXEC.                                                    03760000
      *                                                                 03510000
       1200-EX. EXIT.                                                   03770000
                                                                        04050000
       1210-TASTO-PA1 SECTION.                                          03780000
      *--------------*                    * RITORNO AL CICS *           03960000
           MOVE '** RITORNO AL CICS **' TO ERR-CICS.                            
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)                      
                        ERASE WAIT END-EXEC.                                    
           EXEC CICS RETURN END-EXEC.                                           
      *                                                                         
       1210-EX. EXIT.                                                           
                                                                        04050000
       1220-TASTO-CLEAR SECTION.                                        03950000
      *--------------*                    * RITORNO MENU' PRINCIPALE *  03960000
           MOVE +250      TO W-LEN.                                             
           EXEC CICS XCTL PROGRAM  (W-PGM-MENU-GEN)                     03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04020000
       1220-EX. EXIT.                                                   04030000
                                                                        04040000
                                                                        04050000
                                                                        04060000
                                                                        04070000
       1230-TASTO-PF3 SECTION.                                          04080000
      *--------------*                     - RITORNO PGM PRECEDENTE -   04090000
           MOVE '1230-TASTO-PF3' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE W-PGM-MENU-RAMO  TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1230-EX. EXIT.                                                   04170000
                                                                        04060000
       1240-TASTO-PF4 SECTION.                                          04080000
      *--------------*                     - RITORNO MENU DI RAMO -     04090000
           MOVE '1240-TASTO-PF4' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           MOVE 'END'           TO W-CTL-END.                           04130000
           MOVE W-PGM-MENU-RAMO TO W-XCTL-PGM.                          04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1240-EX. EXIT.                                                   04170000
                                                                        04180000
                                                                                
                                                                                
       1250-TASTO-ANYK SECTION.                                         04220000
      *--------------*                                                  04230000
                                                                        04240000
           MOVE '006' TO W-COD-MSG-HOST.                                04290000
                                                                        04310000
           MOVE -1    TO M-COGNL.                                               
           PERFORM 2999-CERCA-ERR.                                      04330000
                                                                        04340000
       1250-EX. EXIT.                                                   04350000
                                                                        04360000
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
                                PA1    (1210-TASTO-PA1)                         
                                PF3    (1230-TASTO-PF3)                 04710000
                                ENTER  (1425-CONTROLLA-CONF)            04740000
                                PF4    (1240-TASTO-PF4)                         
                                ANYKEY (1250-TASTO-ANYK)                04750000
           END-EXEC.                                                    04760000
       1400-EX. EXIT.                                                   04770000
                                                                        05140000
       1425-CONTROLLA-CONF SECTION.                                     05310000
      *-----------------*                                               05320000
           MOVE '1425-CONTROLLA-CONF' TO W-ULT-LABEL.                   05330000
      * ---                                                             05340000
           INSPECT M-CONF-OPI REPLACING ALL LOW-VALUE BY ' '.                   
           INSPECT M-CONF-OPI REPLACING ALL '_' BY ' '.                         
           MOVE    M-CONF-OPI TO W-CONF-OP.                                     
                                                                                
           IF W-CONF-OP  = SPACES OR                                            
              W-CONF-OP  = LOW-VALUE                                            
              MOVE '001'  TO W-COD-MSG-HOST                                     
              MOVE -1     TO M-CONF-OPL                                         
           END-IF.                                                              
                                                                                
           IF W-COD-MSG-HOST NOT = SPACES                                       
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                              
                                                                                
           IF W-CONF-OP  = 'SI'                                                 
              PERFORM 1426-INSERIMENTO                                          
           ELSE                                                                 
            IF W-CONF-OP  = 'NO'                                                
              MOVE -1          TO M-COGNL                                       
              MOVE PROT-FSET   TO M-CONF-OPA                                    
            ELSE                                                                
              MOVE -1          TO M-CONF-OPL                                    
              MOVE '016' TO W-COD-MSG-HOST                                      
              PERFORM 2999-CERCA-ERR                                            
            END-IF                                                              
           END-IF.                                                              
                                                                                
           MOVE '1'            TO COM-GIRO.                                     
           MOVE SPACE          TO M-TEST-CONFO                                  
                                  M-CONF-OPO                                    
                                  M-TES-FISSOO.                                 
                                                                                
           PERFORM 2130-NORM-ATTR.                                      05370000
           PERFORM 3000-FINE-ELAB.                                      05380000
                                                                        05390000
                                                                        05400000
       1425-EX. EXIT.                                                   05410000
                                                                        05420000
       1426-INSERIMENTO SECTION.                                        05450000
      *---------------*                                                 05460000
           MOVE '1426-INSERIMENTO' TO W-ULT-LABEL.                      05470000
      *----                                                             05480000
           MOVE M-A-NASCI         TO W-AA-NASC.                                 
           MOVE M-M-NASCI         TO W-MM-NASC.                                 
           MOVE M-G-NASCI         TO W-GG-NASC.                                 
           MOVE W-DATA-NASCITA    TO DATA-NASCITA  OF DCL-CPSDIP.               
      *                                                                         
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-COGNI REPLACING ALL '_' BY ' '.                            
           MOVE M-COGNI           TO W-COGN.                                    
           MOVE W-COGN            TO COGNOME  OF DCL-CPSDIP.                    
      *                                                                         
           INSPECT M-NOMEI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-NOMEI REPLACING ALL '_' BY ' '.                            
           MOVE M-NOMEI           TO W-NOME.                                    
           MOVE W-NOME            TO NOME     OF DCL-CPSDIP.                    
      *                                                                         
           INSPECT M-QU-INI REPLACING ALL LOW-VALUE BY ' '.                     
           INSPECT M-QU-INI REPLACING ALL '_' BY ' '.                           
           MOVE M-QU-INI          TO W-QU-IN.                                   
           MOVE W-QU-IN           TO QUALIFICA-INTERNA   OF DCL-CPSDIP.         
      *                                                                         
           MOVE M-MATRI           TO W-MATRICOLA.                               
      *    COMPUTE COD-MATRICOLA-DIP OF DCL-CPSDIP =                            
      *            W-MATRICOLA + 90000.                                         
           MOVE W-MATRICOLA TO COD-MATRICOLA-DIP OF DCL-CPSDIP.                 
      *                                                                         
           INSPECT M-COD-FII REPLACING ALL LOW-VALUE BY ' '.                    
           INSPECT M-COD-FII REPLACING ALL '_' BY ' '.                          
           MOVE M-COD-FII      TO W-COD-FI.                                     
           MOVE W-COD-FI       TO COD-FISC  OF DCL-CPSDIP.                      
      *                                                                         
           MOVE APP-GG1  TO RUN-DATE-GG OF W-RUN-DATE.                          
           MOVE APP-MM1  TO RUN-DATE-MM OF W-RUN-DATE.                          
           MOVE APP-ANNO TO RUN-DATE-AA OF W-RUN-DATE.                          
           MOVE W-RUN-DATE TO RUN-DATE OF DCL-CPSDIP.                         
      *                                                                         
           EXEC SQL INSERT INTO CPS04.CWDIPENDENTI                              
                         VALUES (:DCL-CPSDIP.COD-MATRICOLA-DIP,                 
                                 :DCL-CPSDIP.COGNOME,                           
                                 :DCL-CPSDIP.NOME,                              
                                 :DCL-CPSDIP.DATA-NASCITA,                      
                                 :DCL-CPSDIP.QUALIFICA-INTERNA,                 
                                 :DCL-CPSDIP.COD-FISC,                          
                                 :DCL-CPSDIP.RUN-DATE)                          
           END-EXEC.                                                    05010000
DUMPDB*    MOVE 'PPPPP'     TO W-DUMP-T.                                        
DUMPDB*    ADD W-DUMP      TO W-PACKED.                                         
                                                                                
           MOVE '1'               TO COM-GIRO.                                  
           MOVE 1                 TO W-FLAG.                                    
           MOVE '017'             TO W-COD-MSG-HOST.                            
      *                                                                         
           IF W-COD-MSG-HOST NOT = SPACES                                       
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                              
      *                                                                         
                                                                                
       1426-EX. EXIT.                                                           
                                                                                
       2000-CORPO-ELAB SECTION.                                         05450000
      *---------------*                                                 05460000
           MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       05470000
      *----                                                             05480000
           IF W-NOME-PGM = COM-NOME-PGM                                 05490000
            THEN                                                        05500000
              PERFORM 2100-RECEIVE                                      05510000
              PERFORM 2200-CONTROLLI                                    05520000
              PERFORM 2500-CONFERMA                                     05530000
            ELSE                                                        05540000
              PERFORM 2900-RIEMP-MASK                                   05550000
           END-IF.                                                      05560000
                                                                        05570000
       2000-EX. EXIT.                                                   05580000
                                                                        05590000
                                                                        05600000
                                                                        05610000
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
                                                                        05720000
                                                                        05730000
                                                                        05740000
                                                                        05750000
       2110-REC-MAPPA SECTION.                                          05760000
      *--------------*                                                  05770000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        05780000
      * ---                                                             05790000
           EXEC CICS RECEIVE MAP    ('MF04MAP')                         05800000
                             MAPSET ('MF04MAP') END-EXEC.               05810000
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
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-COGNI REPLACING ALL '_' BY ' '.                            
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
           INSPECT M-COD-FII REPLACING ALL LOW-VALUE BY ' '.                    
           INSPECT M-COD-FII REPLACING ALL '_' BY ' '.                          
           MOVE    M-COD-FII TO W-COD-FI.                                       
                                                                        06020000
       2120-EX. EXIT.                                                   06110000
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
                               M-COD-FIA.                                       
       2130-EX. EXIT.                                                   06220000
                                                                        06260000
       2200-CONTROLLI SECTION.                                          06270000
      *--------------*                                                  06280000
           MOVE '2200-CONTROLLI' TO W-ULT-LABEL.                        06290000
      * ---                                                             06300000
                                                                        06370000
           MOVE SPACE TO M-MSG-1O                                       06380000
                         M-MSG-2O                                       06380000
                         M-TEST-CONFO                                   06380000
                         M-CONF-OPO                                     06380000
                         M-TES-FISSOO.                                  06380000
                                                                        06440000
           IF M-COGNI = SPACE                                                   
              MOVE FSET-BRT TO M-COGNA                                          
              MOVE -1       TO M-COGNL                                          
              MOVE '001'    TO W-COD-MSG-HOST                           06420000
           END-IF.                                                      06430000
                                                                        06440000
           IF M-NOMEI = SPACE                                                   
              MOVE FSET-BRT TO M-NOMEA                                          
              MOVE -1       TO M-NOMEL                                          
              MOVE '001'    TO W-COD-MSG-HOST                           06420000
           END-IF.                                                      06430000
                                                                        06440000
           IF M-A-NASCI  = SPACE                                                
              MOVE FSET-BRT TO M-A-NASCA                                        
              MOVE -1       TO M-A-NASCL                                        
              MOVE '001'    TO W-COD-MSG-HOST                           06480000
           END-IF.                                                      06490000
                                                                        06440000
           IF M-M-NASCI  = SPACE                                                
              MOVE FSET-BRT TO M-M-NASCA                                        
              MOVE -1       TO M-M-NASCL                                        
              MOVE '001'    TO W-COD-MSG-HOST                           06480000
           END-IF.                                                      06490000
                                                                        06440000
           IF M-G-NASCI  = SPACE                                                
              MOVE FSET-BRT TO M-G-NASCA                                        
              MOVE -1       TO M-G-NASCL                                        
              MOVE '001'    TO W-COD-MSG-HOST                           06480000
           END-IF.                                                      06490000
                                                                        06500000
           IF M-QU-INI   = SPACE                                                
              MOVE FSET-BRT TO M-QU-INA                                         
              MOVE -1       TO M-QU-INL                                         
              MOVE '001'    TO W-COD-MSG-HOST                           06540000
           END-IF.                                                      06550000
                                                                        06620000
           IF M-MATRO  = SPACE                                                  
              MOVE FSET-BRT TO M-MATRA                                          
              MOVE -1       TO M-MATRL                                          
              MOVE '001'    TO W-COD-MSG-HOST                           06540000
           END-IF.                                                      06550000
                                                                        06700000
           IF M-COD-FII  = SPACE                                                
              MOVE FSET-BRT TO M-COD-FIA                                        
              MOVE -1       TO M-COD-FIL                                        
              MOVE '001'    TO W-COD-MSG-HOST                           06540000
           END-IF.                                                      06550000
                                                                        06700000
           IF W-COD-MSG-HOST NOT = SPACE                                06710000
              PERFORM 2999-CERCA-ERR                                    06720000
           END-IF.                                                      06730000
                                                                        06730100
           IF (M-M-NASCI NOT EQUAL SPACES) OR                                   
             (M-M-NASCI NOT EQUAL LOW-VALUE)                                    
              MOVE M-M-NASCI      TO STRINGA                                    
              MOVE 2              TO LL-STRINGA                                 
              MOVE 'D'            TO ALLINEAMENTO                               
              PERFORM 2210-PASSA-ROUTINE                                        
              PERFORM 2215-TEST-RITORNO                                         
              MOVE STRINGA  TO W-M-NASC                                         
              INSPECT W-M-NASC REPLACING ALL LOW-VALUE BY ZEROES                
              INSPECT W-M-NASC REPLACING ALL SPACE BY ZEROES                    
              MOVE W-M-NASC TO M-M-NASCO                                        
           END-IF.                                                              
                                                                        06440000
           IF (M-M-NASCI  > '12') OR                                            
              (M-M-NASCI  < '01')                                               
              MOVE FSET-BRT TO M-M-NASCA                                        
              MOVE -1       TO M-M-NASCL                                        
              MOVE '026'    TO W-COD-MSG-HOST                           06480000
           END-IF.                                                      06490000
      *                                                                         
           IF (M-G-NASCI NOT EQUAL SPACES) OR                                   
             (M-G-NASCI NOT EQUAL LOW-VALUE)                                    
              MOVE M-G-NASCI      TO STRINGA                                    
              MOVE 2              TO LL-STRINGA                                 
              MOVE 'D'            TO ALLINEAMENTO                               
              PERFORM 2210-PASSA-ROUTINE                                        
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
              PERFORM 2210-PASSA-ROUTINE                                        
              PERFORM 2217-TEST-RITORNO                                         
              MOVE STRINGA  TO W-A-NASC                                         
              INSPECT W-A-NASC REPLACING ALL LOW-VALUE BY ZEROES                
              INSPECT W-A-NASC REPLACING ALL SPACE BY ZEROES                    
              MOVE W-A-NASC TO M-A-NASCO                                        
           END-IF.                                                              
      *                                                                         
           IF M-M-NASCO = '04' OR '06' OR '09' OR '11'                          
             IF M-G-NASCO > '30' OR < '01'                                      
                MOVE FSET-BRT TO M-G-NASCA                                      
                MOVE -1     TO M-G-NASCL                                        
                MOVE '026'  TO W-COD-MSG-HOST                                   
                PERFORM 2999-CERCA-ERR                                          
             END-IF                                                             
           ELSE                                                                 
             IF M-M-NASCO NOT = '02'                                            
               IF M-G-NASCO > '31' OR < '01'                                    
                 MOVE FSET-BRT TO M-G-NASCA                                     
                 MOVE -1     TO M-G-NASCL                                       
                 MOVE '026'  TO W-COD-MSG-HOST                                  
                 PERFORM 2999-CERCA-ERR                                         
               END-IF                                                           
             ELSE                                                               
               PERFORM 2221-ANNO-BISESTILE                                      
             END-IF                                                             
           END-IF.                                                              
                                                                                
      *                                                                         
           IF M-A-NASCI < '1000'                                                
              MOVE FSET-BRT          TO M-A-NASCA                               
              MOVE -1                TO M-A-NASCL                               
              MOVE '026'             TO W-COD-MSG-HOST                          
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                              
      *                                                                         
           MOVE M-A-NASCI            TO APPO-ANNO.                              
           MOVE M-M-NASCI            TO APPO-MESE.                              
           MOVE M-G-NASCI            TO APPO-GIORNO.                            
           MOVE COM-DATA-SISTEMA     TO W-DATA-APP1.                            
           MOVE APP-GG1              TO APP-GG2.                                
           MOVE APP-MM1              TO APP-MM2.                                
           MOVE APP-AA1              TO APP-AA2.                                
           IF W-APPO-DATA > W-DATA-APP2                                         
              MOVE FSET-BRT          TO M-A-NASCA                               
              MOVE -1                TO M-A-NASCL                               
              MOVE '029'             TO W-COD-MSG-HOST                          
              PERFORM 2999-CERCA-ERR                                            
           END-IF.                                                              
      *                                                                         
           MOVE M-MATRI        TO Z-MATR.                                       
           PERFORM 2298-ALLINEA.                                        06750000
           IF Z-SW = 1                                                  06760000
           THEN                                                         06770000
              MOVE FSET-BRT TO M-MATRA                                          
              MOVE -1       TO M-MATRL                                          
              MOVE '014'    TO W-COD-MSG-HOST                           06800000
           ELSE                                                         06810000
             IF Z-SW = 3                                                        
             THEN                                                       06770000
                MOVE FSET-BRT TO M-MATRA                                        
                MOVE -1       TO M-MATRL                                        
                MOVE '015'    TO W-COD-MSG-HOST                         06800000
             ELSE                                                       06810000
               MOVE Z-MATR-OK-05 TO M-MATRO W-MATR                              
             END-IF                                                     06830000
           END-IF.                                                      06830000
                                                                        06840000
           INSPECT M-COD-FII REPLACING ALL LOW-VALUE BY ' '.                    
           INSPECT M-COD-FII REPLACING ALL '_' BY ' '.                          
           MOVE    M-COD-FII TO W-COD-FI.                                       
                                                                                
           MOVE SPACES       TO RC-IET012CT.                                    
                                                                                
           IF W-COD-FI NOT EQUAL SPACES                                         
              MOVE W-COD-FI       TO CF-IET012CT                                
              MOVE 20             TO LL-IET012CT                                
              PERFORM 2211-PASSA-ROUTINE                                        
              PERFORM 2218-TEST-RITORNO                                         
           END-IF.                                                              
                                                                                
           IF W-COD-MSG-HOST NOT = SPACE                                07080000
              PERFORM 2999-CERCA-ERR                                    07090000
           END-IF.                                                      07100000
                                                                        07110000
           PERFORM 2220-ACCESSO-SQL.                                            
                                                                                
       2200-EX. EXIT.                                                   07180000
                                                                        07220000
       2210-PASSA-ROUTINE SECTION.                                              
      *----------------*                                                07240000
           MOVE '2210-PASSA-ROUTINE' TO W-ULT-LABEL.                            
      * ---                                                             07260000
           EXEC CICS LINK  PROGRAM('CW10DEMO')                          07290000
                           COMMAREA(AREA-IET010CT)                      07300000
                           LENGTH(LL-IET010CT)    END-EXEC.             07310000
                                                                        07340000
      *---                                                                      
       2210-EX. EXIT.                                                   07430000
                                                                        07440000
       2211-PASSA-ROUTINE SECTION.                                              
      *----------------*                                                07240000
           MOVE '2211-PASSA-ROUTINE' TO W-ULT-LABEL.                            
      * ---                                                             07260000
           EXEC CICS LINK  PROGRAM('CW12DEMO')                          07290000
                           COMMAREA(AREA-IET012CT)                      07300000
                           LENGTH(LL-IET012CT)    END-EXEC.             07310000
                                                                        07340000
      *---                                                                      
       2211-EX. EXIT.                                                   07430000
                                                                        07440000
       2215-TEST-RITORNO SECTION.                                               
      *----------------*                                                07240000
           MOVE '2215-TEST-RITORNO' TO W-ULT-LABEL.                             
      * ---                                                             07260000
           IF RC-IET010CT = 'E1'                                        07290000
              MOVE FSET-BRT     TO M-M-NASCA                                    
              MOVE -1      TO M-M-NASCL                                 07300000
              MOVE '001'   TO W-COD-MSG-HOST                            07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
             IF RC-IET010CT = 'E2'                                      07290000
                MOVE FSET-BRT     TO M-M-NASCA                                  
                MOVE -1      TO M-M-NASCL                               07300000
                MOVE '014'   TO W-COD-MSG-HOST                          07300000
                PERFORM 2999-CERCA-ERR                                  07310000
             ELSE                                                               
                IF RC-IET010CT = 'E3'                                   07290000
                   MOVE FSET-BRT     TO M-M-NASCA                               
                   MOVE -1      TO M-M-NASCL                            07300000
                   MOVE '023'   TO W-COD-MSG-HOST                       07300000
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
              MOVE FSET-BRT     TO M-G-NASCA                                    
              MOVE -1      TO M-G-NASCL                                 07300000
              MOVE '001'   TO W-COD-MSG-HOST                            07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
             IF RC-IET010CT = 'E2'                                      07290000
                MOVE FSET-BRT     TO M-G-NASCA                                  
                MOVE -1      TO M-G-NASCL                               07300000
                MOVE '014'   TO W-COD-MSG-HOST                          07300000
                PERFORM 2999-CERCA-ERR                                  07310000
             ELSE                                                               
                IF RC-IET010CT = 'E3'                                   07290000
                   MOVE FSET-BRT     TO M-G-NASCA                               
                   MOVE -1      TO M-G-NASCL                            07300000
                   MOVE '024'   TO W-COD-MSG-HOST                       07300000
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
              MOVE FSET-BRT     TO M-A-NASCA                                    
              MOVE -1      TO M-A-NASCL                                 07300000
              MOVE '001'   TO W-COD-MSG-HOST                            07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
             IF RC-IET010CT = 'E2'                                      07290000
                MOVE FSET-BRT     TO M-A-NASCA                                  
                MOVE -1      TO M-A-NASCL                               07300000
                MOVE '014'   TO W-COD-MSG-HOST                          07300000
                PERFORM 2999-CERCA-ERR                                  07310000
             ELSE                                                               
                IF RC-IET010CT = 'E3'                                   07290000
                   MOVE FSET-BRT     TO M-A-NASCA                               
                   MOVE -1      TO M-A-NASCL                            07300000
                   MOVE '027'   TO W-COD-MSG-HOST                       07300000
                   PERFORM 2999-CERCA-ERR                               07310000
                END-IF                                                          
             END-IF                                                             
           END-IF.                                                              
                                                                        07340000
      *---                                                                      
       2217-EX. EXIT.                                                   07430000
                                                                        07340000
       2218-TEST-RITORNO SECTION.                                               
      *----------------*                                                07240000
           MOVE '2218-TEST-RITORNO' TO W-ULT-LABEL.                             
      * ---                                                             07260000
           IF RC-IET012CT = SPACES                                      07290000
              MOVE CF-IET012CT   TO M-COD-FIO                                   
                                    COM-COD-FI                                  
              MOVE PROT-FSET     TO M-COD-FIA                                   
           ELSE                                                                 
              PERFORM 2219-CODICE-MESSAGGIO                                     
           END-IF.                                                              
                                                                        07340000
      *---                                                                      
       2218-EX. EXIT.                                                   07430000
                                                                        09250000
       2219-CODICE-MESSAGGIO SECTION.                                   09270000
      *---------------*                                                 09280000
           MOVE '2219-CODICE-MESSAGGIO' TO W-ULT-LABEL.                 09290000
      *---                                                              09300000
           IF RC-IET012CT = 'E1'                                        07290000
              MOVE FSET-BRT     TO M-COD-FIA                                    
              MOVE -1           TO M-COD-FIL                            07300000
              MOVE '001'        TO W-COD-MSG-HOST                       07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           ELSE                                                                 
              MOVE FSET-BRT     TO M-COD-FIA                                    
              MOVE -1           TO M-COD-FIL                            07300000
              MOVE '028'        TO W-COD-MSG-HOST                       07300000
              PERFORM 2999-CERCA-ERR                                    07310000
           END-IF.                                                              
      *---                                                                      
       2219-EX. EXIT.                                                   07430000
                                                                        09250000
                                                                        07470000
       2220-ACCESSO-SQL SECTION.                                        07480000
      *--------------------*                                            07490000
           MOVE '2220-ACCESSO-SQL' TO W-ULT-LABEL.                      07500000
      * ---                                                             07510000
      * VERIFICO CHE LA MATRICOLA DIGITATA NON SIA PRESENTE *                   
      * IN TABELLA                                          *                   
      * ---                                                                     
           MOVE M-MATRO      TO W-MATR.                                         
           MOVE W-MATR       TO W-MATRICOLA.                                    
                                                                                
           EXEC SQL SELECT  COUNT(*)                                            
                      INTO :W-COUNT                                             
                      FROM  CPS04.CWDIPENDENTI                                  
                     WHERE  COD_MATRICOLA_DIP = :W-MATRICOLA                    
           END-EXEC.                                                            
                                                                                
           MOVE SQLCODE TO W-SQLCODE.                                   07600000
           IF W-SQLCODE-OK                                              07610000
            THEN                                                        07620000
              IF W-COUNT > +0                                           07630000
                THEN                                                            
                MOVE FSET-BRT TO M-MATRA                                        
                MOVE -1       TO M-MATRL                                        
                MOVE '010'    TO W-COD-MSG-HOST                                 
                PERFORM 2999-CERCA-ERR                                          
              END-IF                                                            
           END-IF.                                                              
                                                                                
                                                                                
       2220-EX. EXIT.                                                   07710000
                                                                        07720000
                                                                        05400000
                                                                        07750000
       2221-ANNO-BISESTILE SECTION.                                     08360000
      *------------*                                                    08370000
           MOVE '2221-ANNO-BISESTILE' TO W-ULT-LABEL.                   08380000
      * ---                                                             08390000
           MOVE M-A-NASCO   TO W-A-NASC-NUM.                                    
           DIVIDE 4 INTO W-A-NASC-NUM GIVING W-RISULTATO                        
                                    REMAINDER W-RESTO.                          
           IF W-RESTO = 0                                                       
             IF M-G-NASCO > '29' OR < '01'                                      
                MOVE FSET-BRT     TO M-G-NASCA                                  
                MOVE -1     TO M-G-NASCL                                        
                MOVE '026'  TO W-COD-MSG-HOST                                   
                PERFORM 2999-CERCA-ERR                                          
             END-IF                                                             
           ELSE                                                                 
             IF M-G-NASCO > '28' OR < '01'                                      
                MOVE FSET-BRT     TO M-G-NASCA                                  
                MOVE -1     TO M-G-NASCL                                        
                MOVE '026'  TO W-COD-MSG-HOST                                   
                PERFORM 2999-CERCA-ERR                                          
             END-IF                                                             
           END-IF.                                                              
                                                                                
       2221-EX. EXIT.                                                   07710000
                                                                        07720000
      **------------------------------------------------------------**  08230000
      **          ROUTINE DI ALLINEAMENTO PER I CAMPI NUMERICI      **  08240000
      **          --------------------------------------------      **  08250000
      **                                                            **  08260000
      **  MUOVERE IL CAMPO DA ALLINEARE E CONTROLLARE IN Z-MATR **      08270000
      **  LA ROUTINE RESTITUIRA' I SEGUENTI CODICI DI RITORNO       **  08280000
      **      Z-SW = 3   -->  SE CAMPO UGUALE A ZERO                **  08290000
      **      Z-SW = 2   -->  SE CAMPO NON DIGITATO                 **  08300000
      **      Z-SW = 1   -->  SE CAMPO NON NUMERICO                 **  08310000
      **      Z-SW = 0   -->  SE CAMPO CORRETTO. MUOVERE IL CAMPO   **  08320000
      **                       Z-MATR-OK-XX NEL CAMPO DI OUTPUT     **  08330000
      **                                                            **  08340000
      **-------------------------------------------------------------*  08350000
       2298-ALLINEA SECTION.                                            08360000
      *------------*                                                    08370000
           MOVE '2298-ALLINEA' TO W-ULT-LABEL.                          08380000
      * ---                                                             08390000
           MOVE ZERO          TO Z-SW.                                  08400000
           MOVE SPACES        TO Z-APPO.                                08410000
                                                                        08420000
           INSPECT Z-MATR REPLACING ALL LOW-VALUE BY SPACE.             08430000
           INSPECT Z-MATR REPLACING ALL '_'       BY SPACE.             08440000
                                                                        08450000
           IF Z-MATR = SPACES                                           08460000
             THEN                                                       08470000
              MOVE 2 TO Z-SW                                            08480000
             ELSE                                                       08490000
              INSPECT  Z-MATR REPLACING LEADING SPACE BY ZERO           08500000
              UNSTRING Z-MATR DELIMITED BY ALL SPACE INTO Z-APPO        08510000
              INSPECT Z-APPO REPLACING ALL SPACES BY ZERO               08520000
              IF Z-APPO NOT NUMERIC                                     08530000
                THEN                                                    08540000
                 MOVE 1 TO Z-SW                                         08550000
ABEND?*          MOVE 0 TO Z-SW                                         08550000
              END-IF                                                    08560000
           END-IF.                                                      08570000
                                                                        08580000
           IF Z-APPO = ZEROES                                           08590000
             THEN                                                       08600000
                MOVE 3 TO Z-SW                                          08610000
           END-IF.                                                      08620000
                                                                        08630000
       2298-EX. EXIT.                                                   08640000
                                                                        08650000
      *2299-ALLINEA SECTION.                                            08360000
      *------------*                                                    08370000
      *    MOVE '2299-ALLINEA' TO W-ULT-LABEL.                          08380000
      * ---                                                             08390000
      *    MOVE ZERO          TO Z-SW.                                  08400000
      *    MOVE SPACES        TO Z-APPO-2.                              08410000
                                                                        08420000
      *    INSPECT Z-MATR REPLACING ALL LOW-VALUE BY SPACE.             08430000
      *    INSPECT Z-MATR REPLACING ALL '_'       BY SPACE.             08440000
                                                                        08450000
      *    IF Z-MATR = SPACES                                           08460000
      *      THEN                                                       08470000
      *       MOVE 2 TO Z-SW                                            08480000
      *      ELSE                                                       08490000
      *       INSPECT  Z-MATR REPLACING LEADING SPACE BY ZERO           08500000
      *       UNSTRING Z-MATR DELIMITED BY ALL SPACE INTO Z-APPO        08510000
      *       INSPECT Z-APPO REPLACING ALL SPACES BY ZERO               08520000
      *       IF Z-APPO NOT NUMERIC                                     08530000
      *         THEN                                                    08540000
      *          MOVE 1 TO Z-SW                                         08550000
      *       END-IF                                                    08560000
      *    END-IF.                                                      08570000
      *                                                                 08580000
      *    IF Z-APPO = ZEROES                                           08590000
      *      THEN                                                       08600000
      *         MOVE 3 TO Z-SW                                          08610000
      *    END-IF.                                                      08620000
      *                                                                 08630000
      *2299-EX. EXIT.                                                   08640000
                                                                        08630000
       2500-CONFERMA SECTION.                                           09010000
      *-------------*                                                   09020000
           MOVE '2500-CONFERMA' TO W-ULT-LABEL.                         09030000
      *                                                                         
           MOVE '2'   TO COM-GIRO.                                      05360000
           MOVE SPACES  TO M-MSG-1O                                             
                           M-MSG-2O.                                            
           MOVE PROT-FSET TO M-COGNA                                            
                             M-NOMEA                                            
                             M-A-NASCA                                          
                             M-M-NASCA                                          
                             M-G-NASCA                                          
                             M-QU-INA                                           
                             M-MATRA                                            
                             M-COD-FIA.                                         
                                                                        05400000
      * ---                                                             05340000
           MOVE 'CONFERMA OPERAZIONE:'    TO M-TEST-CONFO.                      
           MOVE ALL '_'   TO M-CONF-OPO.                                        
           MOVE '(SI/NO)' TO M-TES-FISSOO.                                      
           MOVE UNPROT        TO M-CONF-OPA.                                    
           MOVE -1            TO M-CONF-OPL.                                    
           MOVE SPACES        TO M-MSG-1O                                       
                                 M-MSG-2O.                                      
                                                                        07730000
                                                                        09210000
       2500-EX. EXIT.                                                   09220000
                                                                        09260000
       2900-RIEMP-MASK SECTION.                                         09270000
      *---------------*                                                 09280000
           MOVE '2900-RIEMP-MASK' TO W-ULT-LABEL.                       09290000
      *---                                                              09300000
           MOVE LOW-VALUE TO MF04MAPO.                                  09310000
           MOVE -1        TO M-COGNL.                                           
      *                                                                         
      *    MOVE FSET      TO M-COGNA                                            
      *                      M-NOMEA                                            
      *                      M-A-NASCA                                          
      *                      M-M-NASCA                                          
      *                      M-G-NASCA                                          
      *                      M-QU-INA                                           
      *                      M-MATRA.                                           
       2900-EX. EXIT.                                                   09370000
                                                                        09410000
       2998-DBERROR  SECTION.                                           09420000
      *------------*                                                    09430000
           MOVE W-ULT-LABEL TO W-ULT-LABEL-SQL.                         09440000
           MOVE W-TRS-ID   TO TRS-ID-SQL.                               09450000
           MOVE W-NOME-PGM TO NOME-PGM-SQL.                             09460000
           MOVE SQLCODE    TO SQL-CODICE.                               09470000
           MOVE ERR-SQL    TO M-MSG-1O.                                 09480000
           EXEC CICS DUMP TRANSACTION DUMPCODE(SQL-CODICE)                      
                                         END-EXEC.                              
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       09490000
           PERFORM 3000-FINE-ELAB.                                      09500000
                                                                        09510000
       2998-EX. EXIT.                                                   09520000
                                                                        09560000
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
           MOVE COM-DATA-SISTEMA   TO M-DATA-SO.                        10070000
           PERFORM 3130-FORMATTA-MAPPA.                                 09950000
           PERFORM 3140-INVIO-MAPPA.                                    09960000
                                                                        09970000
       3100-EX. EXIT.                                                   09980000
                                                                        10150000
       3130-FORMATTA-MAPPA SECTION.                                     10160000
      *-------------------*                                             10170000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   10180000
      *---                                                              10190000
           IF M-COGNA = PROT-FSET AND W-FLAG NOT = 1                            
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
            ELSE                                                        10230000
             IF W-FLAG = 1                                              10230000
                MOVE ALL SPACE  TO M-COGNO                              10230000
             END-IF                                                     10230000
             INSPECT M-COGNO REPLACING ALL LOW-VALUE BY '_'                     
             INSPECT M-COGNO REPLACING ALL SPACE BY '_'                         
           END-IF.                                                      10260000
                                                                        10270000
           IF M-NOMEA = PROT-FSET AND W-FLAG NOT = 1                            
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
            ELSE                                                        10230000
             IF W-FLAG = 1                                              10230000
                MOVE ALL SPACE  TO M-NOMEO                              10230000
             END-IF                                                     10230000
             INSPECT M-NOMEO REPLACING ALL LOW-VALUE BY '_'                     
             INSPECT M-NOMEO REPLACING ALL SPACE BY '_'                         
           END-IF.                                                      10260000
                                                                        10270000
           IF M-A-NASCA = PROT-FSET AND W-FLAG NOT = 1                          
            THEN                                                        10290000
              NEXT SENTENCE                                             10300000
            ELSE                                                        10310000
             IF W-FLAG = 1                                              10230000
                MOVE ALL SPACE  TO M-A-NASCO                            10230000
             END-IF                                                     10230000
              INSPECT M-A-NASCO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-A-NASCO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10340000
                                                                                
           IF M-M-NASCA = PROT-FSET AND W-FLAG NOT = 1                          
            THEN                                                        10290000
              NEXT SENTENCE                                             10300000
            ELSE                                                        10310000
             IF W-FLAG = 1                                              10230000
                MOVE ALL SPACE  TO M-M-NASCO                            10230000
             END-IF                                                     10230000
              INSPECT M-M-NASCO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-M-NASCO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10340000
                                                                                
           IF M-G-NASCA = PROT-FSET AND W-FLAG NOT = 1                          
            THEN                                                        10290000
              NEXT SENTENCE                                             10300000
            ELSE                                                        10310000
             IF W-FLAG = 1                                              10230000
                MOVE ALL SPACE  TO M-G-NASCO                            10230000
             END-IF                                                     10230000
              INSPECT M-G-NASCO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-G-NASCO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10340000
                                                                        10340000
           IF M-QU-INA = PROT-FSET AND W-FLAG NOT = 1                           
            THEN                                                        10342001
              NEXT SENTENCE                                             10343001
            ELSE                                                        10344001
             IF W-FLAG = 1                                              10230000
                MOVE ALL SPACE  TO M-QU-INO                             10230000
             END-IF                                                     10230000
              INSPECT M-QU-INO REPLACING ALL LOW-VALUE BY '_'                   
              INSPECT M-QU-INO REPLACING ALL SPACE BY '_'                       
           END-IF.                                                      10347001
                                                                        10350000
           IF M-MATRA = PROT-FSET AND W-FLAG NOT = 1                            
            THEN                                                        10370000
              NEXT SENTENCE                                             10380000
            ELSE                                                        10390000
             IF W-FLAG = 1                                              10230000
                MOVE ALL ZEROES TO M-MATRO                              10230000
                INSPECT M-MATRO REPLACING ALL ZEROES  BY '_'                    
             END-IF                                                     10230000
              INSPECT M-MATRO REPLACING ALL LOW-VALUE BY '_'                    
              INSPECT M-MATRO REPLACING ALL SPACE BY '_'                        
           END-IF.                                                      10420000
                                                                        10430000
           IF M-COD-FIA = PROT-FSET AND W-FLAG NOT = 1                          
            THEN                                                        10370000
              NEXT SENTENCE                                             10380000
            ELSE                                                        10390000
             IF W-FLAG = 1                                              10230000
                MOVE SPACES     TO M-COD-FIO                            10230000
      *         INSPECT M-MATRO REPLACING ALL ZEROES  BY '_'                    
             END-IF                                                     10230000
              INSPECT M-COD-FIO REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-COD-FIO REPLACING ALL SPACE BY '_'                      
           END-IF.                                                      10420000
                                                                        10430000
           IF W-FLAG = 1                                                        
            THEN                                                        10370000
              MOVE ALL SPACE  TO M-CONF-OPO                                     
              MOVE -1         TO M-COGNL                                        
           END-IF.                                                      10420000
       3130-EX. EXIT.                                                   10680000
                                                                        10720000
       3140-INVIO-MAPPA SECTION.                                        10730000
      *----------------*                                                10740000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      10750000
      *---                                                              10760000
           IF W-NOME-PGM = COM-NOME-PGM AND W-FLAG NOT = 1              10770000
            THEN                                                        10780000
              EXEC CICS SEND                                            10790000
                        MAP    ('MF04MAP')                              10800000
                        MAPSET ('MF04MAP')                              10810000
                        CURSOR                                          10820000
                        DATAONLY
                        FREEKB
              END-EXEC                                                  10830000
            ELSE                                                        10840000
              MOVE W-NOME-PGM  TO COM-NOME-PGM                          10850000
              EXEC CICS SEND                                            10860000
                        MAP    ('MF04MAP')                              10870000
                        MAPSET ('MF04MAP')                              10880000
                        CURSOR                                          10890000
                        ERASE
                        FREEKB
              END-EXEC                                                  10900000
           END-IF.                                                      10910000
                                                                        10920000
           EXEC CICS RETURN                                             10930000
                     TRANSID  ('RR04')                                  10940000
                     COMMAREA (W-COMMAREA)                              10950000
                     LENGTH   (W-LEN)                                   10960000
                     END-EXEC.                                          10970000
                                                                        10980000
       3140-EX. EXIT.                                                   10990000
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
