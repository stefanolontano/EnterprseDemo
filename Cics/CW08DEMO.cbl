       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW08DEMO.                                            00020000
      * ---                                                             00030000
      ******************************************************************00040000
      * PROGETTO        :                                              *00050000
      * ID. TRANSAZIONE :                                              *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :                                              *00080000
      * OGGETTO         :                                              *        
      * TIPO OPERAZIONE : ANNULLAMENTO                                  00100000
      * RIFERIMENTO P.E.:                                              *        
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 17/03/1999                                   *        
      * ULTIMA MODIFICA : 22/03/1999                                   *        
      ******************************************************************00150000
      * ---                                                             00160000
       ENVIRONMENT DIVISION.                                            00170000
       CONFIGURATION SECTION.                                           00180000
       DATA DIVISION.                                                   00190000
       WORKING-STORAGE SECTION.                                         00200000
                                                                        00210000
      * -------------------------------------------------------------- *00220000
      *      DEFINIZIONE VARIABILI HOST                                *00230000
      * -------------------------------------------------------------- *00240000
       01  W-COD-MSG-HOST                  PIC  X(3)       VALUE SPACE. 00250000
       01  W-SQLCODE                       PIC S9(3)  COMP VALUE +0.    00260000
           88  W-SQLCODE-OK                                VALUE +0.    00270000
           88  W-SQLCODE-NOT-FOUND                         VALUE +100.  00280000
      * ------------------------------------------------------------- *         
      *  SW-SQL  SI USA PER CAPIRE SE SQLCODE = +100  E'  PERCHE SONO *         
      *          FINITE  LE OCCORRENZE O PERECHE  NON HA TROVATO IL   *         
      *          DATO                                                 *         
      *  SW-INSPECT  SI USA PER CAPIRE QUANDO NON DEVE METERE         *         
      *              UNDERSCORE  NEI CAMPI DELLA  MAPPA               *         
      * ------------------------------------------------------------- *         
       01  SW-SQL                          PIC 9         VALUE 0.               
       01  SW-INSPECT                      PIC 9         VALUE 0.               
       01  W-COGN                          PIC X(30)     VALUE SPACES.          
       01  W-NOME                          PIC X(15)     VALUE SPACES.          
       01  W-A-NASC                        PIC X(4)      VALUE SPACES.          
       01  W-M-NASC                        PIC X(2)      VALUE SPACES.          
       01  W-G-NASC                        PIC X(2)      VALUE SPACES.          
       01  W-QU-IN                         PIC X(08)     VALUE SPACES.          
       01  W-MATR                          PIC 9(5)      VALUE ZEROES.          
       01  W-MATRICOLA                     PIC S9(5)V    USAGE COMP-3.          
       01  W-CONF-OP                       PIC X(2)      VALUE SPACES.          
       01  W-COD-FISC                      PIC X(16)     VALUE SPACES.          
       01  W-DATA-NASCITA.                                                      
         02  W-AA-NASC                     PIC X(4)      VALUE SPACES.          
         02  FILLER                        PIC X         VALUE '-'.             
         02  W-MM-NASC                     PIC X(2)      VALUE SPACES.          
         02  FILLER                        PIC X         VALUE '-'.             
         02  W-GG-NASC                     PIC X(2)      VALUE SPACES.          
                                                                        00380000
                                                                        01570000
                                                                        01580000
      * -------------------------------------------------------------- *01590000
      *    DEFINIZIONE CAMPI STANDART DELLA TRANSAZIONE                *01600000
      * -------------------------------------------------------------- *01610000
       01  W-CTL-END                       PIC  X(4)   VALUE 'LOOP'.    01630000
       01  W-NOME-PGM                      PIC  X(8)   VALUE 'CW08DEMO'.01640000
       01  W-NOME-MAP                      PIC  X(7)   VALUE 'MF08MAP'. 01640000
       01  W-NOME-MAPSET                   PIC  X(7)   VALUE 'MF08MAP'. 01640000
       01  W-PGM-LOGON                     PIC  X(8)   VALUE 'CW01DEMO'.01640000
       01  W-PGM-MENU-GEN                  PIC  X(8)   VALUE 'CW02DEMO'.01640000
       01  W-PGM-MENU-RAMO                 PIC  X(8)   VALUE 'CW03DEMO'.01640000
       01  W-PGM-VSAM                      PIC  X(8)   VALUE 'CW13DEMO'.03990000
       01  W-TRS-ID                        PIC  X(4)   VALUE SPACE.     01650000
       01  W-XCTL-PGM                      PIC  X(8)   VALUE SPACE.     01660000
       01  W-ULT-LABEL                     PIC  X(15)  VALUE SPACES.    01670000
       01  W-LEN                           PIC S9(4)   COMP VALUE +250.         
       01  W-TERMID                        PIC  X(4)   VALUE SPACE.     01700000
                                                                        01720000
                                                                        01730000
      * -------------------------------------------------------------- *00430000
      *  DEFINIZIONE CAMPI STANDART PER CODE DI TEMPORARY STORAGE      *00440000
      * -------------------------------------------------------------- *00450000
       01  CPSCODA.                                                     00460000
           02  FILLER                      PIC XXX     VALUE 'CPS'.     00470000
           02  CPS-TERM                    PIC X(4)    VALUE SPACES.    00480000
           02  CPS-NUM-CODA                PIC X       VALUE '1'.       00490000
      *--                                                               00500000
       01  CPS-LEN-CODA                    PIC S9(4)   COMP VALUE +84.  00460000
       01  CPS-ITEM-CODA                   PIC S9(4)   COMP VALUE +0.   00460000
       01  CPS-DENOM.                                                   00460000
         03  CPS-MATR                      PIC 9(5)    VALUE 0.                 
         03  CPS-COGNOME                   PIC X(30)   VALUE SPACE.             
         03  CPS-NOME                      PIC X(15)   VALUE SPACE.             
         03  CPS-DATA-NASC.                                                     
             05  CPS-A-NASC                PIC X(4)    VALUE SPACE.             
             05  FILLER                    PIC X       VALUE '-'.               
             05  CPS-M-NASC                PIC X(2)    VALUE SPACE.             
             05  FILLER                    PIC X       VALUE '-'.               
             05  CPS-G-NASC                PIC X(2)    VALUE SPACE.             
         03  CPS-QU-IN                     PIC X(8)    VALUE SPACE.             
         03  CPS-COD-FISC                  PIC X(16)   VALUE SPACE.             
                                                                                
                                                                                
      * -------------------------------------------------------------- *01980000
      *    DEFINIZIONE  MESSAGGIO ABEND SQL                            *01990000
      * -------------------------------------------------------------- *02000000
       01  ERR-SQL.                                                     02010000
           02  FILLER                      PIC X(11)                    02020000
                                                 VALUE 'ERRORE SQL '.   02030000
           02  SQL-CODICE                  PIC ----.                    02040000
           02  FILLER                      PIC X(12)                    02050000
                                                VALUE ' ALLA LABEL '.   02060000
           02  ULT-LABEL-SQL               PIC X(15).                   02070000
           02  FILLER                      PIC X(10)                    02080000
                                                  VALUE ' TRANSID: '.   02090000
           02  TRS-ID-SQL                  PIC X(4).                    02100000
           02  FILLER                      PIC X(06)                    02110000
                                                  VALUE ' PGM: '.       02120000
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
                                                VALUE 'ERRORE CICS '.   02230000
           02  COD-ERR                     PIC X(4).                    02240000
           02  FILLER                      PIC X(12)                    02250000
                                                VALUE ' ALLA LABEL '.   02260000
           02  ULT-LABEL-CICS              PIC X(15).                   02270000
           02  FILLER                      PIC X(10)                    02280000
                                                VALUE ' TRANSID: '.     02290000
           02  TRS-ID-CICS                 PIC X(4).                    02300000
           02  FILLER                      PIC X(06)                    02310000
                                                VALUE ' PGM: '.         02320000
           02  NOME-PGM-CICS               PIC X(8).                    02330000
           02  FILLER                      PIC X(8).                    02340000
                                                                        02350000
                                                                        02360000
      * -------------------------------------------------------------- *02370000
      *    DEFINIZIONE SQLA E TABELLE                                  *02380000
      * -------------------------------------------------------------- *02390000
           EXEC SQL  INCLUDE SQLCA          END-EXEC.                   02400000
      * ---                                                             02410000
           EXEC SQL  INCLUDE CWDIPE         END-EXEC.                           
                                                                        02470000
                                                                        02480000
      * -------------------------------------------------------------- *02500000
      *    DEFINIZIONE DELLA MAPPA                                     *02510000
      * -------------------------------------------------------------- *02520000
           COPY MF08MAP.                                                02530000
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
                                                                        02710000
      * -------------------------------------------------------------- *02720000
      *    DEFINIZIONE COMMAREA.                                       *        
      * -------------------------------------------------------------- *02760000
       01  W-COMMAREA.                                                  02770000
           COPY CWCOMMA.                                                02780000
                                                                        02846000
      * -------------------------------------------------------------- *02640000
      * DECLARE CURSOR                                                 *02650000
      * -------------------------------------------------------------- *02660000
                                                                                
           EXEC SQL DECLARE CUR-DIP CURSOR FOR                                  
                    SELECT  COD_MATRICOLA_DIP,                                  
                            COGNOME,                                            
                            NOME,                                               
                            DATA_NASCITA,                                       
                            QUALIFICA_INTERNA,                                  
                            COD_FISC                                            
                    FROM CPS04.CWDIPENDENTI                                     
                    WHERE                                                       
                            COGNOME LIKE :W-COGN                                
                    ORDER BY       COGNOME                                      
           END-EXEC.                                                            
                                                                                
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
           MOVE EIBTRMID TO CPS-TERM.                                   03010000
           PERFORM 1100-COND-ANOMAL.                                    03020000
           PERFORM 1300-TESTA-RIEN.                                     03030000
                                                                        03040000
      *---------------------------------------------------------------*         
      *   COM-GIRO  E VARIABILE DI COMMAREA  CHE SI USA PER CAPIRE    *         
      *   QUANDO  SI DOVREBE  FARE L'ANNULLAMENTO DELLA OCCORRENZA    *         
      *---------------------------------------------------------------*         
           IF  COM-GIRO = '2' OR  COM-GIRO = '3'                        03050000
             THEN                                                               
                PERFORM 1400-TASTI-LAST                                 03070000
             ELSE                                                       03100000
                PERFORM 1200-TASTI-FUNZ                                 03090000
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
                                                                        03290000
                                                                        03300000
                                                                        03310000
       1110-ABEND-CICS SECTION.                                         03320000
      *---------------*                                                 03330000
           MOVE W-ULT-LABEL  TO ULT-LABEL-CICS.                         03340000
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
                                                                        03460000
                                                                        03470000
       1120-COND-MFAIL SECTION.                                         03480000
      *---------------*                                                 03490000
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
                                PA1    (1210-TASTO-PA1)                 03720000
                                CLEAR  (1220-TASTO-CLEAR)               03720000
                                PF3    (1230-TASTO-PF3)                 03730000
                                PF4    (1240-TASTO-PF4)                 03730000
                                ANYKEY (1250-TASTO-ANYKEY)              03750000
                                END-EXEC.                               03760000
       1200-EX. EXIT.                                                   03770000
                                                                                
                                                                                
                                                                                
       1210-TASTO-PA1 SECTION.                                          03950000
      *--------------*                                                  03960000
                                                                                
           MOVE '** RITORNO AL CICS **' TO ERR-CICS.                    02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
       1210-EX. EXIT.                                                   04030000
                                                                                
                                                                                
                                                                        03780000
       1220-TASTO-CLEAR SECTION.                                        03950000
      *--------------*                    * RITORNO MENU' GENERALE *    03960000
           PERFORM 1900-DELETEQ-TS.                                             
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE W-PGM-MENU-GEN   TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04020000
       1220-EX. EXIT.                                                   04030000
                                                                        04050000
                                                                        04060000
                                                                        04070000
       1230-TASTO-PF3 SECTION.                                          04080000
      *--------------*                     * RITORNO PROG  PRECEDENTE * 04090000
           MOVE '1230-TASTO-PF3' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           PERFORM 1900-DELETEQ-TS.                                             
           EXEC CICS XCTL PROGRAM  (W-PGM-MENU-RAMO)                    03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04160000
       1230-EX. EXIT.                                                   04170000
                                                                        04180000
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1237-TASTO-PF7 PER  VEDERE LA MATRICOLA PRECEDENTE*          
      *----------------------------------------------------------------*        
                                                                                
       1237-TASTO-PF7 SECTION.                                          04080000
      *--------------*                                                  04090000
           MOVE '1237-TASTO-PF7' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           MOVE DFHCOMMAREA TO W-COMMAREA.                              04470000
           MOVE  SPACES TO  M-MSG-1O.                                   09650000
                                                                                
           IF COM-GIRO = '3'                                                    
             THEN                                                               
               MOVE -1      TO M-CONF-OPL                                       
               MOVE  0      TO SW-INSPECT                                       
               MOVE '013' TO W-COD-MSG-HOST                             04290000
               PERFORM 2999-CERCA-ERR                                   04330000
             ELSE                                                               
               MOVE COM-ITEM      TO CPS-ITEM-CODA                              
               IF CPS-ITEM-CODA = 1                                             
                  THEN                                                          
                     MOVE -1      TO M-COGNL                                    
                     MOVE  0      TO SW-INSPECT                                 
                     MOVE '018' TO W-COD-MSG-HOST                       04290000
                     PERFORM 2999-CERCA-ERR                             04330000
                  ELSE                                                          
                     SUBTRACT 1 FROM CPS-ITEM-CODA                              
                     MOVE  CPS-ITEM-CODA  TO COM-ITEM                           
                     PERFORM 1800-LEGGI-CODA                                    
                     PERFORM 1530-PROTE-MESS                            05310000
                     PERFORM 3140-INVIO-MAPPA                                   
                 END-IF                                                 04300000
           END-IF.                                                      04300000
                                                                        04160000
       1237-EX. EXIT.                                                   04170000
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1238-TASTO-PF8  PER  VEDERE LA MATRICOLA SUCESSIVA*          
      *----------------------------------------------------------------*        
                                                                                
                                                                                
       1238-TASTO-PF8 SECTION.                                          04080000
      *--------------*                                                  04090000
           MOVE '1238-TASTO-PF8' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           MOVE DFHCOMMAREA TO W-COMMAREA.                              04470000
           MOVE  SPACES TO  M-MSG-1O.                                   09650000
           IF COM-GIRO = '3'                                                    
             THEN                                                               
               MOVE -1      TO M-CONF-OPL                                       
               MOVE  0      TO SW-INSPECT                                       
               MOVE '013' TO W-COD-MSG-HOST                             04290000
               PERFORM 2999-CERCA-ERR                                   04330000
             ELSE                                                               
               MOVE COM-ITEM  TO CPS-ITEM-CODA                                  
               IF CPS-ITEM-CODA = COM-TOT-PAG                                   
                  THEN                                                          
                     MOVE -1      TO M-COGNL                                    
                     MOVE  0      TO SW-INSPECT                                 
                     MOVE '019' TO W-COD-MSG-HOST                       04290000
                     PERFORM 2999-CERCA-ERR                             04330000
                  ELSE                                                          
                     ADD 1    TO CPS-ITEM-CODA                                  
                     MOVE  CPS-ITEM-CODA  TO COM-ITEM                           
                     PERFORM 1800-LEGGI-CODA                                    
                     PERFORM 1530-PROTE-MESS                            05310000
                     PERFORM 3140-INVIO-MAPPA                                   
               END-IF                                                   04300000
           END-IF.                                                      04300000
                                                                        04160000
       1238-EX. EXIT.                                                   04170000
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1239-TASTO-ENTER      E'  PER CONTROLLARE  SE  NEL  *        
      * TERZO  GIRO  SI CONFERMA L'ANNULLAMENTO  O  MENO               *        
      *----------------------------------------------------------------*        
                                                                                
                                                                                
       1239-TASTO-ENTER  SECTION.                                       05310000
      *-----------------*                                               05320000
           MOVE '1239-TASTO-ENTER' TO W-ULT-LABEL.                      05330000
      * ---                                                             05340000
                                                                                
           IF COM-GIRO = '3'                                                    
            THEN                                                                
               INSPECT M-CONF-OPI REPLACING ALL LOW-VALUE BY ' '                
               INSPECT M-CONF-OPI REPLACING ALL '_' BY ' '                      
               MOVE    M-CONF-OPI TO W-CONF-OP                                  
                                                                                
               IF M-CONF-OPO = SPACES                                           
                  MOVE -1     TO M-CONF-OPL                                     
                  MOVE  '__'  TO M-CONF-OPO                                     
                  MOVE  0     TO SW-INSPECT                                     
                  MOVE '001'  TO W-COD-MSG-HOST                                 
                  PERFORM 2999-CERCA-ERR                                        
               END-IF                                                           
                                                                                
               IF M-CONF-OPO  NOT EQUAL 'SI' AND                                
                  M-CONF-OPO  NOT EQUAL 'NO'                                    
                  MOVE -1     TO M-CONF-OPL                                     
                  MOVE  '__'  TO M-CONF-OPO                                     
                  MOVE  0     TO SW-INSPECT                                     
                  MOVE '016'  TO W-COD-MSG-HOST                                 
                  PERFORM 2999-CERCA-ERR                                        
               END-IF                                                           
                                                                                
               IF M-CONF-OPO = 'SI'                                             
                 THEN                                                           
                    PERFORM 1450-ANNULLAMENTO                                   
                 ELSE                                                           
                    MOVE  '2'  TO COM-GIRO                                      
                    PERFORM 1520-RIPRISTINO-ATTR                                
                    PERFORM 3140-INVIO-MAPPA                            09960000
               END-IF                                                           
                                                                                
            ELSE                                                                
              MOVE  SPACES TO  M-MSG-1O                                         
              IF M-COGNO = COM-COGNOME                                          
                 THEN                                                           
                    MOVE  '3'   TO COM-GIRO                                     
                    PERFORM 2500-CONFERMA                                       
                    PERFORM 3140-INVIO-MAPPA                            09960000
                 ELSE                                                           
                    MOVE  '1'  TO COM-GIRO                                      
                    PERFORM 1900-DELETEQ-TS                                     
                    PERFORM 2120-NORMALIZZA                             05680000
                    PERFORM 2200-CONTROLLI                              05520000
                    PERFORM 3140-INVIO-MAPPA                            05520000
              END-IF                                                            
            END-IF.                                                     05390000
                                                                        05400000
       1239-EX. EXIT.                                                   05410000
                                                                                
                                                                                
                                                                                
       1240-TASTO-PF4 SECTION.                                          04080000
      *--------------*                     * RITORNO MENU' DI RAMO *    04090000
           MOVE '1240-TASTO-PF4' TO W-ULT-LABEL.                        04100000
      *---                                                              04110000
           PERFORM 1900-DELETEQ-TS.                                             
           EXEC CICS XCTL PROGRAM  (W-PGM-MENU-RAMO)                    03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04160000
       1240-EX. EXIT.                                                   04170000
                                                                                
                                                                                
                                                                                
       1250-TASTO-ANYKEY SECTION.                                       04220000
      *--------------*                                                  04230000
                                                                                
           IF COM-GIRO = '3'                                                    
               MOVE -1    TO M-CONF-OPL                                         
             ELSE                                                               
               MOVE -1    TO M-COGNL                                            
           END-IF.                                                              
           MOVE  0      TO SW-INSPECT.                                          
           MOVE '006' TO W-COD-MSG-HOST.                                04270000
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
                                                                        04650000
      *----------------------------------------------------------------*        
      * LA ROUTINE 1400-TASTI-LAST  E'  PER CONTROLLARE  I TASTI       *        
      * FUNZIONE   QUANDO  LA  MAPPA  E  RIEMPITA  CON  I  DATI        *        
      *----------------------------------------------------------------*        
                                                                        04660000
       1400-TASTI-LAST SECTION.                                         04670000
      *---------------*                                                 04680000
           EXEC CICS HANDLE AID                                         04690000
                                PA1    (1210-TASTO-PA1)                 03720000
                                CLEAR  (1220-TASTO-CLEAR)               03720000
                                PF3    (1230-TASTO-PF3)                 03730000
                                PF7    (1237-TASTO-PF7)                 03730000
                                PF8    (1238-TASTO-PF8)                 03730000
                                ENTER  (1239-TASTO-ENTER)               04740000
                                PF4    (1240-TASTO-PF4)                 03730000
                                ANYKEY (1250-TASTO-ANYKEY)              03750000
                                END-EXEC.                               04760000
       1400-EX. EXIT.                                                   04770000
                                                                        04810000
                                                                        05140000
                                                                                
                                                                        05420000
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1450-ANNULAMENTO   E'  PER  L'ANNULLAMENTO  DELLA   *        
      * OCCORRENZA   NELLA  TABELLA                                    *        
      *----------------------------------------------------------------*        
                                                                                
       1450-ANNULLAMENTO SECTION.                                       05450000
      *---------------*                                                 05460000
           MOVE '1450-ANNULLAMENTO' TO W-ULT-LABEL.                     05470000
      *----                                                             05480000
           MOVE M-MATRO         TO COD-MATRICOLA-DIP  OF DCL-CPSDIP.            
                                                                        04970000
           EXEC SQL                                                             
                DELETE                                                          
                    FROM CPS04.CWDIPENDENTI                                     
                WHERE COD_MATRICOLA_DIP = :DCL-CPSDIP.COD-MATRICOLA-DIP         
           END-EXEC.                                                    05020000
                                                                        05030000
           MOVE SQLCODE TO W-SQLCODE.                                   05040000
                                                                        05050000
           IF W-SQLCODE-OK                                                      
              THEN                                                              
                 MOVE  1       TO   COM-GIRO                                    
                 MOVE  1       TO   SW-INSPECT                                  
                 PERFORM 1920-IMPOSTA-COM                                       
                 PERFORM 1510-PULISCI-MAPPA                                     
                 PERFORM 1520-RIPRISTINO-ATTR                           05380000
                 PERFORM 1900-DELETEQ-TS                                        
                 PERFORM 1950-LINK                                              
                 IF  COM-COD-RIT = 0                                            
                   THEN                                                         
                      MOVE '017' TO W-COD-MSG-HOST                              
                      PERFORM 2999-CERCA-ERR                            05180000
                   ELSE                                                         
                      EXEC CICS SYNCPOINT ROLLBACK END-EXEC             03390000
                      MOVE '032' TO W-COD-MSG-HOST                              
                      PERFORM 2999-CERCA-ERR                            05180000
                 END-IF                                                 05110000
              ELSE                                                              
                 PERFORM 2998-DBERROR                                           
           END-IF.                                                      05110000
                                                                        05120000
                                                                                
       1450-EX. EXIT.                                                   05410000
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1510-PULISCI-MAPPA  E'  PER  PULIRE  TUTTI  I  CAMPI*        
      * DELLA   MAPPA                                                  *        
      *----------------------------------------------------------------*        
                                                                                
                                                                                
       1510-PULISCI-MAPPA  SECTION.                                     05310000
      *---------------*                                                 05460000
           MOVE '1510-PULISCI-MAPPA' TO W-ULT-LABEL.                    05470000
      *----                                                             05480000
           MOVE -1    TO M-COGNL.                                               
           MOVE SPACE TO M-MSG-1O      M-COGNO     M-NOMEO                      
                         M-MSG-2O      M-QU-INO    M-MATRO                      
                         M-A-NASCO     M-M-NASCO   M-G-NASCO                    
                         M-COD-FISCO                                            
                         M-TEST-CONFO  M-CONF-OPO  M-TES-FISSOO.                
                                                                                
                                                                                
       1510-EX.   EXIT.                                                 05450000
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1520-RIPRISTINO-ATTR   E'  PER  RIPRISTINARE  I     *        
      * ATTRIBUTI DEI CAMPI DELLA   MAPPA                              *        
      *----------------------------------------------------------------*        
                                                                                
       1520-RIPRISTINO-ATTR SECTION.                                    05310000
      *---------------*                                                 05460000
           MOVE '1520-RIPRISTINO-ATTR' TO W-ULT-LABEL.                  05470000
      *----                                                             05480000
           MOVE -1         TO  M-COGNL.                                         
           MOVE  UNPROT    TO  M-COGNA.                                         
           MOVE  FSET      TO  M-COGNA.                                         
           MOVE PROT-FSET  TO  M-NOMEA                                          
                               M-MATRA                                          
                               M-QU-INA                                         
                               M-COD-FISCA                                      
                               M-A-NASCA                                        
                               M-M-NASCA                                        
                               M-G-NASCA                                        
                               M-CONF-OPA.                                      
           MOVE SPACE      TO  M-TEST-CONFO                                     
                               M-CONF-OPO                                       
                               M-TES-FISSOO                                     
                               M-MSG-1O.                                        
                                                                                
       1520-EX.   EXIT.                                                 05450000
                                                                                
                                                                                
       1530-PROTE-MESS  SECTION.                                        05310000
      *---------------*                                                 05460000
           MOVE '1530-PROTE-MESS' TO W-ULT-LABEL.                       05470000
      *----                                                             05480000
           MOVE  -1     TO     M-COGNL.                                         
           MOVE UNPROT  TO     M-COGNA.                                         
           MOVE FSET    TO     M-COGNA.                                         
           MOVE PROT    TO     M-CONF-OPA.                                      
           MOVE SPACE   TO     M-TEST-CONFO                                     
                               M-CONF-OPO                                       
                               M-TES-FISSOO.                                    
                                                                                
       1530-EX.   EXIT.                                                 05450000
                                                                                
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1600-CARICA-CODA  GESTISCE  IL  CURSORE  DELLA      *        
      * TABELLA  E CHIAMA  DELLE  ROUTINE DI  LETURA  DELLA  TABELLA   *        
      * E  DI  SCRITURA  DELLA  CODA  TS                               *        
      *----------------------------------------------------------------*        
                                                                                
       1600-CARICA-CODA SECTION.                                        05450000
      *---------------*                                                 05460000
           MOVE '1600-CARICA-CODA' TO W-ULT-LABEL.                      05470000
      *----                                                             05480000
           MOVE    M-COGNI           TO   W-COGN.                               
           INSPECT W-COGN  REPLACING ALL LOW-VALUE BY '%'.                      
           INSPECT W-COGN  REPLACING ALL '_' BY '%'.                            
           INSPECT W-COGN  REPLACING ALL SPACES BY '%'.                         
      *                                                                         
           EXEC SQL OPEN CUR-DIP  END-EXEC.                                     
                                                                                
           MOVE SQLCODE TO W-SQLCODE.                                           
           IF NOT W-SQLCODE-OK                                                  
              THEN                                                              
               PERFORM 2998-DBERROR                                             
           END-IF.                                                              
                                                                                
           PERFORM 1650-FETCH UNTIL                                             
            (W-SQLCODE-NOT-FOUND OR NOT W-SQLCODE-OK).                          
           IF W-SQLCODE-NOT-FOUND                                               
               THEN                                                             
                 IF SW-SQL = 0                                                  
                    THEN                                                        
                      MOVE  1      TO SW-INSPECT                                
                      PERFORM 1510-PULISCI-MAPPA                                
                      PERFORM 1520-RIPRISTINO-ATTR                      05380000
                      MOVE '011' TO W-COD-MSG-HOST                      4290000 
                      PERFORM 2999-CERCA-ERR                                    
                    ELSE                                                        
                      MOVE 0  TO SW-SQL                                         
                      PERFORM 1000-INIZIO-ELAB                          02910000
                 END-IF                                                         
           END-IF.                                                              
                                                                                
           EXEC SQL CLOSE CUR-DIP  END-EXEC.                                    
                                                                                
           MOVE SQLCODE TO W-SQLCODE.                                           
           IF NOT W-SQLCODE-OK                                                  
              THEN                                                              
               PERFORM 2998-DBERROR                                             
           END-IF.                                                              
       1600-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
       1650-FETCH   SECTION.                                                    
      *---------------*                                                 05450000
      *    MOVE '1650-FETCH' TO W-ULT-LABEL.                            05460000
      *----                                                             05470000
           EXEC SQL FETCH   CUR-DIP                                             
           INTO   :DCL-CPSDIP.COD-MATRICOLA-DIP,                                
                  :DCL-CPSDIP.COGNOME,                                          
                  :DCL-CPSDIP.NOME,                                             
                  :DCL-CPSDIP.DATA-NASCITA,                                     
                  :DCL-CPSDIP.QUALIFICA-INTERNA,                                
                  :DCL-CPSDIP.COD-FISC                                          
           END-EXEC.                                                            
           MOVE SQLCODE TO W-SQLCODE.                                           
           IF W-SQLCODE-OK                                                      
               THEN                                                             
                 MOVE   1      TO  SW-SQL                                       
                 PERFORM 1700-SCRIVI-CODA                                       
           END-IF.                                                              
                                                                                
       1650-EX. EXIT.                                                           
                                                                                
                                                                                
       1700-SCRIVI-CODA SECTION.                                                
      *---------------*                                                 05450000
      *    MOVE '1700-SCRIVI-CODA' TO W-ULT-LABEL.                      05460000
      *----                                                             05470000
           MOVE COD-MATRICOLA-DIP  OF DCL-CPSDIP      TO CPS-MATR.              
           MOVE COGNOME            OF DCL-CPSDIP      TO CPS-COGNOME.           
           MOVE NOME               OF DCL-CPSDIP      TO CPS-NOME.              
           MOVE DATA-NASCITA       OF DCL-CPSDIP      TO CPS-DATA-NASC.         
           MOVE QUALIFICA-INTERNA  OF DCL-CPSDIP      TO CPS-QU-IN.             
           MOVE COD-FISC           OF DCL-CPSDIP      TO CPS-COD-FISC.          
                                                                                
           EXEC CICS HANDLE CONDITION QIDERR (1700-EX) END-EXEC.                
           EXEC CICS WRITEQ TS QUEUE  (CPSCODA)                                 
                               FROM   (CPS-DENOM)                               
                               LENGTH (CPS-LEN-CODA)                            
                               ITEM   (CPS-ITEM-CODA)                           
           END-EXEC.                                                            
           MOVE CPS-ITEM-CODA TO COM-TOT-PAG.                                   
           MOVE W-COMMAREA TO DFHCOMMAREA.                                      
           ADD 1 TO CPS-ITEM-CODA.                                              
                                                                                
       1700-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1800-LEGGI-CODA  FA  LA  LETURA  DELLA  CODA        *        
      *----------------------------------------------------------------*        
                                                                                
       1800-LEGGI-CODA SECTION.                                                 
      *---------------*                                                 05450000
      *    MOVE '1800-LEGGI-CODA' TO W-ULT-LABEL.                       05460000
      *----                                                             05470000
           EXEC CICS HANDLE CONDITION QIDERR (1800-EX) END-EXEC.                
           EXEC CICS READQ TS  QUEUE  (CPSCODA)                                 
                               INTO   (CPS-DENOM)                               
                               LENGTH (CPS-LEN-CODA)                            
                               ITEM   (CPS-ITEM-CODA)                           
           END-EXEC.                                                            
                                                                                
           PERFORM 1850-IMPOSTA-CAMPI.                                          
                                                                                
       1800-EX. EXIT.                                                           
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1850-IMPOSTA-CAMPI   FA  L'IMPOSTAZIONE  DEI  CAMPI *        
      * DELLA  CODA   NEI  CAMPI  DELLA  MAPPA                         *        
      *----------------------------------------------------------------*        
                                                                                
                                                                                
       1850-IMPOSTA-CAMPI SECTION.                                              
      *---------------*                                                 05450000
      *    MOVE '1850-IMPOSTA-CAMPI' TO W-ULT-LABEL.                    05460000
      *----                                                             05470000
           MOVE  -1                 TO M-COGNL.                                 
           MOVE  CPS-MATR           TO M-MATRO.                                 
           MOVE  CPS-COGNOME        TO M-COGNO.                                 
           MOVE  CPS-NOME           TO M-NOMEO.                                 
           MOVE  CPS-A-NASC         TO M-A-NASCO.                               
           MOVE  CPS-M-NASC         TO M-M-NASCO.                               
           MOVE  CPS-G-NASC         TO M-G-NASCO.                               
           MOVE  CPS-QU-IN          TO M-QU-INO.                                
           MOVE  CPS-COD-FISC       TO M-COD-FISCO.                             
           MOVE  M-COGNO            TO COM-COGNOME.                             
                                                                                
       1850-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 1900-DELETEQ-TS   FA  L'ANNULLAMENTO  DELLA  CODA   *        
      *----------------------------------------------------------------*        
                                                                                
       1900-DELETEQ-TS SECTION.                                                 
      *---------------*                                                 05450000
      *    MOVE '1900-DELETEQ-TS' TO W-ULT-LABEL.                       05460000
      *----                                                             05470000
           EXEC CICS HANDLE CONDITION QIDERR (1900-EX)  END-EXEC.               
           EXEC CICS DELETEQ TS QUEUE        (CPSCODA)  END-EXEC.               
                                                                                
       1900-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
       1920-IMPOSTA-COM SECTION.                                                
      *---------------*                                                 05450000
      *    MOVE '1920-IMPOSTA-COM' TO W-ULT-LABEL.                      05460000
      *----                                                             05470000
           MOVE  M-MATRO          TO COM-MATRICOLA.                             
           MOVE  M-COGNO          TO COM-COGNOME.                               
           MOVE  M-NOMEO          TO COM-NOME.                                  
           MOVE  M-A-NASCO        TO W-AA-NASC.                                 
           MOVE  M-M-NASCO        TO W-MM-NASC.                                 
           MOVE  M-G-NASCO        TO W-GG-NASC.                                 
           MOVE  W-DATA-NASCITA   TO COM-DATA-NASCITA.                          
           MOVE  M-QU-INO         TO COM-QUALIFICA-INTERNA.                     
           MOVE  M-COD-FISCO      TO COM-COD-FI.                                
                                                                                
       1920-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
       1950-LINK SECTION.                                                       
      *---------------*                                                 05450000
      *    MOVE '1950-LINK' TO W-ULT-LABEL.                             05460000
      *----                                                             05470000
           MOVE W-COMMAREA    TO    DFHCOMMAREA.                                
           EXEC CICS LINK PROGRAM  (W-PGM-VSAM)                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04160000
                                                                                
       1950-EX. EXIT.                                                           
                                                                                
                                                                                
                                                                                
       2000-CORPO-ELAB SECTION.                                                 
      *---------------*                                                 05450000
      *    MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       05460000
      *----                                                             05470000
           IF W-NOME-PGM = COM-NOME-PGM                                 05490000
            THEN                                                        05500000
              PERFORM 2100-RECEIVE                                      05510000
              PERFORM 2200-CONTROLLI                                    05520000
              IF COM-GIRO = '3'                                                 
                   MOVE  SPACES TO M-MSG-1O                                     
                   PERFORM 2500-CONFERMA                                05530000
              END-IF                                                            
            ELSE                                                        05540000
              PERFORM 2900-RIEMP-MASK                                   05550000
           END-IF.                                                      05560000
                                                                        05570000
       2000-EX. EXIT.                                                   05580000
                                                                        05590000
                                                                        05620000
       2100-RECEIVE SECTION.                                            05630000
      *------------*                                                    05640000
           MOVE '2100-RECEIVE' TO W-ULT-LABEL.                          05650000
      * ---                                                             05660000
                                                                                
           PERFORM 2110-REC-MAPPA.                                      05670000
           PERFORM 2120-NORMALIZZA.                                     05680000
                                                                        05700000
       2100-EX. EXIT.                                                   05710000
                                                                        05750000
                                                                                
                                                                                
       2110-REC-MAPPA SECTION.                                          05760000
      *--------------*                                                  05770000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        05780000
      * ---                                                             05790000
           EXEC CICS RECEIVE MAP    ('MF08MAP')                         05800000
                             MAPSET ('MF08MAP') END-EXEC.               05810000
                                                                        05820000
       2110-EX. EXIT.                                                   05830000
                                                                        05840000
                                                                                
                                                                        05870000
       2120-NORMALIZZA SECTION.                                         05880000
      *---------------*                                                 05890000
           MOVE '2120-NORMALIZZA' TO W-ULT-LABEL.                       05900000
      * ---                                                             05910000
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-COGNI REPLACING ALL '_' BY ' '.                            
                                                                        06020000
           MOVE SPACE TO M-MSG-1O                                       06380000
                         M-MSG-2O                                       06380000
                         M-TEST-CONFO                                   06380000
                         M-CONF-OPO                                     06380000
                         M-TES-FISSOO.                                  06380000
       2120-EX. EXIT.                                                   06110000
                                                                        06120000
                                                                        06150000
      *----------------------------------------------------------------*        
      * LA ROUTINE 2130-PROT-ATTR   FA  LA  PROTEZIONE  DEI  CAMPI     *        
      *DELLA  MAPPA                                                   *         
      *----------------------------------------------------------------*        
                                                                                
       2130-PROT-ATTR SECTION.                                          06160000
      *--------------*                                                  06170000
           MOVE '2130-PROT-ATTR' TO W-ULT-LABEL.                        06180000
      * ---                                                             06190000
           MOVE   -1        TO M-COGNL.                                         
           MOVE   UNPROT    TO M-COGNA.                                         
           MOVE   FSET      TO M-COGNA.                                         
           MOVE   PROT-FSET TO                                                  
                               M-NOMEA                                          
                               M-A-NASCA                                        
                               M-M-NASCA                                        
                               M-G-NASCA                                        
                               M-QU-INA                                         
                               M-MATRA                                          
                               M-COD-FISCA.                                     
       2130-EX. EXIT.                                                   06220000
                                                                        06230000
                                                                        06250000
                                                                        06260000
      *----------------------------------------------------------------*        
      * LA ROUTINE 2200-CONTROLLI   NELL  PRIMO  GIRO  CHIAMA   LA     *        
      * ROUTINE DI CARICAMENTO  DELLA  CODA                            *        
      *----------------------------------------------------------------*        
                                                                                
       2200-CONTROLLI SECTION.                                          06270000
      *--------------*                                                  06280000
           MOVE '2200-CONTROLLI' TO W-ULT-LABEL.                        06290000
      * ---                                                             06300000
                                                                        06440000
           IF W-COD-MSG-HOST NOT = SPACE                                06710000
              PERFORM 2999-CERCA-ERR                                    06720000
           END-IF.                                                      06730000
                                                                        07170000
           IF COM-GIRO = '1'                                                    
             THEN PERFORM 1600-CARICA-CODA                                      
                  MOVE 1 TO CPS-ITEM-CODA                                       
                  MOVE 1 TO COM-ITEM                                            
                  PERFORM 1800-LEGGI-CODA                                       
                  PERFORM 2130-PROT-ATTR                                05690000
                  MOVE '2'    TO COM-GIRO                                       
                  PERFORM 1400-TASTI-LAST                               03070000
           END-IF.                                                              
                                                                                
       2200-EX. EXIT.                                                   07180000
                                                                        05400000
                                                                        07750000
                                                                        09000000
      *----------------------------------------------------------------*        
      * LA ROUTINE 2500-CONFERMA   IMPOSTA   NELLA  MAPPA   LA         *        
      * DOMANDA  PER CONFERMARE  L'ANNULLAMENTO                        *        
      *----------------------------------------------------------------*        
       2500-CONFERMA SECTION.                                           09010000
      *-------------*                                                   09020000
           MOVE '2500-CONFERMA' TO W-ULT-LABEL.                         09030000
      *                                                                         
      * ---                                                             05340000
           MOVE  PROT-FSET   TO  M-COGNA.                                       
           MOVE 'CONFERMA OPERAZIONE:'    TO M-TEST-CONFO.                      
           MOVE -1     TO M-CONF-OPL.                                           
           MOVE ALL '_'   TO M-CONF-OPO.                                        
           MOVE FSET      TO M-CONF-OPA.                                        
           MOVE '(SI/NO)' TO M-TES-FISSOO.                                      
                                                                        09210000
       2500-EX. EXIT.                                                   09220000
                                                                        09230000
      *----------------------------------------------------------------*        
      * LA ROUTINE 2900-RIEMP-MASK   PREPARA  LA  MAPPA  VUOTA         *        
      *----------------------------------------------------------------*        
                                                                                
       2900-RIEMP-MASK SECTION.                                         09270000
      *---------------*                                                 09280000
           MOVE '2900-RIEMP-MASK' TO W-ULT-LABEL.                       09290000
      *---                                                              09300000
           MOVE LOW-VALUE  TO MF08MAPO.                                 09310000
           MOVE -1         TO  M-COGNL.                                         
           MOVE UNPROT     TO  M-COGNA.                                         
           MOVE FSET       TO  M-COGNA.                                         
           MOVE ALL '_'    TO  M-COGNO                                          
                               M-NOMEO                                          
                               M-A-NASCO                                        
                               M-M-NASCO                                        
                               M-G-NASCO                                        
                               M-QU-INO                                         
                               M-MATRO                                          
                               M-COD-FISCO.                                     
           MOVE PROT-FSET  TO  M-NOMEA                                          
                               M-MATRA                                          
                               M-QU-INA                                         
                               M-COD-FISCA                                      
                               M-A-NASCA                                        
                               M-M-NASCA                                        
                               M-G-NASCA.                                       
                                                                                
       2900-EX. EXIT.                                                   09370000
                                                                        09380000
                                                                        09390000
      *----------------------------------------------------------------*        
      * LA ROUTINE 2998-DBERROR   GESTISCE  L'ERRORE  SQL              *        
      *----------------------------------------------------------------*        
                                                                        09410000
       2998-DBERROR  SECTION.                                           09420000
      *------------*                                                    09430000
           MOVE W-ULT-LABEL  TO ULT-LABEL-SQL.                          09440000
           MOVE W-TRS-ID   TO TRS-ID-SQL.                               09450000
           MOVE W-NOME-PGM TO NOME-PGM-SQL.                             09460000
           MOVE SQLCODE    TO SQL-CODICE.                               09470000
           MOVE ERR-SQL    TO M-MSG-1O.                                 09480000
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       09490000
           PERFORM 3000-FINE-ELAB.                                      09500000
                                                                        09510000
       2998-EX. EXIT.                                                   09520000
                                                                        09530000
                                                                        09540000
      *----------------------------------------------------------------*        
      * LA ROUTINE 2999-CERCA-ERR  CONTROLLA  NELLA  TABELLA  DEI      *        
      * MESSAGGI  D'ERRORE  E  IMPOSTA  IL  MASSAGGIO  IN CAMPO  MAPPA *        
      *----------------------------------------------------------------*        
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
                  WHEN W-COD-MSG-HOST  =  ELEM-COD-MSG(IND-TAB)         09660000
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
             THEN                                                               
              PERFORM 3100-RIENTRO                                      09820000
             ELSE                                                               
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
                                                                                
      *----------------------------------------------------------------*        
      * LA ROUTINE 3130-FORMATTA-MAPPA  FORMATTA  LA  MAPPA  PRIMA     *        
      * DI  INVIARLA                                                   *        
      *----------------------------------------------------------------*        
                                                                                
       3130-FORMATTA-MAPPA SECTION.                                     10160000
      *-------------------*                                             10170000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   10180000
      *---                                                              10190000
           IF M-COGNA = PROT-FSET                                               
               OR SW-INSPECT = 0                                        04290000
            THEN                                                        10210000
              NEXT SENTENCE                                             10220000
            ELSE                                                        10230000
              INSPECT M-COGNO   REPLACING ALL LOW-VALUE BY '_'                  
              INSPECT M-COGNO   REPLACING ALL SPACE BY '_'                      
                                                                        10270000
              INSPECT M-NOMEO       REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-NOMEO       REPLACING ALL SPACE BY '_'                  
              INSPECT M-A-NASCO     REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-A-NASCO     REPLACING ALL SPACE BY '_'                  
              INSPECT M-M-NASCO     REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-M-NASCO     REPLACING ALL SPACE BY '_'                  
              INSPECT M-G-NASCO     REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-G-NASCO     REPLACING ALL SPACE BY '_'                  
              INSPECT M-QU-INO      REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-QU-INO      REPLACING ALL SPACE BY '_'                  
              INSPECT M-MATRO       REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-MATRO       REPLACING ALL SPACE BY '_'                  
              INSPECT M-COD-FISCO   REPLACING ALL LOW-VALUE BY '_'              
              INSPECT M-COD-FISCO   REPLACING ALL SPACE BY '_'                  
           END-IF.                                                      10260000
                                                                        10430000
                                                                        10510000
       3130-EX. EXIT.                                                   10680000
                                                                        10690000
                                                                        10720000
      *----------------------------------------------------------------*        
      * LA ROUTINE 3140-INVIO-MAPPA  INVIA  E  FA  RETURN  TRANSID     *        
      * DELLA  MAPPA  STESA                                            *        
      *----------------------------------------------------------------*        
                                                                                
       3140-INVIO-MAPPA SECTION.                                        10730000
      *----------------*                                                10740000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      10750000
      *---                                                              10760000
           IF W-NOME-PGM = COM-NOME-PGM                                 10770000
            THEN                                                        10780000
              EXEC CICS SEND                                            10790000
                        MAP    ('MF08MAP')                              10800000
                        MAPSET ('MF08MAP')                              10810000
                        CURSOR                                          10820000
                        DATAONLY
                        FREEKB
              END-EXEC                                                  10830000
            ELSE                                                        10840000
              MOVE W-NOME-PGM  TO COM-NOME-PGM                          10850000
              EXEC CICS SEND                                            10860000
                        MAP    ('MF08MAP')                              10870000
                        MAPSET ('MF08MAP')                              10880000
                        CURSOR                                          10890000
                        ERASE
                        FREEKB
              END-EXEC                                                  10900000
           END-IF.                                                      10910000
                                                                        10920000
           EXEC CICS RETURN                                             10930000
                     TRANSID  ('RR08')                                  10940000
                     COMMAREA (W-COMMAREA)                              10950000
                     LENGTH   (W-LEN)                                   10960000
                     END-EXEC.                                          10970000
                                                                        10980000
       3140-EX. EXIT.                                                   10990000
                                                                        11000000
                                                                        11010000
      *----------------------------------------------------------------*        
      * LA ROUTINE 3200-PASSA-CTL    PASSA  IL  CONTROLLO  NEI  ALTRI  *        
      * PROGRAMMI                                                      *        
      *----------------------------------------------------------------*        
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
