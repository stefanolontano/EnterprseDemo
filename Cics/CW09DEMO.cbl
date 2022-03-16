       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW09DEMO.                                            00020000
      * ---                                                             00030000
      ******************************************************************00040000
      * PROGETTO        : CPS                                          *00050000
      * ID. TRANSAZIONE : RR09                                         *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :                                              *00080000
      * OGGETTO         :                                              *        
      * TIPO OPERAZIONE : VISUALIZZAZIONE                               00100000
      * TABELLE USATE   : IETDIPENDENTI                                *        
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 05/03/1999                                   *        
      * ULTIMA MODIFICA : 08/03/1999                                   *        
      ******************************************************************00150000
      ******************************************************************00040000
      * QUESTO PROGRAMMA CONSENTE DI VISUALIZZARE I DATI CONTENUTI     *00050000
      * NELLA TABELLA IETDIPENDENTI ATTRAVERSO UNA RICERCA PER COGNOME *00060000
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
       01  W-A-NASC                        PIC X(4)  VALUE SPACES.              
       01  W-M-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-G-NASC                        PIC X(2)  VALUE SPACES.              
       01  W-QU-IN                         PIC X(08) VALUE SPACES.              
       01  W-DATA-NASCITA.                                                      
         02  W-AA-NASC                     PIC X(4)  VALUE SPACES.              
         02  FILLER                        PIC X     VALUE '-'.                 
         02  W-MM-NASC                     PIC X(2)  VALUE SPACES.              
         02  FILLER                        PIC X     VALUE '-'.                 
         02  W-GG-NASC                     PIC X(2)  VALUE SPACES.              
                                                                        00380000
       01  W-COMODO1                       PIC X(30).                   00440043
       01  W-COMODO1-R  REDEFINES  W-COMODO1.                           00440043
          05  W-ELEMENTO1   OCCURS  30.                                 00440043
              10  W-ELEM-APP1        PIC X.                             00440043
       01  W-IND1                          PIC 99     VALUE 0.          00440043
                                                                                
       01  W-COMODO2                        PIC X(30).                  00440043
       01  W-COMODO2-R  REDEFINES  W-COMODO2.                           00440043
          05  W-ELEMENTO2   OCCURS  30.                                 00440043
              10  W-ELEM-APP2        PIC X.                             00440043
       01  W-IND2                          PIC 99     VALUE 0.          00440043
                                                                        01580000
      * -------------------------------------------------------------- *01590000
      *    DEFINIZIONE CAMPI STANDART DELLA TRANSAZIONE                *01600000
      * -------------------------------------------------------------- *01610000
       01  W-CTL-END                       PIC  X(4)   VALUE 'LOOP'.    01630000
       01  W-NOME-PGM                      PIC  X(8)   VALUE 'CW09DEMO'.01640000
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
           02  FILLER                      PIC XXX    VALUE 'CPS'.      00470000
           02  IE-TERM                     PIC X(4)   VALUE SPACES.     00480000
                                                                        00500000
       01  CPS-LEN-CODA                    PIC S9(4)  COMP VALUE +84.   00460000
       01  CPS-ITEM-CODA                   PIC S9(4)  COMP VALUE +0.    00460000
       01  CPS-DENOM.                                                   00460000
         03  CPS-MATR                      PIC 9(5)   VALUE 0.                  
         03  CPS-COGNOME                   PIC X(30)  VALUE SPACE.              
         03  CPS-NOME                      PIC X(15)  VALUE SPACE.              
         03  CPS-DATA-NASC.                                                     
             05  CPS-A-NASC                PIC X(4)   VALUE SPACE.              
             05  FILLER                    PIC X      VALUE '-'.                
             05  CPS-M-NASC                PIC X(2)   VALUE SPACE.              
             05  FILLER                    PIC X      VALUE '-'.                
             05  CPS-G-NASC                PIC X(2)   VALUE SPACE.              
         03  CPS-QU-IN                     PIC X(8)   VALUE SPACE.              
         03  CPS-COD-FISC                  PIC X(16)  VALUE SPACE.              
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
           EXEC SQL  INCLUDE SQLCA   END-EXEC.                          02400000
                                                                        02410000
           EXEC SQL  INCLUDE CWDIPE  END-EXEC.                                  
                                                                        02480000
      * -------------------------------------------------------------- *02500000
      *    DEFINIZIONE DELLA MAPPA                                     *02510000
      * -------------------------------------------------------------- *02520000
           COPY MF09MAP.                                                02530000
                                                                        02560000
      * -------------------------------------------------------------- *02570000
      *    DEFINIZIONE DELLE COPY COMUNI                               *02580000
      * -------------------------------------------------------------- *02590000
           COPY CWATTRIB.                                               02600000
           COPY CWMESS.                                                 02610000
           COPY DFHAID.                                                 02620000
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
                    FROM    CPS04.CWDIPENDENTI                                  
                    WHERE                                                       
                            COGNOME LIKE :W-COMODO1                             
                    ORDER BY COGNOME                                            
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
           MOVE EIBTRMID TO IE-TERM.                                    03010000
           PERFORM 1100-COND-ANOMAL.                                    03020000
           PERFORM 1300-TESTA-RIEN.                                     03030000
           PERFORM 1400-TASTI-LAST.                                     03070000
                                                                                
                                                                        03110000
       1000-EX. EXIT.                                                   03120000
      *                                                                 03180000
                                                                        03160000
      *                                                                 03180000
       1100-COND-ANOMAL SECTION.                                        03170000
      *-------------------------*                                       03180000
           EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        03190000
                                                           END-EXEC.    03200000
           EXEC CICS HANDLE CONDITION  MAPFAIL (1120-COND-MFAIL)        03210000
                                                           END-EXEC.    03230000
                                                                        03240000
           EXEC SQL WHENEVER SQLERROR GO TO 2998-DBERROR   END-EXEC.    03250000
                                                                        03260000
       1100-EX. EXIT.                                                   03270000
      *                                                                 03300000
                                                                                
      *                                                                 03310000
       1110-ABEND-CICS SECTION.                                         03320000
      *------------------------*                                        03330000
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
      *                                                                 03470000
                                                                                
      *                                                                 03470000
       1120-COND-MFAIL SECTION.                                         03480000
      *------------------------*                                        03490000
           EXEC CICS XCTL PROGRAM ('CW03DEMO')                          03500000
                          COMMAREA (W-COMMAREA)                                 
                          LENGTH (W-LEN)                                        
           END-EXEC.                                                            
                                                                        03510000
       1120-EX. EXIT.                                                   03520000
      *                                                                 03490000
                                                                                
      *                                                                 04090000
       1220-TASTO-CLEAR SECTION.                                        04080000
      *-----------------------             * RITORNO MENU' GENERALE *   04090000
           MOVE '1230-TASTO-CLEAR' TO W-ULT-LABEL.                      04100000
      *                                                                 04110000
           PERFORM 1790-DELETEQ-TS.                                             
           MOVE 'END'            TO W-CTL-END.                          04130000
           MOVE 'CW02DEMO'       TO W-XCTL-PGM.                         04140000
           PERFORM 3000-FINE-ELAB.                                      04150000
                                                                        04160000
       1220-EX. EXIT.                                                   04170000
      *                                                                 04090000
                                                                        03780000
       1230-TASTO-PF3 SECTION.                                          03950000
      *--------------*                * RITORNO PROGRAMMA PRECEDENTE *  03960000
           PERFORM 1790-DELETEQ-TS.                                             
           MOVE +250      TO W-LEN.                                             
           EXEC CICS XCTL PROGRAM  ('CW03DEMO')                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04020000
       1230-EX. EXIT.                                                   04030000
      *                                                                 04090000
                                                                        03780000
      *                                                                 04090000
       1240-TASTO-PF4 SECTION.                                          03950000
      *--------------*                * RITORNO MENU DI RAMO *          03960000
           PERFORM 1790-DELETEQ-TS.                                             
           MOVE +250      TO W-LEN.                                             
           EXEC CICS XCTL PROGRAM  ('CW03DEMO')                         03990000
                          COMMAREA (W-COMMAREA)                         04000000
                          LENGTH   (W-LEN) END-EXEC.                    04010000
                                                                        04020000
       1240-EX. EXIT.                                                   04030000
      *                                                                 04090000
                                                                                
      *                                                                 04090000
       1237-TASTO-PF7 SECTION.                                          04080000
      *------------------------                                         04090000
           MOVE '1237-TASTO-PF7' TO W-ULT-LABEL.                        04100000
                                                                        04110000
           IF COM-ITEM = 0                                                      
           THEN                                                                 
              MOVE -1    TO M-COGNL                                     04290000
              MOVE '013' TO W-COD-MSG-HOST                              04290000
              PERFORM 2999-CERCA-ERR                                    04330000
           ELSE                                                                 
              IF COM-ITEM = 1                                                   
              THEN                                                              
                 MOVE COM-COGNOME  TO M-COGNI                           04290000
                 MOVE -1    TO M-COGNL                                  04290000
                 MOVE '018' TO W-COD-MSG-HOST                           04290000
                 PERFORM 2999-CERCA-ERR                                 04330000
              ELSE                                                              
                 SUBTRACT 1 FROM  COM-ITEM                                      
                               CPS-ITEM-CODA                                    
                 PERFORM 2610-LEGGI-CODA                                        
              END-IF                                                    04300000
           END-IF.                                                      04300000
           PERFORM 3000-FINE-ELAB.                                      05380000
                                                                        04160000
       1237-EX. EXIT.                                                   04170000
      *                                                                 04090000
                                                                                
      *                                                                 04090000
       1238-TASTO-PF8 SECTION.                                          04080000
      *------------------------                                         04090000
           MOVE '1238-TASTO-PF8' TO W-ULT-LABEL.                        04100000
                                                                        04110000
           IF COM-ITEM = 0                                                      
           THEN                                                                 
              MOVE -1    TO M-COGNL                                     04290000
              MOVE '013' TO W-COD-MSG-HOST                              04290000
              PERFORM 2999-CERCA-ERR                                    04330000
           ELSE                                                                 
              IF COM-ITEM = COM-TOT-PAG                                         
              THEN                                                              
                 MOVE COM-COGNOME  TO M-COGNI                           04290000
                 MOVE -1    TO M-COGNL                                  04290000
                 MOVE '019' TO W-COD-MSG-HOST                           04290000
                 PERFORM 2999-CERCA-ERR                                 04330000
              ELSE                                                              
                 ADD 1    TO CPS-ITEM-CODA                                      
                             COM-ITEM                                           
                 PERFORM 2610-LEGGI-CODA                                        
              END-IF                                                    04300000
           END-IF.                                                      04300000
           PERFORM 3000-FINE-ELAB.                                      05380000
                                                                        04160000
       1238-EX. EXIT.                                                   04170000
      *                                                                         
                                                                                
      *                                                                         
       1239-TASTO-ENTER.                                                   70000
      *--------------*                                                  06280000
           MOVE '1239-TASTO-ENTER' TO W-ULT-LABEL.                      06290000
      * ---                                                             06300000
           PERFORM 2120-NORMALIZZA.                                     05680000
           PERFORM 2130-NORM-ATTR.                                      05690000
           MOVE SPACES TO M-MSG-1O.                                             
           MOVE SPACES TO M-MSG-2O.                                             
           INSPECT M-COGNI  REPLACING ALL '_'       BY SPACES.                  
           INSPECT M-COGNI  REPLACING ALL LOW-VALUE BY SPACES.                  
           MOVE M-COGNI     TO W-COGN.                                  06440000
           MOVE SPACES      TO W-COD-MSG-HOST.                          06440000
           IF W-COGN = SPACE                                                    
              MOVE FSET-BRT TO M-COGNA                                          
              MOVE -1       TO M-COGNL                                          
              MOVE '001'    TO W-COD-MSG-HOST                           06420000
           END-IF.                                                      06430000
                                                                        06440000
           IF W-COD-MSG-HOST NOT = SPACE                                06710000
           THEN                                                         06430000
              PERFORM 2999-CERCA-ERR                                    06720000
           ELSE                                                                 
              IF M-COGNI = COM-COGNOME                                          
                 MOVE -1       TO M-COGNL                                       
                 PERFORM 3000-FINE-ELAB                                 05380000
              ELSE                                                              
                 PERFORM 2131-PULISCI-MAPPA                             05690000
                 PERFORM 1790-DELETEQ-TS                                        
                 PERFORM 2450-CARICA-CODA                                       
                 IF CPS-ITEM-CODA = 0                                           
                     MOVE 0        TO COM-ITEM                                  
                     MOVE FSET-BRT TO M-COGNA                                   
                     MOVE -1       TO M-COGNL                                   
                     MOVE '011'    TO W-COD-MSG-HOST                    06420000
                     MOVE SPACES   TO COM-COGNOME                       06420000
                     PERFORM 2999-CERCA-ERR                             06720000
                 ELSE                                                           
                     MOVE 1 TO CPS-ITEM-CODA                                    
                     MOVE CPS-ITEM-CODA TO COM-ITEM                             
                     PERFORM 2610-LEGGI-CODA                                    
                     PERFORM 3000-FINE-ELAB                             05380000
                 END-IF                                                 06730000
              END-IF                                                    06730000
            END-IF.                                                     06730000
                                                                                
       1239-EX. EXIT.                                                   07180000
      *                                                                         
                                                                                
      *                                                                         
       1250-TASTO-ANYKEY SECTION.                                       04220000
      *--------------*                                                  04230000
                                                                        04240000
           MOVE '006' TO W-COD-MSG-HOST.                                04270000
                                                                                
           MOVE -1    TO M-COGNL.                                               
           PERFORM 2999-CERCA-ERR.                                      04330000
                                                                        04340000
       1250-EX. EXIT.                                                   04350000
      *                                                                 04360000
                                                                                
      *                                                                 04360000
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
      *                                                                 04500000
                                                                                
      *                                                                 04500000
       1310-TRANS-DIS SECTION.                                          04540000
      *--------------*                                                  04550000
           MOVE '*** TRANSAZIONE NON PERMESSA ***' TO ERR-CICS.         04560000
           EXEC CICS SEND TEXT FROM   (ERR-CICS)                        04570000
                               LENGTH (78)                              04580000
                               ERASE WAIT END-EXEC.                     04590000
           EXEC CICS RETURN END-EXEC.                                   04600000
                                                                        04610000
       1310-EX. EXIT.                                                   04620000
      *                                                                 04500000
                                                                                
      *                                                                 04500000
       1400-TASTI-LAST SECTION.                                         04670000
      *---------------*                                                 04680000
           MOVE '1400-TAST-LAST' TO W-ULT-LABEL.                        05470000
           EXEC CICS HANDLE AID                                         04690000
                                CLEAR  (1220-TASTO-CLEAR)               04710000
                                PF3    (1230-TASTO-PF3)                 04710000
                                PF7    (1237-TASTO-PF7)                 03730000
                                PF8    (1238-TASTO-PF8)                 03730000
                                ENTER  (1239-TASTO-ENTER)               04740000
                                PF4    (1240-TASTO-PF4)                 04710000
                                ANYKEY (1250-TASTO-ANYKEY)              04750000
                                END-EXEC.                               04760000
       1400-EX. EXIT.                                                   04770000
      *                                                                 04810000
                                                                        05140000
      *                                                                         
       2450-CARICA-CODA SECTION.                                        05450000
      *---------------*                                                 05460000
           MOVE '2450-CARICA-CODA' TO W-ULT-LABEL.                      05470000
      *                                                                 05480000
                                                                                
           MOVE M-COGNI TO W-COGN.                                              
           MOVE W-COGN TO  W-COMODO1.                                   06300043
           MOVE SPACES TO  W-COMODO2.                                   06300043
                                                                        06300043
           PERFORM VARYING W-IND1 FROM 1 BY 1                           05180043
                   UNTIL   W-ELEM-APP1(W-IND1) NOT = SPACE              05190043
                           OR   W-IND1  > 30                            05190043
      *            MOVE  '%'   TO  W-ELEM-APP1(W-IND1)                  05200043
           END-PERFORM.                                                 05220043
           PERFORM VARYING W-IND2 FROM 1 BY 1                           05180043
                   UNTIL   W-IND1  > 30                                 05190043
                   MOVE W-ELEM-APP1(W-IND1) TO                          05200043
                        W-ELEM-APP2(W-IND2)                             05200043
                        ADD 1 TO W-IND1                                 05200043
           END-PERFORM.                                                 05220043
           MOVE W-COMODO2 TO W-COMODO1                                          
           PERFORM VARYING W-IND1 FROM 30 BY -1                         05180043
                   UNTIL   W-ELEM-APP1(W-IND1) NOT = SPACE              05190043
                           OR   W-IND1      = 0                         05190043
                   MOVE  '%'   TO  W-ELEM-APP1(W-IND1)                  05200043
           END-PERFORM.                                                 05220043
      *    INSPECT W-COGN  REPLACING ALL LOW-VALUE BY '%'.                      
      *    INSPECT W-COGN  REPLACING ALL SPACES BY '%'.                         
           EXEC SQL OPEN CUR-DIP  END-EXEC.                                     
                                                                                
           PERFORM 2460-FETCH UNTIL W-SQLCODE-NOT-FOUND.                        
                                                                                
           EXEC SQL CLOSE CUR-DIP  END-EXEC.                                    
       2450-EX. EXIT.                                                           
                                                                                
      ******************************************************************        
      * CICLO DI RICERCA DEI DATI                                      *        
      ******************************************************************        
                                                                                
       2460-FETCH SECTION.                                                      
      *---------------*                                                 05450000
           MOVE '2460-FETCH' TO W-ULT-LABEL.                            05460000
                                                                        05470000
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
              PERFORM 2500-SCRIVI-CODA                                          
           ELSE                                                                 
             IF NOT W-SQLCODE-NOT-FOUND                                         
             THEN                                                               
               PERFORM 2998-DBERROR                                             
             END-IF                                                             
           END-IF.                                                              
                                                                                
       2460-EX. EXIT.                                                           
      *                                                                         
                                                                                
      *                                                                         
       2500-SCRIVI-CODA SECTION.                                                
      *---------------*                                                 05450000
           MOVE '2500-SCRIVI-CODA' TO W-ULT-LABEL.                      05460000
      *                                                                 05470000
           MOVE COD-MATRICOLA-DIP  OF DCL-CPSDIP      TO CPS-MATR.              
           MOVE COGNOME            OF DCL-CPSDIP      TO CPS-COGNOME.           
           MOVE NOME               OF DCL-CPSDIP      TO CPS-NOME.              
           MOVE DATA-NASCITA       OF DCL-CPSDIP      TO CPS-DATA-NASC.         
           MOVE QUALIFICA-INTERNA  OF DCL-CPSDIP      TO CPS-QU-IN.             
           MOVE COD-FISC           OF DCL-CPSDIP      TO CPS-COD-FISC.          
                                                                                
           EXEC CICS HANDLE CONDITION QIDERR (2500-EX) END-EXEC.                
           EXEC CICS WRITEQ TS QUEUE  (CPSCODA)                                 
                               FROM   (CPS-DENOM)                               
                               LENGTH (CPS-LEN-CODA)                            
                               ITEM   (CPS-ITEM-CODA)                           
           END-EXEC.                                                            
           MOVE     CPS-ITEM-CODA    TO COM-TOT-PAG.                            
           ADD 1 TO CPS-ITEM-CODA.                                              
                                                                                
       2500-EX. EXIT.                                                           
      *                                                                         
      ******************************************************************        
      * LETTURA DELLA CODA E INVIO IN MAPPA DEI CAMPI VALORIZZATI      *        
      ******************************************************************        
                                                                        05450000
       2610-LEGGI-CODA SECTION.                                                 
      *---------------*                                                 05450000
           MOVE '2610-LEGGI-CODA' TO W-ULT-LABEL.                       05460000
      *                                                                 05470000
           EXEC CICS HANDLE CONDITION QIDERR (2610-EX) END-EXEC.                
           EXEC CICS READQ TS  QUEUE  (CPSCODA)                                 
                               INTO   (CPS-DENOM)                               
                               LENGTH (CPS-LEN-CODA)                            
                               ITEM   (COM-ITEM)                                
           END-EXEC.                                                            
           MOVE -1                  TO M-COGNL.                                 
           MOVE  CPS-MATR           TO M-MATRO.                                 
           MOVE  CPS-COGNOME        TO M-COGNO COM-COGNOME.                     
           MOVE  CPS-NOME           TO M-NOMEO.                                 
           MOVE  CPS-A-NASC         TO M-A-NASCO.                               
           MOVE  CPS-M-NASC         TO M-M-NASCO.                               
           MOVE  CPS-G-NASC         TO M-G-NASCO.                               
           MOVE  CPS-QU-IN          TO M-QU-INO.                                
           MOVE  CPS-COD-FISC       TO M-COD-FIO.                               
           MOVE  SPACES             TO M-MSG-1O                                 
                                       M-MSG-2O.                                
                                                                                
       2610-EX. EXIT.                                                           
      *                                                                 05450000
                                                                                
      *                                                                         
       1790-DELETEQ-TS SECTION.                                                 
      *-------------------------                                        05450000
           MOVE '1790-DELETEQ-TS' TO W-ULT-LABEL.                       05460000
                                                                        05470000
           EXEC CICS HANDLE CONDITION QIDERR (1790-EX) END-EXEC.                
           EXEC CICS DELETEQ TS QUEUE (CPSCODA)  END-EXEC.                      
                                                                                
       1790-EX. EXIT.                                                           
      *                                                                 05450000
                                                                                
      *                                                                         
       2000-CORPO-ELAB SECTION.                                                 
      *------------------------                                         05450000
           MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       05460000
                                                                        05470000
           IF W-NOME-PGM = COM-NOME-PGM                                 05490000
            THEN                                                        05500000
              PERFORM 2100-RECEIVE                                      05510000
            ELSE                                                        05540000
              PERFORM 2900-RIEMP-MASK                                   05550000
           END-IF.                                                      05560000
                                                                        05570000
       2000-EX. EXIT.                                                   05580000
      *                                                                 05590000
                                                                                
      *                                                                 05620000
       2100-RECEIVE SECTION.                                            05630000
      *---------------------                                            05640000
           MOVE '2100-RECEIVE' TO W-ULT-LABEL.                          05650000
      *                                                                 05660000
           PERFORM 2110-REC-MAPPA.                                      05670000
           PERFORM 2120-NORMALIZZA.                                     05680000
           PERFORM 2130-NORM-ATTR.                                      05690000
                                                                        05700000
       2100-EX. EXIT.                                                   05710000
      *                                                                 05750000
                                                                                
      *                                                                 05750000
       2110-REC-MAPPA SECTION.                                          05760000
      *--------------*                                                  05770000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        05780000
                                                                        05790000
           EXEC CICS RECEIVE MAP    ('MF09MAP')                         05800000
                             MAPSET ('MF09MAP') END-EXEC.               05810000
                                                                        05820000
       2110-EX. EXIT.                                                   05830000
      *                                                                 05840000
                                                                                
      *                                                                 05870000
       2120-NORMALIZZA SECTION.                                         05880000
      *---------------*                                                 05890000
           MOVE '2120-NORMALIZZA' TO W-ULT-LABEL.                       05900000
      *                                                                 05910000
           INSPECT M-COGNI REPLACING ALL LOW-VALUE BY ' '.                      
           INSPECT M-COGNI REPLACING ALL '_' BY ' '.                            
           MOVE    M-COGNI TO W-COGN.                                           
                                                                        06020000
       2120-EX. EXIT.                                                   06110000
      *                                                                 06120000
                                                                                
      *                                                                 06150000
       2130-NORM-ATTR SECTION.                                          06160000
      *--------------*                                                  06170000
           MOVE '2130-NORM-ATTR' TO W-ULT-LABEL.                        06180000
                                                                        06190000
           MOVE        FSET TO M-COGNA.                                         
       2130-EX. EXIT.                                                   06220000
      *                                                                 06230000
                                                                                
      *                                                                 06150000
       2131-PULISCI-MAPPA  SECTION.                                     06160000
      *--------------*                                                  06170000
           MOVE '2131-PULISCI-MAPPA' TO W-ULT-LABEL.                    06180000
                                                                        06190000
           MOVE  ALL '_'            TO M-MATRO.                                 
           MOVE  ALL '_'            TO M-NOMEO.                                 
           MOVE  ALL '_'            TO M-A-NASCO.                               
           MOVE  ALL '_'            TO M-M-NASCO.                               
           MOVE  ALL '_'            TO M-G-NASCO.                               
           MOVE  ALL '_'            TO M-QU-INO.                                
           MOVE  ALL '_'            TO M-COD-FIO.                               
           MOVE  ALL ' '            TO M-MSG-1O                                 
                                       M-MSG-2O.                                
       2131-EX. EXIT.                                                   06220000
      *                                                                 06230000
                                                                                
      *                                                                 09260000
       2900-RIEMP-MASK SECTION.                                         09270000
      *---------------*                                                 09280000
           MOVE '2900-RIEMP-MASK' TO W-ULT-LABEL.                       09290000
                                                                        09300000
           MOVE LOW-VALUE TO MF09MAPO.                                  09310000
           MOVE FSET      TO M-COGNA.                                           
           MOVE -1        TO M-COGNL.                                           
           MOVE ALL '_'   TO M-COGNO.                                           
           MOVE 0         TO COM-ITEM.                                          
           MOVE SPACES    TO COM-COGNOME.                                       
       2900-EX. EXIT.                                                   09370000
      *                                                                 09380000
                                                                                
      *                                                                         
       2998-DBERROR  SECTION.                                           09420000
      *----------------------                                           09430000
           MOVE W-ULT-LABEL TO W-ULT-LABEL-SQL.                         09440000
           MOVE W-TRS-ID   TO TRS-ID-SQL.                               09450000
           MOVE W-NOME-PGM TO NOME-PGM-SQL.                             09460000
           MOVE SQLCODE    TO SQL-CODICE.                               09470000
           MOVE ERR-SQL    TO M-MSG-1O.                                 09480000
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       09490000
           PERFORM 3000-FINE-ELAB.                                      09500000
                                                                        09510000
       2998-EX. EXIT.                                                   09520000
      *                                                                 09530000
                                                                                
      *                                                                 09560000
       2999-CERCA-ERR SECTION.                                          09570000
      *-----------------------                                          09580000
           MOVE '2999-CERCA-ERR' TO W-ULT-LABEL.                        09590000
      *                                                                 09600000
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
      *                                                                 09750000
                                                                                
      *                                                                 09760000
       3000-FINE-ELAB SECTION.                                          09770000
      *-----------------------                                          09780000
           MOVE '3000-FINE-ELAB' TO W-ULT-LABEL.                        09790000
                                                                        09800000
           IF W-CTL-END = 'LOOP'                                        09810000
             THEN                                                               
              PERFORM 3100-RIENTRO                                      09820000
             ELSE                                                               
              PERFORM 3200-PASSA-CTL                                    09840000
           END-IF.                                                      09850000
                                                                        09860000
       3000-EX. EXIT.                                                   09870000
      *                                                                 09880000
                                                                                
      *                                                                 09890000
       3100-RIENTRO SECTION.                                            09900000
      *---------------------                                            09910000
           MOVE '3100-RIENTRO' TO W-ULT-LABEL.                          09920000
      *                                                                 09930000
           MOVE COM-DATA-SISTEMA   TO M-DATA-SO.                        10070000
           PERFORM 3130-FORMATTA-MAPPA.                                 09950000
           PERFORM 3140-INVIO-MAPPA.                                    09960000
                                                                        09970000
       3100-EX. EXIT.                                                   09980000
      *                                                                 10150000
                                                                                
      *                                                                 10150000
       3130-FORMATTA-MAPPA SECTION.                                     10160000
      *-------------------*                                             10170000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   10180000
                                                                        10190000
              INSPECT M-COGNO REPLACING ALL LOW-VALUE BY '_'                    
              INSPECT M-COGNO REPLACING ALL SPACE BY '_'.                       
                                                                        10510000
       3130-EX. EXIT.                                                   10680000
      *                                                                 10690000
                                                                                
      *                                                                 10720000
       3140-INVIO-MAPPA SECTION.                                        10730000
      *----------------*                                                10740000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      10750000
                                                                        10760000
           IF W-NOME-PGM = COM-NOME-PGM                                 10770000
            THEN                                                        10780000
              EXEC CICS SEND                                            10790000
                        MAP    ('MF09MAP')                              10800000
                        MAPSET ('MF09MAP')                              10810000
                        CURSOR                                          10820000
                        DATAONLY
                        FREEKB
              END-EXEC                                                  10830000
            ELSE                                                        10840000
              MOVE W-NOME-PGM  TO COM-NOME-PGM                          10850000
              EXEC CICS SEND                                            10860000
                        MAP    ('MF09MAP')                              10870000
                        MAPSET ('MF09MAP')                              10880000
                        CURSOR                                          10890000
                        ERASE
                        FREEKB
              END-EXEC                                                  10900000
           END-IF.                                                      10910000
                                                                        10920000
           EXEC CICS RETURN                                             10930000
                     TRANSID  ('RR09')                                  10940000
                     COMMAREA (W-COMMAREA)                              10950000
                     LENGTH   (W-LEN)                                   10960000
                     END-EXEC.                                          10970000
                                                                        10980000
       3140-EX. EXIT.                                                   10990000
      *                                                                 11000000
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
