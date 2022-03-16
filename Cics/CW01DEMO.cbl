       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW01DEMO.                                            00020000
      ******************************************************************00030000
      * PROGETTO        : DEMO COMPUWARE DI RAPID RESPONSE             *00040000
      * ID. TRANSAZIONE : RR01                                         *00050000
      *----------------------------------------------------------------*00060000
      * OGGETTO         : LOGON APPLICAZIONE                           *00080000
      * TIPO OPERAZIONE : _____________                                *00090000
      *----------------------------------------------------------------*00110000
      * CREAZIONE       : 23/03/1999                                   *00120000
      * ULTIMA MODIFICA : 23/03/1999                                   *00130009
      ******************************************************************00140000
       ENVIRONMENT DIVISION.                                            00150000
       CONFIGURATION SECTION.                                           00160000
       DATA DIVISION.                                                   00170000
       WORKING-STORAGE SECTION.                                         00180000
      * -------------------------------------------------------------- *00190000
      *    DEFINIZIONE VARIABILI HOST PER ISTRUZIONI SQL               *00200000
      * -------------------------------------------------------------- *00210000
       01  W-SQLCODE                       PIC S9(03)  COMP VALUE +0.   00230005
           88  W-SQLCODE-OK          VALUE +0.                          00240005
           88  W-SQLCODE-NOT-FOUND   VALUE +100.                        00250005
       01  W-USER                          PIC X(8)   VALUE SPACES.     00450000
       01  W-PASSW                         PIC X(8)   VALUE SPACES.     00450000
       01  W-ACCESSO                       PIC X(10)  VALUE SPACES.     00450000
                                                                        00310000
      * -------------------------------------------------------------- *00330000
      *    DEFINIZIONE CAMPI STANDARD DELLA TRANSAZIONE                *00340000
      * -------------------------------------------------------------- *00350000
       01  W-COD-MSG-HOST                  PIC X(3).                    00220000
       01  W-CTL-END                       PIC X(4)   VALUE 'LOOP'.     00370000
       01  W-NOME-PGM                      PIC X(8)   VALUE 'CW01DEMO'. 00380000
       01  W-TRS-ID                        PIC X(4)   VALUE SPACE.      00400000
       01  W-XCTL-PGM                      PIC X(8)   VALUE SPACE.      00410000
       01  W-ULT-LABEL                     PIC X(15)  VALUE SPACES.     00420000
       01  W-LEN                           PIC S9(4)  COMP VALUE +250.  00430000
       01  W-TERMID                        PIC X(4)   VALUE SPACES.     00440000
                                                                        00500000
      * -------------------------------------------------------------- *00510000
      *    CAMPI PER ASKTIME E FORMATTIME                              *00520000
      * -------------------------------------------------------------- *00530000
       01  W-APPO-DATA.                                                 00540000
           05 W-APPO-GG             PIC XX.                             00550000
           05 W-APPO-MM             PIC XX.                             00550000
           05 W-APPO-AA             PIC XX.                             00550000
                                                                        00570000
       01  W-ABSTIME                    PIC S9(08) COMP.                00580000
       01  W-DATE                       PIC X(06).                      00590000
       01  W-CONV-DATA.                                                 00610000
           05 W-CONV-GG                 PIC XX.                         00620000
           05 FILLER                    PIC X    VALUE '/'.             00630000
           05 W-CONV-MM                 PIC XX.                         00640000
           05 FILLER                    PIC X    VALUE '/'.             00650000
           05 W-CONV-AAAA.                                              00660000
              10 W-COM-DATA-SE          PIC XX.                         00550000
              10 W-COM-DATA-AA          PIC XX.                         00560000
                                                                        00670000
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
      * -------------------------------------------------------------- *00680000
      *    DEFINIZIONE  MESSAGGIO ABEND CICS                           *00690000
      * -------------------------------------------------------------- *00700000
       01  APPOGGIO-CICS                   PIC X(79)  VALUE SPACES.     00710000
       01  ERR-CICS.                                                    00720000
           02  FILLER                      PIC X(12)                    00730000
                                           VALUE 'ERRORE CICS '.        00740000
           02  COD-ERR                     PIC X(4).                    00750000
           02  FILLER                      PIC X(12)                    00760000
                                           VALUE ' ALLA LABEL '.        00770000
           02  W-ULT-LABEL-CICS            PIC X(15).                   00780000
           02  FILLER                      PIC X(10)                    00790000
                                           VALUE ' TRANSID: '.          00800000
           02  TRS-ID-CICS                 PIC X(4).                    00810000
           02  FILLER                      PIC X(06)                    00820000
                                           VALUE ' PGM: '.              00830000
           02  NOME-PGM-CICS               PIC X(8).                    00840000
           02  FILLER                      PIC X(8).                    00850000
                                                                        00860000
                                                                        00870000
      * -------------------------------------------------------------- *00880000
      *    DEFINIZIONE SQLCA E TABELLE                                 *00930005
      * -------------------------------------------------------------- *00940005
           EXEC SQL  INCLUDE SQLCA     END-EXEC.                        00950005
                                                                        00980005
           EXEC SQL  INCLUDE CWUTEN  END-EXEC.                          00970005
                                                                        01010000
      * -------------------------------------------------------------- *01000000
      *    DEFINIZIONE DELLA MAPPA                                     *00890000
      * -------------------------------------------------------------- *00900000
           COPY MF01MAP.                                                00910000
                                                                        00920000
      * -------------------------------------------------------------- *00930000
      *    DEFINIZIONE DEGLI ATTRIBUTI E DEI TASTI FUNZIONALI          *00940000
      * -------------------------------------------------------------- *00950000
           COPY CWATTRIB.                                               00960000
           COPY DFHAID.                                                 00970000
                                                                        00980000
      * -------------------------------------------------------------- *00990000
      *    DEFINIZIONE DELLA TABELLA MESSAGGI                          *01000000
      * -------------------------------------------------------------- *01010000
           COPY CWMESS.                                                 01020000
                                                                        01170000
      * -------------------------------------------------------------- *01180000
      *                     DEFINIZIONE COMMAREA                       *01190000
      * -------------------------------------------------------------- *01200000
       01  W-COMMAREA.                                                  01210000
           COPY CWCOMMA.                                                01220000
                                                                        01260000
                                                                        01290000
       LINKAGE SECTION.                                                 01300000
       01  DFHCOMMAREA.                                                         
           05  FILLER  PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.           
      *                                                                         
       PROCEDURE DIVISION.                                              01330000
      *                                                                 01340000
       0000-MAIN SECTION.                                               01350000
      *----------------*                                                01360000
           PERFORM 1000-INIZIO-ELAB.                                    01370000
           PERFORM 2000-CORPO-ELAB.                                     01380000
           PERFORM 3000-FINE-ELAB.                                      01390000
           GOBACK.                                                      01400000
                                                                        01410000
                                                                        01440000
       1000-INIZIO-ELAB SECTION.                                        01450000
      *----------------*                                                01460000
           MOVE EIBTRNID        TO W-TRS-ID.                            01470000
           MOVE DFHCOMMAREA     TO W-COMMAREA.                          02510000
           PERFORM 1100-COND-ANOMAL.                                    01480000
           PERFORM 1200-TASTI-FUNZ.                                     01490000
                                                                        01510000
       1000-EX. EXIT.                                                   01520000
                                                                        01530000
                                                                        01560000
      * -------------------------------------------------------------- *01180000
      * INTERCETTAZIONE DI CONDIZIONI DI ANOMALIA                      *01190000
      * -------------------------------------------------------------- *01200000
                                                                                
       1100-COND-ANOMAL SECTION.                                        01570000
      *---------------*                                                 01580000
           EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        01590000
                                                           END-EXEC.    01600000
           EXEC CICS HANDLE CONDITION  MAPFAIL (1120-COND-MFAIL)        01610000
                                                           END-EXEC.    01620000
           EXEC SQL WHENEVER SQLERROR GO TO 2998-DBERROR   END-EXEC.    03250000
       1100-EX. EXIT.                                                   01630000
                                                                        01640000
       1110-ABEND-CICS SECTION.                                         01680000
      *---------------*                                                 01690000
           MOVE W-ULT-LABEL  TO W-ULT-LABEL-CICS.                       01700000
           MOVE W-TRS-ID     TO TRS-ID-CICS.                            01710000
           MOVE W-NOME-PGM   TO NOME-PGM-CICS.                          01720000
           EXEC CICS ASSIGN ABCODE (COD-ERR) END-EXEC.                  01730000
           EXEC CICS HANDLE ABEND CANCEL    END-EXEC.                   01740000
           EXEC CICS SYNCPOINT ROLLBACK     END-EXEC.                   01750000
           MOVE ERR-CICS     TO M-MSG-1O.                               01760000
           PERFORM 3000-FINE-ELAB.                                      01770000
                                                                        01780000
       1110-EX. EXIT.                                                   01790000
                                                                        01820000
       1120-COND-MFAIL SECTION.                                         01830000
      *---------------*                                                 01840000
           MOVE '** ERRORE DI MPFAIL **' TO ERR-CICS.                   02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
       1120-EX. EXIT.                                                   01900000
                                                                        01910000
                                                                        01920000
      * -------------------------------------------------------------- *01180000
      * GESTIONE TASTI FUNZIONE                                        *01190000
      * -------------------------------------------------------------- *01200000
                                                                                
       1200-TASTI-FUNZ SECTION.                                         01930000
      *---------------*                                                 01940000
           MOVE '1200-TASTI-FUNZ' TO W-ULT-LABEL.                       01950000
                                                                        01960000
           EXEC CICS HANDLE AID CLEAR  (1210-TASTO-PA1)                 01970000
                                PA1    (1210-TASTO-PA1)                 01990000
                                ANYKEY (1250-TASTO-ANYK)                01990000
           END-EXEC.                                                    02000000
                                                                        02010000
                                                                                
       1200-EX. EXIT.                                                   02020000
                                                                        02030000
                                                                        02290000
       1210-TASTO-PA1 SECTION.                                          02300000
      *---------------*                                                 02310000
           MOVE '** RITORNO AL CICS **' TO ERR-CICS.                    02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
                                                                        02110000
       1210-EX. EXIT.                                                   02350000
                                                                        02290000
       1250-TASTO-ANYK SECTION.                                         02300000
      *---------------*                                                 02310000
           MOVE '006'               TO W-COD-MSG-HOST.                  02320000
           MOVE  W-COD-MSG-HOST     TO M-MSG-1O.                        02320000
           PERFORM 2999-CERCA-ERR.                                      02330000
                                                                        02340000
       1250-EX. EXIT.                                                   02350000
                                                                        02710000
       2000-CORPO-ELAB SECTION.                                         02720000
      *---------------*                                                 02730000
           MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       02740000
      * ---                                                             02750000
                                                                        02760000
           IF W-NOME-PGM = COM-NOME-PGM                                 02770000
            THEN                                                        02780000
              PERFORM 2100-RECEIVE                                      02790000
              PERFORM 2200-CONTROLLI                                    02800000
              PERFORM 2700-PASSA-CTL                                    02810000
            ELSE                                                                
              PERFORM 2900-RIEMPI-MASK                                  02810000
           END-IF.                                                      02840000
                                                                        02850000
       2000-EX. EXIT.                                                   02860000
                                                                        02870000
                                                                        02900000
       2100-RECEIVE SECTION.                                            02910000
      *------------*                                                    02920000
           MOVE '2100-RECEIVE' TO W-ULT-LABEL.                          02930000
      * ---                                                             02940000
           PERFORM 2110-REC-MAPPA.                                      02950000
           PERFORM 2120-NORMALIZZA.                                     02960000
                                                                        02970000
       2100-EX. EXIT.                                                   02980000
      *                                                                 02990000
                                                                        03020000
       2110-REC-MAPPA SECTION.                                          03030000
      *--------------*                                                  03040000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        03050000
      *                                                                 03060000
           EXEC CICS RECEIVE MAP    ('MF01MAP')                         03070000
                             MAPSET ('MF01MAP')                                 
           END-EXEC.                                                            
                                                                        03090000
       2110-EX. EXIT.                                                   03100000
                                                                        03140000
      * -------------------------------------------------------------- *01180000
      * PULIZIA DEI CAMPI DI MAPPA DIGITABILI                          *01190000
      * -------------------------------------------------------------- *01200000
                                                                                
       2120-NORMALIZZA SECTION.                                         03150000
      *---------------*                                                 03160000
           MOVE '2120-NORMALIZZA' TO W-ULT-LABEL.                       03170000
      * ---                                                             03180000
           INSPECT M-USERI REPLACING ALL LOW-VALUE BY ' '.              03190000
           INSPECT M-USERI REPLACING ALL '_'       BY ' '.              03200000
           INSPECT M-PASSWI REPLACING ALL LOW-VALUE BY ' '.             03190000
           INSPECT M-PASSWI REPLACING ALL '_'       BY ' '.             03200000
                                                                        03210000
       2120-EX. EXIT.                                                   03220000
      *                                                                 03250000
                                                                        03260000
      * -------------------------------------------------------------- *01180000
      * CONTROLLI SUI CAMPI DIGITATI IN MAPPA                          *01190000
      * -------------------------------------------------------------- *01200000
                                                                                
       2200-CONTROLLI SECTION.                                          03270000
      *--------------*                                                  03280000
           MOVE '2200-CONTROLLI' TO W-ULT-LABEL.                        03290000
      * ---                                                             03300000
           MOVE FSET             TO M-USERA.                            03310000
           MOVE UNPROT-DRK-FSET  TO M-PASSWA.                           03310000
           IF M-USERI = SPACES AND M-PASSWI = SPACES                    03460000
             THEN                                                       03330000
                 MOVE '001'         TO W-COD-MSG-HOST                   03340000
                 MOVE -1            TO M-USERL                          03350000
                 MOVE FSET-BRT      TO M-USERA                          03360000
           END-IF.                                                      03380000
      *                                                                         
           IF  (M-USERI = SPACES)  AND                                  03460000
               (M-PASSWI NOT EQUAL SPACES)                              03460000
             THEN                                                       03330000
                 MOVE '003'         TO W-COD-MSG-HOST                   03340000
                 MOVE -1            TO M-USERL                          03350000
           END-IF.                                                      03380000
                                                                                
      *                                                                         
           IF  (M-USERI NOT EQUAL SPACES)  AND                          03460000
               (M-PASSWI = SPACES)                                      03460000
             THEN                                                       03330000
                 MOVE '002'         TO W-COD-MSG-HOST                   03340000
                 MOVE -1            TO M-PASSWL                         03350000
           END-IF.                                                      03380000
                                                                        03660000
           IF  (M-USERI NOT = SPACES)  AND                              03460000
                (M-PASSWI NOT = SPACES)                                 03460000
             THEN MOVE SPACES TO W-COD-MSG-HOST                         03330000
           END-IF.                                                      03380000
      *                                                                         
           IF W-COD-MSG-HOST NOT = SPACES                                       
             THEN                                                               
              PERFORM 2999-CERCA-ERR                                    03370000
           ELSE                                                                 
              PERFORM 2205-CERCA-UTENTE                                         
           END-IF.                                                      03380000
      *                                                                         
       2200-EX. EXIT.                                                   03760000
                                                                        03740000
                                                                        03750000
       2205-CERCA-UTENTE SECTION.                                               
           MOVE '2205-CERCA-UTENTE' TO W-ULT-LABEL.                     03290000
      * ---                                                             03300000
           MOVE M-USERI          TO W-USER.                             03310000
           MOVE M-PASSWI         TO W-PASSW.                            03310000
           EXEC SQL SELECT UTENTE, PASSW,                               04220005
                           TIPO_ACCESSO                                 04220005
                    INTO  :UTENTE,                                      04230005
                          :PASSW,                                       04230005
                          :TIPO-ACCESSO                                 04230005
                    FROM  CPS04.CWUTENTI                                04240005
                    WHERE UTENTE = :W-USER                              04250006
           END-EXEC.                                                    04260005
           MOVE SQLCODE TO W-SQLCODE.                                   04280005
           IF W-SQLCODE-OK THEN                                         04290005
                IF W-PASSW NOT EQUAL TO PASSW OF DCL-CPSUTE                     
                   MOVE '031'         TO W-COD-MSG-HOST                 03340000
                   MOVE -1            TO M-PASSWL                       03350000
                   PERFORM 2999-CERCA-ERR                               03370000
                 END-IF                                                         
              MOVE TIPO-ACCESSO OF DCL-CPSUTE TO W-ACCESSO                      
              PERFORM 2206-IMPOSTA-UTENTE                               04300005
           ELSE                                                                 
              IF W-USER NOT EQUAL TO UTENTE OF DCL-CPSUTE                       
                 MOVE '004'         TO W-COD-MSG-HOST                   03340000
                 MOVE -1            TO M-USERL                          03350000
                 MOVE FSET-BRT      TO M-USERA                          03360000
                 PERFORM 2999-CERCA-ERR                                 03370000
               END-IF                                                           
           END-IF.                                                              
       2205-EX. EXIT.                                                           
                                                                                
      *                                                                         
                                                                                
       2206-IMPOSTA-UTENTE SECTION.                                             
      *---------------------------                                              
           MOVE '2206-IMPOSTA-UTENTE' TO W-ULT-LABEL.                   03290000
      *                                                                 03300000
                                                                                
              IF W-ACCESSO = 'OPERATORE1'                                       
                THEN MOVE '1' TO COM-TIPO-UTENTE.                               
              IF W-ACCESSO = 'OPERATORE2'                                       
                THEN MOVE '2' TO COM-TIPO-UTENTE.                               
              IF W-ACCESSO = 'OPERATORE3'                                       
                THEN MOVE '3' TO COM-TIPO-UTENTE.                               
              IF W-ACCESSO = 'OPERATORE4'                                       
                THEN MOVE '4' TO COM-TIPO-UTENTE.                               
                                                                                
       2206-EX. EXIT.                                                           
      *                                                                 03790000
                                                                        04430005
       2700-PASSA-CTL SECTION.                                          03990000
      *------------------------                                         04000000
           MOVE '2700-PASSA-CTL'          TO W-ULT-LABEL.               04010000
                                                                        04020000
           MOVE 'END'                     TO W-CTL-END.                 04030000
                                                                        04040000
           MOVE 'CW02DEMO'                 TO W-XCTL-PGM.               05110002
                                                                        05120000
       2700-EX. EXIT.                                                   04520000
      *                                                                 04430005
                                                                                
      *-------------------------------------------------------------- * 01180000
      *VALORIZZAZIONE DELLA MAPPA IN PRIMA VOLTA                      * 01190000
      *-------------------------------------------------------------- * 01200000
                                                                                
       2900-RIEMPI-MASK  SECTION.                                       03990000
      *------------------------                                         04000000
                                                                                
           MOVE '2900-RIEMPI-MASK'        TO W-ULT-LABEL.               04010000
           MOVE LOW-VALUE     TO  MF01MAPO.                             04000000
           MOVE -1            TO  M-USERL.                              04000000
       2900-EX. EXIT.                                                   04520000
      *                                                                 04530000
                                                                        04600000
      *-------------------------------------------------------------- * 01180000
      *GESTIONE ERRORI DB2                                            * 01190000
      *-------------------------------------------------------------- * 01200000
                                                                                
       2998-DBERROR  SECTION.                                           09420000
      *------------*                                                    09430000
           EXEC CICS DUMP TRANSACTION DUMPCODE('M818')                          
                                         END-EXEC                               
           MOVE W-ULT-LABEL TO W-ULT-LABEL-SQL.                         09440000
           MOVE W-TRS-ID   TO TRS-ID-SQL.                               09450000
           MOVE W-NOME-PGM TO NOME-PGM-SQL.                             09460000
           MOVE SQLCODE    TO SQL-CODICE.                               09470000
           MOVE ERR-SQL    TO M-MSG-1O.                                 09480000
           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       09490000
           PERFORM 3000-FINE-ELAB.                                      09500000
                                                                        09510000
       2998-EX. EXIT.                                                   09520000
                                                                        04680000
       2999-CERCA-ERR SECTION.                                          04690000
      *--------------*                                                  04700000
           MOVE '2999-CERCA-ERR' TO W-ULT-LABEL.                        04710000
      *---                                                              04720000
           SET IND-TAB TO 1.                                            04730000
                                                                        04740000
           SEARCH ELEM-TAB-MSG AT END                                   04750000
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'            04760000
                    TO M-MSG-1O                                         04770000
                  WHEN W-COD-MSG-HOST = ELEM-COD-MSG (IND-TAB)          04780000
                       MOVE ELEM-DESC-MSG( IND-TAB)                     04790000
                         TO M-MSG-1O                                    04800000
           END-SEARCH.                                                  04810000
           PERFORM 3000-FINE-ELAB.                                      04820000
       2999-EX. EXIT.                                                   04830000
                                                                        04870000
       3000-FINE-ELAB SECTION.                                          04880000
      * -------------*                                                  04890000
           MOVE '3000-FINE-ELAB' TO W-ULT-LABEL.                        04900000
      * ---                                                             04910000
           MOVE SPACES TO W-COD-MSG-HOST.                                       
           IF W-CTL-END = 'LOOP'                                        04920000
             THEN                                                       04930000
              PERFORM 3100-RIENTRO                                      04940000
             ELSE                                                       04950000
              PERFORM 3200-PASSA-CTL                                    04960000
           END-IF.                                                      04970000
                                                                        04980000
       3000-EX. EXIT.                                                   04990000
                                                                        05020000
                                                                        05030000
       3100-RIENTRO SECTION.                                            05040000
      *------------*                                                    05050000
           MOVE '3100-RIENTRO' TO W-ULT-LABEL.                          05060000
      *---                                                              05070000
           PERFORM 3110-DATA-ORA.                                       05080000
           PERFORM 3130-FORMATTA-MAPPA.                                 05090000
           PERFORM 3140-INVIO-MAPPA.                                    05100000
                                                                        05110000
       3100-EX. EXIT.                                                   05120000
                                                                        05130000
                                                                        05500000
       3110-DATA-ORA SECTION.                                           04570000
                                                                        04580000
           MOVE '3110-DATA-ORA' TO W-ULT-LABEL.                         04590000
      *                                                                 04600000
           EXEC CICS ASKTIME    ABSTIME(W-ABSTIME)                      05210000
           END-EXEC.                                                    05220000
           EXEC CICS FORMATTIME ABSTIME (W-ABSTIME)                     05230000
                                DDMMYY   (W-DATE)                       05240000
           END-EXEC.                                                    05260000
                                                                        05270000
           MOVE W-DATE            TO W-APPO-DATA.                       05280000
                                                                        05290000
           IF W-APPO-AA < '93'                                          05320000
             THEN                                                       05330000
                 MOVE '20'        TO W-COM-DATA-SE                      05340000
           ELSE                                                         05350000
                 MOVE '19'        TO W-COM-DATA-SE                      05360000
           END-IF.                                                      05370000
           MOVE W-APPO-GG         TO W-CONV-GG.                         05380000
           MOVE W-APPO-MM         TO W-CONV-MM.                         05380000
           MOVE W-APPO-AA         TO W-COM-DATA-AA.                     05380000
           MOVE W-CONV-DATA       TO COM-DATA-SISTEMA.                  05380000
           MOVE COM-DATA-SISTEMA  TO M-DATA-SO.                                 
      *                                                                 04600000
       3110-EX. EXIT.                                                   05390000
      *                                                                 04600000
       3130-FORMATTA-MAPPA SECTION.                                     05510000
      *-------------------*                                             05520000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   05530000
      *---                                                              05540000
           INSPECT M-USERO REPLACING ALL ' '       BY '_'.              05550000
           INSPECT M-USERO REPLACING ALL LOW-VALUE BY '_'.              05560000
           IF COM-MESSAGGIO EQUAL SPACES                                05570000
              NEXT SENTENCE                                             05580000
           ELSE                                                         05590000
              MOVE COM-MESSAGGIO TO M-MSG-1O                            05600000
              MOVE SPACES        TO COM-MESSAGGIO                       05610000
           END-IF.                                                      05620000
                                                                        05630000
       3130-EX. EXIT.                                                   05640000
                                                                        05650000
                                                                        05680000
       3140-INVIO-MAPPA SECTION.                                        05690000
      *----------------*                                                05700000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      05710000
      *---                                                              05720000
           IF W-NOME-PGM = COM-NOME-PGM                                 05730000
            THEN                                                        05740000
              EXEC CICS SEND                                            05750000
                        MAP    ('MF01MAP')                              05760000
                        MAPSET ('MF01MAP')                              05770000
                        DATAONLY                                        05780000
                        CURSOR 
                        FREEKB                                          05790000
              END-EXEC                                                  05800000
           ELSE                                                         05810000
              MOVE W-NOME-PGM         TO COM-NOME-PGM                   05820000
              EXEC CICS SEND                                            05830000
                        MAP    ('MF01MAP')                              05840000
                        MAPSET ('MF01MAP')                              05850000
                        ERASE                                           05860000
                        CURSOR
                        FREEKB                                          05870000
             END-EXEC                                                   05880000
           END-IF.                                                      05890000
                                                                        05900000
           EXEC CICS RETURN                                             05910000
                        TRANSID  ('RR01')                               05920000
                        COMMAREA (W-COMMAREA)                           05930000
                        LENGTH   (W-LEN)                                05940000
           END-EXEC.                                                    05950000
                                                                        05960000
       3140-EX. EXIT.                                                   05970000
                                                                        05980000
                                                                        05990000
                                                                        06000000
       3200-PASSA-CTL SECTION.                                          06010000
      *--------------*                                                  06020000
           MOVE '3200-PASSA-CTL' TO W-ULT-LABEL.                        06030000
      *---                                                              06040000
           MOVE SPACES     TO        COM-MESSAGGIO.                     06050000
                                                                        06060000
           EXEC CICS XCTL                                               06070000
                        PROGRAM  (W-XCTL-PGM)                           06080000
                        COMMAREA (W-COMMAREA)                           06090000
                        LENGTH   (W-LEN)                                06100000
           END-EXEC.                                                    06110000
                                                                        06120000
       3200-EX. EXIT.                                                   06130000
/*                                                                      06130000
