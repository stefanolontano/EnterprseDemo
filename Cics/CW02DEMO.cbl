       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW02DEMO.                                            00020000
      ******************************************************************00040000
      * PROGETTO        : CENTRO PRODUZIONE SOFTWARE - I&T              00050000
      * ID. TRANSAZIONE : CW02DEMO                                     *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE : 1 - GESTIONE DIPENDENTI                      *00080000
      * OGGETTO         : MENU GENERALE*                                00090003
      * TIPO OPERAZIONE : _____________                                *00100000
      * RIFERIMENTO P.E.: 1.0                                          *00110000
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 04/03/1999                                   *00130002
      * ULTIMA MODIFICA : 04/03/1999                                   *00140002
      ******************************************************************00150000
      * ---                                                             00160000
       ENVIRONMENT DIVISION.                                            00170000
       CONFIGURATION SECTION.                                           00180000
       DATA DIVISION.                                                   00190000
       WORKING-STORAGE SECTION.                                         00200000
                                                                                
                                                                                
      *       DEFINIZIONE VARIABILI HOST                               *00220000
      * -------------------------------------------------------------- *00230000
      *----------------------------------------------------------------*00340000
                                                                        01641016
       01  W-SCELTA                        PIC X(1)   VALUE SPACES.     01650016
           88 W-SCELTA-1        VALUE '1'.                              01660016
           88 W-SCELTA-2        VALUE '2'.                              01661016
           88 W-SCELTA-3        VALUE '3'.                              01662016
                                                                        01664016
      * -------------------------------------------------------------- *01710000
      *    DEFINIZIONE CAMPI STANDARD DELLA TRANSAZIONE                *01720000
      * -------------------------------------------------------------- *01730000
       01  W-MSG-HOST                      PIC X(3)   VALUE SPACES.     01740000
       01  W-CTL-END                       PIC X(4)   VALUE 'LOOP'.     01750000
       01  W-NOME-PGM                      PIC X(8)   VALUE 'CW02DEMO'. 01760000
       01  W-NOME-MAP                      PIC X(8)   VALUE 'MF02MAP'.  01770000
       01  W-NOME-MAPSET                   PIC X(8)   VALUE 'MF02MAP'.  01770000
       01  W-TRS-ID                        PIC X(4)   VALUE SPACE.      01780000
       01  W-XCTL-PGM                      PIC X(8)   VALUE SPACE.      01790000
       01  W-ULT-LABEL                     PIC X(15)  VALUE SPACES.     01800000
       01  W-LEN                           PIC S9(3)  COMP VALUE +250.  01810000
       01  W-TERMID                        PIC X(4)   VALUE SPACES.     01820000
       01  W-PGM-LOGON                     PIC X(8)   VALUE 'CW01DEMO'. 01820000
       01  W-PGM-MENU-GEN                  PIC X(8)   VALUE 'CW02DEMO'. 01820000
       01  W-PGM-MENU-RAMO                 PIC X(8)   VALUE 'CW03DEMO'. 01820000
                                                                        01840000
                                                                        01850000
      * -------------------------------------------------------------- *01860000
      *    DEFINIZIONE  CAMPI  PER DATA E ORA                          *01870000
      * -------------------------------------------------------------- *01880000
       01  COMODO-TIME                     PIC 9(6).                    01890000
                                                                        02010000
       01  DATA-CUR.                                                    02020000
           02  GG-CUR                      PIC XX.                      02030000
           02  FILLER                      PIC X.                       02040000
           02  MM-CUR                      PIC XX.                      02050000
           02  FILLER                      PIC X.                       02060000
           02  AA1-CUR.                                                 02070000
               03 AA19                     PIC XX VALUE '19'.           02080000
               03 AA                       PIC XX.                      02090000
       01  W-DATA.                                                      02100000
           05  FILLER                  PIC XX.                          02110000
           05  W-AA                    PIC XX .                         02120000
           05  W-MM                    PIC XX .                         02130000
           05  W-GG                    PIC XX .                         02140000
                                                                        02150000
                                                                        02200000
      * -------------------------------------------------------------- *02960000
      *    DEFINIZIONE  MESSAGGIO ABEND CICS                           *02970000
      * -------------------------------------------------------------- *02980000
       01  APPOGGIO-CICS                   PIC X(79)  VALUE SPACES.     02990000
       01  ERR-CICS.                                                    03000000
           02  FILLER                      PIC X(12)                    03010000
                                           VALUE 'ERRORE CICS '.        03020000
           02  COD-ERR                     PIC X(4).                    03030000
           02  FILLER                      PIC X(12)                    03040000
                                           VALUE ' ALLA LABEL '.        03050000
           02  ULT-LABEL-CICS              PIC X(15).                   03060000
           02  FILLER                      PIC X(10)                    03070000
                                           VALUE ' TRANSID: '.          03080000
           02  TRS-ID-CICS                 PIC X(4).                    03090000
           02  FILLER                      PIC X(06)                    03100000
                                           VALUE ' PGM: '.              03110000
           02  NOME-PGM-CICS               PIC X(8).                    03120000
           02  FILLER                      PIC X(8).                    03130000
                                                                        03140000
                                                                        03150000
                                                                        03160000
      * -------------------------------------------------------------- *03170000
      *    DEFINIZIONE DELLA MAPPA                                     *03180000
      * -------------------------------------------------------------- *03190000
           COPY MF02MAP.                                                03200000
                                                                        03210000
      * -------------------------------------------------------------- *03220000
      *    DEFINIZIONE DEGLI ATTRIBUTI                                 *03230000
      * -------------------------------------------------------------- *03240000
           COPY CWATTRIB.                                               03250000
           COPY DFHAID.                                                 03260000
                                                                        03270000
      * -------------------------------------------------------------- *03280000
      *    DEFINIZIONE DELLA TABELLA MESSAGGI                          *03290000
      * -------------------------------------------------------------- *03300000
           COPY CWMESS.                                                 03310000
                                                                        03320000
                                                                        03330000
      * -------------------------------------------------------------- *03410000
      *                     DEFINIZIONE COMMAREA                       *03420000
      * -------------------------------------------------------------- *03430000
       01  W-COMMAREA.                                                  03440000
           COPY CWCOMMA.                                                03450000
                                                                        03460000
                                                                        03470000
                                                                        03480000
       LINKAGE SECTION.                                                 03500000
                                                                                
CICS   01  DFHCOMMAREA.                                                         
CICS       05  FILLER  PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.           
      *                                                                 03510000
                                                                        03530000
       PROCEDURE DIVISION.                                              03540000
                                                                                
       0000-MAIN SECTION.                                               03550000
      *---------*                                                       03560000
           PERFORM 1000-INIZIO-ELAB.                                    03570000
           PERFORM 2000-CORPO-ELAB.                                     03580000
           PERFORM 3000-FINE-ELAB.                                      03590000
           GOBACK.                                                      03600000
       EX-0000-MAIN SECTION.                                            03610000
                                                                        03620000
                                                                        03630000
                                                                        03640000
       1000-INIZIO-ELAB SECTION.                                        03650000
      *----------------*                                                03660000
           MOVE EIBTRNID             TO W-TRS-ID.                       03670000
           PERFORM 1100-COND-ANOMAL.                                    03680000
           PERFORM 1200-TASTI-FUNZ.                                     03690000
           PERFORM 1300-TESTA-RIEN.                                     03710000
       1000-EX. EXIT.                                                   03720000
                                                                        03730000
                                                                        03740000
                                                                        03750000
                                                                        03760000
       1100-COND-ANOMAL SECTION.                                        03770000
      *---------------*                                                 03780000
           EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        03790000
                                                           END-EXEC.    03800000
           EXEC CICS HANDLE CONDITION  MAPFAIL (1120-COND-MFAIL)        03810000
                                                           END-EXEC.    03820000
       1100-EX. EXIT.                                                   03830000
                                                                        03840000
                                                                        03850000
       1110-ABEND-CICS SECTION.                                         03880000
      *---------------*                                                 03890000
           MOVE W-ULT-LABEL    TO ULT-LABEL-CICS.                       03900000
           MOVE W-TRS-ID     TO TRS-ID-CICS.                            03910000
           MOVE W-NOME-PGM   TO NOME-PGM-CICS.                          03920000
           EXEC CICS ASSIGN ABCODE (COD-ERR) END-EXEC.                  03930000
           EXEC CICS HANDLE ABEND CANCEL    END-EXEC.                   03940000
           EXEC CICS SYNCPOINT ROLLBACK     END-EXEC.                   03950000
           MOVE ERR-CICS     TO W-MSG-HOST.                             03960000
           MOVE W-MSG-HOST TO M-MSG-2O.                                 03960000
           MOVE PROT-BRT     TO M-MSG-2A M-MSG-1A.                      03960000
           MOVE SPACES       TO M-MSG-1O.                               03960000
           PERFORM 2999-CERCA-ERR.                                      03970000
           PERFORM 3000-FINE-ELAB.                                      03970000
                                                                        03980000
       1110-EX. EXIT.                                                   03990000
                                                                        04000000
                                                                        04010000
                                                                        04020000
       1120-COND-MFAIL SECTION.                                         04050000
      *---------------*                                                 04060000
           MOVE '1120-COND-MFAIL' TO W-ULT-LABEL.                       04160000
      *---                                                              04170000
           MOVE '** ERRORE DI MPFAIL **' TO ERR-CICS.                   02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
      *                                                                         
       1120-EX. EXIT.                                                   04090000
                                                                        04100000
                                                                        04110000
                                                                        04120000
                                                                        04130000
       1200-TASTI-FUNZ SECTION.                                         04140000
      *---------------*                                                 04150000
           MOVE '1200-TASTI-FUNZ' TO W-ULT-LABEL.                       04160000
      *---                                                              04170000
           EXEC CICS HANDLE AID CLEAR  (1220-TASTO-CLEAR)               04180000
                                ENTER  (1239-TASTO-ENTER)               04151002
                                PF1    (1244-TASTO-HELP)                        
                                PF3    (1230-TASTO-PF3)                         
                                PF4    (1240-TASTO-PF4)                         
                                PA1    (1210-TASTO-PA1)                         
                                ANYKEY (1250-TASTO-ANYK)                04200000
           END-EXEC.                                                    04210000
                                                                        04220000
       1200-EX. EXIT.                                                   04230000
                                                                        04240000
                                                                        04400000
       1220-TASTO-CLEAR SECTION.                                        04410000
      *--------------*                                                  04420000
           MOVE '1220-TASTO-CLEAR' TO W-ULT-LABEL.                      04410000
      *--------------*                                                  04420000
           MOVE SPACES       TO       COM-MESSAGGIO.                    04430000
           MOVE +250         TO       W-LEN.                                    
           MOVE W-PGM-LOGON  TO       W-XCTL-PGM.                               
           PERFORM 3200-XCTL.                                           04460000
      *                                                                         
       1220-EX. EXIT.                                                   04470000
      *                                                                 04480000
       1239-TASTO-ENTER  SECTION.                                       04490000
      *--------------*                                                  04420000
           MOVE '1239-TASTO-ENTER' TO W-ULT-LABEL.                      04410000
      *--------------*                                                  04420000
           INSPECT M-SCELTAI REPLACING ALL '_' BY SPACE.                        
           INSPECT M-SCELTAI REPLACING ALL LOW-VALUES BY SPACE.                 
           MOVE M-SCELTAI  TO W-SCELTA.                                 04480000
           PERFORM 2200-CONTROLLI.                                              
      *                                                                         
       1239-EX. EXIT.                                                   04490000
      *                                                                         
       1244-TASTO-HELP  SECTION.                                        04520000
      *---------------*                                                 04530000
           MOVE SPACES              TO M-MSG-1O.                        04540000
           MOVE '007'               TO M-MSG-2O.                        04540000
           MOVE PROT-BRT            TO M-MSG-2A M-MSG-1A.               04540000
           MOVE -1                  TO M-SCELTAL.                       04540000
           PERFORM 2999-CERCA-ERR.                                      04550000
           PERFORM 3000-FINE-ELAB.                                      04560000
      *                                                                         
       1244-EX. EXIT.                                                   04570000
      *                                                                 04500000
       1210-TASTO-PA1  SECTION.                                         04490000
      *--------------*                                                  04420000
           MOVE '1210-TASTO-PA1' TO W-ULT-LABEL.                        04410000
      *--------------*                                                  04420000
           MOVE '** RITORNO AL CICS  **' TO ERR-CICS.                   02620000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              02630000
                          ERASE WAIT END-EXEC.                          02640000
           EXEC CICS RETURN END-EXEC.                                   02650000
      *                                                                 04510000
       1210-EX. EXIT.                                                   04570000
      *                                                                 04500000
       1230-TASTO-PF3  SECTION.                                         04490000
      *--------------*                                                  04420000
           MOVE '1230-TASTO-PF3' TO W-ULT-LABEL.                        04410000
      *--------------*                                                  04420000
           PERFORM 1220-TASTO-CLEAR.                                    04560000
      *                                                                 04510000
       1230-EX. EXIT.                                                   04570000
      *                                                                         
       1240-TASTO-PF4  SECTION.                                         04490000
      *--------------*                                                  04420000
           MOVE '1240-TASTO-PF4' TO W-ULT-LABEL.                        04410000
      *--------------*                                                  04420000
           PERFORM 1220-TASTO-CLEAR.                                    04560000
      *                                                                 04510000
       1240-EX. EXIT.                                                   04570000
      *                                                                         
       1250-TASTO-ANYK SECTION.                                         04520000
      *---------------*                                                 04530000
           MOVE SPACES              TO M-MSG-1O.                        04540000
           MOVE '006'               TO M-MSG-2O.                        04540000
           MOVE PROT-BRT            TO M-MSG-2A M-MSG-1A.               04540000
           MOVE -1                  TO M-SCELTAL.                       04540000
           PERFORM 2999-CERCA-ERR.                                      04550000
           PERFORM 3000-FINE-ELAB.                                      04560000
       1250-EX. EXIT.                                                   04570000
                                                                        04580000
                                                                        04590000
                                                                        04600000
                                                                        04610000
       1300-TESTA-RIEN SECTION.                                         04620000
      *---------------*                                                 04630000
           IF EIBCALEN = ZERO                                           04640000
            THEN                                                        04650000
              PERFORM 1310-TRANS-DIS                                    04660000
           END-IF.                                                      04670000
                                                                        04680000
           MOVE DFHCOMMAREA TO W-COMMAREA.                              04690000
                                                                        04700000
       1300-EX. EXIT.                                                   04710000
                                                                        04720000
                                                                        04730000
                                                                        04740000
                                                                        04750000
       1310-TRANS-DIS SECTION.                                          04760000
      *--------------*                                                  04770000
           MOVE '1310-TRANS-DIS' TO W-ULT-LABEL.                        04900000
      * ---                                                             04910000
           MOVE '*** TRANSAZIONE NON PERMESSA ***' TO ERR-CICS.         04780000
           EXEC CICS SEND TEXT FROM (ERR-CICS) LENGTH (78)              04790000
                          ERASE WAIT END-EXEC.                          04800000
           EXEC CICS RETURN END-EXEC.                                   04810000
                                                                        04820000
       1310-EX. EXIT.                                                   04830000
                                                                        04840000
                                                                        04850000
                                                                        04860000
                                                                        04870000
       2000-CORPO-ELAB SECTION.                                         04880000
      *---------------*                                                 04890000
           MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       04900000
      * ---                                                             04910000
                                                                        04920000
           IF W-NOME-PGM = COM-NOME-PGM                                 04930000
            THEN                                                        04940000
              PERFORM 2100-RECEIVE                                      04950000
              PERFORM 1239-TASTO-ENTER                                          
      *       PERFORM 2700-PASSA-CTL                                    04970000
            ELSE                                                        04980000
              PERFORM 2900-RIEMP-MASK                                   04990000
           END-IF.                                                      05000000
                                                                        05010000
       2000-EX. EXIT.                                                   05020000
                                                                        05030000
                                                                        05040000
       2100-RECEIVE SECTION.                                            05050000
      *------------*                                                    05060000
           MOVE '2100-RECEIVE' TO W-ULT-LABEL.                          05070000
      * ---                                                             05080000
           PERFORM 2110-REC-MAPPA.                                      05090000
           PERFORM 2120-NORMALIZZA.                                     05100000
                                                                        05110000
       2100-EX. EXIT.                                                   05120000
                                                                        05130000
                                                                        05140000
                                                                        05150000
                                                                        05160000
       2110-REC-MAPPA SECTION.                                          05170000
      *--------------*                                                  05180000
           MOVE '2110-REC-MAPPA' TO W-ULT-LABEL.                        05190000
      * ---                                                             05200000
           EXEC CICS RECEIVE MAP    ('MF02MAP')                         05210000
                             MAPSET ('MF02MAP')END-EXEC.                05220000
                                                                        05230000
       2110-EX. EXIT.                                                   05240000
                                                                        05250000
                                                                        05260000
                                                                        05270000
                                                                        05280000
       2120-NORMALIZZA SECTION.                                         05290000
      *---------------*                                                 05300000
           MOVE '2120-NORMALIZZA' TO W-ULT-LABEL.                       05310000
      * ---                                                             05320000
           INSPECT M-SCELTAI REPLACING ALL LOW-VALUE BY ' '.            05330000
           INSPECT M-SCELTAI REPLACING ALL '_'       BY ' '.            05340000
                                                                        05370000
       2120-EX. EXIT.                                                   05380000
                                                                        05390000
                                                                        05400000
                                                                        05420000
       2200-CONTROLLI SECTION.                                          05430000
      *--------------*                                                  05440000
           MOVE '2200-CONTROLLI' TO W-ULT-LABEL.                        05450000
      * ---                                                             05460000
           MOVE FSET-BRT TO M-SCELTAA.                                  05430002
                                                                        05480000
           IF M-SCELTAI = SPACE OR LOW-VALUE                            05490000
              MOVE SPACES   TO   M-MSG-1O                               05500000
              MOVE PROT-BRT TO   M-MSG-2A M-MSG-1A                      05500000
              MOVE '001'    TO   M-MSG-2O                               05500000
              MOVE -1       TO M-SCELTAL                                05510000
              MOVE FSET-BRT TO M-SCELTAA                                05520000
              PERFORM 2999-CERCA-ERR                                    05530000
           END-IF.                                                      05540000
                                                                        05550000
                                                                        05660000
           IF M-SCELTAI < '1' OR > '3'                                  05630012
             THEN                                                       05680000
                 MOVE SPACES        TO M-MSG-1O                         05690000
                 MOVE '007'         TO M-MSG-2O                         05690000
                 MOVE PROT-BRT      TO M-MSG-2A M-MSG-1A                05690000
                 MOVE -1            TO M-SCELTAL                        05700000
                 MOVE FSET-BRT      TO M-SCELTAA                        05710000
                 PERFORM 2999-CERCA-ERR                                 05720000
           END-IF.                                                      05730000
                                                                        05740000
           IF M-SCELTAI = '2' OR = '3'                                  05630012
             THEN                                                       05680000
                 MOVE SPACES        TO M-MSG-1O                         05690000
                 MOVE '005'         TO M-MSG-2O                         05690000
                 MOVE PROT-BRT      TO M-MSG-2A M-MSG-1A                05690000
                 MOVE -1            TO M-SCELTAL                        05700000
                 MOVE FSET-BRT      TO M-SCELTAA                        05710000
                 PERFORM 2999-CERCA-ERR                                 05720000
                                                                        05750000
           END-IF.                                                      05880000
                                                                        06020000
           IF M-SCELTAI = '1'                                           05630012
                 PERFORM 2700-PASSA-CTL                                 05750000
           END-IF.                                                      05880000
       2200-EX. EXIT.                                                   06030000
                                                                        06040000
                                                                        06240000
       2700-PASSA-CTL SECTION.                                          06710000
      *--------------*                                                  06720000
           MOVE '2700-PASSA-CTL'          TO W-ULT-LABEL.               06730000
      *---                                                              06740000
                                                                        06820000
           MOVE 'END'    TO W-CTL-END.                                  06860007
                                                                        07090000
           MOVE M-SCELTAO    TO W-SCELTA.                               07121019
                                                                        07122019
           IF W-SCELTA-1                                                07129015
              MOVE W-PGM-MENU-RAMO TO W-XCTL-PGM                        07129215
              PERFORM 3200-XCTL.                                        07129215
                                                                                
           IF W-SCELTA-2                                                07129015
              MOVE SPACES    TO M-MSG-1O                                07129215
              MOVE '005'     TO M-MSG-2O                                07129215
              MOVE PROT-BRT  TO M-MSG-2A M-MSG-1A                       07129215
              MOVE -1         TO M-SCELTAL                                      
              MOVE 'XXXXXXX'  TO W-XCTL-PGM                             07129915
              GO TO 2700-EX.                                            07130015
                                                                                
            IF W-SCELTA-3                                               07130415
               MOVE SPACES    TO M-MSG-1O                               07129215
               MOVE '005'     TO M-MSG-2O                               07129215
               MOVE PROT-BRT  TO M-MSG-2A M-MSG-1A                      07129215
               MOVE -1         TO M-SCELTAL                                     
               MOVE 'XXXXXXXX' TO W-XCTL-PGM                            07130615
               GO TO 2700-EX.                                           07130715
       2700-EX. EXIT.                                                   07100000
                                                                        07110000
                                                                        07120000
       2900-RIEMP-MASK SECTION.                                         07150000
      *---------------*                                                 07160000
           MOVE '2900-RIEMP-MASK' TO W-ULT-LABEL.                       07170000
      *---                                                              07180000
           MOVE LOW-VALUE      TO MF02MAPO.                             07190000
           MOVE -1             TO M-SCELTAL.                            07200000
                                                                        07210000
       2900-EX. EXIT.                                                   07220000
      *                                                                         
       2999-CERCA-ERR SECTION.                                          09570000
      *--------------*                                                  09580000
           MOVE '2999-CERCA-ERR' TO W-ULT-LABEL.                        09590000
      * ---                                                             09600000
           MOVE M-MSG-2O TO W-MSG-HOST.                                         
           SET IND-TAB TO 1.                                            09610000
                                                                        09620000
           SEARCH ELEM-TAB-MSG AT END                                   09630000
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'            09640000
                    TO M-MSG-2O                                         09650000
                  WHEN W-MSG-HOST    =  ELEM-COD-MSG(IND-TAB)           09660000
                       MOVE ELEM-DESC-MSG(IND-TAB)  TO M-MSG-2O         09670000
           END-SEARCH.                                                  09680000
                                                                        09690000
           PERFORM 3000-FINE-ELAB.                                      09700000
                                                                        09710000
       2999-EX. EXIT.                                                   09720000
      *                                                                         
       3000-FINE-ELAB SECTION.                                          07460000
      * -------------*                                                  07470000
           MOVE '3000-FINE-ELAB' TO W-ULT-LABEL.                        07480000
      * ---                                                             07490000
           MOVE SPACES   TO W-MSG-HOST.                                         
      *                                                                         
           IF W-CTL-END = 'LOOP'                                        07500000
             THEN                                                       07510000
              PERFORM 3100-RIENTRO                                      07520000
             ELSE                                                       07530000
              PERFORM 3200-XCTL                                         07540000
           END-IF.                                                      07550000
                                                                        07560000
       3000-EX. EXIT.                                                   07570000
                                                                        07580000
                                                                        07590000
                                                                        07600000
                                                                        07610000
       3100-RIENTRO SECTION.                                            07620000
      *------------*                                                    07630000
           MOVE '3100-RIENTRO' TO W-ULT-LABEL.                          07640000
      *---                                                              07650000
           PERFORM 3110-DATA-ORA.                                       07660000
           PERFORM 3130-FORMATTA-MAPPA.                                 07670000
           PERFORM 3140-INVIO-MAPPA.                                    07680000
                                                                        07690000
       3100-EX. EXIT.                                                   07700000
                                                                        07710000
                                                                        07720000
                                                                        07730000
                                                                        07740000
       3110-DATA-ORA SECTION.                                           07750000
      *-------------*                                                   07760000
           MOVE '3110-DATA-ORA' TO W-ULT-LABEL.                         07770000
      *-------------*                                                   07760000
           MOVE COM-DATA-SISTEMA TO M-DATA-SO.                          07780000
                                                                        07820000
       3110-EX. EXIT.                                                   07830000
                                                                        07840000
                                                                        07850000
                                                                        07860000
                                                                        07870000
       3130-FORMATTA-MAPPA SECTION.                                     07880000
      *-------------------*                                             07890000
           MOVE '3130-FORMATTA-MAPPA' TO W-ULT-LABEL.                   07900000
      *---                                                              07910000
           INSPECT M-SCELTAO REPLACING ALL ' '       BY '_'.            07920000
           INSPECT M-SCELTAO REPLACING ALL LOW-VALUE BY '_'.            07930000
           IF COM-MESSAGGIO NOT EQUAL SPACES                            07960000
              MOVE COM-MESSAGGIO    TO  M-MSG-2O                        07970000
              MOVE SPACES           TO  COM-MESSAGGIO                   07980000
           END-IF.                                                      07990000
       3130-EX. EXIT.                                                   08000000
                                                                        08010000
                                                                        08020000
                                                                        08030000
                                                                        08040000
       3140-INVIO-MAPPA SECTION.                                        08050000
      *----------------*                                                08060000
           MOVE '3140-INVIO-MAPPA' TO W-ULT-LABEL.                      08070000
      *---                                                              08080000
           IF W-NOME-PGM = COM-NOME-PGM                                 08090000
            THEN                                                        08100000
              EXEC CICS SEND                                            08110000
                        MAP    ('MF02MAP')                              08120000
                        MAPSET ('MF02MAP')                              08130000
                        DATAONLY                                        08140000
                        CURSOR
                        FREEKB                                          08150000
              END-EXEC                                                  08160000
           ELSE                                                         08170000
              MOVE W-NOME-PGM         TO COM-NOME-PGM                   08180000
              EXEC CICS SEND                                            08190000
                        MAP    ('MF02MAP')                              08200000
                        MAPSET ('MF02MAP')                              08210000
                        ERASE                                           08220000
                        CURSOR
                        FREEKB                                          08230000
             END-EXEC                                                   08240000
           END-IF.                                                      08250000
                                                                        08260000
           EXEC CICS RETURN                                             08270000
                        TRANSID  ('RR02')                               08280000
                        COMMAREA (W-COMMAREA)                           08290000
                        LENGTH   (W-LEN)                                08300000
           END-EXEC.                                                    08310000
                                                                        08320000
       3140-EX. EXIT.                                                   08330000
                                                                        08340000
                                                                        08350000
                                                                        08360000
       3200-XCTL SECTION.                                               08370000
      *--------------*                                                  08380000
           MOVE '3200-XCTL' TO W-ULT-LABEL.                             08390000
      *---                                                              08400000
           MOVE SPACE      TO        COM-MESSAGGIO.                     08410000
                                                                        08420000
           EXEC CICS XCTL                                               08430000
                        PROGRAM (W-XCTL-PGM)                            08440000
                        COMMAREA (W-COMMAREA)                           08450000
                        LENGTH   (W-LEN)                                08460000
           END-EXEC.                                                    08470000
       3200-EX. EXIT.                                                   08480000
