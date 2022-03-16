       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW13DEMO.                                            00020000
      * ---                                                             00030000
      ******************************************************************00040000
      * PROGETTO        :                                              *00050000
      * ID. TRANSAZIONE :                                              *00060000
      *----------------------------------------------------------------*00070000
      * AREA COMPETENTE :                                              *00080000
      * OGGETTO         :                                              *        
      * TIPO OPERAZIONE : SCRITTURA SUL VSAM DOPO ANNULLAMENTO          00100000
      * RIFERIMENTO P.E.:                                              *        
      *----------------------------------------------------------------*00120000
      * CREAZIONE       : 22/03/1999                                   *        
      * ULTIMA MODIFICA : 23/03/1999                                   *        
      ******************************************************************00150000
      * ---                                                             00160000
       ENVIRONMENT DIVISION.                                            00170000
       CONFIGURATION SECTION.                                           00180000
       DATA DIVISION.                                                   00190000
       WORKING-STORAGE SECTION.                                         00200000
                                                                        00210000
                                                                                
      * -------------------------------------------------------------- *00220000
      *      TRACCIATO RECORD DEL FILE VSAM                            *00230000
      * -------------------------------------------------------------- *00240000
           COPY CWFILE.                                                         
                                                                                
                                                                                
      * -------------------------------------------------------------- *00220000
      *      DEFINIZIONE VARIABILI HOST                                *00230000
      * -------------------------------------------------------------- *00240000
       01  W-COD-MSG-HOST                  PIC  X(3)  VALUE SPACE.      00250000
                                                                        00380000
                                                                        01570000
                                                                        01580000
      * -------------------------------------------------------------- *01590000
      *    DEFINIZIONE CAMPI STANDART DELLA TRANSAZIONE                *01600000
      * -------------------------------------------------------------- *01610000
       01  W-NOME-PGM                      PIC  X(8)   VALUE 'CW13DEMO'.01640000
       01  W-NOME-FILE                     PIC  X(8)   VALUE 'ARCHIVIO'.01640000
       01  W-ULT-LABEL                     PIC  X(15)  VALUE SPACES.    01670000
       01  W-TRS-ID                        PIC  X(4)   VALUE SPACES.    01670000
                                                                        01720000
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
           02  ULT-LABEL-CICS              PIC X(15).                   02270000
           02  FILLER                      PIC X(10)                    02280000
                                           VALUE ' TRANSID: '.          02290000
           02  TRS-ID-CICS                 PIC X(4).                    02300000
           02  FILLER                      PIC X(06)                    02310000
                                           VALUE ' PGM: '.              02320000
           02  NOME-PGM-CICS               PIC X(8).                    02330000
           02  FILLER                      PIC X(8).                    02340000
                                                                        02350000
                                                                        02480000
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
                                                                        03040000
                                                                        03110000
       1000-EX. EXIT.                                                   03120000
                                                                                
                                                                        03160000
      *---------------*                                                 03180000
       1100-COND-ANOMAL SECTION.                                        03170000
      *---------------*                                                 03180000
           EXEC CICS HANDLE ABEND      LABEL   (1110-ABEND-CICS)        03190000
                                                           END-EXEC.    03200000
                                                                        03260000
       1100-EX. EXIT.                                                   03270000
                                                                        03300000
                                                                        03310000
       1110-ABEND-CICS SECTION.                                         03320000
      *---------------*                                                 03330000
           MOVE W-ULT-LABEL  TO ULT-LABEL-CICS.                         03340000
           MOVE W-TRS-ID   TO TRS-ID-CICS.                              03350000
           MOVE W-NOME-PGM TO NOME-PGM-CICS.                            03360000
           EXEC CICS ASSIGN ABCODE (COD-ERR) END-EXEC.                  03370000
      *    EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                       03390000
                                                                        03420000
       1110-EX. EXIT.                                                   03430000
                                                                        03440000
                                                                        03470000
       2000-CORPO-ELAB  SECTION.                                                
      *---------------*                                                 05450000
      *    MOVE '2000-CORPO-ELAB' TO W-ULT-LABEL.                       05460000
      *----                                                             05470000
           MOVE DFHCOMMAREA TO W-COMMAREA.                              04470000
           EXEC CICS HANDLE CONDITION  
                                       NOTFND  (2300-INSERISCI)         03210000
                                       NOTOPEN (2050-COND-NOTFND)       03230000
                                       DUPREC  (2200-AGGIORNA)          03230000
                                       INVREQ  (2200-AGGIORNA)          03230000
                                       ERROR   (2050-COND-NOTFND)       03230000
           END-EXEC.                                                    03230000
                                                                        03240000
           PERFORM 2100-LEGGI-FILE.                                     05510000
           PERFORM 2200-AGGIORNA.                                       05510000
           PERFORM 2300-INSERISCI.                                      05510000
                                                                                
       2000-EX. EXIT.                                                   05580000
                                                                        05590000
       2050-COND-NOTFND SECTION.                                        03480000
      *--------------*                                                  04090000
           MOVE '2050-COND-NOTFND' TO W-ULT-LABEL.                      04100000
      *---                                                              04110000
           MOVE '** AGG. VSAM NON EFFETTUATO **'  TO ERR-CICS.          02620000
           MOVE  1   TO  COM-COD-RIT.                                           
           PERFORM 3000-FINE-ELAB.                                      02930000
                                                                        03510000
       2050-EX. EXIT.                                                   03520000
                                                                                
                                                                                
                                                                        05620000
       2100-LEGGI-FILE SECTION.                                                 
      *---------------*                                                 05450000
      *    MOVE '2100-LEGGI-FILE'         TO   W-ULT-LABEL.             05460000
      *----                                                             05470000
           MOVE COM-MATRICOLA             TO   VS-KEY.                          
           EXEC CICS READ      FILE      (W-NOME-FILE)                          
                               RIDFLD    (VS-KEY)                               
                               INTO      (VS-ARCHIVIO)                          
                               LENGTH    (VS-LUNG)                              
                               KEYLENGTH (VS-LUNG-KEY)                          
                               EQUAL                                            
                               UPDATE                                           
           END-EXEC.                                                            
                                                                                
       2100-EX. EXIT.                                                           
                                                                                
                                                                                
       2200-AGGIORNA  SECTION.                                          06270000
      *--------------*                                                  06280000
           MOVE '2200-AGGIORNA'        TO   W-ULT-LABEL.                06290000
      * ---                                                             06300000
           MOVE COM-MATRICOLA          TO   VS-MATRICOLA.               00102004
           MOVE COM-COGNOME            TO   VS-COGNOME.                 00103004
           MOVE COM-NOME               TO   VS-NOME.                    00104004
           MOVE COM-DATA-NASCITA       TO   VS-DATA-NASCITA.            00105009
           MOVE COM-QUALIFICA-INTERNA  TO   VS-QUALIFICA-INTERNA.       00106004
                                                                                
           EXEC CICS REWRITE   FILE      (W-NOME-FILE)                          
                               FROM      (VS-ARCHIVIO)                          
                               LENGTH    (VS-LUNG)                              
           END-EXEC.                                                            
           MOVE  0   TO  COM-COD-RIT.                                           
           PERFORM 3000-FINE-ELAB.                                              
                                                                                
       2200-EX. EXIT.                                                   09220000
                                                                        09690000
                                                                        06250000
       2300-INSERISCI SECTION.                                          06270000
      *--------------*                                                  06280000
           MOVE '2300-INSERISCI'       TO     W-ULT-LABEL.              06290000
      * ---                                                             06300000
           MOVE COM-MATRICOLA          TO   VS-MATRICOLA.               00102004
           MOVE COM-COGNOME            TO   VS-COGNOME.                 00103004
           MOVE COM-NOME               TO   VS-NOME.                    00104004
           MOVE COM-DATA-NASCITA       TO   VS-DATA-NASCITA.            00105009
           MOVE COM-QUALIFICA-INTERNA  TO   VS-QUALIFICA-INTERNA.       00106004
                                                                                
           EXEC CICS WRITE     FILE      (W-NOME-FILE)                          
                               RIDFLD    (VS-KEY)                               
                               FROM      (VS-ARCHIVIO)                          
                               LENGTH    (VS-LUNG)                              
                               KEYLENGTH (VS-LUNG-KEY)                          
           END-EXEC.                                                            
           MOVE  0   TO  COM-COD-RIT.                                           
           PERFORM  3000-FINE-ELAB.                                             
                                                                                
       2300-EX. EXIT.                                                   09220000
                                                                                
                                                                        09230000
                                                                                
       3000-FINE-ELAB SECTION.                                          09770000
      * -------------*                                                  09780000
           MOVE '3000-FINE-ELAB'     TO    W-ULT-LABEL.                 09790000
      * ---                                                             09800000
           MOVE  W-COMMAREA   TO DFHCOMMAREA.                           04470000
           EXEC CICS   RETURN   END-EXEC.                               09810000
                                                                        09860000
       3000-EX. EXIT.                                                   09870000
                                                                        09880000
