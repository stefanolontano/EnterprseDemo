       ID DIVISION.                                                             
       PROGRAM-ID.          CW11DEMO.                                           
       DATE-WRITTEN.                                                            
       AUTHOR.                                                                  
      *----------------------------------------------------------------*        
      *   NOME PROGRAMMA:   CW11DEMO                                   *        
      *   NOME TRANSID  :   RR11                                       *        
      *                                                                *        
      *   TIPO PROGRAMMA:   STAMPA                                     *        
      *   PROTOTIPO     :   RPSTAMPA                                   *        
      *----------------------------------------------------------------*        
       ENVIRONMENT DIVISION.                                                    
      *                                                                         
       CONFIGURATION SECTION.                                                   
       SPECIAL-NAMES.                                                           
           DECIMAL-POINT IS COMMA.                                              
      *                                                                         
       DATA DIVISION.                                                           
           EJECT                                                                
       WORKING-STORAGE SECTION.                                                 
      *-----------------------------------------------------                    
      * GESTIONE ERRORE CICS                                                    
      *-----------------------------------------------------                    
       01  MSG-ERRORE-CICS.                                                     
           05 FILLER              PIC X(13)   VALUE 'ERRORE CICS  '.            
           05 FILLER              PIC X(11)   VALUE 'NEL PROG : '.              
           05 CICS-PGM            PIC X(10)   VALUE SPACES.                     
           05 FILLER              PIC X(03)   VALUE ' - '.                      
           05 FILLER              PIC X(13)   VALUE 'ISTRUZIONE : '.            
           05 COMODO-CICS         PIC X(03)   VALUE 'XXX'.                      
           05 FILLER              PIC X(27)   VALUE SPACES.                     
      *                                                                         
       01  COD-MSG-HOST           PIC X(3)    VALUE SPACES.                     
      *                                                                         
       01  W-LEN                  PIC S9(3) COMP  VALUE +250.                   
      *                                                                         
       01  FILLER.                                                              
           05 TERM-MSG               PIC X(37)                                  
                     VALUE 'TERMINALE NON ABILITATO ALLA STAMPA.'.              
       01  FILLER.                                                              
           05 FILLER                 PIC X(32)                                  
                     VALUE '--- AREA DI IDENTIFICAZIONE ---'.                   
           05 W-PGM                  PIC X(8)         VALUE 'CW11DEMO'.         
           05 W-TRANSAZIONE          PIC X(4)         VALUE 'RR11'.             
      *----------------------------------------------------------------*        
       01  FILLER                      PIC X(32)                                
                  VALUE '---AREA DI LAVORO STANDARD-----'.                      
      *----------------------------------------------------------------*        
       01  W-CONT-PAG                  PIC S9(04)  COMP  VALUE +0.              
       01  FLAG-PRIMA-VOLTA            PIC 9 VALUE 0.                           
                                                                                
      *----------------------------------------------------------------*        
      * CAMPI PER IL FORMATO  DELLA PAGINA E PER LA GESTIONE DEL SALTO *        
      *  PAGINA *                                                               
      *----------------------------------------------------------------*        
       01  W-RIGHE-DA-STAMPARE         PIC 99          VALUE 0.                 
      *----------------------------------------------------------------*        
      *       NUMERO DI RIGHE DI DETTAGLIO DA STAMPARE                 *        
      *       E RELATIVO CAMPO D'APPOGGIO PER L'ELABORAZIONE:          *        
      *       I DUE VALORI DEVONO ESSERE UGUALI                        *        
      *----------------------------------------------------------------*        
       01  W-RIGHE-DETTAGLIO           PIC 99          VALUE 66.                
       01  W-RIGHE-DETT-APP            PIC 99          VALUE 66.                
      *----------------------------------------------------------------*        
      * LA CODA TD VIENE SCRITTA A BLOCCHI DI RIGHE DI STAMPA                   
      * PRECEDENTEMENTE CARICATE SU UN BUFFER CHE HA UNA LUNGHEZZA PARI         
      * A 14 RIGHE PIU' UN CARATTERE DI CONTROLLO DI FINE-BUFFER                
      *----------------------------------------------------------------*        
      * IL VALORE DI QUESTO CAMPO E' : LUNGHEZZA RIGA BUFFER  *        *        
      *                                NUMERO RIGHE BUFFER + 1         *        
      *                                132*14 + 1 = +1849              *        
      *----------------------------------------------------------------*        
       01  W-LUNG-LENG               PIC S9(4) COMP VALUE +1849.                
       01  LENG                      PIC S9(4) COMP VALUE +1849.                
       01  CODA                      PIC X(4)       VALUE 'BR67'.               
      *-----------------------------------------------------------------        
      *             R I G A            B U F F E R                              
      *   DIMENSIONE RIGA BUFFER   =   BYTE RIGA-STAMPA + 1                     
      *----------------------------------------------------------------*        
       01  BUFFER.                                                              
           03 FILLER.                                                           
              05 FILLER OCCURS 14.                                              
                10 ELEMENTO             PIC  X(132).                            
           03 FINE-BUFFER               PIC X.                                  
                                                                                
       01 IND-STAMPA                PIC 9 VALUE 0.                              
      *----------------------------------------------------------------*        
      *       CARATTERI DI CONTROLLO                                            
      *       CARATTERE DI END OF MESSAGE.                                      
      *       SEGNA LA FINE DEL BUFFER CARICATO SULLA CODA TD.                  
      *----------------------------------------------------------------*        
       01  ENDMESS.                                                             
           05 EM                     PIC 99 COMP VALUE 25.                      
           05 FX REDEFINES EM.                                                  
                10 FILLER                 PIC  X.                               
                10 EOM                    PIC  X.                               
      *----------------------------------------------------------------*        
      *       CARATTERE DI END OF PAGE.                                         
      *       SEGNA LA FINE DEL BUFFER CARICATO SULLA CODA TD.                  
      *----------------------------------------------------------------*        
       01  ENDPAGE.                                                             
           05  EP                           PIC 99   VALUE 12 COMP.             
           05  F REDEFINES  EP.                                                 
               10  F                        PIC X.                              
               10  ENDP                     PIC X.                              
                                                                                
      *----------------------------------------------------------------*        
      *       CONDIZIONE DI SALTO A RIGA INFERIORE                              
      *----------------------------------------------------------------*        
       01  NEWLINE.                                                             
           05 NLN                    PIC 99 COMP VALUE 21.                      
           05 TX REDEFINES NLN.                                                 
                10 FILLER                 PIC  X.                               
                10 NL                     PIC  X.                               
      *----------------------------------------------------------------*        
      *             R I G A       D I      S T A M P A                          
      *----------------------------------------------------------------*        
       01  W-RIGA-STAMPA.                                                       
           05 W-CAMPO-RIGA-STAMPA           PIC X(131).                         
           05 W-CAR-CONTR-STAMPA            PIC X.                              
      *-----------------------------------------------------------------        
      *             R I G A            S P A C E S                              
      *-----------------------------------------------------------------        
       01  W-RIGA-SPACES.                                                       
           05 W-CAMPO-RIGA-SPACES         PIC X(132)  VALUE SPACES.             
                                                                                
      *-------------------------------------------------------                  
      *    DESCRIZIONE  TRACCIATI DELLE TESTATE PER LA STAMPA*                  
      *-------------------------------------------------------                  
      *                                                                         
       01  W-TESTATA1.                                                          
           03  FILLER      PIC  X(45)  VALUE SPACE.                             
           03  FILLER      PIC  X(18)  VALUE 'STAMPA  DIPENDENTI'.              
           03  FILLER      PIC  X(54)  VALUE SPACE.                             
           03  ST-DATA-S   PIC  X(10)  VALUE SPACE.                             
           03  FILLER      PIC  X(05)  VALUE SPACE.                             
      *                                                                         
       01  W-TESTATA2.                                                          
           03  FILLER           PIC  X(04)   VALUE SPACE.                       
           03  FILLER           PIC  X(09)   VALUE 'MATRICOLA'.                 
           03  FILLER           PIC  X(05)   VALUE SPACE.                       
           03  FILLER           PIC  X(07)   VALUE 'COGNOME'.                   
           03  FILLER           PIC  X(25)   VALUE SPACE.                       
           03  FILLER           PIC  X(04)   VALUE 'NOME'.                      
           03  FILLER           PIC  X(11)   VALUE SPACE.                       
           03  FILLER           PIC  X(12)   VALUE 'DATA NASCITA'.              
           03  FILLER           PIC  X(10)   VALUE SPACE.                       
           03  FILLER           PIC  X(10)   VALUE 'QUALIFICA '.                
           03  FILLER           PIC  X(07)   VALUE 'INTERNA'.                   
           03  FILLER           PIC  X(18)   VALUE SPACE.                       
           03  FILLER           PIC  X(04)   VALUE 'PAG '.                      
           03  PAGINA           PIC  ZZZ9    VALUE ZEROES.                      
           03  FILLER           PIC  X(02)   VALUE SPACE.                       
      *                                                                         
       01  W-DETTAGLIO.                                                         
           03  FILLER         PIC  X(04)  VALUE SPACES.                         
           03  ST-MATRICOLA   PIC  9(05).                                       
           03  FILLER         PIC  X(09)  VALUE SPACES.                         
           03  ST-COGNOME     PIC  X(30).                                       
           03  FILLER         PIC  X(03)  VALUE SPACES.                         
           03  ST-NOME        PIC  X(15).                                       
           03  FILLER         PIC  X(02)  VALUE SPACES.                         
           03  ST-COD-FISC    PIC  X(16).                                       
           03  FILLER         PIC  X(02)  VALUE SPACES.                         
           03  ST-DATA        PIC  X(10).                                       
           03  FILLER         PIC  X(05)  VALUE SPACES.                         
           03  ST-QU-INT      PIC  X(08).                                       
           03  FILLER         PIC  X(23)  VALUE SPACES.                         
                                                                                
                                                                                
      *------------------------------------------------------------             
      **         TRACCIATO RECORD DELLA CODA                      *             
      *-------------------------------------------------------------            
       01  W-IND                           PIC 9(2).                            
       01  W-CONT-TS                       PIC S9(4) COMP VALUE +0.             
       01  W-CODA-TS                       PIC X(08).                           
       01  LENGTH-TS                       PIC S9(04) COMP VALUE +850.          
       01  CODA-TS.                                                             
           03  W-ELE-STAMPA  OCCURS 10.                                         
                05  FILLER         PIC X(1).                                    
                05  W-MATR         PIC 9(05).                                   
                05  W-COGN         PIC X(30).                                   
                05  W-NOME         PIC X(15).                                   
                05  W-COD-FISC     PIC X(16).                                   
                05  W-DT-NAT.                                                   
                    07  W-ANNO     PIC X(04).                                   
                    07  TRAT       PIC X     VALUE '/'.                         
                    07  W-MESE     PIC X(02).                                   
                    07  TRA        PIC X     VALUE '/'.                         
                    07  W-GIORNO   PIC X(02).                                   
                05  W-QU-IN        PIC X(08).                                   
      *------------------------------------------------------------             
      *          TRACCIATO RECORD D'APPOGGIO DELLA CODA           *             
      *------------------------------------------------------------             
       01  APPO-STAMPA.                                                         
           03  APPO-ELE-STAMPA OCCURS 10.                                       
                05  FILLER            PIC X(1).                                 
                05  APPO-MATR         PIC 9(05).                                
                05  APPO-COGN         PIC X(30).                                
                05  APPO-NOME         PIC X(15).                                
                05  APPO-DT-NAT       PIC X(10).                                
                05  APPO-QU-IN        PIC X(08).                                
      *-----------------------------------------------------------------        
      * RIGA PER EVENTUALI MESSAGGI DA MANDARE IN STAMPA                        
      *-----------------------------------------------------------------        
       01  W-MSG-STAMPA.                                                        
           03 FILLER                      PIC X(130)   VALUE SPACES.            
      *-----------------------------------------------------------------        
      *  VARIABILI DI COMODO                                                    
      *-----------------------------------------------------------------        
      *                                                                         
       01  CONTATORE                    PIC 99       VALUE ZERO.                
       01  VARIABILI.                                                           
           03 CONT-PAG                  PIC 99       VALUE 0.                   
      *-----------------------------------------------------------------        
      *    COMMAREA                                                             
      *-----------------------------------------------------------------        
       01 W-COMMAREA.                                                           
           COPY CWCOMMA.                                                        
      *-----------------------------------------------------------------        
           COPY CWMESS.                                                         
      *-----------------------------------------------------------------        
       LINKAGE SECTION.                                                         
      *----------------------------------------------------------------*        
      *    AREA DI COMUNICAZIONE *                                              
      *----------------------------------------------------------------*        
       01  DFHCOMMAREA.                                                         
           05  FILLER PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.            
      *                                                                         
      *----------------------------------------------------------------*        
       PROCEDURE DIVISION.                                                      
      *                                                                         
           PERFORM 1000-INIZIO       THRU END-1000-INIZIO.                      
      *                                                                         
           PERFORM 2000-ELABORAZIONE THRU  END-2000-ELABORAZIONE                
                                     UNTIL W-CONT-TS > COM-TOT-PAG.             
      *                                                                         
           PERFORM 3000-FINE          THRU END-3000-FINE.                       
      *                                                                         
       1000-INIZIO.                                                             
      *                                                                         
           PERFORM 1100-OPERAZIONI-INIZIALI                                     
                                   THRU END-1100-OPERAZIONI-INIZIALI.           
      *                                                                         
      *    PERFORM 1200-ENQ-RISORSA THRU END-1200-ENQ-RISORSA.                  
      *                                                                         
       END-1000-INIZIO.                                                         
           EXIT.                                                                
      *----------------------------------------------------------------*        
      *     OPERAZIONI INIZIALI                                        *        
      *----------------------------------------------------------------*        
       1100-OPERAZIONI-INIZIALI.                                                
      *                                                                         
           EXEC CICS                                                            
                HANDLE CONDITION                                                
                       ERROR   (1110-GESTIONE-ERRORE-CICS)                      
           END-EXEC.                                                            
      *                                                                         
           EXEC CICS                                                            
                HANDLE ABEND                                                    
                       LABEL   (1110-GESTIONE-ERRORE-CICS)                      
           END-EXEC.                                                            
      *    CALL 'RRULUNG'  USING COMMAREA-DI-RAMO                               
      *                          LUNG-COMMAREA                                  
      *                          LUNG-COMMAREA.                                 
      *                                                                         
           MOVE 002                      TO COMODO-CICS.                        
                                                                                
           EXEC CICS RETRIEVE                                                   
                     INTO    (W-COMMAREA)                                       
                     LENGTH  (W-LEN)                                            
           END-EXEC.                                                            
      *                                                                         
           PERFORM 1120-LEGGI-CODA-TS THRU END-1120-LEGGI-CODA-TS.              
      *                                                                         
      *    MOVE C43510-CF TO W-C43510-CF.                                       
      *    MOVE C43510-CT TO W-C43510-CT.                                       
                                                                                
       END-1100-OPERAZIONI-INIZIALI.                                            
           EXIT.                                                                
      *---------------------------------------------------------------          
      * GESTIONE ABEND CICS                                                     
      *-----------------------------------------------------------------        
       1110-GESTIONE-ERRORE-CICS.                                               
      * ----------------------------------------------------------------        
                                                                                
                                                                                
           MOVE  'CW11DEMO' TO CICS-PGM.                                        
                                                                                
           EXEC  CICS                                                           
                 IGNORE  CONDITION                                              
                         ERROR                                                  
           END-EXEC.                                                            
                                                                                
           MOVE MSG-ERRORE-CICS TO COM-MESSAGGIO.                               
           IF COMODO-CICS = '030'                                               
              MOVE '173'        TO COD-MSG-HOST                                 
              PERFORM 1115-IMPOSTA-MESSAGGIO THRU                               
                                    END-1115-IMPOSTA-MESSAGGIO.                 
           PERFORM 3000-FINE      THRU END-3000-FINE.                           
                                                                                
       END-1110-GESTIONE-ERRORE-CICS.                                           
           EXIT.                                                                
      *---------------------------------------------------------------          
       1115-IMPOSTA-MESSAGGIO.                                                  
      *---------------------------------------------------------------          
           SET IND-TAB TO 1.                                                    
           SEARCH ELEM-TAB-MSG AT END                                           
                  MOVE  '** CODICE MESSAGGIO NON TROVATO **'                    
                         TO COM-MESSAGGIO                                       
                  WHEN COD-MSG-HOST  =  ELEM-COD-MSG (IND-TAB)                  
                       MOVE ELEM-DESC-MSG( IND-TAB)                             
                         TO COM-MESSAGGIO                                       
           END-SEARCH.                                                          
       END-1115-IMPOSTA-MESSAGGIO.                                      09158199
             EXIT.                                                      09159099
      *-----------------------------------------------------------------        
       1120-LEGGI-CODA-TS.                                                      
      *----------------------------------------------------------------         
                                                                                
           EXEC CICS HANDLE CONDITION ITEMERR(1110-GESTIONE-ERRORE-CICS)        
                                      QIDERR (1110-GESTIONE-ERRORE-CICS)        
           END-EXEC.                                                            
                                                                                
           MOVE '003'             TO COMODO-CICS.                               
      *                                                                         
           MOVE COM-NOME-CODA     TO W-CODA-TS.                                 
GABRY *    MOVE COM-TOT-PAG       TO W-CONT-TS.                                 
           ADD 1                  TO W-CONT-TS.                                 
      *                                                                         
           EXEC CICS READQ TS QUEUE(W-CODA-TS)                                  
                              INTO  (CODA-TS)                                   
                              LENGTH(LENGTH-TS)                                 
                              ITEM (W-CONT-TS)                                  
           END-EXEC.                                                            
                                                                                
       END-1120-LEGGI-CODA-TS.  EXIT.                                           
      *----------------------------------------------------------------*        
      *          ACQUISIZIONE DELLA RISORSA                            *        
      *----------------------------------------------------------------*        
      *1200-ENQ-RISORSA.                                                        
      *                                                                         
      *    MOVE COM-COD-STAMPANTE  TO   CODA.                                   
      *                                                                         
      *    MOVE +4                 TO   LENG.                                   
      *                                                                         
      *    MOVE  '001'             TO   COMODO-CICS.                            
      *                                                                         
      *    EXEC CICS ENQ                                                        
      *              RESOURCE   (CODA)                                          
      *              LENGTH     (LENG)                                          
      *    END-EXEC.                                                            
      *                                                                         
      *END-1200-ENQ-RISORSA.                                                    
      *    EXIT.                                                                
      *----------------------------------------------------------------*        
      *   NEL CICLO ELABORATIVO SI IMPOSTANO I CAMPI DI WORKING        *        
      *   PREDISPOSTI PER LA STAMPA CON I DATI LETTI IN ARCHIVIO       *        
      *   E SI CARICA IL BUFFER CON I DATI ELABORATI. OGNIQUALVOLTA IL *        
      *   BUFFER E' PIENO -- 14 RIGHE -- IL SUO CONTENUTO VIENE        *        
      *   SCARICATO SULLA STAMPANTE                                    *        
      *----------------------------------------------------------------*        
                                                                                
       2000-ELABORAZIONE.                                                       
                                                                                
           PERFORM 2100-GESTIONE-STAMPA THRU END-2100-GESTIONE-STAMPA.          
      *                                                                         
           PERFORM 1120-LEGGI-CODA-TS   THRU END-1120-LEGGI-CODA-TS.            
      *                                                                         
           PERFORM 3100-CHIUSURA-STAMPA THRU END-3100-CHIUSURA-STAMPA.          
MR         MOVE 15                 TO CONTATORE.                                
MR         MOVE 5                  TO IND-STAMPA.                               
MR         PERFORM 2116-SCRIVI-TD  THRU END-2116-SCRIVI-TD.                     
      *                                                                         
       END-2000-ELABORAZIONE.                                                   
           EXIT.                                                                
      *---------------------------------------------------------------          
       2100-GESTIONE-STAMPA.                                                    
      *                                                                         
           IF FLAG-PRIMA-VOLTA = 0                                              
              MOVE  1                 TO FLAG-PRIMA-VOLTA                       
              MOVE  66                TO  W-RIGHE-DA-STAMPARE                   
              MOVE  66                TO  W-RIGHE-DETTAGLIO                     
              PERFORM 2110-TESTA1     THRU END-2110-TESTA1                      
              PERFORM 2120-TESTA2     THRU END-2120-TESTA2                      
           ELSE                                                                 
              PERFORM 2140-CTL-RIGHE  THRU END-2140-CTL-RIGHE.                  
      *                                                                         
           PERFORM 2130-RIGA-DETT   THRU END-2130-RIGA-DETT                     
             VARYING W-IND FROM 1 BY 1 UNTIL W-IND > 10.                        
      *                                                                         
       END-2100-GESTIONE-STAMPA. EXIT.                                          
      *----------------------------------------------------------------         
       2110-TESTA1.                                                             
           PERFORM 2125-RIGHE-SPACES THRU END-2125-RIGHE-SPACES 2 TIMES.        
           MOVE COM-DATA-SISTEMA     TO ST-DATA-S.                              
           MOVE W-TESTATA1           TO W-RIGA-STAMPA.                          
           PERFORM 2115-STAMPA-RIGA  THRU END-2115-STAMPA-RIGA.                 
       END-2110-TESTA1.   EXIT.                                                 
      *----------------------------------------------------------------         
       2120-TESTA2.                                                             
           PERFORM 2125-RIGHE-SPACES THRU END-2125-RIGHE-SPACES 2 TIMES.        
           ADD 1                     TO W-CONT-PAG.                             
           MOVE W-CONT-PAG           TO PAGINA.                                 
           MOVE W-TESTATA2           TO W-RIGA-STAMPA.                          
           PERFORM 2115-STAMPA-RIGA  THRU END-2115-STAMPA-RIGA.                 
       END-2120-TESTA2.  EXIT.                                                  
      *----------------------------------------------------------------         
       2130-RIGA-DETT.                                                          
                                                                                
           PERFORM 2125-RIGHE-SPACES THRU END-2125-RIGHE-SPACES.                
      *                                                                         
           MOVE  W-MATR(W-IND)       TO  ST-MATRICOLA.                          
           MOVE  W-COGN(W-IND)       TO  ST-COGNOME.                            
           MOVE  W-NOME(W-IND)       TO  ST-NOME.                               
           MOVE  W-COD-FISC(W-IND)   TO  ST-COD-FISC.                           
           MOVE  W-DT-NAT(W-IND)     TO  ST-DATA.                               
           MOVE  W-QU-IN(W-IND)      TO  ST-QU-INT.                             
      *                                                                         
           MOVE W-DETTAGLIO          TO W-RIGA-STAMPA.                          
           PERFORM 2115-STAMPA-RIGA  THRU END-2115-STAMPA-RIGA.                 
                                                                                
       END-2130-RIGA-DETT. EXIT.                                                
      *---------------------------------------------------------------          
       2140-CTL-RIGHE.                                                          
      *                                                                         
           IF W-RIGHE-DA-STAMPARE = 10                                          
              PERFORM 2125-RIGHE-SPACES THRU END-2125-RIGHE-SPACES UNTIL        
                 W-RIGHE-DA-STAMPARE = 0                                        
              MOVE 66                   TO W-RIGHE-DA-STAMPARE                  
              MOVE 66                   TO W-RIGHE-DETTAGLIO                    
              PERFORM 2120-TESTA2 THRU  END-2120-TESTA2.                        
      *                                                                         
       END-2140-CTL-RIGHE.                                                      
           EXIT.                                                                
      *----------------------------------------------------------------*        
      *    NL E' UN CARATTERE DI CONTROLLO PER LA STAMPA               *        
      *----------------------------------------------------------------*        
       2115-STAMPA-RIGA.                                                        
           MOVE NL                TO W-CAR-CONTR-STAMPA                         
           PERFORM 2116-SCRIVI-TD THRU END-2116-SCRIVI-TD                       
           SUBTRACT 1       FROM W-RIGHE-DA-STAMPARE W-RIGHE-DETTAGLIO.         
       END-2115-STAMPA-RIGA.                                                    
           EXIT.                                                                
      *-----------------------------------------------------------------        
       2125-RIGHE-SPACES.                                                       
      *                                                                         
           MOVE W-RIGA-SPACES       TO W-RIGA-STAMPA.                           
           PERFORM 2115-STAMPA-RIGA THRU END-2115-STAMPA-RIGA.                  
      *                                                                         
       END-2125-RIGHE-SPACES.                                                   
           EXIT.                                                                
      *----------------------------------------------------------------*        
       3110-RIGHE-SPACES1.                                                      
      *                                                                         
           MOVE W-RIGA-SPACES        TO W-RIGA-STAMPA.                          
           PERFORM 3115-STAMPA-RIGA1 THRU END-3115-STAMPA-RIGA1.                
      *                                                                         
       END-3110-RIGHE-SPACES1.                                                  
           EXIT.                                                                
      *----------------------------------------------------------------*        
       3115-STAMPA-RIGA1.                                                       
           MOVE NL                TO W-CAR-CONTR-STAMPA                         
           PERFORM 2116-SCRIVI-TD THRU END-2116-SCRIVI-TD.                      
       END-3115-STAMPA-RIGA1.                                                   
           EXIT.                                                                
      *----------------------------------------------------------------*        
       2116-SCRIVI-TD.                                                          
           ADD 1          TO CONTATORE                                          
      *                                                                         
           IF CONTATORE LESS 15                                                 
              MOVE W-RIGA-STAMPA TO ELEMENTO(CONTATORE)                         
              MOVE SPACES TO W-RIGA-STAMPA                                      
              GO TO END-2116-SCRIVI-TD.                                         
      *                                                                         
           PERFORM 2117-SCRITTURA-TD THRU END-2117-SCRITTURA-TD.                
      *                                                                         
           MOVE SPACES        TO BUFFER.                                        
           MOVE 1             TO CONTATORE.                                     
           MOVE W-RIGA-STAMPA TO ELEMENTO(CONTATORE).                           
           MOVE SPACES        TO W-RIGA-STAMPA.                                 
      *                                                                         
       END-2116-SCRIVI-TD.                                                      
           EXIT.                                                                
           SKIP3                                                                
      *----------------------------------------------------------------*        
       2117-SCRITTURA-TD.                                                       
      *                                                                         
           MOVE  '004'           TO     COMODO-CICS.                            
           ADD   1               TO     IND-STAMPA.                             
      *                                                                         
           IF IND-STAMPA < 5                                                    
              MOVE  EOM          TO     FINE-BUFFER                             
           ELSE                                                                 
              MOVE 0             TO  IND-STAMPA                                 
              MOVE ENDP          TO FINE-BUFFER.                                
      *                                                                         
           MOVE  W-LUNG-LENG     TO  LENG.                                      
           EXEC CICS WRITEQ  TD                                                 
                     QUEUE   (CODA)                                             
                     FROM    (BUFFER)                                           
                     LENGTH  (LENG)                                             
           END-EXEC.                                                            
       END-2117-SCRITTURA-TD.                                                   
           EXIT.                                                                
      *-----------------------------------------------------------------        
       3000-FINE.                                                               
                                                                                
           PERFORM 3100-CHIUSURA-STAMPA THRU END-3100-CHIUSURA-STAMPA.          
                                                                                
           PERFORM 3200-RILASCIA-RISORSA THRU END-3200-RILASCIA-RISORSA.        
                                                                                
      *                                                                         
           PERFORM 3300-FINE-PROGRAMMA.                                         
           GOBACK.                                                              
      *                                                                         
       END-3000-FINE.                                                           
           EXIT.                                                                
      *                                                                         
      *-----------------------------------------------------------------        
       3100-CHIUSURA-STAMPA.                                                    
      *                                                                         
           IF W-RIGHE-DETTAGLIO NOT = 0                                         
              PERFORM 2125-RIGHE-SPACES THRU END-2125-RIGHE-SPACES              
                     W-RIGHE-DETTAGLIO TIMES                                    
              PERFORM 3110-RIGHE-SPACES1                                        
                     THRU END-3110-RIGHE-SPACES1 4 TIMES.                       
      *                                                                         
       END-3100-CHIUSURA-STAMPA.                                                
           EXIT.                                                                
      *-----------------------------------------------------------------        
       3200-RILASCIA-RISORSA.                                                   
      *                                                                         
           MOVE  '005'                    TO  COMODO-CICS.                      
           MOVE  +4                       TO  LENG.                             
      *                                                                         
           EXEC CICS DEQ                                                        
                     RESOURCE (CODA)                                            
                     LENGTH   (LENG)                                            
           END-EXEC.                                                            
       END-3200-RILASCIA-RISORSA.                                               
           EXIT.                                                                
           SKIP3                                                                
      *-----------------------------------------------------------------        
      *                                                                         
       3300-FINE-PROGRAMMA.                                                     
      *                                                                         
           MOVE  '006'                    TO  COMODO-CICS.                      
      *                                                                         
           EXEC CICS RETURN END-EXEC.                                           
