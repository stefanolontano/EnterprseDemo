       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. CW10DEMO.                                            00020000
      ******************************************************************00030000
      * PROGETTO        :                                              *00040000
      * OGGETTO         : ROUTINE COMPATTAMENTO/ALLINEAMENTO CARATTERI *00040000
      * DATA CREAZIONE  : 07/03/1999                                   *00040000
      * ULTIMA MODIFICA : ../../....                                   *00040000
      ******************************************************************00030000
      *                                                                 00030000
      *                                                                 00030000
      *================================================================*00030000
      * ACCETTA IN INPUT UNA STRINGA E LA RESTITUISCE COMPATTATA CON   *00040000
      * LA SUA LUNGHEZZA EFFETTIVA, ELIMINANDO GLI SPAZI NON SIGNIFI-  *00040000
      * CATIVI.                                                        *00040000
      * INOLTRE EFFETTUA, A RICHIESTA, L'ALLINEAMENTO A DESTRA O AL    *00040000
      * CENTRO IN BASE ALLA LUNGHEZZA DICHIARATA.                      *00040000
      *================================================================*00030000
      *                                                                *00040000
      * COPY DI LAVORO ===> CWC010                                    * 00040000
      *                                                                *00040000
      * AREA DI TRANSITO 'AREA-IET010CT' COSTITUITA DA:               * 00040000
      *                                                                *00040000
      * - LL-IET010CT = LUNGHEZZA AREA DI TRANSITO (FISSA)             *00040000
      *                                                                *00040000
      * - STRINGA      = STRINGA ALFANUMERICA (MAX 132 BYTES)          *00040000
      *                                                                *00040000
      * - LL-STRINGA   = LUNGHEZZA EFFETTIVA DELLA STRINGA DA          *00040000
      *                  COMPATTARE (MAX 132)                          *00040000
      *                  NB: DOPO IL COMPATTAMENTO ASSUME IL VALORE    *00040000
      *                      DEI BYTES REALMENTE OCCUPATI              *00040000
      *                                                                *00040000
      * - ALLINEAMENTO = CODICE ALFANUMERICO DI 1 BYTE.                *00040000
      *                  VALORI AMMESSI: 'C' = CENTRATURA              *00040000
      *                                  'D' = ALLINEA A DESTRA        *00040000
      *                  VALORE DEFAULT: ... = ALLINEA A SINISTRA      *00040000
      *                                                                *00040000
      * - RC-IET010CT = CODICE DI RITORNO.                             *00040000
      *----------------------------------------------------------------*00040000
      * DECODIFICA DI RC-IET010CT:                                     *00040000
      *                                                                *00040000
      * - SPACES       = OPERAZIONE CORRETTAMENTE ESEGUITA             *00040000
      * - 'E1'         = STRINGA DA TRATTARE NON SIGNIFICATIVA         *00040000
      * - 'E2'         = LUNGHEZZA STRINGA NON SIGNIFICATIVA           *00040000
      * - 'E3'         = LUNGHEZZA STRINGA NON CORRETTA.               *00040000
      *================================================================*00060000
       ENVIRONMENT DIVISION.                                            00070000
       CONFIGURATION SECTION.                                           00080000
       DATA DIVISION.                                                   00101000
      ******************************************************************        
       WORKING-STORAGE SECTION.                                         00102000
      ******************************************************************        
      *                                                                         
       01  IND1                       PIC 9(3).                                 
      *                                                                         
       01  IND2                       PIC 9(3).                                 
      *                                                                         
       01  LL-BLANK                   PIC 9(3).                                 
      *                                                                         
       01  LL-ORIGINE                 PIC 9(3).                                 
      *                                                                         
       01  ELEM-PRECEDENTE            PIC X.                                    
      *                                                                         
       01  STRINGA-IN.                                                          
           03  RIGA-IN    OCCURS  132.                                          
               05  ELEM-IN            PIC X.                                    
      *                                                                         
       01  STRINGA-WORK.                                                        
           03  RIGA-WORK  OCCURS  132.                                          
               05  ELEM-WORK          PIC X.                                    
      *                                                                         
       COPY  CWC010.                                                            
      *                                                                         
      ******************************************************************        
       LINKAGE SECTION.                                                 00102000
      ******************************************************************        
       01  DFHCOMMAREA.                                                         
           05  FILLER  PIC X OCCURS 1 TO 32767 DEPENDING ON EIBCALEN.           
      *                                                                         
      *                                                                         
       01  BLLCELLS.                                                            
           03  F                     PIC S9(8) COMP.                            
      *                                                                         
      ******************************************************************        
       PROCEDURE DIVISION.                                              00102000
      ******************************************************************        
      *                                                                         
       INIZIO-IET010CT.                                                         
      *                                                                         
              MOVE  DFHCOMMAREA    TO    AREA-IET010CT.                         
      *                                                                         
              MOVE  SPACES         TO    STRINGA-IN                             
                                         STRINGA-WORK                           
                                         ELEM-PRECEDENTE                        
                                         RC-IET010CT.                           
      *                                                                         
              MOVE  ZERO           TO    LL-BLANK                               
                                         LL-ORIGINE                             
                                         IND1                                   
                                         IND2.                                  
      *                                                                         
              IF  STRINGA  =  SPACES OR LOW-VALUE                               
                  MOVE  'E1'       TO    RC-IET010CT                            
                  GO TO FINE-IET010CT.                                          
      *                                                                         
              IF  LL-STRINGA  NOT  NUMERIC                                      
                  MOVE  'E2'       TO    RC-IET010CT                            
                  GO TO FINE-IET010CT.                                          
      *                                                                         
              IF  LL-STRINGA  =  ZERO  OR  >  132                               
                  MOVE  'E3'       TO    RC-IET010CT                            
                  GO TO FINE-IET010CT.                                          
      *                                                                         
              MOVE  STRINGA        TO    STRINGA-IN.                            
              MOVE  LL-STRINGA     TO    LL-ORIGINE.                            
      *                                                                         
              PERFORM  COMPRESSIONE                                             
                       VARYING  IND1  FROM  1  BY  1                            
                       UNTIL    IND1  >  LL-STRINGA.                            
      *                                                                         
              MOVE  STRINGA-WORK   TO    STRINGA.                               
              MOVE  IND2           TO    LL-STRINGA.                            
      *                                                                         
              IF  ELEM-IN (LL-ORIGINE) = SPACE OR LOW-VALUE                     
                  SUBTRACT  1    FROM    LL-STRINGA.                            
      *                                                                         
              IF  LL-STRINGA = LL-ORIGINE                                       
                  GO TO  FINE-IET010CT.                                         
      *                                                                         
              IF  ALLINEAMENTO = 'D'                                            
                  PERFORM  ALLINEA-DESTRA                                       
                  GO TO  FINE-IET010CT.                                         
      *                                                                         
              IF  ALLINEAMENTO = 'C'                                            
                  PERFORM  ALLINEA-CENTRO.                                      
      *                                                                         
       FINE-IET010CT.                                                           
      *                                                                         
              MOVE  AREA-IET010CT   TO    DFHCOMMAREA.                          
      *                                                                         
              EXEC  CICS  RETURN  END-EXEC.                                     
      *                                                                         
      *-----------------------------------------------------------------        
       COMPRESSIONE  SECTION.                                                   
      *                                                                         
              IF  ELEM-IN(IND1) = SPACE  AND  ELEM-PRECEDENTE = SPACE           
                  MOVE  ELEM-IN(IND1)     TO  ELEM-PRECEDENTE                   
              ELSE                                                              
                  MOVE  ELEM-IN(IND1)     TO  ELEM-PRECEDENTE                   
                  ADD   1                 TO  IND2                              
                  MOVE  ELEM-IN(IND1)     TO  ELEM-WORK(IND2).                  
      *                                                                         
       EX-COMPRESSIONE.                                                         
              EXIT.                                                             
      *-----------------------------------------------------------------        
       ALLINEA-DESTRA  SECTION.                                                 
      *                                                                         
              MOVE  SPACES                TO  STRINGA-IN                        
                                              STRINGA-WORK.                     
      *                                                                         
              MOVE  STRINGA               TO  STRINGA-IN.                       
      *                                                                         
              MOVE  LL-ORIGINE            TO  IND2.                             
      *                                                                         
              PERFORM  DESTRA  VARYING  IND1  FROM  LL-STRINGA                  
                       BY  -1  UNTIL    IND1     =  ZERO.                       
      *                                                                         
              MOVE  STRINGA-WORK          TO  STRINGA.                          
              MOVE  LL-ORIGINE            TO  LL-STRINGA.                       
      *                                                                         
       EX-ALLINEA-DESTRA.                                                       
              EXIT.                                                             
      *-----------------------------------------------------------------        
       DESTRA  SECTION.                                                         
      *                                                                         
              MOVE  ELEM-IN(IND1)         TO  ELEM-WORK(IND2).                  
      *                                                                         
              SUBTRACT  1               FROM  IND2.                             
      *                                                                         
       EX-DESTRA.                                                               
              EXIT.                                                             
      *-----------------------------------------------------------------        
       ALLINEA-CENTRO  SECTION.                                                 
      *                                                                         
              SUBTRACT  LL-STRINGA      FROM  LL-ORIGINE                        
                                      GIVING  LL-BLANK.                         
      *                                                                         
              IF  LL-BLANK  <  2                                                
                  GO TO  EX-ALLINEA-CENTRO                                      
              ELSE                                                              
                  DIVIDE    2           INTO  LL-BLANK  ROUNDED                 
                  SUBTRACT  LL-BLANK    FROM  LL-ORIGINE                        
                                      GIVING  IND2.                             
      *                                                                         
              MOVE  SPACES                TO  STRINGA-IN                        
                                              STRINGA-WORK.                     
      *                                                                         
              MOVE  STRINGA               TO  STRINGA-IN.                       
      *                                                                         
              PERFORM  CENTRO  VARYING  IND1  FROM  LL-STRINGA                  
                       BY  -1  UNTIL    IND1     =  ZERO.                       
      *                                                                         
              MOVE  STRINGA-WORK          TO  STRINGA.                          
              MOVE  LL-ORIGINE            TO  LL-STRINGA.                       
      *                                                                         
       EX-ALLINEA-CENTRO.                                                       
              EXIT.                                                             
      *-----------------------------------------------------------------        
       CENTRO  SECTION.                                                         
      *                                                                         
              MOVE  ELEM-IN(IND1)         TO  ELEM-WORK(IND2).                  
      *                                                                         
              SUBTRACT  1               FROM  IND2.                             
      *                                                                         
       EX-CENTRO.                                                               
              EXIT.                                                             
      *-----------------------------------------------------------------        
