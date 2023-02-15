      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. EXER0203.
       AUTHOR.     JOHNATHAN.
      *================================================================*
      *              C A P G E M I N I - S I S T E M A S               *
      *================================================================*
      *    PROGRAMA....: EXER0203
      *    PROGRAMADOR.: JOHNATHAN
      *    ANALISTA....: ARI BORGES                                *
      *    DATA........: 16/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   GERAR ARQUIVO CSV COM O VALOR TOTAL         *
      *                    DE DEPOSITOS E A DATA DO DEPOSITO           *
      *                    MAIS RECENTE, OBTIDOS EM UM ARQUIVO         *
      *                    QUE CONTEM OS DEPOSITOS DOS CLIENTES        *
      *                    POR CPF.                                    *
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQENT01                                  ENT02103
      *      ARQSAI01                                  SAI02103
      *----------------------------------------------------------------*
      *    ROTINAS.....:                                               *
      *                                                                *
      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
           SELECT ARQENT01 ASSIGN      TO UT-S-ARQENT01
                      FILE STATUS      IS WRK-FS-ARQENT01.
.
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS      IS WRK-FS-ARQSAI01.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================
      *                                                                *
      *----------------------------------------------------------------
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ARQUIVO DOS REGISTROS DE ENTRADA E SAIDA                    *
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    INPUT:     ARQUIVO DE ENTRADA                               *
      *               ORG. SEQUENCIAL   -   LRECL = 175                *
      *----------------------------------------------------------------*

       FD  ARQENT01
           RECORDING MODE IS F
           LABEL RECORD   IS STANDARD
           BLOCK CONTAINS  0 RECORDS.
       01 FD-ARQENT01             PIC X(36).

      *---------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 97                *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(33).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'EXER0102 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC  X(008) VALUE 'EXER0203'.
       77 WRK-MASK-QTDREG         PIC  ZZ.ZZ9.
       77 ACU-LIDOS-ARQENT01      PIC  9(005) VALUE ZEROS.
       77 ACU-GRAVA-ARQSAI01      PIC  9(005) VALUE ZEROS.
       77 ACU-VALOR-DEP           PIC  S9(017)V99 COMP-3  VALUE +0.
       77 WRK-DATA-RECENTE        PIC  9(008) VALUE ZEROS.
       77 WRK-DATA-CORRENTE       PIC  9(008) VALUE ZEROS.
       77 WRK-DATA-LIDA           PIC  9(008) VALUE ZEROS.
       77 WRK-DATA-ANT            PIC  9(008) VALUE ZEROS.
       77 WRK-CPF-LIDO            PIC  9(011) VALUE ZEROS.
       77 WRK-CPF-ANT             PIC  9(011) VALUE ZEROS.
       77 WRK-VAL-DEP             PIC  9(015) VALUE ZEROS.

      *
       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
          88 WRK-CN-ARQENT01      VALUE 'ENT01113'.
          88 WRK-CN-ARQSAI01      VALUE 'SAI01113'.

       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
          88 WRK-CN-OPEN          VALUE 'OPEN '.
          88 WRK-CN-CLOSE         VALUE 'CLOSE'.
          88 WRK-CN-READ          VALUE 'READ '.
          88 WRK-CN-WRITE         VALUE 'WRITE'.

       01 WRK-DATA-CONTR-INV.
          05 WRK-ANO              PIC  9(004) VALUE ZEROS.
          05 WRK-MES              PIC  9(002) VALUE ZEROS.
          05 WRK-DIA              PIC  9(002) VALUE ZEROS.
       
       01 WRK-CABEC.
          05 WRK-CABEC-ARQSAI01.
             07 FILLER               PIC  X(061) VALUE
                'CPF DO CLIENTE;DATA ULTIMO DEPOSITO;'
                'VALOR TOTAL DOS DEPOSITOS'.             
           
      *----------------------------------------------------------------
       01 FILLER                  PIC  X(050) VALUE
             'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.
          05 WRK-FS-ARQENT01      PIC  X(002) VALUE SPACES.
             88 WRK-FS-ENT01-OK               VALUE '00'.
             88 WRK-FS-ENT01-FIM              VALUE '10'.

      *
       01 WRK-FS-ARQSAI01         PIC  X(002) VALUE SPACES.
          88 WRK-FS-SAI01-OK                  VALUE '00'.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE ENTRADA E SAIDA

           COPY ENT02103.
           COPY SAI02103.

      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'EXER0102 - FIM DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
      *----------------------------------------------------------------
      *
           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL WRK-FS-ENT01-FIM
      *
           PERFORM 9900-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *    
           SET WRK-CN-OPEN                    TO TRUE
           OPEN INPUT ARQENT01
                OUTPUT ARQSAI01
      *
           IF (WRK-FS-ENT01-OK  EQUAL '00')
           AND (WRK-FS-ARQSAI01 EQUAL '00')
              MOVE FUNCTION CURRENT-DATE(1:8) TO WRK-DATA-CORRENTE  
           ELSE
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 
           
           PERFORM 3800-LER-DEPOSITO
           
           IF WRK-FS-ENT01-FIM
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERAÇÃO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: VAZIO                               *'
              DISPLAY '* ARQUIVO: ENT01103                            *'
              DISPLAY '* FILE-STATUS:' WRK-FS-ARQENT01 '              *'
              DISPLAY '* 'WRK-PROGRAMA'  CANCELADO                    *'
              DISPLAY '************************************************'
              PERFORM 9900-FINALIZAR 
           END-IF 
           
           SET WRK-CN-WRITE                   TO TRUE
           SET WRK-CN-ARQSAI01                TO TRUE

           WRITE FD-ARQSAI01 FROM WRK-CABEC.
           IF NOT WRK-FS-SAI01-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF      

           MOVE ARQENT01-CPF                  TO WRK-CPF-ANT

           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      * CONTROLE DE PROCESSAMENTO ATE O FIM DO ARQUIVO DE ENTRADA      *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*
           PERFORM 3100-TRATA-DEPOSITO
              UNTIL (ARQENT01-CPF NOT EQUAL WRK-CPF-ANT )
              OR    (WRK-FS-ENT01-FIM)
           
           PERFORM 3900-GRAVAR-SAIDA 

           IF NOT WRK-FS-ENT01-FIM 
                  MOVE ZEROS              TO ACU-VALOR-DEP 
                  MOVE ARQENT01-CPF       TO WRK-CPF-ANT
                  MOVE WRK-DATA-CONTR-INV TO WRK-DATA-ANT
                                             WRK-DATA-CORRENTE
           END-IF
           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *    ACUMULAR DEPOSITOS E SALVAR DATA MAIS RECENTE               *
      *----------------------------------------------------------------*
       3100-TRATA-DEPOSITO SECTION.
      *----------------------------------------------------------------*
      *                                                                *
           IF WRK-DATA-LIDA GREATER WRK-DATA-RECENTE 
              MOVE WRK-DATA-LIDA  TO WRK-DATA-RECENTE
           END-IF 

           COMPUTE ACU-VALOR-DEP = ACU-VALOR-DEP + ARQENT01-VAL-DEPOS 

           PERFORM 3800-LER-DEPOSITO
           .
      *----------------------------------------------------------------*
       3100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA DE LEITURA DO ARQUIVO ARQENT01
      *----------------------------------------------------------------*
       3800-LER-DEPOSITO SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                     ARQSAI01-REGISTRO 
           SET WRK-CN-OPEN                TO TRUE
           SET WRK-CN-ARQENT01            TO TRUE

           READ ARQENT01 INTO ARQENT01-REGISTRO.
      *
           IF  (WRK-FS-ARQENT01  EQUAL '00')
           OR  (WRK-FS-ARQENT01 EQUAL '10')
               IF WRK-FS-ARQENT01 EQUAL '00'
                 ADD 1 TO ACU-LIDOS-ARQENT01 
               ELSE 
                 NEXT SENTENCE  
           ELSE
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF.

      *
      *----------------------------------------------------------------*
       3800-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3900-GRAVAR-SAIDA SECTION.
      *----------------------------------------------------------------*
           MOVE WRK-CPF-ANT        TO ARQSAI01-CPF-CLI 
           MOVE ARQENT01-DIG-CPF   TO ARQSAI01-COD-DIG
           MOVE ARQENT01-DAT-DEPOS TO ARQSAI01-DAT-ULTD
           MOVE ACU-VALOR-DEP      TO ARQSAI01-VAL-TLD 

           MOVE '.'                TO ARQSAI01-DAT-ULTD (3:1)
                                      ARQSAI01-DAT-ULTD (6:1)
           
           SET WRK-CN-WRITE        TO TRUE 
           SET WRK-CN-ARQENT01     TO TRUE

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO.

           IF NOT WRK-FS-SAI01-OK 
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF 

           COMPUTE ACU-GRAVA-ARQSAI01 = ACU-GRAVA-ARQSAI01 + 1

           INITIALIZE                  ARQSAI01-REGISTRO 
           .
           
      *----------------------------------------------------------------*
       3900-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
       9100-ERROS-ARQUIVOS SECTION .
      *----------------------------------------------------------------*
           IF NOT WRK-FS-ENT01-OK AND WRK-FS-ARQSAI01
              DISPLAY '************************************************'
              DISPLAY '*       ERRO EM OPERACAO COM ARQUIVOS          *'
              DISPLAY '* COMANDO: 'WRK-COMANDO'                       *'
              DISPLAY '* ARQUIVO: 'WRK-ARQUIVO'                       *'
              DISPLAY '* FILE-STATUS ENT:' WRK-FS-ARQENT01           '*'
              DISPLAY '* FILE-STATUS SAI:' WRK-FS-ARQSAI01           '*'
              DISPLAY '* 'WRK-PROGRAMA'  CANCELADO                    *'
              DISPLAY '************************************************'
           END-IF

           DISPLAY '***************************************************'
           DISPLAY '* QTDE DE REGISTROS LIDOS 'ACU-LIDOS-ARQENT01'     '
           DISPLAY '* QTDE DE REGISTROS GRAVADOS ' ACU-GRAVA-ARQSAI01' '
           DISPLAY '* 'WRK-PROGRAMA' - FIM DO PROGRAMA.                '
           DISPLAY '***************************************************'

           PERFORM 9900-FINALIZAR.
      *----------------------------------------------------------------*
       9100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
       9900-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           CLOSE ARQENT01
           CLOSE ARQSAI01
           
           IF WRK-FS-ENT01-OK AND WRK-FS-ARQSAI01 
              DISPLAY '************************************************'
              DISPLAY '* QTDE DE REGISTROS LIDOS 'ACU-LIDOS-ARQENT01'  '
              DISPLAY '* QTDE DE REGISTROS GRAVADOS'ACU-GRAVA-ARQSAI01''
              DISPLAY '* 'WRK-PROGRAMA' - FIM DO PROGRAMA.             '
              DISPLAY '************************************************'
           STOP RUN.

          END PROGRAM EXER0102.
      *----------------------------------------------------------------*