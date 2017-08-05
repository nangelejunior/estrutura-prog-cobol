      ******************************************************************MODEL
       IDENTIFICATION                  DIVISION.                        MODEL
      ******************************************************************MODEL
       PROGRAM-ID. MODEL01.                                             MODEL01
       AUTHOR.       FABRICA DE SOFTWARE.                               MODEL 
       DATE-WRITTEN. DATA DE ESCRITA.                                   MODEL
       DATE-COMPILED.                                                   MODEL
      ******************************************************************MODEL
      *REMARKS.                                                         MODEL
      *     *----------------------------------------------------------*MODEL
      *     *#NOME     : NOME DO PROGRAMA                              *MODEL
      *     *----------------------------------------------------------*MODEL
      *     *#TIPO     : TIPO DO PROGRAMA                              *MODEL
      *     *----------------------------------------------------------*MODEL
      *     *#ANALISTA : NOME DO ANALISTA                              *MODEL
      *     *----------------------------------------------------------*MODEL
      *     *#FUNCAO   : FUNCAO DO PROGRAMA                            *MODEL
      *     *----------------------------------------------------------*MODEL
      *     * VERSAO       - FABRICA DE SOFTWARE      -     01.01.1900 *MODEL
      *     *              - DESCRICAO DA VERSAO                       *MODEL
      *     *----------------------------------------------------------*MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       ENVIRONMENT                     DIVISION.                        MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       CONFIGURATION                   SECTION.                         MODEL
      ******************************************************************MODEL
       SPECIAL-NAMES.                                                   MODEL
           DECIMAL-POINT               IS COMMA.                        MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       INPUT-OUTPUT                    SECTION.                         MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
       FILE-CONTROL.                                                    MODEL
           SELECT EARQUIVO ASSIGN      TO SYS010                        MODEL
                  FILE     STATUS      IS WS-FS-SYS010.                 MODEL
      *                                                                 MODEL
           SELECT SARQUIVO ASSIGN      TO SYS020                        MODEL
                  FILE     STATUS      IS WS-FS-SYS020.                 MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       DATA                            DIVISION.                        MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       FILE                            SECTION.                         MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
       FD  EARQUIVO                                                     MODEL
           RECORDING MODE              IS F                             MODEL
           LABEL     RECORD            IS STANDARD                      MODEL
           BLOCK     CONTAINS          0  RECORDS.                      MODEL
       01  FD-REG-EARQUIVO.                                             MODEL
           03  FILLER              PIC     X(200).                      MODEL
      *                                                                 MODEL
       FD  SARQUIVO                                                     MODEL
           RECORDING MODE              IS F                             MODEL
           LABEL     RECORD            IS STANDARD                      MODEL
           BLOCK     CONTAINS          0  RECORDS.                      MODEL
       01  FD-REG-SARQUIVO.                                             MODEL
           03  FILLER              PIC     X(200).                      MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       WORKING-STORAGE                 SECTION.                         MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       77  FILLER                  PIC     X(32)         VALUE          MODEL
           'III WORKING-STORAGE SECTION III'.                           MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    AREA DE FILE STATUS                                         *MODEL
      ******************************************************************MODEL
       01  WS-FS-SYS010            PIC     X(02)         VALUE SPACES.  MODEL
       01  WS-FS-SYS020            PIC     X(02)         VALUE SPACES.  MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    AREA DE CONTADORES                                          *MODEL
      ******************************************************************MODEL
       01  WS-CT-LIDOS             PIC     9(06)         VALUE ZEROS.   MODEL
       01  WS-CT-GRAVADOS          PIC     9(06)         VALUE ZEROS.   MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    AREA DE AUXILIARES                                          *MODEL
      ******************************************************************MODEL
       01  WS-MSG-SYS010           PIC     X(18)         VALUE          MODEL
           ' DO ARQUIVO SYS010'.                                        MODEL
       01  WS-MSG-SYS020           PIC     X(18)         VALUE          MODEL
           ' DO ARQUIVO SYS020'.                                        MODEL
      *                                                                 MODEL
       01  WS-DATA-MAQ             PIC     X(08)         VALUE SPACES.  MODEL
       01  WS-TIME-MAQ             PIC     X(06)         VALUE SPACES.  MODEL
      *                                                                 MODEL
       01  WS-MENSAGEM             PIC     X(40)         VALUE SPACES.  MODEL
       01  WS-DATA-DISPLAY.                                             MODEL
           03  WS-DATA-BR.                                              MODEL
               05  WS-DIA          PIC     X(02)         VALUE SPACES.  MODEL
               05  FILLER          PIC     X             VALUE '/'.     MODEL
               05  WS-MES          PIC     X(02)         VALUE SPACES.  MODEL
               05  FILLER          PIC     X             VALUE '/'.     MODEL
               05  WS-ANO          PIC     X(04)         VALUE SPACES.  MODEL
               05  FILLER          PIC     X             VALUE ' '.     MODEL
           03  WS-HORA-BR.                                              MODEL
               05  WS-HORA         PIC     X(02)         VALUE ZEROS.   MODEL
               05  FILLER          PIC     X             VALUE ':'.     MODEL
               05  WS-MINUTO       PIC     X(02)         VALUE ZEROS.   MODEL
               05  FILLER          PIC     X             VALUE ':'.     MODEL
               05  WS-SEGUNDO      PIC     X(02)         VALUE ZEROS.   MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    AREA DE LINKAGE                                             *MODEL
      ******************************************************************MODEL
      *--- PARAMETRO DE ENTRADA                                        *MODEL
       01  WS-ENTRADA.                                                  MODEL
           03  WS-PAR-ENTRADA      PIC     9(005)        VALUE ZEROS.   MODEL
      *                                                                 MODEL
      *--- PARAMETRO DE SAIDA                                          *MODEL
       01  WS-SAIDA.                                                    MODEL
           03  WS-PAR-SAIDA        PIC     9(010)        VALUE ZEROS.   MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    AREA DE COPY BOOKS                                          *MODEL
      ******************************************************************MODEL
       COPY ARQUIVOE.                                                   MODEL
      *                                                                 MODEL
       COPY ARQUIVOS.                                                   MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    AREA DE DB2                                                 *MODEL
      ******************************************************************MODEL
       EXEC SQL                                                         MODEL
           INCLUDE SQLCA                                                MODEL
       END-EXEC.                                                        MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       77  FILLER                  PIC     X(32)         VALUE          MODEL
           'FFF FIM DA WORKING-STORAGE SECTION FFF'.                    MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       LINKAGE                         SECTION.                         MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
       01  LK-ENTRADA              PIC     9(005).                      MODEL
       01  LK-SAIDA                PIC     9(010).                      MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
       PROCEDURE                       DIVISION USING LK-PAR-ENTRADA    MODEL
                                                      LK-PAR-SAIDA.     MODEL
      ******************************************************************MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    ROTINA PRINCIPAL                                            *MODEL
      ******************************************************************MODEL
       RTPRINCIPAL                     SECTION.                         MODEL
      *                                                                 MODEL
           PERFORM RTINICIALIZA.                                        MODEL
      *                                                                 MODEL
           PERFORM RTPROCESSA.                                          MODEL
      *                                                                 MODEL
           PERFORM RTFINALIZA.                                          MODEL
      *                                                                 MODEL
       RTPRINCIPALX.                   EXIT.                            MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    ROTINA DE INICIALIZACAO                                     *MODEL
      ******************************************************************MODEL
       RTINICIALIZA                    SECTION.                         MODEL
      *                                                                 MODEL
           INITIALIZE ARQUIVOE                                          MODEL
                      ARQUIVOS                                          MODEL
                      WS-ENTRADA                                        MODEL
                      WS-SAIDA.                                         MODEL
      *                                                                 MODEL
           DISPLAY '*************************************************'. MODEL
           MOVE '#MODEL.900I - INICIO DO PROCESSAMENTO '                MODEL
                                       TO WS-MENSAGEM.                  MODEL
      *                                                                 MODEL
           ACCEPT WS-DATA-MAQ          FROM DATE.                       MODEL
           ACCEPT WS-HORA-MAQ          FROM TIME.                       MODEL
      *                                                                 MODEL
           MOVE WS-DATA-MAQ            TO WS-DATA-BR.                   MODEL
           MOVE WS-HORA-MAQ            TO WS-HORA-BR.                   MODEL
      *                                                                 MODEL
           DISPLAY WS-MENSAGEM.                                         MODEL
           DISPLAY WS-DATA-DISPLAY.                                     MODEL
           DISPLAY '*************************************************'. MODEL
      *                                                                 MODEL
           MOVE LK-ENTRADA             TO WS-ENTRADA.                   MODEL
           MOVE LK-SAIDA               TO WS-SAIDA.                     MODEL
      *                                                                 MODEL
           OPEN INPUT  EARQUIVO                                         MODEL
                OUTPUT SARQUIVO.                                        MODEL
      *                                                                 MODEL
           IF WS-FS-SYS010             NOT EQUAL ZEROS                  MODEL
               DISPLAY '#MODEL.901I - ERRO NA ABERTURA' WS-MSG-SYS010   MODEL
               DISPLAY '#MODEL.901I - FILE STATUS = '   WS-FS-SYS010    MODEL
               PERFORM RTFINALIZA                                       MODEL
           END-IF.                                                      MODEL
      *                                                                 MODEL
           IF WS-FS-SYS020             NOT EQUAL ZEROS                  MODEL
               DISPLAY '#MODEL.902I - ERRO NA ABERTURA' WS-MSG-SYS020   MODEL
               DISPLAY '#MODEL.902I - FILE STATUS = '   WS-FS-SYS020    MODEL
               PERFORM RTFINALIZA                                       MODEL
           END-IF.                                                      MODEL
      *                                                                 MODEL
       RTINICIALIZAX.                  EXIT.                            MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    ROTINA DE INICIALIZACAO                                     *MODEL
      ******************************************************************MODEL
       RTPROCESSA                      SECTION.                         MODEL
      *                                                                 MODEL
      *--- LOGICA DO PROGRAMA                                           MODEL
      *                                                                 MODEL
       RTPROCESSAX.                    EXIT.                            MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    ROTINA DE FECHAMENTO DOS ARQUIVOS                           *MODEL
      ******************************************************************MODEL
       RTFECHA                         SECTION.                         MODEL
      *                                                                 MODEL
           CLOSE EARQUIVO                                               MODEL
                 SARQUIVO.                                              MODEL
      *                                                                 MODEL
           IF WS-FS-SYS010             NOT EQUAL ZEROS                  MODEL
               DISPLAY '#MODEL.909I - ERRO NO FECHAMENTO' WS-MSG-SYS010 MODEL
               DISPLAY '#MODEL.909I - FILE STATUS = '     WS-FS-SYS010  MODEL
               PERFORM RTFINALIZA                                       MODEL
           END-IF.                                                      MODEL
      *                                                                 MODEL
           IF WS-FS-SYS020             NOT EQUAL ZEROS                  MODEL
               DISPLAY '#MODEL.910I - ERRO NO FECHAMENTO' WS-MSG-SYS020 MODEL
               DISPLAY '#MODEL.910I - FILE STATUS = '   WS-FS-SYS020    MODEL
               PERFORM RTFINALIZA                                       MODEL
           END-IF.                                                      MODEL
      *                                                                 MODEL
       RTFECHAX.                       EXIT.                            MODEL
      *                                                                 MODEL
      ******************************************************************MODEL
      *    ROTINA DE FINALIZACAO                                       *MODEL
      ******************************************************************MODEL
       RTFINALIZA                      SECTION.                         MODEL
      *                                                                 MODEL
           MOVE WS-SAIDA               TO LK-SAIDA.                     MODEL
      *                                                                 MODEL
           DISPLAY '*************************************************'. MODEL
           MOVE '#MODEL.999I - FIM DO PROCESSAMENTO '                   MODEL
                                       TO WS-MENSAGEM.                  MODEL
      *                                                                 MODEL
           ACCEPT WS-DATA-MAQ          FROM DATE.                       MODEL
           ACCEPT WS-HORA-MAQ          FROM TIME.                       MODEL
      *                                                                 MODEL
           MOVE WS-DATA-MAQ            TO WS-DATA-BR.                   MODEL
           MOVE WS-HORA-MAQ            TO WS-HORA-BR.                   MODEL
      *                                                                 MODEL
           DISPLAY WS-MENSAGEM.                                         MODEL
           DISPLAY WS-DATA-DISPLAY.                                     MODEL
           DISPLAY '*************************************************'. MODEL
      *                                                                 MODEL
           STOP RUN.                                                    MODEL
      *                                                                 MODEL
       RTFINALIZAX.                    EXIT.                            MODEL
      ******************************************************************MODEL
      *    FIM DO PROGRAMA                                             *MODEL
      ******************************************************************MODEL