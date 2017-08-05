      $SET ANS85 MF ALIGN(4)
      ******************************************************************
       IDENTIFICATION                  DIVISION.
      ******************************************************************
       PROGRAM-ID. TESTE.
       AUTHOR.       NEUCLAIR J. ANGELE JR.
       DATE-WRITTEN. 21 FEV 2011.
       DATE-COMPILED.
      ******************************************************************
      *REMARKS.                                                        *
      *    *-----------------------------------------------------------*
      *    *#NOME     : TESTE                                          *
      *    *-----------------------------------------------------------*
      *    *#TIPO     : COBOL MICRO FOCUS                              *
      *    *-----------------------------------------------------------*
      *    *#ANALISTA : NEUCLAIR J. ANGELE JR.                         *
      *    *-----------------------------------------------------------*
      *    *#FUNCAO   : TESTAR FUNCOES DA SCREEN SECTION               *
      *    *-----------------------------------------------------------*
      *    * VERSAO 01    -    FABRICA DE SOFTWARE    -    21.02.2011  *
      *    *-----------------------------------------------------------*
      ******************************************************************
      *
      ******************************************************************
       ENVIRONMENT                     DIVISION.
      ******************************************************************
      *
      ******************************************************************
       CONFIGURATION                   SECTION.
      ******************************************************************
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
      ******************************************************************
       INPUT-OUTPUT                   SECTION.
      ******************************************************************
      *
           SELECT ARQUIVO ASSIGN TO DISK
                  ORGANIZATION   IS INDEXED
                  ACCESS  MODE   IS SEQUENTIAL
                  RECORD  KEY    IS FD-CHAVE
                  FILE    STATUS IS WS-FS-ARQUIVO.
      *
      ******************************************************************
       DATA                            DIVISION.
      ******************************************************************
      *
      ******************************************************************
       FILE                            SECTION.
      ******************************************************************
      *
       FD  ARQUIVO
           LABEL     RECORD IS STANDARD
           VALUE OF FILE-ID IS 'SPRINT'.
       01  FD-REG-RELATO.
           03  FD-CHAVE            PIC     X(08)         VALUE ZEROS.
           03  FD-RELATO           PIC     X(80)         VALUE SPACES.
      *
      ******************************************************************
       WORKING-STORAGE                 SECTION.
      ******************************************************************
      *
       01  WS-FS-ARQUIVO           PIC     X(02)         VALUE SPACES.
      *
       01  WS-OPCAO                PIC     X(02)         VALUE SPACES.
       01  WS-TECLA                PIC     X(02)         VALUE SPACES.
       01  WS-POSICAO              PIC     9(04)         VALUE ZEROS.
       01  WS-PRIMEIRA             PIC     9(08)         VALUE ZEROS.
       01  WS-ULTIMA               PIC     9(08)         VALUE ZEROS.
      *
       01  WS-TAB.
           03  WS-TB-ARQUIVO OCCURS 25 TIMES INDEXED BY I01-WSTAB.
               05  WS-TB-LINHA     PIC     9(08).
               05  WS-TB-RELATO    PIC     X(80).
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
      *
      ******************************************************************
       RTPRINCIPAL                     SECTION.
      ******************************************************************
      *
           PERFORM RTINICIO.
      *
           PERFORM RTPROCESSA.
      *
           PERFORM RTFINALIZA.
      *
       RTPRINCIPALX.                   EXIT.
      *
      ******************************************************************
       RTINICIO                        SECTION.
      ******************************************************************
      *
           SET I01-WSTAB               TO 1.
      *
           OPEN I-O ARQUIVO.
      *
           IF WS-FS-ARQUIVO            NOT EQUAL ZEROS
               DISPLAY 'ERRO ABERTURA = ' WS-FS-ARQUIVO
               STOP RUN
           END-IF.
      *
           PERFORM RTLEARQUIVO.
      *
           IF WS-FS-ARQUIVO            EQUAL '10'
               DISPLAY 'ARQUIVO VAZIO = ' WS-FS-ARQUIVO
           END-IF.
      *
       RTINICIOX.                      EXIT.
      *
      ******************************************************************
       RTPROCESSA                      SECTION.
      ******************************************************************
      *
       RTREESCREVE.
      *
           PERFORM RTLEARQUIVO         UNTIL WS-FS-ARQUIVO EQUAL '10' OR
                                             I01-WSTAB   GREATER  25.
      *
           PERFORM RTMONTA.
      *
       RTOPCAO.
           ACCEPT  WS-OPCAO            AT 2580 WITH AUTO UPDATE.
           ACCEPT  WS-TECLA            FROM ESCAPE KEY.
      *
           IF WS-FS-ARQUIVO            EQUAL '23'
               CONTINUE
           ELSE
               MOVE  WS-TB-LINHA(1)    TO FD-CHAVE
               PERFORM RTLIMPATAB
               SET     I01-WSTAB       TO 1
           END-IF.
      *
           EVALUATE WS-TECLA
               WHEN '00'
                   START ARQUIVO       KEY IS GREATER FD-CHAVE
                   IF WS-FS-ARQUIVO    EQUAL '23'
                       GO              TO RTOPCAO
                   END-IF
                   GO                  TO RTREESCREVE
               WHEN '01'
                   CONTINUE
               WHEN '99'
                   START ARQUIVO       KEY IS LESS    FD-CHAVE
                   IF WS-FS-ARQUIVO    EQUAL '23'
                       GO              TO RTOPCAO
                   END-IF
                   GO                  TO RTREESCREVE
           END-EVALUATE.
      *
           PERFORM RTFECHA.
      *
       RTPROCESSAX.                    EXIT.
      *
      ******************************************************************
       RTLEARQUIVO                     SECTION.
      ******************************************************************
      *
           READ ARQUIVO.
      *
           EVALUATE WS-FS-ARQUIVO
               WHEN ZEROS
                   MOVE FD-REG-RELATO  TO WS-TB-ARQUIVO(I01-WSTAB)
                   SET I01-WSTAB       UP BY 1
               WHEN '10'
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'ERRO DE LEITURA = ' WS-FS-ARQUIVO
                   PERFORM RTFECHA
                   PERFORM RTFINALIZA
           END-EVALUATE.
      *
       RTLEARQUIVOX.                   EXIT.
      *
      ******************************************************************
       RTMONTA                         SECTION.
      ******************************************************************
      *
           DISPLAY ERASE.
      *
           SET     I01-WSTAB           TO 1.
      *
           MOVE    0101                TO WS-POSICAO.
      *
           PERFORM                     UNTIL I01-WSTAB EQUAL 25
               DISPLAY WS-TB-RELATO(I01-WSTAB)
                                       AT WS-POSICAO
               SET I01-WSTAB           UP BY 1
               ADD 0100                TO WS-POSICAO
           END-PERFORM.
      *
       RTMONTAX.                       EXIT.
      *
      ******************************************************************
       RTLIMPATAB                      SECTION.
      ******************************************************************
      *
           SET     I01-WSTAB           TO 1.
      *
           PERFORM                     UNTIL I01-WSTAB EQUAL 25
               MOVE ZEROS              TO WS-TB-LINHA(I01-WSTAB)
               MOVE SPACES             TO WS-TB-RELATO(I01-WSTAB)
               SET I01-WSTAB           UP BY 1
           END-PERFORM.
      *
       RTLIMPATABX.                    EXIT.
      *
      ******************************************************************
       RTFECHA                         SECTION.
      ******************************************************************
      *
           CLOSE ARQUIVO.
      *
           IF WS-FS-ARQUIVO            NOT EQUAL ZEROS
               DISPLAY 'ERRO NO FECHAMENTO = ' WS-FS-ARQUIVO
           END-IF.
      *
       RTFECHAX.                       EXIT.
      *
      ******************************************************************
       RTFINALIZA                      SECTION.
      ******************************************************************
      *
           STOP RUN.
      *
       RTFINALIZAX.                    EXIT.
      ******************************************************************
      *                         FIM DO PROGRAMA                        *
      ******************************************************************
