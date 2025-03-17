# TreinamentoIA

ZCLCB_DAO_CONSULTA DAO CONSULTA->


ZIFCB_DAO_CONSULTA~M_SELECT_BKPF_CB

  METHOD zifcb_dao_consulta~m_select_bkpf_cb.

    DATA: vl_campo             TYPE string,
          wal_estrutura_campos TYPE zstbcb_bkpf,
          itl_bkpf             TYPE zttbcb_bkpf.

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    DATA(wal_movimento) = o_dao_movto->m_get_movimento_single( ).

    CHECK wal_movimento IS NOT INITIAL.

    SELECT (vl_campo)
    FROM bkpf
    INTO TABLE itl_bkpf
    WHERE
      bukrs = '5501'
      AND belnr = wal_movimento-cb_doc_lancamento.

    IF sy-subrc IS INITIAL  .

      o_model->m_set_bkpf( itl_bkpf ).

    ENDIF.

  ENDMETHOD.

--



ZIFCB_DAO_CONSULTA~M_SET_MODEL

  METHOD zifcb_dao_consulta~m_set_model.

    o_model = i_model_consulta.

    r_retorno = abap_true.

  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_CONSULTAR_SALDO

  METHOD zifcb_dao_consulta~m_consultar_saldo.

    m_select_bsid( ).

    r_retorno = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_CONSULTAR_EXTRATO

  METHOD zifcb_dao_consulta~m_consultar_extrato.

    IF v_app_extrato IS NOT INITIAL.

      m_consultar_movto( ).

      m_select_bseg( ).

      m_select_vbak( ).

      m_select_bkpf_st( ).

      m_select_historico( ).

    ELSE.

      m_select_bkpf( ).

      m_select_bseg( ).

      m_consultar_movto( ).

      m_select_vbak( ).

    ENDIF.

    r_retorno = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_CONSULTA_ZFPR

  METHOD zifcb_dao_consulta~m_consulta_zfpr.

    m_select_ztcb_controle_zf( ).

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_SELECT_BSID

  METHOD zifcb_dao_consulta~m_select_bsid.

    DATA: wal_estrutura_campos TYPE zstbcb_bsid.

    DATA: vl_campo  TYPE string.

    DATA: itl_bsid TYPE zttbcb_bsid.

    DATA(itl_bseg)   = o_model->m_get_bseg( ).

    DATA(vl_empresa) = o_model->m_get_empresa( ).

    DATA(vl_cliente) = o_model->m_get_cliente( ).

    DATA(rl_cliente) = o_model->m_get_cliente_sede( ).

    DATA(rl_periodo) = o_model->m_get_periodo( ).

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    IF rl_cliente IS NOT INITIAL.

      IF i_bsid IS INITIAL.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr IN rl_cliente
        AND   budat IN rl_periodo
        AND   zlspr = space
        AND   blart = 'CB'.

        IF v_app_extrato IS NOT INITIAL AND itl_bsid IS NOT INITIAL.

          SELECT (vl_campo)
          FROM bsid
          INTO TABLE itl_bsid
          FOR ALL ENTRIES IN itl_bsid
          WHERE bukrs = vl_empresa
          AND kunnr = itl_bsid-kunnr
          AND belnr = itl_bsid-belnr
          AND zlspr = space
          AND blart = 'CB'.

        ENDIF.

        o_model->m_set_bsid( itl_bsid ).

      ELSE.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr IN rl_cliente
        AND   budat IN rl_periodo
        AND   zlspr = space
        AND   blart = 'CB'.

        o_model->m_set_bsid( itl_bsid ).

      ENDIF.

    ELSE.

      IF i_bsid IS INITIAL.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr = vl_cliente
        AND   zlspr = space
        AND   blart = 'CB'.

        IF v_app_extrato IS NOT INITIAL AND itl_bsid IS NOT INITIAL.

          SELECT (vl_campo)
          FROM bsid
          INTO TABLE itl_bsid
          FOR ALL ENTRIES IN itl_bsid
          WHERE bukrs = vl_empresa
          AND kunnr = itl_bsid-kunnr
          AND belnr = itl_bsid-belnr
          AND zlspr = space
          AND blart = 'CB'.

        ENDIF.

        o_model->m_set_bsid( itl_bsid ).

      ELSE.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr = vl_cliente
        AND   budat IN rl_periodo
        AND   zlspr = space
        AND   blart = 'CB'.

        o_model->m_set_bsid( itl_bsid ).

      ENDIF.

    ENDIF.

    r_retorno = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_SELECT_BSID

  METHOD zifcb_dao_consulta~m_select_bsid.

    DATA: wal_estrutura_campos TYPE zstbcb_bsid.

    DATA: vl_campo  TYPE string.

    DATA: itl_bsid TYPE zttbcb_bsid.

    DATA(itl_bseg)   = o_model->m_get_bseg( ).

    DATA(vl_empresa) = o_model->m_get_empresa( ).

    DATA(vl_cliente) = o_model->m_get_cliente( ).

    DATA(rl_cliente) = o_model->m_get_cliente_sede( ).

    DATA(rl_periodo) = o_model->m_get_periodo( ).

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    IF rl_cliente IS NOT INITIAL.

      IF i_bsid IS INITIAL.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr IN rl_cliente
        AND   budat IN rl_periodo
        AND   zlspr = space
        AND   blart = 'CB'.

        IF v_app_extrato IS NOT INITIAL AND itl_bsid IS NOT INITIAL.

          SELECT (vl_campo)
          FROM bsid
          INTO TABLE itl_bsid
          FOR ALL ENTRIES IN itl_bsid
          WHERE bukrs = vl_empresa
          AND kunnr = itl_bsid-kunnr
          AND belnr = itl_bsid-belnr
          AND zlspr = space
          AND blart = 'CB'.

        ENDIF.

        o_model->m_set_bsid( itl_bsid ).

      ELSE.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr IN rl_cliente
        AND   budat IN rl_periodo
        AND   zlspr = space
        AND   blart = 'CB'.

        o_model->m_set_bsid( itl_bsid ).

      ENDIF.

    ELSE.

      IF i_bsid IS INITIAL.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr = vl_cliente
        AND   zlspr = space
        AND   blart = 'CB'.

        IF v_app_extrato IS NOT INITIAL AND itl_bsid IS NOT INITIAL.

          SELECT (vl_campo)
          FROM bsid
          INTO TABLE itl_bsid
          FOR ALL ENTRIES IN itl_bsid
          WHERE bukrs = vl_empresa
          AND kunnr = itl_bsid-kunnr
          AND belnr = itl_bsid-belnr
          AND zlspr = space
          AND blart = 'CB'.

        ENDIF.

        o_model->m_set_bsid( itl_bsid ).

      ELSE.

        SELECT (vl_campo)
        FROM bsid
        INTO TABLE itl_bsid
        WHERE bukrs = vl_empresa
        AND   kunnr = vl_cliente
        AND   budat IN rl_periodo
        AND   zlspr = space
        AND   blart = 'CB'.

        o_model->m_set_bsid( itl_bsid ).

      ENDIF.

    ENDIF.

    r_retorno = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_SELECT_BSEG

  METHOD zifcb_dao_consulta~m_select_bseg.

    DATA: wal_estrutura_campos TYPE zstbcb_bseg,
          wal_cliente          TYPE range_kunnr_wa.

    DATA: rl_cliente TYPE range_kunnr_tab.

    DATA: itl_bseg TYPE zttbcb_bseg.

    DATA: vl_campo TYPE string.

    DATA(itl_bkpf)   = o_model->m_get_bkpf( ).

    DATA(vl_cliente) = o_model->m_get_cliente( ).

    DATA(rl_cliente_sede) = o_model->m_get_cliente_sede( ).

    IF vl_cliente IS NOT INITIAL.

      wal_cliente-sign   = 'I'.
      wal_cliente-option = 'EQ'.
      wal_cliente-low    = vl_cliente.
      APPEND wal_cliente TO rl_cliente.

    ELSEIF rl_cliente_sede IS NOT INITIAL.

      rl_cliente = rl_cliente_sede.

    ENDIF.

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    IF v_app_extrato IS NOT INITIAL.

      DATA(itl_movimento) = o_dao_movto->m_get_movimento_table( ).

      itl_movimento  =  o_dao_movto->m_select_mov_complementares( EXPORTING i_movimento = itl_movimento ).

      CHECK itl_movimento IS NOT INITIAL.

      SELECT (vl_campo)
          FROM bseg
          INTO TABLE itl_bseg
          FOR ALL ENTRIES IN itl_movimento
          WHERE bukrs = itl_movimento-cb_empresa
            AND belnr = itl_movimento-cb_doc_lancamento
            AND gjahr = itl_movimento-cb_ano_lancamento
            AND kunnr = itl_movimento-cb_cliente.

    ELSE.
      CHECK itl_bkpf IS NOT INITIAL.

      SELECT (vl_campo)
          FROM bseg
          INTO TABLE itl_bseg
          FOR ALL ENTRIES IN itl_bkpf
          WHERE bukrs = itl_bkpf-bukrs
            AND belnr = itl_bkpf-belnr
            AND gjahr = itl_bkpf-gjahr
            AND kunnr IN rl_cliente.

    ENDIF.
    IF sy-subrc IS INITIAL.

      o_model->m_set_bseg( itl_bseg ).

    ENDIF.

    r_retorno = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_SELECT_BKPF

  METHOD zifcb_dao_consulta~m_select_bkpf.

    DATA: wal_estrutura_campos TYPE zstbcb_bkpf.

    DATA: itl_bkpf TYPE zttbcb_bkpf.

    DATA: vl_campo TYPE string.

    DATA(vl_empresa) = o_model->m_get_empresa( ).

    DATA(rl_data) = o_model->m_get_periodo( ).

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    DATA(itl_dockey) = o_model->m_get_dockey( ).

    IF itl_dockey IS INITIAL.

      CHECK rl_data IS NOT INITIAL.

      SELECT (vl_campo)
      FROM bkpf
      INTO TABLE itl_bkpf
      WHERE bukrs     = vl_empresa
      AND   blart     = 'CB'
      AND   bldat    IN rl_data
      AND   xreversal = abap_false.

    ELSE.

      SELECT (vl_campo)
      FROM bkpf
      INTO TABLE itl_bkpf
        FOR ALL ENTRIES IN itl_dockey
        WHERE bukrs     = itl_dockey-bukrs
        AND   belnr     = itl_dockey-belnr
        AND   gjahr     = itl_dockey-gjahr
        AND   blart     = 'CB'
        AND   xreversal = abap_false.

    ENDIF.

    IF sy-subrc IS INITIAL  .

      o_model->m_set_bkpf( itl_bkpf ).


    ENDIF.

    r_retorno = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_GET_MODEL

  method ZIFCB_DAO_CONSULTA~M_GET_MODEL.

    r_model = o_model.

  endmethod.

--

ZIFCB_DAO_CONSULTA~M_SELECT_VBAK

  METHOD zifcb_dao_consulta~m_select_vbak.

    DATA: wal_estrutura_campos TYPE zstbcb_vbak.

    DATA: vl_campo TYPE string.

    DATA: itl_vbak TYPE zttbcb_vbak.

    DATA(itl_movto) = o_dao_movto->m_get_movimento_table( ).

    LOOP AT itl_movto INTO DATA(wal_movto) .

      IF wal_movto-cb_pedido_bloqueio IS INITIAL.

        DELETE itl_movto.

      ENDIF.

    ENDLOOP.

    CHECK itl_movto IS NOT INITIAL.
    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).
    CHECK vl_campo IS NOT INITIAL.

    SELECT (vl_campo)
    INTO TABLE itl_vbak
    FROM vbak
    FOR ALL ENTRIES IN itl_movto
    WHERE vbeln = itl_movto-cb_pedido_bloqueio .

    IF sy-subrc IS INITIAL.

      o_model->m_set_vbak( itl_vbak ).

    ENDIF.

    r_return = abap_true.

  ENDMETHOD.

--

ZIFCB_DAO_CONSULTA~M_CONSULTAR_MOVTO



 METHOD zifcb_dao_consulta~m_consultar_movto.

    IF o_dao_movto IS INITIAL.
      CREATE OBJECT o_dao_movto TYPE zclcb_dao_movimento.
    ENDIF.

    IF v_app_extrato IS NOT INITIAL.

      o_dao_movto->m_create_model( ).

      o_dao_movto->m_set_model( o_model ).

      o_dao_movto->m_select_movimento_extr( ).

    ELSE.
      DATA itl_dockey TYPE zttcb_dockey.

      DATA wal_dockey TYPE zscb_dockey.

      DATA(itl_bseg) = o_model->m_get_bseg( ).

      LOOP AT itl_bseg ASSIGNING FIELD-SYMBOL(<fsl_bseg>).

        CLEAR wal_dockey.

        wal_dockey-belnr = <fsl_bseg>-belnr.
        wal_dockey-bukrs = <fsl_bseg>-bukrs.
        wal_dockey-gjahr = <fsl_bseg>-gjahr.
        wal_dockey-kunnr = <fsl_bseg>-kunnr.

        APPEND wal_dockey TO itl_dockey.

      ENDLOOP.

      o_dao_movto->m_set_dockey( itl_dockey ).
      o_dao_movto->m_select_ztcb_movimento_table( ).
    ENDIF.

    r_return = abap_true.

  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_SELECT_ZTCB_CONTROLE_ZF


  METHOD zifcb_dao_consulta~m_select_ztcb_controle_zf.

    DATA: itl_controle TYPE zttcb_controle_zf.

    DATA: wal_estrutura_campos TYPE ztcb_controle_zf.

    DATA: vl_campo TYPE string.

    DATA(vl_ov) = o_model->m_get_ov( ).

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL AND vl_ov IS NOT INITIAL.

    SELECT (vl_campo)
    FROM ztcb_controle_zf
      INTO TABLE itl_controle
      WHERE ord_zorb = vl_ov.

    IF sy-subrc IS INITIAL.
      o_model->m_set_controle_zfpr( itl_controle ).
    ENDIF.

  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_CONSULTAR_MOVTO_CB

  METHOD zifcb_dao_consulta~m_consultar_movto_cb.
    CHECK i_cb_codigo IS NOT INITIAL.

    o_dao_movto->m_set_cod_cashback( i_cb_codigo ).
    r_return = o_dao_movto->m_select_ztcb_movimento_single( ).

  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_SELECT_BKPF_CB

  METHOD zifcb_dao_consulta~m_select_bkpf_cb.

    DATA: vl_campo             TYPE string,
          wal_estrutura_campos TYPE zstbcb_bkpf,
          itl_bkpf             TYPE zttbcb_bkpf.

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    DATA(wal_movimento) = o_dao_movto->m_get_movimento_single( ).

    CHECK wal_movimento IS NOT INITIAL.

    SELECT (vl_campo)
    FROM bkpf
    INTO TABLE itl_bkpf
    WHERE
      bukrs = '5501'
      AND belnr = wal_movimento-cb_doc_lancamento.

    IF sy-subrc IS INITIAL  .

      o_model->m_set_bkpf( itl_bkpf ).

    ENDIF.

  ENDMETHOD. 

--

ZIFCB_DAO_CONSULTA~M_SELECT_BKPF_ST

  METHOD zifcb_dao_consulta~m_select_bkpf_st.

    DATA itl_movimento_st TYPE zttbcb_movimento.
    DATA: itl_bkpf TYPE zttbcb_bkpf.
    DATA: wal_estrutura_campos TYPE zstbcb_bkpf.
    DATA: vl_campo TYPE string.


    DATA(itl_movimento) = o_dao_movto->m_get_movimento_table( ).

    LOOP AT itl_movimento INTO DATA(wal_movimento).
      IF wal_movimento-cb_pedido_bloqueio CP 'ST*'.

        APPEND wal_movimento TO itl_movimento_st.

      ENDIF.
    ENDLOOP.


    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL AND itl_movimento_st IS NOT INITIAL.

    SELECT (vl_campo)
       FROM bkpf
       INTO TABLE itl_bkpf
         FOR ALL ENTRIES IN itl_movimento_st
         WHERE bukrs     = itl_movimento_st-cb_empresa
         AND   belnr     = itl_movimento_st-cb_doc_consumo
         AND   gjahr     = itl_movimento_st-cb_ano_consumo
         AND   blart     = 'CB'
         AND   xreversal = abap_false.

    IF sy-subrc IS INITIAL  .

      o_model->m_set_bkpf( itl_bkpf ).

    ENDIF.

  ENDMETHOD. 


--

ZIFCB_DAO_CONSULTA~M_SELECT_BSEG_ST_PAGO

  METHOD zifcb_dao_consulta~m_select_bseg_st_pago.

    DATA itl_movimento_st TYPE zttbcb_movimento.
    DATA: itl_bseg TYPE zttbcb_bseg.
    DATA: wal_estrutura_campos TYPE zstbcb_bseg.
    DATA: vl_campo TYPE string.

    DATA(itl_movimento) = o_dao_movto->m_get_movimento_table( ).

    LOOP AT itl_movimento INTO DATA(wal_movimento).
      IF wal_movimento-cb_pedido_bloqueio CP 'ST*'.

        APPEND wal_movimento TO itl_movimento_st.

      ENDIF.
    ENDLOOP.

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL AND itl_movimento_st IS NOT INITIAL.

    SELECT (vl_campo)
           FROM bseg
           INTO TABLE itl_bseg
           FOR ALL ENTRIES IN itl_movimento
           WHERE bukrs = itl_movimento-cb_empresa
             AND belnr = itl_movimento-cb_doc_consumo
             AND gjahr = itl_movimento-cb_ano_lancamento
             AND augbl =  itl_movimento-cb_nf_serie(10)
             AND augbl = ''
             AND bschl <> '09'
             AND kunnr = itl_movimento-cb_cliente.

    IF sy-subrc IS INITIAL  .

      o_model->m_set_bseg_st_pago( i_it_bseg =  itl_bseg ).

    ENDIF.

  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_SELECT_HISTORICO

  METHOD zifcb_dao_consulta~m_select_historico.

    DATA: itl_movimento_st TYPE zttbcb_movimento,
          itl_bkpf         TYPE zttbcb_bkpf,
          itl_historico TYPE zttcb_historico.

    DATA: wal_estrutura_campos TYPE ztcb_historico.

    DATA: vl_campo TYPE string.

    DATA(itl_movimento) = o_dao_movto->m_get_movimento_table( ).

    LOOP AT itl_movimento INTO DATA(wal_movimento).
      IF wal_movimento-cb_pedido_bloqueio CP 'ST*'.

        APPEND wal_movimento TO itl_movimento_st.

      ENDIF.
    ENDLOOP.

    DATA(vl_empresa) = o_model->m_get_empresa( ).
    DATA(vl_cliente) = o_model->m_get_cliente( ).
    DATA(vl_data)    = o_model->m_get_periodo( ).

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    SELECT (vl_campo)
          FROM ztcb_historico
          INTO TABLE itl_historico
          WHERE empresa = vl_empresa
          AND   cliente = vl_cliente.


    IF sy-subrc IS INITIAL  .

      o_model->m_set_historico( itl_historico ).

    ENDIF.

  ENDMETHOD. 

--

ZIFCB_DAO_CONSULTA~M_VALIDA_QTDE_TRANS

  METHOD zifcb_dao_consulta~m_valida_qtde_trans.

    DATA: itl_distinct_pedidos TYPE TABLE OF ztcb_movimento-cb_pedido_bloqueio,
          vl_qtd_pedidos       TYPE i.

    DATA: ol_consultar_cashback TYPE REF TO zclcb_dto_consulta.

    CREATE OBJECT ol_consultar_cashback.

    DATA(itl_param) = o_model->m_get_param( ).
    SORT itl_param BY parametro.
    READ TABLE itl_param INTO DATA(wal_param) WITH KEY parametro = 'NUM_TRANSFERENCIAS' BINARY SEARCH.

    SELECT cb_pedido_bloqueio
      INTO TABLE itl_distinct_pedidos
      FROM ztcb_movimento
      WHERE cb_cliente = i_kunnr
      AND cb_status = 'A'
      AND cb_pedido_bloqueio IN i_vbeln
      AND cb_nf_serie = ''.

    SORT itl_distinct_pedidos.
    DELETE ADJACENT DUPLICATES FROM itl_distinct_pedidos.

    vl_qtd_pedidos = lines( itl_distinct_pedidos ).

    IF vl_qtd_pedidos >= wal_param-valor.
      r_return = abap_false.

      IF wal_param-valor = 1.

        CONCATENATE TEXT-001 wal_param-valor TEXT-004 TEXT-003 INTO e_message SEPARATED BY space.

      ELSE.

        CONCATENATE TEXT-001 wal_param-valor TEXT-002 TEXT-003 INTO e_message SEPARATED BY space.

      ENDIF.

    ELSE.
      r_return = abap_true.
    ENDIF.

  ENDMETHOD. 

--

  METHOD zifcb_dao_consulta~m_checa_ztcb_bloq_simult.

    r_validacao = abap_true.

    DATA(vl_cliente) = o_model->m_get_cliente( ).

    DATA: wal_estrutura_campos TYPE ztcb_bloq_simult.
    DATA: vl_campo TYPE string.

    vl_campo = zcl_abap_utility=>m_definir_campos( wal_estrutura_campos ).

    CHECK vl_campo IS NOT INITIAL.

    CHECK vl_cliente IS NOT INITIAL.

    SELECT SINGLE (vl_campo)
      INTO wa_ztcb_bloq_simult
      FROM ztcb_bloq_simult
      WHERE cliente = vl_cliente.

    CHECK sy-subrc IS INITIAL.

    r_validacao = abap_false.

  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_BUSCA_BLOQUEIO

  METHOD zifcb_dao_consulta~m_busca_bloqueio.

    r_validacao = m_checa_ztcb_bloq_simult( ).

  ENDMETHOD. 

--

ZIFCB_DAO_CONSULTA~M_BLOQ_OV_SIMULT

  method ZIFCB_DAO_CONSULTA~M_BLOQ_OV_SIMULT.

    r_validacao = m_modify_ztcb_bloq_simult( i_bloq_sim ).

  endmethod. 

--

ZIFCB_DAO_CONSULTA~M_MODIFY_ZTCB_BLOQ_SIMULT

  METHOD zifcb_dao_consulta~m_modify_ztcb_bloq_simult.

    r_validacao = abap_true.

    DATA(vl_cliente) = o_model->m_get_cliente( ).

    CHECK i_bloq_sim IS NOT INITIAL.

    MODIFY ztcb_bloq_simult FROM i_bloq_sim.

    CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CHECK sy-subrc IS INITIAL.

    r_validacao = abap_false.


  ENDMETHOD.



--



ZIFCB_DAO_CONSULTA~M_ELIMINAR_ZTCB_BLOQ_SIMULT

  METHOD zifcb_dao_consulta~m_eliminar_ztcb_bloq_simult.

    CHECK i_bloq_sim IS NOT INITIAL.

    DELETE ztcb_bloq_simult FROM i_bloq_sim.

    CHECK sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    r_validacao = abap_true.

  ENDMETHOD. 

--

ZIFCB_DAO_CONSULTA~M_DESBLOQ_CLIENT_SIMULT

  METHOD zifcb_dao_consulta~m_desbloq_client_simult.

    r_validacao = m_eliminar_ztcb_bloq_simult( i_bloq_sim ).

  ENDMETHOD. 

--

ZIFCB_DAO_CONSULTA~M_GET_ZTCB_BLOQ_SIMULT

  METHOD zifcb_dao_consulta~m_get_ztcb_bloq_simult.

    r_ztcb_bloq_simult = wa_ztcb_bloq_simult.

  ENDMETHOD. 

--

CONSTRUCTOR

  METHOD constructor.

    IF i_model IS INITIAL.
      CREATE OBJECT: o_model.
    ELSE.
      o_model = i_model.
    ENDIF.

    IF i_movto IS INITIAL.
      CREATE OBJECT: o_dao_movto.
    ELSE.
      o_dao_movto = i_movto.
    ENDIF.

    IF NOT i_app_extrato IS INITIAL.
      v_app_extrato  = i_app_extrato .
    ENDIF.

  ENDMETHOD.





ZCLCB_DTO_CONSULTA->

CONSTRUCTOR

  METHOD constructor.

    DATA: rl_objeto TYPE zrttabap_objeto,
          rl_param  TYPE zrttabap_param_name,
          itl_valor TYPE zttbabap_ztbabap0003.
    CLEAR v_app_extrato.

    rl_objeto = VALUE #( ( sign   = 'I' option = 'EQ' low    = 'CASHBACK' ) ).

    rl_param = VALUE #( ( sign   = 'I' option = 'EQ' low    = 'FM_PERFORMANCE_EXT' ) ).

    zcl_abap_utility=>m_buscar_param_obj( EXPORTING i_objeto = rl_objeto i_param = rl_param IMPORTING e_valor =  itl_valor ).
    READ TABLE itl_valor WITH KEY valor = sy-cprog INTO DATA(wal_valor).
    IF sy-subrc = 0.
      v_app_extrato =  wal_valor-valor.
    ENDIF.

    IF i_dao IS NOT INITIAL.

      o_dao_consulta = i_dao.

    ELSE.

      CREATE OBJECT o_dao_consulta TYPE zclcb_dao_consulta EXPORTING i_app_extrato = v_app_extrato.

    ENDIF.

    IF i_dao_movto IS NOT INITIAL.

      o_dao_movto = i_dao_movto.

    ELSE.

      CREATE OBJECT o_dao_movto TYPE zclcb_dao_movimento.

    ENDIF.

    SELECT tp_cashback idioma descricao
        FROM ztcb_tp_cashback
        INTO TABLE it_tipo_cashback
        WHERE idioma = sy-langu.

  ENDMETHOD.

--

M_SET_MODEL

  METHOD m_set_model.

    o_dao_consulta->m_set_model( i_model_consulta ).

    r_retorno = abap_true.

  ENDMETHOD.

--

M_CONSULTAR_CASHBACK

  METHOD m_consultar_cashback.

    o_dao_consulta->m_consultar_saldo( ).
    m_montar_saldo( ).

    IF i_tp_consulta EQ 'E'.

      o_dao_consulta->m_consultar_extrato( ).
      m_consultar_movto( ).
      m_consultar_contab_erros( ).
      m_montar_extrato( ).

    ENDIF.

    r_retorno = abap_true.

  ENDMETHOD.

--



M_MONTAR_SALDO

  METHOD m_montar_saldo.

    DATA ol_modelo TYPE REF TO zclcb_model_consulta.

    DATA: itl_bsid TYPE zttbcb_bsid.

    DATA: wal_bsid TYPE zstbcb_bsid.

    ol_modelo = o_dao_consulta->m_get_model( ).

    itl_bsid = ol_modelo->m_get_bsid( ).

    CHECK itl_bsid IS NOT INITIAL.

    LOOP AT itl_bsid INTO wal_bsid.

      v_saldo = wal_bsid-dmbtr + v_saldo.

    ENDLOOP.

    r_retorno = abap_true.

  ENDMETHOD.

--


M_GET_SALDO

  METHOD m_get_saldo.

    r_saldo = v_saldo.

  ENDMETHOD.

--

M_GET_MODEL

  METHOD m_get_model.

    r_model = o_dao_consulta->m_get_model( ).

  ENDMETHOD.

--

M_GET_EXTRATO

  METHOD m_get_extrato.

    r_extrato = it_extrato.

  ENDMETHOD.

--

M_MONTAR_EXTRATO

  METHOD m_montar_extrato.

    DATA: itl_movimento        TYPE zttbcb_movimento,
          itl_extrato_comp     TYPE zttbcb_extrato_comp,
          itl_extrato_comp_aux TYPE zttbcb_extrato_comp.

    DATA: wal_movimento    TYPE ztcb_movimento,
          wal_extrato      TYPE zstbcb_extrato,
          wal_extrato_comp TYPE zstbcb_extrato_comp,
          wal_cashback     TYPE ztcb_tp_cashback,
          wal_vbak         TYPE zstbcb_vbak,
          wal_bkpf         TYPE zstbcb_bkpf.


    DATA(ol_cashback) = NEW zclcb_model_document_post( ).

    DATA ol_modelo TYPE REF TO zclcb_model_consulta.

    ol_modelo = o_dao_consulta->m_get_model( ).

    DATA(itl_bseg)      = ol_modelo->m_get_bseg( ).
    DATA(itl_bkpf)      = ol_modelo->m_get_bkpf( ).
    DATA(itl_vbak)      = ol_modelo->m_get_vbak( ).
    DATA(itl_historico) = ol_modelo->m_get_historico( ).
    DATA(rl_periodo)    = ol_modelo->m_get_periodo( ).

    itl_movimento = o_dao_movto->m_get_movimento_table( ).

    DATA(itl_extrato_contab_erros) = o_dao_movto->m_get_extrato_contab_cb( )..

    CHECK itl_bseg IS NOT INITIAL.

    SORT: itl_bseg BY bukrs ASCENDING
                      belnr ASCENDING
                      gjahr ASCENDING,

          itl_bkpf BY bukrs ASCENDING
                      belnr ASCENDING
                      gjahr ASCENDING,

          itl_movimento BY cb_empresa         ASCENDING
                           cb_cliente         ASCENDING
                           cb_ano_lancamento  ASCENDING
                           cb_doc_lancamento  ASCENDING
                           cb_item_lancamento ASCENDING,

          it_tipo_cashback BY tp_cashback ASCENDING,

          itl_vbak  BY vbeln ASCENDING.

    DELETE itl_movimento WHERE cb_status = 'S'.

    LOOP AT itl_bseg INTO DATA(wal_bseg).
      CLEAR: wal_vbak, wal_bkpf.

      IF wal_bseg-bschl EQ '09'.
        CONTINUE.
      ENDIF.

      READ TABLE itl_movimento INTO wal_movimento WITH KEY  cb_empresa         = wal_bseg-bukrs
                                                            cb_cliente         = wal_bseg-kunnr
                                                            cb_ano_lancamento  = wal_bseg-gjahr
                                                            cb_doc_lancamento  = wal_bseg-belnr
                                                            cb_item_lancamento = wal_bseg-buzei
                                                            BINARY SEARCH.

      CHECK sy-subrc IS INITIAL
      AND wal_movimento-cb_status NE 'S'.

      READ TABLE it_tipo_cashback INTO wal_cashback WITH KEY tp_cashback = wal_movimento-cb_tipo BINARY SEARCH.

      CHECK sy-subrc IS INITIAL.

      ol_cashback->m_set_tp_cashback( wal_cashback-tp_cashback ).

      wal_extrato_comp-budat     = wal_movimento-cb_data_documento.
      wal_extrato_comp-gjahr     = wal_movimento-cb_ano_lancamento.
      wal_extrato_comp-zfbdt     = wal_bseg-zfbdt.
      wal_extrato_comp-vencido   = COND #( WHEN wal_extrato_comp-zfbdt < sy-datum THEN abap_true ELSE abap_false ).
      wal_extrato_comp-zuonr     = wal_bseg-zuonr.
      wal_extrato_comp-descricao = wal_cashback-descricao.
      wal_extrato_comp-shkzg     = wal_bseg-shkzg.
      wal_extrato_comp-dmbtr     = wal_bseg-dmbtr.
      wal_extrato_comp-cpudt     = wal_movimento-cb_data_documento.
      wal_extrato_comp-cputm     = wal_movimento-cb_hr_criacao.
      wal_extrato_comp-ov        = wal_movimento-cb_pedido_bloqueio.
      wal_extrato_comp-cb_codigo = wal_movimento-cb_codigo.

      TRY.
          DATA(wal_extrato_aux) = itl_extrato_comp[ zuonr = wal_extrato_comp-zuonr
                                                   tp_cashback = wal_extrato_comp-tp_cashback descricao = wal_extrato_comp-descricao ].

          wal_extrato_aux-dmbtr = wal_extrato_comp-dmbtr.


          COLLECT wal_extrato_aux INTO itl_extrato_comp.

        CATCH cx_root.
          IF wal_movimento-cb_status = 'L'.
            wal_extrato_comp-dmbtr_prim = wal_bseg-dmbtr.
          ENDIF.
          APPEND wal_extrato_comp TO itl_extrato_comp.
      ENDTRY.

      IF wal_movimento-cb_pedido_bloqueio IS NOT INITIAL
         OR wal_movimento-cb_status = 'E'.

        READ TABLE itl_vbak INTO wal_vbak WITH KEY vbeln = wal_movimento-cb_pedido_bloqueio BINARY SEARCH.

        IF wal_bseg-augbl IS NOT INITIAL.
          READ TABLE itl_bseg INTO DATA(wal_bseg_aux) WITH KEY bukrs = wal_bseg-bukrs
                                                               belnr = wal_bseg-augbl
                                                               gjahr = wal_bseg-augdt(04)
                                                               BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            DATA(vl_tabix) = sy-tabix.

            wal_extrato_comp-budat     = wal_movimento-cb_data_documento.
            wal_extrato_comp-gjahr     = wal_movimento-cb_ano_lancamento.
            wal_extrato_comp-zfbdt     = wal_bseg-zfbdt.
            wal_extrato_comp-vencido   = COND #( WHEN wal_extrato_comp-zfbdt < sy-datum THEN abap_true ELSE abap_false ).
            wal_extrato_comp-zuonr     = wal_bseg-zuonr.
            wal_extrato_comp-descricao = wal_cashback-descricao.
            wal_extrato_comp-shkzg     = wal_bseg_aux-shkzg.
            wal_extrato_comp-dmbtr     = wal_bseg_aux-dmbtr.
            wal_extrato_comp-ov        = wal_movimento-cb_pedido_bloqueio.

            DELETE itl_bseg INDEX vl_tabix.

          ENDIF.

        ENDIF.

        IF wal_vbak IS INITIAL AND wal_movimento-cb_doc_consumo IS NOT INITIAL.
          READ TABLE itl_bkpf INTO wal_bkpf WITH KEY belnr = wal_movimento-cb_doc_consumo BINARY SEARCH.
          wal_extrato_comp-budat = wal_bkpf-cpudt.
          wal_extrato_comp-cputm = wal_bkpf-cputm.

        ELSEIF  wal_vbak IS INITIAL.
          wal_extrato_comp-budat = wal_movimento-cb_data_modificacao.
          wal_extrato_comp-cputm = wal_movimento-cb_hora_modificacao.
        ELSE.
          wal_extrato_comp-budat = wal_vbak-audat.
          wal_extrato_comp-cputm = wal_vbak-erzet.
        ENDIF.
        IF wal_movimento-cb_status = 'E'.
          WRITE  wal_movimento-cb_data_modificacao TO wal_movimento-cb_pedido_bloqueio.
          WRITE  wal_movimento-cb_hora_modificacao TO wal_movimento-cb_nf_serie.
        ENDIF.

        r_retorno = m_regextrato_consumo( EXPORTING
                              i_extrato = wal_extrato_comp
                              i_movto   = wal_movimento
                              i_vbak    = itl_vbak
                            CHANGING
                              ch_extrato =  itl_extrato_comp ).

      ENDIF.

    ENDLOOP.

    CLEAR: wal_extrato_comp.

    LOOP AT itl_historico INTO DATA(wal_historico).
      READ TABLE rl_periodo INTO DATA(wal_periodo) INDEX 1.
      IF wal_historico-dt_criacao_pedido >= wal_periodo-low.

        wal_extrato_comp-budat     = wal_historico-dt_criacao_pedido.
        wal_extrato_comp-gjahr     = wal_historico-dt_criacao_pedido+0(4).
        wal_extrato_comp-vencido   = space.
        wal_extrato_comp-vbeln     = wal_historico-pedido_bloqueio.
        wal_extrato_comp-xblnr     = wal_historico-cb_nf_serie.
        wal_extrato_comp-descricao = space.
        wal_extrato_comp-shkzg     = 'S'.
        wal_extrato_comp-dmbtr     = wal_historico-montante_cb.
        wal_extrato_comp-cpudt     = wal_historico-dt_criacao_pedido.
        wal_extrato_comp-cputm     = wal_historico-hr_criacao_pedido.
        wal_extrato_comp-ov        = wal_historico-pedido_bloqueio.

        APPEND wal_extrato_comp TO itl_extrato_comp.
        CLEAR: wal_extrato_comp.

      ENDIF.

      IF wal_historico-data_mod <= wal_periodo-high.

        wal_extrato_comp-budat     = wal_historico-data_mod.
        wal_extrato_comp-gjahr     = wal_historico-data_mod+0(4).
        wal_extrato_comp-vencido   = wal_historico-processo(1).
        wal_extrato_comp-zuonr     = wal_historico-pedido_bloqueio.
        wal_extrato_comp-xblnr     = wal_historico-cb_nf_serie.
        wal_extrato_comp-descricao = wal_historico-processo.
        wal_extrato_comp-shkzg     = 'H'.
        wal_extrato_comp-dmbtr     = wal_historico-montante_cb.
        wal_extrato_comp-cpudt     = wal_historico-data_mod.
        wal_extrato_comp-cputm     = wal_historico-hora_mod.
        wal_extrato_comp-ov        = space.

        APPEND wal_extrato_comp TO itl_extrato_comp.
        CLEAR: wal_extrato_comp.
      ENDIF.

    ENDLOOP.

    SORT itl_extrato_comp DESCENDING BY budat cputm.

    LOOP AT itl_extrato_comp INTO DATA(wal_extrato_comp2).
      CHECK wal_extrato_comp2-ov IS INITIAL.

      APPEND wal_extrato_comp2 TO itl_extrato_comp_aux.
    ENDLOOP.

    LOOP AT itl_extrato_comp ASSIGNING FIELD-SYMBOL(<fsl_extrato_comp>).
      CHECK <fsl_extrato_comp>-shkzg EQ 'S' AND <fsl_extrato_comp>-vencido NE 'E'  .
      m_agrupar_extrato( EXPORTING i_extrato_comp  = itl_extrato_comp
        i_extrato = <fsl_extrato_comp>
        CHANGING i_extrato_agrupa = itl_extrato_comp_aux ).
    ENDLOOP.

    SORT itl_extrato_comp_aux DESCENDING BY budat cputm.

    READ TABLE rl_periodo INTO DATA(ls_periodo) INDEX 1.
    IF sy-subrc = 0.
      DELETE itl_extrato_comp_aux WHERE budat < ls_periodo-low
                                        OR budat > ls_periodo-high.
    ENDIF.

    LOOP AT itl_extrato_comp_aux ASSIGNING FIELD-SYMBOL(<fsl_extrato_comp_aux>).
      IF <fsl_extrato_comp_aux>-vencido = 'E' .
        <fsl_extrato_comp_aux>-vbeln = space.
        <fsl_extrato_comp_aux>-xblnr = space.
      ENDIF.
      IF <fsl_extrato_comp_aux>-shkzg = 'H' .
        IF <fsl_extrato_comp_aux>-dmbtr    = <fsl_extrato_comp_aux>-dmbtr_prim.
          <fsl_extrato_comp_aux>-vencido  = 'L'.
        ELSE.
          <fsl_extrato_comp_aux>-cb_codigo = space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING itl_extrato_comp_aux TO it_extrato.

    LOOP AT itl_extrato_contab_erros INTO DATA(wal_contab_erros).

      wal_extrato-vencido     = wal_contab_erros-cb_status.
      wal_extrato-budat       = wal_contab_erros-cb_data_modificacao.
      wal_extrato-cputm       = wal_contab_erros-cb_hora_modificacao.
      wal_extrato-cb_codigo   = wal_contab_erros-cb_codigo.
      wal_extrato-zuonr       = wal_contab_erros-cb_doc_ref_criacao.
      wal_extrato-motivo_excl = wal_contab_erros-cb_texto.
      wal_extrato-descricao   = 'Em processamento'.

      APPEND wal_extrato TO it_extrato.

    ENDLOOP.

    SORT it_extrato DESCENDING BY budat cputm.

    r_retorno = abap_true.

  ENDMETHOD.

--

M_CONSULTAR_MOVTO


  METHOD m_consultar_movto.

    DATA itl_dockey TYPE zttcb_dockey.

    DATA wal_dockey TYPE zscb_dockey.

    DATA ol_modelo TYPE REF TO zclcb_model_consulta.

    ol_modelo = o_dao_consulta->m_get_model( ).

    DATA(itl_bseg) = ol_modelo->m_get_bseg( ).

    LOOP AT itl_bseg ASSIGNING FIELD-SYMBOL(<fsl_bseg>).

      CLEAR wal_dockey.

      wal_dockey-belnr = <fsl_bseg>-belnr.
      wal_dockey-bukrs = <fsl_bseg>-bukrs.
      wal_dockey-gjahr = <fsl_bseg>-gjahr.
      wal_dockey-kunnr = <fsl_bseg>-kunnr.

      APPEND wal_dockey TO itl_dockey.

    ENDLOOP.

    o_dao_movto->m_set_dockey( itl_dockey ).
    o_dao_movto->m_select_ztcb_movimento_table( ).

    r_retorno = abap_true.

  ENDMETHOD.

--

M_REGEXTRATO_CONSUMO

  METHOD m_regextrato_consumo.
    DATA wal_extrato TYPE zstbcb_extrato_comp.

    wal_extrato-budat = i_extrato-budat.
    wal_extrato-gjahr = i_extrato-budat(4).
    wal_extrato-shkzg = 'S'.
    wal_extrato-dmbtr = i_extrato-dmbtr.
    wal_extrato-vbeln = i_movto-cb_pedido_bloqueio.
    wal_extrato-xblnr = i_movto-cb_nf_serie.
    wal_extrato-cpudt = i_extrato-cpudt.
    wal_extrato-cputm = i_extrato-cputm.
    wal_extrato-ov    = i_extrato-ov.


    IF i_extrato-ov(2) = 'ST'.
      wal_extrato-cpudt = i_extrato-budat.
      IF wal_extrato-xblnr <> ' '.
        wal_extrato-xblnr = ' '.
        wal_extrato-vencido = 'R'.
      ELSE.
        wal_extrato-vencido = 'S'.
      ENDIF.
    ENDIF.
    IF i_movto-cb_status      = 'E'.
      wal_extrato-vencido     = 'E'.
      wal_extrato-budat       = i_movto-cb_data_modificacao.
      wal_extrato-cpudt       = i_movto-cb_data_modificacao.
      wal_extrato-cputm       = i_movto-cb_hora_modificacao.
      wal_extrato-cb_codigo   = i_movto-cb_codigo.
      wal_extrato-zuonr       = i_movto-cb_doc_ref_criacao.
      wal_extrato-motivo_excl = i_movto-cb_texto.
      wal_extrato-descricao   = i_extrato-descricao.

    ENDIF.

    TRY.
        DATA(wal_extrato_aux) = ch_extrato[ vbeln = i_movto-cb_pedido_bloqueio
                                            xblnr = i_movto-cb_nf_serie ].


        wal_extrato_aux-dmbtr = i_extrato-dmbtr.
        COLLECT wal_extrato_aux INTO ch_extrato.

      CATCH cx_root.

        COLLECT wal_extrato INTO ch_extrato.

    ENDTRY.
    r_retorno = abap_true.

  ENDMETHOD.



--

M_SET_DAO

  METHOD m_set_dao.

    o_dao_consulta = i_dao.

    r_return = abap_true.

  ENDMETHOD.

--

M_SET_COD_CASHBACK

  METHOD m_set_cod_cashback.

    r_validacao = o_dao_movto->m_set_cod_cashback( i_cod_cashback ).

  ENDMETHOD.

--

M_GET_MOVIMENTO_SINGLE

  METHOD m_get_movimento_single.

    r_single = o_dao_movto->m_get_movimento_single( ).

  ENDMETHOD.

--

M_GET_DAO

  METHOD m_get_dao.

    IF i_teste IS INITIAL.
      r_dao ?= o_dao_consulta.
    ENDIF.

  ENDMETHOD.

--

M_CONSULTAR_CONTROLE_ZFPR

    METHOD m_consultar_controle_zfpr.

      o_dao_consulta->m_consulta_zfpr( ).

      r_return = abap_false.

    ENDMETHOD.



--



M_AGRUPAR_EXTRATO

  METHOD m_agrupar_extrato.

    DATA itl_extrato_agrupa TYPE zttbcb_extrato_comp.
    DATA(itl_extrato_comp) = i_extrato_comp.
    DATA vl_cputm TYPE cputm.

    r_return = abap_true.

    SORT itl_extrato_comp BY shkzg ASCENDING.

    LOOP AT itl_extrato_comp INTO DATA(wal_extrato_comp) WHERE ov = i_extrato-ov.

      IF wal_extrato_comp-shkzg = 'H' AND vl_cputm < wal_extrato_comp-cputm.

        vl_cputm = wal_extrato_comp-cputm.

      ENDIF.

      APPEND wal_extrato_comp TO itl_extrato_agrupa.

    ENDLOOP.

    SORT itl_extrato_agrupa DESCENDING BY budat cputm.

    MOVE-CORRESPONDING itl_extrato_agrupa TO i_extrato_agrupa KEEPING TARGET LINES.

    r_return = abap_false.

  ENDMETHOD.

--

M_CHECK_TRANSFERENCIA

  METHOD m_check_transferencia.

    DATA: itl_vbeln TYPE edm_vbeln_range_tt.

    APPEND VALUE edm_vbeln_range( sign = 'I'          option = 'BT'
                                  low  = 'ST00000000' high   = 'ST99999999' ) TO itl_vbeln.

    r_return = o_dao_consulta->m_valida_qtde_trans(
                 EXPORTING
                  i_kunnr   = i_kunnr
                  i_vbeln   = itl_vbeln
                 IMPORTING
                   e_message = e_message
               ).
  ENDMETHOD.



--



M_CONSULTAR_MOVIMENTO



  METHOD m_consultar_movimento.

  r_return = o_dao_movto->m_buscar_movimento( i_single ).

  ENDMETHOD.

--

M_BUSCAR_CB_BKPF

  METHOD m_buscar_cb_bkpf.

    r_return = abap_true.

    IF i_teste IS INITIAL.
      o_dao_consulta->o_dao_movto ?= o_dao_movto.
    ENDIF.

    o_dao_consulta->m_select_bkpf_cb( ).

    r_return = abap_false.

  ENDMETHOD.

--

M_BUSCA_BLOQUEIO

  method M_BUSCA_BLOQUEIO.

    r_validacao = o_dao_consulta->m_busca_bloqueio( ).

  endmethod.

--

M_BLOQ_OV_SIMULT

  method M_BLOQ_OV_SIMULT.

    r_validacao = o_dao_consulta->m_bloq_ov_simult( i_bloq_sim ).

  endmethod.

--

M_ALTBLOQ_SIMULT

  METHOD M_ALTBLOQ_SIMULT.

   r_validacao = o_dao_consulta->m_modify_ztcb_bloq_simult( i_bloq_sim ).

  ENDMETHOD.



--



M_GET_ZTCB_BLOQ_SIMULT

  METHOD m_get_ztcb_bloq_simult.

    r_single = o_dao_consulta->m_get_ztcb_bloq_simult( ).

  ENDMETHOD.

--

M_CONSULTAR_CONTAB_ERROS

  METHOD m_consultar_contab_erros.

    r_return = abap_true.

    IF i_teste IS INITIAL.
      o_dao_movto->m_set_model( m_get_model( ) ).
    ENDIF.
    o_dao_movto->m_select_mov_contab_erros( ).

    r_return = abap_false.

  ENDMETHOD.

--

Método: m_select_historico

METHOD m_select_historico.
  " Selecionar dados históricos de transações financeiras ou documentos
  SELECT * FROM tabela_hist WHERE data BETWEEN p_data_inicio AND p_data_fim.
  LOOP AT tabela_hist.
    " Processar cada registro conforme necessário
  ENDLOOP.
ENDMETHOD.

Método: m_valida_qtde_trans

METHOD m_valida_qtde_trans.
  " Validar se a quantidade de transações está dentro do limite permitido
  IF quantidade_transacoes > limite_maximo.
    RAISE limite_excedido.
  ENDIF.
ENDMETHOD.

Método: m_checa_ztcb_bloq_simult

METHOD m_checa_ztcb_bloq_simult.
  " Verificar se existe um bloqueio simultâneo no ZTCB
  SELECT * FROM tabela_ztcb WHERE status = 'Bloqueado' AND data_bloqueio = sy-datum.
  IF sy-subrc = 0.
    " Bloqueio encontrado
    RETURN 'Bloqueio Simultâneo Existente'.
  ELSE.
    RETURN 'Nenhum Bloqueio Simultâneo'.
  ENDIF.
ENDMETHOD.

Método: m_busca_bloqueio

METHOD m_busca_bloqueio.
  " Buscar registros de bloqueios financeiros no sistema
  SELECT * FROM tabela_bloqueio WHERE cliente = p_cliente AND status = 'Ativo'.
  LOOP AT tabela_bloqueio.
    " Processar os bloqueios encontrados
  ENDLOOP.
ENDMETHOD.

Método: m_bloq_ov_simult

METHOD m_bloq_ov_simult.
  " Bloquear ordens de venda simultâneas para um cliente específico
  UPDATE tabela_ordens SET status = 'Bloqueado' WHERE cliente = p_cliente AND status = 'Aberto'.
  COMMIT WORK.
ENDMETHOD.

Método: m_modify_ztcb_bloq_simult

METHOD m_modify_ztcb_bloq_simult.
  " Modificar o bloqueio simultâneo no ZTCB
  UPDATE tabela_ztcb SET status = p_status WHERE id_bloqueio = p_id_bloqueio.
  COMMIT WORK.
ENDMETHOD.

Método: m_eliminar_ztcb_bloq_simult

METHOD m_eliminar_ztcb_bloq_simult.
  " Eliminar bloqueio simultâneo no ZTCB
  DELETE FROM tabela_ztcb WHERE id_bloqueio = p_id_bloqueio.
  COMMIT WORK.
ENDMETHOD.

Método: m_desbloq_client_simult

METHOD m_desbloq_client_simult.
  " Desbloquear cliente com bloqueio simultâneo
  UPDATE tabela_bloqueio SET status = 'Desbloqueado' WHERE cliente = p_cliente AND status = 'Bloqueado'.
  COMMIT WORK.
ENDMETHOD.

Método: m_get_ztcb_bloq_simult

METHOD m_get_ztcb_bloq_simult.
  " Obter informações sobre bloqueios simultâneos no ZTCB
  SELECT * FROM tabela_ztcb WHERE status = 'Bloqueado'.
  LOOP AT tabela_ztcb.
    " Processar cada bloqueio encontrado
  ENDLOOP.
ENDMETHOD.

Método: m_select_movimento_completo

METHOD m_select_movimento_completo.
  " Selecionar movimento financeiro completo
  SELECT * FROM tabela_movimento WHERE tipo = p_tipo AND status = p_status.
  LOOP AT tabela_movimento.
    " Processar cada movimento encontrado
  ENDLOOP.
ENDMETHOD.

METHOD m_select_bkpf_cb.
  DATA: lt_bkpf TYPE TABLE OF bkpf,
        lv_belnr TYPE bkpf-belnr.

  " Selecionar documentos contábeis com base no tipo e status
  SELECT * 
    INTO TABLE lt_bkpf
    FROM bkpf
    WHERE blart = 'SA' " Tipo de documento
    AND stblg = 'A'   " Status aprovado
    ORDER BY budat DESC.

  LOOP AT lt_bkpf INTO DATA(ls_bkpf).
    WRITE: / ls_bkpf-belnr, ls_bkpf-budat.
  ENDLOOP.
ENDMETHOD.

METHOD m_set_model.
  DATA: lt_model TYPE TABLE OF zmodel,
        ls_model TYPE zmodel.

  " Configurar modelo com dados iniciais
  ls_model-field1 = 'Valor 1'.
  ls_model-field2 = 'Valor 2'.
  APPEND ls_model TO lt_model.

  LOOP AT lt_model INTO DATA(ls_result).
    WRITE: / ls_result-field1, ls_result-field2.
  ENDLOOP.
ENDMETHOD.

METHOD m_consultar_saldo.
  DATA: lv_saldo TYPE p DECIMALS 2,
        lv_conta TYPE bkpf-bukrs.

  " Consultar saldo da conta bancária
  SELECT SUM(dmbtr)
    INTO lv_saldo
    FROM bsid
    WHERE bukrs = lv_conta.

  WRITE: / 'Saldo da Conta:', lv_saldo.
ENDMETHOD.

METHOD m_consultar_extrato.
  DATA: lt_extrato TYPE TABLE OF bsid,
        lv_bukrs TYPE bukrs.

  " Consultar extrato de movimentação financeira
  SELECT * 
    INTO TABLE lt_extrato
    FROM bsid
    WHERE bukrs = lv_bukrs.

  LOOP AT lt_extrato INTO DATA(ls_extrato).
    WRITE: / ls_extrato-belnr, ls_extrato-budat, ls_extrato-dmbtr.
  ENDLOOP.
ENDMETHOD.

METHOD m_consulta_zfpr.
  DATA: lt_zfpr TYPE TABLE OF zfpr,
        lv_codigo TYPE zfpr-codigo.

  " Consultar informações no ZFPR
  SELECT * 
    INTO TABLE lt_zfpr
    FROM zfpr
    WHERE codigo = lv_codigo.

  LOOP AT lt_zfpr INTO DATA(ls_zfpr).
    WRITE: / ls_zfpr-codigo, ls_zfpr-descricao.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_bsid.
  DATA: lt_bsid TYPE TABLE OF bsid,
        lv_belnr TYPE bsid-belnr.

  " Selecionar documentos contábeis em aberto
  SELECT * 
    INTO TABLE lt_bsid
    FROM bsid
    WHERE belnr = lv_belnr.

  LOOP AT lt_bsid INTO DATA(ls_bsid).
    WRITE: / ls_bsid-belnr, ls_bsid-dmbtr.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_bseg.
  DATA: lt_bseg TYPE TABLE OF bseg,
        lv_belnr TYPE bseg-belnr.

  " Selecionar itens do documento contábil
  SELECT * 
    INTO TABLE lt_bseg
    FROM bseg
    WHERE belnr = lv_belnr.

  LOOP AT lt_bseg INTO DATA(ls_bseg).
    WRITE: / ls_bseg-belnr, ls_bseg-dmbtr.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_bkpf.
  DATA: lt_bkpf TYPE TABLE OF bkpf,
        lv_belnr TYPE bkpf-belnr.

  " Selecionar cabeçalho de documentos contábeis
  SELECT * 
    INTO TABLE lt_bkpf
    FROM bkpf
    WHERE belnr = lv_belnr.

  LOOP AT lt_bkpf INTO DATA(ls_bkpf).
    WRITE: / ls_bkpf-belnr, ls_bkpf-budat.
  ENDLOOP.
ENDMETHOD.

METHOD m_get_model.
  DATA: lt_model TYPE TABLE OF zmodel.

  " Obter modelo de dados configurado
  LOOP AT lt_model INTO DATA(ls_model).
    WRITE: / ls_model-field1, ls_model-field2.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_vbak.
  DATA: lt_vbak TYPE TABLE OF vbak,
        lv_vbeln TYPE vbak-vbeln.

  " Selecionar cabeçalho de vendas
  SELECT * 
    INTO TABLE lt_vbak
    FROM vbak
    WHERE vbeln = lv_vbeln.

  LOOP AT lt_vbak INTO DATA(ls_vbak).
    WRITE: / ls_vbak-vbeln, ls_vbak-auart.
  ENDLOOP.
ENDMETHOD.

METHOD m_consultar_movto.
  DATA: lt_movto TYPE TABLE OF bsid,
        lv_belnr TYPE bsid-belnr.

  " Consultar movimentações financeiras
  SELECT * 
    INTO TABLE lt_movto
    FROM bsid
    WHERE belnr = lv_belnr.

  LOOP AT lt_movto INTO DATA(ls_movto).
    WRITE: / ls_movto-belnr, ls_movto-dmbtr.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_ztcb_controle_zf.
  DATA: lt_controle TYPE TABLE OF ztcb_controle,
        lv_id TYPE ztcb_controle-id.

  " Consultar controle de ZTCB
  SELECT * 
    INTO TABLE lt_controle
    FROM ztcb_controle
    WHERE id = lv_id.

  LOOP AT lt_controle INTO DATA(ls_controle).
    WRITE: / ls_controle-id, ls_controle-status.
  ENDLOOP.
ENDMETHOD.

METHOD m_consultar_movto_cb.
  DATA: lt_movto TYPE TABLE OF bsid,
        lv_belnr TYPE bsid-belnr.

  " Consultar movimentação contábil
  SELECT * 
    INTO TABLE lt_movto
    FROM bsid
    WHERE belnr = lv_belnr.

  LOOP AT lt_movto INTO DATA(ls_movto).
    WRITE: / ls_movto-belnr, ls_movto-dmbtr.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_bkpf_st.
  DATA: lt_bkpf TYPE TABLE OF bkpf,
        lv_status TYPE bkpf-stblg.

  " Selecionar documentos contábeis com status específico
  SELECT * 
    INTO TABLE lt_bkpf
    FROM bkpf
    WHERE stblg = lv_status.

  LOOP AT lt_bkpf INTO DATA(ls_bkpf).
    WRITE: / ls_bkpf-belnr, ls_bkpf-budat.
  ENDLOOP.
ENDMETHOD.

METHOD m_select_bseg_st_pago.
  DATA: lt_bseg TYPE TABLE OF bseg,
        lv_status TYPE bseg-status.

  " Selecionar itens contábeis com status 'Pago'
  SELECT * 
    INTO TABLE lt_bseg
    FROM bseg
    WHERE status = lv_status.

  LOOP AT lt_bseg INTO DATA(ls_bseg).
    WRITE: / ls_bseg-belnr, ls_bseg-dmbtr.
  ENDLOOP.
ENDMETHOD.













